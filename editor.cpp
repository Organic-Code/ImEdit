/****************************************************************************************
 *    MIT License                                                                       *
 *                                                                                      *
 *    Copyright (c) 2024 Lucas Lazare                                                   *
 *                                                                                      *
 *    Permission is hereby granted, free of charge, to any person obtaining a copy      *
 *    of this software and associated documentation files (the "Software"), to deal     *
 *    in the Software without restriction, including without limitation the rights      *
 *    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell         *
 *    copies of the Software, and to permit persons to whom the Software is             *
 *    furnished to do so, subject to the following conditions:                          *
 *                                                                                      *
 *    The above copyright notice and this permission notice shall be included in all    *
 *    copies or substantial portions of the Software.                                   *
 *                                                                                      *
 *    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR        *
 *    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,          *
 *    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE       *
 *    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER            *
 *    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,     *
 *    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE     *
 *    SOFTWARE.                                                                         *
 *                                                                                      *
 ****************************************************************************************/

#include <algorithm>
#include <sstream>
#include <cctype>
#include <chrono>
#include <cmath>
#include <sstream>
#include "editor.h"

#define IMEDIT_CALL_PMC(x)                                                             \
        bool called_pmc = false;                                                       \
        if (_should_call_pmc && _public_methods_callback) {                            \
            called_pmc = true;                                                         \
            _should_call_pmc = false;                                                  \
            _public_methods_callback(_public_methods_callback_data, [this](){ x(); }); \
        }
#define IMEDIT_CALL_PMC_CAPT(x,y)                                                         \
        bool called_pmc = false;                                                          \
        if (_should_call_pmc && _public_methods_callback) {                               \
            called_pmc = true;                                                            \
            _should_call_pmc = false;                                                     \
            _public_methods_callback(_public_methods_callback_data, [this,y](){ x(y); }); \
        }
#define IMEDIT_RESTORE_PMC if (called_pmc) { _should_call_pmc = true; }

namespace {
    ImVec2 operator+(const ImVec2& lhs, const ImVec2& rhs) {
        return {lhs.x + rhs.x, lhs.y + rhs.y};
    }
    ImVec2 operator-(const ImVec2& lhs, const ImVec2& rhs) {
        return {lhs.x - rhs.x, lhs.y - rhs.y};
    }
    ImVec2 operator*(const ImVec2& lhs, float rhs) {
        return {lhs.x * rhs, lhs.y * rhs};
    }

    bool isopening(char c) {
        return c == '(' || c == '[' || c == '{';
    }

    bool isclosing(char c) {
        return c == ')' || c == ']' || c == '}';
    }

    bool iscomma(char c) {
        return c == ',';
    }

    bool iscseparator(char c) {
        switch (c) {
            case '!':
            case '%':
            case '&':
            case '*':
            case '+':
            case ',':
            case '-':
            case '.':
            case '/':
            case ':':
            case ';':
            case '<':
            case '=':
            case '>':
            case '?':
            case '^':
            case '|':
            case '~':
                return true;
            default:
                return false;
        }
    }

    bool istokseparator(char c) {
        return isopening(c) || isclosing(c) || iscomma(c) || std::isspace(c) || iscseparator(c);
    }

    unsigned int char_count_for_utf8(char beg) noexcept {
        if ((beg & 0b1000'0000) == 0b0000'0000) {
            return 1;
        }

        if ((beg & 0b1110'0000) == 0b1100'0000) {
            return 2;
        }

        if ((beg & 0b1111'0000) == 0b1110'0000) {
            return 3;
        }

        if ((beg & 0b1111'1000) == 0b1111'0000) {
            return 4;
        }

        assert(false && "Invalid UTF8 glyph head");
        return 1;
    }

    // returns true if within a utf8 char, but returns false if in an ascii char or at the head of an utf8 char
    bool is_within_utf8(char c) {
        return (c & 0b1100'0000) == 0b1000'0000;
    }

    unsigned int char_count_for_utf8(ImWchar beg) noexcept {
        return char_count_for_utf8(static_cast<char>(beg));
    }

    std::size_t utf8_string_size(const std::string& str) {
        std::size_t char_count = 0;
        for (unsigned int i = 0 ; i < str.size() ; i += char_count_for_utf8(str[i])) {
            ++char_count;
        }

        return char_count;
    }

}


ImEdit::editor::iterator ImEdit::editor::iterator::operator++(int) noexcept {
    auto copy = *this;
    ++*this;
    return copy;
}

ImEdit::editor::iterator &ImEdit::editor::iterator::operator++() noexcept {
    assert((current.line + 1 < ed->_lines.size()
            || current.token + 1 < ed->_lines.back().tokens.size()
            || !ed->_lines.back().tokens.empty() && current.char_index < ed->_lines.back().tokens.back().data.size())
            && "Trying to increment past .end()");
    current = ed->move_coordinates_right(current);
    return *this;
}

ImEdit::editor::iterator ImEdit::editor::iterator::operator--(int) noexcept {
    auto copy = *this;
    --*this;
    return copy;
}

ImEdit::editor::iterator &ImEdit::editor::iterator::operator--() noexcept {
    assert((current.line > 0 || current.token > 0 || current.char_index > 0) && "Trying to decrement before .begin()");
    current = ed->move_coordinates_left(current);
    return *this;
}

char ImEdit::editor::iterator::operator*() const noexcept {
    assert(ed && "Dereferencing a default initialized iterator");

    if (ed->_lines[current.line].tokens.empty()) {
        return '\n';
    }
    auto& data = ed->_lines[current.line].tokens[current.token].data;
    if (data.size() == current.char_index) {
        if (current.token == ed->_lines[current.line].tokens.size() - 1) {
            return '\n';
        }
        return ed->_lines[current.line].tokens[current.token + 1].data[0];
    }
    return data[current.char_index];
}

bool ImEdit::editor::iterator::operator==(const ImEdit::editor::iterator &other) const noexcept {
    return ed == other.ed && ed->coordinates_eq(current, other.current);
}

bool ImEdit::editor::iterator::operator!=(const ImEdit::editor::iterator &other) const noexcept {
    return !(*this == other);
}

ImEdit::editor::iterator ImEdit::editor::begin() noexcept {
    return iterator{this, coordinates{0, 0, 0}};
}

ImEdit::editor::iterator ImEdit::editor::end() noexcept {
    if (_lines.empty()) {
        return begin();
    }

    if (_lines.back().tokens.empty()) {
        return iterator{this, coordinates{static_cast<unsigned int>(_lines.size() - 1), 0, 0}};
    }

    return iterator{this, coordinates{static_cast<unsigned int>(_lines.size() - 1),
                                      static_cast<unsigned int>(_lines.back().tokens.size() - 1),
                                      static_cast<unsigned int>(_lines.back().tokens.back().data.size())}};
}


void ImEdit::editor::set_data(const std::string &data) {
    clear();
    std::istringstream iss(data);

    // White spaces are included into this array.
    std::vector<line> unparsed_tokens;

    while (!iss.eof()) {
        constexpr unsigned starting_size = 128;

        std::string str;
        str.resize(starting_size / 2, '\0');
        unsigned int already_assigned = 0;

        do {
            iss.clear();
            str.resize(str.size() * 2, '\0');
            iss.getline(str.data() + already_assigned, static_cast<long>(str.size() + 1 - already_assigned));
            already_assigned = str.size();

        } while (iss.fail());

        std::string line = str.data(); // trimming extras '\0' NOLINT(*-redundant-string-cstr)
        unparsed_tokens.emplace_back();
        auto it = line.begin();
        while (it != line.end()) {

            std::string current_token;
            std::deque<token>& tokens = unparsed_tokens.back().tokens;

            if (std::isspace(*it)) {
                do {
                    current_token.push_back(*it++);
                } while (it != line.end() && std::isspace(*it));
                tokens.push_back({std::move(current_token), token_type::blank});
            }
            else if (isclosing(*it)) {
                current_token.push_back(*it++);
                tokens.push_back({std::move(current_token), token_type::closing});
            }
            else if (isopening(*it)) {
                current_token.push_back(*it++);
                tokens.push_back({std::move(current_token), token_type::opening});
            }
            else if (iscomma(*it)) {
                current_token.push_back(*it++);
                tokens.push_back({std::move(current_token), token_type::punctuation});
            }
            else {
                do {
                    current_token.push_back(*it++);
                } while (it != line.end() && !istokseparator(*it));
                tokens.push_back({std::move(current_token), token_type::unknown});
            }

        }
    }
    // TODO: make this tokenization optional
    // TODO: call lexer function
}

void ImEdit::editor::render() {
    if (_default_font != nullptr) {
        ImGui::PushFont(_default_font);
    }

    if (_longest_line_px == 0) {
        find_longest_line();
    }

    // space to the left for displaying line numbers, breakpoints, and such
    const auto extra_padding = compute_extra_padding();
    const auto region_avail = ImGui::GetContentRegionAvail();
    const ImVec2 draw_region{_width ?
                                    *_width != 0 ? std::max(_longest_line_px + extra_padding, *_width) : region_avail.x
                                    : _longest_line_px + extra_padding,
                             _height ?
                                    *_height != 0 ? *_height : region_avail.y
                                    : std::min(ImGui::GetTextLineHeightWithSpacing() * static_cast<float>(_lines.size()), region_avail.y)};

    const ImVec2 window_region{
            std::min(_width ? *_width : _longest_line_px + extra_padding, ImGui::GetContentRegionAvail().x),
            std::min(_height ? *_height : ImGui::GetTextLineHeightWithSpacing() * static_cast<float>(_lines.size() + 1), region_avail.y)
    };



    if (!ImGui::BeginChild(_imgui_id.c_str(), window_region, ImGuiChildFlags_None, ImGuiWindowFlags_HorizontalScrollbar)) {
        ImGui::EndChild();
        if (_default_font != nullptr) {
            ImGui::PopFont();
        }
        return;
    }

    _imgui_cursor_position = ImGui::GetCursorScreenPos();
    auto imgui_cursor = _imgui_cursor_position;
    auto draw_list = ImGui::GetWindowDrawList();

    const auto space_length = glyph_size();


    if (draw_region.x == 0 || draw_region.y == 0) {
        ImGui::EndChild();
        if (_default_font != nullptr) {
            ImGui::PopFont();
        }
        return;
    }

    if (_should_grab_focus) {
        _should_grab_focus = false;
        ImGui::SetWindowFocus();
    }

    ImGui::InvisibleButton("invisible button",
                           {draw_region.x, std::max(draw_region.y, ImGui::GetTextLineHeightWithSpacing() * static_cast<float>(_lines.size()))});

    const auto rect_min = ImGui::GetItemRectMin();
    const auto rect_max = ImGui::GetItemRectMax();

    handle_mouse_input();
    handle_kb_input();


    if (_scroll_up_next_frame) {
        ImGui::SetScrollY(ImGui::GetScrollY() - ImGui::GetTextLineHeightWithSpacing());
        _scroll_up_next_frame = false;
    }

    if (_scroll_down_next_frame) {
        ImGui::SetScrollY(ImGui::GetScrollY() + ImGui::GetTextLineHeightWithSpacing());
        _scroll_down_next_frame = false;
    }

    auto line_numbers_max_glyphs = std::to_string(_lines.size()).size();

    auto first_rendered_line = static_cast<unsigned int>(std::floor(ImGui::GetScrollY()/ ImGui::GetTextLineHeightWithSpacing()));
    auto last_rendered_line = first_rendered_line + static_cast<unsigned int>(draw_region.y / ImGui::GetTextLineHeightWithSpacing()) + 1;
    last_rendered_line = std::min(last_rendered_line, static_cast<unsigned int>(_lines.size()));

    imgui_cursor.y += ImGui::GetTextLineHeightWithSpacing() * static_cast<float>(first_rendered_line);

    draw_list->AddRectFilled(imgui_cursor,
                             {imgui_cursor.x + draw_region.x, imgui_cursor.y + draw_region.y + ImGui::GetTextLineHeightWithSpacing() * 2},
                             _style.background_color);

    auto tooltip_this_frame = _tooltips.end();
    auto mouse_coord = screen_to_token_coordinates(ImGui::GetMousePos());

    for (unsigned int i = first_rendered_line ; i < last_rendered_line ; ++i) {
        const line &line = _lines[i];

        assert(!_cursors.empty());
        if (_cursors.back().coord.line == i) {
            imgui_cursor.y -= (ImGui::GetTextLineHeightWithSpacing() - glyph_size().y) / 2;
            draw_list->AddRectFilled(imgui_cursor, {imgui_cursor.x + draw_region.x,
                                                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()},
                                     _style.current_line_color);
            imgui_cursor.y += (ImGui::GetTextLineHeightWithSpacing() - glyph_size().y) / 2;
        }


        // Rendering leftmost padding with line numbers
        if (line.has_breakpoint) {
            auto radius = space_length.y / 3;
            draw_list->AddCircleFilled({
                    imgui_cursor.x + space_length.x / 2 + static_cast<float>(line_numbers_max_glyphs - 1) * space_length.x,
                    imgui_cursor.y + space_length.y / 2},
                   radius, _style.breakpoint_color);
        }
        else if (is_mouse_in_breakpoint_column() && mouse_coord.line == i) {

            auto radius = space_length.y / 3;
            draw_list->AddCircleFilled({
                    imgui_cursor.x + space_length.x / 2 + static_cast<float>(line_numbers_max_glyphs - 1) * space_length.x,
                    imgui_cursor.y + space_length.y / 2},
                   radius, _style.breakpoint_hover_color);
        }
        else {
            // drawing line numbers
            auto line_number = std::to_string(i);
            auto extra_shift = static_cast<float>(line_numbers_max_glyphs - line_number.size()) * space_length.x;

            draw_list->AddText({imgui_cursor.x + extra_shift, imgui_cursor.y}, _style.line_number_color,
                               line_number.c_str());
        }





        imgui_cursor.x += extra_padding - 5;
        draw_list->AddLine(imgui_cursor, {imgui_cursor.x, imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()},
                           _style.line_number_separator_color);
        imgui_cursor.x += 5;

        if (line.background) {
            imgui_cursor.y -= (ImGui::GetTextLineHeightWithSpacing() - glyph_size().y) / 2;
            draw_list->AddRectFilled(imgui_cursor, {imgui_cursor.x + draw_region.x - extra_padding,
                                                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()},
                                     *line.background);
            imgui_cursor.y += (ImGui::GetTextLineHeightWithSpacing() - glyph_size().y) / 2;
        }

        // drawing search results
        for (region searched : _regex_results) {
            render_region_line(searched, i, draw_region, imgui_cursor, _style.search_match_color, _style.search_match_outline_color);
        }

        // Drawing selected region
        for (region select: _selections) {
            render_region_line(select, i, draw_region, imgui_cursor, _style.selection_color, _style.selection_outline_color);
        }


        bool is_leading_space = true;
        unsigned int column = 0;

        for (const token &token: line.tokens) {
            if (imgui_cursor.x >= draw_region.x + _imgui_cursor_position.x + extra_padding) {
                break;
            }

            // If you are getting an error here, maybe you defined your own token types and forgot to add them to ImEdit::editor.get_style().token_colors
            const token_style style = _style.token_style.at(token.type);
            const std::string &data = token.data;


            if (token.type == token_type::blank) {

                bool show_space = _show_leading_space && is_leading_space;
                for (char c: data) {
                    if (c == '\t') {
                        unsigned int number_of_spaces = _tab_length - column % _tab_length;
                        float tab_length = space_length.x * static_cast<float>(number_of_spaces);
                        if (show_space) {
                            auto line_height = ImGui::GetFontSize() / 20;
                            auto line_y_pos = (ImGui::GetTextLineHeightWithSpacing() - line_height) / 2;
                            draw_list->AddLine(
                                    ImVec2(imgui_cursor.x + 2, imgui_cursor.y + line_y_pos),
                                    ImVec2(imgui_cursor.x + tab_length - 2, imgui_cursor.y + line_y_pos),
                                    style.color,
                                    line_height);
                        }
                        imgui_cursor.x += tab_length;
                        column += number_of_spaces;
                    } else {
                        if (show_space) {
                            auto radius = ImGui::GetFontSize() / 20;
                            auto pos_y_shift = (ImGui::GetTextLineHeightWithSpacing() - radius) / 2;
                            draw_list->AddCircleFilled(
                                    ImVec2(imgui_cursor.x + space_length.x / 2, imgui_cursor.y + pos_y_shift),
                                    radius,
                                    style.color
                            );
                        }
                        imgui_cursor.x += space_length.x;
                        ++column;
                    }
                }

            } else {

                is_leading_space = false;

                bool font_pushed = false;
                if (style.bold) {
                    if (style.italic) {
                        if (_bold_italic_font != nullptr) {
                            ImGui::PushFont(_bold_italic_font);
                            font_pushed = true;
                        }
                    } else {
                        if (_bold_font != nullptr) {
                            ImGui::PushFont(_bold_font);
                            font_pushed = true;
                        }
                    }
                } else if (style.italic) {
                    if (_italic_font != nullptr) {
                        ImGui::PushFont(_italic_font);
                        font_pushed = true;
                    }
                }

                draw_list->AddText(imgui_cursor, style.color, data.data(), data.data() + data.size());
                auto text_size = calc_text_size(data.data(), data.data() + data.size());

                if (token.id != 0 && ImGui::IsMouseHoveringRect(imgui_cursor, imgui_cursor + text_size)
                    && ImGui::IsItemHovered()) {
                    auto tooltip = _tooltips.find(token);
                    if (tooltip != _tooltips.end()) {
                        tooltip_this_frame = tooltip;
                        if (_tooltip && *_tooltip != tooltip) {
                            _tooltip_chrono.reset();
                        }
                        if (!_tooltip || *_tooltip != tooltip) {
                            _tooltip = tooltip;
                            auto now = std::chrono::system_clock::now();
                            if (!_tooltip_chrono) {
                                _tooltip_chrono = now;
                            }
                            if (now - *_tooltip_chrono <= _tooltip_delay) {
                                _tooltip_pos = imgui_cursor + ImVec2{0, glyph_size().y};
                            }
                            _tooltip_last_hovered_at = now;
                        }
                    }
                }

                if (font_pushed) {
                    ImGui::PopFont();
                }

                imgui_cursor.x += text_size.x;
                column += data.size();
            }
        }

        // Render cursor
        if (ImGui::IsWindowFocused() || _tooltip_has_focus || _always_show_cursors) {
            for (cursor &c: _cursors) {
                if (i == c.coord.line) {
                    auto time = std::chrono::duration_cast<std::chrono::milliseconds>(
                            std::chrono::system_clock::now().time_since_epoch());
                    time %= 1500;

                    if (time.count() > 400) {
                        auto x = _imgui_cursor_position.x +
                                 static_cast<float>(column_count_to(c.coord)) * space_length.x + extra_padding;
                        draw_list->AddLine(
                                {x, imgui_cursor.y + 2}, {x, imgui_cursor.y + space_length.y - 2},
                                _style.cursor_color
                        );
                    }
                }
            }
        }

        imgui_cursor.x = _imgui_cursor_position.x;
        imgui_cursor.y += ImGui::GetTextLineHeightWithSpacing();
    }

    ImGui::EndChild();

    if (tooltip_this_frame == _tooltips.end() && std::chrono::system_clock::now() - _tooltip_last_hovered_at > _tooltip_grace_period) {
        _tooltip.reset();
    }

    if (_tooltip) {
        assert(*_tooltip != _tooltips.end());
        show_tooltip();
    } else {
        reset_current_tooltip();
    }

    if (_showing_breakpoint_window) {
        show_breakpoint_window();
    }

    if (_default_font != nullptr) {
        ImGui::PopFont();
    }
}

void ImEdit::editor::render_region_line(region r, unsigned int current_line, ImVec2 draw_region, ImVec2 imgui_cursor, ImColor fill, ImColor outline) noexcept {
    if (coordinates_eq(r.beg, r.end)) {
        return;
    }

    auto draw_list = ImGui::GetWindowDrawList();
    imgui_cursor.y -= (ImGui::GetTextLineHeightWithSpacing() - glyph_size().y) / 2;

    coordinates min, max;
    if (coordinates_lt(r.beg, r.end)) {
        min = r.beg;
        max = r.end;
    } else {
        min = r.end;
        max = r.beg;
    }

    std::optional<ImVec2> select_draw_start, select_draw_end;
    if (min.line == current_line) {
        if (max.line == current_line) {
            // selected = [column(min), column(max)]
            if (min.token != 0 || min.char_index != 0) {
                select_draw_start = {
                        imgui_cursor.x + static_cast<float>(column_count_to(min)) * glyph_size().x,
                        imgui_cursor.y
                };
            } else {
                select_draw_start = imgui_cursor;
            }
            select_draw_end = {
                    imgui_cursor.x + static_cast<float>(column_count_to(max)) * glyph_size().x,
                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()
            };
        } else {
            // selected = [column(min), end_of_line]
            if (min.token != 0 || min.char_index != 0) {
                select_draw_start = {
                        imgui_cursor.x + static_cast<float>(column_count_to(min)) * glyph_size().x,
                        imgui_cursor.y
                };
            } else {
                select_draw_start = imgui_cursor;
            }
            select_draw_end = {
                    imgui_cursor.x + draw_region.x,
                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()
            };
        }
    } else if (min.line < current_line) {
        if (max.line == current_line) {
            // selected = [start_of_line, column(max)]
            select_draw_start = imgui_cursor;
            select_draw_end = {
                    imgui_cursor.x + static_cast<float>(column_count_to(max)) * glyph_size().x,
                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()
            };
        } else if (max.line > current_line) {
            //selected = [start_of_line, end_of_line]
            select_draw_start = imgui_cursor;
            select_draw_end = {
                    imgui_cursor.x + draw_region.x,
                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()
            };
        }

    }

    if (select_draw_start) {
        assert(select_draw_end);
        draw_list->AddRectFilled(*select_draw_start, *select_draw_end, fill);
        draw_list->AddRect(*select_draw_start, *select_draw_end, outline);
    }
}

ImEdit::editor::editor(std::string id) :
        _lines{},
        _style{get_default_style()},
        _imgui_id{std::move(id)},
        _shortcuts{get_default_shortcuts()}
{
    _cursors.emplace_back();
}

void ImEdit::editor::move_cursors_up() {
    IMEDIT_CALL_PMC(move_cursors_up)

    clear_search();
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_up(cursor.coord, cursor.wanted_column);
    }
    _selections.clear();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}


void ImEdit::editor::move_cursors_down() {
    IMEDIT_CALL_PMC(move_cursors_down)

    clear_search();
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_down(cursor.coord, cursor.wanted_column);
    }
    _selections.clear();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}
void ImEdit::editor::move_cursors_left() {
    IMEDIT_CALL_PMC(move_cursors_left)

    clear_search();
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_left(cursor.coord);
        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selections.clear();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}
void ImEdit::editor::move_cursors_right() {
    IMEDIT_CALL_PMC(move_cursors_right)

    clear_search();
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_right(cursor.coord);
        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selections.clear();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}


ImEdit::coordinates ImEdit::editor::move_coordinates_up(coordinates coord, unsigned int wanted_column) const noexcept {
    if (coord.line == 0) {
        coord.char_index = 0;
        coord.token = 0;
        return coord;
    }

    coord = coordinates_for(wanted_column, coord.line - 1);
    return coord;
}


ImEdit::coordinates ImEdit::editor::move_coordinates_down(coordinates coord, unsigned int wanted_column) const noexcept {
    if (coord.line == _lines.size() - 1) {
        coord.token = _lines.back().tokens.size() - 1;
        coord.char_index = _lines.back().tokens.back().data.size();
        return coord;
    }

    coord = coordinates_for(wanted_column, coord.line + 1);
    return coord;
}

void ImEdit::editor::move_cursors_left_token() {
    IMEDIT_CALL_PMC(move_cursors_left_token)

    clear_search();
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_left_token(cursor.coord);
        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selections.clear();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}


void ImEdit::editor::move_cursors_right_token() {
    IMEDIT_CALL_PMC(move_cursors_right_token)

    clear_search();
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_right_token(cursor.coord);
        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selections.clear();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::move_cursors_to_beg() {
    IMEDIT_CALL_PMC(move_cursors_to_beg)

    clear_search();
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_begline(cursor.coord);
        cursor.wanted_column = 0;
    }
    _selections.clear();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::move_cursors_to_end() {
    IMEDIT_CALL_PMC(move_cursors_to_end)

    clear_search();
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_endline(cursor.coord);
        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selections.clear();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::move_cursors_to_endfile() {
    IMEDIT_CALL_PMC(move_cursors_to_endfile)

    clear_search();
    _cursors.clear();
    _selections.clear();
    cursor c;
    if (!_lines.empty()) {
        if (_lines.back().tokens.empty()) {
            c.coord = {static_cast<unsigned int>(_lines.size() - 1), 0, 0};
        } else {
            c.coord = {
                    static_cast<unsigned int>(_lines.size() - 1),
                    static_cast<unsigned int>(_lines.back().tokens.size() - 1),
                    static_cast<unsigned int>(_lines.back().tokens.back().data.size())};
            c.wanted_column = column_count_to(c.coord);
        }
    }
    _cursors.emplace_back(c);

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::move_cursors_to_begfile() {
    IMEDIT_CALL_PMC(move_cursors_to_begfile)

    clear_search();
    _cursors.clear();
    _selections.clear();
    _cursors.emplace_back();

    IMEDIT_RESTORE_PMC
}


ImEdit::coordinates ImEdit::editor::move_coordinates_left(coordinates coord) const noexcept {
    if (coord.char_index > 0) {
        while (coord.char_index-- > 0 &&  is_within_utf8(_lines[coord.line].tokens[coord.token].data[coord.char_index]));
        return coord;
    }

    if (coord.token > 0) {
        --coord.token;

        const auto& sz = _lines[coord.line].tokens[coord.token].data.size();
        if (sz == 0) {
            coord.char_index = 0;
        } else {
            coord.char_index = sz;
            while (coord.char_index-- > 0 &&  is_within_utf8(_lines[coord.line].tokens[coord.token].data[coord.char_index]));
        }
        return coord;
    }

    if (coord.line > 0) {
        --coord.line;
        if (_lines[coord.line].tokens.empty()) {
            coord.token = 0;
            coord.char_index = 0;
        } else {
            coord.token = _lines[coord.line].tokens.size() - 1;
            coord.char_index = _lines[coord.line].tokens.back().data.size();
        }
        return coord;
    }

    return coord;
}

ImEdit::coordinates ImEdit::editor::move_coordinates_right(coordinates coord) const noexcept {
    if (_lines[coord.line].tokens.empty()) {
        if (coord.line + 1 < _lines.size()) {
            ++coord.line;
        }
        return coord;
    }

    if (coord.char_index < _lines[coord.line].tokens[coord.token].data.size()) {
        coord.char_index += char_count_for_utf8(_lines[coord.line].tokens[coord.token].data[coord.char_index]);
        return coord;
    }

    // End of token: moving to next token, after first glyph
    if (coord.token < _lines[coord.line].tokens.size() - 1) {
        ++coord.token;
        if (_lines[coord.line].tokens[coord.token].data.empty()) {
            coord.char_index = 0;
        }
        else {
            coord.char_index = char_count_for_utf8(_lines[coord.line].tokens[coord.token].data.front());
        }
        return coord;
    }

    if (coord.line < _lines.size() - 1) {
        ++coord.line;
        coord.token = 0;
        coord.char_index = 0;
        return coord;
    }
    return coord;
}

ImEdit::coordinates ImEdit::editor::move_coordinates_left_token(coordinates co) const noexcept {
    if (co.char_index > 0) {
        co.char_index = 0;
    }
    else if (co.token > 0) {
        --co.token;
        // cursor.coord.char_index == 0
    }
    else {
        co = move_coordinates_left(co); // just one left: previous line
    }

    return co;
}

ImEdit::coordinates ImEdit::editor::move_coordinates_right_token(coordinates co) const noexcept {
    if (_lines[co.line].tokens.empty()) { // NOLINT(*-branch-clone)
        co = move_coordinates_right(co);
    }
    else if (auto char_count = _lines[co.line].tokens[co.token].data.size() ; co.char_index < char_count) {
        co.char_index = char_count;
    }
    else if (co.token + 1 < _lines[co.line].tokens.size()) {
        ++co.token;
        co.char_index = _lines[co.line].tokens[co.token].data.size();
    }
    else {
        co = move_coordinates_right(co); // just one right: next line
    }

    return co;
}

ImEdit::coordinates ImEdit::editor::move_coordinates_endline(coordinates co) const noexcept { // NOLINT(*-convert-member-functions-to-static)
    if (!_lines[co.line].tokens.empty()) {
        co.token = _lines[co.line].tokens.size() - 1;
        co.char_index = _lines[co.line].tokens.back().data.size();
    }
    return co;
}

// Moves to first non-blank token if co wasn’t already there, and to actual beginning of line otherwise
ImEdit::coordinates ImEdit::editor::move_coordinates_begline(coordinates co) const noexcept {
    if (!_lines[co.line].tokens.empty()) {
        const auto& tokens = _lines[co.line].tokens;
        unsigned int first_non_blank_idx = 0;
        for (;first_non_blank_idx < tokens.size() && tokens[first_non_blank_idx].type == token_type::blank ; ++first_non_blank_idx);

        if (first_non_blank_idx != tokens.size() &&
                (co.token > first_non_blank_idx || co.token == first_non_blank_idx && co.char_index > 0)) {
            co.token = first_non_blank_idx;
            co.char_index = 0;
        } else {
            co.token = 0;
            co.char_index = 0;
        }
    }
    return co;
}

void ImEdit::editor::selection_toggle_right() {
    IMEDIT_CALL_PMC(selection_toggle_right)

    for (cursor& c : _cursors) {
        auto begin = c.coord;
        auto end = move_coordinates_right(c.coord);
        c.coord = end;
        c.wanted_column = column_count_to(c.coord);
        toggle_selection({begin, end});
    }
    sanitize_selections();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::selection_toggle_left() {
    IMEDIT_CALL_PMC(selection_toggle_left)

    for (cursor& c : _cursors) {
        auto begin = move_coordinates_left(c.coord);
        auto end = c.coord;
        c.coord = begin;
        c.wanted_column = column_count_to(c.coord);
        toggle_selection({begin, end});
    }
    sanitize_selections();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::selection_toggle_right_token() {
    IMEDIT_CALL_PMC(selection_toggle_right_token)

    for (cursor& c : _cursors) {
        auto begin = c.coord;
        auto end = move_coordinates_right_token(c.coord);
        c.coord = end;
        c.wanted_column = column_count_to(c.coord);
        toggle_selection({begin, end});
    }
    sanitize_selections();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::selection_toggle_left_token() {
    IMEDIT_CALL_PMC(selection_toggle_left_token)

    for (cursor& c : _cursors) {
        auto begin = move_coordinates_left_token(c.coord);
        auto end = c.coord;
        c.coord = begin;
        c.wanted_column = column_count_to(c.coord);
        toggle_selection({begin, end});
    }
    sanitize_selections();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::selection_toggle_up() {
    IMEDIT_CALL_PMC(selection_toggle_up)

    for (cursor& c : _cursors) {
        auto begin = move_coordinates_up(c.coord, c.wanted_column);
        auto end = c.coord;
        c.coord = begin;
        toggle_selection({begin, end});
    }
    sanitize_selections();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::selection_toggle_down() {
    IMEDIT_CALL_PMC(selection_toggle_down)

    for (cursor& c : _cursors) {
        auto begin = c.coord;
        auto end = move_coordinates_down(c.coord, c.wanted_column);
        c.coord = end;
        toggle_selection({begin, end});
    }
    sanitize_selections();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::selection_begline() {
    IMEDIT_CALL_PMC(selection_begline)

    for (cursor& c : _cursors) {
        auto begin = move_coordinates_begline(c.coord);
        auto end = c.coord;
        c.coord = begin;
        c.wanted_column = 0;
        toggle_selection({begin, end});
    }
    sanitize_selections();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::selection_endline() {
    IMEDIT_CALL_PMC(selection_endline)

    for (cursor& c : _cursors) {
        auto begin = c.coord;
        auto end = move_coordinates_endline(c.coord);
        c.coord = end;
        c.wanted_column = column_count_to(c.coord);
        toggle_selection({begin, end});
    }
    sanitize_selections();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::selection_begfile() {
    IMEDIT_CALL_PMC(selection_begfile)

    coordinates max = {0,0,0};
    for (const cursor& c : _cursors) {
        if (coordinates_lt(max, c.coord)) {
            max = c.coord;
        }
    }
    _cursors.clear();
    _cursors.emplace_back();
    _selections.clear();
    _selections.push_back({{0,0,0}, max});

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::selection_endfile() {
    IMEDIT_CALL_PMC(selection_endfile)

    coordinates end_coord;
    if (!_lines.empty()) {
        if (_lines.back().tokens.empty()) {
            end_coord = {static_cast<unsigned int>(_lines.size() - 1), 0, 0};
        } else {
            end_coord = {
                    static_cast<unsigned int>(_lines.size() - 1),
                    static_cast<unsigned int>(_lines.back().tokens.size() - 1),
                    static_cast<unsigned int>(_lines.back().tokens.back().data.size())};
        }
    } else {
        IMEDIT_RESTORE_PMC
        return; // nothing to select
    }
    auto min = end_coord;
    for (const cursor& c : _cursors) {
        if (coordinates_lt(c.coord, min)) {
            min = c.coord;
        }
    }
    _cursors.clear();
    _cursors.push_back({end_coord, column_count_to(end_coord)});
    _selections.clear();
    _selections.push_back({min, end_coord});

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::toggle_selection(ImEdit::region r) {
    if (coordinates_eq(r.beg, r.end)) {
        return;
    }
    assert(coordinates_lt(r.beg, r.end));

    for (unsigned int i = 0 ; i < _selections.size() ; ++i) {
        // selection with mouse grabbing does not ensure .beg < .end
        auto r2 = sorted_region(_selections[i]);

        if (coordinates_within_ex(r.beg, r2) || coordinates_within_ex(r.end, r2)) {
            if (coordinates_eq(r2.end, r.end)) {
                greater_coordinates_of(_selections[i]) = r.beg;
            }
            else if (coordinates_eq(r2.beg, r.beg)) {
                smaller_coordinates_of(_selections[i]) = r.end;
            }
            else {
                if (coordinates_lt(r.beg, r2.beg)) {
                    smaller_coordinates_of(_selections[i]) = r.beg;
                }
                if (coordinates_lt(r2.end, r.end)) {
                    greater_coordinates_of(_selections[i]) = r.end;
                }
            }
            return;
        }
        else if (coordinates_eq(r.beg, r2.beg)) {
            if (coordinates_eq(r.end, r2.end)) {
                _selections.erase(std::next(_selections.begin(), i));
                return;
            }
            else {
                smaller_coordinates_of(_selections[i]) = r.end;
                return;
            }
        }
        else if (coordinates_eq(r.end, r2.end)) {
            greater_coordinates_of(_selections[i]) = r.beg;
            return;
        }
        else if (coordinates_eq(r.end, r2.beg)) {
            smaller_coordinates_of(_selections[i]) = r.beg;
            return;
        }
    }

    // no matching selection : create a new one
    _selections.push_back(r);
}

unsigned int ImEdit::editor::column_count_to(ImEdit::coordinates coord) const noexcept {
    if (coord.token == 0 && coord.char_index == 0) {
        return 0;
    }

    assert(coord.line < _lines.size());
    if (coord.token == 0 && _lines[coord.line].tokens.empty()) {
        return 0;
    }

    assert(coord.token < _lines[coord.line].tokens.size());
    assert(coord.char_index <= _lines[coord.line].tokens[coord.token].data.size());

    unsigned int glyph_count = 0;
    for (unsigned int i = 0 ; i < coord.token; ++i) {
        const token& tok = _lines[coord.line].tokens[i];

        if (tok.type == token_type::blank) {
            for (auto c : tok.data) {
                if (c == '\t') {
                    glyph_count += _tab_length - glyph_count % _tab_length;
                } else {
                    ++glyph_count;
                }
            }
        } else {
            glyph_count += utf8_string_size(_lines[coord.line].tokens[i].data);
        }
    }

    const token& tok = _lines[coord.line].tokens[coord.token];
    if (tok.type == token_type::blank) {
        for (unsigned int i = 0 ; i < coord.char_index ; ++i) {
            if (tok.data[i] == '\t') {
                glyph_count += _tab_length - glyph_count % _tab_length;
            } else {
                ++glyph_count;
            }
        }
    } else {
        glyph_count += utf8_string_size(tok.data.substr(0, coord.char_index));
    }
    return glyph_count;
}

ImEdit::coordinates ImEdit::editor::coordinates_for(unsigned int column_count, unsigned int line) const noexcept {
    assert(line < _lines.size());

    coordinates coord;
    coord.line = line;
    coord.token = 0;
    coord.char_index = 0;

    if (_lines[coord.line].tokens.empty()) {
        return coord;
    }

    auto is_last_glyph = [this](coordinates coord) {
        return coord.token == _lines[coord.line].tokens.size() - 1
                && coord.char_index == _lines[coord.line].tokens[coord.token].data.size();
    };

    while (column_count_to(coord) < column_count && !is_last_glyph(coord)) {
        coord = move_coordinates_right(coord);
    }

    auto& str = _lines[coord.line].tokens[coord.token].data;
    if (coord.char_index != 0 && str[coord.char_index - 1] == '\t') {
        auto actual_tab_length = _tab_length - column_count % _tab_length;
        if (actual_tab_length != _tab_length && actual_tab_length >= _tab_length / 2) {
            while (coord.char_index-- > 0 &&  is_within_utf8(_lines[coord.line].tokens[coord.token].data[coord.char_index]));
        }
    }

    return coord;
}

void ImEdit::editor::sanitize_cursors() {
    // Delete duplicate cursors
    bool delete_performed;
    do {
        delete_performed = false;


        // FIXME: why restart the loops at 0 each time instead of resuming from index?
        auto to_erase = _cursors.end();
        for (auto it = _cursors.begin() ; it != _cursors.end() && to_erase == _cursors.end() ; ++it) {
            for (auto it2 = std::next(it) ; it2 != _cursors.end() ; ++it2) {
                if (coordinates_eq(it->coord, it2->coord)) {
                    to_erase = it;
                    break;
                }
            }
        }

        if (to_erase != _cursors.end()) {
            delete_performed = true;
            _cursors.erase(to_erase);
        }

    } while (delete_performed);


    // Delete cursors that are inside a selection
    do {
        delete_performed = false;


        // FIXME: why restart the loops at 0 each time instead of resuming from index?
        auto to_erase = _cursors.end();
        for (auto it = _cursors.begin() ; it != _cursors.end() && to_erase == _cursors.end() ; ++it) {
            for (const auto& select : _selections) {
                if (coordinates_within_ex(it->coord, select)) {
                    to_erase = it;
                }
            }
        }

        if (to_erase != _cursors.end()) {
            delete_performed = true;
            _cursors.erase(to_erase);
        }

    } while (delete_performed);
    assert(!_cursors.empty());
}

void ImEdit::editor::clear_cursors_within_selections() {
    for (const region& select : _selections) {
        unsigned int idx = 0;
        while (idx < _cursors.size()) {
            if (coordinates_within_ex(_cursors[idx].coord, select)) {
                _cursors.erase(std::next(_cursors.begin(), idx));
            } else {
                ++idx;
            }
        }
    }
    assert(!_cursors.empty());
}

void ImEdit::editor::sanitize_selections() {
    // Merging selections first.
    // TODO: Test me (all branches)
    bool perform_merge;
    do {
        perform_merge = false;

        unsigned int first = _selections.size();
        unsigned int second = first;
        for (unsigned int i = 0; i < _selections.size() && !perform_merge; ++i) {
            for (unsigned int j = 0; j < _selections.size(); ++j) {
                if (i == j) {
                    continue;
                }

                if (coordinates_within(_selections[j].beg, _selections[i])) {
                    perform_merge = true; // merge to be performed;
                    first = i;
                    second = j;
                    break;
                }
            }
        }

        if (perform_merge) {
            if (coordinates_lt_eq(_selections[first].end, _selections[second].end)) {
                _selections[first].end = _selections[second].end;
            } else {
                _selections[second].end = _selections[first].end;
            }
            _selections.erase(std::next(_selections.begin(), second));
        }

    } while (perform_merge);

    for (unsigned int i = 0 ; i < _selections.size() ; ++i) {
        bool delete_it = true;
        if (!coordinates_eq(_selections[i].beg, _selections[i].end)) {
            for (const cursor &c: _cursors) {
                if (coordinates_eq(c.coord, _selections[i].beg) || coordinates_eq(c.coord, _selections[i].end)) {
                    delete_it = false;
                    break;
                }
            }
        }
        if (delete_it) {
            _selections.erase(std::next(_selections.begin(), i));
            --i;
        }
    }
}

void ImEdit::editor::add_cursor(coordinates coords) {
    assert(coords.line < _lines.size());
    assert(coords.token < _lines[coords.line].tokens.size());
    assert(coords.char_index <= _lines[coords.line].tokens[coords.token].data.size());

    _cursors.push_back({coords, column_count_to(coords)});
    sanitize_cursors();
}

void ImEdit::editor::remove_cursor(coordinates coords) {
    auto cursor_it = _cursors.end();
    for (auto it = _cursors.begin() ; it != _cursors.end() ; ++it) {
        if (coordinates_eq(coords, it->coord)) {
            cursor_it = it;
            break;
        }
    }

    if (cursor_it != _cursors.end()) {
        _cursors.erase(cursor_it);
        sanitize_selections();
        if (_cursors.empty()) {
            _cursors.emplace_back();
        }
    }
}

bool ImEdit::editor::has_cursor(ImEdit::coordinates coords) {
    return std::any_of(_cursors.begin(), _cursors.end(), [this, &coords](const cursor& c) {
        return coordinates_eq(coords, c.coord);
    });
}

ImEdit::coordinates ImEdit::editor::mouse_position() {
    coordinates_cbl cbl = screen_to_token_coordinates(ImGui::GetMousePos());
    if (cbl.is_left) {
        cbl.token = cbl.char_index = 0;
    }
    return cbl.as_default_coords();
}

void ImEdit::editor::set_line_color(unsigned int line, std::optional<ImColor> color) noexcept {
    assert(line < _lines.size());
    _lines[line].background = color;
}


void ImEdit::editor::clear() {
    _cursors.clear();
    _tooltips.clear();
    _lines.clear();
    _selections.clear();
    _tooltips.clear();
    _last_frame_mouse_coords.reset();
    _glyph_size.reset();
    _longest_line_idx = 0;
    _longest_line_px = 0;
    reset_current_tooltip();

    _cursors.emplace_back();
}

void ImEdit::editor::find_longest_line() {
    _longest_line_idx = 0;
    _longest_line_px = 0;
    auto space_size = glyph_size().x;
    for (unsigned int i = 0 ; i < _lines.size() ; ++i) {
        float length = calc_line_size(i);
        if (length > _longest_line_px) {
            _longest_line_px = length;
            _longest_line_idx = i;
        }
    }
}

float ImEdit::editor::calc_line_size(unsigned int line) const noexcept {
    if (_lines[line].tokens.empty()) {
        return 0;
    }

    auto space_size = glyph_size().x;

    coordinates c;
    c.line = line;
    c.token = _lines[line].tokens.size() - 1;
    c.char_index = _lines[line].tokens.back().data.size();

    return static_cast<float>(column_count_to(c)) * space_size;
}

void ImEdit::editor::handle_kb_input() {
    if (!_allow_keyboard_input) {
        return;
    }

    if (!ImGui::IsWindowFocused()) {
        return;
    }

    // TODO notify lexer, also with previous data.

    ImGuiIO& im_io = ImGui::GetIO();
    im_io.WantCaptureKeyboard = true;
    im_io.WantTextInput = true;

    const bool alt = im_io.ConfigMacOSXBehaviors ? im_io.KeyCtrl : im_io.KeyAlt;
    const bool ctrl = im_io.ConfigMacOSXBehaviors ? im_io.KeyAlt : im_io.KeyCtrl;
    const bool shift = im_io.KeyShift;
    const bool super = im_io.KeySuper;

    input::modifiers mod_flags = input::none;
    mod_flags = static_cast<input::modifiers>((shift << 0) | (ctrl << 1) | (alt << 2) | (super << 3));




    bool shortcut_triggered = false;
    for (const auto& shortcut : _shortcuts) {

        if (mod_flags == shortcut.first.mod_flag) {
            bool all_key_pressed = true;
            for (ImGuiKey k: shortcut.first.keys) {
                if (!ImGui::IsKeyPressed(k)) {
                    all_key_pressed = false;
                    break;
                }
            }

            if (all_key_pressed) {
                shortcut_triggered = true;
                shortcut.second(_shortcuts_data, *this);
            }
        }
    }

    if (shortcut_triggered) {
        return;
    }

    if (!im_io.InputQueueCharacters.empty()) {
        delete_selections();
        for (ImWchar c : im_io.InputQueueCharacters) {
            input_char_utf16(c);
        }
        im_io.InputQueueCharacters.resize(0); // resize(0) makes it so that we keep the memory buffer instead of discarding it
    }
}

void ImEdit::editor::delete_glyph(coordinates co) {
    // TODO call lexer, also with previous data
    // nothing to delete
    if (co.token == 0 && co.char_index == 0 && co.line == 0) {
        return;
    }

    assert(co.line < _lines.size());
    // deleting empty line
    if (_lines[co.line].tokens.empty()) {
        _lines.erase(_lines.begin() + co.line);
        for (cursor& c : _cursors) {
            if (c.coord.line > co.line) {
                --c.coord.line;

            } else if (c.coord.line == co.line) {
                --c.coord.line;
                if (_lines[c.coord.line].tokens.empty()) {
                    c.coord.token = 0;
                    c.coord.char_index = 0;
                } else {
                    c.coord.token = _lines[c.coord.line].tokens.size() - 1;
                    c.coord.char_index = _lines[c.coord.line].tokens.back().data.size();
                }
            }
        }
        return;
    }


    assert(co.token < _lines[co.line].tokens.size());
    assert(co.char_index <= _lines[co.line].tokens[co.token].data.size());

    // Deleting an equivalent of '\n'
    if (co.token == 0 && co.char_index == 0) {
        const std::size_t original_line_tok_count = _lines[co.line - 1].tokens.size();
        for (token &tok: _lines[co.line].tokens) {
            _lines[co.line - 1].tokens.emplace_back(std::move(tok));
        }
        _lines.erase(_lines.begin() + co.line);

        for (cursor &c: _cursors) {
            if (c.coord.line == co.line) {
                --c.coord.line;
                c.coord.token += original_line_tok_count;
            } else if (c.coord.line > co.line) {
                --c.coord.line;
            }
        }

        auto length = calc_line_size(co.line - 1);
        if (length > _longest_line_px) {
            _longest_line_px = length;
            _longest_line_idx = co.line - 1;
        }
    }
    else {
    // Deleting a regular glyph

        const auto deleted_coord = co;
        if (co.char_index == 0) {
            assert(co.token > 0);
            --co.token;
            co.char_index = _lines[co.line].tokens[co.token].data.size();
            assert(co.char_index != 0 && "Sumbled upon an empty token. This should never happen, please check your lexer.");
        }

        auto& str = _lines[co.line].tokens[co.token].data;
        while (co.char_index-- > 0 &&  is_within_utf8(str[co.char_index]));
        const auto char_count = char_count_for_utf8(str[co.char_index]);
        str.erase(co.char_index, char_count_for_utf8(str[co.char_index]));

        for (cursor& cursor : _cursors) {
            if (cursor.coord.line == deleted_coord.line && cursor.coord.token == deleted_coord.token) {
                if (cursor.coord.char_index >= deleted_coord.char_index && cursor.coord.char_index != 0) {
                    cursor.coord.char_index -= char_count;
                }
            }
        }

        // Deleting token if empty
        if (_lines[co.line].tokens[co.token].data.empty()) {
            _lines[co.line].tokens.erase(_lines[co.line].tokens.begin() + co.token);

            for (cursor& cursor : _cursors) {
                if (cursor.coord.line == co.line) {
                    if (cursor.coord.token == co.token) {
                        // Moving cursor away from current token (that was deleted) while keeping it at the same position
                        if (cursor.coord.token != 0 || cursor.coord.char_index != 0) {
                            cursor.coord = move_coordinates_right(move_coordinates_left(cursor.coord));
                        }

                    } else if (cursor.coord.token > co.token) {
                        --cursor.coord.token;
                    }
                }
            }
        }

        if (co.line == _longest_line_idx) {
            find_longest_line();
        }
    }

    sanitize_cursors();

}

void ImEdit::editor::handle_mouse_input() {
    if (!_allow_mouse_input || !ImGui::IsItemHovered()) {
        return;
    }

    auto mouse_coord = screen_to_token_coordinates(ImGui::GetMousePos());

    if (!mouse_coord.is_left) {
        ImGui::SetMouseCursor(ImGuiMouseCursor_TextInput);
    }

    if (mouse_coord.is_left) {
        mouse_coord.token = mouse_coord.char_index = 0;
    }

    if (ImGui::IsMouseDragging(ImGuiMouseButton_Left, 0.1f) && _last_frame_mouse_coords && !_last_frame_mouse_coords->is_left) {
        coordinates coord = mouse_coord.as_default_coords();

        // left click dragging = update selection
        if (_selections.empty()) {
            _selections.emplace_back();
            coordinates previous_coord = _last_frame_mouse_coords->as_default_coords();
            _selections.back().beg = previous_coord;
            _selections.back().end = coord;
        }
        else {
            _selections.back().end = coord;
        }

        _cursors.clear();
        _cursors.push_back({coord, column_count_to(coord)});
    }

    ImGuiIO &im_io = ImGui::GetIO();
    const bool alt = im_io.ConfigMacOSXBehaviors ? im_io.KeyCtrl : im_io.KeyAlt;
    const bool ctrl = im_io.ConfigMacOSXBehaviors ? im_io.KeyAlt : im_io.KeyCtrl;
    const bool shift = im_io.KeyShift;


    if (ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left)) {
        _showing_breakpoint_window = false;

        if (!mouse_coord.is_left) {
            coordinates coord = mouse_coord.as_default_coords();
            coordinates sel_beg = coord;
            sel_beg.char_index = 0;
            coordinates sel_end = coord;
            sel_end.char_index = _lines[sel_end.line].tokens[sel_end.token].data.size();

            cursor cursor{sel_end, column_count_to(sel_end)};
            if (!ctrl) {
                _selections.clear();
                _selections.push_back({sel_beg, sel_end});
                _cursors.clear();
                _cursors.emplace_back(cursor);
            } else {
                _selections.push_back({sel_beg, sel_end});

                bool cursor_exists{false};
                for (const auto &c: _cursors) {
                    if (coordinates_eq(c.coord, cursor.coord)) {
                        cursor_exists = true;
                        break;
                    }
                }
                if (!cursor_exists) {
                    _cursors.emplace_back(cursor);
                }
            }
            sanitize_selections();
            clear_cursors_within_selections();
        }
    }
    else if (ImGui::IsMouseClicked(ImGuiMouseButton_Left)) {
        _showing_breakpoint_window = false;

        if (is_mouse_in_breakpoint_column()) {
            if (mouse_coord.line < _lines.size()) {
                _lines[mouse_coord.line].has_breakpoint = !_lines[mouse_coord.line].has_breakpoint;
                _breakpoint_toggled(_breakpoint_data, mouse_coord.line, *this);
            }
        }
        else if (!ctrl) {
            coordinates coord = mouse_coord.as_default_coords();

            _selections.clear();
            _cursors.clear();
            _cursors.push_back({coord, column_count_to(coord)});
        }
        else {
            coordinates coord = mouse_coord.as_default_coords();

            auto it = std::find_if(_cursors.begin(), _cursors.end(), [this, &coord](const cursor &cursor) {
                return coordinates_eq(cursor.coord, coord);
            });
            if (it == _cursors.end()) {
                bool is_within_selection = false;
                for (const auto& r : _selections) {
                    if (coordinates_within_ex(coord, r)) {
                        is_within_selection = true;
                        break;
                    }
                }
                if (!is_within_selection) {
                    _cursors.push_back({coord, column_count_to(coord)});
                }
            } else if (_cursors.size() > 1) { // don’t erase last cursor
                _cursors.erase(it);
                sanitize_selections();
            }
        }
    }
    else if (ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
        if (_showing_breakpoint_window) {
            _showing_breakpoint_window = false;
        }
        else if (is_mouse_in_breakpoint_column()) {
            if (mouse_coord.line < _lines.size()) {
                _selected_breakpoint_line = mouse_coord.line;
                _showing_breakpoint_window = true;
                _breakpoint_window_just_opened = true;
            }
        }
    }

    _last_frame_mouse_coords = mouse_coord;
}

ImEdit::coordinates_cbl ImEdit::editor::screen_to_token_coordinates(ImVec2 pos) const {
    // TODO what about folded regions?
    auto glyph_size = editor::glyph_size();
    coordinates_cbl coords;
    pos.x -= _imgui_cursor_position.x;
    pos.y -= _imgui_cursor_position.y;

    pos.x -= compute_extra_padding();

    auto line = std::min(static_cast<unsigned>(pos.y / ImGui::GetTextLineHeightWithSpacing()),
                         static_cast<unsigned>(_lines.size() - 1));
    coords.line = line;

    if (pos.x < 0) {
        coords.is_left = true;
        coords.token = 0;
        coords.char_index = static_cast<unsigned int>(std::round(-pos.x / glyph_size.x));
        return coords;
    }
    auto base_coords = coordinates_for(static_cast<unsigned>(std::round(pos.x / glyph_size.x)), line);


    coords.token = base_coords.token;
    coords.char_index = base_coords.char_index;

    return coords;
}

// Padding for line numbers, breakpoints, and such.
float ImEdit::editor::compute_extra_padding() const noexcept {
    auto glyph_length = glyph_size().x;
    return _lines.empty() ? 5 * glyph_length : std::floor(std::log10(static_cast<float>(_lines.size())) + 4) * glyph_length;
}

bool ImEdit::editor::coordinates_eq(coordinates lhs, coordinates rhs) const noexcept {
    if (lhs.line != rhs.line) {
        return false;
    }

    if (lhs.token == rhs.token) {
        if (lhs.char_index == rhs.char_index) {
            return true;
        }
        return false;

    } else {
        // adjacent tokens: if left token is the end of token, right is at idx 0 -> coordinates are still equal
        if (lhs.token > rhs.token) {
            std::swap(lhs, rhs);
        }

        if (lhs.token + 1 != rhs.token) {
            return false;
        }

        if (lhs.char_index == _lines[lhs.line].tokens[lhs.token].data.size() && rhs.char_index == 0) {
            return true;
        }
        return false;
    }
}

bool ImEdit::editor::coordinates_lt(coordinates lhs, coordinates rhs) const noexcept {
    if (lhs.line < rhs.line) {
        return true;
    }
    if (lhs.line > rhs.line) {
        return false;
    }

    // same line

    if (lhs.token < rhs.token) {
        // need to check if same position, lhs on the end of their token and rhs on the start of their token, with lhs next to rhs
        if (lhs.token + 1 != rhs.token) {
            return true;
        }
        else {
            if (lhs.char_index == _lines[lhs.line].tokens[lhs.token].data.size() && rhs.char_index == 0) {
                return false;
            }
            else {
                return true;
            }
        }
    }
    if (lhs.token > rhs.token) {
        return false;
    }

    // same token

    if (lhs.char_index < rhs.char_index) {
        return true;
    }
    else {
        return false;
    }
}

bool ImEdit::editor::coordinates_lt_eq(coordinates lhs, coordinates rhs) const noexcept {
    return coordinates_lt(lhs, rhs) || coordinates_eq(lhs, rhs);
}

ImVec2 ImEdit::editor::glyph_size() const noexcept {
    if (!_glyph_size) {
        _glyph_size = calc_text_size(" ");
    }
    return *_glyph_size;
}

void ImEdit::editor::add_selection(region r) noexcept {
    IMEDIT_CALL_PMC_CAPT(add_selection, r)

    _cursors.push_back({r.end, column_count_to(r.end)});
    if (coordinates_lt(r.end, r.beg)) {
        std::swap(r.beg, r.end);
    }
    _selections.emplace_back(r);

    IMEDIT_RESTORE_PMC
}

ImVec2 ImEdit::editor::calc_text_size(const char *text, const char *text_end) noexcept {
    auto font = ImGui::GetFont();
    return font->CalcTextSizeA(font->FontSize, FLT_MAX, -1.f, text, text_end, nullptr);
}

bool ImEdit::editor::coordinates_within(ImEdit::coordinates coord, ImEdit::region r) const noexcept {
    if (coordinates_lt(r.end, r.beg)) {
        std::swap(r.end, r.beg);
    }
    return coordinates_lt_eq(r.beg, coord) && coordinates_lt_eq(coord, r.end);
}

bool ImEdit::editor::coordinates_within_ex(ImEdit::coordinates coord, ImEdit::region r) const noexcept {
    if (coordinates_lt(r.end, r.beg)) {
        std::swap(r.end, r.beg);
    }
    return coordinates_lt(r.beg, coord) && coordinates_lt(coord, r.end);
}

ImEdit::region ImEdit::editor::sorted_region(region r) const noexcept {
    if (coordinates_lt(r.beg, r.end)) {
        return r;
    }
    return {r.end, r.beg};
}

ImEdit::coordinates& ImEdit::editor::greater_coordinates_of(region& r) const noexcept {
    if (coordinates_lt(r.beg, r.end)) {
        return r.end;
    }
    return r.beg;
}

ImEdit::coordinates& ImEdit::editor::smaller_coordinates_of(region& r) const noexcept {
    if (coordinates_lt(r.beg, r.end)) {
        return r.beg;
    }
    return r.end;
}

void ImEdit::editor::delete_selections() {
    IMEDIT_CALL_PMC(delete_selections)

    // TODO call lexer, also with previous data
    // FIXME deleting a selection while cursor is at the beginning causes bad cursor placement


    // Deleting last selection first so that we don’t have to move a bunch of coordinates.
    std::vector<region> sorted_selections = _selections;
    std::sort(sorted_selections.begin(), sorted_selections.end(), [this](const region& r1, const region& r2) {
        return coordinates_lt(r2.beg, r1.beg);
    });
    for (auto & select : sorted_selections) {

        const coordinates beg = smaller_coordinates_of(select);
        const coordinates end = greater_coordinates_of(select);

        // FIXME move cursors if needed
        std::vector<unsigned int> cursor_needs_move;
        for (unsigned int i = 0 ; i < _cursors.size() ; ++i) {
            if (coordinates_within(_cursors[i].coord, select)) {
                _cursors[i].coord = beg;
            }
            else if (coordinates_lt(end, _cursors[i].coord)) {
                cursor_needs_move.emplace_back(i);
            }
        }

        if (beg.line == end.line) {

            auto& line = _lines[beg.line];
            assert(!line.tokens.empty());
            if (beg.token == end.token) {
                bool token_deleted = false;
                line.tokens[beg.token].data.erase(beg.char_index, end.char_index - beg.char_index);
                if (line.tokens[beg.token].data.empty()) {
                    line.tokens.erase(std::next(line.tokens.begin(), beg.token));
                    token_deleted = true;
                }

                for (unsigned int idx : cursor_needs_move) {
                    if (_cursors[idx].coord.line == end.line && _cursors[idx].coord.token == end.token) {
                        _cursors[idx].coord.char_index -= (end.char_index - beg.char_index);
                    }
                }

                if (token_deleted) {
                    for (unsigned int idx : cursor_needs_move) {
                        if (_cursors[idx].coord.line == end.line && _cursors[idx].coord.token >= end.token) {
                            _cursors[idx].coord.token--;
                        }
                    }
                }

            } else {
                unsigned int deleted_token_count = end.token - beg.token - 1;

                line.tokens.erase(std::next(line.tokens.begin(), beg.token + 1),
                                  std::next(line.tokens.begin(), end.token));
                if (beg.char_index == 0) {
                    ++deleted_token_count;
                    line.tokens.erase(std::next(line.tokens.begin(), beg.token));

                    // deleting end token (may be a partial delete)
                    line.tokens[beg.token].data.erase(0, end.char_index);
                    if (line.tokens[beg.token].data.empty()) {
                        ++deleted_token_count;
                        line.tokens.erase(std::next(line.tokens.begin(), beg.token + 1));
                    }
                } else {
                    line.tokens[beg.token].data.erase(beg.char_index);

                    // deleting end token (may be a partial delete)
                    line.tokens[beg.token + 1].data.erase(0, end.char_index);
                    if (line.tokens[beg.token + 1].data.empty()) {
                        ++deleted_token_count;
                        line.tokens.erase(std::next(line.tokens.begin(), beg.token + 1));
                    }
                }



                for (unsigned int idx : cursor_needs_move) {
                    if (_cursors[idx].coord.line == end.line) {
                        if (_cursors[idx].coord.token == end.token) {
                            _cursors[idx].coord.token -= deleted_token_count;
                            _cursors[idx].coord.char_index -= end.char_index;
                        }
                        else {
                            _cursors[idx].coord.token -= deleted_token_count;
                        }
                    }
                }
            }

        } else {
            int extra_token_deleted = 0;
            {
                auto &beg_line = _lines[beg.line];
                if (!beg_line.tokens.empty()) {
                    if (beg.token + 1 < beg_line.tokens.size()) {
                        beg_line.tokens.erase(std::next(beg_line.tokens.begin(), beg.token + 1), beg_line.tokens.end());
                    }
                    if (beg.char_index == 0) {
                        ++extra_token_deleted;
                        beg_line.tokens.erase(std::next(beg_line.tokens.begin(), beg.token));
                    } else if (beg.char_index < beg_line.tokens[beg.token].data.size()) {
                        beg_line.tokens[beg.token].data.erase(beg.char_index);
                    }
                }
            } // beg_line invalidated after _lines.erase call

            _lines.erase(std::next(_lines.begin(), beg.line + 1), std::next(_lines.begin(), end.line));

            auto& end_line = _lines[beg.line + 1];
            end_line.tokens.erase(end_line.tokens.begin(), std::next(end_line.tokens.begin(), end.token));
            end_line.tokens.front().data.erase(0, end.char_index);
            if (end_line.tokens.front().data.empty()) {
                ++extra_token_deleted;
                end_line.tokens.erase(end_line.tokens.begin());
            }

            for (token& tok : end_line.tokens) {
                _lines[beg.line].tokens.emplace_back(std::move(tok));
            }
            _lines.erase(std::next(_lines.begin(), beg.line + 1));

            for (unsigned int idx : cursor_needs_move) {
                if (_cursors[idx].coord.line == end.line) {
                    _cursors[idx].coord.token += beg.token + 1;
                    _cursors[idx].coord.token -= end.token;
                    _cursors[idx].coord.token -= extra_token_deleted;
                }
                _cursors[idx].coord.line -= (end.line - beg.line);
            }
        }
    }
    _selections.clear();
    sanitize_cursors();

    IMEDIT_RESTORE_PMC
}

bool ImEdit::editor::has_match() {
    return !_regex_results.empty();
}

void ImEdit::editor::clear_search() {
    _regex_results.clear();
    _regex_results_index = 0;
}


bool ImEdit::editor::regex_search(const std::regex &regex, std::regex_constants::match_flag_type flags) {
    IMEDIT_CALL_PMC_CAPT(regex_search, regex)
    assert(!(flags & std::regex_constants::match_prev_avail) && "Invalid flag input");

    clear_search();
    for (auto match_iterator = std::regex_iterator(begin(), end(), regex, flags); match_iterator != std::regex_iterator<iterator>{} ; ++match_iterator) {
        const std::match_results<editor::iterator>& match = *match_iterator;
        _regex_results.push_back({match[0].first.get_coord(), match[0].second.get_coord()});
    }

    IMEDIT_RESTORE_PMC
    return has_match();
}

bool ImEdit::editor::select_next() {
    IMEDIT_CALL_PMC(select_next)
    if (!has_match()) {
        return false;
    }

    if (_regex_results_index == _regex_results.size()) {
        _regex_results_index = 0;
    }

    _selections.clear();
    _cursors.clear();
    _selections.push_back(_regex_results[_regex_results_index]);
    const auto& end = _regex_results[_regex_results_index].end;
    _cursors.push_back({end, column_count_to(end)});


    IMEDIT_RESTORE_PMC
    return ++_regex_results_index == _regex_results.size();
}

bool ImEdit::editor::select_previous() {
    IMEDIT_CALL_PMC(select_previous)
    if (!has_match()) {
        return false;
    }

    if (_regex_results_index == 0) {
        _regex_results_index = _regex_results.size();
    }
    --_regex_results_index;

    _selections.clear();
    _cursors.clear();
    _selections.push_back(_regex_results[_regex_results_index]);
    const auto& end = _regex_results[_regex_results_index].end;
    _cursors.push_back({end, column_count_to(end)});



    IMEDIT_RESTORE_PMC
    return _regex_results_index == 0;
}

void ImEdit::editor::select_all() {
    IMEDIT_CALL_PMC(select_previous)
    if (!has_match()) {
        return;
    }

    _selections.clear();
    _cursors.clear();
    for (region r : _regex_results) {
        _selections.push_back(r);
        _cursors.push_back({r.end, column_count_to(r.end)});
    }

    IMEDIT_RESTORE_PMC
}


void ImEdit::editor::input_char_utf16(ImWchar ch) {
    IMEDIT_CALL_PMC_CAPT(input_char_utf16, ch)


    clear_search();
    for (cursor& c : _cursors) {
        if (_lines[c.coord.line].tokens.empty()) {
            _lines[c.coord.line].tokens.push_back({"", token_type::unknown});
        }
        auto& string = _lines[c.coord.line].tokens[c.coord.token].data;

        // Credits to these if branches: Dear ImGui
        // convert utf16 to utf8
        // TODO: does this work when IMGUI_USE_WCHAR32 is defined?
        auto advance = 0;
        if (ch < 0x80) {
            string.insert(c.coord.char_index, 1, (char) ch);
            advance += 1;
        } else if (ch < 0x800) {
            string.insert(c.coord.char_index, 1, (char) (0xC0 + (ch >> 6)));
            string.insert(c.coord.char_index + 1, 1, (char) (0x80 + (ch & 0x3f)));
            advance += 2;
        } else if (ch >= 0xdc00 && ch < 0xe000) {
            // Do nothing
        } else if (ch >= 0xd800 && ch < 0xdc00) {
            string.insert(c.coord.char_index, 1, (char) (0xf0 + (ch >> 18)));
            string.insert(c.coord.char_index + 1, 1, (char) (0x80 + ((ch >> 12) & 0x3f)));
            string.insert(c.coord.char_index + 2, 1, (char) (0x80 + ((ch >> 6) & 0x3f)));
            string.insert(c.coord.char_index + 3, 1, (char) (0x80 + ((ch) & 0x3f)));
            advance += 4;
        } else //if (c < 0x10000)
        {
            string.insert(c.coord.char_index, 1, (char) (0xe0 + (ch >> 12)));
            string.insert(c.coord.char_index + 1, 1, (char) (0x80 + ((ch >> 6) & 0x3f)));
            string.insert(c.coord.char_index + 2, 1, (char) (0x80 + ((ch) & 0x3f)));
            advance += 3;
        }


        auto length = calc_line_size(c.coord.line);
        if (length > _longest_line_px) {
            _longest_line_px = length;
            _longest_line_idx = c.coord.line;
        }

        for (cursor& c2 : _cursors) {
            if (c.coord.line == c2.coord.line && c.coord.token == c2.coord.token && c.coord.char_index < c2.coord.char_index) {
                c2.coord.char_index += advance;
            }
        }
        c.coord.char_index += advance;
    }

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::input_raw_char(char ch) {
    IMEDIT_CALL_PMC_CAPT(input_raw_char, ch)


    clear_search();
    assert(ch != '\n' && "Call input_newline() instead");
    for (cursor& c : _cursors) {
        input_raw_char(ch, c);
    }

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::input_raw_char(char ch, ImEdit::cursor &pos) {
    assert(ch != '\n' && "Call input_newline() instead");
    if (_lines[pos.coord.line].tokens.empty()) {
        _lines[pos.coord.line].tokens.push_back({"", token_type::unknown});
    }
    _lines[pos.coord.line].tokens[pos.coord.token].data.insert(pos.coord.char_index, 1, ch);

    for (cursor& c2 : _cursors) {
        if (pos.coord.line == c2.coord.line && pos.coord.token == c2.coord.token && pos.coord.char_index <= c2.coord.char_index) {
            c2.coord.char_index++; // just inserted a char before that cursor
        }
    }
}

void ImEdit::editor::input_newline() {
    IMEDIT_CALL_PMC(input_newline)

    // TODO : call lexer, also with previous data
    clear_search();
    delete_selections();
    for (cursor& c : _cursors) {
        _lines.insert(std::next(_lines.cbegin(), c.coord.line + 1), line{});

        if (!_lines[c.coord.line].tokens.empty()) {
            auto& line = _lines[c.coord.line];
            if (c.coord.char_index < line.tokens[c.coord.token].data.size()) {
                auto& original = line.tokens[c.coord.token];
                token tok;
                tok.data = original.data.substr(c.coord.char_index);
                original.type = token_type::unknown;
                original.id = 0;
                line.tokens[c.coord.token].data.erase(c.coord.char_index);
                _lines[c.coord.line + 1].tokens.emplace_back(std::move(tok));

                if (line.tokens[c.coord.token].data.empty()) {
                    line.tokens.erase(std::next(line.tokens.begin(), c.coord.token));
                    --c.coord.token;
                }
            }
            while (c.coord.token + 1 < line.tokens.size()) {
                _lines[c.coord.line + 1].tokens.push_back(line.tokens[c.coord.token + 1]);
                line.tokens.erase(std::next(line.tokens.begin(), c.coord.token + 1));
            }
        }

        for (cursor& c2 : _cursors) {
            if (c2.coord.line > c.coord.line) {
                ++c2.coord.line;
            }
        }
        ++c.coord.line;
        c.coord.char_index = c.coord.token = c.wanted_column = 0;
    }

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::input_newline_nomove() {
    IMEDIT_CALL_PMC(input_newline)

    clear_search();
    for (cursor& c : _cursors) {
        _lines.insert(std::next(_lines.cbegin(), c.coord.line + 1), line{});
        for (cursor& c2 : _cursors) {
            if (c2.coord.line > c.coord.line) {
                ++c2.coord.line;
            }
        }
    }

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::input_delete() {
    IMEDIT_CALL_PMC(input_delete)

    clear_search();
    if (!_selections.empty()) {
        delete_selections();
    } else {
        for (cursor &cursor: _cursors) {
            const auto& co = cursor.coord;
            // Do nothing if we are after the last char of the last line
            if (co.line != _lines.size() - 1 || co.token != _lines.back().tokens.size() - 1 ||
                co.char_index != _lines.back().tokens.back().data.size()) {
                delete_glyph(move_coordinates_right(cursor.coord));
            }
        }
    }

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::input_backspace() {
    IMEDIT_CALL_PMC(input_backspace)

    clear_search();
    if (!_selections.empty()) {
        delete_selections();
    } else {
        for (cursor &cursor: _cursors) {
            delete_glyph(cursor.coord);
        }
    }

    IMEDIT_RESTORE_PMC
}


void ImEdit::editor::add_breakpoint(unsigned int line_no) noexcept {
    assert(line_no < _lines.size());
    _lines[line_no].has_breakpoint = true;
}

void ImEdit::editor::remove_breakpoint(unsigned int line_no) noexcept {
    assert(line_no < _lines.size());
    _lines[line_no].has_breakpoint = false;
}

bool ImEdit::editor::has_breakpoint(unsigned int line_no) noexcept {
    assert(line_no < _lines.size());
    return _lines[line_no].has_breakpoint;
}

void ImEdit::editor::show_tooltip() {

    if (_tooltip_chrono && std::chrono::system_clock::now() - *_tooltip_chrono > _tooltip_delay) {

        assert(_tooltip_pos);
        ImGui::SetNextWindowPos(*_tooltip_pos);
        ImGui::SetNextWindowFocus();
        if (ImGui::Begin("##tooltip", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_AlwaysAutoResize)) {
            std::visit([this](auto &&arg) {
                           using T = std::decay_t<decltype(arg)>;
                           if constexpr (std::is_same_v<T, std::string>) {
                               ImGui::Text("%s", arg.c_str());
                           } else if constexpr (std::is_same_v<T, std::function<void(void *,
                                                                                     const ImEdit::token &)>>) {
                               arg(_tooltip_data, (*_tooltip)->first);
                           } else {
                               static_assert(false, "Missing visitor branches");
                           }
                       },
                       (*_tooltip)->second);
            if (ImGui::IsWindowFocused()) {
                handle_kb_input();
                _tooltip_has_focus = true;
            } else {
                _tooltip_has_focus = false;
            }

            if (!ImGui::IsWindowAppearing() && !ImGui::IsWindowHovered(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem)) {
                if (std::chrono::system_clock::now() - _tooltip_last_hovered_at > _tooltip_grace_period) {
                    _tooltip.reset();
                }
            } else {
                _tooltip_last_hovered_at = std::chrono::system_clock::now();
            }
        }
        ImGui::End();
    }
}

void ImEdit::editor::show_breakpoint_window() {
    if (_breakpoint_window_just_opened) {
       ImGui::SetNextWindowPos(ImGui::GetMousePos() + ImVec2{5, 5});
       _breakpoint_window_just_opened = false;
    }
    if (ImGui::Begin("##breakpoint", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_AlwaysAutoResize)) {
        assert(_breakpoint_window_filler);
        _breakpoint_window_filler(_breakpoint_data, _selected_breakpoint_line, *this);
    }
    ImGui::End();
}

void ImEdit::editor::reset_current_tooltip() {
    _tooltip.reset();
    _tooltip_pos.reset();
    _tooltip_chrono.reset();
    _tooltip_last_hovered_at = std::chrono::system_clock::now() - _tooltip_grace_period - std::chrono::milliseconds(1);
    _tooltip_has_focus = false;
}

void ImEdit::editor::font_changed() const noexcept {
    _glyph_size.reset();
}

void ImEdit::editor::set_data(std::deque<line> lines) {
    clear();
    _lines = std::move(lines);
}

void ImEdit::editor::copy_to_clipboard() const {
    IMEDIT_CALL_PMC(copy_to_clipboard)

    std::ostringstream oss;
    bool is_first_selection{true};

    std::vector<region> sorted_selections = _selections;
    std::sort(sorted_selections.begin(), sorted_selections.end(), [this](const region& r1, const region& r2) {
        return coordinates_lt(r1.beg, r2.beg);
    });

    for (const region& select : sorted_selections) {
        if (coordinates_eq(select.beg, select.end)) {
            continue;
        }

        if (!is_first_selection) {
            oss << '\n';
        }

        region fixed{};
        if (coordinates_lt(select.beg, select.end)) {
            fixed = select;
        }
        else {
            fixed.beg = select.end;
            fixed.end = select.beg;
        }

        if (fixed.beg.line == fixed.end.line) {
            if (fixed.beg.token == fixed.end.token) {
                oss << _lines[fixed.beg.line].tokens[fixed.beg.token].data.substr(
                            fixed.beg.char_index,
                            fixed.end.char_index - fixed.beg.char_index
                        );
            } else {
                oss << _lines[fixed.beg.line].tokens[fixed.beg.token].data.substr(fixed.beg.char_index);
                for (unsigned int i = fixed.beg.token + 1; i < fixed.end.token; ++i) {
                    oss << _lines[fixed.beg.line].tokens[i].data;
                }
                oss << _lines[fixed.end.line].tokens[fixed.end.token].data.substr(0, fixed.end.char_index);
            }
        }
        else {
            if (!_lines[fixed.beg.line].tokens.empty()) {
                oss << _lines[fixed.beg.line].tokens[fixed.beg.token].data.substr(fixed.beg.char_index);
                for (unsigned int i = fixed.beg.token + 1; i < _lines[fixed.beg.line].tokens.size(); ++i) {
                    oss << _lines[fixed.beg.line].tokens[i].data;
                }
            }
            oss << '\n';
            for (unsigned int i = fixed.beg.line + 1; i < fixed.end.line; ++i) {
                for (const auto &tok: _lines[i].tokens) {
                    oss << tok.data;
                }
                oss << '\n';
            }
            if (!_lines[fixed.end.line].tokens.empty()) {
                for (unsigned int i = 0; i < fixed.end.token ; ++i) {
                    oss << _lines[fixed.end.line].tokens[i].data;
                }
                oss << _lines[fixed.end.line].tokens[fixed.end.token].data.substr(0, fixed.end.char_index);
            }
        }

        is_first_selection = false;
    }

    ImGui::SetClipboardText(oss.str().c_str());

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::paste_from_clipboard() {
    IMEDIT_CALL_PMC(paste_from_clipboard)

    delete_selections();
    std::string str = ImGui::GetClipboardText();

    auto nb_lines = std::count(str.begin(), str.end(), '\n') + 1;
    if (nb_lines == _cursors.size() && _cursors.size() != 1) {

        auto cursors = _cursors;
        std::sort(cursors.begin(), cursors.end(), [this](const cursor& lhs, const cursor& rhs) {
            return coordinates_lt(lhs.coord, rhs.coord);
        });

        auto cursor = cursors.begin();
        auto it = str.begin();
        for (unsigned int i = 0 ; i < nb_lines ; ++i, ++cursor) {
            while (*it != '\n' && *it != '\0') {
                input_raw_char(*it++, *cursor);
            }
            if (*it != '\0') {
                ++it;
            }
        }

    } else {
        auto it = str.begin();
        while (*it != '\0') {
            auto length = char_count_for_utf8(*it);
            bool compound = length > 1;
            while (--length) {
                input_raw_char(*it++);
                assert(*it != '\0');
            }

            if (!compound && *it == '\n') {
                input_newline();
            } else {
                input_raw_char(*it);
            }
            ++it;
        }
    }
    _longest_line_px = 0;

    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::cut_to_clipboard() {
    IMEDIT_CALL_PMC(cut_to_clipboard)
    copy_to_clipboard();
    delete_selections();
    IMEDIT_RESTORE_PMC
}

void ImEdit::editor::add_shortcut(input in, std::function<void(void *, editor &)> callback) {
    _shortcuts.emplace_back(std::move(in), std::move(callback));
}

void ImEdit::editor::add_shortcut(input in, void (editor::*member_function)()) {
    add_shortcut(std::move(in), [member_function](void*, editor& ed) {
                     (ed.*member_function)();
                 }
    );
}

void ImEdit::editor::add_shortcut(input in, void (editor::*member_function)() const) {
    add_shortcut(std::move(in), [member_function](void*, editor& ed) {
                     (ed.*member_function)();
                 }
    );
}

std::vector<std::pair<ImEdit::input, std::function<void(void *, ImEdit::editor &)>>> ImEdit::editor::get_default_shortcuts() {
    std::vector<std::pair<input, std::function<void(void *, editor &)>>> shortcuts;
    auto add_memb_fn = [&shortcuts](input in, auto fn) {
        shortcuts.emplace_back(std::move(in), [fn](void*, editor& ed) {
            (ed.*fn)();
        });
    };

    add_memb_fn({{ImGuiKey_Delete}, input::modifiers::none}, &editor::input_delete);
    add_memb_fn({{ImGuiKey_Enter}, input::modifiers::none}, &editor::input_newline);
    add_memb_fn({{ImGuiKey_Backspace}, input::modifiers::none}, &editor::input_backspace);
    add_memb_fn({{ImGuiKey_DownArrow}, input::modifiers::none}, &editor::move_cursors_down);
    add_memb_fn({{ImGuiKey_UpArrow}, input::modifiers::none}, &editor::move_cursors_up);
    add_memb_fn({{ImGuiKey_LeftArrow}, input::modifiers::none}, &editor::move_cursors_left);
    add_memb_fn({{ImGuiKey_RightArrow}, input::modifiers::none}, &editor::move_cursors_right);
    add_memb_fn({{ImGuiKey_End}, input::modifiers::none}, &editor::move_cursors_to_end);
    add_memb_fn({{ImGuiKey_Home}, input::modifiers::none}, &editor::move_cursors_to_beg);
    shortcuts.emplace_back(input{{ImGuiKey_Tab}, input::modifiers::none}, [](void*, editor& ed) {
        ed.input_char_utf16('\t');
    });

    add_memb_fn({{ImGuiKey_Enter}, input::modifiers::control}, &editor::input_newline_nomove);
    add_memb_fn({{ImGuiKey_DownArrow}, input::modifiers::control}, &editor::scroll_down_next_frame);
    add_memb_fn({{ImGuiKey_UpArrow}, input::modifiers::control}, &editor::scroll_up_next_frame);
    add_memb_fn({{ImGuiKey_LeftArrow}, input::modifiers::control}, &editor::move_cursors_left_token);
    add_memb_fn({{ImGuiKey_RightArrow}, input::modifiers::control}, &editor::move_cursors_right_token);
    add_memb_fn({{ImGuiKey_V}, input::modifiers::control}, &editor::paste_from_clipboard);
    add_memb_fn({{ImGuiKey_C}, input::modifiers::control}, &editor::copy_to_clipboard);
    add_memb_fn({{ImGuiKey_X}, input::modifiers::control}, &editor::cut_to_clipboard);
    add_memb_fn({{ImGuiKey_End}, input::modifiers::control}, &editor::move_cursors_to_endfile);
    add_memb_fn({{ImGuiKey_Home}, input::modifiers::control}, &editor::move_cursors_to_begfile);

    add_memb_fn({{ImGuiKey_LeftArrow}, input::modifiers::shift}, &editor::selection_toggle_left);
    add_memb_fn({{ImGuiKey_RightArrow}, input::modifiers::shift}, &editor::selection_toggle_right);
    add_memb_fn({{ImGuiKey_UpArrow}, input::modifiers::shift}, &editor::selection_toggle_up);
    add_memb_fn({{ImGuiKey_DownArrow}, input::modifiers::shift}, &editor::selection_toggle_down);
    add_memb_fn({{ImGuiKey_End}, input::shift}, &editor::selection_endline);
    add_memb_fn({{ImGuiKey_Home}, input::shift}, &editor::selection_begline);

    add_memb_fn({{ImGuiKey_LeftArrow}, input::modifiers(input::shift | input::control)},
                &editor::selection_toggle_left_token);
    add_memb_fn({{ImGuiKey_RightArrow}, input::modifiers(input::shift | input::control)},
                &editor::selection_toggle_right_token);
    add_memb_fn({{ImGuiKey_End}, input::modifiers(input::shift | input::control)}, &editor::selection_endfile);
    add_memb_fn({{ImGuiKey_Home}, input::modifiers(input::shift | input::control)}, &editor::selection_begfile);

    return shortcuts;
}

ImEdit::style ImEdit::editor::get_default_style() {
    struct style s{};
    s.cursor_color                               = ImColor(255, 255, 255, 255);
    s.background_color                           = ImColor( 43,  43,  43, 255);
    s.selection_color                            = ImColor( 33,  66, 131, 255);
    s.selection_outline_color                    = s.selection_color;
    s.search_match_color                         = ImColor( 60,  89,  61, 255);
    s.search_match_outline_color                 = ImColor( 60, 112,  75, 255);
    s.line_number_color                          = ImColor(158, 160, 159, 255);
    s.line_number_separator_color                = ImColor( 55,  55,  55, 255);
    s.current_line_color                         = ImColor( 50,  50,  50, 255);
    s.breakpoint_color                           = ImColor(219,  92,  92, 255);
    s.breakpoint_hover_color                     = ImColor(219,  92,  92, 100);
    s.token_style[token_type::unknown]           = token_style{ImColor(  0,   0,   0, 255), false, false};
    s.token_style[token_type::keyword]           = token_style{ImColor(210,  40,  58, 255), false, false};
    s.token_style[token_type::comment]           = token_style{ImColor(120, 120, 120, 255), false,  true};
    s.token_style[token_type::multiline_comment] = token_style{ImColor(120, 120, 120, 255), false,  true};
    s.token_style[token_type::blank]             = token_style{ImColor(255, 255, 255, 175), false, false};
    s.token_style[token_type::variable]          = token_style{ImColor(209, 218, 218, 255), false, false};
    s.token_style[token_type::string_literal]    = token_style{ImColor(219, 193,  82, 255),  true, false};
    s.token_style[token_type::num_literal]       = token_style{ImColor(169, 123, 227, 255), false, false};
    s.token_style[token_type::type]              = token_style{ImColor(167, 236,  33, 255), false, false};
    s.token_style[token_type::none]              = token_style{ImColor(255, 255, 255, 255), false, false};
    s.token_style[token_type::function]          = token_style{ImColor(238, 182,  98, 255), false, false};
    s.token_style[token_type::opening]           = token_style{ImColor(226, 214, 187, 255), false, false};
    s.token_style[token_type::closing]           = token_style{ImColor(226, 214, 187, 255), false, false};
    s.token_style[token_type::operator_]         = token_style{ImColor(249,  38, 114, 255), false, false};
    s.token_style[token_type::punctuation]       = token_style{ImColor(226, 214, 187, 255), false, false};
    s.token_style[token_type::max]               = token_style{ImColor(  0,   0,   0, 255), false, false};
    return s;
}

bool ImEdit::editor::is_mouse_in_breakpoint_column() const noexcept {
    auto mouse_coord = screen_to_token_coordinates(ImGui::GetMousePos());
    return _breakpoint_toggled && ImGui::IsWindowHovered(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem) && mouse_coord.is_left && mouse_coord.char_index > 1;
}
