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
    const ImVec2 draw_region{std::max(_longest_line_px + extra_padding, _width ? *_width : 0),
                             _height ?
                                    *_height != 0 ? *_height : region_avail.y
                                    : ImGui::GetTextLineHeightWithSpacing() * static_cast<float>(_lines.size())};

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
        return;
    }

    ImGui::InvisibleButton("invisible button",
                           {draw_region.x, std::max(draw_region.y, ImGui::GetTextLineHeightWithSpacing() * static_cast<float>(_lines.size()))});

    const auto rect_min = ImGui::GetItemRectMin();
    const auto rect_max = ImGui::GetItemRectMax();
    ImGui::PushClipRect(rect_min, rect_max, true);

    draw_list->AddRectFilled(imgui_cursor,
                             {imgui_cursor.x + draw_region.x, imgui_cursor.y + draw_region.y},
                             _style.background_color);

    handle_mouse_input();
    handle_kb_input();


    if (_scroll_up_next_frame) {
        ImGui::SetScrollY(ImGui::GetScrollY() - ImGui::GetTextLineHeightWithSpacing() * 3);
        _scroll_up_next_frame = false;
    }

    if (_scroll_down_next_frame) {
        ImGui::SetScrollY(ImGui::GetScrollY() + ImGui::GetTextLineHeightWithSpacing() * 3);
        _scroll_down_next_frame = false;
    }


    auto line_numbers_max_glyphs = std::to_string(_lines.size()).size();
    auto tooltip_this_frame = _tooltips.end();
    for (unsigned int i = 0 ; i < _lines.size() ; ++i) {
        // TODO do not render lines that are too much at the beginning or too much at the end (look-up for scroll)

        // Do not render if out of draw region
        if (imgui_cursor.y > draw_region.y + _imgui_cursor_position.y) {
            break;
        }

        const line &line = _lines[i];

        assert(!_cursors.empty());
        if (_cursors.back().coord.line == i) {
            draw_list->AddRectFilled(imgui_cursor, {imgui_cursor.x + draw_region.x,
                                                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()},
                                     _style.current_line_color);
        }

        // drawing line numbers
        auto line_number = std::to_string(i);
        auto extra_shift = static_cast<float>(line_numbers_max_glyphs - line_number.size()) * space_length.x;

        draw_list->AddText({imgui_cursor.x + extra_shift, imgui_cursor.y}, _style.line_number_color,
                           line_number.c_str());

        imgui_cursor.x += extra_padding - 5;
        draw_list->AddLine(imgui_cursor, {imgui_cursor.x, imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()},
                           _style.line_number_separator_color);
        imgui_cursor.x += 5;

        if (imgui_cursor.y >= draw_region.y + _imgui_cursor_position.y) {
            break;
        }

        if (line.background) {
            draw_list->AddRectFilled(imgui_cursor, {imgui_cursor.x + draw_region.x,
                                                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()},
                                     *line.background);
        }

        // Drawing selected region
        for (region select: _selections) {
            if (coordinates_eq(select.beg, select.end)) {
                continue;
            }

            coordinates min, max;
            if (coordinates_lt(select.beg, select.end)) {
                min = select.beg;
                max = select.end;
            } else {
                min = select.end;
                max = select.beg;
            }

            std::optional<ImVec2> select_draw_start, select_draw_end;
            if (min.line == i) {
                if (max.line == i) {
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
            } else if (min.line < i) {
                if (max.line == i) {
                    // selected = [start_of_line, column(max)]
                    select_draw_start = imgui_cursor;
                    select_draw_end = {
                            imgui_cursor.x + static_cast<float>(column_count_to(max)) * glyph_size().x,
                            imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()
                    };
                } else if (max.line > i) {
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
                draw_list->AddRectFilled(*select_draw_start, *select_draw_end, _style.selection_color);
            }
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
                            auto line_height = ImGui::GetFontSize() / 10;
                            auto line_y_pos = (space_length.y - line_height) / 2;
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
                            draw_list->AddCircleFilled(
                                    ImVec2(imgui_cursor.x + space_length.x / 2, imgui_cursor.y + space_length.y / 2),
                                    ImGui::GetFontSize() / 10,
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

                if (token.id != std::byte{0} && ImGui::IsMouseHoveringRect(imgui_cursor, imgui_cursor + text_size)
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
        if (ImGui::IsWindowFocused() || _tooltip_has_focus) {
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

    ImGui::PopClipRect();
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

    if (_default_font != nullptr) {
        ImGui::PopFont();
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
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_up(cursor.coord, cursor.wanted_column);
    }
    _selections.clear();
    manage_extra_cursors();
}


void ImEdit::editor::move_cursors_down() {
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_down(cursor.coord, cursor.wanted_column);
    }
    _selections.clear();
    manage_extra_cursors();
}
void ImEdit::editor::move_cursors_left() {
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_left(cursor.coord);
        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selections.clear();
    manage_extra_cursors();
}
void ImEdit::editor::move_cursors_right() {
    for (auto& cursor : _cursors) {
        cursor.coord = move_coordinates_right(cursor.coord);
        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selections.clear();
    manage_extra_cursors();
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

void ImEdit::editor::move_cursors_to_end() {
    for (auto& cursor : _cursors) {
        if (!_lines[cursor.coord.line].tokens.empty()) {
            cursor.coord.token = _lines[cursor.coord.line].tokens.size() - 1;
            cursor.coord.char_index = _lines[cursor.coord.line].tokens.back().data.size();
            cursor.wanted_column = column_count_to(cursor.coord);
        }
    }
    _selections.clear();
    manage_extra_cursors();
}

void ImEdit::editor::move_cursors_to_beg() {
    for (auto& cursor : _cursors) {
        cursor.coord.token = 0;
        cursor.coord.char_index = 0;
        cursor.wanted_column = 0;
    }
    _selections.clear();
    manage_extra_cursors();
}

void ImEdit::editor::move_cursors_left_token() {
    for (auto& cursor : _cursors) {
        if (cursor.coord.char_index > 0) {
            cursor.coord.char_index = 0;
        }
        else if (cursor.coord.token > 0) {
            --cursor.coord.token;
            // cursor.coord.char_index == 0
        }
        else {
            cursor.coord = move_coordinates_left(cursor.coord); // just one left: previous line
        }

        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selections.clear();
    manage_extra_cursors();
}


void ImEdit::editor::move_cursors_right_token() {
    for (auto& cursor : _cursors) {
        if (_lines[cursor.coord.line].tokens.empty()) { // NOLINT(*-branch-clone)
            cursor.coord = move_coordinates_right(cursor.coord);
        }
        else if (auto char_count = _lines[cursor.coord.line].tokens[cursor.coord.token].data.size() ; cursor.coord.char_index < char_count) {
            cursor.coord.char_index = char_count;
        }
        else if (cursor.coord.token + 1 < _lines[cursor.coord.line].tokens.size()) {
            ++cursor.coord.token;
            cursor.coord.char_index = _lines[cursor.coord.line].tokens[cursor.coord.token].data.size();
        }
        else {
            cursor.coord = move_coordinates_right(cursor.coord); // just one right: next line
        }

        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selections.clear();
    manage_extra_cursors();

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

void ImEdit::editor::manage_extra_cursors() {

    // Merging selections first.
    bool perform_merge;
    do {
        perform_merge = false;

        unsigned int a = _selections.size();
        unsigned int b = a;
        for (unsigned int i = 0; i < _selections.size() && !perform_merge; ++i) {
            for (unsigned int j = 0; j < _selections.size(); ++j) {
                if (i == j) {
                    continue;
                }

                if (coordinates_lt_eq(_selections[j].beg, _selections[i].end)) {
                    perform_merge = true; // merge to be performed;
                    a = i;
                    b = j;
                    break;
                }
            }
        }

        if (perform_merge) {
            if (coordinates_lt_eq(_selections[a].end, _selections[b].end)) {
                _selections[a].end = _selections[b].end;
            }
            _selections.erase(std::next(_selections.begin(), b));
        }

    } while (perform_merge);


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
                if (coordinates_within(it->coord, select)) {
                    to_erase = it;
                }
            }
        }

        if (to_erase != _cursors.end()) {
            delete_performed = true;
            _cursors.erase(to_erase);
        }

    } while (delete_performed);
}

void ImEdit::editor::add_cursor(coordinates coords) {
    assert(coords.line < _lines.size());
    assert(coords.token < _lines[coords.line].tokens.size());
    assert(coords.char_index <= _lines[coords.line].tokens[coords.token].data.size());

    _cursors.push_back({coords, column_count_to(coords)});
    manage_extra_cursors();
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
        float length = calc_line_size(i, space_size);
        if (length > _longest_line_px) {
            _longest_line_px = length;
            _longest_line_idx = i;
        }
    }
}

float ImEdit::editor::calc_line_size(unsigned int line, float space_size) const noexcept {
    if (_lines[line].tokens.empty()) {
        return 0;
    }

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

    // check if we are after the last char, in which case we do nothing
    if (co.line == _lines.size() - 1 && co.token == _lines.back().tokens.size() - 1 &&
        co.char_index == _lines.back().tokens.back().data.size()) {
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
            if (cursor.coord.line == deleted_coord.line && cursor.coord.token == deleted_coord.token
                && cursor.coord.char_index >= deleted_coord.char_index) {
                cursor.coord.char_index -= char_count;
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

    manage_extra_cursors();

}

void ImEdit::editor::handle_mouse_input() {
    if (!_allow_mouse_input || !ImGui::IsItemHovered()) {
        return;
    }

    ImGui::SetMouseCursor(ImGuiMouseCursor_TextInput);
    auto mouse_coord = screen_to_token_coordinates(ImGui::GetMousePos());

    if (ImGui::IsMouseDragging(0) && _last_frame_mouse_coords) {
        if (mouse_coord.is_left) {
            mouse_coord.token = mouse_coord.char_index = 0;
        }
        coordinates coord = mouse_coord.as_default_coords();

        // left click dragging = update selection
        if (_selections.empty()) {
            _selections.emplace_back();
            coordinates previous_coord = _last_frame_mouse_coords->as_default_coords();
            if (_last_frame_mouse_coords->is_left) {
                previous_coord.token = previous_coord.char_index = 0;
            }
            _selections.back().beg = previous_coord;
            _selections.back().end = coord;
        }
        else {
            _selections.back().end = coord;
        }

        _cursors.clear();
        _cursors.push_back({coord, column_count_to(coord)});
    }

    if (ImGui::IsMouseClicked(0)) {

        _selections.clear();

        ImGuiIO &im_io = ImGui::GetIO();
        const bool alt = im_io.ConfigMacOSXBehaviors ? im_io.KeyCtrl : im_io.KeyAlt;
        const bool ctrl = im_io.ConfigMacOSXBehaviors ? im_io.KeyAlt : im_io.KeyCtrl;
        const bool shift = im_io.KeyShift;

        coordinates new_cursor_coords = mouse_coord.as_default_coords();
        if (!alt || !shift) {
            _cursors.clear();
            _cursors.push_back({new_cursor_coords, column_count_to(new_cursor_coords)});
        } else {

            auto it = std::find_if(_cursors.begin(), _cursors.end(), [this, &new_cursor_coords](const cursor &cursor) {
                return coordinates_eq(cursor.coord, new_cursor_coords);
            });
            if (it == _cursors.end()) {
                _cursors.push_back({new_cursor_coords, column_count_to(new_cursor_coords)});
            } else if (_cursors.size() > 1) { // don’t erase last cursor
                _cursors.erase(it);
            }
        }
    }

    _last_frame_mouse_coords = mouse_coord;
}

ImEdit::coordinates_cbl ImEdit::editor::screen_to_token_coordinates(ImVec2 pos) {
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
        coords.token = coords.char_index = 0;
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
    _cursors.push_back({r.end, column_count_to(r.end)});
    if (coordinates_lt(r.end, r.beg)) {
        std::swap(r.beg, r.end);
    }
    _selections.emplace_back(r);
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

void ImEdit::editor::delete_selections() {
    // TODO call lexer, also with previous data

    assert(_selections.empty() || _cursors.size() == _selections.size());
    for (unsigned int i = 0 ; i < _selections.size() ; ++i) {

        coordinates beg;
        coordinates end;
        if (coordinates_lt(_selections[i].beg, _selections[i].end)) {
            beg = _selections[i].beg;
            end = _selections[i].end;
        } else {
            beg = _selections[i].end;
            end = _selections[i].beg;
        }

        if (beg.line == end.line) {

            auto& line = _lines[beg.line];
            assert(!line.tokens.empty());
            if (beg.token != end.token) {
                line.tokens.erase(std::next(line.tokens.begin(), beg.token + 1), std::next(line.tokens.begin(), end.token));
                if (beg.char_index == 0) {
                    line.tokens.erase(std::next(line.tokens.begin(), beg.token));
                } else if (beg.char_index < line.tokens[beg.token].data.size()) {
                    line.tokens[beg.token].data.erase(beg.char_index);
                }
                line.tokens[beg.token + 1].data.erase(0, end.char_index);
            }
            else {
                line.tokens[beg.token].data.erase(beg.char_index, end.char_index - beg.char_index);
            }

        } else {
            {
                auto &beg_line = _lines[beg.line];
                if (!beg_line.tokens.empty()) {
                    if (beg.token + 1 < beg_line.tokens.size() - 1) {
                        beg_line.tokens.erase(std::next(beg_line.tokens.begin(), beg.token + 1), beg_line.tokens.end());
                    }
                    if (beg.char_index == 0) {
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
                end_line.tokens.erase(end_line.tokens.begin());
            }

            for (token& tok : end_line.tokens) {
                _lines[beg.line].tokens.emplace_back(std::move(tok));
            }
            _lines.erase(std::next(_lines.begin(), beg.line + 1));
        }

        // FIXME : shift next cursors (lines, token and/or glyph)
        // FIXME : shift other selections

        _cursors[i].coord = beg;
        _cursors[i].wanted_column = column_count_to(beg);
    }
    _selections.clear();
}

void ImEdit::editor::input_char_utf16(ImWchar ch) {
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
}

void ImEdit::editor::input_raw_char(char ch) {
    assert(ch != '\n' && "Call input_newline() instead");
    for (cursor& c : _cursors) {
        if (_lines[c.coord.line].tokens.empty()) {
            _lines[c.coord.line].tokens.push_back({"", token_type::unknown});
        }
        _lines[c.coord.line].tokens[c.coord.token].data.insert(c.coord.char_index, 1, ch);

        for (cursor& c2 : _cursors) {
            if (c.coord.line == c2.coord.line && c.coord.token == c2.coord.token && c.coord.char_index < c2.coord.char_index) {
                c2.coord.char_index++; // just inserted a char before that cursor
            }
        }
        c.coord.char_index++; // inserted a char at this index
    }
}

void ImEdit::editor::input_newline() {
    // TODO : call lexer, also with previous data
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
                original.id &= std::byte(0);
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
}

void ImEdit::editor::input_newline_nomove() {
    for (cursor& c : _cursors) {
        _lines.insert(std::next(_lines.cbegin(), c.coord.line + 1), line{});
        for (cursor& c2 : _cursors) {
            if (c2.coord.line > c.coord.line) {
                ++c2.coord.line;
            }
        }
    }
}

void ImEdit::editor::input_delete() {
    if (!_selections.empty()) {
        delete_selections();
    } else {
        for (cursor &cursor: _cursors) {
            delete_glyph(move_coordinates_right(cursor.coord));
        }
    }
}

void ImEdit::editor::input_backspace() {
    if (!_selections.empty()) {
        delete_selections();
    } else {
        for (cursor &cursor: _cursors) {
            delete_glyph(cursor.coord);
        }
    }
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
    std::ostringstream oss;
    bool is_first_selection{true};
    for (const region& select : _selections) {
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
}

void ImEdit::editor::paste_from_clipboard() {
    delete_selections();
    const char* str = ImGui::GetClipboardText();

    // TODO: if str.line_count <= _cursors.size(), paste each line to a different cursor

    while (*str != '\0') {
        auto length = char_count_for_utf8(*str);
        bool compound = length > 1;
        while (--length) {
            input_raw_char(*str++);
            assert(*str != '\0');
        }

        if (!compound && *str == '\n') {
            input_newline();
        } else {
            input_raw_char(*str);
        }
        ++str;
    }
}

void ImEdit::editor::cut_to_clipboard() {
    copy_to_clipboard();
    delete_selections();
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

    return shortcuts;
}

ImEdit::style ImEdit::editor::get_default_style() {
    struct style s{};
    s.cursor_color                                = ImColor(255, 255, 255, 255);
    s.background_color                            = ImColor( 43,  43,  43, 255);
    s.selection_color                             = ImColor( 33,  66, 131, 255);
    s.line_number_color                           = ImColor(158, 160, 159, 255);
    s.line_number_separator_color                 = ImColor( 55,  55,  55, 255);
    s.current_line_color                          = ImColor( 50,  50,  50, 255);
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
