#include <algorithm>
#include <sstream>
#include <cctype>
#include <chrono>
#include <cmath>
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

    bool istokseparator(char c) {
        return isopening(c) || isclosing(c) || iscomma(c) || std::isspace(c); // todo : add C style punctuation
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

        std::string line = str.data(); // trimming extras '\0'
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

    _imgui_cursor_position = ImGui::GetCursorScreenPos();

    // TODO: wrapping
    if (_longest_line_px == 0) {
        find_longest_line();
    }

    const auto draw_width = _draw_width ? *_draw_width : _longest_line_px;
    const auto draw_height = _draw_height ? *_draw_height : ImGui::GetTextLineHeightWithSpacing() * static_cast<float>(_lines.size() + 1);

    const auto space_length = glyph_size();
    auto imgui_cursor = ImGui::GetCursorScreenPos();
    const ImVec2 draw_region{draw_width, draw_height};
    auto draw_list = ImGui::GetWindowDrawList();

    // space to the left for displaying line numbers, breakpoints, and such
    const auto extra_padding = compute_extra_padding();

    draw_list->AddRectFilled(imgui_cursor,
                             {imgui_cursor.x + draw_region.x + extra_padding, imgui_cursor.y + draw_region.y},
                             _style.background_color);


    float max_line_width = 0.f;
    auto line_numbers_max_glyphs = std::to_string(_lines.size()).size();
    for (unsigned int i = 0 ; i < _lines.size() ; ++i) {
        const line& line = _lines[i];

        assert(!_cursors.empty());
        if (_cursors.back().coord.line == i) {
            draw_list->AddRectFilled(imgui_cursor, {imgui_cursor.x + draw_region.x + extra_padding,
                                                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()},
                                     _style.current_line_color);
        }

        // drawing line numbers
        auto line_number = std::to_string(i);
        auto extra_shift = static_cast<float>(line_numbers_max_glyphs - line_number.size()) * space_length.x;
        draw_list->AddText({imgui_cursor.x + extra_shift, imgui_cursor.y}, _style.line_number_color, line_number.c_str());

        imgui_cursor.x += extra_padding - 5;
        draw_list->AddLine(imgui_cursor, {imgui_cursor.x, imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()},
                           _style.line_number_separator_color);
        imgui_cursor.x += 5;

        if (imgui_cursor.y >= draw_region.y + _imgui_cursor_position.y) {
            break;
        }

        if (line.background) {
            draw_list->AddRectFilled(imgui_cursor, {imgui_cursor.x + draw_region.x,
                                                    imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()}, *line.background);
        }

        // Drawing selected region
        if (_selection) {
            coordinates min, max;
            if (coordinates_lt(_selection->beg, _selection->end)) {
                min = _selection->beg;
                max = _selection->end;
            } else {
                min = _selection->end;
                max = _selection->beg;
            }

            std::optional<ImVec2> select_draw_start, select_draw_end;
            if (min.line == i) {
                if (max.line == i) {
                    // selected = [column(min), column(max)]
                    if (min.token != 0 || min.glyph != 0) {
                        select_draw_start = {
                                imgui_cursor.x + static_cast<float>(column_count_to(coordinates_move_left(min))) * glyph_size().x,
                                imgui_cursor.y
                        };
                    } else {
                        select_draw_start = imgui_cursor;
                    }
                    select_draw_end = {
                            imgui_cursor.x + static_cast<float>(column_count_to(max)) * glyph_size().x,
                            imgui_cursor.y + ImGui::GetTextLineHeightWithSpacing()
                    };
                }
                else {
                    // selected = [column(min), end_of_line]
                    if (min.token != 0 || min.glyph != 0) {
                        select_draw_start = {
                                imgui_cursor.x + static_cast<float>(column_count_to(coordinates_move_left(min))) * glyph_size().x,
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
                }
                else if (max.line > i) {
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

        // TODO: scrolling : ne pas tout rendre ?

        bool is_leading_space = true;
        unsigned int column = 0;



        for (const auto & token : line.tokens) {
            if (imgui_cursor.x >= draw_region.x + _imgui_cursor_position.x) {
                break;
            }

            // If you are getting an error here, maybe you defined your own token types and forgot to add them to ImEdit::editor.get_style().token_colors
            const ImColor color = _style.token_colors.at(token.type);
            const std::string& data = token.data;


            if (token.type == token_type::blank) {

                bool show_space = _show_leading_space && is_leading_space;
                for (char c : data) {
                    if (c == '\t') {
                        unsigned int number_of_spaces = _tab_length - column % _tab_length;
                        float tab_length = space_length.x * static_cast<float>(number_of_spaces);
                        if (show_space) {
                            auto line_height = ImGui::GetFontSize() / 10;
                            auto line_y_pos = (space_length.y - line_height) / 2 ;
                            draw_list->AddLine(
                                    ImVec2(imgui_cursor.x + 2, imgui_cursor.y + line_y_pos),
                                    ImVec2(imgui_cursor.x + tab_length - 2, imgui_cursor.y + line_y_pos),
                                    color,
                                    line_height);
                        }
                        imgui_cursor.x += tab_length;
                        column += number_of_spaces;
                    } else {
                        if (show_space) {
                            draw_list->AddCircleFilled(
                                    ImVec2(imgui_cursor.x + space_length.x / 2, imgui_cursor.y + space_length.y / 2),
                                    ImGui::GetFontSize() / 10,
                                    color
                                    );
                        }
                        imgui_cursor.x += space_length.x;
                        ++column;
                    }
                }

            }
            else {

                is_leading_space = false;
                draw_list->AddText(imgui_cursor, color, data.data(), data.data() + data.size());
                auto text_size = ImGui::CalcTextSize(data.data(), data.data() + data.size());
                if (!token.tooltip.empty()) {
                    if (ImGui::IsMouseHoveringRect(imgui_cursor, imgui_cursor + text_size)) {
                        ImGui::BeginTooltip();
                        ImGui::Text("%s", token.tooltip.data());
                        ImGui::EndTooltip();
                    }
                }

                imgui_cursor.x += text_size.x;
                column += data.size();
            }
        }

        // Render cursor
        for (cursor& c : _cursors) {
            if (i == c.coord.line) {
                auto time = std::chrono::duration_cast<std::chrono::milliseconds>(
                        std::chrono::system_clock::now().time_since_epoch());
                time %= 1500;

                if (time.count() > 400) {
                    auto x = _imgui_cursor_position.x + static_cast<float>(column_count_to(c.coord)) * space_length.x + extra_padding;
                    draw_list->AddLine(
                            {x, imgui_cursor.y + 2}, {x, imgui_cursor.y + space_length.y - 2},
                            _style.cursor_color
                    );
                }
            }
        }

        max_line_width = std::max(max_line_width, imgui_cursor.x - _imgui_cursor_position.x);
        imgui_cursor.x = _imgui_cursor_position.x;
        imgui_cursor.y += ImGui::GetTextLineHeightWithSpacing();
    }


    ImGui::InvisibleButton(_imgui_id.c_str(),
                           {std::max(draw_region.x + extra_padding, max_line_width + 2), draw_region.y});
    handle_mouse_input();
    handle_kb_input();
}



ImEdit::editor::editor(std::string id) :
        _lines{},
        _style{get_default_style()},
        _imgui_id{std::move(id)}
{
    _cursors.emplace_back();
}

ImEdit::style ImEdit::editor::get_default_style() {
    struct style s{};
    s.cursor_color                                = ImColor(255, 255, 255, 255);
    s.background_color                            = ImColor( 43,  43,  43, 255);
    s.selection_color                             = ImColor( 33,  66, 131, 255);
    s.line_number_color                           = ImColor(158, 160, 159, 255);
    s.line_number_separator_color                 = ImColor( 55,  55,  55, 255);
    s.current_line_color                          = ImColor( 50,  50,  50, 255);
    s.token_colors[token_type::unknown]           = ImColor(  0,   0,   0, 255);
    s.token_colors[token_type::keyword]           = ImColor(210,  40,  58, 255);
    s.token_colors[token_type::comment]           = ImColor(120, 120, 120, 255);
    s.token_colors[token_type::multiline_comment] = ImColor(120, 120, 120, 255);
    s.token_colors[token_type::blank]             = ImColor(255, 255, 255, 175);
    s.token_colors[token_type::variable]          = ImColor(209, 218, 218, 255);
    s.token_colors[token_type::string_literal]    = ImColor(219, 193,  82, 255);
    s.token_colors[token_type::num_literal]       = ImColor(169, 123, 227, 255);
    s.token_colors[token_type::type]              = ImColor(167, 236,  33, 255);
    s.token_colors[token_type::none]              = ImColor(255, 255, 255, 255);
    s.token_colors[token_type::function]          = ImColor(238, 182,  98, 255);
    s.token_colors[token_type::opening]           = ImColor(226, 214, 187, 255);
    s.token_colors[token_type::closing]           = ImColor(226, 214, 187, 255);
    s.token_colors[token_type::operator_]         = ImColor(249,  38, 114, 255);
    s.token_colors[token_type::punctuation]       = ImColor(226, 214, 187, 255);
    s.token_colors[token_type::max]               = ImColor(  0,   0,   0, 255);
    return s;
}


void ImEdit::editor::cursors_move_up() {
    for (auto& cursor : _cursors) {
        cursor.coord = coordinates_move_up(cursor.coord, cursor.wanted_column);
    }
    _selection.reset();
    delete_non_unique_cursors();
}
void ImEdit::editor::cursors_move_down() {
    for (auto& cursor : _cursors) {
        cursor.coord = coordinates_move_down(cursor.coord, cursor.wanted_column);
    }
    _selection.reset();
    delete_non_unique_cursors();
}
void ImEdit::editor::cursors_move_left() {
    for (auto& cursor : _cursors) {
        cursor.coord = coordinates_move_left(cursor.coord);
        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selection.reset();
    delete_non_unique_cursors();
}
void ImEdit::editor::cursors_move_right() {
    for (auto& cursor : _cursors) {
        cursor.coord = coordinates_move_right(cursor.coord);
        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selection.reset();
    delete_non_unique_cursors();
}


ImEdit::coordinates ImEdit::editor::coordinates_move_up(coordinates coord, unsigned int wanted_column) const noexcept {
    if (coord.line == 0) {
        coord.glyph = 0;
        coord.token = 0;
        return coord;
    }

    coord = coordinates_for(wanted_column, coord.line - 1);
    return coord;
}

ImEdit::coordinates ImEdit::editor::coordinates_move_down(coordinates coord, unsigned int wanted_column) const noexcept {
    if (coord.line == _lines.size() - 1) {
        coord.token = _lines.back().tokens.size() - 1;
        coord.glyph = _lines.back().tokens.back().data.size();
        return coord;
    }

    coord = coordinates_for(wanted_column, coord.line + 1);
    return coord;
}

ImEdit::coordinates ImEdit::editor::coordinates_move_left(coordinates coord) const noexcept {
    if (coord.glyph > 0) {
        --coord.glyph;
        return coord;
    }

    if (coord.token > 0) {
        --coord.token;

        const auto& sz = _lines[coord.line].tokens[coord.token].data.size();
        if (sz == 0) {
            coord.glyph = 0;
        } else {
            coord.glyph = sz - 1;
        }
        return coord;
    }

    if (coord.line > 0) {
        --coord.line;
        if (_lines[coord.line].tokens.empty()) {
            coord.token = 0;
            coord.glyph = 0;
        } else {
            coord.token = _lines[coord.line].tokens.size() - 1;
            coord.glyph = _lines[coord.line].tokens.back().data.size();
        }
        return coord;
    }

    return coord;
}

ImEdit::coordinates ImEdit::editor::coordinates_move_right(coordinates coord) const noexcept {
    if (_lines[coord.line].tokens.empty()) {
        if (coord.line + 1 < _lines.size()) {
            ++coord.line;
        }
        return coord;
    }

    if (coord.glyph < _lines[coord.line].tokens[coord.token].data.size()) {
        ++coord.glyph;
        return coord;
    }

    if (coord.token < _lines[coord.line].tokens.size() - 1) {
        ++coord.token;
        coord.glyph = 1;
        return coord;
    }

    if (coord.line < _lines.size() - 1) {
        ++coord.line;
        coord.token = 0;
        coord.glyph = 0;
        return coord;
    }
    return coord;
}

void ImEdit::editor::cursors_move_to_end() {
    for (auto& cursor : _cursors) {
        if (!_lines[cursor.coord.line].tokens.empty()) {
            cursor.coord.token = _lines[cursor.coord.line].tokens.size() - 1;
            cursor.coord.glyph = _lines[cursor.coord.line].tokens.back().data.size();
            cursor.wanted_column = column_count_to(cursor.coord);
        }
    }
    _selection.reset();
    delete_non_unique_cursors();
}

void ImEdit::editor::cursors_move_to_beg() {
    for (auto& cursor : _cursors) {
        cursor.coord.token = 0;
        cursor.coord.glyph = 0;
        cursor.wanted_column = 0;
    }
    _selection.reset();
    delete_non_unique_cursors();
}

void ImEdit::editor::cursors_move_left_token() {
    // TODO: change for going from word to word instead of from token to token ?
    for (auto& cursor : _cursors) {
        if (cursor.coord.glyph > 0) {
            cursor.coord.glyph = 0;
        }
        else if (cursor.coord.token > 0) {
            --cursor.coord.token;
            // cursor.coord.glyph == 0
        }
        else {
            cursor.coord = coordinates_move_left(cursor.coord); // just one left: previous line
        }

        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selection.reset();
    delete_non_unique_cursors();
}


void ImEdit::editor::cursors_move_right_token() {
    // TODO: change for going from word to word instead of from token to token ? Or let this be decided by the lexer ?
    for (auto& cursor : _cursors) {
        if (_lines[cursor.coord.line].tokens.empty()) {
            cursor.coord = coordinates_move_right(cursor.coord);
        }
        else if (auto glyph_count = _lines[cursor.coord.line].tokens[cursor.coord.token].data.size() ; cursor.coord.glyph < glyph_count) {
            cursor.coord.glyph = glyph_count;
        }
        else if (cursor.coord.token + 1 < _lines[cursor.coord.line].tokens.size()) {
            ++cursor.coord.token;
            cursor.coord.glyph = _lines[cursor.coord.line].tokens[cursor.coord.token].data.size();
        }
        else {
            cursor.coord = coordinates_move_right(cursor.coord); // just one right: next line
        }

        cursor.wanted_column = column_count_to(cursor.coord);
    }
    _selection.reset();
    delete_non_unique_cursors();

}

unsigned int ImEdit::editor::column_count_to(ImEdit::coordinates coord) const noexcept {
    if (coord.token == 0 && coord.glyph == 0) {
        return 0;
    }

    assert(coord.line < _lines.size());
    if (coord.token == 0 && _lines[coord.line].tokens.empty()) {
        return 0;
    }

    assert(coord.token < _lines[coord.line].tokens.size());
    assert(coord.glyph <= _lines[coord.line].tokens[coord.token].data.size());

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
            glyph_count += _lines[coord.line].tokens[i].data.size();
        }
    }

    const token& tok = _lines[coord.line].tokens[coord.token];
    if (tok.type == token_type::blank) {
        for (unsigned int i = 0 ; i < coord.glyph ; ++i) {
            if (tok.data[i] == '\t') {
                glyph_count += _tab_length - glyph_count % _tab_length;
            } else {
                ++glyph_count;
            }
        }
    } else {
        glyph_count += coord.glyph;
    }
    return glyph_count;
}

ImEdit::coordinates ImEdit::editor::coordinates_for(unsigned int column_count, unsigned int line) const noexcept {
    assert(line < _lines.size());

    coordinates coord;
    coord.line = line;
    coord.token = 0;
    coord.glyph = 0;

    if (_lines[coord.line].tokens.empty()) {
        return coord;
    }

    auto is_last_glyph = [this](coordinates coord) {
        return coord.token == _lines[coord.line].tokens.size() - 1
                && coord.glyph == _lines[coord.line].tokens[coord.token].data.size();
    };

    while (column_count_to(coord) < column_count && !is_last_glyph(coord)) {
        coord = coordinates_move_right(coord);
    }
    return coord;
}

void ImEdit::editor::delete_non_unique_cursors() {
    bool delete_performed;
    do {
        delete_performed = false;

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
}

void ImEdit::editor::cursor_add(ImEdit::coordinates coords) {
    assert(coords.line < _lines.size());
    assert(coords.token < _lines[coords.line].tokens.size());
    assert(coords.glyph <= _lines[coords.line].tokens[coords.token].data.size());

    _cursors.push_back({coords, column_count_to(coords)});
    delete_non_unique_cursors();
}

void ImEdit::editor::cursor_remove(ImEdit::coordinates coords) {
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

void ImEdit::editor::clear() {
    _cursors.clear();
    _tooltips.clear();
    _lines.clear();
    _longest_line_idx = 0;
    _longest_line_px = 0;

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
    c.glyph = _lines[line].tokens.back().data.size();

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

    if (!ctrl && !shift && !alt) {
        if (ImGui::IsKeyPressed(ImGuiKey_Delete)) {
            for (cursor &cursor: _cursors) {
                // check if we are at the last char, in which case we do nothing
                if (cursor.coord.line == _lines.size() - 1 && cursor.coord.token == _lines.back().tokens.size() - 1 &&
                    cursor.coord.glyph == _lines.back().tokens.back().data.size()) {
                    continue;
                }
                delete_glyph(coordinates_move_right(cursor.coord));
            }
        }

        if (ImGui::IsKeyPressed(ImGuiKey_Backspace)) {
            for (cursor &cursor: _cursors) {
                delete_glyph(cursor.coord);
            }
        }

        if (ImGui::IsKeyPressed(ImGuiKey_DownArrow)) {
            cursors_move_down();
        } else if (ImGui::IsKeyPressed(ImGuiKey_UpArrow)) {
            cursors_move_up();
        } else if (ImGui::IsKeyPressed(ImGuiKey_LeftArrow)) {
            cursors_move_left();
        } else if (ImGui::IsKeyPressed(ImGuiKey_RightArrow)) {
            cursors_move_right();
        } else if (ImGui::IsKeyPressed(ImGuiKey_End)) {
            cursors_move_to_end();
        } else if (ImGui::IsKeyPressed(ImGuiKey_Home)) {
            cursors_move_to_beg();
        }
    }

    if (ctrl && !shift && !alt) {
        if (ImGui::IsKeyPressed(ImGuiKey_DownArrow)) {
            // TODO scroll down
        } else if (ImGui::IsKeyPressed(ImGuiKey_UpArrow)) {
            // TODO scroll up
        } else if (ImGui::IsKeyPressed(ImGuiKey_LeftArrow)) {
            cursors_move_left_token();
        } else if (ImGui::IsKeyPressed(ImGuiKey_RightArrow)) {
            cursors_move_right_token();
        }
    }

    if (!im_io.InputQueueCharacters.empty()) {
        for (ImWchar c : im_io.InputQueueCharacters) {
            for (cursor& cursor : _cursors) {
                _lines[cursor.coord.line].tokens[cursor.coord.token].data.insert(cursor.coord.glyph, 1, c);
                ++cursor.coord.glyph;

                auto length = calc_line_size(cursor.coord.line);
                if (length > _longest_line_px) {
                    _longest_line_px = length;
                    _longest_line_idx = cursor.coord.line;
                }
            }
        }
        im_io.InputQueueCharacters.resize(0); // resize(0) makes it so that we keep the memory buffer instead of discarding it
    }
}

void ImEdit::editor::delete_glyph(coordinates co) {
    // nothing to delete
    if (co.token == 0 && co.glyph == 0 && co.line == 0) {
        return;
    }

    assert(co.line < _lines.size());
    if (_lines[co.line].tokens.empty()) {
    // deleting empty line
        _lines.erase(_lines.begin() + co.line);
        for (cursor& c : _cursors) {
            if (c.coord.line > co.line) {
                --c.coord.line;

            } else if (c.coord.line == co.line) {
                --c.coord.line;
                if (_lines[c.coord.line].tokens.empty()) {
                    c.coord.token = 0;
                    c.coord.glyph = 0;
                } else {
                    c.coord.token = _lines[c.coord.line].tokens.size() - 1;
                    c.coord.glyph = _lines[c.coord.line].tokens.back().data.size();
                }
            }
        }
        return;
    }


    assert(co.token < _lines[co.line].tokens.size());
    assert(co.glyph <= _lines[co.line].tokens[co.token].data.size());

    if (co.token == 0 && co.glyph == 0) {
    // Deleting an equivalent of '\n'

        const std::size_t original_line_tok_count =_lines[co.line - 1].tokens.size();
        for (token& tok : _lines[co.line].tokens) {
            _lines[co.line - 1].tokens.emplace_back(std::move(tok));
        }
        _lines.erase(_lines.begin() + co.line);

        for (cursor& c : _cursors) {
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

    } else {
    // Deleting a regular glyph

        for (cursor& cursor : _cursors) {
            if (cursor.coord.line == co.line && cursor.coord.token == co.token && cursor.coord.glyph >= co.glyph) {
                cursor.coord = coordinates_move_left(cursor.coord);
            }
        }

        if (co.glyph == 0) {
            assert(co.token > 0);
            --co.token;
            co.glyph = _lines[co.line].tokens[co.token].data.size();
        }


        // Deleting token if empty
        _lines[co.line].tokens[co.token].data.erase(co.glyph - 1, 1);
        if (_lines[co.line].tokens[co.token].data.empty()) {
            _lines[co.line].tokens.erase(_lines[co.line].tokens.begin() + co.token);

            for (cursor& cursor : _cursors) {
                if (cursor.coord.line == co.line) {
                    if (cursor.coord.token == co.token) {
                        // Moving cursor away from current token (that was deleted) while keeping it at the same position
                        if (cursor.coord.token != 0 || cursor.coord.glyph != 0) {
                            cursor.coord = coordinates_move_right(coordinates_move_left(cursor.coord));
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

    delete_non_unique_cursors();

}

void ImEdit::editor::handle_mouse_input() {
    if (!_allow_mouse_input || !ImGui::IsItemHovered()) {
        return;
    }

    ImGui::SetMouseCursor(ImGuiMouseCursor_TextInput);

    if (ImGui::IsMouseDragging(0)) {
        auto coord = screen_to_token_coordinates(ImGui::GetMousePos());
        if (coord.is_left) {
            coord.token = coord.glyph = 0;
        }
        // left click dragging = update selection
        if (!_selection) {
            _selection.emplace();
            _selection->beg = _selection->end = {coord.line, coord.token, coord.glyph};
        }
        else {
            _selection->end = {coord.line, coord.token, coord.glyph};
        }

        _cursors.clear();
        _cursors.emplace_back();
        _cursors.back().coord = {coord.line, coord.token, coord.glyph};
        _cursors.back().wanted_column = column_count_to(_cursors.back().coord);

        return;
    }

    if (!ImGui::IsMouseClicked(0)) {
        return;
    }
    _selection.reset();

    ImGuiIO& im_io = ImGui::GetIO();
    const bool alt = im_io.ConfigMacOSXBehaviors ? im_io.KeyCtrl : im_io.KeyAlt;
    const bool ctrl = im_io.ConfigMacOSXBehaviors ? im_io.KeyAlt : im_io.KeyCtrl;
    const bool shift = im_io.KeyShift;

    auto mouse_coord = screen_to_token_coordinates(ImGui::GetMousePos());
    if (!mouse_coord.is_left) {
        coordinates new_cursor_coords = {
                mouse_coord.line, mouse_coord.token, mouse_coord.glyph
        };
        if (!alt || !shift) {
            _cursors.clear();
            _cursors.push_back({new_cursor_coords, column_count_to(new_cursor_coords)});
        } else {

            auto it = std::find_if(_cursors.begin(), _cursors.end(), [this, &new_cursor_coords](const cursor& cursor) {
                return coordinates_eq(cursor.coord, new_cursor_coords);
            });
            if (it == _cursors.end()) {
                _cursors.push_back({new_cursor_coords, column_count_to(new_cursor_coords)});
            } else if (_cursors.size() > 1) { // don’t erase last cursor
                _cursors.erase(it);
            }
        }
    }
}

ImEdit::coordinates_cbl ImEdit::editor::screen_to_token_coordinates(ImVec2 pos) {
    // TODO what about folded regions?
    auto glyph_size = editor::glyph_size();
    coordinates_cbl coords;
    pos.x -= _imgui_cursor_position.x;
    pos.y -= _imgui_cursor_position.y;

    pos.x -= compute_extra_padding();

    auto line = std::min(static_cast<unsigned>(std::round((pos.y - 3)/ ImGui::GetTextLineHeightWithSpacing())),
                         static_cast<unsigned>(_lines.size() - 1));
    auto base_coords = coordinates_for(static_cast<unsigned>(std::round(pos.x / glyph_size.x)), line);

    coords.line = base_coords.line;
    coords.token = base_coords.token;
    coords.glyph = base_coords.glyph;

    return coords;
}

float ImEdit::editor::compute_extra_padding() const noexcept {
    auto glyph_length = glyph_size().x;
    return _lines.empty() ? 5 * glyph_length : std::floor(std::log10(static_cast<float>(_lines.size())) + 4) * glyph_length;
}

bool ImEdit::editor::coordinates_eq(coordinates lhs, coordinates rhs) const noexcept {
    if (lhs.line != rhs.line) {
        return false;
    }

    if (lhs.token == rhs.token) {
        if (lhs.glyph == rhs.glyph) {
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

        if (lhs.glyph == _lines[lhs.line].tokens[lhs.token].data.size() && rhs.glyph == 0) {
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
            if (lhs.glyph == _lines[lhs.line].tokens[lhs.token].data.size() && rhs.glyph == 0) {
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

    if (lhs.glyph < rhs.glyph) {
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
        _glyph_size = ImGui::CalcTextSize(" ");
    }
    return *_glyph_size;
}

void ImEdit::editor::selection_set(ImEdit::region r) noexcept {
    if (coordinates_lt(r.end, r.beg)) {
        std::swap(r.beg, r.end);
    }
    _selection = r;
}

