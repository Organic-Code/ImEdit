#ifndef IMEDIT_SIMPLE_TYPES_H
#define IMEDIT_SIMPLE_TYPES_H

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

#include <optional>
#include <deque>
#include <string>
#include <optional>
#include <vector>
#include <unordered_map>

#if __has_include(<imgui.h>)
#include <imgui.h>
#elif __has_include(<imgui/imgui.h>)
#include <imgui/imgui.h>
#else
#error "cannot include <imgui.h> or <imgui/imgui.h>"
#endif

namespace ImEdit {

#ifndef IMEDIT_CUSTOM_TOKEN_TYPES
#define IMEDIT_CUSTOM_TOKEN_TYPES
#endif

    struct token_type {
        // You can add token types by defining IMEDIT_CUSTOM_TOKEN_TYPES.
        // IMEDIT_CUSTOM_TOKEN_TYPES must end with a comma
        // Any token types added this way MUST be added to ImEdit::editor._style.token_style
        enum enum_ {
            unknown,
            keyword,
            comment,
            multiline_comment,
            blank,
            variable,
            string_literal,
            num_literal,
            type,
            none,
            function,
            opening, // ( [ { do ...
            closing, // ) ] } end ...
            operator_,
            punctuation,
            IMEDIT_CUSTOM_TOKEN_TYPES
            max
        };

        // hash
        unsigned long operator()(enum_ e) const noexcept {
            return static_cast<unsigned long>(e);
        }
    };

    struct token {
        std::string data{};
        token_type::enum_ type{};
        unsigned short id{0}; // if two tokens have data, id, and type all set as equal by lexer, selecting one will highlight the other. id == 0: ignored (TODO)

        bool operator==(const token& other) const noexcept {
            return id == other.id && type == other.type && data == other.data;
        }
    };

    struct token_hash {
        std::size_t operator()(const token& tok) const noexcept {
            return str_hash(tok.data) >> 8 | static_cast<std::size_t>(tok.id) << (sizeof(std::size_t) * 8 - 8);
        }
        std::hash<std::string> str_hash;
    };

    struct line {
        std::deque<token> tokens{};
        std::optional<ImColor> background{};
        bool has_breakpoint{false};
    };

    struct token_style {
        token_style() : color{}, bold{false}, italic{false} {}
        token_style(ImColor c) : color{c}, bold{false}, italic{false} {} // NOLINT(*-explicit-constructor)
        token_style(ImColor c, bool b) : color{c}, bold{b}, italic{false} {}
        token_style(ImColor c, bool b, bool i) : color{c}, bold{b}, italic{i} {}

        ImColor color;
        bool bold : 1; // You need to set editor::_bold_font (and eventually editor::_bold_italic_font) to use this
        bool italic : 1; // You need to set editor::_italic_font (and eventually editor::_bold_italic_font) to use this
    };

    struct style {
        ImColor cursor_color{};
        ImColor background_color{};
        ImColor selection_color{};
        ImColor selection_outline_color{};
        ImColor search_match_color{};
        ImColor search_match_outline_color{};
        ImColor line_number_color{};
        ImColor line_number_separator_color{};
        ImColor current_line_color{};
        ImColor breakpoint_color{};
        ImColor breakpoint_hover_color{};
        std::unordered_map<token_type::enum_, ImEdit::token_style, token_type> token_style{};
    };

    struct coordinates {
        unsigned int line{};
        unsigned int token{};
        unsigned int char_index{}; // index within the token
    };

    // used to represent coordinates that can be to the left of the glyphs (in the numbers column)
    struct coordinates_cbl : coordinates {
        bool is_left{false}; // true: token == 0, glyph indicates how many glyphs to the left the coord is.

        [[nodiscard]] coordinates as_default_coords() const noexcept {
            assert(!is_left || char_index == 0);
            return {line, token, char_index};
        }
    };

    // used for records, as tokenizer can fiddle with tokens
    struct simple_coord {
        unsigned int line{};
        unsigned int char_index{};
    };

    struct cursor {
        coordinates coord{};
        unsigned int wanted_column{}; // where the cursor wishes to be at (can be more to the right than possible on a given line)
    };

    struct region {
        coordinates beg{};
        coordinates end{};
    };

    struct simple_region {
        simple_coord beg{};
        simple_coord end{};
    };

    struct input {
        enum modifiers : unsigned char {
            none               = 0,
            shift              = 1 << 0, // Shift
            control            = 1 << 1, // Ctrl
            alternate          = 1 << 2, // Alt
            super              = 1 << 3, // meta/hyper/super/cmd
        };

        std::vector<ImGuiKey> keys;
        modifiers mod_flag{none};
    };

    struct record {

        // cursor positions
        struct cursor_position {
            std::vector<simple_region> selections;
            std::vector<simple_coord> positions;
        };

        // char deletion
        struct chars_deletion {
            std::vector<std::vector<char>> deleted_chars;
            std::vector<simple_coord> delete_location;

            std::vector<simple_coord> cursors_coords;
        };

        // char addition
        struct chars_addition {
            std::vector<std::vector<char>> added_chars;
            std::vector<simple_coord> add_location;

            std::vector<simple_coord> cursors_coords;
        };

        // new line
        struct new_line {
            simple_coord new_line_location;

            std::vector<simple_coord> cursors_coords;
        };

        // line deletion
        struct del_line {
            simple_coord line_deletion_location;

            std::vector<simple_coord> cursors_coords;
        };

        // selection deletion
        struct del_selection {
            std::vector<line> deleted_selections;
            simple_region selection_location;

            std::vector<simple_coord> cursors_coords;
        };

        // pasting
        struct paste {
            std::vector<simple_coord> coordinates;
            std::string data;

            std::vector<simple_coord> cursors_coords;
        };

        std::variant<
                std::monostate,
                cursor_position,
                chars_deletion,
                chars_addition,
                new_line,
                del_line,
                del_selection,
                paste> value;
    };
}

#endif //IMEDIT_SIMPLE_TYPES_H
