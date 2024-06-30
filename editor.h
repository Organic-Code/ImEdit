#ifndef IMEDIT_EDITOR_H
#define IMEDIT_EDITOR_H

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

#include <vector>
#include <string>
#include <deque>
#include <optional>
#include <unordered_map>
#include <functional>
#include <variant>
#include <chrono>

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
        // Any token types added this way MUST be added to ImEdit::editor::_style.token_style
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
            opening, // ( [ {
            closing, // ) ] }
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
        std::byte id{0}; // if two tokens have data, id, and type all set as equal by lexer, selecting one will highlight the other. id == 0: ignored (TODO)

        bool operator==(const token& other) const noexcept {
            return id == other.id && type == other.type && data == other.data;
        }
    };

    struct token_hash {
        std::size_t operator()(const token& tok) const {
            return str_hash(tok.data) >> 8 | static_cast<std::size_t>(tok.id) << (sizeof(std::size_t) * 8 - 8);
        }
        std::hash<std::string> str_hash;
    };

    struct line {
        std::deque<token> tokens{};
        std::optional<ImColor> background{};
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
        ImColor line_number_color{};
        ImColor line_number_separator_color{};
        ImColor current_line_color{};
        std::unordered_map<token_type::enum_, token_style, token_type> token_style{};
    };

    struct coordinates {
        unsigned int line{};
        unsigned int token{};
        unsigned int glyph{}; // index within the token
    };

    // used to represent coordinates that can be to the left of the glyphs (in the numbers column)
    struct coordinates_cbl : coordinates {
        bool is_left{false}; // true: token == 0, glyph indicates how many glyphs to the left the coord is.

        [[nodiscard]] coordinates as_default_coords() const noexcept {
            return {line, token, glyph};
        }
    };

    struct cursor {
        coordinates coord{};
        unsigned int wanted_column{}; // where the cursor wishes to be at (can be more to the right than possible on a given line)
    };

    struct region {
        coordinates beg{};
        coordinates end{};
    };


    class editor {
    public:
        explicit editor(std::string id); // id is passed down to imgui

        void render();
        void set_data(const std::string& data);
        void set_data(std::deque<line> lines) {
            clear();
            _lines = std::move(lines);
        }

        void add_selection(region r) noexcept;
        const std::vector<region>& get_selections() const noexcept { return _selections; }
        void delete_selections();

        void add_cursor(coordinates coords);
        void remove_cursor(coordinates coords);

        void move_cursors_up();
        void move_cursors_down();
        void move_cursors_left();
        void move_cursors_right();
        void move_cursors_to_end(); // endline
        void move_cursors_to_beg(); // begline
        void move_cursors_left_token(); // moves by one token to the left
        void move_cursors_right_token(); // moves by one token to the right

        void input_char(ImWchar c); // Simulates a keyboard input

        void clear();

        void delete_glyph(coordinates); // deletes the glyph at the given coordinates, ie just before a cursor that would have those coordinates

        void font_changed() const noexcept { // Call this whenever the font is modified
            _glyph_size.reset();
        }

        void reset_current_tooltip();

        static style get_default_style(); // similar to monokai

        bool _allow_keyboard_input{true}; // set to false to inhibit keyboard management
        bool _allow_mouse_input{true}; // set to false to inhibit mouse management
        bool _show_leading_space{true};
        unsigned int _tab_length{4};
        std::optional<float> _height{}; // empty optional <=> height equal content size. value set to 0 <=> take all available height. Other <=> take specified height
        std::optional<float> _width{}; // similar to _height

        // We are assuming the font and its variants are monospace.
        ImFont* _default_font{nullptr}; // Call editor::font_changed after setting this. if nullptr, the font is not pushed in the ImGui stack at all
        ImFont* _bold_font{nullptr}; // if nullptr, the font is not pushed in the ImGui stack at all
        ImFont* _italic_font{nullptr}; // if nullptr, the font is not pushed in the ImGui stack at all
        ImFont* _bold_italic_font{nullptr}; // if nullptr, the font is not pushed in the ImGui stack at all

        style _style{};

        // Tooltips.
        // When matching token is hovered and _tooltips[token] exists, calls ImGui::BeginTooltip,
        // and either prints the variant’s string, or calls the std::function so that you can draw whatever you wish in the tooltip.
        // Callback is called with _tooltip_data as its first parameter.
        using tooltip_callback = std::function<void(void* tooltip_data, const token&)>;
        std::unordered_map<token, std::variant<std::string, tooltip_callback>, token_hash> _tooltips; // when modifying this, remember to call editor::reset_current_tooltip
        void* _tooltip_data{nullptr}; // Passed to tooltip callbacks as user data
        std::chrono::milliseconds _tooltip_delay{std::chrono::seconds(1)}; // Delay before the tooltip appears
        std::chrono::milliseconds _tooltip_grace_period{std::chrono::milliseconds(250)}; // Delay for which the tooltip stays up, even after the mouse went away

    private:

        // Counts the number of columns in the line in order to attain the specified coordinates
        [[nodiscard]] unsigned int column_count_to(coordinates) const noexcept;

        // Returns the coordinates for given line such that column_count_to(coordinates) returns column_count, if possible for that line
        [[nodiscard]] coordinates coordinates_for(unsigned int column_count, unsigned int line) const noexcept;

        [[nodiscard]] coordinates move_coordinates_up(coordinates, unsigned int wanted_column) const noexcept;
        [[nodiscard]] coordinates move_coordinates_down(coordinates, unsigned int wanted_column) const noexcept;
        [[nodiscard]] coordinates move_coordinates_left(coordinates) const noexcept;
        [[nodiscard]] coordinates move_coordinates_right(coordinates) const noexcept;

        [[nodiscard]] coordinates_cbl screen_to_token_coordinates(ImVec2 pos);

        [[nodiscard]] bool coordinates_eq(coordinates lhs, coordinates rhs) const noexcept;
        [[nodiscard]] bool coordinates_lt(coordinates lhs, coordinates rhs) const noexcept;
        [[nodiscard]] bool coordinates_lt_eq(coordinates lhs, coordinates rhs) const noexcept;
        [[nodiscard]] bool coordinates_within(coordinates coord, region r) const noexcept;

        // Deletes cursors that are at the same place to leave only on of them,
        // and merges selections that should be merged together (when using multiple cursors)
        void manage_extra_cursors();

        [[nodiscard]] ImVec2 glyph_size() const noexcept;

        [[nodiscard]] static ImVec2 calc_text_size(const char* text, const char* text_end = nullptr) noexcept;

        void find_longest_line();
        [[nodiscard]] float calc_line_size(unsigned int line, float space_size = ImGui::CalcTextSize(" ").x) const noexcept;

        void handle_kb_input();
        void handle_mouse_input();
        float compute_extra_padding() const noexcept;

        void show_tooltip();

        std::vector<cursor> _cursors{};
        std::deque<line> _lines{};

        std::optional<coordinates_cbl> _last_frame_mouse_coords{};

        unsigned int _longest_line_idx{};
        float _longest_line_px{};

        ImVec2 _imgui_cursor_position{};
        std::vector<region> _selections{};

        const std::string _imgui_id;

        std::optional<ImVec2> _tooltip_pos{};
        std::optional<decltype(_tooltips)::iterator> _tooltip{};
        std::optional<std::chrono::system_clock::time_point> _tooltip_chrono{};
        std::chrono::system_clock::time_point _tooltip_last_hovered_at{};

        mutable std::optional<ImVec2> _glyph_size{};
    };
}

#endif //IMEDIT_EDITOR_H
