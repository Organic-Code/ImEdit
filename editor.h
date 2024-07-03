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

#include "simple_types.h"


namespace ImEdit {

    class editor {
        /*************************************************************************************
         *
         * Public API
         *
         */
    public:
        // iterates over characters
        struct iterator {
            iterator(editor* e, coordinates c) noexcept : ed{e}, current{c} {}
            iterator(const iterator& other) noexcept = default;
            iterator& operator=(const iterator& other) noexcept = default;

            iterator operator++(int) noexcept;
            iterator& operator++() noexcept;
            iterator operator--(int) noexcept;
            iterator& operator--() noexcept;
            char operator*() const noexcept;
            bool operator==(const iterator& other) const noexcept;
            bool operator!=(const iterator& other) const noexcept;

            [[nodiscard]] coordinates get_coord() const noexcept { return current; }
            [[nodiscard]] editor& editor() noexcept { return *ed; };

        private:
            class editor* ed;
            coordinates current;
        };


        explicit editor(std::string id); // id is passed down to imgui

        void render();

        /**
         * The following methods do not call _public_methods_callback
         */
        // TODO specify when those are invalidated
        // iterates over data, giving away char. Output only.
        iterator begin() noexcept;
        iterator end() noexcept;

        void set_data(const std::string& data);
        void set_data(std::deque<line> lines);
        [[nodiscard]] const std::deque<line>& get_data() const noexcept { return _lines; }

        [[nodiscard]] const std::vector<region>& get_selections() const noexcept { return _selections; }

        // checks if there is a cursor at coords
        bool has_cursor(coordinates coords);
        void add_cursor(coordinates coords);
        void remove_cursor(coordinates coords);

        // returns the coordinates of the mouse in editor coordinates
        coordinates mouse_position();

        // Call these if you want ImEdit to scroll up or down the next time it is rendered
        void scroll_up_next_frame() noexcept { _scroll_up_next_frame = true; } // default ctrl+up
        void scroll_down_next_frame() noexcept { _scroll_down_next_frame = true; } // default ctrl+down

        // clears all data (keeps shortcuts)
        void clear();

        // editor._shortcuts_data can be set, and will be passed as parameter to this callback
        void add_shortcut(input in, std::function<void(void* shortcuts_data, editor& this_editor)> callback);
        void add_shortcut(input in, void(editor::*member_function)());
        void add_shortcut(input in, void(editor::*const_member_function)() const);
        void clear_shortcuts() { _shortcuts.clear(); }

        void delete_glyph(coordinates); // deletes the glyph at the given coordinates, ie just before a cursor that would have those coordinates

        void font_changed() const noexcept; // Call this whenever the font is modified

        // hides tooltip, if one is currently being shown
        void reset_current_tooltip();


        static std::vector<std::pair<input, std::function<void(void*, editor&)>>> get_default_shortcuts();
        static style get_default_style(); // similar to monokai


        /**
         * All the following methods call _public_methods_callback
         */
        void add_selection(region r) noexcept;
        void delete_selections(); // delete the contents of every selection

        void copy_to_clipboard() const;
        void paste_from_clipboard();
        void cut_to_clipboard();

        void move_cursors_up();
        void move_cursors_down();
        void move_cursors_left();
        void move_cursors_right();
        void move_cursors_to_end(); // endline
        void move_cursors_to_beg(); // begline
        void move_cursors_left_token(); // moves by one token to the left
        void move_cursors_right_token(); // moves by one token to the right
        void move_cursors_to_endfile();
        void move_cursors_to_begfile();

        // Use the following methods to manipulate the selection for all cursors
        void selection_toggle_right(); // default shift+right
        void selection_toggle_left(); // default shift+left
        void selection_toggle_right_token(); // default shift+ctrl+right
        void selection_toggle_left_token(); // default shift+ctrl+left
        void selection_toggle_up(); // default shift+up
        void selection_toggle_down(); // default shift+down
        void selection_begline(); // default shift+begin
        void selection_endline(); // default shift+end
        void selection_begfile(); // selects from cursors to the beginning of the file (default ctrl+shift+begin)
        void selection_endfile(); // selects from cursors to the end of the file (default : ctrl+shift+end)

        void input_char_utf16(ImWchar c); // Simulates a keyboard input. Moves cursors
        void input_raw_char(char c); // Inputs a specific char. Moves cursors. Do not use input_raw_char('\n'), use input_newline instead.
        void input_newline(); // Inputs a new line. Moves cursors.
        void input_newline_nomove(); // Creates a new empty line. Does not move cursors
        void input_delete(); // deletes the char after each cursor
        void input_backspace(); // deletes the char before each cursor




        bool _allow_keyboard_input{true}; // set to false to inhibit keyboard management
        bool _allow_mouse_input{true}; // set to false to inhibit mouse management
        bool _show_leading_space{true};
        unsigned int _tab_length{4};
        std::optional<float> _height{}; // empty optional <=> height equal content size. value set to 0 <=> take all available height. Other <=> take specified height
        std::optional<float> _width{}; // similar to _height

        // FIXME: set a single accessor for those variables?
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

        void* _shortcuts_data{nullptr}; // Passed to shortcut callbacks as user data

        std::function<void(void* data, std::function<void()>)> _public_methods_callback{}; // TODO explain this. Mention editor::_allow_mouse_input
        void* _public_methods_callback_data{nullptr};


    /*************************************************************************************
     *
     * Private methods and variables
     *
     */
    private:

        // Counts the number of columns in the line in order to attain the specified coordinates
        [[nodiscard]] unsigned int column_count_to(coordinates) const noexcept;

        // Returns the coordinates for given line such that column_count_to(coordinates) returns column_count, if possible for that line.
        // if not possible, returns the max column
        [[nodiscard]] coordinates coordinates_for(unsigned int column_count, unsigned int line) const noexcept;

        [[nodiscard]] coordinates move_coordinates_up(coordinates, unsigned int wanted_column) const noexcept;
        [[nodiscard]] coordinates move_coordinates_down(coordinates, unsigned int wanted_column) const noexcept;
        [[nodiscard]] coordinates move_coordinates_left(coordinates) const noexcept;
        [[nodiscard]] coordinates move_coordinates_right(coordinates) const noexcept;
        [[nodiscard]] coordinates move_coordinates_left_token(coordinates) const noexcept;
        [[nodiscard]] coordinates move_coordinates_right_token(coordinates) const noexcept;
        [[nodiscard]] coordinates move_coordinates_endline(coordinates) const noexcept;
        [[nodiscard]] coordinates move_coordinates_begline(coordinates) const noexcept;

        [[nodiscard]] coordinates_cbl screen_to_token_coordinates(ImVec2 pos);

        [[nodiscard]] bool coordinates_eq(coordinates lhs, coordinates rhs) const noexcept;
        [[nodiscard]] bool coordinates_lt(coordinates lhs, coordinates rhs) const noexcept;
        [[nodiscard]] bool coordinates_lt_eq(coordinates lhs, coordinates rhs) const noexcept;
        [[nodiscard]] bool coordinates_within(coordinates coord, region r) const noexcept; // is in [beg ; end]
        [[nodiscard]] bool coordinates_within_ex(coordinates coord, region r) const noexcept; // is in ]beg ; end[

        [[nodiscard]] region sorted_region(region) const noexcept;
        [[nodiscard]] coordinates& greater_coordinates_of(region&) const noexcept;
        [[nodiscard]] coordinates& smaller_coordinates_of(region&) const noexcept;

        // Deletes cursors that are at the same place to leave only on of them,
        // and merges selections that should be merged together (when using multiple cursors)
        void sanitize_cursors();
        void clear_cursors_within_selections();
        void sanitize_selections(); // merges contiguous selections, delete empty selections and selections that have no associated cursors
        void toggle_selection(region r); // sanitize_selections should be called after a call to this method/after a call to a batch on this method

        [[nodiscard]] ImVec2 glyph_size() const noexcept;

        [[nodiscard]] static ImVec2 calc_text_size(const char* text, const char* text_end = nullptr) noexcept;

        void find_longest_line();
        [[nodiscard]] float calc_line_size(unsigned int line) const noexcept;

        void handle_kb_input();
        void handle_mouse_input();
        float compute_extra_padding() const noexcept;

        // Input a char at a specific cursor coordinates
        void input_raw_char(char c, cursor& pos);

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
        bool _tooltip_has_focus{false};

        std::vector<std::pair<input, std::function<void(void* shortcuts_data, editor& this_editor)>>> _shortcuts{};

        mutable std::optional<ImVec2> _glyph_size{};

        bool _scroll_up_next_frame{false};
        bool _scroll_down_next_frame{false};

        mutable bool _should_call_pmc{true}; // if public function call and this is true, set to false, call _public_methods_callback, and re-set to true before function exit
    };
}

#endif //IMEDIT_EDITOR_H
