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
#include <regex>
#include <list>
#include <any>

#if __has_include(<imgui.h>)
#include <imgui.h>
#elif __has_include(<imgui/imgui.h>)
#include <imgui/imgui.h>
#else
#error "cannot include <imgui.h> or <imgui/imgui.h>"
#endif

#include "simple_types.h"


namespace ImEdit {

    // TODO add noexcept where it’s due
    class editor {
        /*************************************************************************************
         *
         * Public API
         *
         */
    public:
        // iterates over characters
        struct iterator {
            using difference_type = ptrdiff_t;
            using value_type = char;
            using reference = value_type; // no reference possible because of '\n'
            using pointer = char*;
            using iterator_category = std::bidirectional_iterator_tag;

            iterator() = default;
            iterator(const ImEdit::editor* e, coordinates c) noexcept : ed{e}, current{c} {}
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
            [[nodiscard]] const ImEdit::editor& editor() noexcept { return *ed; }

        private:
            const ImEdit::editor* ed{nullptr};
            coordinates current{};
        };


        explicit editor(std::string id); // id is passed down to imgui

        void render();

        /**
         * The following methods do not call _public_methods_callback
         */
        // TODO specify when those are invalidated
        // iterates over data, giving away char. Output only.
        iterator begin() const noexcept;
        iterator end() const noexcept;

        void set_data(const std::string& data);
        void set_data(std::deque<line> lines);
        [[nodiscard]] const std::deque<line>& get_data() const noexcept { return _lines; }
        void set_line_color(unsigned int line, std::optional<ImColor> color) noexcept;

        [[nodiscard]] const std::vector<region>& get_selections() const noexcept { return _selections; }

        // checks if there is a cursor at coords
        bool has_cursor(coordinates coords);
        void add_cursor(coordinates coords);
        void remove_cursor(coordinates coords);
        void set_cursor(coordinates coords); // Sets a single cursor at given coordinates. Remove other cursors.

        // returns the coordinates of the mouse in editor coordinates
        coordinates mouse_position();

        // Call these if you want ImEdit to scroll up or down the next time it is rendered
        void scroll_up_next_frame() noexcept { _scroll_up_next_frame = true; } // default ctrl+up
        void scroll_down_next_frame() noexcept { _scroll_down_next_frame = true; } // default ctrl+down

        // clears all data (keeps shortcuts)
        void clear();

        // editor._shortcuts_data can be set, and will be passed as parameter to this callback
        void add_shortcut(input in, std::function<void(std::any shortcuts_data, editor& this_editor)> callback);
        void add_shortcut(input in, void(editor::*member_function)());
        void add_shortcut(input in, void(editor::*const_member_function)() const);
        void clear_shortcuts() { _shortcuts.clear(); }

        void delete_glyph(coordinates); // deletes the glyph at the given coordinates, ie just before a cursor that would have those coordinates

        void font_changed() const noexcept; // Call this whenever the font is modified

        // hides tooltip, if one is currently being shown
        void reset_current_tooltip();


        static std::vector<std::pair<input, std::function<void(std::any, editor&)>>> get_default_shortcuts();
        static style get_default_style(); // similar to monokai

        // checks if there is a current regex match (see editor::regex_search)
        bool has_match();
        void clear_search();

        void grab_focus() { _should_grab_focus = true; }

        /**
         * The following methods call _public_methods_callback
         */
        void delete_extra_cursors(); // keeps only one cursor

        void add_selection(region r) noexcept;
        void delete_selections(); // delete the contents of every selection

        // See _undo_history_size
        void undo();
        void redo();

        // discards previous selections, discards all cursors but the last. Call this before calling select_next
        // return true if any match was found
        // Any call to move_cursor_* or input_* clears the search.
        bool regex_search(const std::regex& regex, std::regex_constants::match_flag_type = std::regex_constants::match_default);
        // Selects the next value considered by regex_search
        // returns false if there are no next matches. Calling select_next while on last occurrence will loop back to first occurrence
        bool select_next();
        // returns false if there is no previous match. Calling select_previous while on first occurrence will loop back to last occurrence
        bool select_previous();
        // Selects all matching occurences
        void select_all();

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

        void delete_next_token(); // if something is selected, deletes selection instead (on a per cursor basis). (default : ctrl+del)
        void delete_previous_token(); // if something is selected, deletes selection instead (on a per cursor basis). (default : ctrl+backspace)

        void input_char_utf16(ImWchar c); // Simulates a keyboard input. Moves cursors
        void input_raw_char(char c); // Inputs a specific char. Moves cursors. Do not use input_raw_char('\n'), use input_newline instead.
        void input_newline(); // Inputs a new line. Moves cursors.
        void input_newline_nomove(); // Creates a new empty line. Does not move cursors
        void input_delete(); // deletes the char after each cursor
        void input_backspace(); // deletes the char before each cursor

        // see editor::_breakpoints_tooltip_generator and editor::_breakpoint_toggled
        void add_breakpoint(unsigned int line_no) noexcept;
        void remove_breakpoint(unsigned int line_no) noexcept;
        bool has_breakpoint(unsigned int line_no) noexcept;

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
        using tooltip_callback = std::function<void(std::any tooltip_data, const token&)>;
        std::unordered_map<token, std::variant<std::string, tooltip_callback>, token_hash> _tooltips; // when modifying this, remember to call editor::reset_current_tooltip
        std::any _tooltip_data{nullptr}; // Passed to tooltip callbacks as user data
        std::chrono::milliseconds _tooltip_delay{std::chrono::seconds(1)}; // Delay before the tooltip appears
        std::chrono::milliseconds _tooltip_grace_period{std::chrono::milliseconds(250)}; // Delay for which the tooltip stays up, even after the mouse went away

        std::any _shortcuts_data{nullptr}; // Passed to shortcut callbacks as user data (see editor::add_shortcut)

        std::function<void(std::any data, std::function<void()>)> _public_methods_callback{}; // TODO explain this. Mention editor::_allow_mouse_input
        std::any _public_methods_callback_data{nullptr};

        // data is filled by _breakpoint_data
        std::function<void(std::any data, unsigned int line_number, editor& this_editor)> _breakpoint_window_filler{}; // This function is called to fill the breakpoint tooltip. The tooltip is a regular ImGui window
        std::function<void(std::any data, unsigned int line_number, editor& this_editor)> _breakpoint_toggled{}; // called whenever a breakpoint is added or removed. If empty, breakpoints can’t be added or removed via a click anymore
        std::any _breakpoint_data{nullptr};

        bool _always_show_cursors{false}; // By default, cursor is hidden when editor isn’t focused.

        size_t _undo_history_size{200};




    /*************************************************************************************
     *
     * Utilities methods. Those do not call _public_methods_callback
     *
     */
        [[nodiscard]] bool coordinates_eq(coordinates lhs, coordinates rhs) const noexcept;
        [[nodiscard]] bool coordinates_lt(coordinates lhs, coordinates rhs) const noexcept;
        [[nodiscard]] bool coordinates_lt_eq(coordinates lhs, coordinates rhs) const noexcept;
        [[nodiscard]] bool coordinates_within(coordinates coord, region r) const noexcept; // is in [beg ; end]
        [[nodiscard]] bool coordinates_within_ex(coordinates coord, region r) const noexcept; // is in ]beg ; end[

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

        [[nodiscard]] coordinates_cbl screen_to_token_coordinates(ImVec2 pos) const;

        [[nodiscard]] simple_coord to_simple_coords(coordinates) const noexcept;
        [[nodiscard]] std::vector<simple_coord> cursors_as_simple() const;
        [[nodiscard]] std::vector<simple_coord> cursors_as_simple(const std::vector<cursor>& cursors) const;
        [[nodiscard]] std::vector<simple_region> selections_as_simple() const;
        [[nodiscard]] coordinates from_simple_coords(simple_coord) const noexcept;
        [[nodiscard]] static coordinates from_simple_coords_within(simple_coord, const line&) noexcept;
        [[nodiscard]] std::vector<cursor> cursors_from_simple(const std::vector<simple_coord>&) const;
        [[nodiscard]] std::vector<region> selections_from_simple(const std::vector<simple_region>&) const;

        [[nodiscard]] region sorted_region(region) const noexcept;
        [[nodiscard]] coordinates& greater_coordinates_of(region&) const noexcept;
        [[nodiscard]] coordinates& smaller_coordinates_of(region&) const noexcept;

        void delete_selection(region select); // Does NOT remove select from _selections

        std::optional<unsigned int> region_idx_for_cursor(cursor& c);

        [[nodiscard]] bool is_mouse_in_breakpoint_column() const noexcept;

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

        void render_region_line(region r, unsigned int current_line, ImVec2 draw_region, ImVec2 imgui_cursor, ImColor fill, ImColor outline) noexcept;

        // Input a char at a specific cursor coordinates
        void input_raw_char(char c, cursor& pos);

        void show_tooltip();
        void show_breakpoint_window();

        void add_cursor_undo_record();
        void add_char_deletion_record(std::vector<char>, coordinates, bool deleted_token, const std::vector<cursor>& cursors_location);
        void add_char_addition_record(std::vector<char>, coordinates);
        void add_line_addition_record(coordinates);
        void add_line_deletion_record(coordinates);
        void add_selection_deletion_record(std::vector<line>, region);
        void add_paste_record(const std::vector<coordinates>& coord, std::string data);
        void commit_record(record r);

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

        std::vector<std::pair<input, std::function<void(std::any shortcuts_data, editor& this_editor)>>> _shortcuts{};

        mutable std::optional<ImVec2> _glyph_size{};

        bool _scroll_up_next_frame{false};
        bool _scroll_down_next_frame{false};
        bool _should_grab_focus{false};

        bool _showing_breakpoint_window{false};
        bool _breakpoint_window_just_opened{true};
        unsigned int _selected_breakpoint_line{0};

        mutable bool _should_call_pmc{true}; // if public function call and this is true, set to false, call _public_methods_callback, and re-set to true before function exit
        bool _should_create_records{true}; // set to false when within undo/redo
        bool _cursor_moved_by_action_since_last_record{false}; // set to true when cursor is moved by mouse, arrow moves, or others. Set to false by some add_*_record methods

        std::vector<region> _regex_results{};
        unsigned int _regex_results_index{};

        std::list<record> _undo_record{};
        std::list<record>::iterator _undo_record_it{_undo_record.end()};
    };
}

#endif //IMEDIT_EDITOR_H
