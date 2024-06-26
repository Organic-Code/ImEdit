#ifndef IMEDIT_EDITOR_H
#define IMEDIT_EDITOR_H

#include <vector>
#include <string>
#include <deque>
#include <optional>
#include <unordered_map>

namespace ImEdit {


#ifndef IMEDIT_CUSTOM_TOKEN_TYPES
#define IMEDIT_CUSTOM_TOKEN_TYPES
#endif

    struct token_type {
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
        std::string_view tooltip{}; // todo : move to an unordered map <token, string> in editor ? (using id + data)
        std::byte id{0}; // if two tokens have data and id both set as equal by lexer, selecting one will highlight the other. 0 = ignored
    };

    struct line {
        std::deque<token> tokens{};
        std::optional<ImColor> background{};
    };

    struct style {
        ImColor cursor_color{};
        ImColor background_color{};
        ImColor selection_color{};
        ImColor line_number_color{};
        ImColor line_number_separator_color{};
        ImColor current_line_color{};
        std::unordered_map<token_type::enum_, ImColor, token_type> token_colors{};
    };

    struct coordinates {
        unsigned int line{};
        unsigned int token{};
        unsigned int glyph{}; // index within the token
    };

    // used to represent coordinates that can be to the left of the glyphs (in the numbers column)
    struct coordinates_cbl : coordinates {
        bool is_left{false}; // true: token == 0, glyph indicates how many glyphs to the left the coord is.
    };

    struct cursor {
        coordinates coord{};
        unsigned int wanted_column{}; // where the cursor wishes to be at (can be more to the right than possible on a given line)
    };


    class editor {
    public:
        editor();

        void render();
        void set_data(const std::string& data);
        void set_data(std::deque<line> lines) {
            _lines = std::move(lines);
        }

        [[nodiscard]] style& get_style() noexcept {
            return _style;
        }

        [[nodiscard]] std::string_view store_tooltip(std::string tooltip) {
            _tooltips.push_back(std::move(tooltip));
            return _tooltips.back();
        }

        void cursor_add(coordinates coords);
        void cursor_remove(coordinates coords);

        void cursor_move_up();
        void cursor_move_down();
        void cursor_move_left();
        void cursor_move_right();
        void cursor_move_to_end(); // endline
        void cursor_move_to_beg(); // begline

        void clear();

        void delete_glyph(coordinates);

        static style get_default_style();

        bool _allow_keyboard_input{true};
        bool _allow_mouse_input{true};
        bool _show_leading_space{true};
        unsigned int _tab_length{4};
        std::optional<float> _draw_height{}; // TODO
        std::optional<float> _draw_width{}; // TODO

    private:

        // Counts the number of columns in the line in order to attain the specified coordinates
        [[nodiscard]] unsigned int column_count_to(coordinates) const noexcept;

        // Returns the coordinates for given line such that column_count_to(coordinates) returns column_count, if possible for that line
        [[nodiscard]] coordinates coordinates_for(unsigned int column_count, unsigned int line) const noexcept;

        [[nodiscard]] coordinates coordinates_move_up(coordinates, unsigned int wanted_column) const noexcept;
        [[nodiscard]] coordinates coordinates_move_down(coordinates, unsigned int wanted_column) const noexcept;
        [[nodiscard]] coordinates coordinates_move_left(coordinates) const noexcept;
        [[nodiscard]] coordinates coordinates_move_right(coordinates) const noexcept;

        [[nodiscard]] coordinates_cbl screen_to_token_coordinates(ImVec2 pos);

        void delete_non_unique_cursors();

        void find_longest_line();
        [[nodiscard]] float calc_line_size(unsigned int line, float space_size = ImGui::CalcTextSize(" ").x) const noexcept;

        void handle_kb_input();
        void handle_mouse_input();
        float compute_extra_padding() const noexcept;

        std::vector<cursor> _cursors{};
        std::deque<std::string> _tooltips{};
        std::deque<line> _lines{};
        style _style{};

        unsigned int _longest_line_idx{};
        float _longest_line_px{300};

        ImVec2 _imgui_cursor_position;
    };
}

#endif //IMEDIT_EDITOR_H
