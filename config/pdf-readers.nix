{ ... }:
{
  programs.sioyek = {
    enable = true;
    config = {
      "startup_commands" = "toggle_custom_color;toggle_statusbar";
      # default_dark_mode 1
      "toggle_custom_colors" = "1";
      "page_separator_width" = "10";
      "show_doc_path" = "1";
      "show_document_name_in_statusbar" = "1";
      #Theme
      #-------------------------------------------------------------------------------------------------------------
      # base16-sioyek (https://github.com/loiccoyle/base16-sioyek)
      # by Loic Coyle
      # Gruvbox dark, hard scheme byDawid Kurek (dawikur@gmail.com), morhetz (https://github.com/morhetz/gruvbox)
      "custom_background_color" = "#1d2021";
      "custom_text_color" = "#ebdbb2";
      "page_separator_color" = "#1d2021";
      "search_highlight_color" = "#fabd2f";
      "status_bar_color" = "#1d2021";
      "status_bar_text_color" = "#ebdbb2";
      "ui_text_color" = "#ebdbb2";
      "ui_selected_text_color" = "#ebdbb2";
      "ui_background_color" = "#3c3836";
      "ui_selected_background_color" = "#665c54";
      "background_color" = "#1d2021";
      "visual_mark_color" = "0.4 0.36078432 0.32941177 0.2";
      "text_highlight_color" = "#665c54";
      "link_highlight_color" = "#83a598";
      "synctex_highlight_color" = "#fb4934";
      #-------------------------------------------------------------------------------------------------------------
    };
    bindings = {
      "move_left" = "l";
      "move_right" = "h";
      "move_down" = "<C-e>";
      "move_up" = "<C-y>";
      "toggle_statusbar" = "y";
      # toggle_dark_mode i
      "toggle_custom_color" = "i";
      "zoom_in" = "<C-i>";
      "zoom_out" = "<C-o>";
      # fit_to_page_height_smart b
      # fit_to_page_width_smart w
      "fit_to_page_height" = "b";
      "fit_to_page_width" = "w";
      "previous_page" = "<C-h>";
      "next_page" = "<C-l>";
      "new_window" = "<C-n>";
      "close_window" = "q";
      "add_bookmark" = "B";
      "add_highlight" = "H";
      "overview_definition" = "L";
      "visual_mark_under_cursor" = "<C-v>";
    };
  };
}
