{ config, lib, pkgs, ... }:
with lib;
let
  unstable = import <unstable> { };
  cfg = config.modules.alacritty;

  title = "Alacritty";
  term_font = "Iosevka NF";
  pos_x = 0;
  pos_y = 0;
  pad_x = 10;
  pad_y = 0;
  off_x = 0;
  off_y = 0;
  gloff_x = 0;
  gloff_y = 0;
in {
  options.modules.alacritty = { enable = mkEnableOption "alacritty"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ (with unstable; alacritty) ];

    xdg.configFile."alacritty/alacritty.toml".source =
      ((pkgs.formats.toml { }).generate "alacritty-config" {
        ipc_socket = true;
        live_config_reload = true;

        scrolling = {
          history = 10000;
          multiplier = 3;
        };

        selection.semantic_escape_chars = '',│`|:"' ()[]{}<>	'';

        shell = {
          args = [ "-l" "-c" "tmuxinator default" ];
          program = "/home/ross/.nix-profile/bin/zsh";
        };

        window = {
          decorations = "full";
          decorations_theme_variant = "None";
          dynamic_padding = true;
          dynamic_title = true;
          opacity = 1.0;

          class = {
            general = title;
            instance = title;
          };

          padding = {
            x = pad_x;
            y = pad_y;
          };

          position = {
            x = pos_x;
            y = pos_y;
          };
        };

        bell = {
          animation = "EaseOutCubic";
          color = "#ffffff";
          command = "None";
          duration = 0;
        };

        colors = {
          draw_bold_text_with_bright_colors = false;
          transparent_background_colors = true;
        };

        cursor = {
          blink_interval = 750;
          thickness = 0.15;
          unfocused_hollow = true;

          style = {
            blinking = "On";
            shape = "Block";
          };
        };

        debug = {
          highlight_damage = false;
          log_level = "Warn";
          persistent_logging = false;
          print_events = false;
          render_timer = false;
          renderer = "None";
        };

        env = { TERM = "alacritty"; };

        font = {
          builtin_box_drawing = true;
          size = 12.0;

          normal = {
            family = term_font;
            style = "Regular";
          };

          italic = {
            family = term_font;
            style = "Italic";
          };

          bold = {
            family = term_font;
            style = "Bold";
          };

          bold_italic = {
            family = term_font;
            style = "Bold Italic";
          };

          glyph_offset = {
            x = gloff_x;
            y = gloff_y;
          };

          offset = {
            x = off_x;
            y = off_y;
          };
        };

        hints = {
          alphabet = "jfkdls;ahgurieowpq";

          enabled = [{
            command = "xdg-open";
            post_processing = true;
            regex = ''
              (ipfs:|ipns:|magnet:|mailto:|gemini:|gopher:|https:|http:|news:|file:|git:|ssh:|ftp:)[^u0000-u001Fu007F-<>"\s{-}\^⟨⟩`]+'';
            binding = {
              key = "U";
              mods = "Control|Shift";
            };
            mouse = {
              enabled = true;
              mods = "None";
            };
          }];
        };

        keyboard.bindings = [
          {
            action = "Paste";
            key = "Paste";
          }
          {
            action = "Copy";
            key = "Copy";
          }
          {
            action = "ClearLogNotice";
            key = "L";
            mods = "Control";
          }
          {
            chars = "f";
            key = "L";
            mode = "~Vi|~Search";
            mods = "Control";
          }
          {
            action = "ScrollPageUp";
            key = "PageUp";
            mode = "~Alt";
            mods = "Shift";
          }
          {
            action = "ScrollPageDown";
            key = "PageDown";
            mode = "~Alt";
            mods = "Shift";
          }
          {
            action = "ScrollToTop";
            key = "Home";
            mode = "~Alt";
            mods = "Shift";
          }
          {
            action = "ScrollToBottom";
            key = "End";
            mode = "~Alt";
            mods = "Shift";
          }
          {
            action = "ClearSelection";
            key = "Escape";
            mode = "Vi|~Search";
          }
          {
            action = "ToggleViMode";
            key = "I";
            mode = "Vi|~Search";
          }
          {
            action = "ScrollToBottom";
            key = "I";
            mode = "Vi|~Search";
          }
          {
            action = "ToggleViMode";
            key = "C";
            mode = "Vi|~Search";
            mods = "Control";
          }
          {
            action = "ScrollLineUp";
            key = "Y";
            mode = "Vi|~Search";
            mods = "Control";
          }
          {
            action = "ScrollLineDown";
            key = "E";
            mode = "Vi|~Search";
            mods = "Control";
          }
          {
            action = "ScrollToTop";
            key = "G";
            mode = "Vi|~Search";
          }
          {
            action = "ScrollToBottom";
            key = "G";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "ScrollPageUp";
            key = "B";
            mode = "Vi|~Search";
            mods = "Control";
          }
          {
            action = "ScrollPageDown";
            key = "F";
            mode = "Vi|~Search";
            mods = "Control";
          }
          {
            action = "ScrollHalfPageUp";
            key = "U";
            mode = "Vi|~Search";
            mods = "Control";
          }
          {
            action = "ScrollHalfPageDown";
            key = "D";
            mode = "Vi|~Search";
            mods = "Control";
          }
          {
            action = "Copy";
            key = "Y";
            mode = "Vi|~Search";
          }
          {
            action = "ClearSelection";
            key = "Y";
            mode = "Vi|~Search";
          }
          {
            action = "ClearSelection";
            key = "Copy";
            mode = "Vi|~Search";
          }
          {
            action = "ToggleNormalSelection";
            key = "V";
            mode = "Vi|~Search";
          }
          {
            action = "ToggleLineSelection";
            key = "V";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "ToggleBlockSelection";
            key = "V";
            mode = "Vi|~Search";
            mods = "Control";
          }
          {
            action = "ToggleSemanticSelection";
            key = "V";
            mode = "Vi|~Search";
            mods = "Alt";
          }
          {
            action = "Open";
            key = "Return";
            mode = "Vi|~Search";
          }
          {
            action = "Up";
            key = "K";
            mode = "Vi|~Search";
          }
          {
            action = "Down";
            key = "J";
            mode = "Vi|~Search";
          }
          {
            action = "Left";
            key = "H";
            mode = "Vi|~Search";
          }
          {
            action = "Right";
            key = "L";
            mode = "Vi|~Search";
          }
          {
            action = "Up";
            key = "Up";
            mode = "Vi|~Search";
          }
          {
            action = "Down";
            key = "Down";
            mode = "Vi|~Search";
          }
          {
            action = "Left";
            key = "Left";
            mode = "Vi|~Search";
          }
          {
            action = "Right";
            key = "Right";
            mode = "Vi|~Search";
          }
          {
            action = "First";
            key = "Key0";
            mode = "Vi|~Search";
          }
          {
            action = "Last";
            key = "Key4";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "FirstOccupied";
            key = "Key6";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "High";
            key = "H";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "Middle";
            key = "M";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "Low";
            key = "L";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "SemanticLeft";
            key = "B";
            mode = "Vi|~Search";
          }
          {
            action = "SemanticRight";
            key = "W";
            mode = "Vi|~Search";
          }
          {
            action = "SemanticRightEnd";
            key = "E";
            mode = "Vi|~Search";
          }
          {
            action = "WordLeft";
            key = "B";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "WordRight";
            key = "W";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "WordRightEnd";
            key = "E";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "Bracket";
            key = "Key5";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "SearchForward";
            key = "Slash";
            mode = "Vi|~Search";
          }
          {
            action = "SearchBackward";
            key = "Slash";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "SearchNext";
            key = "N";
            mode = "Vi|~Search";
          }
          {
            action = "SearchPrevious";
            key = "N";
            mode = "Vi|~Search";
            mods = "Shift";
          }
          {
            action = "SearchConfirm";
            key = "Return";
            mode = "Search|Vi";
          }
          {
            action = "SearchCancel";
            key = "Escape";
            mode = "Search";
          }
          {
            action = "SearchCancel";
            key = "C";
            mode = "Search";
            mods = "Control";
          }
          {
            action = "SearchClear";
            key = "U";
            mode = "Search";
            mods = "Control";
          }
          {
            action = "SearchDeleteWord";
            key = "W";
            mode = "Search";
            mods = "Control";
          }
          {
            action = "SearchHistoryPrevious";
            key = "P";
            mode = "Search";
            mods = "Control";
          }
          {
            action = "SearchHistoryNext";
            key = "N";
            mode = "Search";
            mods = "Control";
          }
          {
            action = "SearchHistoryPrevious";
            key = "Up";
            mode = "Search";
          }
          {
            action = "SearchHistoryNext";
            key = "Down";
            mode = "Search";
          }
          {
            action = "SearchFocusNext";
            key = "Return";
            mode = "Search|~Vi";
          }
          {
            action = "SearchFocusPrevious";
            key = "Return";
            mode = "Search|~Vi";
            mods = "Shift";
          }
          {
            action = "Paste";
            key = "V";
            mode = "~Vi";
            mods = "Control|Shift";
          }
          {
            action = "Copy";
            key = "C";
            mods = "Control|Shift";
          }
          {
            action = "SearchForward";
            key = "F";
            mode = "~Search";
            mods = "Control|Shift";
          }
          {
            action = "SearchBackward";
            key = "B";
            mode = "~Search";
            mods = "Control|Shift";
          }
          {
            action = "ClearSelection";
            key = "C";
            mode = "Vi|~Search";
            mods = "Control|Shift";
          }
          {
            action = "PasteSelection";
            key = "Insert";
            mods = "Shift";
          }
          {
            action = "ResetFontSize";
            key = "Key0";
            mods = "Control";
          }
          {
            action = "IncreaseFontSize";
            key = "Equals";
            mods = "Control";
          }
          {
            action = "IncreaseFontSize";
            key = "Plus";
            mods = "Control";
          }
          {
            action = "IncreaseFontSize";
            key = "NumpadAdd";
            mods = "Control";
          }
          {
            action = "DecreaseFontSize";
            key = "Minus";
            mods = "Control";
          }
        ];
      });
  };
}
