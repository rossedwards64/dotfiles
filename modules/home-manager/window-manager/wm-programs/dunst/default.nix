{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
let
  cfg = config.modules.dunst;
  defaultFrameColour = "#89b4fa";
  background = "#1e1e2e";
  foreground = "#cdd6fa";
  criticalFrameColour = "#fab387";
in
{
  options.modules.dunst = {
    enable = mkEnableOption "dunst";
  };

  config = mkIf cfg.enable {
    services.dunst = {
      enable = true;

      iconTheme = {
        name = "rose-pine-moon";
        package = pkgs.rose-pine-icon-theme;
        size = "32x32";
      };

      settings = {
        global = {
          monitor = 0;
          follow = "keyboard";
          width = 300;
          height = 300;
          origin = "top-right";
          offset = "10x10";
          scale = 0;
          notification_limit = 0;
          progress_bar = true;
          progress_bar_height = 10;
          progress_bar_frame_width = 1;
          progress_bar_min_width = 150;
          progress_bar_max_width = 300;
          indicate_hidden = "yes";
          transparency = 0;
          separator_height = 2;
          padding = 8;
          horizontal_padding = 0;
          text_icon_padding = 0;
          frame_width = 3;
          frame_color = defaultFrameColour;
          separator_color = "frame";
          sort = "yes";
          font = "Iosevka 10";
          line_height = 0;
          markup = "full";
          format = "<b>%s</b>\\n%b\\n%p";
          alignment = "left";
          vertical_alignment = "center";
          show_age_threshold = 60;
          ellipsize = "middle";
          ignore_newline = "no";
          stack_duplicates = true;
          hide_duplicate_count = false;
          show_indicators = "yes";
          icon_position = "left";
          min_icon_size = 0;
          max_icon_size = 32;
          sticky_history = "yes";
          history_length = 20;
          browser-open = "xdg-open";
          always_run_script = true;
          title = "Dunst";
          class = "Dunst";
          corner_radius = 10;
          ignore_dbusclose = false;
          force_xwayland = false;
          force_xinerama = false;
          mouse_left_click = "do_action, open_url, close_current";
          mouse_middle_click = "context";
          mouse_right_click = "close_all";
        };

        experimental = {
          per_monitor_dpi = false;
        };

        urgency_low = {
          inherit background foreground;
          timeout = 10;
        };

        urgency_normal = {
          inherit background foreground;
          timeout = 10;
        };

        urgency_critical = {
          inherit background foreground;
          frame_color = criticalFrameColour;
          timeout = 0;
        };

        fullscreen_delay_everything = {
          fullscreen = "delay";
        };

        fullscreen_delay_critical = {
          msg_urgency = "critical";
          fullscreen = "show";
        };

        music = {
          appname = "Music";
        };

        volume = {
          summary = "Volume*";
          highlight = "#CB8CF4";
        };

        battery = {
          appname = "Power Warning";
        };
      };
    };
  };
}
