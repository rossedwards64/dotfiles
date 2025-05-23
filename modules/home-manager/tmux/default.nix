{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.tmux;
  xdg = config.xdg;
in
{
  options.modules.tmux = {
    enable = mkEnableOption "tmux";
  };

  config = mkIf cfg.enable {
    xdg.configFile = {
      "tmuxinator/default.yml".text = ''
        name: default
        root: ~/

        windows:
          - project:
              layout: 5ad0,157x33,0,0{78x33,0,0,0,78x33,79,0[78x16,79,0,9,78x16,79,17,10]}
              panes:
                - clear
                - clear
                - clear
          - misc:
              panes:
                - clear
          - spotify:
              panes:
                - ${pkgs.ncspot}/bin/ncspot
          - monitor:
              panes: 
                - ${pkgs.btop}/bin/btop
      '';
    };

    programs.tmux = {
      enable = true;
      tmuxinator.enable = true;

      baseIndex = 1;
      prefix = "C-Space";
      shell = "${pkgs.zsh}/bin/zsh";
      historyLimit = 100000;

      plugins = with pkgs; [
        tmuxPlugins.better-mouse-mode

        {
          plugin = tmuxPlugins.catppuccin;
          extraConfig = ''
            set -g @catppuccin_flavour 'mocha'
            set -g @catppuccin_date_time "%Y-%m-%d %H:%M"
            set -g @catppuccin_user on
            set -g @catppuccin_host on
            set -g @catppuccin_window_tabs_enabled on
            set -g @catppuccin_window_status_style 'rounded'
            set -g status-right-length 100
            set -g status-right "#{E:@catppuccin_status_application}#{E:@catppuccin_status_directory}#{E:@catppuccin_status_user}#{E:@catppuccin_status_host}"
          '';
        }

        tmuxPlugins.extrakto
        tmuxPlugins.fingers
        tmuxPlugins.fuzzback
        tmuxPlugins.fzf-tmux-url
        tmuxPlugins.mode-indicator
        tmuxPlugins.prefix-highlight

        {
          plugin = tmuxPlugins.resurrect;
          extraConfig = ''
            set -g @resurrect-strategy-vim 'session'
            set -g @resurrect-strategy-nvim 'session'
            set -g @resurrect-capture-pane-contents 'on'          
          '';
        }

        tmuxPlugins.sensible
        tmuxPlugins.tilish
        tmuxPlugins.yank
      ];

      extraConfig = ''
        unbind C-b
        unbind r
        unbind v
        unbind h
        unbind %
        unbind '"'
        unbind w
        unbind n
        unbind -T copy-mode-vi Space;
        unbind -T copy-mode-vi Enter;

        bind n command-prompt "rename-window '%%'"
        bind w new-window -c "#{path_current_path}"
        bind r source-file ${xdg.configHome}/tmux/tmux.conf \; display "Reloaded ${xdg.configHome}/tmux/tmux.conf"
        bind v split-window -h -c "#{pane_current_path}"
        bind h split-window -v -c "#{pane_current_path}"
        bind -nr S-left previous-window
        bind -nr S-right next-window
        bind -nr M-k swap-pane -U
        bind -nr M-j swap-pane -D
        bind -nr C-M-h resize-pane -L 5
        bind -nr C-M-l resize-pane -R 5
        bind -nr C-M-k resize-pane -U 5
        bind -nr C-M-j resize-pane -D 5

        bind -T copy-mode-vi v send-keys -X begin-selection
        bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "wl-copy --primary"

        set -g display-time 4000
        set -g status-interval 1
        set -g mouse on
        set -g allow-rename off
        set -sg escape-time 0
        set -g -a terminal-overrides ',*:Ss=\E[%p1%d q:Se\=E[2 q'
        setw -g monitor-activity on
        setw -g aggressive-resize on
        set-option -g set-titles on
        set-window-option -g pane-base-index 1
        set-window-option -g automatic-rename on
        set-window-option -g mode-keys vi

        set-option -g status-style fg=yellow,bg=black #yellow and base02

        set-window-option -g window-status-style fg=brightblue,bg=default #base0 and default

        set-window-option -g window-status-current-style fg=brightred,bg=default #orange and default

        set-option -g pane-border-style fg=black #base02
        set-option -g pane-active-border-style fg=brightgreen #base01

        set-option -g message-style fg=brightred,bg=black #orange and base01

        set-option -g display-panes-active-colour blue #blue
        set-option -g display-panes-colour brightred #orange

        set-window-option -g clock-mode-colour green #green

        set-window-option -g window-status-bell-style fg=black,bg=red #base02, red

        b_bg="#504945"

        seg_a="#a89984 #282828"
        seg_b="$b_bg #ddc7a1"

        inactive_bg="#32302f"
        inactive_fg="#ddc7a1"
        active_bg=$b_bg
        active_fg="#ddc7a1"

        set -gw window-status-current-style bold
      '';
    };
  };
}
