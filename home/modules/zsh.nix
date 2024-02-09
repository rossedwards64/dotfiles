{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.zsh;
in {
  options.modules.zsh = { enable = mkEnableOption "zsh"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ zoxide oh-my-zsh ];

    programs = {
      zoxide = {
        enable = true;
        enableZshIntegration = true;
      };

      zsh = {
        enable = true;
        enableAutosuggestions = true;
        enableCompletion = true;

        sessionVariables = {
          ALTERNATE_EDITOR = "nvim";
          ARCHFLAGS = "-arch x86_64";
          BROWSER = "firefox";
          CARGO_HOME = "$XDG_DATA_HOME/cargo";
          CMAKE_GENERATOR = "Ninja";
          EDITOR = "emacs";
          EMACSDIR = "$XDG_CONFIG_HOME/emacs";
          GDBHISTFILE = "$XDG_DATA_HOME/gdb/history";
          GNUPGHOME = "$XDG_DATA_HOME/gnupg";
          GOPATH = "$XDG_DATA_HOME/go";
          GRADLE_USER_HOME = "$XDG_DATA_HOME/gradle";
          GTK2_RC_FILES = "$XDG_CONFIG_HOME/gtk-2.0/gtkrc";
          HISTCONTROL = "ignoreboth";
          KDEHOME = "$XDG_CONFIG_HOME/kde";
          LEIN_HOME = "$XDG_DATA_HOME/lein";
          MOZ_ENABLE_WAYLAND = 1;
          NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/npmrc";
          PASSWORD_STORE_DIR = "$XDG_DATA_HOME/pass";
          ROSWELL_HOME = "$XDG_CONFIG_HOME/roswell";
          RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
          TEXMFCONFIG = "$XDG_CONFIG_HOME/texlive/texmf-config";
          TEXMFHOME = "$XDG_DATA_HOME/texlive/texmf";
          TEXMFVAR = "$XDG_STATE_HOME/texlive/texmf-var";
          VISUAL = "emacsclient -c -a emacs";
          WAKATIME_HOME = "$XDG_CONFIG_HOME/wakatime";
          WGETRC = "$XDG_CONFIG_HOME/wget/wgetrc";
          WINEPREFIX = "$XDG_DATA_HOME/wineprefixes/default";
          XCURSOR_SIZE = 24;
          XINITRC = "$XDG_CONFIG_HOME/X11/xinitrc";
        };

        initExtraFirst = ''
          if [[ -n "$TERM" ]] && [[ "$TERM" != "dumb" ]]; then
               export BOLD="$(tput bold)"\n
               export MAGENTA="$(tput setaf 5)"\n
               export RED="$(tput setaf 1)"\n
               export CYAN="$(tput setaf 6)"\n
               export RMYELLOW="$(tput setaf 3)"\n
               export GREEN="$(tput setaf 2)"\n
               export BLUE="$(tput setaf 4)"\n
               export NORM="$(tput sgr0)"\n
          fi'';

        initExtraBeforeCompInit = ''
          zstyle ':completion:*' completer _expand _complete _ignored _approximate
                zstyle ':completion:*' cache-path $XDG_CACHE_HOME/zsh/zcompcache
                zstyle :compinstall filename "$XDG_CONFIG_HOME/.zshrc"
                fpath+=$XDG_CONFIG_HOME/.zfunc'';

        completionInit = ''
          autoload -Uz compinit
        '';

        autocd = true;
        dotDir = ".config/zsh";

        shellAliases = {
          vim = "nvim";
          mv = "mv -iv";
          cp = "cp -iv";
          rm = "rm -iv";
          ls = "eza --icons --color=always";
          la = "eza --icons --color=always -ah";
          l = "eza --icons --color=always -lah";
          cd = "z";
          cat = "bat";
          grep = "rg";
          find = "fd";
          du = "dust -Hr";
          clear = "clear && stty sane";
          update-system = "sudo nixos-rebuild switch";
          update-home = "home-manager switch";
        };

        oh-my-zsh = {
          enable = true;
          extraConfig = "zstyle ':omz:update' mode auto";
          plugins = [
            "colored-man-pages"
            "command-not-found"
            "cp"
            "extract"
            "fd"
            "fzf"
            "git"
            "gitfast"
            "gpg-agent"
            "history"
            "isodate"
            "jsontools"
            "lein"
            "otp"
            "pass"
            "ripgrep"
            "ros"
            "rsync"
            "tmux"
            "torrent"
          ];
        };

        syntaxHighlighting = { enable = true; };

        initExtra = "eval $(zoxide init zsh)";
      };
    };
  };
}
