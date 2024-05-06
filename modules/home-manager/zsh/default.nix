{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.zsh;
  xdg = config.xdg;
in {
  options.modules.zsh = { enable = mkEnableOption "zsh"; };

  config = mkIf cfg.enable {
    programs = {
      zsh = {
        enable = true;
        autosuggestion.enable = true;
        enableCompletion = true;

        history = {
          size = 10000;
          save = 10000;
          path = "${xdg.dataHome}/zsh/history";
          share = true;
          extended = true;
          ignoreDups = true;
          ignoreSpace = true;
          ignoreAllDups = true;
        };

        sessionVariables = {
          FLAKE = "$HOME/.dotfiles";
          ALTERNATE_EDITOR = "${pkgs.neovim}/bin/nvim";
          ARCHFLAGS = "-arch x86_64";
          BROWSER = "${pkgs.firefox}/bin/firefox";
          CARGO_HOME = "${xdg.dataHome}/cargo";
          CMAKE_GENERATOR = "Ninja";
          EDITOR = "${pkgs.emacs}/bin/emacs";
          EMACSDIR = "${xdg.configHome}/emacs";
          GDBHISTFILE = "${xdg.dataHome}/gdb/history";
          GOPATH = "${xdg.dataHome}/go";
          GRADLE_USER_HOME = "${xdg.dataHome}/gradle";
          KDEHOME = "${xdg.configHome}/kde";
          LEIN_HOME = "${xdg.dataHome}/lein";
          NPM_CONFIG_USERCONFIG = "${xdg.configHome}/npm/npmrc";
          PASSWORD_STORE_DIR = "${xdg.dataHome}/pass";
          ROSWELL_HOME = "${xdg.configHome}/roswell";
          RUSTUP_HOME = "${xdg.dataHome}/rustup";
          TEXMFCONFIG = "${xdg.configHome}/texlive/texmf-config";
          TEXMFHOME = "${xdg.dataHome}/texlive/texmf";
          TEXMFVAR = "${xdg.stateHome}/texlive/texmf-var";
          VISUAL = "${pkgs.emacs}/bin/emacsclient -c -a emacs";
          WAKATIME_HOME = "${xdg.configHome}/wakatime";
          WINEPREFIX = "${xdg.dataHome}/wineprefixes/default";
          XCURSOR_SIZE = 24;
          XINITRC = "${xdg.configHome}/X11/xinitrc";
          STACK_XDG = 1;
          ZSH_COMPDUMP = "\${ZSH}/cache/.zcompdump-\${HOST}";
          PLATFORMIO_CORE_DIR = "${xdg.configHome}/platformio";
        };

        initExtraFirst = ''
          if [[ -n "$TERM" ]] && [[ "$TERM" != "dumb" ]]; then
               export BOLD="$(${pkgs.ncurses}/bin/tput bold)"\n
               export MAGENTA="$(${pkgs.ncurses}/bin/tput setaf 5)"\n
               export RED="$(${pkgs.ncurses}/bin/tput setaf 1)"\n
               export CYAN="$(${pkgs.ncurses}/bin/tput setaf 6)"\n
               export RMYELLOW="$(${pkgs.ncurses}/bin/tput setaf 3)"\n
               export GREEN="$(${pkgs.ncurses}/bin/tput setaf 2)"\n
               export BLUE="$(${pkgs.ncurses}/bin/tput setaf 4)"\n
               export NORM="$(${pkgs.ncurses}/bin/tput sgr0)"\n
          fi
        '';

        initExtraBeforeCompInit = ''
          zstyle ':completion:*' completer _expand _complete _ignored _approximate
          zstyle ':completion:*' cache-path ${xdg.cacheHome}/zsh/zcompcache
          zstyle :compinstall filename "${xdg.configHome}/.zshrc"
          fpath+=${xdg.configHome}/.zfunc
        '';

        completionInit = ''
          autoload -Uz compinit
        '';

        autocd = true;
        dotDir = ".config/zsh";

        shellAliases = {
          vim = "${pkgs.neovim}/bin/nvim";
          mv = "${pkgs.coreutils}/bin/mv -iv";
          cp = "${pkgs.coreutils}/bin/cp -iv";
          rm = "${pkgs.coreutils}/bin/rm -iv";
          ls = "${pkgs.eza}/bin/eza --icons --color=always";
          la = "${pkgs.eza}/bin/eza --icons --color=always -ah";
          l = "${pkgs.eza}/bin/eza --icons --color=always -lah";
          cd = "z";
          cat = "${pkgs.bat}/bin/bat";
          grep = "${pkgs.ripgrep}/bin/rg";
          find = "${pkgs.fd}/bin/fd";
          du = "${pkgs.dust}/bin/dust -Hr";
          clear = "clear && ${pkgs.coreutils}/bin/stty sane";
          update-home =
            "${pkgs.nh}/bin/nh home switch --nom -c $HOST -- --impure";
          upgrade-home =
            "${pkgs.nh}/bin/nh home switch --nom --update -c $HOST -- --impure";
          update-system = "${pkgs.nh}/bin/nh os switch --nom -- --impure";
          upgrade-system =
            "${pkgs.nh}/bin/nh os switch --nom --update -- --impure";
          nix-shell = "nix-shell --command zsh";
        };

        oh-my-zsh = {
          enable = true;
          extraConfig = "zstyle ':omz:update' mode auto";
          plugins = [
            "alias-finder"
            "aliases"
            "colored-man-pages"
            "command-not-found"
            "cp"
            "direnv"
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

        initExtra = "eval $(${pkgs.zoxide}/bin/zoxide init zsh)";
      };

      zoxide = {
        enable = true;
        enableZshIntegration = true;
      };

      fzf = {
        enable = true;
        enableZshIntegration = true;
      };

      eza = {
        enable = true;
        enableZshIntegration = true;
      };

      direnv = {
        enable = true;
        enableZshIntegration = true;
      };

      broot = {
        enable = true;
        enableZshIntegration = true;
      };

      starship = {
        enable = true;
        enableZshIntegration = true;
      };
    };
  };
}
