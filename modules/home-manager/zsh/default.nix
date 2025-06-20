{
  lib,
  config,
  pkgs,
  inputs,
  system,
  ...
}:
with lib;
let
  cfg = config.modules.zsh;
  xdg = config.xdg;
  emacsPackage = inputs.emacs-overlay.packages.${system}.emacs-git-pgtk;
in
{
  options.modules.zsh = {
    enable = mkEnableOption "zsh";
  };

  config = mkIf cfg.enable {
    programs = {
      zsh = {
        enable = true;
        enableCompletion = true;
        historySubstringSearch.enable = true;

        autosuggestion = {
          enable = true;
          strategy = [
            "history"
            "completion"
          ];
        };

        history = {
          append = true;
          expireDuplicatesFirst = true;
          extended = true;
          ignoreAllDups = true;
          ignoreDups = true;
          ignoreSpace = true;
          path = "${xdg.dataHome}/zsh/history";
          save = 10000;
          share = true;
          size = 10000;
        };

        sessionVariables = {
          _JAVA_AWT_WM_NONREPARENTING = "1";
          ALTERNATE_EDITOR = "${pkgs.neovim}/bin/nvim";
          ARCHFLAGS = "-arch x86_64";
          BROWSER = "${pkgs.firefox}/bin/firefox";
          CARGO_HOME = "${xdg.dataHome}/cargo";
          DIRENV_ALLOW_NIX = "1";
          EDITOR = "${emacsPackage}/bin/emacs";
          EMACSDIR = "${xdg.configHome}/emacs";
          GDBHISTFILE = "${xdg.dataHome}/gdb/history";
          GOPATH = "${xdg.dataHome}/go";
          GRADLE_USER_HOME = "${xdg.dataHome}/gradle";
          KDEHOME = "${xdg.configHome}/kde";
          LEDGER_FILE = "$HOME/Documents/finance/$(${pkgs.coreutils}/bin/date -I | cut -d'-' -f1).journal";
          LEIN_HOME = "${xdg.dataHome}/lein";
          NH_FLAKE = "$HOME/.dotfiles";
          PASSWORD_STORE_DIR = "${xdg.dataHome}/pass";
          PLATFORMIO_CORE_DIR = "${xdg.configHome}/platformio";
          ROSWELL_HOME = "${xdg.configHome}/roswell";
          RUSTUP_HOME = "${xdg.dataHome}/rustup";
          SDL_VIDEODRIVER = "wayland,x11";
          STACK_XDG = 1;
          TEXMFCONFIG = "${xdg.configHome}/texlive/texmf-config";
          TEXMFHOME = "${xdg.dataHome}/texlive/texmf";
          TEXMFVAR = "${xdg.stateHome}/texlive/texmf-var";
          VISUAL = "${emacsPackage}/bin/emacsclient -c -a emacs";
          WAKATIME_HOME = "${xdg.configHome}/wakatime";
          WINEPREFIX = "${xdg.dataHome}/wineprefixes/default";
          XCURSOR_SIZE = 24;
          ZSH_COMPDUMP = "\${ZSH}/cache/.zcompdump-\${HOST}";
        };

        initContent = lib.mkBefore ''
          if [[ -n "$TERM" ]] && [[ "$TERM" != "dumb" ]]; then
               export BOLD="$(${pkgs.ncurses}/bin/tput bold)"
               export MAGENTA="$(${pkgs.ncurses}/bin/tput setaf 5)"
               export RED="$(${pkgs.ncurses}/bin/tput setaf 1)"
               export CYAN="$(${pkgs.ncurses}/bin/tput setaf 6)"
               export RMYELLOW="$(${pkgs.ncurses}/bin/tput setaf 3)"
               export GREEN="$(${pkgs.ncurses}/bin/tput setaf 2)"
               export BLUE="$(${pkgs.ncurses}/bin/tput setaf 4)"
               export NORM="$(${pkgs.ncurses}/bin/tput sgr0)"
          fi

          zstyle ':completion:*' completer _expand _complete _ignored _approximate
          zstyle ':completion:*' cache-path ${xdg.cacheHome}/zsh/zcompcache
          zstyle :compinstall filename "${xdg.configHome}/.zshrc"
          fpath+=${xdg.configHome}/.zfunc

          nixify() {
            if [ ! -e flake.nix ]; then
              nix flake new -t github:nix-community/nix-direnv .
            elif [ ! -e .envrc ]; then
              echo 'use flake' > .envrc
              direnv allow
            fi
          }

          eval $(${pkgs.zoxide}/bin/zoxide init zsh)
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
          update-home = "${pkgs.nh}/bin/nh home switch --backup-extension=bak -- --impure";
          upgrade-home = "${pkgs.nh}/bin/nh home switch --backup-extension=bak --update -- --impure";
          update-system = "${pkgs.nh}/bin/nh os switch -- --impure";
          upgrade-system = "${pkgs.nh}/bin/nh os switch --update -- --impure";
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
            "ros"
            "rsync"
            "tmux"
            "torrent"
          ];
        };

        syntaxHighlighting = {
          enable = true;
          highlighters = [
            "main"
            "brackets"
            "cursor"
            "root"
          ];
        };
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
        git = true;
        icons = "always";
      };

      direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv.enable = true;
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
