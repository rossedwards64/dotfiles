{ lib, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, config, ... }:
    let
      xdg = config.xdg;
    in
    {
      programs.zsh.initContent = lib.mkBefore ''
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
    };
}
