{ lib, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, config, ... }:
    let
      inherit (config) xdg;
      tputCommand = lib.getExe' pkgs.ncurses "tput";
    in
    {
      programs.zsh.initContent = lib.mkBefore ''
        if [[ -n "$TERM" ]] && [[ "$TERM" != "dumb" ]]; then
             export BOLD="$(${tputCommand} bold)"
             export MAGENTA="$(${tputCommand} setaf 5)"
             export RED="$(${tputCommand} setaf 1)"
             export CYAN="$(${tputCommand} setaf 6)"
             export RMYELLOW="$(${tputCommand} setaf 3)"
             export GREEN="$(${tputCommand} setaf 2)"
             export BLUE="$(${tputCommand} setaf 4)"
             export NORM="$(${tputCommand} sgr0)"
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

        eval $(${lib.getExe pkgs.zoxide} init zsh)
      '';
    };
}
