{
  ...
}:
{
  flake.modules = {
    nixos.base =
      { pkgs, ... }:
      {
        programs.zsh.enable = true;

        environment = {
          systemPackages = with pkgs; [ zsh ];

          sessionVariables = rec {
            XDG_CACHE_HOME = "$HOME/.cache";
            XDG_CONFIG_HOME = "$HOME/.config";
            XDG_DATA_HOME = "$HOME/.local/share";
            XDG_STATE_HOME = "$HOME/.local/state";
            XDG_BIN_HOME = "$HOME/.local/bin";
            PATH = [ "${XDG_BIN_HOME}" ];
          };
        };
      };

    homeManager.base =
      { pkgs, config, ... }:
      let
        inherit (config) xdg;
      in
      {
        home.packages = with pkgs; [
          bc
          dust
          ffmpeg
          hledger
          imv
          just
          killall
          neovim
          p7zip
          procps
          rar
          rizin
          rlwrap
          rsync
          stow
          tokei
          unzip
          wget
          wl-clipboard
        ];

        programs = {
          zsh = {
            enable = true;
            enableCompletion = true;
            autocd = true;
            dotDir = "${xdg.configHome}/zsh";
            historySubstringSearch.enable = true;
            completionInit = "autoload -Uz compinit";

            autosuggestion = {
              enable = true;
              strategy = [
                "history"
                "completion"
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
        };
      };
  };
}
