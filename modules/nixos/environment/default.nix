{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.environment;
in
{
  options.modules.environment = {
    enable = mkEnableOption "environment";
  };

  config = mkIf cfg.enable {
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
}
