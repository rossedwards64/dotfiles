{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.games;
in {
  options.modules.games = { enable = mkEnableOption "games"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ scanmem ];

    programs.sm64ex = { enable = true; };
  };
}
