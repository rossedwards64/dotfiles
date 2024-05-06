{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.gpg;
  xdg = config.xdg;
in {
  options.modules.gpg = { enable = mkEnableOption "gpg"; };

  config = mkIf cfg.enable {
    programs.gpg = {
      enable = true;
      homedir = "${xdg.dataHome}/gnupg";
    };

    services.gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-qt;
    };
  };
}
