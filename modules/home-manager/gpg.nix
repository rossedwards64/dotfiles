{ lib, config, pkg, ... }:
with lib;
let
  cfg = config.modules.gpg;  
in {
  options.modules.gpg = { enable = mkEnableOption "gpg"; };

  config = mkIf cfg.enable {
    programs.gpg = {
      enable = true;
      homedir = "${config.xdg.dataHome}/gnupg";
    };

    services.gpg-agent = {
      enable = true;
      pinentryFlavor = "qt";
    };
  };
}
