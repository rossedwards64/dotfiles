{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.pass;
  xdg = config.xdg;
in {
  options.modules.pass = { enable = mkEnableOption "pass"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      pass-git-helper
      passExtensions.pass-audit
      passExtensions.pass-checkup
      passExtensions.pass-genphrase
      passExtensions.pass-import
      passExtensions.pass-otp
      passExtensions.pass-tomb
      passExtensions.pass-update
    ];

    programs.password-store = {
      enable = true;
      package = pkgs.pass-wayland;

      settings = { PASSWORD_STORE_DIR = "${xdg.dataHome}/pass"; };
    };
  };
}
