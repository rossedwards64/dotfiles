{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.pass;
in {
  options.modules.pass = { enable = mkEnableOption "pass"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      pass-git-helper
      pass-wayland
      passExtensions.pass-audit
      passExtensions.pass-checkup
      passExtensions.pass-genphrase
      passExtensions.pass-import
      passExtensions.pass-otp
      passExtensions.pass-tomb
      passExtensions.pass-update
    ];

    programs.password-store = with pkgs; {
      enable = true;
      package = pass-wayland;

      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
      };
    };
  };
}
