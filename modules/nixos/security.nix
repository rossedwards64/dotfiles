{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.security;
in {
  options.modules.security = { enable = mkEnableOption "security"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      gnupg
      openssl
      openssl.dev
      openssl.out
      pinentry
    ];

    security.rtkit.enable = true;

    programs = {
      gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
        pinentryFlavor = "qt";
      };
    };
  };
}
