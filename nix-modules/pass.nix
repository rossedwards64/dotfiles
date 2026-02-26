{
  flake.modules.homeManager.base =
    { pkgs, config, ... }:
    {
      home.packages = with pkgs; [
        pass-git-helper
        passExtensions.pass-audit
        passExtensions.pass-checkup
        passExtensions.pass-genphrase
        passExtensions.pass-import
        passExtensions.pass-otp
        passExtensions.pass-update
      ];

      programs.password-store = {
        enable = true;
        package = pkgs.pass-wayland;

        settings = {
          PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
        };
      };
    };
}
