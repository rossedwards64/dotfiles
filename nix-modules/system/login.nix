{ lib, ... }:
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      security.pam.services.swaylock.text = "auth include login";
      environment.etc."greetd/environments".text = ''
        ${lib.getExe pkgs.sway}
        ${lib.getExe pkgs.hyprland}
        ${lib.getExe pkgs.niri}
      '';

      services = {
        udisks2.enable = true;

        greetd = {
          enable = true;

          settings = {
            default_session = {
              user = "ross";
              command = ''
                ${lib.getExe pkgs.tuigreet} --cmd ${lib.getExe pkgs.sway} -t -g \
                    'WELCOME TO WORM LINUX' --asterisks
              '';
            };
          };
        };
      };
    };
}
