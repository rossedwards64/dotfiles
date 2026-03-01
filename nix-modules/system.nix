{ inputs, ... }:
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      environment = {
        systemPackages = with pkgs; [
          cachix
          libnotify
          openssl
          openssl.dev
          openssl.out
        ];
      };

      time.timeZone = "Europe/London";
      i18n.defaultLocale = "en_GB.UTF-8";
      console.useXkbConfig = true;

      security = {
        polkit.enable = true;
        rtkit.enable = true;
      };

      hardware = {
        graphics = {
          enable = true;
          enable32Bit = true;
        };
      };

      services = {
        accounts-daemon.enable = true;
        flatpak.enable = true;
        openssh.enable = true;
        jackett.enable = true;
        udev.packages = with pkgs; [
          platformio-core.udev
          openocd
        ];

        pipewire = {
          enable = true;
          audio.enable = true;
          alsa.enable = true;
          alsa.support32Bit = true;
          pulse.enable = true;
          wireplumber.enable = true;
        };
      };
    };
}
