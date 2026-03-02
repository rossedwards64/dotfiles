{ lib, config, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      wayland.windowManager.sway.config.startup = [
        {
          command = "${lib.getExe' pkgs.gnome-keyring "gnome-keyring-daemon"} --start --foreground --components=pkcs11,secrets,ssh";
        }
        { command = "${lib.getExe pkgs.myxer}"; }
        { command = "${lib.getExe pkgs.swayidle} -w"; }
        {
          command = "${lib.getExe' pkgs.wl-clipboard "wl-paste"} --type image --watch cliphist store";
        }
        {
          command = "${lib.getExe' pkgs.wl-clipboard "wl-paste"} --type text --watch cliphist store";
        }
        {
          command = "${lib.getExe pkgs.autotiling} -w 1 2 3 4 5 6 7 8 9 10";
        }
        { command = "${lib.getExe pkgs.alacritty}"; }
        { command = "${lib.getExe pkgs.librewolf}"; }
        { command = "${lib.getExe pkgs.lutris}"; }
        { command = "${lib.getExe' config.flake.pkgs.emacs "emacsclient"} -c -a=''"; }
        {
          command = "${lib.getExe' pkgs.procps "pkill"} fuzzel";
          always = true;
        }
        {
          command = "${lib.getExe' pkgs.systemd "sytemctl"} --user restart waybar";
          always = true;
        }
        {
          command = "${lib.getExe' pkgs.systemd "sytemctl"} --user restart wob";
          always = true;
        }
        {
          command = "${lib.getExe' pkgs.systemd "sytemctl"} --user restart swaync";
          always = true;
        }
        {
          command = "${lib.getExe' pkgs.swaynotificationcenter "swaync-client"} -R -rs -sw";
          always = true;
        }
      ];
    };
}
