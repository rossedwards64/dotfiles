{ config, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      wayland.windowManager.sway.config.startup = [
        {
          command = "${pkgs.gnome-keyring}/bin/gnome-keyring-daemon --start --foreground --components=pkcs11,secrets,ssh";
        }
        { command = "${pkgs.myxer}/bin/myxer"; }
        { command = "${pkgs.swayidle}/bin/swayidle -w"; }
        {
          command = "${pkgs.wl-clipboard}/bin/wl-paste --type image --watch cliphist store";
        }
        {
          command = "${pkgs.wl-clipboard}/bin/wl-paste --type text --watch cliphist store";
        }
        {
          command = "${pkgs.autotiling}/bin/autotiling -w 1 2 3 4 5 6 7 8 9 10";
        }
        { command = "${pkgs.alacritty}/bin/alacritty"; }
        { command = "${pkgs.librewolf}/bin/librewolf"; }
        { command = "${pkgs.lutris}/bin/lutris"; }
        { command = "${config.flake.pkgs.emacs}/bin/emacsclient -c -a=''"; }
        {
          command = "${pkgs.procps}/bin/pkill fuzzel";
          always = true;
        }
        {
          command = "${pkgs.systemd}/bin/systemctl --user restart waybar";
          always = true;
        }
        {
          command = "${pkgs.systemd}/bin/systemctl --user restart wob";
          always = true;
        }
        {
          command = "${pkgs.systemd}/bin/systemctl --user restart swaync";
          always = true;
        }
        {
          command = "${pkgs.swaynotificationcenter}/bin/swaync-client -R -rs -sw";
          always = true;
        }
      ];
    };
}
