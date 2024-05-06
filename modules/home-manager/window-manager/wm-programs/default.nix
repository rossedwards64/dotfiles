{ pkgs, ... }: {
  imports = [ ./dunst ./fuzzel ./swayidle ./swaylock ./swaync ./waybar ./wob ];
  home.packages = with pkgs; [ brightnessctl swaynotificationcenter ];

  programs = { imv.enable = true; };

  services = {
    udiskie = {
      enable = true;
      tray = "always";
      notify = true;
      automount = true;
    };

    playerctld.enable = true;
  };

  modules = {
    dunst.enable = true;
    fuzzel.enable = true;
    swayidle.enable = true;
    swaylock.enable = true;
    swaync.enable = true;
    waybar.enable = true;
    wob.enable = true;
  };
}
