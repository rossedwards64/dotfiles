{ ... }: {
  imports = [ ./dunst ./fuzzel ./swayidle ./swaylock ./waybar ./wob ];
  programs = { imv.enable = true; };
  services = { playerctld.enable = true; };

  modules = {
    dunst.enable = true;
    fuzzel.enable = true;
    swayidle.enable = true;
    swaylock.enable = true;
    waybar.enable = true;
    wob.enable = true;
  };
}
