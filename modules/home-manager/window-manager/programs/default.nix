{ config, ... }:

{
  imports = [ ./dunst ./rofi ./swayidle ./swaylock ./waybar ./wob ];

  config.modules = {
    dunst.enable = true;
    rofi.enable = true;
    swayidle.enable = true;
    swaylock.enable = true;
    waybar.enable = true;
    wob.enable = true;
  };
}
