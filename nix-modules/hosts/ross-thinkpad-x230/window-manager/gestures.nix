{ config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-thinkpad-x230".module = {
    wayland.windowManager.sway.extraConfig = ''
      bindgesture {
          pinch:inward+up move up
          pinch:inward+down move down
          pinch:inward+left move left
          pinch:inward+right move right
          swipe:right workspace prev
          swipe:left workspace next
      }
    '';
  };
}
