{ config, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      wayland.windowManager.sway.extraConfig = ''
        bindsym --locked {
            XF86AudioRaiseVolume exec ${config.flake.scripts.runWob}/bin/wob "-t" "volume" "-v" "5+"
            XF86AudioLowerVolume exec ${config.flake.scripts.runWob}/bin/wob "-t" "volume" "-v" "5-"
            XF86AudioMute exec ${config.flake.scripts.toggleMute}/bin/toggle-mute -s
            XF86AudioMicMute exec ${config.flake.scripts.toggleMute}/bin/toggle-mute -m
            XF86AudioPlay exec ${pkgs.playerctl}/bin/playerctl play-pause
            XF86AudioNext exec ${pkgs.playerctl}/bin/playerctl next
            XF86AudioPrev exec ${pkgs.playerctl}/bin/playerctl previous
            XF86MonBrightnessUp exec ${config.flake.scripts.runWob}/bin/wob "-t" "brightness" "-v" "5+"
            XF86MonBrightnessDown exec ${config.flake.scripts.runWob}/bin/wob "-t" "brightness" "-v" "5-"
        }
      '';
    };
}
