{ lib, config, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      wayland.windowManager.sway.extraConfig = ''
        bindsym --locked {
            XF86AudioRaiseVolume exec ${lib.getExe config.flake.scripts.runWob} "-t" "volume" "-v" "5+"
            XF86AudioLowerVolume exec ${lib.getExe config.flake.scripts.runWob} "-t" "volume" "-v" "5-"
            XF86AudioMute exec ${lib.getExe config.flake.scripts.toggleMute} -s
            XF86AudioMicMute exec ${lib.getExe config.flake.scripts.toggleMute} -m
            XF86AudioPlay exec ${lib.getExe pkgs.playerctl} play-pause
            XF86AudioNext exec ${lib.getExe pkgs.playerctl} next
            XF86AudioPrev exec ${lib.getExe pkgs.playerctl} previous
            XF86MonBrightnessUp exec ${lib.getExe config.flake.scripts.runWob} "-t" "brightness" "-v" "5+"
            XF86MonBrightnessDown exec ${lib.getExe config.flake.scripts.runWob} "-t" "brightness" "-v" "5-"
        }
      '';
    };
}
