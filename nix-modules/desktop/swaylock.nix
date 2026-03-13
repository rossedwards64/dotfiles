{ config, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      programs.swaylock = {
        enable = true;
        package = pkgs.swaylock-effects;

        settings = {
          inherit (config.flake.meta) font;
          font-size = 36;
          daemonize = true;
          clock = true;
          indicator = true;
          screenshots = true;
          grace = 5;
          hide-keyboard-layout = true;
          indicator-radius = 100;
          indicator-thickness = 7;
          fade-in = 0.2;
          effect-blur = "7x5";
          effect-vignette = "0.5:0.5";
        };
      };
    };
}
