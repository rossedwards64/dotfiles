{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      services.swayidle = {
        enable = true;

        events = {
          before-sleep = "${pkgs.playerctl}/bin/playerctl pause; ${pkgs.swaylock-effects}/bin/swaylock -fF";
          after-resume = "${pkgs.sway}/bin/swaymsg 'output * power on'";
        };

        timeouts = [
          {
            timeout = 300;
            command = "${pkgs.swaylock-effects}/bin/swaylock -fF";
          }
        ];
      };
    };
}
