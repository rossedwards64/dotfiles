{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      services.swayidle = {
        enable = true;

        events = {
          before-sleep = "${lib.getExe pkgs.playerctl} pause; ${lib.getExe pkgs.swaylock-effects} -fF";
          after-resume = "${lib.getExe' pkgs.sway "swaymsg"} 'output * power on'";
        };

        timeouts = [
          {
            timeout = 300;
            command = "${lib.getExe pkgs.swaylock-effects} -fF";
          }
        ];
      };
    };
}
