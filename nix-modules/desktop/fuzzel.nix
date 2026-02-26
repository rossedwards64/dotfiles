{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      programs.fuzzel = {
        enable = true;

        settings = {
          main = {
            icon-theme = "rose-pine-moon";
            terminal = "${pkgs.alacritty}/bin/alacritty";
            anchor = "center";
            exit-on-keyboard-focus-loss = false;
          };

          border = {
            width = 3;
            radius = 20;
          };
        };
      };
    };
}
