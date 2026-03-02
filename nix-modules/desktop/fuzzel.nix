{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      programs.fuzzel = {
        enable = true;

        settings = {
          main = {
            icon-theme = "rose-pine-moon";
            terminal = "${lib.getExe pkgs.alacritty}";
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
