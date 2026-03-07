{
  inputs,
  lib,
  ...
}:
{
  flake.modules = {
    nixos.base = {
      imports = [ inputs.niri-flake.nixosModules.niri ];
      niri-flake.cache.enable = true;
    };

    homeManager.base =
      { pkgs, ... }:
      {
        imports = with inputs.niri-flake.homeModules; [
          niri
          stylix
        ];

        programs.niri = {
          enable = true;
          package = pkgs.niri-unstable;
          settings = {
            input.mod-key = "Super";
            screenshot-path = null;
            prefer-no-csd = true;
            hotkey-overlay.skip-at-startup = true;
            xwayland-satellite = {
              enable = true;
              path = lib.getExe pkgs.xwayland-satellite-unstable;
            };
          };
        };
      };
  };
}
