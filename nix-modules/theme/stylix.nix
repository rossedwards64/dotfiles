{ inputs, ... }:
let
  stylix = {
    enable = true;
    opacity = {
      desktop = 0.9;
      terminal = 0.9;
    };
  };
in
{
  flake.modules = {
    nixos.base = {
      imports = [
        inputs.stylix.nixosModules.stylix
        { inherit stylix; }
      ];
    };

    homeManager.base = {
      imports = [
        inputs.stylix.homeModules.stylix
        { inherit stylix; }
      ];
    };
  };
}
