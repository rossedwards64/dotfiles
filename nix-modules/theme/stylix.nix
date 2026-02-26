{ inputs, ... }:
let
  stylix.enable = true;
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
