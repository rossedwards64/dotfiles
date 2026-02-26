{ inputs, ... }:
{
  flake.modules.nixos.base.nix = {
    extraOptions = "experimental-features = nix-command flakes pipe-operators recursive-nix";
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    settings.trusted-users = [
      "ross"
    ];

  };
}
