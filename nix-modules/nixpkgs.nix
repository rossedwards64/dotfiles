{ inputs, ... }:
let
  config.allowUnfree = true;
in
{
  flake.modules = {
    nixos.base.nixpkgs = { inherit config; };
    homeManager.base = {
      nixpkgs = {
        inherit config;
        overlays = [ (import inputs.emacs-overlay) ];
      };
    };
  };
}
