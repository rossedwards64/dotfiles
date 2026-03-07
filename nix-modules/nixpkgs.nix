{ inputs, ... }:
let
  config.allowUnfree = true;
in
{
  flake.modules = {
    nixos.base.nixpkgs = {
      inherit config;
      overlays = [ inputs.nix-cachyos-kernel.overlays.pinned ];
    };

    homeManager.base.nixpkgs = {
      inherit config;
      overlays = [
        inputs.emacs-overlay.overlays.default
        inputs.niri-flake.overlays.niri
      ];
    };
  };
}
