{
  inputs,
  ...
}:
{
  config = {
    nix = {
      settings.trusted-users = [
        "ross"
      ];

      nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    };

    nixpkgs = {
      config.allowUnfree = true;
      overlays = [ inputs.emacs-overlay.overlays.default ];
    };
  };
}
