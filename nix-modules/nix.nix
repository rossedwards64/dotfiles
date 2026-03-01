{ inputs, ... }:
{
  flake.modules = {
    nixos.base.nix = {
      extraOptions = "experimental-features = nix-command flakes pipe-operators recursive-nix";
      nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
      settings.trusted-users = [
        "ross"
      ];
    };

    homeManager.base =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          nix-health
          nix-info
          nixd
          nixfmt
          nix-output-monitor
          nvd
        ];

        programs.nh = {
          enable = true;
          clean = {
            enable = true;
            extraArgs = "--keep-since 3d --keep 2";
          };
        };
      };
  };
}
