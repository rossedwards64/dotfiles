{ inputs, ... }:
{
  flake.modules = {
    nixos.base = {
      imports = [ inputs.nix-index-database.nixosModules.default ];

      nix = {
        extraOptions = "experimental-features = nix-command flakes pipe-operators recursive-nix";
        daemonCPUSchedPolicy = "idle";
        daemonIOSchedClass = "idle";
        nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
        settings = {
          trusted-users = [ "ross" ];
          auto-optimise-store = true;

          substituters = [
            "https://nix-community.cachix.org"
            "https://nix-gaming.cachix.org"
            "https://nix-cache.tokidoki.dev/tokidoki"
            "https://slippi-nix.cachix.org"
          ];

          trusted-public-keys = [
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
            "tokidoki:MD4VWt3kK8Fmz3jkiGoNRJIW31/QAm7l1Dcgz2Xa4hk="
            "slippi-nix.cachix.org-1:2qnPHiOxTRpzgLEtx6K4kXq/ySDg7zHEJ58J6xNDvBo="
          ];
        };
      };
    };

    homeManager.base =
      { pkgs, ... }:
      {
        imports = [ inputs.nix-index-database.homeModules.default ];
        home.packages = with pkgs; [
          nix-health
          nix-info
          nixd
          nixfmt
          nix-output-monitor
          nvd
        ];

        programs = {
          nix-index.enable = true;
          nix-index-database.comma.enable = true;

          nh = {
            enable = true;
            flake = "$HOME/.dotfiles";
            clean = {
              enable = true;
              extraArgs = "--keep-since 3d --keep 2 --optimise";
            };
          };
        };
      };
  };
}
