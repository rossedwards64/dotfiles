{
  description = "Ross Edwards' NixOS Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, nixos-hardware, ... }@inputs:
    let
      inherit (self) outputs;
      lib = nixpkgs.lib // home-manager.lib;
      sysArch = "x86_64-linux";
      args = { inherit inputs outputs; };

      sysModules = [
        ./configuration.nix
        ({ config, pkgs, options, ... }: {
          nix.registry.nixpkgs.flake = nixpkgs;
        })
      ];

      homeConfig = lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = args;
      };
    in {
      nixosConfigurations = {
        ross-desktop = lib.nixosSystem {
          system = sysArch;
          modules = sysModules;
          specialArgs = args;
        };

        ross-thinkpad = lib.nixosSystem {
          system = sysArch;
          modules = [ nixos-hardware.nixosModules.lenovo-thinkpad-x230 ]
            ++ sysModules;
          specialArgs = args;
        };

        ross-thinkpad-x200 = lib.nixosSystem {
          system = sysArch;
          modules = [ nixos-hardware.nixosModules.lenovo-thinkpad-x200s ]
            ++ sysModules;
          specialArgs = args;
        };
      };

      homeConfigurations = {
        ross-desktop = homeConfig;
        ross-thinkpad = homeConfig;
        ross-thinkpad-x200 = homeConfig;
      };
    };
}
