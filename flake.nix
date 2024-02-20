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
      username = "ross";
      lib = nixpkgs.lib // home-manager.lib;
      system = "x86_64-linux";

      args = { inherit inputs outputs username; };

      sysModules = [
        ./modules/nixos
        ({ config, pkgs, options, ... }: {
          nix.registry.nixpkgs.flake = nixpkgs;
        })
      ];

      homeConfig = lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system};
        modules = [
          {
            home = {
              username = "${username}";
              homeDirectory = "/home/${username}";
            };
            programs.home-manager.enable = true;
          }
          ./home/home.nix
        ];
        extraSpecialArgs = args;
      };

    in {
      nixosConfigurations = {
        ross-desktop = lib.nixosSystem {
          system = system;
          modules = [ ./hosts/ross-desktop/configuration.nix ] ++ sysModules;
          specialArgs = args;
        };

        ross-thinkpad = lib.nixosSystem {
          system = system;
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-x230
            ./hosts/ross-thinkpad/configuration.nix
          ] ++ sysModules;
          specialArgs = args;
        };

        ross-thinkpad-x200 = lib.nixosSystem {
          system = system;
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-x200s
            ./hosts/ross-thinkpad-x200/configuration.nix
          ] ++ sysModules;
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
