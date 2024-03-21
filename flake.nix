{
  description = "Ross Edwards' NixOS Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, nixos-hardware, ... }@attrs:
    let
      inherit (self) outputs;

      username = "ross";
      system = "x86_64-linux";
      lib = nixpkgs.lib // home-manager.lib;
      specialArgs = { inherit attrs outputs username; };
      modules = {
        boot.enable = true;
        desktop.enable = true;
        environment.enable = true;
        networking.enable = true;
        programming.enable = true;
        services.enable = true;
        system.enable = true;
        user.enable = true;
      };

      systemModules = [
        ./modules/nixos
        ({ config, pkgs, options, ... }: { inherit modules; })
      ];

      homeConfig = lib.homeManagerConfiguration {
        modules = [ ./home/home.nix ];
        pkgs = nixpkgs.legacyPackages.${system};
        extraSpecialArgs = specialArgs;
      };
    in {
      nixpkgs.config.allowUnfree = true;
      nix = {
        registry.nixpkgs.flake = nixpkgs;
        package = nixpkgs.nixFlakes;
        settings = {
          auto-optimise-store = true;
          experimental-features = [ "nix-command" "flakes" ];
          allowed-users = [ username ];
        };
      };

      nixosConfigurations = {
        ross-desktop = lib.nixosSystem {
          inherit system specialArgs;
          modules = [ ./hosts/ross-desktop/configuration.nix ] ++ systemModules;
        };

        ross-thinkpad = lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-x230
            ./hosts/ross-thinkpad/configuration.nix
          ] ++ systemModules;
        };

        ross-thinkpad-x200 = lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-x200s
            ./hosts/ross-thinkpad-x200/configuration.nix
          ] ++ systemModules;
        };
      };

      homeConfigurations = {
        ross-desktop = homeConfig;
        ross-thinkpad = homeConfig;
        ross-thinkpad-x200 = homeConfig;
      };
    };
}
