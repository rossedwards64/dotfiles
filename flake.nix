{
  description = "Ross Edwards' NixOS Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    utils.url = "github:numtide/flake-utils";
    hyprland-nix.url = "github:spikespaz/hyprland-nix";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, nixpkgs, home-manager, nixos-hardware, hyprland-nix, ... }@attrs:
    let
      inherit (self) outputs;

      username = "ross";
      system = "x86_64-linux";
      lib = nixpkgs.lib // home-manager.lib;
      pkgs = nixpkgs.legacyPackages.${system};
      specialArgs = { inherit attrs outputs username; };
      extraSpecialArgs = specialArgs;

      defaultNixosModules = {
        boot.enable = true;
        desktop.enable = true;
        environment.enable = true;
        fonts.enable = true;
        games.enable = true;
        networking.enable = true;
        programming.enable = true;
        syncthing.enable = true;
        system.enable = true;
        user.enable = true;
      };

      systemModules = [
        ./modules/nixos
        ({ config, pkgs, options, ... }: {
          nixpkgs.config.allowUnfree = true;
          modules = defaultNixosModules;
        })
      ];

      defaultHomeModules = {
        alacritty.enable = true;
        desktop.enable = true;
        firefox.enable = true;
        emacs.enable = true;
        email.enable = true;
        games.enable = true;
        git.enable = true;
        gpg.enable = true;
        music.enable = true;
        pass.enable = true;
        programming.enable = true;
        starship.enable = true;
        system.enable = true;
        theme.enable = true;
        tmux.enable = true;
        topgrade.enable = true;
        zsh.enable = true;
      };

      homeModules = [ ./home/home.nix ];
    in {
      nix = {
        registry.nixpkgs.flake = nixpkgs;
        package = nixpkgs.nixFlakes;
        settings = {
          auto-optimise-store = true;
          experimental-features = [ "nix-command" "flakes" "store" ];
          allowed-users = [ username ];
        };
      };

      nixosConfigurations = {
        ross-desktop = lib.nixosSystem {
          inherit system specialArgs;
          modules = [ ./hosts/ross-desktop/configuration.nix ] ++ systemModules;
        };

        ross-thinkpad-x230 = lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-x230
            ./hosts/ross-thinkpad-x230/configuration.nix
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

      home-manager.backupFileExtension = "backup";

      homeConfigurations = {
        ross-desktop = lib.homeManagerConfiguration {
          inherit pkgs;
          inherit extraSpecialArgs;

          modules = homeModules ++ [
            ({ config, pkgs, options, ... }: {
              modules = lib.attrsets.mergeAttrsList [
                defaultHomeModules
                { sway.enable = true; }
              ];
            })
          ];
        };

        ross-thinkpad-x230 = lib.homeManagerConfiguration {
          inherit pkgs;
          inherit extraSpecialArgs;

          modules = homeModules ++ [
            ({ config, pkgs, options, ... }: { modules = defaultHomeModules; })
          ];
        };

        ross-thinkpad-x200 = lib.homeManagerConfiguration {
          inherit pkgs;
          inherit extraSpecialArgs;

          modules = homeModules ++ [
            ({ config, pkgs, options, ... }: { modules = defaultHomeModules; })
          ];
        };
      };
    };
}
