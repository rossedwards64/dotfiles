{
  description = "Ross Edwards' NixOS Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    utils.url = "github:numtide/flake-utils";

    hyprland.url = "github:hyprwm/Hyprland";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, nixos-hardware, hyprland
    , hyprland-plugins, ... }@attrs:
    let
      inherit (self) outputs;
      username = "ross";
      system = "x86_64-linux";
      lib = nixpkgs.lib // home-manager.lib;
      pkgs = nixpkgs.legacyPackages.${system};
      specialArgs = { inherit attrs outputs username; };
      extraSpecialArgs = specialArgs;

      makeSystem = hostname: extraModules:
        lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            ./modules/nixos
            ./hosts/${hostname}/configuration.nix
            ({ config, pkgs, options, ... }: {
              nixpkgs.config.allowUnfree = true;
              modules = {
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
            })
          ] ++ extraModules;
        };

      makeHome = extraModules:
        lib.homeManagerConfiguration {
          inherit pkgs extraSpecialArgs;

          modules = [
            ./home/home.nix
            ({ config, pkgs, options, ... }: {
              modules = {
                alacritty.enable = true;
                desktop.enable = true;
                firefox.enable = true;
                emacs.enable = true;
                email.enable = true;
                games.enable = true;
                music.enable = true;
                pass.enable = true;
                programming.enable = true;
                starship.enable = true;
                system.enable = true;
                theme.enable = true;
                tmux.enable = true;
                topgrade.enable = true;
                zsh.enable = true;
              } // extraModules;
            })
          ];
        };
    in {
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
        ross-desktop = makeSystem "ross-desktop" [{
          modules = {
            # kde.enable = true;
            qemu.enable = true;
            window-manager.enable = true;
          };
        }];

        ross-thinkpad-x230 = makeSystem "ross-thinkpad-x230" [
          nixos-hardware.nixosModules.lenovo-thinkpad-x230
          {
            modules = {
              thinkpad.enable = true;
              window-manager.enable = true;
            };
          }
        ];

        ross-thinkpad-x200 = makeSystem "ross-thinkpad-x200" [
          nixos-hardware.nixosModules.lenovo-thinkpad-x200s
          {
            modules = {
              thinkpad.enable = true;
              window-manager.enable = true;
            };
          }
        ];
      };

      home-manager.backupFileExtension = "backup";

      homeConfigurations = {
        ross-desktop = makeHome { window-manager.enable = true; };
        ross-thinkpad-x230 = makeHome { window-manager.enable = true; };
        ross-thinkpad-x200 = makeHome { window-manager.enable = true; };
      };
    };
}
