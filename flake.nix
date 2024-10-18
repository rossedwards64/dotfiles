{
  description = "Ross Edwards' NixOS Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    ssbm = {
      url = "github:djanatyn/ssbm-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plasma-manager = {
      url = "github:nix-community/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      flake-utils,
      nixos-hardware,
      stylix,
      ssbm,
      plasma-manager,
      ...
    }@inputs:
    let
      inherit (self) outputs;
      username = "ross";
      system = "x86_64-linux";
      window-manager.enable = true;
      kde.enable = !window-manager.enable;
      lib = nixpkgs.lib // home-manager.lib;
      pkgs = nixpkgs.legacyPackages.${system};
      specialArgs = {
        inherit inputs outputs username;
      };
      extraSpecialArgs = specialArgs;

      font = {
        package = (pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; });
        name = "Iosevka NF";
      };

      largeFontSizes = {
        applications = 14;
        terminal = 12;
        desktop = 14;
        popups = 16;
      };

      smallFontSizes = {
        applications = 12;
        terminal = 10;
        desktop = 12;
        popups = 14;
      };

      stylixConfig = {
        base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";

        image = /home/${username}/Pictures/wallpapers + "/Gurren Lagann/simon.jpg";

        cursor = {
          package = pkgs.catppuccin-cursors.mochaDark;
          name = "Catppuccin-Mocha-Dark-Cursors";
        };

        fonts = {
          monospace = {
            inherit (font) package name;
          };
          sansSerif = {
            inherit (font) package name;
          };
          serif = {
            inherit (font) package name;
          };
        };

        polarity = "dark";
      };

      makeSystem =
        hostname: extraModules:
        lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            ./modules/nixos
            ./hosts/${hostname}/configuration.nix
            stylix.nixosModules.stylix
            ssbm.nixosModule
            { stylix = stylixConfig; }
            (
              {
                config,
                pkgs,
                options,
                ...
              }:
              {
                nixpkgs.config.allowUnfree = true;
                stylix.enable = true;
                ssbm = {
                  overlay.enable = true;
                  cache.enable = true;
                  gcc = {
                    oc-kmod.enable = true;
                    rules.enable = true;
                  };
                };

                modules = {
                  boot.enable = true;
                  desktop.enable = true;
                  environment.enable = true;
                  fonts.enable = true;
                  games.enable = true;
                  networking.enable = true;
                  syncthing.enable = true;
                  system.enable = true;
                  user.enable = true;
                };
              }
            )
          ] ++ extraModules;
        };

      makeThinkpad =
        hostname: extraModules:
        makeSystem hostname (
          [
            {
              modules = {
                inherit window-manager kde;
                thinkpad.enable = true;
              };

              stylix.fonts.sizes = {
                inherit (smallFontSizes)
                  applications
                  desktop
                  terminal
                  popups
                  ;
              };
            }
          ]
          ++ extraModules
        );

      makeHome =
        extraModules:
        lib.homeManagerConfiguration {
          inherit pkgs extraSpecialArgs;
          modules = [
            # ssbm.homeManagerModule
            stylix.homeManagerModules.stylix
            plasma-manager.homeManagerModules.plasma-manager
            ./modules/home-manager
            ./home/home.nix
            { stylix = stylixConfig; }
            (
              {
                config,
                pkgs,
                options,
                ...
              }:
              {
                stylix.enable = true;

                modules = {
		  inherit window-manager;
                  alacritty.enable = true;
                  desktop.enable = true;
                  emacs.enable = true;
                  email.enable = true;
                  firefox.enable = true;
                  games.enable = true;
                  music.enable = true;
                  pass.enable = true;
                  starship.enable = true;
                  system.enable = true;
                  theme.enable = true;
                  tmux.enable = true;
                  topgrade.enable = true;
                  zsh.enable = true;
                };
              }
            )
          ] ++ extraModules;
        };
    in
    {
      nixosConfigurations = {
        ross-desktop = makeSystem "ross-desktop" [
          {
            modules = {
	      inherit window-manager kde;
              qemu.enable = true;
            };

            stylix.fonts.sizes = {
              inherit (largeFontSizes)
                applications
                desktop
                terminal
                popups
                ;
            };
          }
        ];

        ross-thinkpad-x230 = makeThinkpad "ross-thinkpad-x230" [
          nixos-hardware.nixosModules.lenovo-thinkpad-x230
        ];

        ross-thinkpad-x200 = makeThinkpad "ross-thinkpad-x200" [
          nixos-hardware.nixosModules.lenovo-thinkpad-x200s
        ];
      };

      homeConfigurations = lib.attrsets.mergeAttrsList (
        builtins.map (host: { ${host} = makeHome [ ]; }) [
          "${username}@ross-desktop"
          "${username}@ross-thinkpad-x230"
          "${username}@ross-thinkpad-x200"
        ]
      );
    };
}
