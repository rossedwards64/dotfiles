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
    stylix.url = "github:danth/stylix";
  };

  outputs = { self, ... }@inputs:
    let
      inherit (self) outputs;
      username = "ross";
      system = "x86_64-linux";
      nixpkgs = inputs.nixpkgs;
      lib = nixpkgs.lib // inputs.home-manager.lib;
      pkgs = nixpkgs.legacyPackages.${system};
      specialArgs = { inherit inputs outputs username; };
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

      stylix = {
        base16Scheme =
          "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";

        image = /home/${username}/Pictures/wallpapers
          + "/Gurren Lagann/simon.jpg";

        cursor = {
          package = pkgs.catppuccin-cursors.mochaDark;
          name = "Catppuccin-Mocha-Dark-Cursors";
        };

        fonts = {
          monospace = { inherit (font) package name; };
          sansSerif = { inherit (font) package name; };
          serif = { inherit (font) package name; };
        };

        polarity = "dark";
      };

      makeSystem = hostname: extraModules:
        lib.nixosSystem {
          inherit system specialArgs;
          modules = [
            ./modules/nixos
            ./hosts/${hostname}/configuration.nix
            inputs.stylix.nixosModules.stylix
            { inherit stylix; }
            ({ config, pkgs, options, ... }: {
              nixpkgs.config.allowUnfree = true;
              stylix.enable = true;

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
            })
          ] ++ extraModules;
        };

      makeThinkpad = hostname: extraModules:
        makeSystem hostname ([{
          modules = {
            thinkpad.enable = true;
            window-manager.enable = true;
          };
          stylix.fonts.sizes = {
            inherit (smallFontSizes) applications desktop terminal popups;
          };
        }] ++ extraModules);

      makeHome = extraModules:
        lib.homeManagerConfiguration {
          inherit pkgs extraSpecialArgs;
          modules = [
            ./home/home.nix
            inputs.stylix.homeManagerModules.stylix
            { inherit stylix; }
            ({ config, pkgs, options, ... }: {
              stylix.enable = true;
              modules = {
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
                window-manager.enable = true;
                zsh.enable = true;
              };
            })
          ] ++ extraModules;
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
          stylix.fonts.sizes = {
            inherit (largeFontSizes) applications desktop terminal popups;
          };
        }];

        ross-thinkpad-x230 = makeThinkpad "ross-thinkpad-x230"
          [ inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x230 ];

        ross-thinkpad-x200 = makeThinkpad "ross-thinkpad-x200"
          [ inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x200s ];
      };

      homeConfigurations = lib.attrsets.mergeAttrsList
        (builtins.map (host: { ${host} = makeHome [ ]; }) [
          "${username}@ross-desktop"
          "${username}@ross-thinkpad-x230"
          "${username}@ross-thinkpad-x200"
        ]);

    } // inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells = lib.attrsets.mergeAttrsList (builtins.map
          (lang: { ${lang} = import ./dev-shells/${lang} { inherit pkgs; }; }) [
            "c-cpp"
            "clojure"
            "common-lisp"
            "embedded"
            "java"
            "rust"
            "scheme"
          ]);
      });
}
