{ inputs, ... }:
{
  flake.modules = {
    nixos.base =
      { pkgs, ... }:
      {
        imports = with inputs.nix-gaming.nixosModules; [
          platformOptimizations
          wine
        ];

        environment.systemPackages = with pkgs; [
          (steam.override {
            extraLibraries =
              pkgs: with pkgs; [
                SDL
                SDL2_image
                fontconfig
                freetype
                glew110
                keyutils
                libdrm
                libidn
                libkrb5
                libogg
                libpng
                libpulseaudio
                libvorbis
                stdenv.cc.cc.lib
                tbb
                libX11
                libXScrnSaver
                libXcomposite
                libXcursor
                libXext
                libXfixes
                libXi
                libXinerama
                libXmu
                libXrandr
                libXt
                libXtst
                zlib
              ];
          })
        ];

        programs = {
          gamemode.enable = true;
          wine = {
            enable = true;
            ntsync = true;
          };

          steam = {
            enable = true;
            protontricks.enable = true;
            localNetworkGameTransfers.openFirewall = true;
            platformOptimizations.enable = true;
            gamescopeSession = {
              enable = true;
              args = [
                "-w 1920"
                "-h 1080"
                "-f"
              ];
            };

            extraCompatPackages = with pkgs; [
              proton-ge-bin
            ];

            fontPackages = with pkgs; [
              noto-fonts
              noto-fonts-cjk-sans
              noto-fonts-color-emoji
            ];
          };
        };
      };

    homeManager.base =
      { pkgs, ... }:
      {
        nixpkgs.config.permittedInsecurePackages = [
          "dotnet-runtime-6.0.36"
          "dotnet-sdk-6.0.36"
          "dotnet-sdk-6.0.428"
        ];

        home.packages = with pkgs; [
          #devilutionx
          #fallout2-ce
          #fallout-ce
          fceux
          gamemode
          gamescope
          heroic
          itch
          mangohud
          mupen64plus
          mupen64plus
          openjk
          openmw
          openra
          openrct2
          openttd
          protontricks
          scanmem
          snes9x
          space-station-14-launcher
          srb2
          srb2kart
          uzdoom
        ];

        programs = {
          lutris = {
            enable = true;
            extraPackages = with pkgs; [ nspr ];
            protonPackages = with pkgs; [ proton-ge-bin ];
            winePackages = with pkgs; [ wineWow64Packages.full ];
          };

          sm64ex = {
            enable = true;
            package = pkgs.sm64coopdx;
          };
        };
      };
  };
}
