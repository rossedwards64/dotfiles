{ inputs, ... }:
{
  flake.modules = {
    nixos.games =
      { pkgs, ... }:
      {
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
                xorg.libX11
                xorg.libXScrnSaver
                xorg.libXcomposite
                xorg.libXcursor
                xorg.libXext
                xorg.libXfixes
                xorg.libXi
                xorg.libXinerama
                xorg.libXmu
                xorg.libXrandr
                xorg.libXt
                xorg.libXtst
                zlib
              ];
          })
        ];

        programs = {
          gamemode.enable = true;
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

        inputs.ssbm = {
          overlay.enable = true;
          cache.enable = true;
          gcc = {
            oc-kmod.enable = true;
            rules.enable = true;
          };
        };
      };

    homeManager.games =
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
