{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.games;
in
{
  options.modules.games = {
    enable = mkEnableOption "games";
  };

  config = mkIf cfg.enable {
    nixpkgs.config.permittedInsecurePackages = [
      "dotnet-runtime-6.0.36"
      "dotnet-sdk-6.0.36"
      "dotnet-sdk-6.0.428"
    ];

    environment.systemPackages = with pkgs; [
      #devilutionx
      #fallout2-ce
      duckstation
      fallout-ce
      fceux
      gamemode
      gamescope
      gzdoom
      heroic
      itch
      lutris
      mangohud
      mupen64plus
      openjk
      openmw
      openra
      openrct2
      openttd
      protontricks
      snes9x
      space-station-14-launcher
      srb2
      srb2kart

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
          noto-fonts-emoji
        ];
      };
    };

    ssbm = {
      overlay.enable = true;
      cache.enable = true;
      gcc = {
        oc-kmod.enable = true;
        rules.enable = true;
      };
    };
  };
}
