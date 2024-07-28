{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.games;
in {
  options.modules.games = { enable = mkEnableOption "games"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      devilutionx
      duckstation
      dwarf-fortress
      dxvk_2
      fallout-ce
      fallout2-ce
      fceux
      gamemode
      gamescope
      glxinfo
      gzdoom
      heroic
      # itch # dependency is currently broken
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
      steam-tui
      steamPackages.steamcmd
      vkd3d-proton
      vulkan-tools

      (steam.override {
        extraPkgs = pkgs:
          with pkgs; [
            inconsolata
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
      steam = {
        enable = true;
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
        gamescopeSession = {
          enable = true;
          args = [ "-w 1920" "-h 1080" "-f" ];
        };
      };
    };
  };
}

