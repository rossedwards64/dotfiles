{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.games;
in {
  options.modules.games = { enable = mkEnableOption "games"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      atlauncher
      devilutionx
      duckstation
      dxvk_2
      fceux
      flycast
      gamemode
      gamescope
      glxinfo
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
      srb2
      srb2kart
      steam-tui
      steamPackages.steamcmd
      vkd3d-proton
      vulkan-tools
      zsnes2

      (steam.override {
        extraPkgs = pkgs:
          with pkgs; [
            xorg.libXcursor
            xorg.libXi
            xorg.libXinerama
            xorg.libXScrnSaver
            libpng
            libpulseaudio
            libvorbis
            stdenv.cc.cc.lib
            libkrb5
            keyutils
          ];
      })
    ];

    programs = {
      steam = {
        enable = true;
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        gamescopeSession = {
          enable = true;
          args = [ "-w 1920" "-h 1080" "-f" ];
        };
      };
    };
  };
}

