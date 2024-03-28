{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.games;
in {
  options.modules.games = { enable = mkEnableOption "games"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      dxvk_2
      fceux
      gamescope
      glxinfo
      lutris
      mupen64plus
      protontricks
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
      steamPackages.steamcmd
      steam-tui
      vkd3d-proton
      vulkan-tools
      zsnes2
      gamemode
      mangohud
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

