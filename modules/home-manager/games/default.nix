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

    home.packages = with pkgs; [
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
      mangohud
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
      mupen64plus
    ];

    programs = {
      lutris = {
        enable = true;
        extraPackages = with pkgs; [ nspr ];
        protonPackages = with pkgs; [ proton-ge-bin ];
        winePackages = with pkgs; [ wineWow64Packages.full ];
      };

      sm64ex = {
        enable = false;
        package = pkgs.sm64coopdx;
      };
    };
  };
}
