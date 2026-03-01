{ inputs, ... }:
{
  flake.modules = {
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
          heroic
          itch
          mangohud
          mupen64plus
          openjk
          openmw
          openra
          openrct2
          openttd
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
