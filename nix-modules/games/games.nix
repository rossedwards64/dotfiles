{ inputs, ... }:
{
  flake.modules = {
    homeManager.base =
      { pkgs, ... }:
      let
        inherit (pkgs.stdenv.hostPlatform) system;
      in
      {
        nixpkgs.config.permittedInsecurePackages = [
          "dotnet-runtime-6.0.36"
          "dotnet-sdk-6.0.36"
          "dotnet-sdk-6.0.428"
        ];

        home.packages = with pkgs; [
          #devilutionx
          #fallout-ce
          #fallout2-ce
          fceux
          heroic
          inputs.nix-reshade.packages.${system}.reshade
          inputs.nix-reshade.packages.${system}.reshade-shaders
          inputs.umu.packages.${system}.default
          itch
          mangohud
          mupen64plus
          openjk
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
