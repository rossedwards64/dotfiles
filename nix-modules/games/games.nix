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
          faugus-launcher
          fceux
          heroic
          inputs.nix-reshade.packages.${system}.reshade
          inputs.nix-reshade.packages.${system}.reshade-shaders
          itch
          mangohud
          mupen64plus
          openjk
          openra
          openrct2
          openttd
          prismlauncher
          scanmem
          snes9x
          space-station-14-launcher
          srb2
          srb2kart
          umu-launcher
          uzdoom
        ];

        programs = {
          sm64ex = {
            enable = true;
            package = pkgs.sm64coopdx;
          };
        };
      };
  };
}
