{ inputs, lib, ... }:
{
  flake.modules = {
    nixos.base =
      { pkgs, ... }:
      {
        imports = [
          inputs.nix-gaming.nixosModules.platformOptimizations
        ];

        programs.steam = {
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

          package =
            lib.mkDefault
            <| pkgs.steam.override {
              extraProfile = ''
                unset TZ
              '';
            };

          extraCompatPackages = [
            pkgs.proton-ge-bin
            inputs.dw-proton.packages.${pkgs.stdenv.hostPlatform.system}.dw-proton
            inputs.nix-gaming-edge.packages.${pkgs.stdenv.hostPlatform.system}.proton-cachyos
          ];

          fontPackages = with pkgs; [
            noto-fonts
            noto-fonts-cjk-sans
            noto-fonts-color-emoji
          ];

          extraPackages = with pkgs; [
            SDL
            SDL2_image
            fontconfig
            freetype
            glew_1_10
            keyutils
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
            libdrm
            libidn
            libkrb5
            libogg
            libpng
            libpulseaudio
            libvorbis
            stdenv.cc.cc.lib
            tbb
            zlib
          ];
        };
      };

    homeManager.base =
      { pkgs, ... }:
      {
        imports = [ inputs.steam-config-nix.homeModules.default ];
        home.packages = with pkgs; [ protontricks ];
        programs = {
          mangohud.enable = true;
          steam.config = {
            enable = true;
            closeSteam = true;
            defaultCompatTool = "GE-Proton";
          };
        };
      };
  };
}
