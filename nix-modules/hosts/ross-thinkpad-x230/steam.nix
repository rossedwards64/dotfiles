{ config, lib, ... }:
{
  configurations = {
    nixos.ross-thinkpad-x230.module =
      { pkgs, ... }:
      {
        programs.steam.package = pkgs.steam.override {
          extraEnv = {
            GAMEMODERUN = "1";
            PROTON_ENABLE_WAYLAND = "1";
            PROTON_USE_NTSYNC = "1";
            PROTON_USE_WINED3D = "1";
          };
        };
      };

    home."${config.flake.meta.user.username}@ross-thinkpad-x230".module =
      { pkgs, ... }:
      {
        programs.steam.config.apps =
          let
            launchOptions.wrappers = [ (lib.getExe' pkgs.gamescope "-f --") ];
          in
          {
            castle-crashers = {
              id = 204360;
              inherit launchOptions;
            };

            guilty-gear-xx-accent-core-plus-r = {
              id = 348550;
              inherit launchOptions;
            };

            sonic-adventure-2 = {
              id = 213610;
              inherit launchOptions;
            };

            sonic-adventure-dx = {
              id = 71250;
              inherit launchOptions;
            };

            worms-armageddon = {
              id = 217200;
              inherit launchOptions;
            };
          };
      };
  };
}
