{ config, lib, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-thinkpad-x230".module =
    { pkgs, ... }:
    {
      programs.steam.config.apps =
        let
          gameModeRun = lib.getExe' pkgs.gamemode "gamemoderun";
          gamescope = "${lib.getExe pkgs.gamescope} -f";
          mangoHud = lib.getExe pkgs.mangohud;
          envVars = "env PROTON_USE_WINED3D=1 PROTON_USE_NTSYNC=1";
          launchOptionsStr = "${gameModeRun} ${gamescope} -- ${mangoHud} ${envVars} %command%";
        in
        {
          castle-crashers = {
            id = 204360;
            inherit launchOptionsStr;
          };
          guilty-gear-xx-accent-core-plus-r = {
            id = 348550;
            inherit launchOptionsStr;
          };
          sonic-adventure-2 = {
            id = 213610;
            inherit launchOptionsStr;
          };
          sonic-adventure-dx = {
            id = 71250;
            inherit launchOptionsStr;
          };
          worms-armageddon = {
            id = 217200;
            inherit launchOptionsStr;
          };
        };
    };
}
