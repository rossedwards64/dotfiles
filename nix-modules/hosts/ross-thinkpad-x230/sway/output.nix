{ config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-thinkpad-x230".module =
    { pkgs, ... }:
    let
      inherit (config.flake.meta.monitors) laptopScreen;
    in
    {
      wayland.windowManager.sway.config.output.${laptopScreen.name} = {
        scale = "1";
        inherit (laptopScreen) res;
        bg = "${
          pkgs.fetchurl {
            url = "https://static.zerochan.net/Lordgenome.full.198358.jpg";
            sha256 = "sha256-rh4bVRTdM9aoasomvhMQSulGwzc8DgPzP+schDK363Q=";
          }
        } fill";
      };
    };
}
