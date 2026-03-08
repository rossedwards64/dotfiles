{ inputs, lib, ... }:
{
  flake.meta.lib.monitors = {
    getRes =
      monitor:
      let
        res = lib.strings.splitStringBy (
          prev: curr:
          builtins.elem curr [
            "x"
            "@"
          ]
        ) false monitor.res;
      in
      {
        width = builtins.head res;
        height = lib.lists.elemAt res 1;
        refresh = builtins.head <| lib.strings.splitString "Hz" <| lib.lists.elemAt res 2;
      };

    getPos =
      monitor:
      let
        pos = lib.strings.splitString " " monitor.pos;
      in
      {
        x = builtins.head pos;
        y = lib.lists.elemAt pos 1;
      };
  };

  flake.meta.monitors =
    let
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
    in
    {
      hdmi = {
        name = "HDMI-A-1";
        res = "3840x2160@60Hz";
        scale = "2";
        pos = "0 0";
        bg = "${pkgs.fetchurl {
          url = "https://i.pinimg.com/originals/40/e9/da/40e9daa6982435261c840673b008b5dd.jpg";
          sha256 = "sha256-Q8ShPxMnk3TqivNXQ5wcC1fsRE6ISvCDxcEif605c5c=";
        }}";
      };

      dp1 = {
        name = "DP-1";
        res = "1920x1080@144Hz";
        scale = "1";
        pos = "1920 88";
        bg = "${pkgs.fetchurl {
          url = "https://images.alphacoders.com/133/thumb-1920-1334857.png";
          sha256 = "sha256-1bJ4FSrY2G/UKK24w/+sZ5HwpnQgapX4dcRR/j15Jrk=";
        }}";
      };

      dp2 = {
        name = "DP-2";
        res = "1280x1024@75Hz";
        scale = "1";
        pos = "3840 628";
        bg = "${pkgs.fetchurl {
          url = "https://images4.alphacoders.com/128/thumb-1920-1280154.jpg";
          sha256 = "sha256-QxC7Yju8dGZghLOi35ObE9rofONk6Mju+DA2IdvusAI=";
        }}";
      };

      laptopScreen = {
        name = "LVDS-1";
        res = "1366x768@60Hz";
        scale = "1";
        pos = "0 0";
        bg = "${pkgs.fetchurl {
          url = "https://static.zerochan.net/Lordgenome.full.198358.jpg";
          sha256 = "sha256-rh4bVRTdM9aoasomvhMQSulGwzc8DgPzP+schDK363Q=";
        }}";
      };
    };
}
