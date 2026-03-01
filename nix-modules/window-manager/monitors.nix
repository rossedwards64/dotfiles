{ ... }:
{
  flake.meta.monitors = {
    hdmi = {
      name = "HDMI-A-1";
      res = "3840x2160@60Hz";
    };

    dp1 = {
      name = "DP-1";
      res = "1920x1080@144Hz";
    };

    dp2 = {
      name = "DP-2";
      res = "1280x1024@75Hz";
    };

    laptopScreen = {
      name = "LVDS-1";
      res = "1366x768";
    };
  };
}
