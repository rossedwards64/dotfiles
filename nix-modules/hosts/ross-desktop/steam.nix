{ lib, ... }:
{
  configurations = {
    nixos.ross-desktop.module =
      { pkgs, ... }:
      {
        programs.steam.package =
          lib.mkBefore
          <| pkgs.steam.override {
            extraEnv = {
              AMD_VULKAN_ICD = "RADV";
              ENABLE_LAYER_MESA_ANTI_LAG = "1";
              GAMEMODERUN = "1";
              MANGOHUD = "1";
              MESA_GLSL_CACHE_MAX_SIZE = "16G";
              MESA_SHADER_CACHE_MAX_SIZE = "16G";
              PROTON_ADD_CONFIG = "fsr4rdna3";
              PROTON_LOCAL_SHADER_CACHE = "1";
              PULSE_LATENCY_MSEC = "60";
              WINEDLLOVERRIDES = "dinput8,dxgi,dsound,dbghelp=n,b";
              WINE_VK_VULKAN_ONLY = "1";
            };
          };
      };
  };
}
