{ lib, config, ... }:
{
  configurations = {
    nixos.ross-desktop.module =
      { pkgs, ... }:
      {
        programs.steam.package =
          lib.mkBefore
          <| pkgs.steam.override {
            extraEnv = {
              GAMEMODERUN = "1";
              MANGOHUD = "1";
              AMD_VULKAN_ICD = "RADV";
              PROTON_ADD_CONFIG = "fsr4rdna3";
              PROTON_LOCAL_SHADER_CACHE = "1";
              ENABLE_LAYER_MESA_ANTI_LAG = "1";
              MESA_SHADER_CACHE_MAX_SIZE = "16G";
              WINE_VK_VULKAN_ONLY = "1";
              MESA_GLSL_CACHE_MAX_SIZE = "16G";
              WINEDLLOVERRIDES = "dinput8,dxgi,dsound=n,b";
            };
          };
      };
  };
}
