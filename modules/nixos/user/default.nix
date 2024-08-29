{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.user;
in
{
  options.modules.user = {
    enable = mkEnableOption "user";
  };

  config = mkIf cfg.enable {
    users.users.ross = {
      isNormalUser = true;
      description = "Ross Edwards";
      extraGroups = [
        "networkmanager"
        "wheel"
        "video"
        "libvirtd"
        "plugdev"
      ];
      shell = pkgs.zsh;
      useDefaultShell = true;
    };
  };
}
