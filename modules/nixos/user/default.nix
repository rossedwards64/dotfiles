{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.user;
in {
  options.modules.user = { enable = mkEnableOption "user"; };

  config = mkIf cfg.enable {
    users.users.ross = {
      isNormalUser = true;
      description = "Ross Edwards";
      extraGroups = [ "networkmanager" "wheel" "video" ];
      shell = pkgs.zsh;
      useDefaultShell = true;
    };
  };
}
