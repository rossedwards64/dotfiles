{ lib, config, ... }:
with lib;
let cfg = config.modules.git;
in {
  options.modules.git = { enable = mkEnableOption "git"; };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      userEmail = "redwards64@hotmail.com";
      userName = "Ross Edwards";
    };
  };
}
