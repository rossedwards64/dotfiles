{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.emacs;
in {
  options.modules.emacs = { enable = mkEnableOption "emacs"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ emacs29-pgtk auctex ];

    programs.emacs = {
      extraPackages = with pkgs; epkgs: [ mu mu.mu4e epkgs.mu4e ];
    };

    xdg.configFile = {
      "emacs/init.el" = {
        enable = false;
        source = ../emacs/init.el;
      };

      "emacs/early-init.el" = {
        enable = false;
        source = ../emacs/early-init.el;
      };
    };
  };
}
