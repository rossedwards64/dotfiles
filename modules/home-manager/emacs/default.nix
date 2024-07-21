{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.emacs;
  emacsPkg = pkgs.emacs29-pgtk;
in {
  options.modules.emacs = { enable = mkEnableOption "emacs"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs;
      [ (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ])) ];

    programs.emacs = {
      enable = true;
      package = emacsPkg;
      extraPackages = epkgs: [
        pkgs.auctex
        pkgs.emacs-all-the-icons-fonts
        pkgs.mu
        pkgs.mu.mu4e
        epkgs.mu4e
      ];
    };

    services.emacs = {
      enable = true;
      package = emacsPkg;
      startWithUserSession = false;
      client = {
        enable = true;
        arguments = [ "-c" "-a=''" ];
      };
    };

    xdg.configFile = {
      "emacs/early-init.el" = {
        enable = false;
        source = ./config/early-init.el;
      };

      "emacs/init.el" = {
        enable = false;
        source = ./config/init.el;
      };
    };
  };
}
