{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.emacs;
  package = pkgs.emacs29-pgtk;
  extraPackages = epkgs: [
    pkgs.auctex
    pkgs.emacs-all-the-icons-fonts
    pkgs.mu
    pkgs.mu.mu4e
    epkgs.mu4e
  ];
in {
  options.modules.emacs = { enable = mkEnableOption "emacs"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs;
      [ (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ])) ];

    programs.emacs = {
      inherit package extraPackages;
      enable = true;
    };

    services.emacs = {
      inherit package;
      enable = true;
      startWithUserSession = "graphical";
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
