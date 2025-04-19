{
  lib,
  config,
  pkgs,
  inputs,
  system,
  ...
}:
with lib;
let
  cfg = config.modules.emacs;
  package = inputs.emacs-overlay.packages.${system}.emacs-git-pgtk;
in
{
  options.modules.emacs = {
    enable = mkEnableOption "emacs";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      (aspellWithDicts (
        dicts: with dicts; [
          en
          en-computers
          en-science
        ]
      ))
      auctex
      emacs-all-the-icons-fonts
      mu
      mu.mu4e
    ];

    programs.emacs = {
      inherit package;
      enable = true;
    };

    services.emacs = {
      inherit package;
      enable = true;
      startWithUserSession = "graphical";
      client = {
        enable = true;
        arguments = [
          "-c"
          "-a=''"
        ];
      };
    };

    xdg.configFile = {
      "emacs/early-init.el".enable = false;
      "emacs/init.el".enable = false;
    };
  };
}
