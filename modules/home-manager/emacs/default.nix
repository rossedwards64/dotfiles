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
  emacs-git-pgtk = inputs.emacs-overlay.packages.${system}.emacs-git-pgtk;
  emacsPackages = (
    epkgs: with pkgs; [
      (aspellWithDicts (
        dicts: with dicts; [
          en
          en-computers
          en-science
        ]
      ))
      auctex
      emacs-all-the-icons-fonts
      epkgs.vterm
      mu
      mu.mu4e
      (epkgs.treesit-grammars.with-grammars (grammars: with grammars; [ tree-sitter-bash ]))
    ]
  );

  emacsPackage = (pkgs.emacsPackagesFor emacs-git-pgtk).emacsWithPackages emacsPackages;
  extraPrograms = with pkgs; [
    guile
    (sbcl.withPackages (
      ps: with ps; [
        com_dot_inuoe_dot_jzon
        serapeum
        closer-mop
        str
        trivial-types
        unix-opts
      ]
    ))
    python3
    ispell
  ];

  package = pkgs.symlinkJoin {
    name = "emacs-wrapped";
    paths = [ emacsPackage ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/emacs --suffix PATH : "${lib.makeBinPath extraPrograms}"
    '';
  };
in
{
  options.modules.emacs = {
    enable = mkEnableOption "emacs";
  };

  config = mkIf cfg.enable {

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
