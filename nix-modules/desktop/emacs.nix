{
  inputs,
  lib,
  config,
  ...
}:
{
  flake.pkgs =
    let
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
      inherit (inputs.emacs-overlay.packages."x86_64-linux") emacs-git-pgtk;
      emacsPackages = epkgs: with pkgs; [
          auctex
          emacs-all-the-icons-fonts
          epkgs.vterm
          mu
          mu.mu4e
          (epkgs.treesit-grammars.with-grammars (
            grammars: with grammars; [
              tree-sitter-bash
              tree-sitter-c
              tree-sitter-clojure
              tree-sitter-commonlisp
              tree-sitter-elisp
              tree-sitter-haskell
              tree-sitter-nix
              tree-sitter-rust
              tree-sitter-scheme
              tree-sitter-scala
            ]
          ))
        ];

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
    in
    {
      emacs = pkgs.symlinkJoin {
        name = "emacs-wrapped";
        paths = [ emacsPackage ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/emacs --suffix PATH : "${lib.makeBinPath extraPrograms}"
        '';
      };
    };

  flake.modules.homeManager.base =
    let
      package = config.flake.pkgs.emacs;
    in
    {
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

      stylix.targets.emacs.enable = false;
      xdg.configFile = {
        "emacs/early-init.el".enable = false;
        "emacs/init.el".enable = false;
      };
    };
}
