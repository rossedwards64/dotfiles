{ config, pkgs, lib, ... }:
with lib;
let cfg = config.modules.programming;
in {
  options.modules.programming = { enable = mkEnableOption "programming"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      clang-tools_17
      gdb
      jdk17
      nixd
      nixfmt
      nodejs_21
      shellcheck
      SDL2
      (with lispPackages; quicklisp)
      asdf
      clj-kondo
      cljfmt
      clojure
      clojure-lsp
      guile
      leiningen
      roswell
      sbcl
      texlab
    ];

    xdg = {
      enable = true;
      configFile = {
        "sbcl/init.lisp".text = ''
          #-quicklisp
          (let ((quicklisp-init (merge-pathnames
                                  "quicklisp/setup.lisp"
                                  (uiop:xdg-data-pathname))))
            (when (probe-file quicklisp-init)
              (load quicklisp-init)))
        '';
      };
    };
  };
}
