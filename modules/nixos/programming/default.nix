{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.programming;
in {
  options.modules.programming = { enable = mkEnableOption "programming"; };

  config = mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        autoconf
        cargo
        clang
        cmake
        gcc
        git
        gnumake
        libtool
        rustc
        rustup
      ];

      etc = {
        sbclrc = {
          text = ''
            #+asdf (require :asdf)
            (ql:quickload :cffi :silent t)

            (pushnew (merge-pathnames ".nix-profile/lib/" (user-homedir-pathname))
                     cffi:*foreign-library-directories*)
            (pushnew (merge-pathnames "/run/current-system/sw/lib/" (user-homedir-pathname))
                     cffi:*foreign-library-directories*)

            (let ((default-init-file (funcall sb-ext:*userinit-pathname-function*)))
              (unless (or (null default-init-file)
             	           (typep default-init-file 'stream)
             	           (uiop:file-exists-p default-init-file))
                (setf sb-ext:*userinit-pathname-function*
                          (lambda () (uiop:xdg-config-home #P"sbcl/init.lisp")))))
          '';
          mode = "644";
        };
      };
    };
  };
}
