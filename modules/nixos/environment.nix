{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.environment;
in {
  options.modules.environment = { enable = mkEnableOption "environment"; };

  config = mkIf cfg.enable {
    programs.zsh.enable = true;

    environment = {
      systemPackages = with pkgs; [ zsh ];

      sessionVariables = rec {
        XDG_CACHE_HOME = "$HOME/.cache";
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_STATE_HOME = "$HOME/.local/state";
        XDG_BIN_HOME = "$HOME/.local/bin";
        PATH = [ "${XDG_BIN_HOME}" ];
      };

      etc = {
        "zcfan.conf" = {
          text = ''
            max_temp 70
            med_temp 60
            low_temp 40
          '';
          mode = "644";
        };

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
