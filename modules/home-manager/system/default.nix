{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.system;
  xdg = config.xdg;
in {
  options.modules.system = { enable = mkEnableOption "system"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      bat
      broot
      btop
      du-dust
      eza
      fd
      ffmpeg
      fzf
      nil
      nix-health
      nix-info
      nixfmt
      p7zip
      rar
      ripgrep
      rlwrap
      rsync
      tokei
      topgrade
      tree-sitter
      unzip
    ];

    programs = {
      git = {
        enable = true;
        userEmail = "redwards64@hotmail.com";
        userName = "Ross Edwards";
      };

      gpg = {
        enable = true;
        homedir = "${xdg.dataHome}/gnupg";
      };

      tealdeer = {
        enable = true;

        settings = {
          display = {
            compact = true;
            nuse_pager = true;
          };

          updates = { auto_update = true; };
        };
      };
    };

    services = {
      gpg-agent = {
        enable = true;
        pinentryPackage = pkgs.pinentry-qt;
      };
    };

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
