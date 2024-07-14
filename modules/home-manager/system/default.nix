{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.system;
  xdg = config.xdg;
in {
  options.modules.system = { enable = mkEnableOption "system"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      btop
      du-dust
      ffmpeg
      fzf
      gcr
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

      bat = {
        enable = true;
        extraPackages = with pkgs.bat-extras; [
          batdiff
          batman
          batgrep
          batwatch
        ];
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

      fd = {
        enable = true;
        hidden = true;
      };
    };

    services = {
      gnome-keyring = {
        enable = true;
        components = [ "pkcs11" "secrets" "ssh" ];
      };

      gpg-agent = {
        enable = true;
        enableZshIntegration = true;
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
