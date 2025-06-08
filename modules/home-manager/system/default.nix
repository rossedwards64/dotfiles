{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.system;
  xdg = config.xdg;
in
{
  options.modules.system = {
    enable = mkEnableOption "system";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      btop
      du-dust
      ffmpeg
      fzf
      gcr
      nix-health
      nix-info
      nixd
      nixfmt-rfc-style
      p7zip
      rar
      ripgrep
      rlwrap
      rsync
      tokei
      tree-sitter
      unzip
    ];

    xdg.enable = true;
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

          updates = {
            auto_update = true;
          };
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
        components = [
          "pkcs11"
          "secrets"
          "ssh"
        ];
      };

      gpg-agent = {
        enable = true;
        enableZshIntegration = true;
        pinentry.package = pkgs.pinentry-qt;
      };
    };
  };
}
