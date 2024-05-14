{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.system;
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
      nix-health
      nix-info
      p7zip
      rar
      ripgrep
      rlwrap
      rsync
      tealdeer
      tokei
      topgrade
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

      direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv.enable = true;
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

    services.gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-qt;
    };
  };
}
