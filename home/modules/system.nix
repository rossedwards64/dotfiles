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
  };
}
