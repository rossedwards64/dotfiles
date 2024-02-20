{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.programming;
in {
  options.modules.programming = { enable = mkEnableOption "programming"; };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
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
  };
}
