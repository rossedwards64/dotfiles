{ config, pkgs, ... }:

{
  imports = [
    ./alacritty
    ./desktop
    ./emacs
    ./email
    ./firefox
    ./games
    ./music
    ./pass
    ./programming
    ./starship
    ./system
    ./theme
    ./tmux
    ./topgrade
    ./window-manager
    ./zsh
  ];

  home.packages = [
    (import ./scripts/gpufan.nix { inherit pkgs; })
    (import ./scripts/run-game.nix { inherit pkgs; })
  ];
}
