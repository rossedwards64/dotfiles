{ ... }:

{
  imports = [
    ./alacritty.nix
    ./desktop.nix
    ./emacs/emacs.nix
    ./email.nix
    ./fonts.nix
    ./games.nix
    ./git.nix
    ./pass.nix
    ./programming.nix
    ./starship.nix
    ./system.nix
    ./theme.nix
    ./tmux.nix
    ./topgrade.nix
    ./zsh.nix
  ];

  config.modules = {
    alacritty.enable = true;
    desktop.enable = true;
    emacs.enable = true;
    email.enable = true;
    fonts.enable = true;
    games.enable = true;
    git.enable = true;
    pass.enable = true;
    programming.enable = true;
    starship.enable = true;
    system.enable = true;
    theme.enable = true;
    tmux.enable = true;
    topgrade.enable = true;
    zsh.enable = true;
  };
}
