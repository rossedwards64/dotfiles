{ config, ... }:

{
  imports = [
    ./alacritty
    ./desktop
    ./emacs
    ./email
    ./fonts
    ./games
    ./git
    ./gpg
    ./music
    ./pass
    ./programming
    ./starship
    ./system
    ./theme
    ./tmux
    ./topgrade
    ./zsh
  ];

  config.modules = {
    alacritty.enable = true;
    desktop.enable = true;
    emacs.enable = true;
    email.enable = true;
    fonts.enable = true;
    games.enable = true;
    git.enable = true;
    gpg.enable = true;
    music.enable = true;
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
