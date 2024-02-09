{ config, pkgs, ... }:

{
  imports = [ ./modules ];

  programs.home-manager.enable = true;
  xdg.enable = true;

  home = {
    username = "ross";
    homeDirectory = "/home/ross";
    stateVersion = "23.11";

    pointerCursor = with pkgs; {
      gtk.enable = true;
      package = catppuccin-cursors.mochaDark;
      name = "Catppuccin-Mocha-Dark-Cursors";
    };

    sessionPath = [
      "$HOME/.local/bin"
      "$XDG_CONFIG_HOME/emacs/bin"
      "$HOME/.dotfiles/.bin"
      "$XDG_DATA_HOME/cargo/bin"
    ];
  };
}
