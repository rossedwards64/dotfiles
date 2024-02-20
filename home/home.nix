{ pkgs, ... }:

{
  imports = [ ../modules/home-manager ];

  xdg.enable = true;

  home = {
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
    stateVersion = "23.11";
  };
}
