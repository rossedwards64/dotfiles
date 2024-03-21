{ config, lib, pkgs, specialArgs, ... }:
let inherit (specialArgs) username;
in {
  imports = [ ../modules/home-manager ];

  programs.home-manager.enable = true;

  xdg = {
    enable = true;
    mime.enable = true;
    userDirs.enable = true;
  };

  home = {
    inherit username;
    homeDirectory = "/home/${username}";

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
