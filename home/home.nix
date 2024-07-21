{ config, lib, pkgs, specialArgs, ... }:
let inherit (specialArgs) username;
in {
  programs.home-manager.enable = true;

  home = {
    inherit username;
    homeDirectory = "/home/${username}";

    sessionPath = [
      "$HOME/.local/bin"
      "$XDG_CONFIG_HOME/emacs/bin"
      "$HOME/.dotfiles/.bin"
      "$XDG_DATA_HOME/cargo/bin"
    ];

    stateVersion = "23.11";
  };

  xdg = {
    enable = true;
    mime.enable = true;
    userDirs.enable = true;
    systemDirs.data = [ "/home/${username}/.local/share/applications" ];

    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [ "zathura.desktop" ];
        "video/mp4" = [ "mpv.desktop" ];
        "image/png" = [ "imv.desktop" ];
      };
    };
  };

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };
}
