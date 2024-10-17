{
  config,
  lib,
  pkgs,
  specialArgs,
  ...
}:
let
  inherit (specialArgs) username;
in
{
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
      defaultApplications =
        {
          "inode/directory" = "thunar.desktop";
          "application/pdf" = "zathura.desktop";
        }
        // lib.genAttrs ((
          lib.lists.concatMap
            (suffix: [
              "audio/${suffix}"
              "video/${suffix}"
            ])
            [
              "mp4"
              "mpeg"
              "ogg"
              "mp4"
              "mpeg"
              "ogg"
            ]
        )
        ++
          [
            "audio/wav"
            "video/webm"
          ])
            (_: "mpv.desktop")
        // lib.genAttrs (builtins.map (suffix: "image/${suffix}") [
          "bmp"
          "gif"
          "jpeg"
          "png"
          "webp"
        ]) (_: "imv.desktop")
        // lib.genAttrs (builtins.map (suffix: "application/${suffix}") [
          "bzip"
          "bzip2"
          "gzip"
          "x-7z-compressed"
          "x-gzip"
          "x-rar-compressed"
          "x-tar"
          "x-zip-compressed"
          "zip"
        ]) (_: "ark.desktop")
        // lib.genAttrs [
          "application/json"
          "application/x-latex"
          "application/x-sh"
          "application/yaml"
          "text/css"
          "text/csv"
          "text/javascript"
          "text/markdown"
          "text/plain"
          "text/tsv"
          "text/x-asm"
          "text/x-c"
          "text/x-fortran"
          "text/x-java-source"
          "text/x-pascal"
          "text/x-python"
          "x-scheme-handler/mailto"
        ] (_: "emacs.desktop")
        // lib.genAttrs [
          "application/xml"
          "text/html"
          "text/xml"
          "x-scheme-handler/about"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/unknown"
        ] (_: "firefox.desktop");
    };
  };

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };
}
