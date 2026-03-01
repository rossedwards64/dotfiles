{ lib, ... }:
{
  flake.modules = {
    nixos.base =
      { pkgs, ... }:
      {
        xdg = {
          icons.enable = true;
          menus.enable = true;
          sounds.enable = true;
          autostart.enable = true;

          portal = {
            enable = true;
            xdgOpenUsePortal = true;
            wlr.enable = true;
            config.common.default = "gtk";
            extraPortals = with pkgs; [
              xdg-desktop-portal-gnome
              xdg-desktop-portal-gtk
              xdg-desktop-portal-hyprland
            ];
          };
        };
      };

    homeManager.base =
      { pkgs, ... }:
      {
        xdg = {
          enable = true;
          mime.enable = true;
          userDirs.enable = true;

          portal = {
            enable = true;
            xdgOpenUsePortal = true;
            config.common.default = "gtk";
            extraPortals = with pkgs; [
              xdg-desktop-portal-gnome
              xdg-desktop-portal-gtk
              xdg-desktop-portal-hyprland
            ];
          };

          mimeApps = {
            enable = true;
            defaultApplications = {
              "inode/directory" = "thunar.desktop";
              "application/pdf" = "zathura.desktop";
            }
            // lib.genAttrs (
              (lib.lists.concatMap
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
              ++ [
                "audio/wav"
                "video/webm"
              ]
            ) (_: "mpv.desktop")
            // lib.genAttrs (map (suffix: "image/${suffix}") [
              "bmp"
              "gif"
              "jpeg"
              "png"
              "webp"
            ]) (_: "imv.desktop")
            // lib.genAttrs (map (suffix: "application/${suffix}") [
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
      };
  };
}
