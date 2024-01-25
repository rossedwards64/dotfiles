;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! arduino-mode)
(package! autothemer)
(package! catppuccin-theme)
(package! dashboard)
(package! dired-single)
(package! elcord)
(package! org-alert)
(package! page-break-lines)
(package! timu-caribbean-theme)
(package! timu-macos-theme)
(package! kanagawa-theme)
(package! wakatime-mode)

(package! command-log-mode
  :recipe
  (:host github
   :repo "lewang/command-log-mode"))
(package! ef-themes
  :recipe
  (:host github
   :repo "protesilaos/ef-themes"))
(package! lem
  :recipe
  (:host codeberg
   :repo "martianh/lem.el"))
(package! standard-themes
  :recipe
  (:host github
   :repo "protesilaos/standard-themes"))
(package! structurizr-mode
  :recipe
  (:host github
   :repo "gilesp/structurizr-mode"))
(package! tronesque
  :recipe
  (:host github
   :repo "aurelienbottazini/tronesque"))
