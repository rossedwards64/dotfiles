;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;(package! straight :pin "3eca39d")
;;(unpin! rustic)
(package! elcord)
(package! dired-single)
(package! wakatime-mode)
(package! dashboard)
(package! page-break-lines)
(package! autothemer)
;; (package! treesit-auto
;;   :recipe
;;   (:host github
;;    :repo "renzmann/treesit-auto"))
(package! command-log-mode
  :recipe
  (:host github
   :repo "lewang/command-log-mode"))
(package! org-alert)
