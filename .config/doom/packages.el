;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;(package! straight :pin "3eca39d")
<<<<<<< HEAD
;;(package! elcord)
(package! tree-sitter)
(package! dired-single)
(package! sly)
=======
;;(unpin! rustic)
(package! dired-single)
>>>>>>> 377dc60 (topgrade config)
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
