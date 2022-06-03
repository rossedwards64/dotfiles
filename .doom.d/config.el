;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; login
(setq user-full-name "Ross Edwards"
      user-mail-address "redwards64@hotmail.com")

;; window setup
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'initial-frame-alist '(fullscreen . fullboth))
(setq evil-split-window-below t
      evil-vsplit-window-right t
      display-line-numbers-type 'relative
      display-time-day-and-date t
      visible-bell t)
(eval-when-compile
  (require 'cl))

;; theme
(if (equal (system-name) "ross-desktop")
    (setq doom-theme 'modus-vivendi)
    (if (equal (system-name) "ross-thinkpad")
        (setq doom-theme 'manoj-dark)
      (if (equal (system-name) "ross-laptop")
          (setq doom-theme 'modus-operandi))))

(setq doom-themes-treemacs-theme "doom-colors")

(with-eval-after-load 'doom-themes
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; fonts
(setq doom-font (font-spec :family "Iosevka" :size 14 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 14 :weight 'regular)
      doom-big-font (font-spec :family "Iosevka" :size 24 :weight 'regular))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; dashboard
(setq fancy-splash-image (concat doom-private-dir "logos/nerv.png"))

(custom-set-faces!
  '(doom-dashboard-banner :foreground "#8a0000" :weight bold)
  '(doom-dashboard-footer :inherit font-lock-constant-face)
  '(doom-dashboard-footer-icon :inherit all-the-icons-red)
  '(doom-dashboard-loaded :inherit font-lock-warning-face)
  '(doom-dashboard-menu-desc :inherit font-lock-string-face)
  '(doom-dashboard-menu-title :inherit font-lock-function-name-face))

(add-hook! '+doom-dashboard-functions :append
           (insert "\n" (+doom-dashboard--center +doom-dashboard--width "They should make Worm Emacs.")))

;; enable modes
(display-time-mode 1)
(solaire-global-mode 1)
(global-auto-revert-mode 1)
(which-function-mode 1)
(cursor-sensor-mode 1)
(elcord-mode 1)
(after! elcord
  elcord-use-major-mode-as-main-icon t)

;; makefile
(cl-defun get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
   This may not do the correct thing in the presence of links.
   If it does not find FILE, then it shall return the name of FILE in the current directory, suitable for creation."
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (cl-loop
                        for d = default-directory then (expand-file-name ".." d)
                        if (file-exists-p (expand-file-name file d))
                        return d
                        if (equal d root)
                        return nil))))

(add-hook! '(c-mode-hook c++-mode-hook)
           #'(lambda ()
               (set (make-local-variable 'compile-command) (format "make -C %s -k" (substring (get-closest-pathname) 0 -8)))))

;; lsp
(setq lsp-ui-doc-enable t
      lsp-ui-doc-show-with-cursor t
      lsp-ui-doc-show-with-mouse t
      lsp-headerline-breadcrumb-enable t
      lsp-ui-sideline-enable t
      lsp-ui-sideline-show-code-actions t
      lsp-ui-sideline-show-hover t
      lsp-ui-peek-enable t
      lsp-ui-imenu-enable t
      lsp-ui-imenu-auto-refresh t)

(map! :leader
      :desc "Switch from header file to source file, or vice versa."
      "z" #'lsp-clangd-find-other-file)

;; org
(setq org-directory "~/Documents/org/"
      org-agenda-files "~/Documents/org/agenda.org"
      org-support-shift-select t
      org-log-done 'time
      org-pretty-entities-include-sub-superscripts t
      org-startup-with-inline-images t)

;; modeline
(custom-set-faces!
  '(model-line :family "Iosevka" :height 0.9)
  '(mode-line-inactive :family "Iosevka" :height 0.9))

(setq doom-modeline-enable-word-count t
      doom-modeline-persp-name t
      doom-modeline-persp-icon t
      doom-modeline-hud t
      doom-modeline-height 40
      doom-modeline-bar-width 10
      doom-modeline-major-mode-icon t
      doom-modeline-buffer-encoding t
      doom-modeline-indent-info t
      doom-modeline-github t
      doom-modeline-display-default-persp-name t
      all-the-icons-scale-factor 1.1)

(add-hook! 'doom-modeline-mode-hook
           (let ((char-table char-width-table))
             (while (setq char-table (char-table-parent char-table)))
             (dolist (pair doom-modeline-rhs-icons-alist)
               (let ((width 3)
                     (chars (cdr pair))
                     (table (make-char-table nil)))
                 (dolist (char chars)
                   (set-char-table-range table char width))
                 (optimize-char-table table)
                 (set-char-table-parent table char-table)
                 (setq char-width-table table)))))

;; headerline
(defun align-header-line (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 12)))
    (format (format " %%s %%%ds " available-width) (convert-list-to-string left) (convert-list-to-string right))))

(defun convert-list-to-string (list)
  (let* ((string-with-parenthesis (format "%S" list))
         (end (- (length string-with-parenthesis) 2)))
    (substring string-with-parenthesis 2 end)))

(setq header-line-format
     '(:eval (align-header-line
              ;; left
              (list "")
              ;; right
              (list "Writing %m code in %b%*. | Worming out in %F."))))

(add-to-list 'exec-path "~/bin")
(setq-default window-combination-resize t
              x-stretch-cursor t)

;; ligatures
(set-ligatures! 'MAJOR-MODE
    ;; Functional
    :lambda        "lambda keyword"
    :def           "function keyword"
    :composition   "composition"
    :map           "map/dictionary keyword"
    ;; Types
    :null          "null type"
    :true          "true keyword"
    :false         "false keyword"
    :int           "int keyword"
    :float         "float keyword"
    :str           "string keyword"
    :bool          "boolean keyword"
    :list          "list keyword"
    ;; Flow
    :not           "not operator"
    :in            "in operator"
    :not-in        "not in operator"
    :and           "and keyword"
    :or            "or keyword"
    :for           "for keyword"
    :some          "some keyword"
    :return        "return"
    :yield         "yield"
    ;; Other
    :union         "Union keyword"
    :intersect     "Intersect keyword"
    :diff          "diff keyword"
    :tuple         "Tuple Keyword "
    :pipe          "Pipe Keyword" ;; FIXME: find a non-private char
    :dot           "Dot operator")

(plist-put! +ligatures-extra-symbols
  ;; org
  :name          "»"
  :src_block     "»"
  :src_block_end "«"
  :quote         "“"
  :quote_end     "”"
  ;; Functional
  :lambda        "λ"
  :composition   "∘"
  :map           "↦"
  ;; Other
  :union         "⋃"
  :intersect     "∩"
  :diff          "∖"
  :tuple         "⨂"
  :pipe          "" ;; FIXME: find a non-private char
  :dot           "•")  ;; you could also add your own if you want

;;; :app everywhere
(after! emacs-everywhere
  (setq emacs-everywhere-frame-name-format "emacs-anywhere")
  (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)
  (defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
   :override #'emacs-everywhere-set-frame-position
   (cl-destructuring-bind (x y width height)
     (emacs-everywhere-window-geometry window-info)
     (set-frame-position frame
                   (+ x (/ width 2) (- (/ width 2)))
                   (+ y (/ height 2))))))
