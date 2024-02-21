;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")
(setq user-full-name "Dong Wang"
      user-mail-address "moneatts@outlook.com")

(setq-default
 window-combination-resize t
 x-stretch-cursor t
 yas-triggers-in-field t
 )

(setq
 undo-limit 80000000
 auto-save-default t
 scroll-preserve-screen-position 'always
 scroll-margin 2
 word-wrap-by-category t
 )

(global-subword-mode t)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(cond
 ((featurep :system 'windows)
  (setq +main-font "SauceCodePro NF")
  (setq +unicode-font "SauceCodePro NFM")
  (message "window os")
  )
 ((featurep :system 'linux)
  (setq +main-font "SauceCodePro NF")
  (setq +unicode-font "SauceCodePro NFM")
  (message "linux os")
  )
 ((featurep :system 'macos)
  (setq +main-font "SauceCodePro NF")
  (setq +unicode-font "SauceCodePro NFM")
  (message "mac os")
  )
 (t
  (message "未知的操作系统"))
 )
(setq doom-font (font-spec :family +main-font :size 18 :weight 'regular)
      doom-big-font (font-spec :family +main-font :size 20 :weight 'light)
      doom-variable-pitch-font (font-spec :family +main-font) ; inherits :size from doom-font
      doom-serif-font (font-spec :family +main-font :weight 'light)
      doom-symbol-font (font-spec :family +unicode-font)
 )

;; Emacs 启动的时候，使窗口最大化
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-dracula)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")
(setq org-directory (getenv "ORG_HOME"))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; treemacs
(setq +treemacs-git-mode 'extended)
(after! treemacs
  :config
  (setq treemacs-collapse-dirs 200)
  )

;; plantuml
;;(setq plantuml-jar-path (concat (expand-file-name user-emacs-directory) "plantuml/plantuml.jar")
;;      plantuml-default-exec-mode 'jar)

;; modeline
(after! doom-modeline
  :custom
  ;; add padding to the right
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info battery checker repl buffer-encoding indent-info major-mode process vcs mu4e time "    "))
  (doom-modeline-def-modeline 'dashboard
    '(bar window-number buffer-default-directory-simple remote-host)
    '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process time "    "))
  (doom-modeline-def-modeline 'vcs
    '(bar window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process time "    "))
  (doom-modeline-def-modeline 'message
    '(bar window-number modals matches buffer-info-simple buffer-position word-count parrot selection-info)
    '(objed-state misc-info battery debug minor-modes input-method indent-info buffer-encoding major-mode time "    "))
  (doom-modeline-def-modeline 'minimal
    '(bar matches buffer-info-simple)
    '(media-info major-mode time "    "))
  (doom-modeline-def-modeline 'special
    '(bar window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process time "    "))
  (doom-modeline-def-modeline 'project
    '(bar window-number modals buffer-default-directory remote-host buffer-position)
    '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process time "    "))
  ;; time on modeline
  (setq display-time-string-forms
        '((propertize (concat 24-hours ":" minutes))))
  (display-time-mode t)
  ;; battery info on modeline (only on laptops)
  (let ((battery-str (battery)))
    (if (string-match-p (regexp-quote "Power Battery") battery-str)
        (display-battery-mode t)))
  ;; better default
  (setq doom-modeline-bar-width 4
        doom-modeline-hud nil
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-time-icon nil
        doom-modeline-display-misc-in-all-mode-lines nil
        doom-modeline-buffer-file-name-style 'auto)
  )

;; ligature
(defun +appened-to-negation-list (head tail)
  (if (sequencep head)
      (delete-dups
       (if (eq (car tail) 'not)
           (append head tail)
         (append tail head)))
    tail))

(when (modulep! :ui ligatures)
  (setq +ligatures-extras-in-modes
        (+appened-to-negation-list
         +ligatures-extras-in-modes
         '(not c-mode c++-mode emacs-lisp-mode python-mode scheme-mode racket-mode rust-mode)))
  (setq +ligatures-in-modes
        (+appened-to-negation-list
         +ligatures-in-modes
         '(not emacs-lisp-mode scheme-mode racket-mode))))

;; org
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(after! org
  (setq org-agenda-files
        (list (expand-file-name "Inbox.org" org-directory)
              (expand-file-name "Private.org" org-directory)
              (expand-file-name "Todo.org" org-directory)
              (expand-file-name "Work.org" org-directory)
              (expand-file-name "Projects.org" org-directory)
              (expand-file-name "Notes.org" org-directory)
              )))
;; org-babel
(after! org-src
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
