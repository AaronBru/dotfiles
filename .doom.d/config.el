;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq org-babel-python-command "python3")

(setq org-babel-C++-compiler "g++-11 -std=c++20")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function "insert-tab")
(setq-default electric-indent-inhibit t)
(setq c-tab-always-indent nil)

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq plantuml-jar-path "/opt/java/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")

;; Let projectile find all files within a project without needing to have visited
;; them prior
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching nil)
(setq projectile-require-project-root nil)

(setq projectile-project-root-functions '(projectile-root-local
                                          projectile-root-top-down
                                          projectile-root-top-down-recurring
                                          projectile-root-bottom-up))

(map! :leader
      ( :prefix "l"
        :desc "lsp-find-definition" "l" #'lsp-find-definition
        :desc "lsp-find-references" "r" #'lsp-find-references))

;; Some keybinds brought over from my time with vim+cscope+fzf
(map! :desc "Jump to definition" :m "C-]" #'+lookup/definition)
(map! :desc "Find file" :n "C-p" #'helm-find)

;; Fix latex at some point...
(setq org-latex-pdf-process '("latexmk -f -pdf -interaction=nonstopmode -output-directory=%o %f"))

(defun my/helm/compile ()
  (interactive)
  (compile (helm-comp-read "Compile Commands:" compile-commands)))

(defun my/helm/compile/project ()
  (interactive)
  (let ((default-directory (projectile-acquire-root)))
        (my/helm/compile)))

(map! :leader
      ( :prefix "c"
        :desc "Find compile command" "F" #'my/helm/compile/project))

(load "~/.doom.d/build_cmds.el")

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(defconst my-cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . 0)
                        (access-label . [0])
                        (substatement-open . 0)))))

(c-add-style "my-cc-style" my-cc-style)
(add-hook 'c++-mode-hook (lambda () (c-set-style "my-cc-style") ))
;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
