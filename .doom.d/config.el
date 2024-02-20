;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Aaron Bruinsma"
      user-mail-address "aaron.t.bruinsma@gmail.com")

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
;; (setq org-roam-directory "~/org/roam")
;; (setq org-roam-dailies-directory "journals/")
;; (setq org-roam-completion-anywhere t)
;; (setq org-roam-capture-templates
;;       '(("d" "default" plain
;;          "%?" :target
;;          (file+head "${slug}.org" "#+title: ${title}\n")
;;          :unnarrowed t)
;;         ("p" "Capture to project" plain
;;          "%?" :target
;;          (file+head "projects.org" "+title: ${title}\n"))))

;; (defun my/org-roam-capture-inbox ()
;;   (interactive)
;;   (org-roam-capture- :node (org-roam-node-create)
;;                      :templates '(("i" "inbox" plain "* %?"
;;                                    :if-new (file+head "inbox.org" "#+title: Inbox\n"))))
;; )

;; (defun my/org-roam-capture-project ()
;;   (interactive)
;;   (org-roam-capture- :node (org-roam-node-create)
;;                      :templates '(("i" "inbox" plain "* %?"
;;                                    :if-new (file+head "inbox.org" "#+title: Inbox\n")))))

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

(defun run-in-vterm (command name)
  (let ((buf (cond ((get-buffer name))
                   (t (vterm name)))))
     (with-current-buffer buf
       (vterm-send-string command)
       (vterm-send-return)))
)

(defun run-in-vterm-other-window (command name)
  (let ((buf (cond ((get-buffer name))
                   (t (vterm-other-window name)))))
     (with-current-buffer buf
       (vterm-send-string command)
       (vterm-send-return)))
)

;; prepare the arguments
(setq dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
(setq dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))

;; function to start magit on dotfiles
(defun dotfiles-magit-status ()
  (interactive)
  (add-to-list 'magit-git-global-arguments dotfiles-git-dir)
  (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
  (call-interactively 'magit-status))

(map! :leader
      ( :prefix "g"
        :desc "dotfiles-magit-status" "P" #'dotfiles-magit-status))

;; wrapper to remove additional args before starting magit
(defun magit-status-with-removed-dotfiles-args ()
  (interactive)
  (setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
  (setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
  (call-interactively 'magit-status))
;; redirect global magit hotkey to our wrapper
(global-set-key [remap magit-status] 'magit-status-with-removed-dotfiles-args)

;; Let projectile find all files within a project without needing to have visited
;; them prior
(setq projectile-indexing-method 'hybrid)
(setq projectile-enable-caching t)
(after! projectile
    (setq projectile-project-root-files-bottom-up (remove ".project" projectile-project-root-files-bottom-up))
)

;(setq max-specpdl-size 2600)

(map! :leader
      ( :prefix "l"
        :desc "lsp-find-definition" "l" #'lsp-find-definition
        :desc "lsp-find-references" "r" #'lsp-find-references))

;; Some keybinds brought over from my time with vim+cscope+fzf
(map! :desc "Jump to definition" :m "C-]" #'+lookup/definition)
(map! :desc "Find file" :n "C-p" #'helm-find)

(setq helm-findutils-search-full-path t)

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

(if (file-exists-p "~/.doom.d/build_cmds.el")
    (load "~/.doom.d/build_cmds.el")
)

(setq docker-container-shell-file-name "/bin/bash")
(setq vterm-tramp-shells '(("docker" "/bin/bash")))

(use-package! ace-window
  :config
  (map! :leader
        "k" nil
        :desc "ace-window" "k" #'ace-window)
  (setq aw-scope 'global
        aw-ignore-on nil
        ))

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

;; Load vterm immediately as docker features work differently if vterm is loaded after
;; vterm is also not correctly loaded when docker is loaded
(use-package! vterm
  :demand t)

(if (file-exists-p "~/.doom.d/config_local.el")
    (load "~/.doom.d/config_local.el")
)
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
