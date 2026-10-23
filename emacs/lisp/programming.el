;; -*- lexical-binding: t; -*-
(use-package exec-path-from-shell
  :straight t
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(use-package markdown-mode
  :defer t
  :straight t
  :mode
  (("\\.mkd\\'" . markdown-mode)
   ("\\.mdk\\'" . markdown-mode)
   ("\\.mdx\\'" . markdown-mode))
  :hook (markdown-mode . orgtbl-mode)
  :hook (markdown-mode . visual-line-mode)
  :general
  (phundrak/evil
    :keymaps 'markdown-mode-map
    :packages '(markdown-mode evil)
    "M-RET" #'markdown-insert-list-item
    "M-c"   #'markdown-promote
    "M-t"   #'markdown-move-down
    "M-s"   #'markdown-move-up
    "M-r"   #'markdown-demote
    "t"     #'evil-next-visual-line
    "s"     #'evil-previous-visual-line)
  (phundrak/major-leader-key
    :keymaps 'markdown-mode-map
    :packages 'markdown-mode
    "{"   #'markdown-backward-paragraph
    "}"   #'markdown-forward-paragraph
    "]"   #'markdown-complete
    ">"   #'markdown-indent-region
    "»"   #'markdown-indent-region
    "<"   #'markdown-outdent-region
    "«"   #'markdown-outdent-region
    "n"   #'markdown-next-link
    "p"   #'markdown-previous-link
    "f"   #'markdown-follow-thing-at-point
    "k"   #'markdown-kill-thing-at-point
    "c"   '(:ignore t :which-key "command")
    "c]"  #'markdown-complete-buffer
    "cc"  #'markdown-check-refs
    "ce"  #'markdown-export
    "cm"  #'markdown-other-window
    "cn"  #'markdown-cleanup-list-numbers
    "co"  #'markdown-open
    "cp"  #'markdown-preview
    "cv"  #'markdown-export-and-preview
    "cw"  #'markdown-kill-ring-save
    "h"   '(:ignore t :which-key "headings")
    "hi"  #'markdown-insert-header-dwim
    "hI"  #'markdown-insert-header-setext-dwim
    "h1"  #'markdown-insert-header-atx-1
    "h2"  #'markdown-insert-header-atx-2
    "h3"  #'markdown-insert-header-atx-3
    "h4"  #'markdown-insert-header-atx-4
    "h5"  #'markdown-insert-header-atx-5
    "h6"  #'markdown-insert-header-atx-6
    "h!"  #'markdown-insert-header-setext-1
    "h@"  #'markdown-insert-header-setext-2
    "i"   '(:ignore t :which-key "insert")
    "i-"  #'markdown-insert-hr
    "if"  #'markdown-insert-footnote
    "ii"  #'markdown-insert-image
    "il"  #'markdown-insert-link
    "it"  #'markdown-insert-table
    "iw"  #'markdown-insert-wiki-link
    "l"   '(:ignore t :which-key "lists")
    "li"  #'markdown-insert-list-item
    "T"   '(:ignore t :which-key "toggle")
    "Ti"  #'markdown-toggle-inline-images
    "Tu"  #'markdown-toggle-url-hiding
    "Tm"  #'markdown-toggle-markup-hiding
    "Tt"  #'markdown-toggle-gfm-checkbox
    "Tw"  #'markdown-toggle-wiki-links
    "t"   '(:ignore t :which-key "table")
    "tc"  #'markdown-table-move-column-left
    "tt"  #'markdown-table-move-row-down
    "ts"  #'markdown-table-move-row-up
    "tr"  #'markdown-table-move-column-right
    "ts"  #'markdown-table-sort-lines
    "tC"  #'markdown-table-convert-region
    "tt"  #'markdown-table-transpose
    "td"  '(:ignore t :which-key "delete")
    "tdc" #'markdown-table-delete-column
    "tdr" #'markdown-table-delete-row
    "ti"  '(:ignore t :which-key "insert")
    "tic" #'markdown-table-insert-column
    "tir" #'markdown-table-insert-row
    "x"   '(:ignore t :which-key "text")
    "xb"  #'markdown-insert-bold
    "xB"  #'markdown-insert-gfm-checkbox
    "xc"  #'markdown-insert-code
    "xC"  #'markdown-insert-gfm-code-block
    "xi"  #'markdown-insert-italic
    "xk"  #'markdown-insert-kbd
    "xp"  #'markdown-insert-pre
    "xP"  #'markdown-pre-region
    "xs"  #'markdown-insert-strike-through
    "xq"  #'markdown-blockquote-region)
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "/opt/homebrew/bin/pandoc")
)

(use-package gh-md
  :defer t
  :after markdown-mode
  :straight (:build t)
  :general
  (phundrak/major-leader-key
    :packages 'gh-md
    :keymaps 'markdown-mode-map
    "cr" #'gh-md-render-buffer))

(use-package ox-gfm
  :straight (:build t)
  :defer t
  :after (org ox))

(use-package markdown-toc
  :defer t
  :after markdown-mode
  :straight (:build t)
  :general
  (phundrak/major-leader-key
    :packages 'markdown-toc
    :keymaps 'markdown-mode-map
    "iT" #'markdown-toc-generate-toc))

(use-package edit-indirect
  :straight (:build t)
  :defer t)

(setq explicit-shell-file-name "/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

(use-package treesit
  :defer t
  :straight (:type built-in)
  :hook ((bash-ts-mode c-ts-mode c++-ts-mode
          html-ts-mode js-ts-mode typescript-ts-mode
          json-ts-mode rust-ts-mode tsx-ts-mode python-ts-mode
          css-ts-mode yaml-ts-mode) . lsp-deferred))

(use-package treesit-fold
  :after tree-sitter
  :straight (:build t :host github :repo "emacs-tree-sitter/treesit-fold")
  :init (global-treesit-fold-mode t))

(use-package cc-mode
  :straight (:type built-in)
  :defer t
  :init
  (put 'c-c++-backend 'safe-local-variable 'symbolp)
  (add-hook 'c-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'c++-mode-hook #'tree-sitter-hl-mode)
  :config
  (require 'compile)
  :general
  (phundrak/undefine
    :keymaps '(c-mode-map c++-mode-map)
    ";" nil)
  (phundrak/major-leader-key
   :keymaps '(c-mode-map c++-mode-map)
   "l"  '(:keymap lsp-command-map :which-key "lsp" :package lsp-mode))
  (phundrak/evil
   :keymaps '(c-mode-map c++-mode-map)
   "ga" #'projectile-find-other-file
   "gA" #'projectile-find-other-file-other-window))

(use-package clang-format+
  :straight (:build t)
  :defer t
  :init
  (add-hook 'c-mode-common-hook #'clang-format+-mode))

(use-package modern-cpp-font-lock
  :straight (:build t)
  :defer t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package lisp-mode
  :straight (:type built-in)
  :defer t
  :after parinfer-rust-mode
  :hook (lisp-mode . parinfer-rust-mode)
  :config
  (put 'defcommand 'lisp-indent-function 'defun)
  (setq inferior-lisp-program "/usr/bin/sbcl --noinform"))

(use-package python
  :defer t
  :straight (:build t)
  :after ob
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("[./]flake8\\'" . conf-mode)
         ("/Pipfile\\'"   . conf-mode))
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  (add-hook 'python-mode-local-vars-hook #'lsp)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (when (and (executable-find "python3")
           (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  :general
  (phundrak/major-leader-key
   :keymaps 'python-mode-map
   :packages 'lsp-mode
   "l"  '(:keymap lsp-command-map :which-key "lsp")))

(use-package pytest
  :defer t
  :straight (:build t)
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytest-pdb-all
             pytest-last-failed
             pytest-pdb-last-failed
             pytest-module
             pytest-pdb-module)
  :config
  (add-to-list 'pytest-project-root-files "setup.cfg")
  :general
  (phundrak/major-leader-key
   :keymaps 'python-mode-map
   :infix "t"
   :packages 'pytest
   ""  '(:ignore t :which-key "test")
   "a" #'python-pytest
   "f" #'python-pytest-file-dwim
   "F" #'python-pytest-file
   "t" #'python-pytest-function-dwim
   "T" #'python-pytest-function
   "r" #'python-pytest-repeat
   "p" #'python-pytest-dispatch))

(use-package pip-requirements
  :defer t
  :straight (:build t))

(use-package pippel
  :defer t
  :straight (:build t)
  :general
  (phundrak/major-leader-key
   :keymaps 'python-mode-map
   :packages 'pippel
   "P" #'pippel-list-packages))

(use-package cython-mode
  :defer t
  :straight (:build t)
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s")
  :general
  (phundrak/major-leader-key
   :keymaps 'cython-mode-map
   :packages 'cython-mode
   :infix "c"
   ""  '(:ignore t :which-key "cython")
   "c" #'cython-compile))

(use-package flycheck-cython
  :defer t
  :straight (:build t)
  :after cython-mode)

(use-package blacken
  :defer t
  :straight (:build t)
  :init
  (add-hook 'python-mode-hook #'blacken-mode))

(use-package lsp-pyright
  :after lsp-mode
  :defer t
  :straight (:buidl t))

(use-package uv-mode
  :straight (:build t)
  :hook (python-mode . uv-mode-auto-activate-hook))

(use-package rust-mode
  :straight t
  :defer t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :defer t
  :straight (:build t)
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode-local-vars . rustic-setup-lsp)
  :hook (rustic-mode . lsp-deferred)
  :init
  (with-eval-after-load 'org
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  (setq rustic-lsp-client 'lsp-mode)
  :general
  (general-define-key
   :keymaps 'rustic-mode-map
   :packages 'lsp
   "M-t" #'lsp-ui-imenu
   "M-?" #'lsp-find-references)
  (phundrak/major-leader-key
   :keymaps 'rustic-mode-map
   :packages 'lsp-mode
   "l"  '(:keymap lsp-command-map :which-key "lsp"))
  (phundrak/major-leader-key
   :keymaps 'rustic-mode-map
   :packages 'rustic
   "b"  '(:ignore t :which-key "build")
   "bb" #'rustic-cargo-build
   "bB" #'rustic-cargo-bench
   "bc" #'rustic-cargo-check
   "bC" #'rustic-cargo-clippy
   "bd" #'rustic-cargo-doc
   "bf" #'rustic-cargo-fmt
   "bn" #'rustic-cargo-new
   "bo" #'rustic-cargo-outdated
   "br" #'rustic-cargo-run
   "t"  '(:ignore t :which-key "cargo test")
   "ta" #'rustic-cargo-test
   "tt" #'rustic-cargo-current-test)
  :config
  (setq rustic-indent-method-chain    t
        rustic-babel-format-src-block nil
        rustic-format-trigger         nil)
  (remove-hook 'rustic-mode-hook #'flycheck-mode)
  (remove-hook 'rustic-mode-hook #'flymake-mode-off)
  (remove-hook 'rustic-mode-hook #'rustic-setup-lsp))
