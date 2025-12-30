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
  (setq markdown-fontify-code-blocks-natively t))

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
