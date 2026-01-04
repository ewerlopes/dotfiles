(use-package dirvish
  :straight (:build t)
  :defer t
  :init (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")))
  (dirvish-mode-line-format
   '(:left (sort file-time "" file-size symlink) :right (omit yank index)))
  (dirvish-attributes '(file-size collapse subtree-state vc-state git-msg))
  :config
  (when (eq system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls"))
  (dirvish-peek-mode)
  (setopt dired-mouse-drag-files                   t
          mouse-drag-and-drop-region-cross-program t)
  (setopt dired-listing-switches (string-join '("--all"
                                                "--human-readable"
                                                "--time-style=long-iso"
                                                "--group-directories-first"
                                                "-lv1")
                                              " "))
  (let ((my/file (lambda (path &optional dir)
                   (expand-file-name path (or dir user-emacs-directory))))
        (my/dir (lambda (path &optional dir)
                  (expand-file-name (file-name-as-directory path)
                                    (or dir user-emacs-directory)))))
    (setopt image-dired-thumb-size             150
            image-dired-dir                    (funcall my/dir "dired-img")
            image-dired-db-file                (funcall my/file "dired-db.el")
            image-dired-gallery-dir            (funcall my/dir "gallery")
            image-dired-temp-image-file        (funcall my/file "temp-image" image-dired-dir)
            image-dired-temp-rotate-image-file (funcall my/file "temp-rotate-image" image-dired-dir)))
  ; (dirvish-define-preview eza (file)
  ;   "Use `eza' to generate directory preview."
  ;   :require ("eza")
  ;   (when (file-directory-p file)
  ;     `(shell . ("eza" "--color=always" "-al" ,file))))
  ; 
  ; (add-to-list 'dirvish-preview-dispatchers 'eza)
  (setq dired-dwim-target         t
        dired-recursive-copies    'always
        dired-recursive-deletes   'top
        delete-by-moving-to-trash t
        ; dirvish-preview-dispatchers (cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers)
  )
  :general
  (phundrak/evil
    :keymaps 'dirvish-mode-map
    :packages '(dired dirvish)
    "q" #'dirvish-quit
    "TAB" #'dirvish-subtree-toggle)
  (phundrak/major-leader-key
    :keymaps 'dirvish-mode-map
    :packages '(dired dirvish)
    "A"   #'gnus-dired-attach
    "a"   #'dirvish-quick-access
    "d"   #'dirvish-dispatch
    "e"   #'dirvish-emerge-menu
    "f"   #'dirvish-fd-jump
    "F"   #'dirvish-file-info-menu
    "h"   '(:ignore t :which-key "history")
    "hp"  #'dirvish-history-go-backward
    "hn"  #'dirvish-history-go-forward
    "hj"  #'dirvish-history-jump
    "hl"  #'dirvish-history-last
    "l"   '(:ignore t :which-key "layout")
    "ls"  #'dirvish-layout-switch
    "lt"  #'dirvish-layout-toggle
    "m"   #'dirvish-mark-menu
    "s"   #'dirvish-quicksort
    "S"   #'dirvish-setup-menu
    "y"   #'dirvish-yank-menu
    "n"   #'dirvish-narrow))

(setopt dired-listing-switches (string-join '("--all"
                                              "--human-readable"
                                              "--time-style=long-iso"
                                              "--group-directories-first"
                                              "-lv1")
                                            " "))

(use-package dired-rsync
  :if (executable-find "rsync")
  :defer t
  :straight (:build t)
  :general
  (phundrak/evil
    :keymaps 'dired-mode-map
    :packages 'dired-rsync
    "C-r" #'dired-rsync))
