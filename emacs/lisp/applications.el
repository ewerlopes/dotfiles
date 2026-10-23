;; -*- lexical-binding: t; -*-
(use-package magit
  :straight (:build t)
  :defer t
  :init
  (setq forge-add-default-bindings nil)
  :config
  (add-hook 'magit-process-find-password-functions 'magit-process-password-auth-source)
  ;(defun my/magit-log-highlight-angular-keywords (_rev msg)
  ;  "Highlight angular-style keywords in commit messages."
  ;  (let ((boundary 0))
  ;    (when (string-match (rx (seq (or "feat" "fix" "docs" "style" "refactor"
  ;                                     "perf" "test" "ci" "chore")
  ;                                 (* "(" (* (not ")")) ")")
  ;                                 ":"))
  ;                        msg
  ;                        boundary)
  ;        (setq boundary (match-end 0))
  ;        (magit--put-face (match-beginning 0) boundary
  ;                         'magit-keyword msg)))
  ;  msg)
  ;
  ;(advice-add #'magit-log-propertize-keywords
  ;            :after
  ;            #'my/magit-log-highlight-angular-keywords)
  (setopt magit-clone-default-directory "~/coding/"
          magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (with-eval-after-load 'evil-collection
    (phundrak/evil
      :packages '(evil-collection magit)
      :keymaps '(magit-mode-map magit-log-mode-map magit-status-mode-map)
      :states 'normal
      "t" #'magit-tag
      "s" #'magit-stage))
  :general
  (:keymaps '(git-rebase-mode-map)
   :packages 'magit
   "C-t" #'evil-next-line
   "C-s" #'evil-previous-line)
  (phundrak/major-leader-key
    :keymaps 'git-rebase-mode-map
    :packages 'magit
    "," #'with-editor-finish
    "k" #'with-editor-cancel
    "a" #'with-editor-cancel))

(use-package ripgrep
  :if (executable-find "rg")
  :straight (:build t)
  :defer t)

(use-package projectile
  :straight (:build t)
  :diminish projectile-mode
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode)
  (add-to-list 'projectile-ignored-projects "~/")
  (add-to-list 'projectile-globally-ignored-directories "^node_modules$")
  :general
  (phundrak/leader-key
    "p" '(:keymap projectile-command-map :which-key "projectile")))

(use-package recentf
  :straight (:build t :type built-in)
  :custom ((recentf-max-saved-items 200))
  :config
  (add-all-to-list 'recentf-exclude
                  `(,(rx (* any)
                          (or "org/config"
                              ".cache/")
                          (* any)
                          (? (or "html" "pdf" "tex" "epub")))
                    ,(rx (* any)
                          ".elc"
                          eol)
                    ,(rx "/"
                          (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
                          (* any))
                    "/nix/.*"
                    ,(rx "/" "org" "/" "agenda" "/" (* any) ".org" eol))))
