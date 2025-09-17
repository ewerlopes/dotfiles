(use-package dashboard
  :straight (:build t)
  :ensure t
  :after all-the-icons
  :config
  (setq dashboard-banner-logo-title "Ewerton's Vanilla Emacs"
        dashboard-startup-banner    'logo
        dashboard-center-content    t
        dashboard-show-shortcuts    t
        dashboard-set-navigator     t
        dashboard-set-heading-icons t
        dashboard-set-file-icons    t
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

  (setq dashboard-items '((recents  . 15)
                          (agenda   . 10)
                          (projects . 10)))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer))

(use-package git-gutter-fringe
  :straight (:build t)
  :hook ((prog-mode     . git-gutter-mode)
         (org-mode      . git-gutter-mode)
         (markdown-mode . git-gutter-mode)
         (latex-mode    . git-gutter-mode)))

(use-package gruber-darker-theme
      :straight t
      :config
      (load-theme 'gruber-darker t))

(use-package writeroom-mode
:defer t
:straight (:build t)
:init (global-writeroom-mode 1)
:config
(setq writeroom-width             100
      writeroom-fullscreen-effect t
      writeroom-maximize-window   t
      writeroom-mode-line         t
      writeroom-major-modes       '(text-mode org-mode markdown-mode nov-mode Info-mode)))
