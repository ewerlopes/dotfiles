(use-package bufler
  :straight (bufler :build t
                    :files (:defaults (:exclude "helm-bufler.el")))
  :defer t
  :general
  (phundrak/evil
   :keymaps  'bufler-list-mode-map
   :packages 'bufler
   "?"   #'hydra:bufler/body
   "g"   #'bufler
   "f"   #'bufler-list-group-frame
   "F"   #'bufler-list-group-make-frame
   "N"   #'bufler-list-buffer-name-workspace
   "k"   #'bufler-list-buffer-kill
   "p"   #'bufler-list-buffer-peek
   "s"   #'bufler-list-buffer-save
   "RET" #'bufler-list-buffer-switch))

  (use-package beancount
    :straight (:build t)
    :ensure t
    :mode (("\\.beancount\\'" . beancount-mode))
    :config
    ;; Optional: Add configuration settings here.
    ;; Example: Align numbers to a specific column
    (setq beancount-number-alignment-column 52)

    ;; Optional: Define custom keybindings, e.g. for inserting the current date
    ;; (global-set-key (kbd "C-c d") 'beancount-insert-date)

    ;; Ensure beancount executable is in the Emacs PATH for tools like bean-check
    (setenv "PATH" (concat (getenv "PATH") ":/Users/ewerlopes/coding/contas/.venv/bin"))
    (add-to-list 'exec-path "/Users/ewerlopes/coding/contas/.venv/bin")
  )
