(use-package which-key
  :straight (:build t)
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :straight (:build t)
  :init
  (general-auto-unbind-keys)
  :config
  (general-create-definer phundrak/undefine
    :keymaps 'override
    :states '(normal emacs))
  (general-create-definer phundrak/evil
    :states '(normal))
  (general-create-definer phundrak/leader-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer phundrak/major-leader-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "M-m"))

(use-package evil
  :straight (:build t)
  :after (general)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  (require 'evil-vars)
  :config
  (evil-global-set-key 'motion "t" 'evil-next-visual-line)
  (evil-global-set-key 'motion "s" 'evil-previous-visual-line)
  
  (general-define-key
   :keymaps 'evil-motion-state-map
   "SPC" nil
   ","   nil)
  (general-define-key
   :keymaps 'evil-insert-state-map
   "C-t" nil)
  (general-define-key
   :keymaps 'evil-insert-state-map
   "U"   nil
   "C-a" nil
   "C-y" nil
   "C-e" nil)
  (evil-mode 1)
  (setq evil-want-fine-undo t) ; more granular undo with evil
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :straight (:build t)
  :config
  (evil-collection-init))

(use-package hydra
  :straight (:build t)
  :defer t)
