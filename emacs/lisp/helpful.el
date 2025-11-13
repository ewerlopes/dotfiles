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
