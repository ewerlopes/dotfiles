(use-package xenops
    :straight t
    :after org
    ;:hook (add-hook 'org-mode-hook #'xenops-mode)
    :config
    (add-to-list 'org-latex-packages-alist '("" "amsmath" t))
    (add-to-list 'org-latex-packages-alist '("" "amssymb" t))
    (add-to-list 'org-latex-packages-alist '("" "mathtools" t))
    (add-to-list 'org-latex-packages-alist '("" "mathrsfs" t))
    (setq xenops-math-image-scale-factor 1.7
        xenops-reveal-on-entry t)
)
