(use-package xenops
    :straight (:build t)
    :after org
    :config
    (add-to-list 'org-latex-packages-alist '("" "amsmath" t))
    (add-to-list 'org-latex-packages-alist '("" "amssymb" t))
    (add-to-list 'org-latex-packages-alist '("" "mathtools" t))
    (add-to-list 'org-latex-packages-alist '("" "mathrsfs" t))
    (setq xenops-math-image-scale-factor 1.7
        xenops-reveal-on-entry t)
)
