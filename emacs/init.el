(dolist (module '("basic-config.el" "package-manager.el" "keybinding-managers.el" "keybindings.el" "org.el" "visual-config.el"))
    (load (expand-file-name module
            (expand-file-name "lisp" user-emacs-directory))))
