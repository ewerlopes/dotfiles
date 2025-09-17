(dolist (module '("basic-config.el" "custom-elisp.el"  "package-manager.el" "keybinding-managers.el" "keybindings.el" "visual-config.el" "org.el" "programming.el"))
    (load (expand-file-name module
            (expand-file-name "lisp" user-emacs-directory))))
