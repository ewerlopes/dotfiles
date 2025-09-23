(dolist (module '(
    "basic-config.el"
    "custom-elisp.el"
    "package-manager.el"
    "keybinding-managers.el"
    "emacs-builtin.el"
    "autocompletion.el"
    "keybindings.el"
    "visual-config.el"
    "org.el"
    "programming.el"
    "ai-assistant.el"
    ))
    (load (expand-file-name module
            (expand-file-name "lisp" user-emacs-directory))))
