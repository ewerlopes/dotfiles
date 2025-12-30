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
    "latex.el"
    "programming.el"
    "ai-assistant.el"
    "helpful.el"
    "applications.el"
    ))
    (load (expand-file-name module
            (expand-file-name "lisp" user-emacs-directory))))
