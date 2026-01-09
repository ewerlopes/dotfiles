(dolist (module '(
    "package-manager.el"
    "basic-config.el"
    "custom-elisp.el"
    "keybinding-managers.el"
    "emacs-builtin.el"
    "autocompletion.el"
    "keybindings.el"
    "visual-config.el"
    "org.el"
    "programming.el"
    "ai-assistant.el"
    "helpful.el"
    "applications.el"
    "latex.el"
    ))
    (load (expand-file-name module
            (expand-file-name "lisp" user-emacs-directory))))
