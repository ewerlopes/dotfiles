(add-hook 'before-save-hook #'whitespace-cleanup)

(setq-default sentence-end-double-space nil)

(global-subword-mode 1)

(setq scroll-conservatively 1000)

(setq-default initial-major-mode 'emacs-lisp-mode)

(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq epg-pinentry-mode 'loopback)

(dolist (mode '(prog-mode-hook latex-mode-hook))
  (add-hook mode #'display-line-numbers-mode))

(dolist (mode '(prog-mode-hook latex-mode-hook))
  (add-hook mode #'hs-minor-mode))

(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/"
                                                         user-emacs-directory)))
      tramp-backup-directory-alist `(("." . ,(expand-file-name ".tmp/tramp-backups/"
                                                               user-emacs-directory))))

(setq backup-by-copying t)

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file) ; Don't forget to load it, we still need it
  (load custom-file))

(setq delete-by-moving-to-trash t)

(setq-default initial-scratch-message nil)

(if (version<= emacs-version "28")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setopt use-short-answers t))

(global-auto-revert-mode 1)

(setq undo-limit        100000000
      auto-save-default t)

(setq window-combination-resize t) ; take new window space from all other windows

(setq user-full-name       "Ewerton de Oliveira"
      user-real-login-name "Ewerton de Oliveira"
      user-login-name      "ewerlopes"
      user-mail-address    "ewerlopes@gmail.com")

(setq visible-bell t)

(setq x-stretch-cursor t)

(with-eval-after-load 'mule-util
 (setq truncate-string-ellipsis "…"))

(add-to-list 'default-frame-alist '(alpha-background . 0.9))

(require 'time)
(setq display-time-format "%Y-%m-%d %H:%M")
(display-time-mode 1) ; display time in modeline

(let ((battery-str (battery)))
  (unless (or (equal "Battery status not available" battery-str)
              (string-match-p (regexp-quote "N/A") battery-str))
    (display-battery-mode 1)))

(column-number-mode)

(defun modeline-contitional-buffer-encoding ()
  "Hide \"LF UTF-8\" in modeline.

It is expected of files to be encoded with LF UTF-8, so only show
the encoding in the modeline if the encoding is worth notifying
the user."
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(add-hook 'after-change-major-mode-hook #'modeline-contitional-buffer-encoding)

(defvar phundrak/default-font-size 160
  "Default font size.")

(defvar phundrak/default-font-name "Iosevka Nerd Font" ;; "Cascadia Code"
  "Default font.")

(defun my/set-font ()
  (when (find-font (font-spec :name phundrak/default-font-name))
    (set-face-attribute 'default nil
                        :font phundrak/default-font-name
                        :height phundrak/default-font-size)))

(my/set-font)
(add-hook 'server-after-make-frame-hook #'my/set-font)

(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p) " ◉ %s" "  ●  %s - Emacs") project-name))))))

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(when (equal system-type 'darwin)
  ;; Treat option as meta and command as super
   (setq mac-option-key-is-meta t)
   (setq mac-command-key-is-meta nil)
   (setq mac-command-modifier 'super)
   (setq mac-option-modifier 'meta))
