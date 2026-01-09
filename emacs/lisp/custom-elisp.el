(defun phundrak/open-marked-files (&optional files)
  "Open all marked FILES in Dired buffer as new Emacs buffers."
  (interactive)
  (let* ((file-list (if files
                        (list files)
                      (if (eq major-mode 'dired-mode)
                          (dired-get-marked-files)
                        (list (buffer-file-name))))))
   (mapc (lambda (file-path)
           (find-file file-path))
         file-list)))

(defun add-all-to-list (list-var elements &optional append compare-fn)
  "Add ELEMENTS to the value of LIST-VAR if it isn't there yet.

ELEMENTS is a list of values. For documentation on the variables
APPEND and COMPARE-FN, see `add-to-list'."
  (let (return)
    (dolist (elt elements return)
      (setq return (add-to-list list-var elt append compare-fn)))))

(defun split-window-right-and-focus ()
  "Spawn a new window right of the current one and focus it."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun split-window-below-and-focus ()
  "Spawn a new window below the current one and focus it."
  (interactive)
  (split-window-below)
  (windmove-down))

  (with-eval-after-load 'hydra
    (defhydra windows-adjust-size ()
      "
               _t_: shrink
  _c_: enlarge              _r_: right
               _s_: enlarge
  "
      ("c" enlarge-window-horizontally)
      ("t" shrink-window)
      ("s" enlarge-window)
      ("r" shrink-window-horizontally)))

(defun self-screenshot (&optional type)
  "Save a screenshot of type TYPE of the current Emacs frame.
As shown by the function `', type can wield the value `svg',
`png', `pdf'.

This function will output in /tmp a file beginning with \"Emacs\"
and ending with the extension of the requested TYPE."
  (interactive (list
                (intern (completing-read "Screenshot type: "
                                         '(png svg pdf postscript)))))
  (let* ((extension (pcase type
                      ('png        ".png")
                      ('svg        ".svg")
                      ('pdf        ".pdf")
                      ('postscript ".ps")
                      (otherwise (error "Cannot export screenshot of type %s" otherwise))))
         (filename (make-temp-file "Emacs-" nil extension))
         (data     (x-export-frames nil type)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(defun switch-to-messages-buffer ()
  "Switch to Messages buffer."
  (interactive)
  (switch-to-buffer (messages-buffer)))

(defun switch-to-scratch-buffer ()
  "Switch to Messages buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

  (defvar my/emacs-org-config-directory
    (expand-file-name ".config/emacs" (getenv "HOME"))
    "Location of my config as org files.")

  (defun my/tangle-emacs-config ()
    "Tangle all my Emacs config files from org files."
    (interactive)
    (let ((files (f-files my/emacs-org-config-directory
                  (lambda (file) (f-ext-p file "org"))
                  t))
          (org-confirm-babel-evaluate nil))
      (dolist (file files)
        (message "Tangling %s" file)
        (org-babel-tangle-file file))))
