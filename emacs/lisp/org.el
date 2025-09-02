(use-package citeproc
  :after (org)
  :defer t
  :straight (:build t))

(use-package org
  :straight t
  :defer t
  :after engrave-faces
  :commands (orgtbl-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-num-mode))
  :custom-face
  (org-macro ((t (:foreground "#b48ead"))))
  :init
  (auto-fill-mode -1)
  :config
  (defhydra org-babel-transient ()
    "
  ^Navigate^                    ^Interact
  ^^^^^^^^^^^------------------------------------------
  [_t_/_s_] navigate src blocs  [_x_] execute src block
  [_g_]^^   goto named block    [_'_] edit src block
  [_z_]^^   recenter screen     [_q_] quit
  "
    ("q" nil :exit t)
    ("t" org-babel-next-src-block)
    ("s" org-babel-previous-src-block)
    ("g" org-babel-goto-named-src-block)
    ("z" recenter-top-bottom)
    ("x" org-babel-execute-maybe)
    ("'" org-edit-special :exit t))
  (require 'ox-beamer)
  (require 'org-protocol)
  (setq org-hide-leading-stars             nil
        org-hide-macro-markers             t
        org-ellipsis                       " ⤵"
        org-image-actual-width             600
        org-redisplay-inline-images        t
        org-display-inline-images          t
        org-startup-with-inline-images     "inlineimages"
        org-pretty-entities                t
        org-fontify-whole-heading-line     t
        org-fontify-done-headline          t
        org-fontify-quote-and-verse-blocks t
        org-startup-indented               t
        org-startup-align-all-tables       t
        org-use-property-inheritance       t
        org-list-allow-alphabetical        t
        org-M-RET-may-split-line           nil
        org-src-window-setup               'split-window-below
        org-src-fontify-natively           t
        org-src-tab-acts-natively          t
        org-src-preserve-indentation       t
        org-log-done                       'time
        org-directory                      "~/org"
        org-default-notes-file             (expand-file-name "notes.org" org-directory))
  (with-eval-after-load 'oc
   (setq org-cite-global-bibliography '("~/org/bibliography/references.bib")))
  
  (add-hook 'org-mode-hook (lambda ()
                             (interactive)
                             (electric-indent-local-mode -1)))
  (defvar org-notes-file "~/org/notes.org")
  (defvar org-journal-file "~/org/journal.org")
  (defvar org-novel-file "~/org/novel.org")
  (defvar org-agenda-file "~/org/agenda/private.org")
  (setq org-capture-templates
        '(
          ("e" "Email")
          ("ew" "Write Email" entry
            (file+headline org-default-notes-file "Emails")
            (file "~/org/capture/email.orgcaptmpl"))
          ("j" "Journal" entry
            (file+datetree org-journal-file ##)
            (file "~/org/capture/journal.orgcaptmpl"))
          ("l" "Link")
          ("ll" "General" entry
            (file+headline org-default-notes-file "General")
            (file "~/org/capture/link.orgcaptmpl"))
          ("ly" "YouTube" entry
            (file+headline org-default-notes-file "YouTube")
            (file "~/org/capture/youtube.orgcaptmpl"))
          ("L" "Protocol Link" entry
            (file+headline org-default-notes-file "Link")
            (file "~/org/capture/protocol-link.orgcaptmpl"))
          ("n" "Notes")
          ("nc" "Conlanging" entry
            (file+headline org-conlanging-file "Note")
            (file "~/org/capture/notes.orgcaptmpl"))
          ("nn" "General" entry
            (file+headline org-default-notes-file "General")
            (file "~/org/capture/notes.orgcaptmpl"))
          ("nN" "Novel" entry
            (file+headline org-novel-notes-file "Note")
            (file "~/org/capture/notes.orgcaptmpl"))
          ("nq" "Quote" entry
            (file+headline org-default-notes-file "Quote")
            (file "~/org/capture/notes-quote.orgcaptmpl"))
          ("nw" "Worldbuilding" entry
            (file+headline org-wordbuilding-file "Note")
            (file "~/org/capture/notes.orgcaptmpl"))
          ("N" "Novel")
          ("Ni" "Ideas" entry
            (file+headline org-novel-notes-file "Ideas")
            (file "~/org/capture/notes.orgcaptmpl"))
          ("p" "Protocol" entry
            (file+headline org-default-notes-file "Link")
            (file "~/org/capture/protocol.orgcaptmpl"))
          ("r" "Resources")
          ("rc" "Conlanging" entry
            (file+headline org-conlanging-file "Resources")
            (file "~/org/capture/resource.orgcaptmpl"))
          ("re" "Emacs" entry
            (file+headline org-default-notes-file "Emacs")
            (file "~/org/capture/resource.orgcaptmpl"))
          ("ri" "Informatique" entry
            (file+headline org-default-notes-file "Informatique")
            (file "~/org/capture/resource.orgcaptmpl"))
          ("rl" "Linguistics" entry
            (file+headline org-default-notes-file "Linguistics")
            (file "~/org/capture/resource.orgcaptmpl"))
          ("rL" "Linux" entry
            (file+headline org-default-notes-file "Linux")
            (file "~/org/capture/resource.orgcaptmpl"))
          ("rw" "Worldbuilding" entry
            (file+headline org-wordbuilding-file "Resources")
            (file "~/org/capture/resource.orgcaptmpl"))
          ("t" "Tasks")
          ("tb" "Birthday" entry
            (file+headline org-private-agenda-file "Birthday")
            (file "~/org/capture/birthday.orgcaptmpl"))
          ("te" "Event" entry
            (file+headline org-private-agenda-file "Event")
            (file "~/org/capture/event.orgcaptmpl"))
          ("th" "Health" entry
            (file+headline org-private-agenda-file "Health")
            (file "~/org/capture/health.orgcaptmpl"))
          ("ti" "Informatique" entry
            (file+headline org-private-agenda-file "Informatique")
            (file "~/org/capture/informatique.orgcaptmpl"))
          ))
  (defun org-emphasize-bold ()
    "Emphasize as bold the current region."
    (interactive)
    (org-emphasize 42))
  (defun org-emphasize-italic ()
    "Emphasize as italic the current region."
    (interactive)
    (org-emphasize 47))
  (defun org-emphasize-underline ()
    "Emphasize as underline the current region."
    (interactive)
    (org-emphasize 95))
  (defun org-emphasize-verbatim ()
    "Emphasize as verbatim the current region."
    (interactive)
    (org-emphasize 61))
  (defun org-emphasize-code ()
    "Emphasize as code the current region."
    (interactive)
    (org-emphasize 126))
  (defun org-emphasize-strike-through ()
    "Emphasize as strike-through the current region."
    (interactive)
    (org-emphasize 43))
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (makefile . t)
     (python . t)
     (shell . t)
     (sql . t)))
  (setq org-use-sub-superscripts (quote {}))
  (setq org-latex-compiler "xelatex"
        org-latex-prefer-user-labels t)
  (require 'engrave-faces)
  (setq org-latex-src-block-backend 'engraved)
  (dolist (package '(("AUTO" "inputenc" t ("pdflatex"))
                     ("T1"   "fontenc"  t ("pdflatex"))
                     (""     "grffile"  t)))
    (delete package org-latex-default-packages-alist))
  
  (dolist (package '(("AUTO" "babel" nil ("pdflatex"))
                     ("AUTO" "polyglossia" nil ("xelatex" "lualatex"))
                     ("capitalize" "cleveref")
                     (""           "booktabs")
                     (""           "tabularx")))
    (add-to-list 'org-latex-default-packages-alist package t))
  
  (setq org-latex-reference-command "\\cref{%s}")
  (setq org-export-latex-hyperref-format "\\ref{%s}")
  (setq org-latex-pdf-process
        '("tectonic -Z shell-escape --synctex --outdir=%o %f"))
  (dolist (ext '("bbl" "lot"))
    (add-to-list 'org-latex-logfiles-extensions ext t))
  
  
  (eval-after-load "ox-latex"
    '(progn
       (add-to-list 'org-latex-classes
                    '("conlang"
                      "\\documentclass{book}"
                      ("\\chapter{%s}" . "\\chapter*{%s}")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
       (add-to-list 'org-latex-classes
                    `("beamer"
                      ,(concat "\\documentclass[presentation]{beamer}\n"
                               "[DEFAULT-PACKAGES]"
                               "[PACKAGES]"
                               "[EXTRA]\n")
                      ("\\section{%s}" . "\\section*{%s}")
                      ("\\subsection{%s}" . "\\subsection*{%s}")
                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
  
  (add-hook 'org-mode-hook
            (lambda ()
              (dolist (pair '(("[ ]"         . ?☐)
                              ("[X]"         . ?☑)
                              ("[-]"         . ?❍)
                              ("#+title:"    . ?📕)
                              ("#+TITLE:"    . ?📕)
                              ("#+author:"   . ?✎)
                              ("#+AUTHOR:"   . ?✎)
                              ("#+email:"    . ?📧)
                              ("#+EMAIL:"    . ?📧)
                              ("#+include"   . ?⭳)
                              ("#+INCLUDE"   . ?⭳)
                              ("#+begin_src" . ?λ)
                              ("#+BEGIN_SRC" . ?λ)
                              ("#+end_src"   . ?λ)
                              ("#+END_SRC"   . ?λ)))
                (add-to-list 'prettify-symbols-alist pair))
              (prettify-symbols-mode)))
  :general
  (phundrak/evil
    :keymaps 'org-mode-map
    :packages 'org
    "RET" 'org-open-at-point)
  (phundrak/major-leader-key
    :keymaps 'org-mode-map
    :packages 'org
    "RET" #'org-ctrl-c-ret
    "*" #'org-ctrl-c-star
    "," #'org-ctrl-c-ctrl-c
    "'" #'org-edit-special
    "-" #'org-ctrl-c-minus
    "a" #'org-agenda
    "c" #'org-capture
    "C" #'org-columns
    "e" #'org-export-dispatch
    "l" #'org-store-link
    "p" #'org-priority
    "r" #'org-reload
    "b" '(:ignore t :wk t)
    "b." #'org-babel-transient/body
    "bb" #'org-babel-execute-buffer
    "bc" #'org-babel-check-src-block
    "bC" #'org-babel-tangle-clean
    "be" #'org-babel-execute-maybe
    "bf" #'org-babel-tangle-file
    "bn" #'org-babel-next-src-block
    "bo" #'org-babel-open-src-block-result
    "bp" #'org-babel-previous-src-block
    "br" #'org-babel-remove-result-one-or-many
    "bR" #'org-babel-goto-named-result
    "bt" #'org-babel-tangle
    "bi" #'org-babel-view-src-block-info
    "d" '(:ignore t :wk t)
    "dd" #'org-deadline
    "ds" #'org-schedule
    "dt" #'org-time-stamp
    "dT" #'org-time-stamp-inactive
    "i" '(:ignore t :wk t)
    "ib" #'org-insert-structure-template
    "ic" '(:ignore t :wk "conlanging")
    "ica" '(conlanging-eittlandic-insert-adjective-declension :wk "adjective":package conlanging)
    "icn" '(conlanging-eittlandic-insert-noun-declensions :wk "noun":package conlanging)
    "icv" '(conlanging-eittlandic-insert-verb-declension :wk "verb":package conlanging)
    "id" #'org-insert-drawer
    "ie" '(:ignore t :wk t)
    "ieb" #'org-emphasize-bold
    "iec" #'org-emphasize-code
    "ifh" #'org-insert-ultra-fancy-habit-summary
    "iei" #'org-emphasize-italic
    "ies" #'org-emphasize-strike-through
    "ieu" #'org-emphasize-underline
    "iev" #'org-emphasize-verbatim
    "iE" #'org-set-effort
    "if" #'org-footnote-new
    "ih" #'org-insert-heading
    "iH" #'counsel-org-link
    "ii" #'org-insert-item
    "il" #'org-insert-link
    "in" #'org-add-note
    "ip" #'org-set-property
    "is" #'org-insert-subheading
    "it" #'org-set-tags-command
    "iV" '(conlang-store-heading-vuepress :wk "store Vuepress link":package conlanging)
    "iv" '(conlanging-insert-heading-vuepress :wk "vuepress link":package conlanging)
    "j" '(:ignore t :wk t)
    "ja" #'counsel-org-goto-all
    "jh" #'counsel-org-goto
    "t" '(:ignore t :wk t)
    "tc" #'org-table-move-column-left
    "tt" #'org-table-move-row-down
    "ts" #'org-table-move-row-up
    "tr" #'org-table-move-column-right
    "ta" #'org-table-align
    "te" #'org-table-eval-formula
    "tf" #'org-table-field-info
    "tF" #'org-table-edit-formulas
    "th" #'org-table-convert
    "tl" #'org-table-recalculate
    "tp" #'org-plot/gnuplot
    "tS" #'org-table-sort-lines
    "tw" #'org-table-wrap-region
    "tx" #'org-table-shrink
    "tN" #'org-table-create-with-table.el
    "td" '(:ignore t :wk t)
    "tdc" #'org-table-delete-column
    "tdr" #'org-table-kill-row
    "ti" '(:ignore t :wk t)
    "tic" #'org-table-insert-column
    "tih" #'org-table-insert-hline
    "tir" #'org-table-insert-row
    "tiH" #'org-table-hline-and-move
    "tt" '(:ignore t :wk t)
    "ttf" #'org-table-toggle-formula-debugger
    "tto" #'org-table-toggle-coordinate-overlays
    "T" '(:ignore t :wk t)
    "Tc" #'org-toggle-checkbox
    "Ti" #'org-toggle-inline-images
    "Tl" #'org-latex-preview
    "Tn" #'org-num-mode
    "Ts" #'phundrak/toggle-org-src-window-split
    "Tt" #'org-show-todo-tree
    "TT" #'org-todo)
  
  (phundrak/major-leader-key
    :packages 'org
    :keymaps 'org-src-mode-map
    "'" #'org-edit-src-exit
    "k" #'org-edit-src-abort))

(use-package org-contrib
  :after (org)
  :defer t
  :straight (:build t)
  :init
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

;; Resume clocking task when emacs restarts.
(org-clock-persistence-insinuate)
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(require 'org-habit)

(defun get-habits-from-file ()
  (org-map-entries
   (lambda ()
     (when (string= (org-entry-get nil "STYLE") "habit")
       (cons (org-get-heading t t t t)
             (org-entry-get nil "LAST_REPEAT"))))
   "STYLE=\"habit\"" 'file))

(defun count-habit-completions (last-repeat)
  (let* ((now (current-time))
         (week-start (time-subtract now (days-to-time (nth 6 (decode-time now)))))
         (last-done (org-time-string-to-time last-repeat)))
    (if (time-less-p week-start last-done)
        1
      0)))

(defun parse-habit-data (habit-name last-repeat)
  (let ((data '())
        (max-streak 0)
        (current-streak 0))
    (org-map-entries
     (lambda ()
       (let ((state-changes (org-entry-get nil "LOGGING" t)))
         (when state-changes
           (dolist (change (split-string state-changes "\n"))
             (when (string-match "\\[$[0-9]+-[0-9]+-[0-9]+$.*$$ State \"DONE\"" change)
               (push (match-string 1 change) data))))))
     (concat "+STYLE=\"habit\"+" (regexp-quote habit-name))
     'file)
    (setq data (nreverse data))
    (let* ((now (current-time))
           (streak-end now)
           (day-sec 86400))
      (when last-repeat
        (push last-repeat data))
      (dolist (date data)
        (let ((date-time (org-time-string-to-time date)))
          (if (time-less-p
               (time-subtract streak-end (seconds-to-time day-sec))
               date-time)
              (setq current-streak (1+ current-streak))
            (setq max-streak (max max-streak current-streak))
            (setq current-streak 1))
          (setq streak-end date-time)))
      (setq max-streak (max max-streak current-streak)))
    (list current-streak max-streak)))

(defun calculate-trend (habit-name)
  (let* ((last-repeat (cdr (assoc habit-name (get-habits-from-file))))
         (this-week (count-habit-completions last-repeat))
         (last-week-time (time-subtract (current-time) (days-to-time 7)))
         (last-week (if (time-less-p (org-time-string-to-time last-repeat) last-week-time) 0 1)))
    (cond ((> this-week last-week) "↑")
          ((< this-week last-week) "↓")
          (t "→"))))

(defun org-insert-ultra-fancy-habit-summary ()
  (interactive)
  (let ((habits (get-habits-from-file)))
    (insert "| Habit | This Week | Streak | Max Streak | Trend |\n|---|---|---|---|---|\n")
    (dolist (habit habits)
      (let* ((name (car habit))
             (last-repeat (cdr habit))
             (this-week (count-habit-completions last-repeat))
             (streak-data (parse-habit-data name last-repeat))
             (streak (car streak-data))
             (max-streak (cadr streak-data))
             (trend (calculate-trend name)))
        (insert (format "| %s | %d/7 | %d | %d | %s |\n"
                        name this-week streak max-streak trend))))
    (org-table-align)))

(setq org-habit-show-all-today t
      org-habit-show-habits-only-for-today nil)

(setq org-agenda-files (apply 'append
      (mapcar
       (lambda (directory)
         (directory-files-recursively
           directory org-agenda-file-regexp))
           '("~/org/agenda"))))

(eval-after-load 'org
  '(progn
     (setq org-agenda-start-day "-0d")
     (setq org-agenda-start-on-weekday nil)))

(use-package org-super-agenda
  :after org-agenda
  :init
  (setq
        org-agenda-time-grid
        (quote
         ((daily today require-timed)
          (0700 0800 0900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300)
          "......" "-----------------------------------------------------"))
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary nil
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-span 1
        org-agenda-start-with-log-mode t
        org-agenda-custom-commands
        '(("o" "Overview"
                    ((agenda "" ((org-agenda-span 'day)
                                 (org-super-agenda-groups
                                  '((:name "\nToday"
                                      :time-grid t
                                      :date today
                                      :todo "TODAY"
                                      :scheduled today
                                      :order 0)
                                    (:name "Habits"  ; Optionally specify section name
                                      :todo "TODAY"
                                      :habit t
                                      :order 1
                                      )))))
                     (alltodo "" ((org-agenda-overriding-header "\nCategories")
                                  (org-super-agenda-groups
                                   '(
                                     (:name "Started"
                                      :todo "STRT"
                                      :order 5)
                                     (:name "Important"
                                      :tag "Important"
                                      :priority "A"
                                      :order 3)
                                     (:name "Due Today"
                                      :deadline today
                                      :order 4)
                                     (:name "Due Soon"
                                      :deadline future
                                      :order 8)
                                     (:name "Overdue"
                                      :deadline past
                                      :scheduled past
                                      :face error
                                      :order 7)
                                      (:name "To Refile"
                                      :and(
                                          :todo "TODO"
                                          :not (:habit t)
                                      )
                                      :order 9)
                                     (:name "To read"
                                      :tag "Read"
                                      :order 30)
                                     (:name "Waiting"
                                      :todo "WAITING"
                                      :order 20)
                                     (:name "Trivial"
                                            :priority<= "C"
                                            :tag ("Trivial" "Unimportant")
                                            :todo ("SOMEDAY")
                                            :order 90)
                                     (:discard (:anything t))))))))))
  :config
  (org-super-agenda-mode))

(use-package ob-async
  :straight (:build t)
  :defer t
  :after (org ob))

(use-package ob-latex-as-png
  :after org
  :straight (:build t))

(use-package ob-restclient
  :straight (:build t)
  :defer t
  :after (org ob)
  :init
  (add-to-list 'org-babel-load-languages '(restclient . t)))

(use-package toc-org
  :after (org markdown-mode)
  :straight (:build t)
  :init
  (add-to-list 'org-tag-alist '("TOC" . ?T))
  :hook (org-mode . toc-org-enable)
  :hook (markdown-mode . toc-org-enable))

(use-package org-unique-id
  :straight (org-unique-id :build t
                           :type git
                           :host github
                           :repo "Phundrak/org-unique-id")
  :defer t
  :after org
  :init (add-hook 'before-save-hook #'org-unique-id-maybe))

(defun phundrak/toggle-org-src-window-split ()
  "This function allows the user to toggle the behavior of
`org-edit-src-code'. If the variable `org-src-window-setup' has
the value `split-window-right', then it will be changed to
`split-window-below'. Otherwise, it will be set back to
`split-window-right'"
  (interactive)
  (if (equal org-src-window-setup 'split-window-right)
      (setq org-src-window-setup 'split-window-below)
    (setq org-src-window-setup 'split-window-right))
  (message "Org-src buffers will now split %s"
           (if (equal org-src-window-setup 'split-window-right)
               "vertically"
             "horizontally")))

(use-package ox-hugo
  :defer t
  :after ox
  :straight t)

(use-package engrave-faces
  :straight (:build t))

(use-package org-roam
  :straight (:build t)
  :defer t
  :custom
  (org-roam-directory (expand-file-name "org/library/" (getenv "HOME")))
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-mode 1)
  :general
  (phundrak/major-leader-key
    :keymaps 'org-mode-map
    :packages '(org org-roam)
    "h"   #'org-id-get-create
    "r"   '(:ignore t :which-key "roam")
    "ra"  '(:ignore t :which-key "alias")
    "raa" #'org-roam-alias-add
    "rar" #'org-roam-alias-remove))

(use-package org-roam-ui
  :straight (:build t)
  :defer t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package reftex
  :commands turn-on-reftex
  :init (setq reftex-default-bibliography "~/org/bibliography/references.bib"
              reftex-plug-into-AUCTeX     t))

(use-package org-ref
  ;; :after (org ox-bibtex pdf-tools)
  :after org
  :defer t
  :straight (:build t)
  :custom-face
  (org-ref-cite-face ((t (:weight bold))))
  :init
  (setq org-ref-completion-library    'org-ref-ivy-cite
        org-latex-logfiles-extensions '("lof" "lot" "aux" "idx" "out" "log" "fbd_latexmk"
                                        "toc" "nav" "snm" "vrb" "dvi" "blg" "brf" "bflsb"
                                        "entoc" "ps" "spl" "bbl" "pygtex" "pygstyle"))
  (add-hook 'org-mode-hook (lambda () (require 'org-ref)))
  :config
  (setq bibtex-completion-pdf-field    "file"
        bibtex-completion-notes-path   "~/org/bibliography/notes/"
        bibtex-completion-bibliography "~/org/bibliography/references.bib"
        bibtex-completion-library-path "~/org/bibliography/bibtex-pdfs/"
        bibtex-completion-pdf-symbol   "⌘"
        bibtex-completion-notes-symbol "✎")
  :general
  (phundrak/evil
   :keymaps 'bibtex-mode-map
   :packages 'org-ref
   "C-t" #'org-ref-bibtex-next-entry
   "C-s" #'org-ref-bibtex-previous-entry
   "gt"  #'org-ref-bibtex-next-entry
   "gs"  #'org-ref-bibtex-previous-entry)
  (phundrak/major-leader-key
   :keymaps '(bibtex-mode-map)
   :packages 'org-ref
   ;; Navigation
   "t" #'org-ref-bibtex-next-entry
   "s" #'org-ref-bibtex-previous-entry

   ;; Open
   "b" #'org-ref-open-in-browser
   "n" #'org-ref-open-bibtex-notes
   "p" #'org-ref-open-bibtex-pdf

   ;; Misc
   "h" #'org-ref-bibtex-hydra/body
   "i" #'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
   "s" #'org-ref-sort-bibtex-entry

   "l" '(:ignore t :which-key "lookup")
   "la" #'arxiv-add-bibtex-entry
   "lA" #'arxiv-get-pdf-add-bibtex-entry
   "ld" #'doi-utils-add-bibtex-entry-from-doi
   "li" #'isbn-to-bibtex
   "lp" #'pubmed-insert-bibtex-from-pmid)
  (phundrak/major-leader-key
   :keymaps 'org-mode-map
   :pakages 'org-ref
   "iC" #'org-ref-insert-link
   "iL" #'org-ref-insert-ref-link
   "ir" #'org-ref-insert-link-hydra/body
   "iB" #'org-ref-bibtex-hydra/body))

(use-package ivy-bibtex
  :defer t
  :straight (:build t)
  :config
  (setq bibtex-completion-pdf-open-function #'find-file)
  :general
  (phundrak/leader-key
    :keymaps '(bibtex-mode-map)
    :packages 'ivy-bibtex
    "m" #'ivy-bibtex))

(defun my/tangle-config-file ()
  (when (and (eq major-mode 'org-mode)
             (f-ancestor-of-p (f-full "~/.nosync/org/config") default-directory))
    (org-babel-tangle)))

(add-hook 'after-save-hook #'my/tangle-config-file)

(use-package mixed-pitch
  :after org
  :straight (:build t)
  :hook
  (org-mode           . mixed-pitch-mode)
  (emms-browser-mode  . mixed-pitch-mode)
  (emms-playlist-mode . mixed-pitch-mode)
  :config
  (add-hook 'org-agenda-mode-hook (lambda () (mixed-pitch-mode -1))))

(use-package org-appear
  :after org
  :straight (:build t)
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis   t
        org-hide-emphasis-markers t
        org-appear-autolinks      t
        org-appear-autoentities   t
        org-appear-autosubmarkers t)
  (run-at-time nil nil #'org-appear--set-elements))

(use-package org-fragtog
  :defer t
  :after org
  :straight (:build t)
  :hook (org-mode . org-fragtog-mode))

(use-package org-modern
  :straight (:build t)
  :after org
  :defer t
  :custom (org-modern-table nil)
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda))

(use-package org-fancy-priorities
  :after (org all-the-icons)
  :straight (:build t)
  :hook (org-mode        . org-fancy-priorities-mode)
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list `(,(all-the-icons-faicon "flag"     :height 1.1 :v-adjust 0.0)
                                    ,(all-the-icons-faicon "arrow-up" :height 1.1 :v-adjust 0.0)
                                    ,(all-the-icons-faicon "square"   :height 1.1 :v-adjust 0.0))))

(use-package org-ol-tree
  :after (org avy)
  :defer t
  :straight (org-ol-tree :build t
                         :host github
                         :type git
                         :repo "Townk/org-ol-tree")
  :general
  (phundrak/major-leader-key
    :packages 'org-ol-tree
    :keymaps 'org-mode-map
    "O" #'org-ol-tree))

(add-hook 'org-mode-hook
          (lambda ()
            (dolist (pair '(("[ ]"         . ?☐)
                            ("[X]"         . ?☑)
                            ("[-]"         . ?❍)
                            ("#+title:"    . ?📕)
                            ("#+TITLE:"    . ?📕)
                            ("#+author:"   . ?✎)
                            ("#+AUTHOR:"   . ?✎)
                            ("#+email:"    . ?📧)
                            ("#+EMAIL:"    . ?📧)
                            ("#+include"   . ?⭳)
                            ("#+INCLUDE"   . ?⭳)
                            ("#+begin_src" . ?λ)
                            ("#+BEGIN_SRC" . ?λ)
                            ("#+end_src"   . ?λ)
                            ("#+END_SRC"   . ?λ)))
              (add-to-list 'prettify-symbols-alist pair))
            (prettify-symbols-mode)))

(use-package org-tree-slide
  :defer t
  :after org
  :straight (:build t)
  :config
  (setq org-tree-slide-skip-done nil)
  :general
  (phundrak/evil
    :keymaps 'org-mode-map
    :packages 'org-tree-slide
    "<f8>" #'org-tree-slide-mode)
  (phundrak/major-leader-key
    :keymaps 'org-tree-slide-mode-map
    :packages 'org-tree-slide
    "d" (lambda () (interactive (setq org-tree-slide-skip-done (not org-tree-slide-skip-done))))
    "p" #'org-tree-slide-move-next-tree
    "n" #'org-tree-slide-move-previous-tree
    "t" #'org-tree-slide-move-next-tree
    "s" #'org-tree-slide-move-previous-tree
    "u" #'org-tree-slide-content))
