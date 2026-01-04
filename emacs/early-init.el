(setq package-enable-at-startup nil
      inhibit-startup-message   t
      visual-line-fringe-indicators nil ; remove fringe bitmaps on wrapped lines
      frame-resize-pixelwise    t  ; fine resize
      package-native-compile    t) ; native compile packages
(scroll-bar-mode -1)               ; disable scrollbar
(tool-bar-mode -1)                 ; disable toolbar
(tooltip-mode -1)                  ; disable tooltips
(set-fringe-mode 10)               ; give some breathing room
(menu-bar-mode -1)                 ; disable menubar
(global-display-line-numbers-mode) ; enable lines globally
(setq display-line-numbers-type 'relative) ; enable relative line numbers
(global-visual-line-mode 1)        ; enable line wrap
(desktop-save-mode -1) ; Disables desktop-save-mode
