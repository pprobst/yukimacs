;;; yukimacs-theme.el --- A theme based on Tomorrow Night

(deftheme yukimacs "2021-05-19")

(let ((class '((class color) (min-colors 89)))
      (background "#000000")
      (fringe-color "#0A0A0A")
      (current-line "#141414")
      (selection "#323333")
      (foreground "#ffffff")
      (comment "#A5A7A8")
      (cursor "#B9BCBD")
      (red "#CC6666")
      (more-red "#E23C42")
      (orange "#de935f")
      (yellow "#f0c674")
      (green "#6A9B88")
      (more-green "#6A9B76")
      (hl-green "#365148")
      (aqua "#6A8F9B")
      (blue "#5083A9")
      (clear-blue "#6D9AC5")
      (purple "#b294bb")
      (purple-code "#B68BC4")
      (more-purple "#543D62"))

  (custom-theme-set-faces
   'yukimacs

   `(default ((,class (:background ,background :foreground ,foreground))))
   `(fringe ((,class (:background ,fringe-color))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(mode-line ((,class (:background ,current-line :foreground ,foreground))))
   `(region ((,class (:background ,selection))))
   `(error ((,class (:foreground ,more-red :weight bold))))
   `(escape-glyph ((,class (:foreground ,clear-blue))))
   `(highlight ((,class (:background ,hl-green))))
   `(link ((,class (:foreground ,clear-blue :underline t))))
   `(success ((,class (:foreground ,more-green :weight bold))))

   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,green))))
   `(font-lock-doc-string-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,clear-blue))))
   `(font-lock-keyword-face ((,class (:foreground ,purple-code))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,red))))
   `(font-lock-variable-name-face ((,class (:foreground ,foreground))))
   `(font-lock-warning-face ((,class (:foreground ,orange))))

   `(hl-line ((,class (:background ,current-line))))

   `(linum ((,class (:background ,current-line :foreground ,foreground))))

   `(org-date ((,class (:foreground ,purple))))
   `(org-hide ((,class (:foreground ,current-line))))
   `(org-link ((,class (:foreground ,blue))))
   `(org-done ((,class (:bold t :weight bold :foreground ,green
                              :box (:line-width 1 :style none)))))
   `(org-todo ((,class (:bold t :foreground ,red :weight bold
                              :box (:line-width 1 :style none)))))
   `(org-checkbox ((,class (:foreground ,green))))
   `(org-level-1 ((,class (:foreground ,purple))))
   `(org-level-2 ((,class (:foreground ,orange))))
   `(org-level-3 ((,class (:foreground ,yellow))))
   `(org-special-keyword ((,class (:foreground ,purple))))
   `(org-block-begin-line ((,class (:underline nil :foreground ,comment :background ,current-line))))
   `(org-block-background ((,class (:background ,fringe-color))))
   `(org-block-end-line ((,class (:overline nil :foreground ,comment :background ,current-line))))

   `(region ((,class (:background ,purple))))

   `(show-paren-match ((,class (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch ((,class (:background ,orange :foreground ,current-line))))

   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,purple))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,aqua))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,red))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,comment))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,foreground))))

   `(mode-line ((,class (:foreground ,foreground :background nil))))
   `(mode-line-inactive ((,class (:inherit mode-line :background ,fringe-color :foreground ,foreground :box (:line-width -1 :color ,selection) :weight light))))
   `(header-line ((,class (:inherit mode-line :background ,fringe-color :foreground ,foreground :box nil))))

   `(powerline-active0 ((,class (:inherit mode-line))))
   `(powerline-active1 ((,class (:inherit mode-line :background ,fringe-color :foreground ,foreground))))
   `(powerline-active2 ((,class (:inherit mode-line :background ,current-line :foreground ,purple))))
   `(powerline-inactive1 ((,class (:inherit mode-line-inactive :background ,fringe-color))))
   `(powerline-inactive2 ((,class (:inherit mode-line-inactive :background ,background))))

   `(dashboard-text-banner ((,class (:foreground ,purple))))
   `(dashboard-heading ((,class (:foreground ,purple)))))

  (custom-theme-set-variables
   'yukimacs

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])))

(provide-theme 'yukimacs)

;;; yukimacs-theme.el ends here
