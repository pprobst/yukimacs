(deftheme yukimacs "2021-05-19")
;; based on tomorrow night

(let ((class '((class color) (min-colors 89)))
      (background "#0F0F0F")
      (fringe-color "#171717")
      (current-line "#1A1A1A")
      (selection "#323333")
      (foreground "#D2D5D6")
      (comment "#A5A7A8")
      (cursor "#B9BCBD")
      (red "#CC6666")
      (more-red "#E23C42")
      (orange "#de935f")
      (yellow "#f0c674")
      (green "#6A9B88")
      (more-green "#6A9B76")
      (hl-green "#46806D")
      (aqua "#6A8F9B")
      (blue "#5083A9")
      (clear-blue "#6D9AC5")
      (purple "#b294bb"))

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
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,purple))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,red))))
   `(font-lock-variable-name-face ((,class (:foreground ,foreground))))
   `(font-lock-warning-face ((,class (:foreground ,red))))

   `(hl-line ((,class (:background ,current-line))))

   `(linum ((,class (:background ,current-line :foreground ,foreground))))

   `(org-date ((,class (:foreground ,purple))))
   `(org-done ((,class (:foreground ,green))))
   `(org-hide ((,class (:foreground ,current-line))))
   `(org-link ((,class (:foreground ,blue))))
   `(org-todo ((,class (:foreground ,red))))
   `(org-code ((,class (:foreground ,aqua))))
   `(org-checkbox ((,class (:foreground ,green))))
   `(org-level-1 ((,class (:foreground ,red))))
   `(org-level-2 ((,class (:foreground ,orange))))
   `(org-level-3 ((,class (:foreground ,yellow))))
   `(org-special-keyword ((,class (:foreground ,purple))))

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
   `(powerline-active0 ((,class (:inherit mode-line))))
   `(powerline-active1 ((,class (:inherit mode-line :background ,fringe-color :foreground ,foreground))))
   `(powerline-active2 ((,class (:inherit mode-line :background "#1c1c1c" :foreground ,purple))))
   `(powerline-inactive1 ((,class (:inherit mode-line-inactive :background ,fringe-color))))
   `(powerline-inactive2 ((,class (:inherit mode-line-inactive :background "#0f0f0f"))))

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
