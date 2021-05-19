(deftheme yukimacs "2021-05-19")
;; based on tomorrow night

(let ((background "#0F0F0F")
      (fringe-color "#171717")
      (current-line "#1A1A1A")
      (selection "#323333")
      (foreground "#D2D5D6")
      (comment "#A5A7A8")
      (cursor "#B9BCBD")
      (red "#CC6666")
      (orange "#de935f")
      (yellow "#f0c674")
      (green "#6A9B88")
      (aqua "#6A8F9B")
      (blue "#5083A9")
      (purple "#b294bb"))

  (custom-theme-set-faces
   'yukimacs

   `(default ((t (:background ,background :foreground ,foreground))))
   `(fringe ((t (:background ,fringe-color))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(mode-line ((t (:background ,current-line :foreground ,foreground))))
   `(region ((t (:background ,selection))))

   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,green))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,red))))
   `(font-lock-warning-face ((t (:foreground ,red))))

   `(hl-line ((t (:background ,current-line))))

   `(linum ((t (:background ,current-line :foreground ,foreground))))

   `(org-date ((t (:foreground ,purple))))
   `(org-done ((t (:foreground ,green))))
   `(org-hide ((t (:foreground ,current-line))))
   `(org-link ((t (:foreground ,blue))))
   `(org-todo ((t (:foreground ,red))))
   `(org-code ((t (:foreground ,aqua))))

   `(show-paren-match ((t (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch ((t (:background ,orange :foreground ,current-line))))

   `(rainbow-delimiters-depth-1-face ((t (:foreground ,purple))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aqua))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,comment))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,foreground)))))

   `(dashboard-text-banner ((t (:foreground ,red))))
   `(dashboard-heading ((t (:foreground ,red))))

  (custom-theme-set-variables
   'yukimacs

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])))

(provide-theme 'yukimacs)
