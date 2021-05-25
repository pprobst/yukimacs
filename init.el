;;; This fixed garbage collection makes emacs start up faster ;;;;;;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
		 '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t))
(setq load-prefer-newer t)

(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

(unless (package-installed-p 'doom-themes)
  (package-refresh-contents)
  (package-install 'doom-themes))

;; Loads config
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#0F0F0F" :foreground "#D2D5D6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "PfEd" :family "Fantasque Sans Mono"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#171717" :foreground "#d2d5d6" :box (:line-width -1 :color "#323333") :weight light))))
 '(powerline-active0 ((t (:inherit mode-line))))
 '(powerline-active1 ((t (:inherit mode-line :background "#171717" :foreground "#d2d5d6"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#1c1c1c" :foreground "#b294bb"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "#171717"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#0f0f0f")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(yukimacs))
 '(custom-safe-themes
   '("b1ae23f6c2231b5399b95ca36464bd2e28b6f5a6732b4b55303a540d23041808" "33089924f91318911a777743210edb5461239f889719a3c2945a8c3e0698f9f9" "5584ff597d113ad3a7b5436fc5e93d50913ccb2aa6e320277c4cfd1099fe3d6a" "74d7bb968c4871b912f2b593aae014dfa1816cb9b5e9684d1b9aaa042bb709ea" "fdb064a1ce18f7ea3fe4a6557cff22b56ed4aac0d1224cf0f86ad6e16608fb7d" "b6dee747f8df825a77abd81cc9137b929bee4b1cc68c7b264e2ca78d919c7480" "4dcdc05130ba3997b8e5454fba575b8289a3a2b6f7ed21e3cbd226e749dd3053" "094871996e1db187cdc033d110d4c3c3706bfe8606f096b49b3abe19f3b90351" "72fd431e390371b3356bba43207b7cc1558e05a656026ef285e3903be505d05e" "866f8a1b2b44f4cda2eb3ef780d5805458b3381dd03eba5bca556628f0ea1aa4" "4e50d339e3ade28467b4af4a23610faac89e4da69f81d16d35c0f9e2782f81d6" "3e852a63fc1e74e58f3c07c7af55869e3db719d98edc54d994871bd14ac99d9b" "ebf8505f75085fb7d14e6fc5aa67dd0089d0f4f87e794270426bb920fc859387" "ae72a428bb005757ce94d267517ecd553ea99912acd88988854b68844e658fb4" "bf6c3bd026f7a0a1371a36074f812bab06ce6d6cf7b28379f0e41ac97fc6d2bf" "64f64a2d78e9f928c0a969b68b824bc08dc19e8e6f5c5f0fb7fbc38bf7c9569a" "5fe4b6050434ae6620a0eb977add13d0c54614af4de4b539ff464242918d09a9" default))
 '(package-selected-packages
   '(treemacs-all-the-icons all-the-icons-dired all-the-icons evil-nerd-commenter org-bullets ccls no-littering ctrlf orderless popup-kill-ring helpful company-lsp lsp-python-ms flycheck-rust rustic powerline treemacs-magit magit slime-company slime company-jedi company-irony company-c-headers flycheck-clang-analyzer dap-mode auto-yasnippet yasnippet-snippets lsp-ui yasnippet company dashboard treemacs-evil treemacs rainbow-delimiters rainbow-mode switch-window avy ido-vertical-mode auto-package-update beacon evil which-key diminish htmlize doom-themes spacemacs-theme use-package)))
