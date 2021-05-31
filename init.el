;;; init.el

;;; Better startup/performance.
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t)
  (message "Native complation is *not* available, lsp performance will suffer..."))

(unless (functionp 'json-serialize)
  (message "Native JSON is *not* available, lsp performance will suffer..."))

;; Do not steal focus while doing async compilations
(setq warning-suppress-types '((comp)))

(setq comp-deferred-compilation t)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 10000000
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

(unless (package-installed-p 'modus-themes)
  (package-refresh-contents)
  (package-install 'modus-themes))

(setq-default shell-file-name "/bin/sh")

;; Loads config
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#0F0F0F" :foreground "#D2D5D6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "BE5N" :family "Iosevka Comfy")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(yukimacs))
 '(custom-safe-themes
   '("b99b27cb80bddd0944447da9d33393aa40c6ed6c13c9c60acd2f15bad52ed22e" default))
 '(package-selected-packages
   '(toc-org cmake-mode guess-language hl-todo highlight-numbers company-reftex company-math company-auctex auctex marginalia lsp-haskell haskell-mode evil-org org-superstar treemacs-projectile evil-collection treemacs-all-the-icons all-the-icons-dired all-the-icons evil-nerd-commenter ccls no-littering ctrlf orderless popup-kill-ring helpful company-lsp lsp-python-ms flycheck-rust rustic powerline treemacs-magit magit slime-company slime company-jedi company-irony company-c-headers flycheck-clang-analyzer dap-mode auto-yasnippet yasnippet-snippets lsp-ui yasnippet company dashboard treemacs-evil treemacs rainbow-delimiters rainbow-mode switch-window avy ido-vertical-mode auto-package-update beacon evil which-key diminish htmlize doom-themes spacemacs-theme use-package)))

;;; init.el ends here
