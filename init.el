;;; init.el

;; We up the gc threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t)
  (message "Native complation is *not* available, lsp performance will suffer..."))

(unless (functionp 'json-serialize)
  (message "Native JSON is *not* available, lsp performance will suffer..."))

;; Increase the amount of data which Emacs reads from the process. 
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Do not steal focus while doing async compilations.
(setq warning-suppress-types '((comp)))

(setq comp-deferred-compilation t)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; Setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package using straight.el
(straight-use-package 'use-package)

;; Makes :straight t by default
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)
(setq straight-fix-flycheck t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; GCMH - the Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :custom
  (gcmh-idle-delay 1000)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ;; 16 MB
  :hook (after-init . gcmh-mode))

(setq-default shell-file-name "/bin/sh")

(straight-use-package 'org)

;; Add local packages
(add-to-list 'load-path "~/.emacs.d/local")

;; Loads config
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(yukimacs))
 '(custom-safe-themes
   '("af4b5aff75a5d012079df291d327ed6e25ca5b17cf8b6c2ee7be0145f8c9fb52" default))
 '(org-agenda-files '("agenda.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "UKWN" :family "Iosevka Comfy"))))
 '(fixed-pitch ((t (:family "Iosevka Comfy"))))
 '(variable-pitch ((t (:family "Iosevka Comfy")))))

;;; init.el ends here
