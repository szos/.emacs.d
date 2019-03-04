;; Starting Emacs config from scratch.
(package-initialize)

;; (load "~/.emacs.d/exwm.el")
(load "~/.emacs.d/stumpbuffer.el")

;; get rid of the fucking beeps. 					;
(setq visible-bell t)

;; (global-visual-line-mode t) ;; wraps words
(setq-default truncate-lines t)

;;; set columns:
(setq column-number-mode t)
;;; add package sources:
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "httl://elpa.gnu.org/packages/")))
(unless (assoc-default "melpa-stable" package-archives)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages")))

;;; set up use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;;; set up backups location:
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; set up minor mode for EE (thunderbird external editor)
(setq load-path (append load-path '("~/.emacs.d/packages-from-source/")))
(require 'tbemail)

;; set up for haskell:
(require 'haskell-mode)
(require 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)
;(require 'flycheck)
;(setq flycheck-check-syntax-automatically '(save new-line))
;(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

;;; make company faster... 
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(global-company-mode t)

;;; set up emacs-application-framework, which lets us embed programs in emacs,
;;; and use them from emacs in an emacsy way.
(setq load-path (append load-path '("~/.emacs.d/emacs-application-framework/")))
(require 'eaf)
(setq eaf-http-proxy-host "127.0.0.1")
(setq eaf-http-proxy-port "1080")

;; set up matrix client
;; (setq load-path (append load-path '("~/.emacs.d/packages-from-source/matrix-client-el/")))
;; (require 'matrix-client)

;;; set up elfeed and rss
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed/elfeed.org")))

(use-package elfeed
  :ensure t)

;; disable the tool bar and menu bar
(tool-bar-mode -1)
;; (menu-bar-mode -1)

(defun enable-notes ()
  (interactive)
  (end-of-buffer)
  (delete-other-windows)
  (menu-bar-mode -1))

;;; set up paredit hooks

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)

(defun paredit-enable-define ()
  (enable-paredit-mode)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-k") 'kill-region))

(add-hook 'emacs-lisp-mode-hook #'paredit-enable-define)
(add-hook 'lisp-mode-hook #'paredit-enable-define)

;;; set up rainbow delimiters

(require 'rainbow-delimiters)
(require 'cl-lib)
(require 'color)
(cl-loop for index from 1 to rainbow-delimiters-max-face-count
	 do (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
	      (cl-callf color-saturate-name (face-foreground face) 30)))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)

;;; set up slime and inferior lisp
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;; set up guile
(defun start-guile ()
  (interactive)
  (run-geiser)
  (geiser-set-scheme 'guile))

;;; set up chicken scheme
;;(setq scheme-program-name "csi -:c")
;; (require 'quack)
;; (require 'cmuscheme)

;; (define-key scheme-mode-map "\C-c\C-l" 'scheme-load-current-file)
;; (define-key scheme-mode-map "\C-c\C-k" 'scheme-compile-current-file)

;; (defun scheme-load-current-file (&optional switch)
;;   (interactive "P")
;;   (let ((file-name (buffer-file-name)))
;;     (comint-check-source file-name)
;;     (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
;; 					 (file-name-nondirectory file-name)))
;;     (comint-send-string (scheme-proc)
;; 			(concat "(load \"" file-name "\"\)\n"))
;;     (if switch
;;       (switch-to-scheme t)
;;       (message "\"%s\" loaded." file-name))))

;; (defun scheme-compile-current-file (&optional switch)
;;   (interactive "P")
;;   (let ((file-name (buffer-file-name)))
;;     (comint-check-source file-name)
;;     (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
;; 					 (file-name-nondirectory file-name)))
;;     (message "compiling \"%s\" ..." file-name)
;;     (comint-send-string (scheme-proc) (concat "(compile-file \""
;; 					      file-name
;; 					      "\"\)\n"))
;;     (if switch
;;       (switch-to-scheme t)
;;       (message "\"%s\" compiled and loaded." file-name))))

;;; Bind Keys:
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "M-)") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "M-(") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "M-SPC") 'god-mode)
;; (global-set-key (kbd "M-k") 'paredit-kill-region)




;;set up a custom lisp editing mode. 

;;;;; End of File. dont edit below. 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(package-selected-packages (quote (paredit helm exwm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; set up theming
(load-theme 'sanityinc-tomorrow-bright)

;;; set cursor color
(set-cursor-color "#d12323")

;;; set highlighting.
(set-face-attribute 'region nil :background "#af0101")
(set-face-attribute 'region nil :foreground "#fff")

(put 'upcase-region 'disabled nil)

