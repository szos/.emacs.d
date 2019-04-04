;;; -*- lexical-binding: t; -*-
;;; this init is for EXWM. 

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

;; (unless (assoc-default "melpa" package-archives)
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "httl://elpa.gnu.org/packages/")))
(unless (assoc-default "melpa-stable" package-archives)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/")))
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; (package-refresh-contents)

(setq column-number-mode t)
(setq visible-bell t)

(require 'use-package)

(require 'exwm)
;; (require 'exwm-config)
(require 'exwm-systemtray)
(require 'symon)
(require 'desktop-environment)
(require 'helm)
(require 'helm-config)
;; set up latex stuff, auctex is called tex, for some reason. 
(require 'tex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;;(setq TeX-)
;; end latex setup.
(require 'org)
;; (desktop-environment-mode)
;; (exwm-config-default)

(exwm-systemtray-enable)
(setq exwm-systemtray-height 24)
(window-divider-mode 1)
(display-battery-mode 1)
(display-time-mode 1)
;; (symon-mode) ;; this shows system info in the minibuffer
(menu-bar-mode -1) ;; get rid of the extra clutter up top. 
(tool-bar-mode -1) ;; get rid of the extra clutter up top.

;; set helm up
(setq helm-split-window-in-side-p t
      helm-echo-input-in-header-line t
      helm-autoresize-mode t)

(helm-mode 1)

;; (setq company-dabbrev-downcase 0)
;; (setq company-idle-delay 0)
(setq company-idle-delay .2)
(global-company-mode t)

;; make windmove wraparound
(setq windmove-wrap-around t)

;; make the background black.
;;(add-to-list 'default-frame-alist '(background-color . "black")
;; (setq default-frame-alist '((right-divider-width . 6)
;; 			    (buffer-predicate . exwm-layout--other-buffer-predicate)
;; 			    ;; (background-color . "black")
;; 			    ))

;;; set up hungry delete. 
(unless (fboundp 'hungry-delete-mode)
  (package-install 'hungry-delete))

(require 'hungry-delete)
(global-hungry-delete-mode)
;;; end hungry delete setup. 

;; load themes, set up colors. 
(load-theme 'sanityinc-tomorrow-bright)
;; set up modeline
(set-face-background 'mode-line "tomato")
(set-face-foreground 'mode-line "black")

(set-face-foreground 'mode-line-buffer-id "dark blue")
;; set unused modelines
(set-face-background 'mode-line-inactive "dim grey")
(set-face-foreground 'mode-line-inactive "black")
(setq mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification " " mode-line-position (vc-mode vc-mode) "" mode-line-modes mode-line-misc-info mode-line-end-spaces))

;;; set up lisp ;;;

;; set up paredit
(defun paredit-enable-define ()
  (enable-paredit-mode)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-k") 'kill-region))

(add-hook 'emacs-lisp-mode-hook #'paredit-enable-define)
(add-hook 'lisp-mode-hook #'paredit-enable-define)

(setq backward-delete-char-untabify-method 'all) ; this makes paredit work kinda like hungry-delete.

;; end paredit
;; set up parinfer (for testing, for now)
(use-package parinfer
  :ensure t
  :bind (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
	  '(defaults pretty-parens paredit))))

;;; rainbow delimiters for programming mode and slime repl.

(require 'rainbow-delimiters)
(require 'cl-lib)
(require 'color)
(cl-loop for index from 1 to rainbow-delimiters-max-face-count
	 do (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
	      (cl-callf color-saturate-name (face-foreground face) 30)))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)

;;; set up slime to use sbcl
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions, Commands, and Setup for Emacs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indent-buffer ()
  "this indents the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defvar *capitalize-previous-word-insert-space-control-characters*
;;  '(#\, #\. #\? #\! #\@ #\% #\$ #\')
  '(?\, ?\. ?\? ?\! ?\@ ?\% ?\$ ?\'))

(defun writing/capitalize-previous-word ()
  "This capitalizes the previous word, similar to capitalize-word but 
prepending a 'M-b' to it. "
  (interactive)
  (save-excursion
    (backward-word)
    (capitalize-word 1)))

(defun writing/capitalize-sentence ()
  (interactive)
  (save-excursion
    (while (not (char-equal (char-after) ?.))
      (backward-char))
    (forward-char)
    (if (char-equal (char-after) ?\s)
	(forward-char)
      (insert " "))
    (capitalize-word 1)))

;; set up org mode
;; enable line wrapping for org mode documents only
(defun smart-switch-line-wraps-for-org-mode ()
  (unless (string= (buffer-name) "web-bookmarks.org")
    (visual-line-mode)))

(add-hook 'org-mode-hook #'smart-switch-line-wraps-for-org-mode)

;; make it so that ^ and _ dont do super/sub scripting, instead requiring
;; ^{thing} or _{thing}
(setq org-use-sub-superscripts '{})
;; End org mode setup

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (exwm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions, Commands, and Setup for EXWM ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load "~/.emacs.d/exwm-test.el")

(load "~/.emacs.d/exwm-macros.el")

(load "~/.emacs.d/exwm-commands.el")

(defvar *battery-popup-timer*)
;; set up a timer, which checks our battery and throws up a buffer with a
;; warning if its below 20% and discharging
(setq *battery-popup-timer* (run-at-time "60 sec" 180 #'low-battery-popup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Functions, Commands, and Setup ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'exwm-update-class-hook
	  (lambda ()
	    (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
			(string= "gimp" exwm-instance-name))
	      (exwm-workspace-rename-buffer exwm-class-name))))

(add-hook 'exwm-update-title-hook
	  (lambda ()
	    (when (or (not exwm-instance-name)
		      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
		      (string= "gimp" exwm-instance-name))
	      (exwm-workspace-rename-buffer exwm-title))))

(add-hook 'exwm-manage-finish-hook
	  (lambda ()
	    (when (and exwm-class-name
		       (string= exwm-class-name "XTerm"))
	      (exwm-input-set-local-simulation-keys nil))
	    (when (and exwm-class-name
		       (string= exwm-class-name "Firefox"))
	      (exwm-input-set-local-simulation-keys
	       (append `(;;(,(kbd "C-x u") . ,(kbd "C-S-t")) ;; find a way to bind C-x u to C-T
			 ([?\M-F] . [C-next]) ([?\M-B] . [C-prior])
			 ([?\C-o] . [?\C-w])
			 (,(kbd "C-M-b") . ,(kbd "C-["))
			 (,(kbd "C-M-f") . ,(kbd "C-]")))
		       exwm-input-simulation-keys)))
	    (when (and exwm-class-name
		       (string= exwm-class-name "W3M"))
	      (exwm-input-set-local-simulation-keys nil))
	    (when (and exwm-class-name
		       (string= exwm-class-name "qutebrowser"))
	      (exwm-input-set-local-simulation-keys nil))))

;; (setq exwm-manage-finish-hook nil)

;; beyond lies the bindings. 

(setq exwm-input-global-keys
      `((,(kbd "s-r") . exwm-reset)
	(,(kbd "s-w") . exwm-workplace-switch)
	(,(kbd "s-&") . (lambda (command)
			  (interactive (list (read-shell-command "$ ")))
			  (start-process-shell-command command nil command)))
	(,(kbd "s-;") . (lambda (command)
			  (interactive (list (read-shell-command "$ ")))
			  (start-process-shell-command command nil command)))
	(,(kbd "s-ø") . (lambda (command)
			  (interactive (list (read-shell-command "$ ")))
			  (start-process-shell-command command nil command)))
	(,(kbd "s-o") . other-window)
	(,(kbd "M-o") . other-window)
	;; (,(kbd "s-b") . helm-buffers-list)
	(,(kbd "s-<f1>") . volume-mute)
	(,(kbd "s-<f2>") . desktop-environment-volume-decrement)
	(,(kbd "s-<f3>") . desktop-environment-volume-increment)
	;; (,(kbd "s-k") . )
	(,(kbd "s-p") . windmove-up)
	(,(kbd "s-n") . windmove-down)
	(,(kbd "s-b") . windmove-left)
	(,(kbd "s-f") . windmove-right)))


;;; THE DIFFERENCES BETWEEN EXWM-INPUT-SET-KEY AND EXWM-INPUT-GLOBAL-KEYS:
;;; exwm-input-global-keys are available in both char and line mode, while
;;; exwm-input-set-key definitions are only available in line mode. 
(exwm-input-set-key (kbd "<f1>") 'volume-mute)
(exwm-input-set-key (kbd "<f2>") 'desktop-environment-volume-decrement)
(exwm-input-set-key (kbd "<f3>") 'desktop-environment-volume-increment)
(exwm-input-set-key (kbd "<f11>") 'dec-brightness)
(exwm-input-set-key (kbd "<f12>") 'inc-brightness)

;; (require 'exwm-commands)

;; here lies regular keys
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "s-x") 'helm-M-x)
(global-set-key (kbd "C-c m") 'magit-status)
(define-key exwm-mode-map (kbd "C-q") 'exwm-input-send-key)
(define-key org-mode-map (kbd "C-c u") 'writing/capitalize-sentence)
(define-key org-mode-map (kbd "C-M-f") 'org-next-visible-heading)
(define-key org-mode-map (kbd "C-M-b") 'org-previous-visible-heading)
;;(define-key org-mode-map (kbd "C-c s") 'capitalize-previous-word)

;; send-raw-key is exwm-input-send-next-key

(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
      	([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end C-c delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))


;; ;; (define-key exwm-mode-map (kbd "M-o") 'other-window)

;; ;; set up keyboard
;; (setup)

(exwm-enable)
