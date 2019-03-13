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

;; load themes, set up colors. 
(load-theme 'sanityinc-tomorrow-bright)
;; set up modeline
(set-face-background 'mode-line "tomato")
(set-face-foreground 'mode-line "black")

(set-face-foreground 'mode-line-buffer-id "dark blue")
;; set unused modelines
(set-face-background 'mode-line-inactive "dim grey")
(set-face-foreground 'mode-line-inactive "black")

;;; set up lisp ;;;
(defun paredit-enable-define ()
  (enable-paredit-mode)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-k") 'kill-region))

(add-hook 'emacs-lisp-mode-hook #'paredit-enable-define)
(add-hook 'lisp-mode-hook #'paredit-enable-define)

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

(defmacro my/case (test control &rest body)
  "this isnt working right now.... its busting the stack. "
  (when body
    `(if (funcall ,test ,control ,(caar body))
	 (progn ,@(cdar body))
       (my/case ,test ,control ,@(cdr body)))))

(defmacro run-shell-command (command &optional name buffer)
  `(start-process-shell-command ,(if name name command) ,buffer ,command))

(defun volume-mute ()
  (interactive)
  (let* ((amix (split-string 
		(shell-command-to-string
		 "amixer set Master toggle | grep -o -P '(?<=%\\] \\[).*(?=\\])'")
		"\n"))
	 (status (car amix)))
    (cond ((string= status "off")
	   (message "volume muted"))
	  ((string= status "on")
	   (message "volume unmuted"))
	  (t
	   (message "unknown status, check volume from terminal")))))

(defun inc-brightness ()
  (interactive)
  (run-shell-command "xbacklight -inc 5")
  (let ((num (round
	      (string-to-number (shell-command-to-string "xbacklight -get")))))
    (message (format "Brightness: %d" num))))

(defun dec-brightness ()
  (interactive)
  (run-shell-command "xbacklight -dec 5")
  (let ((num (round
	      (string-to-number (shell-command-to-string "xbacklight -get")))))
    (message (format "Brightness: %d" num))))

(defun low-battery-popup ()
  (let* ((string-list (split-string (shell-command-to-string "acpi") " "))
	 (status (caddr string-list))
	 (percentage (string-to-number (car (split-string (cadddr string-list) "%")))))
    (when (and (< percentage 20) (string-equal status "Discharging,"))
      (with-output-to-temp-buffer "Low Battery Warning"
    	(prin1 "Battery is below 20%, plug in your computer. ")
	(terpri)
    	(prin1 "This message will redisplay every 60 seconds. ")
	(terpri)
	(prin1 (format "Battery Level:  %d" percentage))
	(terpri) (terpri)
	(prin1 "This message is brought to you by low-battery-popup")))
    (message "Battery Check")))

(defvar *battery-popup-timer*)
(setq *battery-popup-timer* (run-at-time "60 sec" 60 #'low-battery-popup))

(defun setup ()
  (start-process-shell-command "term" nil "xterm -e setxkbmap no && xmodmap /home/shos/.emacs.d/modmaps/eng-no-swap-super-altgrn.modmap")
  (start-process-shell-command "nm-applet" nil "nm-applet"))

(defun  firefox ()
  (interactive)
  (start-process-shell-command "firefox" nil "firejail firefox -P EXWM"))

(defun newsboat ()
  (interactive)
  (run-shell-command "xterm -class Newsboat -e newsboat"))

(defun pulse-audio ()
  (interactive)
  (run-shell-command "pavucontrol"))

;; (defun tor ()
;;   (run-shell-command "./TOR/Browser/start-tor-browser"))

(defun w3m ()
  (interactive)
  (run-shell-command "xterm -class W3M -e w3m duckduckgo.com"))

(defun mail ()
  (interactive)
  (run-shell-command "thunderbird"))

(defun riot ()
  (interactive)
  (run-shell-command "riot-desktop"))

(defun bitwarden ()
  (interactive)
  (run-shell-command "bitwarden-bin"))

(defun etcher ()
  (interactive)
  (run-shell-command "/opt/Etcher/etcher-electron"))

(defun tmux ()
  (interactive)
  (run-shell-command "xterm -class tmux -e tmux new-session -A -s Main"))

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

(defmacro reset-hook (hook function)
  "this macro takes an (unquoted) hook variable, sets it to nil,
and then adds the function to it. "
  `(progn
     (setq ,hook nil)
     (add-hook ',hook ,function)))

(add-hook 'exwm-manage-finish-hook
	  (lambda ()
	    (when (and exwm-class-name
		       (string= exwm-class-name "XTerm"))
	      (exwm-input-set-local-simulation-keys nil))
	    (when (and exwm-class-name
		       (string= exwm-class-name "Firefox"))
	      (exwm-input-set-local-simulation-keys
	       (cons '([?\M-F] . [C-next])
		     (cons '([?\M-B] . [C-prior])
			   (cons '([?\C-q] . [?\C-w]) exwm-input-simulation-keys)))))
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
	(,(kbd "s-o") . other-window)
	(,(kbd "M-o") . other-window)
	;; (,(kbd "s-b") . helm-buffers-list)
	(,(kbd "s-<f1>") . volume-mute)
	(,(kbd "s-<f2>") . desktop-environment-volume-decrement)
	(,(kbd "s-<f3>") . desktop-environment-volume-increment)
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
(global-set-key (kbd "C-<prior>") 'send-raw)

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
        ([?\C-k] . [S-end delete])
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
