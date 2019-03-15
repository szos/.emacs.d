;;; -*- lexical-binding: t; -*-

;; define exwm commands and functions.

(defun setup ()
  ;; (start-process-shell-command "term" nil "xterm -e setxkbmap no && xmodmap /home/shos/.emacs.d/modmaps/eng-no-swap-super-altgrn.modmap")
  (start-process-shell-command "term" nil "xterm -e setxkbmap no && xmodmap /home/shos/.emacs.d/modmaps/eng-no-test.modmap")
  (start-process-shell-command "nm-applet" nil "nm-applet"))

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

(defun  firefox ()
  (interactive)
  (start-process-shell-command "firefox" nil "firejail firefox -P EXWM"))

(defun newsboat ()
  (interactive)
  (run-shell-command "xterm -class Newsboat -e newsboat"))

(defun pulse-audio ()
  (interactive)
  (run-shell-command "pavucontrol"))

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
