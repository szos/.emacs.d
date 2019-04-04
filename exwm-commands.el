;;; -*- lexical-binding: t; -*-

;; define exwm commands and functions.

(defun setup ()
  ;; (start-process-shell-command "term" nil "xterm -e setxkbmap no && xmodmap /home/shos/.emacs.d/modmaps/eng-no-swap-super-altgrn.modmap")
  (start-process-shell-command "term" nil "xterm -e setxkbmap no && xmodmap /home/shos/.emacs.d/modmaps/eng-no-test.modmap")
  (start-process-shell-command "nm-applet" nil "nm-applet")
  (start-process-shell-command "polkit" nil "lxqt-policykit-agent"))

(defun browserp (buffer-name-as-string)
  (let ((name (car (split-string buffer-name-as-string "<"))))
    (cond ((string= name "Firefox")
	   t)
	  ;; ((string= name "Brave-browser")
	  ;;  t)
	  ;; ((string= name "qutebrowser")
	  ;;  t)
	  (t
	   nil))))

(defvar *encrypting-gpg-identity* "")

(defun save-and-encrypt ()
  "this saves a file, encrypts it via gpg, and then deletes the original
and its backups ;(filename.txt~, #filename.txt#)"
  (interactive)
  (save-buffer)
  (let* ((file-path (buffer-file-name))
	 (enc-file-name (format "%s.gpg" file-path))
	 (encrypted-f (shell-command-to-string
		       (format "gpg --output %s -r %s --encrypt %s"
			       enc-file-name *encrypting-gpg-identity*
			       file-path))))
    (delete-file file-path)
    (if (file-exists-p (format "%s~" file-path))
	(delete-file (format "%s~" file-path)))
    (if (file-exists-p (format "#%s#" file-path))
	(delete-file (format "#%s#" file-path)))))

(defun make-bookmark-firefox ()
  "this function makes new entries in an org file, thus storing bookmarks under
various categories. it prompts the user for the category, and keeps the urls in 
an unordered list. This has only been tested with firefox, but should work with 
other browsers that respect the keybindings of C-l to select the url, and C-c to 
copy the url. "
  (interactive)
  (when (browserp (buffer-name)) ;; when were in a browser buffer. 
    (exwm-input-send-single-key "C-l")	; select the url
    (exwm-input-send-single-key "C-c")	; copy the url
    (run-with-timer
     0.25 nil ; we need to run this in a timer because otherwise it ends
     (lambda ()
       (let ((prev-buffer (current-buffer)) ; up running this before its 
	     (org-headings nil) ; copied the URL from firefox, and we just
	     (org-heading-names nil) ; paste whatevers on the kill ring. 
	     (close-buffer? nil))
	 (if (bufferp (get-buffer "web-bookmarks.org")) ;; if the buffer exists
	     (switch-to-buffer "web-bookmarks.org")	;; go to it
	   (progn					;; otherwise, 
	     (setq close-buffer? t) ;; mark that we want to close it when were done. 
	     (find-file "~/.emacs.d/web-bookmarks.org"))) ;; then find the file.
	 (org-map-entries
	  (lambda ()
	    (push (org-heading-components) org-headings)))
	 (setq org-heading-names (mapcar (lambda (x)
					   (concat "* " (car (cddddr x))))
					 org-headings))
	 (let ((choice (completing-read "Select org heading: "
					(nreverse org-heading-names))))
	   (message "%S" choice)
	   (goto-char (point-min))
	   (outline-show-all)
	   (re-search-forward choice)
	   (next-line)
	   (move-end-of-line 1)
	   (org-meta-return)
	   (yank)
	   (save-buffer)
	   (goto-char (point-min))
	   (if close-buffer?
	       (kill-buffer "web-bookmarks.org")
	     (switch-to-buffer prev-buffer))))))))

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

(defun exwm-show-float/test ()
  (interactive)
  (save-excursion
    (let ((buffer (helm-buffers-list)))
      (when (equal (buffer-local-value 'major-mode buffer) 'exwm-mode)
	(exwm-layout--show (exwm--buffer->id buffer))
	(exwm-floating--start-moveresize (exwm--buffer->id buffer))
	(exwm-floating--stop-moveresize)))))

(defun exwm/switch-buffer-including-floats ()
  "this function depends on helm... and EXWM of course."
  (interactive)
  (save-excursion
    (let ((buffer (helm-buffers-list)))
      (when (equal (buffer-local-value 'major-mode buffer) 'exwm-mode)
	(exwm-layout--show (exwm--buffer->id buffer))
	(exwm-floating--start-moveresize (exwm--buffer->id buffer))
	(exwm-floating--stop-moveresize)))))

(defuni teseter ()
  (exwm-layout--show (exwm--buffer->id (current-buffer)))
  (exwm-floating--start-moveresize (exwm--buffer->id (current-buffer)))
  (exwm-floating--stop-moveresize))

;; (defun teseter ()
;;   (interactive)
;;   (let ((buffer (helm :sources '(helm-source-buffers-list)
;; 		      :buffer "*show-floats-test*"
;; 		      :keymap helm-buffer-map
;; 		      :truncate-lines helm-buffers-truncate-lines)))
;;     (if (equal (buffer-local-value 'major-mode buffer) 'exwm-mode)
;; 	(message "exwm-mode")
;;       (message "not exwm-mode"))))

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
  (run-shell-command "/opt/balenaEtcher/balena-etcher-electron"))

(defun tmux ()
  (interactive)
  (run-shell-command "xterm -class tmux -e tmux new-session -A -s Main"))
