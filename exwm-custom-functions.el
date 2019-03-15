;;; -*- lexical-binding: t; -*-

(cl-defun exwm-input-send-single-key (key-as-string)
  "This function takes a single key, as represented by a string, and sends it 
via exwm-input--fake-key. Do note, it cannot handle something like C-x x, as 
that generates two numbers, instead of one, when sent through kbd and 
string-to-list. if you want to send C-x C-v, you would write that as two calls 
to this function"
  (unless (stringp key-as-string)
    (message (format "Key provided in unknown format, aborting"))
    (cl-return-from exwm-input-send-single-key))
  (let ((key-as-number (car (string-to-list (kbd key-as-string)))))
    (exwm-input--fake-key key-as-number)))

(defun send-c-l ()
  (interactive)
  (exwm-input-send-single-key "C-l"))
(defun send-c-c ()
  (interactive)
  (exwm-input-send-single-key "C-c"))

(defun get-url-from-firefox ()
  (interactive)
  (when (string= (car (split-string (buffer-name) "<")) "Firefox")
    (send-c-l)
    ;; (run-with-timer 0.1 nil #'send-c-c)
    (exwm-input-send-single-key "M-w")
    (switch-to-buffer "*scratch*")
    (yank)))

(defun tt-get-headings ()
  "2019-01-14"
  (interactive)
  (let (($headings nil))
    (org-map-entries
     (lambda ()
       (push (org-heading-components) $headings)))
    (with-output-to-temp-buffer "*xah temp out*"
      (print (nreverse $headings)))))

(defun org-mode-prompt-for-headings ()
  (interactive)
  (let (org-headings
	org-heading-names)
    (org-map-entries
     (lambda ()
       (push (org-heading-components) org-headings)))
    (setq org-heading-names (mapcar (lambda (x)
				      (car (cddddr x)))
				    org-headings))
    (let ((choice (completing-read "Select org heading: "
				   org-heading-names)))
      (message "%S" choice)
      (outline-show-all) ;; expand the entire org document, so we can work with it. 
      (re-search-forward choice)
      ;; (next-line)
      (move-end-of-line 1)
      ;; (org-meta-return)
      ;; (yank)
      ;; (save-buffer)
      )))

(defun message-current-buffer-after-switch ()
  (interactive)
  (let ((prev-buf (current-buffer)))
    (switch-to-buffer "*scratch*")
    (message (buffer-name prev-buf))))

;; ((1 1 nil nil "Programs, Programming, Systems, and Syntax" nil)
;;  (2 2 nil nil "LaTeX" nil)
;;  (1 1 nil nil "test header" nil)
;;  (2 2 nil nil "test subheader" nil)
;;  (2 2 nil nil "test subheader 2" nil))
;; the above is sample output from tt-get-headings.




