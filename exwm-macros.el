;;; -*- lexical-binding: t; -*-

;; (defmacro defcommand (name args &rest body)
;;   `(defun ,name ,args
;;      (interactive)
;;      ,@body))

(defmacro defuni (name args &rest body)
  `(defun ,name ,args
     (interactive)
     ,@body))

(defmacro my/case (test control &rest body)
  "this macro takes a test ;;(an equality function, like string-equal), a control
;;(somthing to test against) and then a list of cases of the form:
;;(test-against-me (form1) (form2)... (formn))
it tests control against test-against-me using test, and runs everything within the 
succeeding case within a progn. heres an example "
  ;; (my/case #'string= "hellp"
  ;; 	   ("hello" (message "hi")
  ;; 	    (print "hello"))
  ;; 	   ("goodbye" (message "byebye")
  ;; 	    (print "goodbye"))
  ;; 	   ("hellp" (message "spelling error")
  ;; 	    (print "hello or help")))
  ;; => "hello or help"
  (when body
    `(if (funcall ,test ,control ,(caar body))
	 (progn ,@(cdar body))
       (my/case ,test ,control ,@(cdr body)))))

(defun make-bookmark-firefox ()
  ;; this function is working. or at least, it was when i tested it just now...
  "this macro should do the following, in order:
1. check if the current buffer is firefox, chrome, brave, icecat, etc. point is 
it must be a browser.
2. select the url
3. copy the url
4. prompt the user for what category to add the url to. if there are 
sub-categories, prompt for those as well. These categories are the org headers 
5. create a new entry in the unordered list that comprises each category.
6. paste the url into that entry.
7. save the org file. "
  (interactive)
  (when (string= (car (split-string (buffer-name) "<")) "Firefox") ;; when were in firefox
    (exwm-input-send-single-key "C-l") ;; select the url
    (exwm-input-send-single-key "C-c") ;; copy the url
    (find-file "~/.emacs.d/web-bookmarks.org")
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
	(goto-char (point-min))
	(outline-show-all)
	(re-search-forward choice)
	(next-line)
	(move-end-of-line 1)
	(org-meta-return)
	(yank)
	(save-buffer)
	(goto-char (point-min))
	;;(kill-buffer "web-bookmarks.org")
	))))

(defun make-bookmark-firefox ()
  (interactive)
  (when (string= (car (split-string (buffer-name) "<")) "Firefox") ;; when were in firefox
    (exwm-input-send-single-key "C-l") ;; select the url
    (exwm-input-send-single-key "C-c") ;; copy the url
    (let ((prev-buffer (current-buffer))
	  (org-headings nil)
	  (org-heading-names nil)
	  (close-buffer? nil))
      (if (bufferp (get-buffer "web-bookmarks.org")) ;; if the buffer exists
	  (switch-to-buffer "web-bookmarks.org") ;; go to it
	(progn ;; otherwise, 
	  (setq close-buffer? t) ;; mark that we want to close it when were done. 
	  (find-file "~/.emacs.d/web-bookmarks.org"))) ;; then find the file.
      (org-map-entries
       (lambda ()
	 (push (org-heading-components) org-headings)))
      (setq org-heading-names (mapcar (lambda (x)
					(concat "* " (car (cddddr x))))
				      org-headings))
      (let ((choice (completing-read "Select org heading: "
				     org-heading-names)))
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
	  (switch-to-buffer prev-buffer))))))

(defuni my/bname ()
  (message (buffer-name)))

(defmacro run-shell-command (command &optional name buffer)
  "this takes a shell command and runs it via start-process-shell-command. this is useful 
for launching applications. it can function the same as start-process-shell-command, with the
arguments mixed around, but was written so you can effectively shell out with a one liner, 
like so: "
  ;;(run-shell-command "xterm -e setxkbmap no")
  ;; becomes
  ;; (start-process-shell-command
  ;;  "xterm_-e_setxkbmap_no" nil "xterm -e setxkbmap no")
  `(start-process-shell-command ,(if name name
				   (replace-regexp-in-string ;(regexp-quote " ")
				     " " "_" command))
				,buffer ,command))

(defmacro quotedp (thing)
  "this is a little test macro, designed to check if an argument is quoted. 
i use this for various bits and bobs, like my iset macro, but because the check 
HAS to be done at compile time (ie at macro time, not after expansion) im not sure
how to use this for other things..."
  (if (and (consp thing)
	   (eq (car thing) 'quote))
      't
    'nil))

(defmacro reset-hook (hook function)
  "this macro takes an (unquoted) hook variable, sets it to nil,
and then adds the function to it. this could use quotedp... i think... 
WARNING: only use on hooks with ONE(1) function, in order to 
replace that function!"
  (if (and (consp hook)
	   (eq (car hook) 'quote))
      `(progn
	 (set ,hook nil)
	 (add-hook ,hook ,function))
    `(progn
       (setq ,hook nil)
       (add-hook ',hook ,function))))

(defmacro reset-hook/test (hook function)
  "this macro takes an (unquoted) hook variable, sets it to nil,
and then adds the function to it. 
WARNING: only use on hooks with ONE(1) function, in order to 
replace that function!"
  `(progn
     (iset ,hook nil)
     ,(if (and (consp hook)
	       (eq (car hook) 'quote))
	  `(add-hook ,hook ,function)
	`(add-hook ',hook ,function))))

(defmacro iset (thing value)
  "ISET stands for Inteligent SET, which differentiates between set and setq
based on whether the thing is quoted or not. this works with ONE(1) thing/value
pair, unlike setq, which can accept many. "
  (if (and (consp thing)
	   (eq (car thing) 'quote))
      `(set ,thing ,value)
    `(setq ,thing ,value)))

(defmacro iset/test (thing value &rest plist-for-set)
  (when (and thing value)
    (if (and (consp thing)
	     (eq (car thing) 'quote))
	`(progn (set ,thing ,value)
		(iset/test ,(car plist-for-set) ,(cadr plist-for-set)
			   ,@(cddr plist-for-set)))
      `(setq ,thing ,value
	     ,@plist-of-things-to-set))))

;; (iset/test 'var1 val1 'var2 val2 'var3 val3)


;; (defmacro iset/testing (thing value &rest set-pairs)
;;   "ISET stands for Inteligent SET, which differentiates between set and setq
;; based on whether the thing is quoted or not. this works with ONE(1) thing/value
;; pair, unlike setq, which can accept many. "
;;   (if (and (consp thing)
;; 	   (eq (car thing) 'quote))
;;       `(let ((x set-pairs))
;; 	 (while x
;; 	   (set (car x) (cadr x))
;; 	   (setq x (cddr x))))

;;       `(set ,thing ,value)
;;     `(setq ,thing ,value)))

;; (reset-hook exwm-manage-finish-hook
;; 	    (lambda ()
;; 	      (when (and exwm-class-name
;; 			 (string= exwm-class-name "XTerm"))
;; 		(exwm-input-set-local-simulation-keys nil))
;; 	      (when (and exwm-class-name
;; 			 (string= exwm-class-name "Firefox"))
;; 		(exwm-input-set-local-simulation-keys
;; 		 (append '(([?\M-F] . [C-next])
;; 			   ([?\M-B] . [C-prior])
;; 			   ([?\C-o] . [?\C-w]))
;; 			 exwm-input-simulation-keys)
;; 		 ;; (cons '([?\M-F] . [C-next])
;; 		 ;; 	     (cons '([?\M-B] . [C-prior])
;; 		 ;; 		   (cons '([?\C-o] . [?\C-w]) exwm-input-simulation-keys)))
;; 		 ))
;; 	      (when (and exwm-class-name
;; 			 (string= exwm-class-name "W3M"))
;; 		(exwm-input-set-local-simulation-keys nil))
;; 	      (when (and exwm-class-name
;; 			 (string= exwm-class-name "qutebrowser"))
;; 		(exwm-input-set-local-simulation-keys nil))))
