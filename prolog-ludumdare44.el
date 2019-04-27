(global-set-key "\C-c\C-k\C-vLD" 'ld44-quick-start)

(global-set-key "\C-cldk" 'ld44-kill)
(global-set-key "\C-cldr" 'ld44-restart)
(global-set-key "\C-cldRc" 'ld44-restart-client)
(global-set-key "\C-cldRs" 'ld44-restart-server)
(global-set-key "\C-cldws" 'ld44-set-windows)

(defun ld44-quick-start ()
 ""
 (interactive)
 (ld44-start))


;; START start/restart/kill

(defun ld44-start ()
 ""
 (interactive)
 (if (non-nil ld44-dir)
  (progn
   (ld44-start-client)
   (ld44-start-server)
   )))

(defun ld44-start-client ()
 ""
 (interactive)
 (run-in-shell (concat "cd " (shell-quote-argument (concat ld44-dir "scripts")) " && ./start-larkc-client.sh") "*LD44-REPL*"))

(defun ld44-start-server ()
 ""
 (interactive)
 (run-in-shell (concat "cd " (shell-quote-argument (concat ld44-dir "scripts")) " && ./start-larkc-server.sh") "*LD44*"))

(defun ld44-kill ()
 ""
 (interactive)
 (ld44-kill-client)
 (ld44-kill-server))

(defun ld44-kill-client ()
 ""
 (interactive)
 (shell-command "(killall -9 start-larkc-client.sh &)")
 (kmax-kill-buffer-no-ask "*LD44-REPL*"))

(defun ld44-kill-server ()
 ""
 (interactive)
 (shell-command "(killall -9 start-larkc-server.sh &)")
 (kmax-kill-buffer-no-ask "*LD44*"))

(defun ld44-restart-client ()
 ""
 (interactive)
 (ld44-kill-client)
 (ld44-start-client))

(defun ld44-restart-server ()
 ""
 (interactive)
 (ld44-kill-server)
 (ld44-start-server))

(defun ld44-restart ()
 ""
 (interactive)
 (ld44-restart-client)
 (ld44-restart-server)
 (ld44-set-windows))

;; END start/restart/kill


(defun ld44-set-windows ()
 ""
 (interactive)
 (delete-other-windows)
 (switch-to-buffer "*LD44-REPL*")
 (split-window-below)
 (other-window 1)
 (switch-to-buffer "*LD44*"))

(defun non-nil (arg)
 (if (symbolp arg)
  (and (boundp arg)
   (not (equal arg nil)))
  t))

(cond
 ((string= system-name "ai.frdcsa.org") (defvar ld44-dir "/var/lib/myfrdcsa/codebases/minor/prolog-ludumdare44/"))
 ((string= system-name "li1167-61.members.linode.com") (defvar ld44-dir "/home/aindilis/prolog-ludumdare44/"))
 (t nil))

(defun run-in-shell (command &optional name optional-message mode)
 ""
 (let (
       (mybuffer
	(progn
	 ;; (split-window-right)
	 (get-buffer-create 
	  (or name
	   (generate-new-buffer-name "*shell*")))))
       )
  (message command)
  (shell mybuffer)
  (if mode
   (kmax-funcall mode nil))
  ;; (switch-to-buffer mybuffer)
  (radar-shell-do-command command optional-message)
  ))

(defun radar-shell-do-command (command &optional optional-message delay)
 ""
 (progn
  (end-of-buffer)
  
  ;; FIXME: have to do this somehow without making it a command that
  ;; is then executed

  ;; (if optional-message
  ;;  (insert (concat optional-message "\n"))
  ;; )

  (insert command)
  (sit-for (or delay 2))
  (ignore-errors
   (eshell-send-input))
  (ignore-errors
   (comint-send-input))
  )
 )

(defun kmax-funcall (function args &optional else finally error-msg)
 ""
 ;; e.g. (kmax-funcall 'concat (list "a" "b"))
 (interactive)
 ;; (kmax-fixme-silent "implement else finally etc")
 (condition-case ex
  (progn
   (eval (append (list 'funcall 'function) args))
   )   
  ('error
   (see (or error-msg (concat "Function <" function "> did not work with args <" (join " " args) ">"))))))

(defun see (data &optional duration)
 ""
 (interactive)
 (message (prin1-to-string data))
 (sit-for (if duration duration 2.0))
 data)

(defun kmax-kill-buffer-no-ask (buffer)
 ""
 (save-excursion
  (pop-to-buffer buffer)
  (let ((process (get-buffer-process buffer)))
   (if (non-nil process)
    (set-process-query-on-exit-flag process nil)))
  (kill-this-buffer)))
