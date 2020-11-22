;;; org-timer-alerts.el --- Automatiic org timers for upcoming events -*- lexical-binding: t; -*-

(require 'alert)
(require 'ts)
(require 'org-ql)

(defcustom org-timer-alert-final-alert-string
  "IT IS  %alert-time\n\n%todo %headline"
  "String for the final alert message, which which can use the following substitutions:
%todo         : the TODO state of the the heading, if any
%headline     : the headline text of the heading
%alert-time   : the time of the event
%warning-time : the number of minutes before the event the warning will be shown
%current-time : the time the alert is sent to the user
%category     : the category property of the org heading, or the name of the file if none")

(defcustom org-timer-alert-warning-string
  "%todo %headline\n at %alert-time\n it is now %current-time\n *THIS IS YOUR %warning-time MINUTE WARNING*"
  "String for alert warning messages, which can use the following substitutions:
%todo         : the TODO state of the the heading, if any
%headline     : the headline text of the heading
%alert-time   : the time of the event
%warning-time : the number of minutes before the event the warning will be shown
%current-time : the time the alert is sent to the user
%category     : the category property of the org heading, or the name of the file if none")

(defcustom org-timer-alerts-files (org-agenda-files)
  "List of org files used to check for events.")

(defcustom org-timer-alerts-default-alert-props
  '(:icon "")
  "Plist used for default properties for alert messages.
Accepts any properties used by `alert':
 :title 
 :icon 
 :category
 :buffer 
 :mode
 :data
 :style
 :persistent
 :never-persist
 :id")

(defcustom org-timer-alerts-default-warning-times '(-10 -5 -2)
  "List of minutes before an event when a warning will be sent.")

(defvar org-timer-alerts--timer-list nil
  "Internal list of timer objects.")

(defun org-timer-alert--string-substitute (string map)
  "MAP is an alist in the form of '((\"%placeholder\" . replacement))
STRING is the original string. Replace all %placeholders with their 
replacement values and return a new string."
  (cl-loop for (holder . replacement) in map
	   when replacement
	   do (setq string (replace-regexp-in-string
			    (concat "%" (pcase holder
					  ((pred symbolp) (symbol-name holder))
					  ((pred stringp) holder)))
			    (pcase replacement
			      ((pred stringp) replacement)
			      ((pred numberp) (number-to-string replacement))
			      ((pred symbolp) (symbol-name replacement))
			      ((pred null) "")
			      (_ ""))
			    string))
	   finally return string))

(defun org-timer-alerts--parser ()
  ":action for `org-ql-select'"
  (-let* (((&alist "ITEM" headline
		   "TIMESTAMP" timestamp
		   "DEADLINE" deadline
		   "SCHEDULED" scheduled
		   "TODO" todo
		   "CATEGORY" category)
	   (org-entry-properties)))
    (cl-loop for time in (list timestamp deadline scheduled)
	     do (when time
		  (setq time (ts-parse-org time))
		  ;; `ts' returns 0 for hour and minute even
		  ;; if a timestamp does not have an
		  ;; associated time. We'll assume that if the
		  ;; time is midnight, there is no time of day
		  ;; in the timestamp and ignore it. 
		  (when (and (not (= 0 (ts-hour time)))
			     (not (= 0 (ts-minute time)))
			     (ts> time (ts-now)))
		    ;; Add warning timers
		    (cl-loop for warning-time
			     in
			     org-timer-alerts-default-warning-times
			     do
			     (let ((replacements
				    `((todo . ,(or todo ""))
				      (headline . ,(or headline ""))
				      (current-time .
						    ,(->> time
							  (ts-adjust
							   'minute
							   warning-time)
							  (ts-format "%H:%M")))
				      (alert-time . ,(ts-format "%H:%M" time))
				      (warning-time . ,(abs warning-time))
				      (category . ,category))))
			       ;;(when (ts> (ts-parse alert-time) (ts-now))
			       (org-timer-alerts--add-timer
				(alist-get 'current-time replacements)
				(org-timer-alert--string-substitute
				 org-timer-alert-warning-string
				 replacements)
				:title (or category "ALERT"))))
		    ;; Add final, actual, notification at time of event. 
		    (let ((replacements
			   `((todo . ,(or todo ""))
			     (headline . ,(or headline ""))
			     (current-time . ,(ts-format "%H:%M" time))
			     (alert-time . ,(ts-format "%H:%M" time))
			     (warning-time . 0)
			     (category . ,category))))
		      (org-timer-alerts--add-timer
		       (alist-get 'current-time replacements)
		       (org-timer-alert--string-substitute
			org-timer-alert-final-alert-string
			replacements)
		       :title (or category "ALERT"))))))))

(defun org-timer-alerts--get-default-val (prop)
  "Get the default value of PROP from `org-timer-alerts-default-alert-props'."
  (plist-get org-timer-alerts-default-alert-props
	     prop))

(defun org-timer-alerts--add-timer (time message &optional &key
					 title icon category buffer mode
					 severity data style persistent
					 never-persist id)
  "Create timers via `run-at-time' and add to `org-timer-alerts--timer-list'"
  (push (run-at-time time
		     nil
		     #'alert
		     message
		     :title (or title
				(org-timer-alerts--get-default-val :title))
		     ;; :app-icon (or icon
		     ;; 		   (org-timer-alerts--get-default-val :icon))
		     :category (or category
				   (org-timer-alerts--get-default-val :category))
		     :buffer (or buffer
				 (org-timer-alerts--get-default-val :buffer))
		     :mode (or mode
			       (org-timer-alerts--get-default-val :mode))
		     :data (or data
			       (org-timer-alerts--get-default-val :data))
		     :style (or style
				(org-timer-alerts--get-default-val :style))
		     :severity (or severity
				   (org-timer-alerts--get-default-val :severity))
		     :persistent (or persistent
				     (org-timer-alerts--get-default-val :persistent))
		     :never-persist (or never-persist
					(org-timer-alerts--get-default-val :never-persist))
		     :id (or id
			     (org-timer-alerts--get-default-val :id)))
	org-timer-alerts--timer-list))

(defun org-timer-alerts-set-all-timers ()
  "Run `org-ql' query to get all headings with today's timestamp."
  (interactive)
  ;; Reset existing timers
  (org-timer-alerts-cancel-all-timers)
  ;; Clear the `org-ql' cache
  (setq org-ql-cache (make-hash-table :weakness 'key))
  ;; Add timers
  (org-ql-select org-timer-alerts-files
    '(ts-active :on today)
    :action #'org-timer-alerts--parser)
  (message "Org-timer-alerts: Alert timers updated."))

(defun org-timer-alerts-cancel-all-timers ()
  "Cancel all the timers."
  (interactive)
  (cl-loop for timer in org-timer-alerts--timer-list
	   do (cancel-timer timer))
  (setq org-timer-alerts--timer-list nil))

(define-minor-mode org-timer-alerts-mode
  "Alert before an event."
  nil
  nil
  nil
  (if org-timer-alerts-mode
      (progn 
	(org-timer-alerts-set-all-timers)
	(add-hook 'org-agenda-mode-hook #'org-timer-alerts-set-all-timers))
    (org-timer-alerts-cancel-all-timers)
    (remove-hook ' org-agenda-mode-hook #'org-timer-alerts-set-all-timers)))

(provide 'org-timer-alerts)

