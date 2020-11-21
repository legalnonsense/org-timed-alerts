;;; org-timer-alerts.el --- Automatiic org timers for upcoming events -*- lexical-binding: t; -*-

(require 'alert)
(require 'ts)
(require 'org-ql)

(defcustom org-timer-alerts-files (org-agenda-files)
  "List of org files used to check for events.")

(defcustom org-timer-alerts-default-alert-props
  '(:icon alert-default-icon)

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
  "List of minutes before the event when a warning will be sent.")

(defvar org-timer-alerts--timer-list nil
  "Internal list of timer objects.")

(defun org-timer-alerts--parser ()
  ":action for `org-ql-select'"
  (-let* (((&alist "ITEM" headline
		   "TIMESTAMP" timestamp
		   "DEADLINE" deadline
		   "SCHEDULED" scheduled
		   "TODO" todo
		   "CATEGORY" category 
		   "ID" id)
	   (org-entry-properties)))
    (cl-loop for time in (list timestamp deadline scheduled)
	     do (when time
		  (setq time (ts-parse-org time))
		  ;; `ts' returns 0 for hour and minute even
		  ;; if the org timestamp does not have an
		  ;; associated time. We'll assume that if the
		  ;; hour is midnight, there is no time of date
		  ;; in the timestamp and ignore it. 
		  (when (and (not (= 0 (ts-hour time)))
			     (ts> time (ts-now)))
		    ;; Add warning timers
		    (cl-loop for warning in org-timer-alerts-default-warning-times
			     do (org-timer-alerts--add-timer
				 (->> time
				      (ts-adjust 'minute warning)
				      (ts-format "%H:%M"))
				 (concat (or todo "")
					 " " (or headline "[Blank headline]")
					 "\n At " (ts-format "%H:%M" time)
					 "\nTHIS IS YOUR "
					 (number-to-string (abs warning))
					 " MINUTE WARNING")
				 :title (or category "ALERT")))
		    ;; Add final, actual, notification at time of event. 
		    (org-timer-alerts--add-timer
		     (->> time
			  (ts-format "%H:%M"))
		     (concat (or category "")  ": " (or todo "")
			     " " (or headline "[Blank headline]")
			     "\n At " (ts-format "%H:%M" time)
			     "\n " (make-string 500 ? ))
		     :title (or category "ALERT")))))))

(defun org-timer-alerts--add-timer (time message &optional &key
					 title icon category buffer mode
					 severity data style persistent
					 never-persist id)
  "Create timers via `run-at-time' and add to `org-timer-alerts--timer-list'"
  (cl-flet ((get-default (prop)
			 (plist-get org-timer-alerts-default-alert-props
				    prop)))
    (push (run-at-time time
		       nil
		       #'alert-libnotify-notify
		       (list 
			:title (or title (get-default :title))
			:message message 
			:app-icon (or icon (get-default :icon))
			:category (or category (get-default :category))
			:buffer (or buffer (get-default :buffer))
			:mode (or mode (get-default :mode))
			:data (or data (get-default :data))
			:style (or style (get-default :style))
			:severity (or severity (get-default :severity))
			:persistent (or persistent (get-default :persistent))
			:never-persist (or never (get-default :never-persist))
			:id (or id (get-default :id))))
	  org-timer-alerts--timer-list)))

(defun org-timer-alerts--set-all-timers ()
  "Run `org-ql' query to get all headings with today's timestamp."
  (setq org-timer-alerts--timer-list nil)
  ;; Clear the `org-ql' cache
  (setq org-ql-cache (make-hash-table :weakness 'key))
  (org-ql-select org-timer-alerts-files
    '(ts-active :on today)
    :action #'org-timer-alerts--parser))

(defun org-timer-alerts--cancel-all-timers ()
  "Cancel all the timers."
  (cl-loop for timer in org-timer-alerts--timer-list
	   do (cancel-timer timer))
  (setq org-timer-alerts--timer-list nil))

(define-minor-mode org-timer-alerts-mode
  "Alert before an event."
  nil
  nil
  nil
  (if org-timer-alerts-mode
      (org-timer-alerts--set-all-timers))
  (org-timer-alerts--cancel-all-times))

(provide 'org-timer-alerts)

