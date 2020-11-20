;;; org-timed-alerts.el --- Automatiic org timers for upcoming events -*- lexical-binding: t; -*-

(use-package alert
  :custom
  ((alert-fade-time 60)
   (alert-default-icon nil)))

(setq org-timed-alerts--debugging-timer-list nil)

(defcustom org-timed-alerts-files (org-agenda-files)
  "List of org files used to check for events.")

(defcustom org-timed-alerts-default-alert-props nil
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

(setq org-timed-alerts-default-warning-times '(-10 -5 -2))
;; "List of minutes before the event when a warning will be sent.")

(defvar org-timed-alerts--timer-list nil
  "Internal list of timer objects.")

(defun org-timed-alerts--parser ()
  "Runs at each org heading and returns a plist of 
relevant properties to be inserted into the calendar buffer."
  (-let* (((&alist "ITEM" headline
		   "TIMESTAMP" timestamp
		   "DEADLINE" deadline
		   "SCHEDULED" scheduled
		   "TODO" todo
		   "CATEGORY" category 
		   "ID" id)
	   (org-entry-properties)))
    (unless id (setq id (org-id-get-create)))
    (cl-loop for time in (list timestamp deadline scheduled)
	     do (when time
		  (setq time (ts-parse-org time))
		  (when (not (= 0 (ts-hour time)))
		    ;; Add the warning timers
		    (cl-loop for warning in org-timed-alerts-default-warning-times
			     do (org-timed-alerts--add-timer
				 (->> time
				      (ts-adjust 'minute warning)
				      (ts-format "%H:%M"))
				 (concat (or todo "")
					 " " (or headline "[Blank headline]")
					 "\n At " (ts-format "%H:%M" time)
					 "\nTHIS IS YOUR "
					 (number-to-string warning)
					 " MINUTE WARNING")
				 :title (or category "ALERT")))
		    (org-timed-alerts--add-timer
		     (->> time
			  (ts-format "%H:%M"))
		     (concat (or category "")  ": " (or todo "")
			     " " (or headline "[Blank headline]")
			     "\n At " (ts-format "%H:%M" time)
			     "\n " (make-string 500 ? ))
		     :title (or category "ALERT")))))))

(defun org-timed-alerts--add-timer (time message &optional &key
					 title icon category buffer mode
					 severity data style persistent
					 never-persist id)
  (cl-flet ((get-default (prop)
			 (plist-get org-timed-alerts-default-alert-props
				    prop)))
    (push (run-at-time time
		       nil
		       #'alert
		       message 
		       :title (or title (get-default :title))
		       :icon (or icon (get-default :icon))
		       :category (or category (get-default :category))
		       :buffer (or buffer (get-default :buffer))
		       :mode (or mode (get-default :mode))
		       :data (or data (get-default :data))
		       :style (or style (get-default :style))
		       :severity (or severity (get-default :severity)
				     :persistent (or persistent (get-default :persistent))
				     :never-persist (or never (get-default :never-persist))
				     :id (or id (get-default :id))))
	  org-timed-alerts--timer-list)))

(setq org-timed-alerts-files '("~/.emacs.d/lisp/org-pretty-outline/test.org"))

(defun org-timed-alerts--set-all-timers ()
  (setq org-ql-cache (make-hash-table :weakness 'key))
  (org-ql-select org-timed-alerts-files
    '(ts-active :on today)
    :action #'org-timed-alerts--parser))

(defun org-timed-alerts--cancel-all-times ()
  (cl-loop for timer in org-timed-alerts--timer-list
	   do (cancel-timer timer)))

(define-minor-mode org-timed-alerts-mode
  "Alert before an event."
  nil
  " alert"
  nil
  (if org-timed-alerts-mode
      (progn
	(setq org-timed-alerts--debugging-timer-list nil)
	(org-timed-alerts--set-all-timers))
    (org-timed-alerts--cancel-all-times)))

