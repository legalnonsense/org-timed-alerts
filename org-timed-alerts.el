;;; org-timed-alerts.el --- Automatiic org timers for upcoming events -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/org-timed-alerts
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (org "9.0") (s "1.12.0")
;;                    (ts "0.2") (org-ql "0.5-pre") (dash "2.16.0"))
;; Keywords: Org, agenda, calendar, alert

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Receive alerts for events (i.e., active timestamps, deadlines, schedules) which
;; have an associated time of day timestamp. Alerts are sent via `alert'. Timers
;; are updated every time you load your agenda. 

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;; Installation

;; Install these required packages:

;; + org-ql
;; + ts.el
;; + alert
;; + dash.el

;; Then put this file in your load-path, and put this in your init
;; file. See the README for a use-package declaration. 

;;     (require 'org-timed-alerts)

;;     Then: 
;;     (org-timed-alerts-mode)
;;     Or, add a hook:
;;     (add-hook 'org-mode-hook #'org-timed-alerts-mode)

;;;; Usage

;; Type M-x org-timed-alerts-mode RET to enable the package. 
;; 
;; To update all timers, open your org-agenda or type:
;; M-x org-timed-alerts-set-all-timers RET

;;;; Tips

;; You can customize settings in the `org-timed-alerts' group.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'alert)
(require 'dash)
(require 'ts)
(require 'org-ql)

;;;; Customization

(defgroup org-timed-alerts nil
  "org-timed-alerts options"
  :tag " org-timed-alerts"
  :group 'org
  :group 'org-timed-alerts
  :prefix "org-timed-alerts-")

(defcustom org-timed-alerts-files nil
  "If nil, use (org-agenda-files). Otherwise, specify a file or list 
of files to search for events."
  :type 'list
  :group 'org-timed-alerts)

(defcustom org-timed-alerts-alert-function #'alert
  "Alert function. Default is #'alert. See `alert' for more possibilities."
  :type 'function
  :group 'org-timed-alerts)

(defcustom org-timed-alerts-final-alert-string
  "IT IS %alert-time\n\nTIME FOR:\n%todo %headline"
  "String for the final alert message, which which can use the following substitutions:
%todo         : the TODO state of the the heading, if any
%headline     : the headline text of the heading
%category     : the category property of the org heading, or the name of the file if none
%alert-time   : the time of the event
%warning-time : the number of minutes before the event the warning will be shown
%current-time : the time the alert is sent to the user"
  :type 'string
  :group 'org-timed-alerts)

(defcustom org-timed-alerts-warning-string
  "%todo %headline\n at %alert-time\n it is now %current-time\n *THIS IS YOUR %warning-time MINUTE WARNING*"
  "String for alert warning messages, which can use the following substitutions:
%todo         : the TODO state of the the heading, if any
%headline     : the headline text of the heading
%category     : the category property of the org heading, or the name of the file if none
%alert-time   : the time of the event
%warning-time : the number of minutes before the event the warning will be shown
%current-time : the time the alert is sent to the user"
  :type 'string
  :group 'org-timed-alerts)

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
 :id
The value of each key should be whatever value is acceptable
to `alert'.  Alternatively, the value may be a function with 
no arguments which runs at each org heading and returns the 
appropriate val.

For example to set the title of each alert
to the root heading, you could use:
  '(:title (lambda ()
	     (save-excursion
	       (while (org-up-heading-safe))
	       (org-get-heading t t t t)))

Which moves up to the root header and sets the value of 
:title to that headline."
  :type '(plist :key-type symbol :value-type sexp)
  :group 'org-timed-alerts)

(defcustom org-timed-alerts-agenda-hook-p t
  "Update all alerts whenever you generate an agenda?"
  :type 'boolean
  :group 'org-timed-alerts)

(defcustom org-timed-alerts-warning-times '(-10 -5)
  "List of minutes before an event when a warning will be sent.
There is no difference between positive and negative values,
i.e., -10 and 10 both mean to send an alert 10 minutes before
the event."
  :type '(list integer)
  :group 'org-timed-alerts)

;;;; Variables

(defvar org-timed-alerts--timer-list nil
  "Internal list of timer objects.")

;;;; Org-ql predicate

(org-ql--defpred ts-repeat ()
  "Find entries with timestamp repeats"
  :body (org-get-repeat))

;;;; Functions

(defun org-timed-alerts--string-substitute (string map marker)
  "MAP is an alist in the form of '((PLACEHOLDER . REPLACEMENT))
STRING is the original string. PLACEHOLDER is a symbol or a string that will
be converted to a string prefixed with a %: \"%PLACEHOLDER\". 
REPLACEMENT can be a string, a number, symbol, or function. Replace all
occurrences of %placeholder with replacement and return a new string."
  (cl-loop for (holder . replacement) in map
	   when replacement
	   do (setq string
		    (replace-regexp-in-string
		     (concat "%"
			     (pcase holder
			       ((pred symbolp) (symbol-name holder))
			       ((pred stringp) holder)
			       ((pred numberp) (number-to-string holder))))
		     (pcase replacement
		       ((pred stringp) replacement)
		       ((pred numberp) (number-to-string replacement))
		       ((pred functionp) (org-timed-alerts--run-func-at-point
					  replacement marker))
		       (_ ""))
		     string))
	   finally return string))

(defun org-timed-alerts--run-func-at-point (func marker)
  "Call FUNC with point at MARKER."  
  (with-current-buffer (marker-buffer marker)
    (save-excursion (goto-char (marker-position marker))
		    (funcall func))))

(defun org-timed-alerts--get-default-prop (prop marker)
  "Get val for PROP from `org-timed-alerts-default-alert-props'.
If val is a function, call it with point at MARKER;
otherwise, return val."
  (let ((val (plist-get
	      org-timed-alerts-default-alert-props
	      prop)))
    (if (functionp val)
	(org-timed-alerts--run-func-at-point val marker)
      val)))

(defun org-timed-alerts--org-ql-action ()
  "Parsing function to be run as the `org-ql' :action.
Adds a marker to `org-entry-properties' and returns 
an alist."
  (append (org-entry-properties)
	  `(("MARKER" . ,(copy-marker
			  (org-element-property
			   :begin
			   (org-element-at-point)))))))

(defun org-timed-alerts--has-time-of-day-p (timestamp)
  "Does TIMESTAMP contain a time of day specification?
TIMESTAMP is string in the form of an org timestamp."
  (when timestamp
    (string-match "[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}.*>" timestamp)))

(defun org-timed-alerts--update-repeated-event (timestamp-string)
  "If TIMESTAMP-STRING has a repeat, update according to the 
repeat interval to show the next occurrence and return a
an TS object the new date."
  (when-let* ((repeat (org-get-repeat timestamp-string))
	      (amount (string-to-number
		       (if (= (length repeat) 4)
			   (substring repeat 1 -1)
			 (substring repeat 0 -1))))
	      (unit (pcase (substring repeat -1)
		      ("w" (prog1 'day
			     (setq amount (* 7 amount))))
		      ("h" 'hour)
		      ("m" 'month)
		      ("d" 'day)
		      ("y" 'year)))
	      (timestamp (ts-parse-org timestamp-string)))
    (while (ts< timestamp (ts-now))
      (setq timestamp (ts-adjust unit amount timestamp)))
    timestamp))

(defun org-timed-alerts--parser (entry)
  "Process data from `org-ql' query and create
timers by calling `org-timed-alerts--add-timer'."
  (-let (((&alist "ITEM" headline
		  "TIMESTAMP" timestamp
		  "DEADLINE" deadline
		  "SCHEDULED" scheduled
		  "TODO" todo
		  "CATEGORY" category
		  "MARKER" marker
		  "ORG-TIMED-ALERTS" custom-alert-intervals)
	  entry))
    (when custom-alert-intervals
      (setq custom-alert-intervals
	    (mapcar #'string-to-number (split-string custom-alert-intervals))))
    (cl-loop
     for time in (list timestamp deadline scheduled)
     when (and time (org-timed-alerts--has-time-of-day-p time))
     do
     ;; If the timestamp repeats, updated it and convert to ts,
     ;; otherwise, just convert it.
     (if (org-get-repeat time)
	 (setq time (org-timed-alerts--update-repeated-event time))
       (setq time (ts-parse-org time)))
     ;; Make sure the timestamp is between now and tomorrow 
     (when (and (ts> time (ts-now))
		(ts< time (ts-adjust 'day 1 (ts-now))))
       (cl-loop
	with current-time = nil
	;; Make sure there are no duplicates in the warning
	;; intervals.
	for warning-time in (-distinct (-snoc
					(or custom-alert-intervals
					    org-timed-alerts-warning-times)
					;; 0 means send an alert at the
					;; time of the event
					0))
	do
	(setq current-time (ts-adjust 'minute (* -1 (abs warning-time)) time))
	(when (ts> current-time (ts-now))
	  (setq current-time (ts-format "%H:%M" current-time))
	  (org-timed-alerts--add-timer
	   (ts-adjust 'minute (* -1 (abs warning-time)) time)
	   ;; Create the message string
	   (org-timed-alerts--string-substitute
	    (if (= warning-time 0)
		org-timed-alerts-final-alert-string
	      org-timed-alerts-warning-string)
	    `((todo . ,(or todo ""))
	      (headline . ,headline)
	      (current-time . ,current-time)
	      (alert-time . ,(ts-format "%H:%M" time))
	      (warning-time . ,(abs warning-time))
	      (category . ,category))
	    marker)
	   marker
	   :title (or (org-timed-alerts--get-default-prop
		       :title marker)
		      category))))))))

(defun org-timed-alerts--add-timer (time message marker &optional &key
					 title icon category buffer mode
					 severity data style persistent
					 never-persist id)
  "Create timers via `run-at-time' and add them to 
     `org-timed-alerts--timer-list'.  TIME is the time to run the alert. 
     MESSAGE is the alert body. Optional keys are those accepted by `alert'."
  (push (run-at-time
	 ;; `run-at-time' only accepts times associated with the
	 ;; current day.  Ohterwise, we have to convert the
	 ;; future time to seconds. 
	 (ts-difference time (ts-now))
	 nil
	 org-timed-alerts-alert-function
	 message
	 :title
	 (or title (org-timed-alerts--get-default-prop :title marker))
	 :icon
	 (or icon (org-timed-alerts--get-default-prop :icon marker))
	 :category
	 (or category (org-timed-alerts--get-default-prop :category marker))
	 :buffer
	 (or buffer (org-timed-alerts--get-default-prop :buffer marker))
	 :mode
	 (or mode (org-timed-alerts--get-default-prop :mode marker))
	 :data
	 (or data (org-timed-alerts--get-default-prop :data marker))
	 :style
	 (or style (org-timed-alerts--get-default-prop :style marker))
	 :severity
	 (or severity (org-timed-alerts--get-default-prop :severity marker))
	 :persistent
	 (or persistent (org-timed-alerts--get-default-prop :persistent marker))
	 :never-persist
	 (or never-persist (org-timed-alerts--get-default-prop :never-persist marker))
	 :id (or id (org-timed-alerts--get-default-prop :id marker)))
	org-timed-alerts--timer-list))

;;;; Commands

(defun org-timed-alerts-list-timers ()
  "Print list of active timers to the message buffer."
  (interactive)
  (message 
   (cl-loop for timer in org-timed-alerts--timer-list
	    for x from 1 to (length org-timed-alerts--timer-list)
	    concat (concat "Timer #"
			   (number-to-string x)
			   "; set for: "
     (let ((time
	    (decode-time
	     (timer--time
	      timer))))
       (concat
	(number-to-string (nth 2 time))
	":"
	(s-pad-left 2 "0"
		    (number-to-string (nth 1 time)))
	" on "
	(number-to-string (nth 5 time))
	"-"
	(number-to-string (nth 4 time))
	"-"
	(number-to-string (nth 3 time))))
     "; with message: "
     (pp (car (elt timer 6)))
     "\n\n"))))

;;;###autoload 
(defun org-timed-alerts-set-all-timers ()
  "Run `org-ql' query to get all headings with today's timestamp."
  (interactive)
  (org-timed-alerts-cancel-all-timers)
  (cl-loop for entry in (org-ql-select (or org-timed-alerts-files
					   (org-agenda-files))
			  `(or
			    (ts-repeat)
			    (ts-active
			     ;; Get timestamps for the current date
			     ;; and following date, to ensure events
			     ;; after midnight are captured. 
			     :from ,(ts-format "%Y-%m-%d" (ts-now))
			     :to ,(ts-format "%Y-%m-%d"
					     (ts-adjust 'day 1 (ts-now)))))
			  :action #'org-timed-alerts--org-ql-action)
	   do (org-timed-alerts--parser entry))
  (message "Org-timed-alerts: timers updated."))

;;;###autoload 
(defun org-timed-alerts-cancel-all-timers ()
  "Cancel all the timers."
  (interactive)
  (cl-loop for timer in org-timed-alerts--timer-list
	   do (cancel-timer timer))
  (setq org-timed-alerts--timer-list nil))

;;;###autoload 
(define-minor-mode org-timed-alerts-mode
  "Get alerts before orgmode events."
  nil
  " alerts"
  nil
  (if org-timed-alerts-mode
      (when org-timed-alerts-agenda-hook-p
	(add-hook 'org-agenda-mode-hook #'org-timed-alerts-set-all-timers))
    (org-timed-alerts-cancel-all-timers)
    (remove-hook 'org-agenda-mode-hook #'org-timed-alerts-set-all-timers)))
  
;;;; Footer

(provide 'org-timed-alerts)

;;; org-timed-alerts.el ends here



