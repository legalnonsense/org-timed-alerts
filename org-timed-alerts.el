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

;;   (eval-after-load 'org
;;     (require 'org-timed-alerts))

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

(defgroup org-timed-alerts nil
  "org-timed-alerts options"
  :tag " org-timed-alerts"
  :group 'org
  :group 'org-timed-alerts
  :prefix "org-timed-alerts-")

(defcustom org-timed-alerts-alert-function #'alert
  "Alert function. Default is #'alert. See `alert' for more possibilities.")

(defcustom org-timed-alerts-final-alert-string
  "IT IS %alert-time\n\nTIME FOR:\n%todo %headline"
  "String for the final alert message, which which can use the following substitutions:
%todo         : the TODO state of the the heading, if any
%headline     : the headline text of the heading
%alert-time   : the time of the event
%warning-time : the number of minutes before the event the warning will be shown
%current-time : the time the alert is sent to the user
%category     : the category property of the org heading, or the name of the file if none"
  :type 'string
  :group 'org-timed-alerts)

(defcustom org-timed-alerts-warning-string
  "%todo %headline\n at %alert-time\n it is now %current-time\n *THIS IS YOUR %warning-time MINUTE WARNING*"
  "String for alert warning messages, which can use the following substitutions:
%todo         : the TODO state of the the heading, if any
%headline     : the headline text of the heading
%alert-time   : the time of the event
%warning-time : the number of minutes before the event the warning will be shown
%current-time : the time the alert is sent to the user
%category     : the category property of the org heading, or the name of the file if none"
  :type 'string
  :group 'org-timed-alerts)

(defcustom org-timed-alerts-files (org-agenda-files)
  "File or list of org files used to check for events."
  :type '(list file)
  :group 'org-timed-alerts)

(defcustom org-timed-alerts-default-alert-props
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

Which moves up to the root header and returns that headline."
  :type '(plist :key-type sexp :value-type sexp)
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

(defvar org-timed-alerts--timer-list nil
  "Internal list of timer objects.")

(defun org-timed-alerts--string-substitute (string map)
  "MAP is an alist in the form of '((PLACEHOLDER . REPLACEMENT))
STRING is the original string. PLACEHOLDER is a symbol that will
be converted to a string prefixed with a %: \"%PLACEHOLDER\". 
REPLACEMENT can be a string, a number, or a symbol. Replace all
occurrences of %placeholder with replacement and return a new string."
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

(defun org-timed-alerts--get-default-prop (prop)
  "Get val for PROP from `org-timed-alerts-default-alert-props'
if val is a function, call it.  Otherwise return val."
  (let ((val (plist-get
	      org-timed-alerts-default-alert-props
	      prop)))
    (if (functionp val)
	(funcall val)
      val)))

(defun org-timed-alerts--parser ()
  ":action for `org-ql-select'"
  (-let* (((&alist "ITEM" headline
		   "TIMESTAMP" timestamp
		   "DEADLINE" deadline
		   "SCHEDULED" scheduled
		   "TODO" todo
		   "CATEGORY" category)
	   (org-entry-properties)))
    (cl-loop
     for time in (list timestamp deadline scheduled)
     when time
     do
     (setq time (ts-parse-org time))
     ;; `ts' returns 0 for hour and minute even
     ;; if a timestamp does not have an
     ;; associated time of day.  We'll assume that
     ;; if the time is midnight, there is no time of day
     ;; specification. 
     (when (and (or (not (= 0 (ts-hour time)))
		    (not (= 0 (ts-minute time))))
		(ts> time (ts-now)))
       (cl-loop
	with current-time = nil
	for warning-time in (cl-pushnew 0 org-timed-alerts-warning-times)
	do
	(setq current-time (ts-adjust 'minute (* -1 (abs warning-time)) time))
	(when (ts>
	       current-time
	       (ts-now))
	  (setq current-time (ts-format "%H:%M" current-time))
	  (org-timed-alerts--add-timer
	   current-time
	   (org-timed-alerts--string-substitute
	    (if (= warning-time 0)
		org-timed-alerts-final-alert-string
	      org-timed-alerts-warning-string)
	    `((todo . ,(or todo ""))
	      (headline . ,headline)
	      (current-time . ,current-time)
	      (alert-time . ,(ts-format "%H:%M" time))
	      (warning-time . ,(abs warning-time))
	      (category . ,category)))
	   :title (or (org-timed-alerts--get-default-prop
		       :title)
		      category))))))))

(defun org-timed-alerts--add-timer (time message &optional &key
					 title icon category buffer mode
					 severity data style persistent
					 never-persist id)
  "Create timers via `run-at-time' and add to `org-timed-alerts--timer-list'"
  (push (run-at-time
	 time
	 nil
	 org-timed-alerts-alert-function
	 message
	 :title (or title
		    (org-timed-alerts--get-default-prop :title))
	 :icon (or icon
		   (org-timed-alerts--get-default-prop :icon))
	 :category (or category
		       (org-timed-alerts--get-default-prop :category))
	 :buffer (or buffer
		     (org-timed-alerts--get-default-prop :buffer))
	 :mode (or mode
		   (org-timed-alerts--get-default-prop :mode))
	 :data (or data
		   (org-timed-alerts--get-default-prop :data))
	 :style (or style
		    (org-timed-alerts--get-default-prop :style))
	 :severity (or severity
		       (org-timed-alerts--get-default-prop :severity))
	 :persistent (or persistent
			 (org-timed-alerts--get-default-prop :persistent))
	 :never-persist (or never-persist
			    (org-timed-alerts--get-default-prop :never-persist))
	 :id (or id (org-timed-alerts--get-default-prop :id)))
	org-timed-alerts--timer-list))

;;;###autoload 
(defun org-timed-alerts-set-all-timers ()
  "Run `org-ql' query to get all headings with today's timestamp."
  (interactive)
  (org-timed-alerts-cancel-all-timers)
  ;; Clear the `org-ql' cache
  ;; (Don't know if necessary but needed for testing.)
  (setq org-ql-cache (make-hash-table :weakness 'key))
  (org-ql-select org-timed-alerts-files
    '(ts-active :on today)
    :action #'org-timed-alerts--parser)
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
      (progn
	(org-timed-alerts-set-all-timers)
	(when org-timed-alerts-agenda-hook-p
	  (add-hook 'org-agenda-mode-hook #'org-timed-alerts-set-all-timers)))
    (org-timed-alerts-cancel-all-timers)
    (remove-hook 'org-agenda-mode-hook #'org-timed-alerts-set-all-timers)))

;; Footer

(provide 'org-timed-alerts)

;;; org-timed-alerts.el ends here



