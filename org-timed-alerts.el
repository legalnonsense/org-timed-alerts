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
;; have an associated time. Alerts are sent via `alert'.
;; 

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
;; + ts
;; + alert

;; Then put this file in your load-path, and put this in your init
;; file:

;; (require 'org-timed-alerts)

;;;; Usage

;; Run this command: 

;; `elgantt-open': Open a Gantt Calendar from your agenda files

;;;; Tips

;; + You can customize settings in the `elgantt' group.

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

(defcustom org-timed-alert-final-alert-string
  "IT IS %alert-time\n\nTIME FOR:\n%todo %headline"
  "String for the final alert message, which which can use the following substitutions:
%todo         : the TODO state of the the heading, if any
%headline     : the headline text of the heading
%alert-time   : the time of the event
%warning-time : the number of minutes before the event the warning will be shown
%current-time : the time the alert is sent to the user
%category     : the category property of the org heading, or the name of the file if none")

(defcustom org-timed-alert-warning-string
  "%todo %headline\n at %alert-time\n it is now %current-time\n *THIS IS YOUR %warning-time MINUTE WARNING*"
  "String for alert warning messages, which can use the following substitutions:
%todo         : the TODO state of the the heading, if any
%headline     : the headline text of the heading
%alert-time   : the time of the event
%warning-time : the number of minutes before the event the warning will be shown
%current-time : the time the alert is sent to the user
%category     : the category property of the org heading, or the name of the file if none")

(defcustom org-timed-alerts-files (org-agenda-files)
  "List of org files used to check for events.")

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
 :id")

(defcustom org-timed-alerts-warning-times '(-10 -5)
  "List of minutes before an event when a warning will be sent.")

(defvar org-timed-alerts--timer-list nil
  "Internal list of timer objects.")

(defun org-timed-alert--string-substitute (string map)
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
	   (org-timed-alert--string-substitute
	    (if (= warning-time 0)
		org-timed-alert-final-alert-string
	      org-timed-alert-warning-string)
	    `((todo . ,(or todo ""))
	      (headline . ,headline)
	      (current-time . ,current-time)
	      (alert-time . ,(ts-format "%H:%M" time))
	      (warning-time . ,(abs warning-time))
	      (category . ,category)))
	   :title category)))))))


(defun org-timed-alerts--add-timer (time message &optional &key
					 title icon category buffer mode
					 severity data style persistent
					 never-persist id)
  "Create timers via `run-at-time' and add to `org-timed-alerts--timer-list'"
  (cl-flet ((get-default (prop)
			 (plist-get org-timed-alerts-default-alert-props prop)))
    (push (run-at-time time
		       nil
		       #'alert
		       message
		       :title (or title
				  (get-default :title))
		       :icon (or icon
				 (get-default :icon))
		       :category (or category
				     (get-default :category))
		       :buffer (or buffer
				   (get-default :buffer))
		       :mode (or mode
				 (get-default :mode))
		       :data (or data
				 (get-default :data))
		       :style (or style
				  (get-default :style))
		       :severity (or severity
				     (get-default :severity))
		       :persistent (or persistent
				       (get-default :persistent))
		       :never-persist (or never-persist
					  (get-default :never-persist))
		       :id (or id
			       (get-default :id)))
	  org-timed-alerts--timer-list)))

  (defun org-timed-alerts-set-all-timers ()
    "Run `org-ql' query to get all headings with today's timestamp."
    (interactive)
    ;; Reset existing timers
    (org-timed-alerts-cancel-all-timers)
    ;; Clear the `org-ql' cache
    (setq org-ql-cache (make-hash-table :weakness 'key))
    ;; Add timers
    (org-ql-select org-timed-alerts-files
      '(ts-active :on today)
      :action #'org-timed-alerts--parser)
    (message "org-timed-alerts: Alert timers updated."))

(defun org-timed-alerts-cancel-all-timers ()
  "Cancel all the timers."
  (interactive)
  (cl-loop for timer in org-timed-alerts--timer-list
	   do (cancel-timer timer))
  (setq org-timed-alerts--timer-list nil))

(define-minor-mode org-timed-alerts-mode
  "Alert before an event."
  nil
  nil
  nil
  (if org-timed-alerts-mode
      (progn 
	(org-timed-alerts-set-all-timers)
	(add-hook 'org-agenda-mode-hook #'org-timed-alerts-set-all-timers))
    (org-timed-alerts-cancel-all-timers)
    (remove-hook 'org-agenda-mode-hook #'org-timed-alerts-set-all-timers)))

(provide 'org-timed-alerts)
