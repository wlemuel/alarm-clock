;;; alarm-clock.el --- Alarm Clock                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Steve Lemuel

;; Author: Steve Lemuel <wlemuel@hotmail.com>
;; Keywords: calendar, tools, convenience
;; Version: 2019.02.12
;; Package-Version: 20190212.1
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/wlemuel/alarm-clock

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

;;; Commentary:

;; This program is an alarm management tool for Emacs.
;; To set an alarm clock, call `M-x alarm-clock-set', then enter time as
;; the following tips.
;; To view alarm clock list, call `M-x alarm-clock-list-view', then use
;; 'a' key to set a new alarm clock,
;; 'C-k' to kill an alarm clock in the current line.

;;; Code:

(require 'parse-time)

(defgroup alarm-clock nil
  "An alarm clock management."
  :group 'applications
  :prefix "alarm-clock-")

(defcustom alarm-clock-sound-file
  (concat
   (file-name-directory (or load-file-name buffer-file-name))
   "alarm.mp3")
  "File to play the alarm sound."
  :type 'file
  :group 'alarm-clock)

(defcustom alarm-clock-play-sound t
  "Whether to play sound when notifying, only avaiable for osx and linux."
  :type 'boolean
  :group 'alarm-clock)

(defcustom alarm-clock-play-sound-repeat 1
  "Number of times to repeat the sound when an alarm rings. Use M-x alarm-clock-stop to quiet the alarm."
  :type 'integer
  :group 'alarm-clock)

(defcustom alarm-clock-play-auto-view-alarms nil
  "If non-nul, display the alarm clock list when ringing an alarm, to allow using SPACE to run alarm-clock-stop"
  :type 'boolean
  :group 'alarm-clock)

(defcustom alarm-clock-system-notify t
  "Whether to notify via system based notification feature."
  :type 'boolean
  :group 'alarm-clock)

(defcustom alarm-clock-cache-file
  (expand-file-name ".alarm-clock.cache" user-emacs-directory)
  "The name of alarm-clock's cache file."
  :type 'string
  :group 'alarm-clock)

(defvar alarm-clock--alist nil
  "List of information about alarm clock.")

(defvar alarm-clock--macos-sender nil
  "Notification sender for MacOS.")

(defvar alarm-clock--stopped nil
  "If true, stop sounding the alarm. Set to t by M-x alarm-clock-stop or pressing SPACE in alarm-clock-list-view window")

(define-derived-mode alarm-clock-mode special-mode "Alarm Clock"
  "Mode for listing alarm-clocks.

\\{alarm-clock-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t)

  (define-key alarm-clock-mode-map [(control k)] 'alarm-clock-kill)
  (define-key alarm-clock-mode-map "d" 'alarm-clock-kill)
  (define-key alarm-clock-mode-map "a" 'alarm-clock-set)
  (define-key alarm-clock-mode-map "i" 'alarm-clock-set)
  (define-key alarm-clock-mode-map "g" 'alarm-clock-list-view)
  (define-key alarm-clock-mode-map " " 'alarm-clock-stop)
  )

;;;###autoload
(defun alarm-clock-set (time message)
  "Set an alarm clock at time TIME.
MESSAGE will be shown when notifying in the status bar."
  (interactive "sAlarm at (e.g: 2 minutes, 60 seconds, 3 days): \nsMessage: ")
  (let* ((time (if (stringp time) (string-trim time) time))
         (message (string-trim message))
         (timer (run-at-time
                 time
                 nil
                 (lambda (message) (alarm-clock--notify "Alarm Clock" message))
                 message)))
    (push (list :time (timer--time timer)
                :message message
                :timer timer)
          alarm-clock--alist))
  (alarm-clock--list-prepare))

;;;###autoload
(defun alarm-clock-list-view ()
  "Display the alarm clocks."
  (interactive)
  ;;(unless alarm-clock--alist
  ;; (user-error "No alarm clocks are set"))
  (alarm-clock--list-prepare)
  (pop-to-buffer "*alarm clock*"))

(defun alarm-clock--sort-list ()
  "Sort the alarm in increasing time"
  (sort alarm-clock--alist (lambda (a b)
                             (let* ((time-a (plist-get a :time))
                                    (time-b (plist-get b :time)))
                               (time-less-p time-b time-a)))))

(defun alarm-clock--list-prepare ()
  "Prefare the list buffer."
  (alarm-clock--cleanup)
  (set-buffer (get-buffer-create "*alarm clock*"))
  (alarm-clock-mode)
  (let* ((format "%-20s %-12s   %s")
         (inhibit-read-only t) )
    (erase-buffer)
    (setq header-line-format (format format " Time" " Remaining" " Message  (Press SPACE to stop a ringing alarm)"))
    (dolist (alarm (alarm-clock--sort-list))
      (let* ((alarm-time (plist-get alarm :time))
             (alarm-message (plist-get alarm :message))
             ;; I think alarms are removed from the list when they fire, so no negative remaining values
             (remaining (format-time-string "%H:%2M:%2S" (time-subtract alarm-time nil) 0) )
             (start (point))
             (time (format-time-string "%F %X" alarm-time)))
        (insert (format format time remaining alarm-message) "\n")
        (put-text-property start (1+ start) 'alarm-clock alarm))
      (goto-char (point-min)))))

;;;###autoload
(defun alarm-clock-stop ()
  "Stop sounding the current alarm."
  (interactive)
  (setq alarm-clock--stopped t)
  (message "Alarm stopped.")
  )

(defun alarm-clock-kill ()
  "Kill the current alarm clock."
  (interactive)
  (let* ((start (line-beginning-position))
         (alarm (get-text-property start 'alarm-clock))
         (inhibit-read-only t))
    (unless alarm
      (user-error "No alarm clock on the current line"))
    (forward-line 1)
    (delete-region start (point))
    (cancel-timer (plist-get alarm :timer))
    (setq alarm-clock--alist (delq alarm alarm-clock--alist))))

(defun alarm-clock--cleanup ()
  "Remove expired records."
  (dolist (alarm alarm-clock--alist)
    (when (time-less-p (plist-get alarm :time) (current-time))
      (setq alarm-clock--alist (delq alarm alarm-clock--alist)))))

(defun alarm-clock--ding-on-timer (program sound repeat) ;; (alarm-clock--ding)
  "Play the alarm sound asynchronously until stopped"
  ;; (message "(alarm-clock--ding-on-timer %s %s %d)" program sound repeat)
  (when (and (not alarm-clock--stopped)
             (> repeat 0))
    (start-process "Alarm Clock" nil program sound)
    (run-at-time 2
                 nil
                 (lambda (repeat) (alarm-clock--ding-on-timer program sound repeat))
                 (- repeat 1)
                 )))

(defun alarm-clock--ding () ;; (alarm-clock--ding)
  "Play ding.
In osx operating system, 'afplay' will be used to play sound,
and 'mpg123' in linux"
  (let ((program (cond ((eq system-type 'darwin) "afplay")
                       ((eq system-type 'gnu/linux) "mpg123")
                       (t "")))
        (sound (expand-file-name alarm-clock-sound-file)))
    (when (and (executable-find program)
               (file-exists-p sound))
      (setq alarm-clock--stopped nil)
      (run-at-time
       "0"
       nil
       (lambda (repeat) (alarm-clock--ding-on-timer program sound repeat))
       alarm-clock-play-sound-repeat))))

(defun alarm-clock--system-notify (title message)
  "Notify with formatted TITLE and MESSAGE by the system notification feature."
  (let ((program (cond ((eq system-type 'darwin) "terminal-notifier")
                       ((eq system-type 'gnu/linux) "notify-send")
                       (t "")))
        (args (cond ((eq system-type 'darwin) `("-title" ,title
                                                ,@(alarm-clock--get-macos-sender)
                                                "-message" ,message
                                                "-ignoreDnD"))
                    ((eq system-type 'gnu/linux) (list "-u" "critical" title message)))))
    (when (executable-find program)
      (apply 'start-process (append (list title nil program) args)))))

(defun alarm-clock--notify (title message)
  "Notify in status bar with formatted TITLE and MESSAGE."
  (and alarm-clock-play-auto-view-alarms
       (alarm-clock-list-view))
  (when alarm-clock-play-sound
    (alarm-clock--ding))
  (when alarm-clock-system-notify
    (alarm-clock--system-notify title message))
  (message (format "[%s] - %s" title message)))

;;;###autoload
(defun alarm-clock-restore ()
  "Restore alarm clocks on startup."
  (interactive)
  (alarm-clock--kill-all)
  (let* ((file alarm-clock-cache-file)
         (alarm-clocks (unless (zerop (or (nth 7 (file-attributes file)) 0))
                         (with-temp-buffer
                           (insert-file-contents file)
                           (read (current-buffer))))))
    (when alarm-clocks
      (dolist (alarm alarm-clocks)
        (alarm-clock-set (parse-iso8601-time-string (plist-get alarm :time))
                         (plist-get alarm :message))))))

;;;###autoload
(defun alarm-clock-save ()
  "Save alarm clocks to local file."
  (interactive)
  (let ((alarm-clocks))
    (dolist (alarm alarm-clock--alist)
      (unless (time-less-p (plist-get alarm :time) (current-time))
        (push (list :time (format-time-string "%FT%T%z" (plist-get alarm :time))
                    :message (plist-get alarm :message))
              alarm-clocks)))
    (with-temp-file alarm-clock-cache-file
      (insert ";; Auto-generated file; don't edit\n")
      (pp alarm-clocks (current-buffer)))))

(defun alarm-clock--kill-all ()
  "Kill all timers."
  (dolist (alarm alarm-clock--alist)
    (cancel-timer (plist-get alarm :timer))
    (setq alarm-clock--alist (delq alarm alarm-clock--alist))))

(defun alarm-clock--turn-autosave-on ()
  "Turn `alarm-clock-save' on."
  (alarm-clock-restore)
  (add-hook 'kill-emacs-hook #'alarm-clock-save))

(defun alarm-clock--turn-autosave-off ()
  "Turn `alarm-clock-save' off."
  (remove-hook 'kill-emacs-hook #'alarm-clock-save))

(defun alarm-clock--get-macos-sender ()
  "Get proper sender for notifying in MacOS"
  (when (not alarm-clock--macos-sender)
    (let* ((versions (split-string
                      (shell-command-to-string "sw_vers -productVersion")
                      "\\." t))
           (major-version (string-to-number (car versions)))
           (minor-version (string-to-number (cadr versions))))
      (unless (and (>= major-version 10)
                   (>= minor-version 15))
        (setq alarm-clock--macos-sender '("-sender" "org.gnu.Emacs")))))
  alarm-clock--macos-sender)

(provide 'alarm-clock)
;;; alarm-clock.el ends here
