(defun send-task-to-dueapp ()
  "Send the current heading to dueapp. Must be SCHEDULEd"
  (interactive)
  (let* ((plist (car (cdr (org-element-property :scheduled  (org-element-at-point)))))
         (year (plist-get plist :year-start))
         (month (plist-get plist :month-start))
         (day (plist-get plist :day-start))
         (hour (plist-get plist :hour-start))
         (minute (plist-get plist :minute-start))
         (seconds (time-to-seconds (encode-time `(0 ,minute ,hour ,day ,month ,year 0 t -25200))))
;;         (seconds (time-to-seconds (encode-time (decode-time (current-time)))))
         (title (cdr (assoc "ITEM" (org-entry-properties)))))
    (shell-command-to-string (format "open 'due://x-callback-url/add?title=%s&duedate=%d'" title seconds))))


(provide 'dm-dueapp-integration)
