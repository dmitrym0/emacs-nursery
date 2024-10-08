(defun dm/org-pomodoro-notify ()
  (let ((seconds (org-pomodoro-remaining-seconds)))
        (when (and (< seconds 300) (not org-pomodoro-notified-getting-there))
          (call-process-shell-command (format "~/bin/hs_message 'Less than 5 minutes left'") nil nil)
          (setq org-pomodoro-notified-getting-there t))
        (when (and (< seconds 60) (not org-pomodoro-notified-almost-over))
          (call-process-shell-command (format "~/bin/hs_message 'Less than 1 minute left'") nil nil)
          (setq org-pomodoro-notified-almost-over t))))
