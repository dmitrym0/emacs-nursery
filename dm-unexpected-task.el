(defun dm/capture-unexpected-task-into-org-roam-daily ()
  "Insert a custom message and timestamp."
  (interactive)
      (set-buffer (org-capture-target-buffer (dm/org-roam-today-daily-path)))
      (goto-char (org-find-or-create-heading "Tasks"))
      )


(defun dm/org-roam-today-daily-path ()
  (let* ((daily-dir (file-name-as-directory org-roam-dailies-directory)) ;; Ensure it's a directory
         (date-format "%Y-%m-%d")  ;; Adjust if you have a custom date format for daily notes
         (today (format-time-string date-format)))
    (expand-file-name (concat org-directory org-roam-dailies-directory today ".org") daily-dir)))
