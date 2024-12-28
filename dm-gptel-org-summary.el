(require 'ts)
(require 'gptel)

(defun dm/insert-date-tree-week-for-date (org-date-string)
  (let* ((ts-date (ts-parse-org org-date-string))
         (date (list (ts-month ts-date) (ts-day ts-date) (ts-year ts-date))))
    (org-datetree-find-iso-week-create date)))

(defun dm/extract-content-below-headline (headline)
  "Grab the content between a top level headline HEADLINE and the next headline"
  (save-excursion
    (goto-char 0)
    (when (re-search-forward (concat "^\\*+ " headline) nil t)
      (next-line)
      (beginning-of-line)
      (let ((start-pos (point))
            (end-pos (or (re-search-forward  "^\\* " nil t) (point-max))))
        (goto-char end-pos)
        (previous-line)
        (end-of-line)
        (setq end-pos (point))
        (buffer-substring-no-properties start-pos end-pos)))))


(defun dm/get-summary-prompt ()
  "Extract the prompt to feed to ChatGPT"
  (with-temp-buffer
    (insert-file-contents "~/org-roam/daily_summary_prompt.org")
    (dotimes (_ 15) ;; remove org-properties from the top of the file
      (kill-whole-line))
    (buffer-string)))

(defun dm/summarize-day ()
  "Extracts all tasks from the Tasks top level headline and summarizes them via chat gpt"
  (message (dm/get-summary-prompt))
  (interactive)
  ;; (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
  (let ((selection (dm/extract-content-below-headline "Tasks")))
    (gptel-request
        (concat (dm/get-summary-prompt) selection)
      :buffer (current-buffer)
      :callback
      (lambda (response info)
        (if (not response)
            (message "ChatGPT response failed with: %s" (plist-get info :status))
          (let* ((daily-title (org-roam-node-title (org-roam-node-at-point)))
                 (node-id (org-roam-node-id (org-roam-node-at-point))))
            (save-excursion
              (find-file "~/org-roam/weekly_work_summary_2024.org")
              (dm/insert-date-tree-week-for-date daily-title)
              (end-of-line)
              (insert "\n")
              (insert (dm/post-process-summary response))
              (message "--- DONE --- "))))))))


(defun dm/post-process-summary (summary)
  (with-temp-buffer
    (insert summary)
    (goto-char 0)
    (while (re-search-forward "\\(MEX-\\|CAC-\\)[0-9]*" nil t)
      (backward-char)
      (dm/thing-to-mex-link)
      (end-of-line))
    (buffer-string)))
