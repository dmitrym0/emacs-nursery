;;; -*- lexical-binding: t; -*-

(defun dm/sweep-current-file()
  (interactive)
  (let ((archive-file (expand-file-name "~/org-roam/secondary_inbox.org")))
    (dm/sweep-inbox (current-buffer) archive-file)))

(defun dm/sweep-inbox (inbox-buffer archive-file)
  (let (entries-to-move)
    (with-current-buffer inbox-buffer
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (unless (org-element-property :priority headline)
            (push (list (org-element-property :begin headline)
                        (org-element-property :end headline))
                  entries-to-move)))))
    (message "Entrries: %s" entries-to-move)
    (dolist (entry  entries-to-move)
      (let* ((begin (car entry))
             (end (cadr entry))
             (content (with-current-buffer inbox-buffer
                        (buffer-substring-no-properties begin end))))
        (with-temp-buffer
          (insert content)
          (insert "\n")
          (write-region (point-min) (point-max) archive-file t))
        (with-current-buffer inbox-buffer
          (delete-region begin end))))))

(provide 'dm-org-inbox-sweeper)
