(defun dm/get-weekday-checkist ()
    (when (file-readable-p "~/workspace/org-dynamics-checklists/template-morning-work-checklist.org")
      (with-temp-buffer
        (insert-file-contents "~/workspace/org-dynamics-checklists/template-morning-work-checklist.org")
        (buffer-string)))
)



(defun dm/get-focus-file ()
    (when (file-readable-p "~/org-roam/focus.org")
      (with-temp-buffer
        (insert-file-contents "~/org-roam/focus.org")
        (goto-char (point-min))
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (kill-line)
        (buffer-string)
        )))


(defun dm/get-checklist-template ()
  (concat (dm/get-weekday-checkist) (dm/get-focus-file))
)



(defun +org-capture/open-frame (&optional initial-input key)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (when (and initial-input (string-empty-p initial-input))
    (setq initial-input nil))
  (when (and key (string-empty-p key))
    (setq key nil))
  (let* ((frame-title-format "")
         (frame (if (+org-capture-frame-p)
                    (selected-frame)
                  (make-frame +org-capture-frame-parameters))))
    (select-frame-set-input-focus frame)  ; fix MacOS not focusing new frames
    (with-selected-frame frame
      (require 'org-capture)
      (condition-case ex
          (letf! ((#'pop-to-buffer #'switch-to-buffer))
            (switch-to-buffer (doom-fallback-buffer))
            (let ((org-capture-initial initial-input)
                  org-capture-entry)
              (when (and key (not (string-empty-p key)))
                (setq org-capture-entry (org-capture-select-template key)))
              (funcall +org-capture-fn)))
        ('error
         (message "org-capture: %s" (error-message-string ex))
         (delete-frame frame))))))
