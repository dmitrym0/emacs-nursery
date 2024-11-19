(defun org-find-or-create-heading (name)
  "Returns the marker for an existing heading NAME
or create a new top level heading and returns its position"
  (save-excursion
    (goto-char 0)
    (let ((pos (org-find-exact-headline-in-buffer name)))
      (if (not pos)
          (progn (goto-char  (buffer-size))
                 (org-insert-heading nil nil t)
                 (insert name)
                 (point))
        pos))))



(defun dm/log-to-daily ()
  (interactive)
  (org-roam-dailies-goto-today)
  (org-insert-new-date-in-log)
  )

(defun org-insert-new-date-in-log ()
  "Finds or creates a heading called 'Log',
and appends an inactive time stamp to it."
  (interactive)
  (goto-char (org-find-or-create-heading "Log"))
  (org-end-of-subtree)
  (org-insert-heading)
  ;; '(16) is the universal argument it gets us a timestamp as well.
  (org-time-stamp-inactive '(16))
  ;; when we only have the top level heading we need to manually
  ;; alt-right the heading so it's second level.
  ;; subsequent inserts are aligned automatically somehow.
  (when (equal 1 (car (org-heading-components)))
    (org-metaright))
  (insert "\n")
  (evil-insert 0)
  )

(defun exit-current-buffer-journal (&rest optional)
  (interactive)
  (current-buffer-journal 0)
  (delete-frame))

;;(define-key org-mode-map (kbd "C-c d h") #'org-insert-new-date-in-log)
(define-key org-mode-map (kbd "C-c d h") #'dm/log-to-daily)
(define-key org-mode-map (kbd "C-c d H") #'org-insert-new-date-in-log)

(defvar current-buffer-journal-mode-map (make-sparse-keymap)
  "Keymap while current-buffer-journal mode is active.")

(define-minor-mode current-buffer-journal
  "Journaling in the current buffer"
  :keymap current-buffer-journal-mode-map)

(define-key current-buffer-journal-mode-map (kbd "C-c C-c") 'exit-current-buffer-journal)

(provide 'dm-org-journal-in-current-buffer)
