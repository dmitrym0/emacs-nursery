;; create dynamic block to list overdue tasks
;; #+BEGIN: dm/list-overdue-items
;; #+END:
;; run with C-c C-c
  (defun dm/get-overdue-tasks ()
    (org-ql-select (org-agenda-files) '(and (todo) (scheduled :to -1))))

  (defun org-dblock-write:dm/list-overdue-items (params)
    (let ((overdue-tasks (dm/get-overdue-tasks)))
      (save-excursion
        (insert "Overdue Tasks:\n")
        (dolist (task overdue-tasks)
          (let* ((headline (substring-no-properties (car (org-element-property :title task))))
                 (id (org-element-property :ID task)))
            (insert "-")
            (org-insert-link nil (format "id:%s" id)  headline)
            (insert "\n")
            ))
        (insert "Done"))))
