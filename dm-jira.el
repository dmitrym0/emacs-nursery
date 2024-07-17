;; handy functions for dealing with daily reports
(require 'ht)




(defun dm/jira-get-heading-and-tags ()
  "Get heading title and tags for current headline"
  (let ((heading (org-entry-get nil "ITEM"))
        (tags (org-element-property :ID  (org-element-at-point))))
    `(,(concat tags " " heading) . ,tags)))

(defun dm/get-tickets-from-current-sprint ()
    "Get all TODO and DONE tasks from the current sprint (generated to org-jira)"
    (org-ql-select "~/.org-jira/mex-current-sprint.org" '(todo "TODO" "DONE") :action #'dm/jira-get-heading-and-tags))

(defun dm/get-and-insert-jira-ticket ()
  "Select a ticket from current sprint and insert it at point"
  (interactive)
  (let ((tickets (dm/get-tickets-from-current-sprint)))
  (insert (alist-get (completing-read "Select a ticket " tickets) tickets nil nil 'equal))))



(defun dm/thing-to-mex-link ()
  "Convert a XXX-1231 looking thing to a ticket in jira"
  (interactive)
  (let ((ticket-id (thing-at-point 'symbol t)))
    (delete-char 1)
    (delete-backward-char (- (length ticket-id) 1))
    (org-insert-link nil (format (concat jiralib-url "browse/%s") ticket-id) ticket-id)))


(defun dm/extract-headlines ()
  (save-excursion
    (goto-char (point-min))
    (let (headlines)
      (while (re-search-forward "^\\*+\\s-+\\(.*\\)" nil t)
        (push (match-string 1) headlines))
      (reverse headlines))))

(defun dm/re-insert-exctracted-headlines ()
  "Strips all org-mode children from headlines and reinserts them back"
  (interactive)
  (let ((headlines (dm/extract-headlines)))
    (dolist (headline headlines)
      (insert "* ")
      (insert headline)
      (insert "\n"))))



(defun dm/selection-to-report ()
  (interactive)
  (let ((selection (buffer-substring-no-properties (region-beginning) (region-end)))) (with-temp-buffer
      ;; (message "Selection %s" selection)
      (insert selection)
      (org-mode)
      ;; extract just the headlines
      (let ((original-max-point (point-max)))
        (message "Extracting headlines, original max-point %s" original-max-point)
        (goto-char original-max-point)
        (dm/re-insert-exctracted-headlines)
        (delete-region (point-min) original-max-point))
      ;;; select the whole buffer
      (mark-whole-buffer)
      (dm/org-formatting-to-geekbot-report)
      (mark-whole-buffer)
      (message "Copy as markdown")
      (embark-org-copy-as-markdown (point-min) (point-max))
      ;; kill current temporary buffer
      (message "Kill buffer")
      (kill-buffer (current-buffer)))))

(defun dm/org-formatting-to-geekbot-report ()
  "Turn a set of org-headings into a list for daily stand up"
  (interactive)
  (let ((original-mark (mark))
        (original-point (point)))

    ;; remove formatting
    (message "Toggle heading")
    (org-toggle-heading)
    ;; ;; reselect
    (set-mark original-mark)
    (goto-char original-point)
    ;; ;; make a list
    ;; (org-toggle-item nil)

    (message "Regex")
    (replace-regexp "DONE" "☑︎" t original-mark original-point)
    (replace-regexp "TODO" "☐" t original-mark original-point)
    (replace-regexp "INPROGRESS" "⏳︎" t original-mark original-point)

    )
  )



(keymap-set embark-identifier-map "D" #'dm/thing-to-mex-link)
(keymap-set embark-org-item-map "D" #'dm/thing-to-mex-link)
(keymap-set embark-general-map "D" #'dm/thing-to-mex-link)


(provide 'dm-jira)
