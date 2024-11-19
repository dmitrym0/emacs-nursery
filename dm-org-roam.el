;;  (setq nnn (org-roam-node-from-title-or-alias "elisp"))


(defun dm/link-to-org-roam-node (node)
  (let ((id (org-roam-node-id node))
        (description (org-roam-node-title node)))
    (insert (org-link-make-string
             (concat "id:" id)
             description))))


(defun dm/find-or-create-org-roam-node-by-name (name)
  (let ((node (org-roam-node-from-title-or-alias name)))
    (if node
        (dm/link-to-org-roam-node node)
      (dm/link-to-org-roam-node (dm/org-roam-fast-make-link name)))))

(defun dm/tag-to-org-link ()
  "Convert a symbol under cursor to an org-roam tag"
  (interactive)
  (let ((tag (thing-at-point 'symbol t))
        (old-pos (point)))
    (condition-case err
        (progn
          (save-excursion
            (goto-char (car (bounds-of-thing-at-point 'symbol)))
            (delete-region (car (bounds-of-thing-at-point 'symbol)) (cdr (bounds-of-thing-at-point 'symbol)))
            (dm/find-or-create-org-roam-node-by-name tag)))
      (error
       ;; if we errored, reinsert the original symbol
       (insert tag)
       (message "Tag: %s -- Caught error: %s" tag err)))))


(defun dm/create-org-roam-node (name)
  (org-roam--title-to-file name))

(defun dm/org-roam-fast-make-link (s)
  ;; https://emacs.stackexchange.com/questions/73158/how-to-programmatically-create-a-new-org-roam-file
  "Make an org-roam node with title S and return a link to it.

                 We eschew the usual org-capture approach for a fast, non-interactive result."
  (let* ((slug (org-roam-node-slug (org-roam-node-create :title s)))
         (filename (format "%s/%s.org"
                           (expand-file-name org-roam-directory)
                           slug))
         (org-id-overriding-file-name filename)
         id)
    (with-temp-buffer
      (insert ":PROPERTIES:\n:ID:        \n:END:\n#+title: "
              s)
      (goto-char 25)
      (org-mode)
      (setq id (org-id-get-create))
      (write-file filename)
      (org-roam-db-update-file filename)
      ;; (format "[[id:%s][%s]]" id s)
      ;; (org-roam-node-from-title-or-alias s)
      )
    filename))



(defun dm/org-roam-fast-make-node-with-title (s)
  ;; https://emacs.stackexchange.com/questions/73158/how-to-programmatically-create-a-new-org-roam-file
  "Make an org-roam node with title S and return it's file path."
  (let* ((slug (org-roam-node-slug (org-roam-node-create :title s)))
         (filename (format "%s/%s.org"
                           (expand-file-name org-roam-directory)
                           slug))
         (org-id-overriding-file-name filename)
         id)
    (with-temp-buffer
      (insert ":PROPERTIES:\n:ID:        \n:END:\n#+title: "
              s)
      (goto-char 25)
      (org-mode)
      (setq id (org-id-get-create))
      (write-file filename)
      (org-roam-db-update-file filename)
      )
    filename))
