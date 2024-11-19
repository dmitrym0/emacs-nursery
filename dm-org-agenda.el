(defun dm/get-agenda-files-containing-keyword-from-grep (keyword)
  "Greps =org-roam-directory= for KEYWORD and returns matching files"
  ;;(shell-command-to-string (format "ag -l -r '\\*\\s+%s' %s |grep -v 'archive' | grep -v '.stversion' | grep '\\.org$'" keyword org-roam-directory)))
  (shell-command-to-string (format "ag -l -r '%s' %s |grep -v 'archive' | grep -v '.stversion' | grep '\\.org$'" keyword org-roam-directory)))

(defun dm/get-agenda-files-with-keyword (keyword)
  (let* ((shell-output (dm/get-agenda-files-containing-keyword-from-grep keyword)))
    (split-string (substring shell-output 0 (- (length shell-output) 1)) "\n")))


(defun dm/update-agenda-file-list (&rest args)
  "Update =org-agenda-files= with files that actually have a TODO heading."
  (interactive)
  (setq org-agenda-files (flatten-tree
                          (append (dm/get-agenda-files-with-keyword "\\*\\s+TODO") `(,(concat org-directory "gcal.org") ,(concat org-directory "vt_cal.org"))))))

(defun dm/update-agenda-file-list-with-keyword (keyword)
  (setq org-agenda-files (dm/get-agenda-files-with-keyword keyword)))


(provide 'dm-org-agenda)
