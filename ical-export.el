(setq org-directory "~/org-roam/")
;; (defun set-org-agenda-files ()
;;   "Set different org-files to be used in `org-agenda`."
;;   ;; (setq org-agenda-files (list (concat org-directory "things.org")
  ;;       		       (concat org-directory "reference.org")
  ;;       		       (concat org-directory "media.org")
  ;;       		       (concat org-directory "shared_with/bob.org")
  ;;       		       "~/src/your_company/admin/things.org"
  ;;       		       "~/src/your_customer/admin/pm.org")))

;; Setting variables for the ics file path
(setq org-agenda-private-local-path "~/Dropbox/temp/4.ics")
(setq org-agenda-private-remote-path "/sshx:user@host:path/dummy.ics")

;; Define a custom command to save the org agenda to a file

(defun org-agenda-export-to-ics ()
  (setq org-agenda-custom-commands-old org-agenda-custom-commands)
  (setq org-agenda-custom-commands
        `(("X" agenda "" nil ,(list org-agenda-private-local-path))))

   (setq org-agenda-files (flatten-tree
                          (append (dm/get-active-agenda-files))))




  ;; (set-org-agenda-files)
  ;; Run all custom agenda commands that have a file argument.
  (org-batch-store-agenda-views)

  ;; Org mode correctly exports TODO keywords as VTODO events in ICS.
  ;; However, some proprietary calendars do not really work with
  ;; standards (looking at you Google), so VTODO is ignored and only
  ;; VEVENT is read.
  (with-current-buffer (find-file-noselect org-agenda-private-local-path)
    (goto-char (point-min))
    (while (re-search-forward "VTODO" nil t)
      (replace-match "VEVENT"))
    (save-buffer))

  ;; Copy the ICS file to a remote server (Tramp paths work).
  ;; (copy-file org-agenda-private-local-path org-agenda-private-remote-path t)
  (dm/update-agenda-file-list)
  (setq org-agenda-custom-commands org-agenda-custom-commands-old )
)



(setq calendar-time-zone -480)
(setq calendar-standard-time-zone-name "PST")
(setq calendar-daylight-time-zone-name "PDT")

(setq org-icalendar-timezone "America/Vancouver")
;;(setq org-icalendar-use-scheduled event-if-todo)
