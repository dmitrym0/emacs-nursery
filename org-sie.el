(require 'dash)

;; simple incremental everything

(setq org-sie-last-review-property "SIE_LAST_REVIEW")
(setq org-sie-next-review-property "SIE_NEXT_REVIEW")


(setq org-review-last-timestamp-format 'inactive)
(setq org-review-next-timestamp-format 'inactive)


(setq org-review-last-property-name org-sie-last-review-property)
(setq org-review-next-property-name org-sie-next-review-property)

;; "How important is this to you?"
;; The higher the value the highter the importance.
;; 1  - not important. Not very interested.
;; 5  - very important. Most is interested.
;; (inverse is passed to org-drill)
(defun setup-org-sie-keybindings-for-agenda ()
  (local-set-key (kbd "C-c d 1")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 5)
                   (org-agenda-next-item 1)
                   ))
  (local-set-key (kbd "C-c d 2")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 4)
                   (org-agenda-next-item 1)
                   ))
  (local-set-key (kbd "C-c d 3")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 3)
                   (org-agenda-next-item 1)
                   ))
  (local-set-key (kbd "C-c d 4")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 2)
                   (org-agenda-next-item 1)
                   ))

  (local-set-key (kbd "C-c d 5")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 1)
                   (org-agenda-next-item 1)
                   ))
  )



(add-hook 'org-agenda-mode-hook 'setup-org-sie-keybindings-for-agenda)




(global-set-key (kbd "C-c d s")
                (lambda ()
                  (interactive)
                  (org-sie-start-sie-on-heading)))


;; from org-roam-review


(defun org-sie-start-sie-on-heading ()
  "Add current heading to org-sie review list"
    (org-set-property org-sie-next-review-property "[2000-01-01 Mon]"))

(defun org-sie-get-next-review-from-agenda (quality)
  (save-window-excursion
    (org-agenda-goto)
    (org-sie-get-next-review quality)))

(defun org-sie-get-next-review (quality)
  "Adapted from org-drill. 1-Soon 5-Later.

QUALITY is a number 0-5 inclusive.

- only use sm5 algorithm for simplicity
- use properties instead of SCHEDULED.
- remove support for 'weighting' a node."
  (-let* ((ofmatrix org-drill-sm5-optimal-factor-matrix)
          ((last-interval repetitions failures total-repeats meanq ease) (org-drill-get-item-data))
          ((next-interval repetitions ease failures meanq total-repeats new-ofmatrix)
           (org-drill-determine-next-interval-sm5 last-interval repetitions
                                                  ease quality failures
                                                  meanq total-repeats ofmatrix))
          (next-interval (round (if (cl-minusp next-interval)
                                    next-interval
                                  (max 1.0 (+ last-interval (- next-interval last-interval))))))
          (new-time (ts-adjust 'day next-interval (ts-now))))
    (setq org-drill-sm5-optimal-factor-matrix new-ofmatrix)
    (org-drill-store-item-data next-interval repetitions failures total-repeats meanq ease)

    (let ((next-review (ts-format "[%Y-%m-%d %a]" new-time)))
      (org-set-property org-sie-next-review-property next-review)
      next-review)))
