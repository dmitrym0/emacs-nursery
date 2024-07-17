(defun dm/get-frame-by-name (fname)
  "If there is a frame with named FNAME, return it, else nil."
  (require 'dash)                       ; For `-some'
  (-some (lambda (frame)
           (when (equal fname (frame-parameter frame 'name))
             frame))
         (frame-list)))

(defun dm/display-buffer-in-named-frame (buffer alist)
  "Display BUFFER in frame with specific name.
The name to use is the value associated with the 'named-frame key
in ALIST.  If a frame with that name already exists, use it.
Otherwise, call `display-buffer-in-pop-up-frame' to create it.

If ALIST does not contain the key 'named-frame, use the name of BUFFER."
  (let* (
         ;; (fname  (or (cdr (assq 'named-frame alist))
         ;;             (buffer-name buffer)))
         (fname "CAPTURE")
         (frame  (dm/get-frame-by-name fname)))
    (if frame
        (window--display-buffer buffer
                                (frame-selected-window frame)
                                'reuse)
      (display-buffer-pop-up-frame
       buffer nil
       ))))


(defmacro dm/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))



(defun dm/popout-select-url (&optional goto keys)
  "As `org-capture', but do all work in a new frame.

    This function by itself doesn't clean up the frame following
    capture.  To do that, add `my/org-capture-delete-capture-frame'
    to `org-capture-after-finalize-hook'."
  (interactive "P")
  (if goto
      (org-capture goto keys)
    (let ((override  '("\\*Org Select\\*\\|\\*Capture\\*\\|CAPTURE-.*|\\*Select|\\*Urls" dm/display-buffer-in-named-frame (named-frame . "Capture")))
          (buffer (get-buffer-create "CAPTURE")))



      (dm/display-buffer-in-named-frame buffer '(named-frame . "CAPTURE"))
      ;;(frame-focus (dm/get-frame-by-name "CAPTURE"))
      ;; (select-frame-set-input-focus (dm/get-frame-by-name "CAPTURE"))
      ;; Force all relevant buffers to open in a specific capture frame.
         ;; (add-to-list 'display-buffer-alist override)

      ;; (dm/with-advice
      ;;     ;; (;; Make Org-mode respect `display-buffer-alist'.
      ;;     ;;  (#'org-switch-to-buffer-other-window :override #'pop-to-buffer)
      ;;     ;;  ;; And stop Org-mode from messing with our window configuration.
      ;;     ;;  (#'delete-other-windows :override #'ignore))
      ;;   (unwind-protect (condition-case err
      ;;                     (dm/select-url)
      ;;                     (error (dm/org-capture-delete-capture-frame)
      ;;                            (signal (car err) (cdr err))))
      ;;     (setq display-buffer-alist
      ;;           (delete override display-buffer-alist))))

      (run-with-timer 1 nil #'dm/select-url)

      )))


(defun dm/org-capture-delete-capture-frame ()
  "Delete a frame named \"Capture\".
For use in `org-capture-after-finalize-hook' to clean up after
`my/org-capture-in-popout-frame'."
  (let ((frame  (my/get-frame-by-name "Capture")))
    (when frame (delete-frame frame))))


(add-hook 'org-capture-after-finalize-hook
          #'my/org-capture-delete-capture-frame)
