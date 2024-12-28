;;; -*- lexical-binding: t; -*-
(require 'dm-org-inbox-sweeper)

(defvar test-inbox-contents
  "* [#A] This stays
* This goes")

(defvar test-inbox-contents-more-complicated
  "* A Headline
* Second headline
* [#A] This stays
* This goes
* Another headline")



(defun create-buffer-with-string (string)
  "Create a new buffer with the content STRING and return it
STRING is the content to be added to the buffer."
  (let ((buffer (generate-new-buffer "*string-buffer*")))
    (with-current-buffer buffer
      (insert string))
    buffer))


(describe "inbox sweeper"
          (it "can generate a temporary buffer with the inbox contents"
              (let ((inbox-buffer (create-buffer-with-string test-inbox-contents)))
                (expect (with-current-buffer inbox-buffer
                          (buffer-string))
                        :to-equal test-inbox-contents)))

          (it "can sweep the inbox"
              (let ((inbox-buffer (create-buffer-with-string test-inbox-contents))
                    (secondary-file (make-temp-file "secondary-inbox.org")))
                    (with-current-buffer inbox-buffer
                        (dm/sweep-inbox (current-buffer) secondary-file))
                    (expect (with-current-buffer inbox-buffer
                                  (buffer-string))
                            ;; need a new line to match the test-inbox-contents
                            :to-equal "* [#A] This stays
")
                    (expect (with-current-buffer (find-file-noselect secondary-file)
                              (buffer-string))
                            :to-equal "* This goes\n")))


          (it "can sweep a more complicated inbox"
              (let ((inbox-buffer (create-buffer-with-string test-inbox-contents-more-complicated))
                    (secondary-file (make-temp-file "secondary-inbox.org")))
                    (with-current-buffer inbox-buffer
                        (dm/sweep-inbox (current-buffer) secondary-file))
                    (expect (with-current-buffer inbox-buffer
                                  (buffer-string))
                            ;; need a new line to match the test-inbox-contents
                            :to-equal "* [#A] This stays
")
                    (message "Secondary file: %s" secondary-file)
                    (expect (with-current-buffer (find-file-noselect secondary-file)
                              (buffer-string))
                            :not :to-be nil)))





          )
