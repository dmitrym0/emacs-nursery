(defun dm/get-bookmark-heading ()
  "Get the heading and url of the current bookmark"
  (let ((heading (org-entry-get nil "ITEM"))
        (url (org-element-property :URL  (org-element-at-point)))
        (tags (nth 5 (org-heading-components)))
        )
    `(,(concat heading " " tags)  . ,url)))

(defun dm/get-all-urls-2 ()
    "Get a list of all the bookmarks in my favurls.org file"
    (org-ql-select "~/org-roam/favurls.org" '(org-get-heading t t) :action #'dm/get-bookmark-heading))


(defun dm/get-fav-url-2 ()
  (interactive)
  (let ((urls (dm/get-all-urls-2)))
    (alist-get (completing-read "Select url " urls) urls nil nil 'equal)))



(provide 'favurls)



