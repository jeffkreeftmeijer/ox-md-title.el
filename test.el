(require 'ert)
(load-file "ox-md-title.el")

(ert-deftest title-test ()
  (org-md-title-add)
  (find-file "ox-md-title.org")
  (org-md-export-as-markdown)
  (should (string-match-p
           "# ox-md-title"
           (with-current-buffer "*Org MD Export*" (buffer-string))))
  (org-md-title-remove))
