(require 'ert)
(load-file "ox-md-title.el")

(ert-deftest title-test ()
  (org-md-title-add)
  (find-file "ox-md-title.org")
  (org-md-export-as-markdown)

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should (string-match-p "# ox-md-title" buffer))
    (should (string-match-p "## Usage" buffer)))
  (org-md-title-remove))
