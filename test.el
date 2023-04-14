(require 'ert)
(load-file "ox-md-title.el")

(defun with-org-md-title (func)
  (org-md-title-add)
  (funcall func)
  (org-md-title-remove))

(ert-deftest title-test ()
  (find-file "ox-md-title.org")
  (with-org-md-title #'org-md-export-as-markdown)

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should (string-match-p "^# ox-md-title" buffer))
    (should (string-match-p "^## Usage" buffer))))
