(require 'ert)
(load-file "ox-md-title.el")

(defun with-org-md-title (func)
  (org-md-title-add)
  (funcall func)
  (org-md-title-remove))

(ert-deftest title-test ()
  (find-file "ox-md-title.org")
  (let ((org-md-title t))
    (with-org-md-title #'org-md-export-as-markdown))

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should (string-match-p "^# ox-md-title" buffer))
    (should (string-match-p "^## Usage" buffer))))

(ert-deftest title-disabled-test ()
  (find-file "ox-md-title.org")
  (let ((org-md-title nil))
    (with-org-md-title #'org-md-export-as-markdown))

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should-not (string-match-p "^# ox-md-title" buffer))
    (should (string-match-p "^# Usage" buffer))))
