(require 'ert)
(load-file "ox-md-title.el")

(defun with-org-md-title (func)
  (org-md-title-add)
  (funcall func)
  (org-md-title-remove))

(ert-deftest title-test ()
  (switch-to-buffer "*ox-md-title-test*")
  (erase-buffer)
  (insert "#+title: Title\n\n* Sub-headline")

  (with-org-md-title #'org-md-export-as-markdown)

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should (string-match-p "^# Title" buffer))
    (should (string-match-p "^## Sub-headline" buffer))))

(ert-deftest subtitle-test ()
  (switch-to-buffer "*ox-md-title-test*")
  (erase-buffer)
  (insert "#+title: Title\n#+subtitle: Subtitle")

  (with-org-md-title #'org-md-export-as-markdown)

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should (string-match-p "^# Title" buffer))
    (should (string-match-p "^## Subtitle" buffer))))

(ert-deftest toc-test ()
  (switch-to-buffer "*ox-md-title-test*")
  (erase-buffer)
  (insert "#+title: Title\n#+options: toc:2\n* Sub-headline")

  (with-org-md-title #'org-md-export-as-markdown)

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should (string-match-p "^# Title" buffer))
    (should (string-match-p "\[Sub-headline\]" buffer))
    (should (string-match-p "^## Sub-headline" buffer))))

(ert-deftest title-without-title-test ()
  (switch-to-buffer "*ox-md-title-test*")
  (erase-buffer)
  (insert "#+title: Title\n\n* Sub-headline")

  (let ((org-export-with-title nil))
    (with-org-md-title #'org-md-export-as-markdown))

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should-not (string-match-p "^# Title" buffer))))
