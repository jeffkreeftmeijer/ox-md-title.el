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

  (let ((org-md-title t))
    (with-org-md-title #'org-md-export-as-markdown))

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should (string-match-p "^# Title" buffer))
    (should (string-match-p "^## Sub-headline" buffer))))

(ert-deftest subtitle-test ()
  (switch-to-buffer "*ox-md-title-test*")
  (erase-buffer)
  (insert "#+title: Title\n#+subtitle: Subtitle")

  (let ((org-md-title t))
    (with-org-md-title #'org-md-export-as-markdown))

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should (string-match-p "^# Title" buffer))
    (should (string-match-p "^## Subtitle" buffer))))

(ert-deftest no-title-test ()
  (switch-to-buffer "*ox-md-title-test*")
  (erase-buffer)
  (insert "* Sub-headline")

  (let ((org-md-title t))
    (with-org-md-title #'org-md-export-as-markdown))

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should-not (string-match-p "^# \n" buffer))
    (should-not (string-match-p "^## \n" buffer))
    (should (string-match-p "^# Sub-headline\n" buffer))))

(ert-deftest title-disabled-test ()
  (switch-to-buffer "*ox-md-title-test*")
  (erase-buffer)
  (insert "#+title: Title\n\n* Sub-headline")

  (let ((org-md-title nil))
    (with-org-md-title #'org-md-export-as-markdown))

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should-not (string-match-p "# Title" buffer))
    (should (string-match-p "^# Sub-headline" buffer))))

(ert-deftest toc-test ()
  (switch-to-buffer "*ox-md-title-test*")
  (erase-buffer)
  (insert "#+title: Title\n#+options: toc:2\n* Sub-headline")

  (let ((org-md-title t))
    (with-org-md-title #'org-md-export-as-markdown))

  (let ((buffer (with-current-buffer
		    "*Org MD Export*"
		  (buffer-string))))

    (should (string-match-p "^# Title" buffer))
    (should (string-match-p "\[Sub-headline\]" buffer))
    (should (string-match-p "^## Sub-headline" buffer))))
