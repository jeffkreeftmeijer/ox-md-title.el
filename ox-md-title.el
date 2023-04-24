;;; ox-md-title.el --- Document titles for ox-md.el

;; Author: Jeff Kreeftmeijer <jeff@kreeft.me>
;; Version: 0.1
;; URL: https://jeffkreeftmeijer.com/ox-md-title

;;; Commentary:

;; ox-md-title.el adds document titles to markdown files generated
;; with ox-md and derivatives.

;;; Code:

(require 'ox-md)

(defgroup org-export-md-title nil
  "Options for org-md-title."
  :tag "Org Markdown Title"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-md-title nil
  "Non-nil means to include the title in the exported document."
  :group 'org-export-md-title
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defun org-md-title--advise-template (orig-fun &rest args)
  (let* ((info (nth 1 args))
	 (style (plist-get info :md-headline-style))
	 (title (plist-get info :title))
	 (subtitle (plist-get info :subtitle)))
    (concat
     (when (and org-md-title title)
       (org-md--headline-title style 1 (org-export-data title info) nil))
     (when (and org-md-title subtitle)
       (org-md--headline-title style 2 (org-export-data subtitle info) nil))
     (apply orig-fun args))))

(defun org-md-title--advise-level (orig-fun headline info)
  (+ (funcall orig-fun headline info)
     (if (and org-md-title (plist-get info :title))
	 1
       0)))

(defun org-md-title-add ()
  (advice-add 'org-export-get-relative-level :around #'org-md-title--advise-level)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (advice-remove 'org-export-get-relative-level #'org-md-title--advise-level)
  (advice-remove 'org-md-template #'org-md-title--advise-template))

(provide 'ox-md-title)

;;; ox-md-title.el ends here
