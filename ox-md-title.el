;;; ox-md-title.el --- Document titles for ox-md.el

;; Author: Jeff Kreeftmeijer <jeff@kreeft.me>
;; Version: 0.2.0
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
       (org-md--headline-title style 0 (org-export-data title info) nil))
     (when (and org-md-title subtitle)
       (org-md--headline-title style 1 (org-export-data subtitle info) nil))
     (apply orig-fun args))))

(defun org-md-title--advise-headline-title (args)
  (when org-md-title
      (setf (nth 1 args) (+ (nth 1 args) 1)))
    args)

(defun org-md-title-add ()
  (advice-add 'org-md--headline-title :filter-args #'org-md-title--advise-headline-title)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (advice-remove 'org-md--headline-title #'org-md-title--advise-headline-title)
  (advice-remove 'org-md-template #'org-md-title--advise-template))

(provide 'ox-md-title)

;;; ox-md-title.el ends here
