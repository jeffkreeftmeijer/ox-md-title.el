;;; ox-md-title.el --- Document titles for ox-md.el

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
  (concat
   (when org-md-title
     (let* ((info (nth 1 args))
	    (style (plist-get info :md-headline-style))
	    (title (plist-get info :title)))
       (when title
	 (org-md--headline-title style 1 (org-export-data title info) nil))))
   (apply orig-fun args)))

(defun org-md-title--advise-headline (orig-fun headline contents info)
  (let* ((level (plist-get info :md-toplevel-hlevel))
	 (level (if (and org-md-title
			 (plist-get info :title))
		    (+ level 1)
		  level)))
    (apply orig-fun (list headline contents (plist-put info :md-toplevel-hlevel level)))))

(defun org-md-title-add ()
  (advice-add 'org-md-headline :around #'org-md-title--advise-headline)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (advice-remove 'org-md-headline #'org-md-title--advise-headline)
  (advice-remove 'org-md-template #'org-md-title--advise-template))

(provide 'ox-md-title)

;;; ox-md-title.el ends here
