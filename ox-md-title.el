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

(defun org-md-title--advise-headline (args)
  (when (and org-md-title (plist-get (nth 1 args) :title))
    (setf (nth 1 args) (+ (nth 1 args) 1)))
  args)

(defun org-md-title--advise-export (orig-fun &rest args)
  (let ((org-md-toplevel-hlevel (if org-md-title 2 1)))
    (apply orig-fun args)))

(defun org-md-title-add ()
  (advice-add 'org-md-export-as-markdown :around #'org-md-title--advise-export)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (advice-remove 'org-md-export-as-markdown #'org-md-title--advise-export)
  (advice-remove 'org-md-template #'org-md-title--advise-template))

(provide 'ox-md-title)

;;; ox-md-title.el ends here
