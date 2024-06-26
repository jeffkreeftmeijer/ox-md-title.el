;;; ox-md-title.el --- Document titles for ox-md.el

;; Author: Jeff Kreeftmeijer <jeff@kreeft.me>
;; Version: 0.3.0
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

(defun org-md-title--advise-template (orig-fun &rest args)
  (let ((info (nth 1 args))
        (orig-result (apply orig-fun args)))
    (if (plist-get info :with-title)
        (let ((style (plist-get info :md-headline-style))
              (title (plist-get info :title))
              (subtitle (plist-get info :subtitle)))
          (concat
           (when title
             (org-md--headline-title style 0 (org-export-data title info) nil))
           (when subtitle
             (org-md--headline-title style 1 (org-export-data subtitle info) nil))
           orig-result))
      orig-result)))

(defun org-md-title--advise-headline-title (args)
  (setf (nth 1 args) (+ (nth 1 args) 1))
  args)

(defun org-md-title-add ()
  (advice-add 'org-md--headline-title :filter-args #'org-md-title--advise-headline-title)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (advice-remove 'org-md--headline-title #'org-md-title--advise-headline-title)
  (advice-remove 'org-md-template #'org-md-title--advise-template))

(provide 'ox-md-title)

;;; ox-md-title.el ends here
