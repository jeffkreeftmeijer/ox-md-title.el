;;; ox-md-title.el --- Document titles for ox-md.el

;;; Commentary:

;; ox-md-title.el adds document titles to markdown files generated
;; with ox-md and derivatives.

;;; Code:

(require 'ox-md)

(defun org-md-title--advise-template (orig-fun &rest args)
  (let* ((info (nth 1 args))
         (style (plist-get info :md-headline-style))
         (title (org-export-data (plist-get info :title) info)))
    (concat
     (org-md--headline-title style 1 title nil)
     (apply orig-fun args))))

(defun org-md-title-add ()
  (setq org-md-toplevel-hlevel 2)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (setq org-md-toplevel-hlevel 1)
  (advice-remove 'org-md-template #'org-md-title--advise-template))

(provide 'ox-md-title)

;;; ox-md-title.el ends here
