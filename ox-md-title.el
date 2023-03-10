(require 'ox-extend)

(defun ox-md-title--advise-template (orig-fun &rest args)
  (let ((info (nth 1 args)))
    (let ((style (plist-get info :md-headline-style))
          (title (org-export-data (plist-get info :title) info)))
      (concat
       (org-md--headline-title (plist-get info :md-headline-style) 1 (org-export-data (plist-get info :title) info) nil)
       (apply orig-fun args)))))

(defun ox-md-title-add ()
  (setq org-md-toplevel-hlevel 2)
  (advice-add 'org-md-template :around #'ox-md-title--advise-template))

(defun ox-md-title-remove ()
  (setq org-md-toplevel-hlevel 1)
  (advice-remove 'org-md-template #'ox-md-title--advise-template))

(add-to-list
 'ox-extend-extensions-alist '('ox-md-title :add ox-md-title-add
                                            :remove ox-md-title-remove))

(provide 'ox-md-title)
