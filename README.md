
# ox-md-title: Document titles for ox-md.el

[Ox-md-title.el](https://github.com/jeffkreeftmeijer/ox-md-title.el) adds document titles to markdown files generated with ox-md and derivatives.

Ox-md-title is disabled by default, even after requiring and enabling the library. It only adds titles to Markdown export when `org-md-title` is non-nil:

```emacs-lisp
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
```

The package works by advising two functions. First, it advises `org-md-template` by prepending the document title. The title is built by calling out to `org-md--headline-title`<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup> with the headline style and title extracted from the `info` variable:

```emacs-lisp
(defun org-md-title--advise-template (orig-fun &rest args)
  (concat
   (when org-md-title
     (let* ((info (nth 1 args))
	    (style (plist-get info :md-headline-style))
	    (title (org-export-data (plist-get info :title) info)))
       (org-md--headline-title style 0 title nil)))
   (apply orig-fun args)))
```

Because a new title is prepended to the document, any already-existing headlines need their levels bumped up. The second advice intercepts calls to `org-md--headline-title` and increments the second parameter before calling the original function:

```emacs-lisp
(defun org-md-title--advise-headline (args)
  (when org-md-title
    (setf (nth 1 args) (+ (nth 1 args) 1)))
  args)
```

Finally, the added functions are added as advice:

```emacs-lisp
(defun org-md-title-add ()
  (advice-add 'org-md--headline-title :filter-args #'org-md-title--advise-headline)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (advice-remove 'org-md--headline-title #'org-md-title--advise-headline)
  (advice-remove 'org-md-template #'org-md-title--advise-template))
```


## Usage

Call `org-md-title-add` to add ox-md-title's advice before exporting<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup>, and call `org-md-title-remove` after:

```emacs-lisp
(use-package ox-gfm)
(require 'ox-md-title)
(org-md-title-add)

(let ((org-md-title t))
  (org-gfm-export-to-markdown))
```

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> This package advises the private `org-md--headline-title` instead of `org-md-headline`, because the latter requires an actual Org headline element, which does not exist.

<sup><a id="fn.2" class="footnum" href="#fnr.2">2</a></sup> This example uses [ox-gfm](https://github.com/larstvei/ox-gfm), which works because it's a backend derived from ox-md.