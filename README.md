
# ox-md-title: Document titles for ox-md.el

[Ox-md-title.el](https://github.com/jeffkreeftmeijer/ox-md-title.el) adds document titles to Markdown files generated with ox-md and derivatives.


## Introduction

Org documents often have their document titles set in a `+#title` export setting at the start of the file. The title is used when exporting to other formats. For example, when exporting to HTML, the title is used in the resulting document's `<title>` and `<h1>` tags.

Markdown doesn't have an equivalent to Org's titles. Instead, it's common to add a top-level headline to the start of the document.

[Ox-md](https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/lisp/ox-md.el), the Markdown exporter shipped with Org mode, adheres to Markdown's lack of explicit titles.<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup> Even though Org documents can have titles through the title export setting, ox-md produces Markdown files with the titles omitted. For example, the current document has export settings, including a title:

```org
#+title: ox-md-title: Document titles for ox-md.el
#+author: Jeff Kreeftmeijer
#+date: 2023-03-10
#+options: toc:nil

[[https://github.com/jeffkreeftmeijer/ox-md-title.el][Ox-md-title.el]] adds document titles to Markdown files generated with ox-md and derivatives.

* Introduction
```

Exporting with `org-md-export-as-markdown` produces a Markdown document without a title headline:

```markdown
[Ox-md-title.el](https://github.com/jeffkreeftmeijer/ox-md-title.el) adds document titles to Markdown files generated with ox-md and derivatives.


# Introduction
```

Ox-md-title deviates from the Markdown standard and adds document titles to behave like the other org exporters. Once enabled, it adds the document title in front of the exported document, and shifts the sub headlines one level down:

```markdown

# ox-md-title: Document titles for ox-md.el

[Ox-md-title.el](https://github.com/jeffkreeftmeijer/ox-md-title.el) adds document titles to Markdown files generated with ox-md and derivatives.


## Introduction
```


## Implementation

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

The package works by advising two functions. First, it advises `org-md-template` by prepending the document title. The title is built by calling out to `org-md--headline-title` with the headline style and title extracted from the `info` variable:

```emacs-lisp
(defun org-md-title--advise-template (orig-fun &rest args)
  (concat
   (when org-md-title
     (let* ((info (nth 1 args))
	    (style (plist-get info :md-headline-style))
	    (title (plist-get info :title))
	    (subtitle (plist-get info :subtitle)))
       (concat
	(when title
	  (org-md--headline-title style 1 (org-export-data title info) nil))
	(when subtitle
	  (org-md--headline-title style 2 (org-export-data subtitle info) nil)))))
   (apply orig-fun args)))
```

Because a new title is prepended to the document, any already-existing headlines need their levels bumped up. The second advice intercepts calls to `org-export-get-relative-level`, which is the internal function the export backends use to determine the level for the current headline. It increments the headline level by one if `org-md-title` is enabled and if the current document has a title set:

```emacs-lisp
(defun org-md-title--advise-level (orig-fun headline info)
  (+ (funcall orig-fun headline info)
     (if (and org-md-title (plist-get info :title))
	 1
       0)))
```

Finally, the added functions are added as advice:

```emacs-lisp
(defun org-md-title-add ()
  (advice-add 'org-export-get-relative-level :around #'org-md-title--advise-level)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (advice-remove 'org-export-get-relative-level #'org-md-title--advise-level)
  (advice-remove 'org-md-template #'org-md-title--advise-template))
```


## Usage

Ox-md-title is currently not available through any of the package registries. Instead, install it from the repository direcly. Install the package with [use-package](https://github.com/jwiegley/use-package) and [straight.el](https://github.com/radian-software/straight.el), and enable it by calling `org-md-title-add`:

```emacs-lisp
(use-package ox-md-title
  :straight
  (ox-md-title :type git :host github :repo "jeffkreeftmeijer/ox-md-title.el")
  :config
  (org-md-title-add))
```

Alternatively, download `ox-md-title.el` and require it manually:

```emacs-lisp
(require 'ox-md-title)
(org-md-title-add)
```

After calling `org-md-title-add`, set thte `org-md-title` variable to add document titles when exporting with ox-md:

```emacs-lisp
(let ((org-md-title t))
  (org-markdown-export-to-markdown))
```

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> A [patch to add titles](https://lists.gnu.org/archive/html/emacs-orgmode/2017-08/msg00553.html) was rejected to keep ox-md compatible with standard Markdown. Instead of adding support for titles in the main implementation, it's suggested that features like this should be implemented in more specific backends:

> The point of "md" export back-end is not to provide the same features as full-fledged ones like "latex" or "html". I wrote it to take care of the boring stuff of markdown syntax. Anyone willing to write a back-end with a different Markdown flavour just needs to concentrate of the differences between the original syntax.