;;; nov.el --- Featureful EPUB reader mode

;; Copyright (C) 2017 Vasilij Schneidermann <mail@vasilij.de>

;; Author: Vasilij Schneidermann <mail@vasilij.de>
;; URL: https://depp.brause.cc/nov.el
;; Version: 0.3.4
;; Package-Requires: ((dash "2.12.0") (esxml "0.3.6") (emacs "24.4"))
;; Keywords: hypermedia, multimedia, epub

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; nov.el provides a major mode for reading EPUB documents.
;;
;; Features:
;;
;; - Basic navigation (jump to TOC, previous/next chapter)
;; - Remembering and restoring the last read position
;; - Jump to next chapter when scrolling beyond end
;; - Storing and following Org links to EPUB files
;; - Renders EPUB2 (.ncx) and EPUB3 (<nav>) TOCs
;; - Hyperlinks to internal and external targets
;; - Supports textual and image documents
;; - Info-style history navigation
;; - View source of document files
;; - Metadata display
;; - Image rescaling

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'esxml-query)
(require 'image)
(require 'shr)
(require 'url-parse)
(require 'xml)

(require 'bookmark)
(require 'imenu)
(require 'org)
(require 'recentf)

(when (not (fboundp 'libxml-parse-xml-region))
  (message "Your Emacs wasn't compiled with libxml support"))


;;; EPUB preparation

(defgroup nov nil
  "EPUB reader mode"
  :group 'multimedia)

(defcustom nov-unzip-program (executable-find "unzip")
  "Path to `unzip` executable."
  :type '(file :must-match t)
  :group 'nov)

(defcustom nov-variable-pitch t
  "Non-nil if a variable pitch face should be used.
Otherwise the default face is used."
  :type 'boolean
  :group 'nov)

(defcustom nov-text-width nil
  "Width filled text shall occupy.
An integer is interpreted as the number of columns.  If nil, use
the full window's width.  If t, disable filling completely.  Note
that this variable only has an effect in Emacs 25.1 or greater."
  :type '(choice (integer :tag "Fixed width in characters")
                 (const   :tag "Use the width of the window" nil)
                 (const   :tag "Disable filling" t))
  :group 'nov)

(defcustom nov-render-html-function 'nov-render-html
  "Function used to render HTML.
It's called without arguments with a buffer containing HTML and
should change it to contain the rendered version of it."
  :type 'function
  :group 'nov)

(defcustom nov-pre-html-render-hook nil
  "Hook run before `nov-render-html'."
  :type 'hook
  :group 'nov)

(defcustom nov-post-html-render-hook nil
  "Hook run after `nov-render-html'."
  :type 'hook
  :group 'nov)

(defcustom nov-save-place-file (locate-user-emacs-file "nov-places")
  "File name where last reading places are saved to and restored from.
If set to `nil', no saving and restoring is performed."
  :type '(choice (file  :tag "File name")
                 (const :tag "Don't save last reading places" nil))
  :group 'nov)

(defvar-local nov-file-name nil
  "Path to the EPUB file backing this buffer.")

(defvar-local nov-temp-dir nil
  "Temporary directory containing the buffer's EPUB files.")

(defvar-local nov-content-file nil
  "Path to the EPUB buffer's .opf file.")

(defvar-local nov-epub-version nil
  "Version string of the EPUB buffer.")

(defvar-local nov-metadata nil
  "Metadata of the EPUB buffer.")

(defvar-local nov-documents nil
  "Alist for the EPUB buffer's documents.
Each alist item consists of the identifier and full path.")

(defvar-local nov-documents-index 0
  "Index of the currently rendered document in the EPUB buffer.")

(defvar-local nov-toc-id nil
  "TOC identifier of the EPUB buffer.")

(defvar-local nov-history nil
  "Stack of documents user has visited.
Each element of the stack is a list (NODEINDEX BUFFERPOS).")

(defvar-local nov-history-forward nil
  "Stack of documents user has visited with `nov-history-back' command.
Each element of the stack is a list (NODEINDEX BUFFERPOS).")

(defun nov-make-path (directory file)
  "Create a path from DIRECTORY and FILE."
  (concat (file-name-as-directory directory) file))

(defun nov-directory-files (directory)
  "Return a list of files in DIRECTORY except for . and .."
  (--remove (string-match-p "/\\.\\(?:\\.\\)?\\'" it)
            (directory-files directory t)))

(defun nov-contains-nested-directory-p (directory)
  "Non-nil if DIRECTORY contain exactly one directory."
  (let* ((files (nov-directory-files directory))
         (file (car files)))
    (and (= (length files) 1)
         (file-directory-p file)
         file)))

(defun nov-unnest-directory (directory child)
  "Move contents of CHILD into DIRECTORY, then delete CHILD."
  ;; FIXME: this will most certainly fail for con/con
  (dolist (item (nov-directory-files child))
    (rename-file item directory))
  (delete-directory child))

(defun nov--fix-permissions (file-or-directory mode)
  (->> (file-modes file-or-directory)
       (file-modes-symbolic-to-number mode)
       (set-file-modes file-or-directory)))

(defun nov-fix-permissions (directory)
  "Iterate recursively through DIRECTORY to fix its files."
  (nov--fix-permissions directory "+rx")
  (dolist (file (nov-directory-files directory))
    (if (file-directory-p file)
        (nov-fix-permissions file)
      (nov--fix-permissions file "+r"))))

(defun nov-unzip-epub (directory filename)
  "Extract FILENAME into DIRECTORY.
Unnecessary nesting is removed with `nov-unnest-directory'."
  (let ((status (call-process nov-unzip-program nil "*nov unzip*" t
                              "-od" directory filename))
        child)
    (while (setq child (nov-contains-nested-directory-p directory))
      (nov-unnest-directory directory child))
    ;; HACK: unzip preserves file permissions, no matter how silly they
    ;; are, so ensure files and directories are readable
    (nov-fix-permissions directory)
    status))

(defmacro nov-ignore-file-errors (&rest body)
  "Like `ignore-errors', but for file errors."
  `(condition-case nil (progn ,@body) (file-error nil)))

(defun nov-slurp (filename &optional parse-xml-p)
  "Return the contents of FILENAME.
If PARSE-XML-P is t, return the contents as parsed by libxml."
  (with-temp-buffer
    (insert-file-contents filename)
    (if parse-xml-p
        (libxml-parse-xml-region (point-min) (point-max))
      (buffer-string))))

(defun nov-mimetype-valid-p (directory)
  "Return t if DIRECTORY contain a valid EPUB mimetype file."
  (nov-ignore-file-errors
    (let ((filename (nov-make-path directory "mimetype")))
      (equal (nov-slurp filename) "application/epub+zip"))))

(defun nov-container-filename (directory)
  "Return the container filename for DIRECTORY."
  (let ((filename (nov-make-path directory "META-INF")))
    (nov-make-path filename "container.xml")))

(defun nov-container-content-filename (content)
  "Return the content filename for CONTENT."
  (let* ((query "container>rootfiles>rootfile[media-type='application/oebps-package+xml']")
         (node (esxml-query query content)))
    (esxml-node-attribute 'full-path node)))

(defun nov-container-valid-p (directory)
  "Return t if DIRECTORY hold a valid EPUB container."
  (let ((filename (nov-container-filename directory)))
    (and filename (file-exists-p filename)
         (let* ((content (nov-slurp filename t))
                (content-file (nov-container-content-filename content)))
           (and content content-file
                (file-exists-p (nov-make-path directory content-file)))))))

(defun nov-epub-valid-p (directory)
  "Return t if DIRECTORY make up a valid EPUB document."
  (if (nov-mimetype-valid-p directory)
      (message "Invalid mimetype")
    (nov-container-valid-p directory)))

(defun nov-urldecode (string)
  "Return urldecoded version of STRING or nil."
  (and string (url-unhex-string string)))

(defun nov-content-version (content)
  "Return the EPUB version for CONTENT."
  (let* ((node (esxml-query "package" content))
         (version (esxml-node-attribute 'version node)))
    (or version
        (error "Version not specified"))))

(defun nov-content-unique-identifier-name (content)
  "Return the unique identifier name referenced in CONTENT.
This is used in `nov-content-unique-identifier' to retrieve the
the specific type of unique identifier."
  (let* ((node (esxml-query "package[unique-identifier]" content))
         (name (esxml-node-attribute 'unique-identifier node)))
    (or name
        (error "Unique identifier name not specified"))))

(defun nov-content-unique-identifier (content)
  "Return the the unique identifier for CONTENT."
  (let* ((name (nov-content-unique-identifier-name content))
         (selector (format "package>metadata>identifier[id='%s']"
                           (esxml-query-css-escape name)))
         (id (car (esxml-node-children (esxml-query selector content)))))
    (if id
        (intern id)
      (error "Unique identifier not found by its name: %s" name))))

;; NOTE: unique identifier is queried separately as identifiers can
;; appear more than once and only one of them can be the unique one
(defvar nov-required-metadata-tags '(title language)
  "Required metadata tags used for `nov-content-metadata'.")

(defvar nov-optional-metadata-tags
  '(contributor coverage creator date description format
    publisher relation rights source subject type)
  "Optional metadata tags used for 'nov-content-metadata'.")

(defun nov-content-metadata (content)
  "Return a metadata alist for CONTENT.
Required keys are 'identifier and everything in
`nov-required-metadata-tags', optional keys are in
`nov-optional-metadata-tags'."
  (let* ((identifier (nov-content-unique-identifier content))
         (candidates (mapcar (lambda (node)
                               (cons (esxml-node-tag node)
                                     (car (esxml-node-children node))))
                             (esxml-query-all "package>metadata>*" content)))
         (required (mapcar (lambda (tag)
                             (let ((candidate (cdr (assq tag candidates))))
                               (when (not candidate)
                                 ;; NOTE: this should ideally be a
                                 ;; warning, but `warn' is too obtrusive
                                 (message "Required metadatum %s not found" tag))
                               (cons tag candidate)))
                           nov-required-metadata-tags))
         (optional (mapcar (lambda (tag) (cons tag (cdr (assq tag candidates))))
                           nov-optional-metadata-tags)))
    (append `((identifier . ,identifier)) required optional)))

(defun nov-content-manifest (directory content)
  "Extract an alist of manifest files for CONTENT in DIRECTORY.
Each alist item consists of the identifier and full path."
  (mapcar (lambda (node)
            (-let [(&alist 'id id 'href href) (esxml-node-attributes node)]
              (cons (intern id)
                    (nov-make-path directory (nov-urldecode href)))))
          (esxml-query-all "package>manifest>item" content)))

(defun nov-content-spine (content)
  "Extract a list of spine identifiers for CONTENT."
  (mapcar (lambda (node) (intern (esxml-node-attribute 'idref node)))
          (esxml-query-all "package>spine>itemref" content)))

(defun nov--content-epub2-files (content manifest files)
  (let* ((node (esxml-query "package>spine[toc]" content))
         (id (or (esxml-node-attribute 'toc node)
                 (error "EPUB 2 NCX ID not found"))))
    (setq nov-toc-id (intern id))
    (let ((toc-file (or (assq nov-toc-id manifest)
                        (error "EPUB 2 NCX file not found"))))
      (cons toc-file files))))

(defun nov--content-epub3-files (content manifest files)
  (let* ((node (esxml-query "package>manifest>item[properties=nav]" content))
         (id (or (esxml-node-attribute 'id node)
                 (error "EPUB 3 <nav> ID not found"))))
    (setq nov-toc-id (intern id))
    (let ((toc-file (or (assq nov-toc-id manifest)
                        (error "EPUB 3 <nav> file not found"))))
      (setq files (--remove (eq (car it) nov-toc-id) files))
      (cons toc-file files))))

(defun nov-content-files (directory content)
  "Create correctly ordered file alist for CONTENT in DIRECTORY.
Each alist item consists of the identifier and full path."
  (let* ((manifest (nov-content-manifest directory content))
         (spine (nov-content-spine content))
         (files (mapcar (lambda (item) (assq item manifest)) spine)))
    (if (version< nov-epub-version "3.0")
        (nov--content-epub2-files content manifest files)
      (nov--content-epub3-files content manifest files))))

(defun nov--walk-ncx-node (node)
  (let ((tag (esxml-node-tag node))
        (children (--filter (eq (esxml-node-tag it) 'navPoint)
                            (esxml-node-children node))))
    (cond
     ((eq tag 'navMap)
      (insert "<ol>\n")
      (mapc 'nov--walk-ncx-node children)
      (insert "</ol>\n"))
     ((eq tag 'navPoint)
      (let ((label-node (esxml-query "navLabel>text" node))
            (content-node (esxml-query "content" node)))
        (let ((href (or (nov-urldecode (esxml-node-attribute 'src content-node))
                        (error "Navigation point is missing href attribute")))
              (label (car (esxml-node-children label-node))))
          (let ((link (format "<a href=\"%s\">%s</a>"
                              (xml-escape-string href)
                              (xml-escape-string (or label href)))))
            (if (null children)
                (insert (format "<li>\n%s\n</li>\n" link))
              (insert (format "<li>\n%s\n<ol>\n" link))
              (mapc 'nov--walk-ncx-node children)
              (insert "</ol>\n</li>\n")))))))))

(defun nov-ncx-to-html (path)
  "Convert NCX document at PATH to HTML, which is inserted into the current buffer."
  (let ((root (esxml-query "navMap" (nov-slurp path t))))
    ;; (with-temp-buffer)
    (nov--walk-ncx-node root)
    ;; (buffer-string)
    ))


;;; UI

(defvar nov-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'nov-render-document)
    (define-key map (kbd "v") 'nov-view-source)
    (define-key map (kbd "V") 'nov-view-content-source)
    (define-key map (kbd "a") 'nov-reopen-as-archive)
    (define-key map (kbd "m") 'nov-display-metadata)
    (define-key map (kbd "n") 'nov-next-document)
    (define-key map (kbd "]") 'nov-next-document)
    (define-key map (kbd "p") 'nov-previous-document)
    (define-key map (kbd "[") 'nov-previous-document)
    (define-key map (kbd "t") 'nov-goto-toc)
    (define-key map (kbd "l") 'nov-history-back)
    (define-key map (kbd "r") 'nov-history-forward)
    (define-key map (kbd "RET") 'nov-browse-url)
    (define-key map (kbd "c") 'nov-copy-url)
    (define-key map (kbd "<follow-link>") 'mouse-face)
    (define-key map (kbd "<mouse-2>") 'nov-browse-url)
    (define-key map (kbd "TAB") 'shr-next-link)
    (define-key map (kbd "M-TAB") 'shr-previous-link)
    (define-key map (kbd "<backtab>") 'shr-previous-link)
    (define-key map (kbd "SPC") 'nov-scroll-up)
    (define-key map (kbd "S-SPC") 'nov-scroll-down)
    (define-key map (kbd "DEL") 'nov-scroll-down)
    (define-key map (kbd "<home>") 'beginning-of-buffer)
    (define-key map (kbd "<end>") 'end-of-buffer)
    map))

(defun nov-clean-up ()
  "Delete temporary files of the current EPUB buffer."
  (when nov-temp-dir
    (let ((identifier (cdr (assq 'identifier nov-metadata)))
          (index (if (integerp nov-documents-index)
                     nov-documents-index
                   0)))
      (nov-save-place identifier index (point)))
    (nov-ignore-file-errors
     (delete-directory nov-temp-dir t))))

(defun nov-clean-up-all ()
  "Delete temporary files of all opened EPUB buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'nov-mode)
        (nov-clean-up)))))

(defun nov-external-url-p (url)
  "Return t if URL refers to an external document."
  (and (url-type (url-generic-parse-url url)) t))

(defun nov-url-filename-and-target (url)
  "Return a list of URL's filename and target."
  (setq url (url-generic-parse-url url))
  (mapcar 'nov-urldecode (list (url-filename url) (url-target url))))

;; adapted from `shr-rescale-image'
(defun nov-insert-image (path alt)
  "Insert an image for PATH at point, falling back to ALT.
This function honors `shr-max-image-proportion' if possible."
  (let ((type (and (null (bound-and-true-p image-transforms-p))
                   (fboundp 'imagemagick-types)
                   'imagemagick)))
    (if (not (display-graphic-p))
        (insert alt)
      (-let* (((x1 y1 x2 y2) (window-inside-pixel-edges
                              (get-buffer-window (current-buffer))))
              (image
               ;; `create-image' errors out for unsupported image types
               (ignore-errors
                 (create-image path type nil
                               :ascent 100
                               :max-width (truncate (* shr-max-image-proportion
                                                       (- x2 x1)))
                               :max-height (truncate (* shr-max-image-proportion
                                                        (- y2 y1)))))))
        (if image
            (insert-image image)
          (insert alt))))))

(defvar nov-original-shr-tag-img-function
  (symbol-function 'shr-tag-img))

(defun nov-render-img (dom &optional url)
  "Custom <img> rendering function for DOM.
Uses `shr-tag-img' for external paths and `nov-insert-image' for
internal ones."
  (let ((url (or url (cdr (assq 'src (cadr dom)))))
        (alt (or (cdr (assq 'alt (cadr dom))) "")))
    (if (nov-external-url-p url)
        ;; HACK: avoid hanging in an infinite loop when using
        ;; `cl-letf' to override `shr-tag-img' with a function that
        ;; might call `shr-tag-img' again
        (funcall nov-original-shr-tag-img-function dom url)
      (setq url (expand-file-name (nov-urldecode url)))
      (nov-insert-image url alt))))

(defun nov-render-title (dom)
  "Custom <title> rendering function for DOM.
Sets `header-line-format' to a combination of the EPUB title and
chapter title."
  (let ((title (cdr (assq 'title nov-metadata)))
        (chapter-title (or (car (esxml-node-children dom))
                           '(:propertize "No title" face italic))))
    ;; this shouldn't happen for properly authored EPUBs
    (when (not title)
      (setq title '(:propertize "No title" face italic)))
    (setq header-line-format (list title ": " chapter-title))))

(defvar nov-shr-rendering-functions
  '(;; default function uses url-retrieve and fails on local images
    (img . nov-render-img)
    ;; titles are rendered *inside* the document by default
    (title . nov-render-title))
  "Alist of rendering functions used with `shr-render-region'.")

(defun nov-render-html ()
  "Render HTML in current buffer with shr."
  (run-hooks 'nov-pre-html-render-hook)
  (let (;; HACK: make buttons use our own commands
        (shr-map nov-mode-map)
        (shr-external-rendering-functions nov-shr-rendering-functions)
        (shr-use-fonts nov-variable-pitch))
    ;; HACK: `shr-external-rendering-functions' doesn't cover
    ;; every usage of `shr-tag-img'
    (cl-letf (((symbol-function 'shr-tag-img) 'nov-render-img))
      (if (eq nov-text-width t)
          (cl-letf (((symbol-function 'shr-fill-line) 'ignore))
            (shr-render-region (point-min) (point-max)))
        (let ((shr-width nov-text-width))
          (shr-render-region (point-min) (point-max))))))
  (run-hooks 'nov-post-html-render-hook))

(defun nov-render-document ()
  "Render the document referenced by `nov-documents-index'.
If the document path refers to an image (as determined by
`image-type-file-name-regexps'), an image is inserted, otherwise
the HTML is rendered with `nov-render-html-function'."
  (interactive)
  (-let* (((id . path) (aref nov-documents nov-documents-index))
          ;; HACK: this should be looked up in the manifest
          (imagep (--find (string-match-p (car it) path)
                          image-type-file-name-regexps))
          ;; NOTE: allows resolving image references correctly
          (default-directory (file-name-directory path))
          (buffer-read-only nil))
    (erase-buffer)

    (cond
     (imagep
      (nov-insert-image path ""))
     ((and (version< nov-epub-version "3.0")
           (eq id nov-toc-id)
           ;; DONE: skip conversion to html if ncx is too big
           (< (nth 7 (file-attributes path))
              (* 100 1024)))
      (nov-ncx-to-html path))
     (t
      (insert (nov-slurp path))))

    (when (not imagep)
      (funcall nov-render-html-function))
    (goto-char (point-min))))

(defun nov-find-document (predicate)
  "Return first item in `nov-documents' PREDICATE is true for."
  (cl-loop for i from 0
           for document in nov-documents
           when (funcall predicate document)
           return i))

(defun nov-goto-document (index)
  "Go to the document denoted by INDEX."
  (let ((history (cons (list nov-documents-index (point))
                       nov-history)))
    (setq nov-documents-index index)
    (nov-render-document)
    (setq nov-history history)))

(defun nov-goto-toc ()
  "Go to the TOC index and render the TOC document."
  (interactive)
  (let ((index (nov-find-document (lambda (doc) (eq (car doc) nov-toc-id)))))
    (when (not index)
      (error "Couldn't locate TOC"))
    (nov-goto-document index)))

(defun nov-view-source ()
  "View the source of the current document in a new buffer."
  (interactive)
  (find-file (cdr (aref nov-documents nov-documents-index))))

(defun nov-view-content-source ()
  "View the source of the content file in a new buffer."
  (interactive)
  (find-file nov-content-file))

(defun nov-reopen-as-archive ()
  "Reopen the EPUB document using `archive-mode'."
  (interactive)
  (with-current-buffer (find-file-literally nov-file-name)
    (archive-mode)))

(defun nov-display-metadata ()
  "View the metadata of the EPUB document in a new buffer."
  (interactive)
  (let ((buffer "*EPUB metadata*")
        (metadata nov-metadata)
        (version nov-epub-version))
    (with-current-buffer (get-buffer-create buffer)
      (special-mode)
      (let (buffer-read-only)
        (erase-buffer)
        (insert (format "EPUB Version: %s\n" version))
        (dolist (item metadata)
          (-let [(key . value) item]
            (insert (format "%s: " (capitalize (symbol-name key))))
            (if value
                (if (eq key 'description)
                    (let ((beg (point)))
                      (insert value)
                      (shr-render-region beg (point)))
                  (insert (format "%s" value)))
              (insert (propertize "None" 'face 'italic)))
            (insert "\n")))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun nov-next-document ()
  "Go to the next document and render it."
  (interactive)
  (when (< nov-documents-index (1- (length nov-documents)))
    (nov-goto-document (1+ nov-documents-index))))

(defun nov-previous-document ()
  "Go to the previous document and render it."
  (interactive)
  (when (> nov-documents-index 0)
    (nov-goto-document (1- nov-documents-index))))

(defun nov-scroll-up ()
  "Scroll with `scroll-up' or visit next chapter if at bottom."
  (interactive)
  (message "%s %s" (point) (point-max))
  (if (= (point) (point-max))
      (nov-next-document)
    (if (< (window-end) (point-max))
        (scroll-up 10)
      (message "At document end.")
      (goto-char (point-max)))))

(defun nov-scroll-down (arg)
  "Scroll with `scroll-down' or visit previous chapter if at top."
  (interactive "P")
  (if (and (<= (window-start) (point-min))
           (> nov-documents-index 0))
      (progn
        (nov-previous-document)
        (goto-char (point-max)))
    (scroll-down arg)))

(defun nov-visit-relative-file (filename target)
  "Visit the document as specified by FILENAME and TARGET."
  (let (index)
    (when (not (zerop (length filename)))
      (let* ((current-path (cdr (aref nov-documents nov-documents-index)))
             (directory (file-name-directory current-path))
             (path (file-truename (nov-make-path directory filename)))
             (match (nov-find-document
                     (lambda (doc) (equal path (file-truename (cdr doc)))))))
        (when (not match)
          (error "Couldn't locate document"))
        (setq index match)))
    (let ((shr-target-id target)) ; shr.el API for Emacs 27.1 and older
      (nov-goto-document (or index nov-documents-index))))
  (when target
    (let ((pos (point-min))
          done)
      (while (and (not done)
                  (setq pos (next-single-property-change pos 'shr-target-id)))
        (when (equal (get-text-property pos 'shr-target-id) target)
          (goto-char pos)
          (recenter (1- (max 1 scroll-margin)))
          (setq done t)))
      (when (not done)
        (error "Couldn't locate target")))))

;; adapted from `shr-browse-url'
(defun nov-browse-url (&optional mouse-event)
  "Follow an external url with `browse-url'.
Internal URLs are visited with `nov-visit-relative-file'."
  (interactive (list last-nonmenu-event))
  (mouse-set-point mouse-event)
  (let ((url (get-text-property (point) 'shr-url)))
    (when (not url)
      (user-error "No link under point"))
    (if (nov-external-url-p url)
        (browse-url url)
      (apply 'nov-visit-relative-file (nov-url-filename-and-target url)))))

(defun nov-copy-url (&optional mouse-event)
  (interactive (list last-nonmenu-event))
  (mouse-set-point mouse-event)
  (let ((url (get-text-property (point) 'shr-url)))
    (when (not url)
      (user-error "No link under point"))
    (kill-new url)
    (message "%s" url)))

(defun nov-saved-places ()
  "Retrieve saved places in `nov-save-place-file'."
  (when (and nov-save-place-file (file-exists-p nov-save-place-file))
    (with-temp-buffer
      (insert-file-contents-literally nov-save-place-file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun nov-saved-place (identifier)
  "Retrieve saved place for IDENTIFIER in `nov-saved-place-file'."
  (cdr (assq identifier (nov-saved-places))))

(defun nov-save-place (identifier index point)
  "Save place as identified by IDENTIFIER, INDEX and POINT.
Saving is only done if `nov-save-place-file' is set."
  (when nov-save-place-file
    (let* ((place `(,identifier (index . ,index)
                                (point . ,point)))
           (places (cons place (assq-delete-all identifier (nov-saved-places))))
           print-level
           print-length)
      (with-temp-file nov-save-place-file
        (insert (prin1-to-string places))))))

(defun nov--index-valid-p (documents index)
  (and (integerp index)
       (>= index 0)
       (< index (length documents))))

(defun nov-history-back ()
  "Go back in the history to the last visited document."
  (interactive)
  (or nov-history
      (user-error "This is the first document you looked at"))
  (-let ((history-forward
          (cons (list nov-documents-index (point))
                nov-history-forward))
         ((index opoint) (car nov-history)))
    (setq nov-history (cdr nov-history))
    (nov-goto-document index)
    (setq nov-history (cdr nov-history))
    (setq nov-history-forward history-forward)
    (goto-char opoint)
    (recenter (1- (max 1 scroll-margin)))))

(defun nov-history-forward ()
  "Go forward in the history of visited documents."
  (interactive)
  (or nov-history-forward
      (user-error "This is the last document you looked at"))
  (-let ((history-forward (cdr nov-history-forward))
         ((index opoint) (car nov-history-forward)))
    (nov-goto-document index)
    (setq nov-history-forward history-forward)
    (goto-char opoint)
    (recenter (1- (max 1 scroll-margin)))))

;;;###autoload
(define-derived-mode nov-mode special-mode "EPUB"
  "Major mode for reading EPUB documents"
  (add-hook 'kill-buffer-hook 'nov-clean-up nil t)
  (add-hook 'kill-emacs-hook 'nov-clean-up-all)
  (add-hook 'change-major-mode-hook 'nov-clean-up nil t)
  (when (not buffer-file-name)
    (error "EPUB must be associated with file"))
  (setq nov-temp-dir (make-temp-file "nov-" t ".epub"))
  (let ((exit-code (nov-unzip-epub nov-temp-dir buffer-file-name)))
    (when (not (integerp exit-code))
      (nov-clean-up)
      (error "EPUB extraction aborted by signal %s" exit-code))
    (when (> exit-code 1) ; exit code 1 is most likely a warning
      (nov-clean-up)
      (error "EPUB extraction failed with exit code %d (see *nov unzip* buffer)"
             exit-code)))
  (when (not (nov-epub-valid-p nov-temp-dir))
    (nov-clean-up)
    (error "Invalid EPUB file"))
  (let* ((content (nov-slurp (nov-container-filename nov-temp-dir) t))
         (content-file (->> (nov-container-content-filename content)
                            (nov-make-path nov-temp-dir)))
         (work-dir (file-name-directory content-file))
         (content (nov-slurp content-file t)))
    (setq nov-content-file content-file)
    (setq nov-epub-version (nov-content-version content))
    (setq nov-metadata (nov-content-metadata content))
    (setq nov-documents (->> (nov-content-files work-dir content)
                             (apply 'vector)))
    (setq nov-documents-index 0))
  (setq buffer-undo-list t)
  (setq nov-file-name (buffer-file-name))
  (setq-local bookmark-make-record-function
              'nov-bookmark-make-record)
  (set-visited-file-name nil t) ; disable autosaves and save questions
  (let ((place (nov-saved-place (cdr (assq 'identifier nov-metadata)))))
    (if place
        (let ((index (cdr (assq 'index place)))
              (point (cdr (assq 'point place))))
          (if (nov--index-valid-p nov-documents index)
              (progn
                (setq nov-documents-index index)
                (nov-render-document)
                (goto-char point))
            (warn "Couldn't restore last position")
            (nov-render-document)))
      (nov-render-document))))

(defun nov-run-hooks ()
  (interactive)
  (run-hooks 'nov-mode-hook))


;;; recentf interop

(defun nov-add-to-recentf ()
  "Add real path to recentf list if possible."
  (when nov-file-name
    (recentf-add-file nov-file-name)))

(add-hook 'nov-mode-hook 'nov-add-to-recentf)
(add-hook 'nov-mode-hook 'hack-dir-local-variables-non-file-buffer)


(defun nov--find-file (file index point)
  "Open FILE(nil means current buffer) in nov-mode and go to the specified INDEX and POINT."
  (when file
    (find-file file))
  (unless (eq major-mode 'nov-mode)
    (nov-mode))
  (when (not (nov--index-valid-p nov-documents index))
    (error "Invalid documents index"))
  (setq nov-documents-index index)
  (nov-render-document)
  (goto-char point))

;; Bookmark interop
(defun nov-bookmark-make-record  ()
  "Create a bookmark epub record."
  (cons (buffer-name)
        `((filename . ,nov-file-name)
          (index . ,nov-documents-index)
          (position . ,(point))
          (handler . nov-bookmark-jump-handler))))

;;;###autoload
(defun nov-bookmark-jump-handler (bmk)
  "The bookmark handler-function interface for bookmark BMK.

See also `nov-bookmark-make-record'."
  (let ((file (bookmark-prop-get bmk 'filename))
        (index (bookmark-prop-get bmk 'index))
        (position (bookmark-prop-get bmk 'position)))
    (nov--find-file file index position)))


;;; Org interop

(defun nov-org-link-follow (path)
  "Follow nov: link designated by PATH."
  (if (string-match "^\\(.*\\)::\\([0-9]+\\):\\([0-9]+\\)$" path)
      (let ((file (match-string 1 path))
            (index (string-to-number (match-string 2 path)))
            (point (string-to-number (match-string 3 path))))
        (nov--find-file file index point))
    (error "Invalid nov.el link")))

(defun nov-org-link-store ()
  "Store current EPUB location as nov: link."
  (when (and (eq major-mode 'nov-mode) nov-file-name)
    (when (not (integerp nov-documents-index))
      (setq nov-documents-index 0))
    (let ((org-store-props-function
           (if (fboundp 'org-link-store-props)
               'org-link-store-props
             'org-store-link-props))
          (link (format "nov:%s::%d:%d"
                        nov-file-name
                        nov-documents-index
                        (point)))
          (description (format "EPUB file at %s" nov-file-name)))
      (funcall org-store-props-function
               :type "nov"
               :link link
               :description description))))

(cond
 ((fboundp 'org-link-set-parameters)
  (org-link-set-parameters
   "nov"
   :follow 'nov-org-link-follow
   :store 'nov-org-link-store))
 ((fboundp 'org-add-link-type)
  (org-add-link-type "nov" 'nov-org-link-follow)
  (add-hook 'org-store-link-functions 'nov-org-link-store)))


;;; Imenu interop

(defun nov-imenu-goto-function (_name filename target)
  "Visit imenu item using FILENAME and TARGET."
  (nov-visit-relative-file filename target))

(defun nov-imenu-create-index ()
  "Generate Imenu index."
  (require 'esxml)
  (let* ((toc-path (cdr (aref nov-documents 0)))
         (ncxp (version< nov-epub-version "3.0"))
         (toc (with-temp-buffer
                (if ncxp
                    (nov-ncx-to-html toc-path)
                  (insert-file-contents toc-path))
                (libxml-parse-html-region (point-min) (point-max)))))
    (mapcar
     (lambda (node)
       (-let* ((href (esxml-node-attribute 'href node))
               (label (mapconcat 'string-trim-whitespace
                                 (esxml-find-descendants #'stringp node) " "))
               ((filename target) (nov-url-filename-and-target href)))
         (list label filename 'nov-imenu-goto-function target)))
     (esxml-query-all "a" toc))))

(defun nov-imenu-setup ()
  (setq imenu-create-index-function 'nov-imenu-create-index))
(add-hook 'nov-mode-hook 'nov-imenu-setup)

(provide 'nov)
;;; nov.el ends here
