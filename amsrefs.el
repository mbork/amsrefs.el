;;; amsrefs.el -- a major mode to assist converting hand-made
;;; bibliographies to amsrefs-compatible ones.

;; amsrefs mode
;;
;; This should probably be a minor mode or something.
(define-derived-mode amsrefs-mode LaTeX-mode "AMSRefs"
  "Major mode for editing amsrefs entries.
\\{amsrefs-mode-map}"
  (define-key amsrefs-mode-map (kbd "<C-tab>") 'move-to-next-amsrefs-field)
  (define-key amsrefs-mode-map (kbd "<C-S-iso-lefttab>") 'move-to-previous-amsrefs-field)
  (define-key amsrefs-mode-map (kbd "C-c C-a") 'convert-region-to-amsrefs-field)
  (define-key amsrefs-mode-map (kbd "C-c C-i") 'amsrefs-insert-entry-template)
  (define-key amsrefs-mode-map (kbd "C-c [") 'amsrefs-insert-entry-template-beginning)
  (define-key amsrefs-mode-map (kbd "C-c ]") 'amsrefs-insert-entry-template-end)
  (define-key amsrefs-mode-map (kbd "C-c C-c") 'LaTeX-mode))

; A list of amsrefs field names (for completion)
(defcustom amsrefs-field-names
  '("author"
    "editor"
    "translator"
    "contribution"
    "isbn"
    "issn"
    "review"
    "partial"
    "address"
    "book"
    "booktitle"
    "conference"
    "copula"
    "date"
    "doi"
    "edition"
    "eprint"
    "fulljournal"
    "hyphenation"
    "institution"
    "journal"
    "label"
    "language"
    "name"
    "note"
    "number"
    "organization"
    "pages"
    "part"
    "place"
    "publisher"
    "reprint"
    "school"
    "series"
    "setup"
    "status"
    "subtitle"
    "title"
    "translation"
    "type"
    "url"
    "volume"
    "xref"
    "year"
    "transition")
  "Alist (with strings only!) for completion in convert-region-to-amsrefs-field")

; A list of amsrefs entry types (for completion)
(defcustom amsrefs-entry-types
  '(("article" . ("author" "title" "journal" "volume" "number" "date" "pages"))
    ("partial" . ("author"))
    ("contribution" . ("author"))
    ("book" . ("author" "title" "publisher" "place" "date"))
    ("collection.article" . ("author"))
    ("conference" . ("author"))
    ("innerbook" . ("author"))
    ("report" . ("author"))
    ("thesis" . ("author"))
    ("periodical" . ("author"))
    ("collection" . ("author"))
    ("proceedings" . ("author"))
    ("manual" . ("author"))
    ("miscellaneous" . ("author"))
    ("misc" . ("author"))
    ("unpublished" . ("author"))
    ("proceedings.article" . ("author"))
    ("techreport" . ("author")))
  "Alist for completion in amsrefs-insert-entry-template.  CDR of
each entry is a list of default fields.")

(defvar amsrefs-field-beginning-regexp "^[ \t]*[a-z]+={"
  "Regexp matching the beginning of an amsrefs field.")

(defun amsrefs-insert-entry-template (label type)
  "Inserts a template of given type for an amsrefs entry."
  (interactive (list (read-from-minibuffer "amsrefs entry label: ")
		     (completing-read "amsrefs entry type: "
				      amsrefs-entry-types
				      nil
				      t)))
  (insert "  \\bib{" label "}{" type "}{")
  (save-excursion
    (mapcar (lambda (name)
	      (convert-region-to-amsrefs-field (point) (point) name)
	      (forward-char 2))
	    (cdr (assoc type amsrefs-entry-types)))
    (insert "\n  }\n"))
  (forward-line)
  (end-of-line)
  (backward-char 2))

(defun amsrefs-insert-entry-template-beginning (label type)
  "Inserts the beginning of a template of given type for an
amsrefs entry."
  (interactive (list (read-from-minibuffer "amsrefs entry label: ")
		     (completing-read "amsrefs entry type: "
				      amsrefs-entry-types
				      nil
				      t)))
  (insert "  \\bib{" label "}{" type "}{\n    "))

(defun amsrefs-insert-entry-template-end ()
  "End the amsrefs entry."
  (interactive)
  (insert "\n  }"))

(defun move-to-next-amsrefs-field ()
  "Move to the next match for AMSREFS-FIELD-BEGINNING-REGEXP."
  (interactive)
  (save-match-data
    (re-search-forward amsrefs-field-beginning-regexp nil t)))

(defun move-to-previous-amsrefs-field ()
  "Move to the previous match for AMSREFS-FIELD-BEGINNING-REGEXP
and put the point after it."
  (interactive)
  (save-match-data
    (re-search-backward amsrefs-field-beginning-regexp nil t
			(if (looking-back amsrefs-field-beginning-regexp) 2))
    (goto-char (match-end 0))))

(defun convert-region-to-amsrefs-field (beg end name)
  "Convert region to an amsrefs field.  If there is no region,
put an empty amsrefs field template at point."
  (interactive
   (let ((name (completing-read "amsrefs field name: "
				amsrefs-field-names
				nil  ; predicate
				t))) ; require-match
     (if (use-region-p)
	 (list (region-beginning) (region-end) name)
       (list (point) (point) name))))
  (let ((region-p (/= beg end))
	(end-mark (make-marker)))
    (goto-char end)
    (insert "},")
    (set-marker end-mark (point))
    (goto-char beg)
    (if (looking-back "^[ \t]*")
	(let ((opoint (point)))
	  (beginning-of-line)
	  (delete-region (point) opoint)
	  (insert "    "))
      (insert "\n    "))
    (insert name "={")
    (if region-p (goto-char end-mark))))



;;; Semi-automatic conversion of hand-made bibliographies
(defcustom amsrefs-convert-bib-patterns-alist
  '(("\\(?:[.,][ \t]*\\|\n\\)\n+"
     . (amsrefs-perform-replace "  }\n"))
    ("[.,;]?[ \t\n]*\\(?:and \\)?\\([A-ZĄĆĘŁŃÓŚŹŻ]\\w?\\.\\)[ \t]*\\([A-ZĄĆĘŁŃÓŚŹŻ]\\w+\\)"
     . (amsrefs-perform-replace "    author={\\2, \\1},\n"))
    ("[.,;]?[ \t\n]*\\(?:and \\)?\\([A-ZĄĆĘŁŃÓŚŹŻ]\\w?\\.[- \t]*[A-ZĄĆĘŁŃÓŚŹŻ]\\w*\\.+\\)[, \t]*\\([A-ZĄĆĘŁŃÓŚŹŻ]\\w+\\)"
     . (amsrefs-perform-replace "    author={\\2, \\1},\n"))
    ("[.,;]?[ \t\n]*\\(?:and \\)?\\([A-ZĄĆĘŁŃÓŚŹŻ]\\w+\\)[ \t]*\\([A-ZĄĆĘŁŃÓŚŹŻ]\\w+\\)"
     . (amsrefs-perform-replace "    author={\\2, \\1},\n"))
					;    ("\\(?:\\.\\|,\\)?[ \t\n]+"
					;     . (amsrefs-perform-replace ""))
    ("^[ \t\n]*\\\\bibitem\\(?:\\[.*?]\\)?{\\([-_a-zA-Z0-9]+\\)}"
     . (amsrefs-perform-replace "  \\\\bib{\\1}{article}{\n"))
    ("^[ \t\n]*\\\\bibitem\\(?:\\[.*?]\\)?{\\([-_a-zA-Z0-9]+\\)}"
     . (amsrefs-perform-replace "  \\\\bib{\\1}{book}{\n"))
    ("^[ \t\n]*\\\\bibitem\\(?:\\[.*?]\\)?{\\([-_a-zA-Z0-9]+\\)}"
     . (amsrefs-perform-replace "  \\\\bib{\\1}{manual}{\n"))
    ("^[ \t\n]*\\\\item\\[\\([-_a-zA-Z0-9]+\\)]"
     . (amsrefs-perform-replace "  \\\\bib{\\1}{article}{\n"))
    ("^[ \t\n]*\\\\item\\[\\([-_a-zA-Z0-9]+\\)]"
     . (amsrefs-perform-replace "  \\\\bib{\\1}{book}{\n"))
    ("^[ \t\n]*\\\\item\\[\\([-_a-zA-Z0-9]+\\)]"
     . (amsrefs-perform-replace "  \\\\bib{\\1}{manual}{\n"))
    ("^\\\\item\\[\\\\mbox{\\[\\([-a-zA-Z0-9]+\\)][ ]?}][ ]" ; specjalnie dla Maligrandy!
     . (amsrefs-perform-replace "  \\\\bib{\\1}{article}{\n"))
					;    ("^[ \t\n]*\\\\bibitem\\(?:\\[.*?]\\)?{\\([-a-z0-9]+\\)}"
					;     . (amsrefs-perform-replace "  \\\\bib{\\1}{book}{\n"))
    ("^\\\\item\\[\\\\mbox{\\[\\([-a-zA-Z0-9]+\\)][ ]?}][ ]" ; specjalnie dla Maligrandy!
     . (amsrefs-perform-replace "  \\\\bib{\\1}{book}{\n"))
    ("^\\\\item\\[\\\\mbox{\\[\\([-a-zA-Z0-9]+\\)][ ]?}][ ]" ; specjalnie dla Maligrandy!
     . (amsrefs-perform-replace "  \\\\bib{\\1}{manual}{\n"))
    ("[ \t\n]*[.,;]?[ \t\n]*\\([0-9]+\\) ?--? ?\\([0-9]+\\)"
     . (amsrefs-perform-replace "    pages={\\1--\\2},\n"))
    ("[ \t\n]*[.,;]?[ \t\n]*(?\\([0-9]\\{4\\}\\))?"
     . (amsrefs-perform-replace "    date={\\1},\n"))
    ("[ \t\n]*[.,;]?[ \t\n]*(?\\([0-9]+\\))?"
     . (amsrefs-perform-replace "    volume={\\1},\n"))
    ("[ \t\n]*[.,;]?[ \t\n]*(?\\([-0-9]+\\))?"
     . (amsrefs-perform-replace "    number={\\1},\n"))
    ("[ \t\n]*[.,;]?[ \t\n]*\\(?:\\\\textbf{\\|{\\\\bf[ \t\n]+\\)\\([0-9]+\\)}"
     . (amsrefs-perform-replace "    volume={\\1},\n"))
    ("[ \t\n]*[.,;]?[ \t\n]*n[or]\\.?[ \t]+\\([-0-9]+\\)"
     . (amsrefs-perform-replace "    number={\\1},\n"))
    ("[ \t\n]*[.,;]?[ \t\n]*\\(?:\\\\emph{\\|\\\\textit{\\|{\\\\em[ \t\n]+\\|{\\\\it[ \t\n]+\\)"
     . (amsrefs-convert-to-field
	"[ \t\n]*[.,;]?[ \t\n]*\\(?:\\\\emph{\\|\\\\textit{\\|{\\\\em[ \t\n]+\\|{\\\\it[ \t\n]+\\)"
	"}"))
    ("[ \t\n]*[.,;]?[ \t\n]*"
     . (amsrefs-convert-to-field "[ \t\n]*[.,;]?[ \t\n]*" "[, \t\n]"))
    )
  "An alist of regexps to match and functions to apply when matched.
Each function should return NIL if unsuccesful and place the
point after the replacement if succesful.")

(defvar amsrefs-bib-overlay
  (let ((ovl (make-overlay 0 0)))
    (overlay-put ovl 'face 'query-replace)
    ovl))

(defmacro with-local-overlay (ovl beg end &rest body)
  `(progn
    (move-overlay ,ovl ,beg ,end (current-buffer))
    (let ((inhibit-quit t))
      (prog1
	  (with-local-quit ,@body)
	(delete-overlay ,ovl)))))

(defun looking-at-case-sensitive (&rest args)
  "A non-case-folding version of LOOKING-AT."
  (let ((case-fold-search nil))
    (apply 'looking-at args)))

(defun amsrefs-perform-replace (to &optional no-query)
  "Ask the user if it should convert FROM to TO, based on the match
data."
  (with-local-overlay amsrefs-bib-overlay (match-beginning 0) (match-end 0)
		      (when (or
			     no-query
			     (y-or-n-p
			      (format "Convert this part to \"%s\"? "
				      (save-match-data (replace-regexp-in-string
							"\n"
							"\\n"
							(match-substitute-replacement to t)
							t
							t)))))
			(undo-boundary)
			(replace-match to t)
			t)))

(defmacro amsrefs-convert-bib-macro (patterns)
  `(cond ,@(mapcar (lambda (pat) `((if (looking-at-case-sensitive ,(car pat))
				       ,(cdr pat))))
		   (symbol-value patterns)))) ; replace-match returns nil -- why???!!!

(defun amsrefs-convert-bib ()
  "Convert bib entries, starting at point."
  (interactive)
  (while (amsrefs-convert-bib-macro
	  amsrefs-convert-bib-patterns-alist))
  (message "No match at point, exiting."))

(defun amsrefs-convert-to-field-with-stripping (beg end prefix suffix name)
  "Convert the text between BEG and END to an amsrefs field named
NAME, stripping PREFIX and SUFFIX."
  (let ((field (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (if (string-match prefix field)
	(setq field (replace-match "" t t field)))
    (let ((index (length field)))
      (unless (zerop index)
	(while (not (string-match suffix field index))
	  (decf index))
	(setq field (replace-match "" t t field))))
    (insert "    " name "={" field "},\n")
    (undo-boundary)))

(defun amsrefs-convert-to-field (prefix suffix)
  "Interactively converts the text beginning at point to a
suitable amsrefs field.  Strips the PREFIX and SUFFIX from the
beginning and end of the text respectively."
  (let ((beg (point)) end (ext 1) (done nil))
    (if (and (looking-at prefix)
	     (setq end (re-search-forward suffix nil t)))
	(with-local-overlay
	 amsrefs-bib-overlay beg end
	 (while (let ((input (read-key
			      "Press a letter to convert (h for help): ")))
		  (cond
		   ((eql input ?\s)
		    (if (re-search-forward suffix nil t)
			(progn (incf ext)
			       (setq end (point))
			       (move-overlay
				amsrefs-bib-overlay beg end))
		      (message "Cannot extend further")
		      (sit-for 1)))
		   ((eql input ?\d)
		    (if (re-search-backward suffix beg t 2)
			(progn (decf ext)
			       (goto-char (match-end 0))
			       (setq end (point))
			       (move-overlay
				amsrefs-bib-overlay beg end)))
		    (message "Cannot shrink further")
		    (sit-for 1))
		   ((eql input ?t)
		    (amsrefs-convert-to-field-with-stripping beg end
							     prefix suffix
							     "title")
		    (setq done t))
		   ((eql input ?j)
		    (amsrefs-convert-to-field-with-stripping beg end
							     prefix suffix
							     "journal")
		    (setq done t))
		   ((eql input ?s)
		    (amsrefs-convert-to-field-with-stripping beg end
							     prefix suffix
							     "status")
		    (setq done t))
		   ((eql input ?S)
		    (amsrefs-convert-to-field-with-stripping beg end
							     prefix suffix
							     "series")
		    (setq done t))
		   ((eql input ?n)
		    (amsrefs-convert-to-field-with-stripping beg end
							     prefix suffix
							     "note")
		    (setq done t))
		   ((eql input ?e)
		    (amsrefs-convert-to-field-with-stripping beg end
							     prefix suffix
							     "eprint")
		    (setq done t))
		   ((eql input ?p)
		    (amsrefs-convert-to-field-with-stripping beg end
							     prefix suffix
							     "publisher")
		    (setq done t))
		   ((eql input ?P)
		    (amsrefs-convert-to-field-with-stripping beg end
							     prefix suffix
							     "pages")
		    (setq done t))
		   ((eql input ?c)
		    (amsrefs-convert-to-field-with-stripping beg end
							     prefix suffix
							     "place")
		    (setq done t))
		   ((eql input ?o)
		    (amsrefs-convert-to-field-with-stripping
		     beg end
		     prefix suffix
		     (completing-read
		      "amsrefs field name: "
		      amsrefs-field-names nil t))
		    (setq done t))
		   ((eql input ?0)
		    (delete-region beg end)
		    (setq done t))
		   ((eql input ?h)
		    (message "SPC/DEL: extend/shrink selection; t: title; j:journal; s:status; S:series; n:note; e:eprint; p:publisher; P:pages; c:place; o:other; 0:delete")
		    (sit-for 4))
		   (t))
		  (and (not done) (not (memql input '(?\a ?\e))))))
	 done))))


;; TODO:
;; v name inverting
;; v minor mode (or other way to "move-to-next-amsrefs-field"?)
;; * recognize parts of the bibentry (mark them as region?)
;; v make the regexp for move-to-next... a defcustom (?)
