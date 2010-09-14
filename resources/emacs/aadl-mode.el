;; aadl-mode
;; Copyright 2004-2006 Ecole nationale superieure des telecommunications
;; Laurent.Pautet@enst.fr, Thomas.Vergnaud@enst.fr, Bechir.Zalila@enst.fr

;; To load this file, just add the following line to your .emacs :
;; (load "/path/to/this/file.el")

(defvar aadl-mode-hook nil)

; Add support for compilation of AADL files directly from emacs
(require 'compile)
(add-hook 'aadl-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "ocarina -aadlv2 -f -p " buffer-file-name))))

(defvar aadl-mode-map
  (let ((aadl-mode-map (make-keymap)))
    (define-key aadl-mode-map "\C-j"  'new-line-and-indent)
    (define-key aadl-mode-map "\C-c;" 'comment-region)
    (define-key aadl-mode-map "\C-c:" 'uncomment-region)
    aadl-mode-map)
  "keymap for AADL major mode")

; We declare that the aadl mode should be used with .aadl files
(add-to-list 'auto-mode-alist '("\\.aadl\\'" . aadl-mode))

; The AADL syntax table
(defvar aadl-mode-syntax-table nil "syntax table for aadl-mode")
(setq aadl-mode-syntax-table (make-syntax-table))
(set-syntax-table aadl-mode-syntax-table)

; Modify the syntax table to be conformant with the AADL syntax

; The '_' character may belong to an AADL word
(modify-syntax-entry ?_ "w" aadl-mode-syntax-table)

; The '.' and the ':' characters are word separators
(modify-syntax-entry ?. "." aadl-mode-syntax-table)
(modify-syntax-entry ?: "." aadl-mode-syntax-table)

; There is only one comment style in AADL beginning with the "--" string
; and ending with a new line or a new page
(modify-syntax-entry ?- ". 12" aadl-mode-syntax-table)
(modify-syntax-entry ?\n ">" aadl-mode-syntax-table)
(modify-syntax-entry ?\f ">" aadl-mode-syntax-table)

; AADL syntactic coloration. Th order of the elements in the global list
; is important. The first element rules override the second element rules 
; which override the third...

(defconst aadl-font-lock-keywords
  (list
   ;; Keywords
   (list (concat
	  "\\<"
	  (regexp-opt
	   '("abstract" "access" "and" "applies" "binding" "calls" "classifier" 
	     "connections" "constant" "delta" "data" "enumeration" "event" 
	     "feature" "features" "flow" "flows" "group" "in" "inherit" "initial"
	     "inverse" "is" "list" "mode" "modes" "not" "out" "parameter" 
	     "path" "port" "private" "process" "processor" "prototypes" 
	     "properties" "with" "renames"
	     "property" "provides" "public" "range" "reference" "refined" 
	     "refines" "requires" "server" "set" "sink" "source" 
	     "subcomponents" "system" "thread" "to" "type" "units" "value"
	     "virtual") t)
	  "\\>") 
	 '(1 font-lock-keyword-face))

   ;; Three keywords followed by an identifier or a scoped name
   (list (concat
	  ;; 1st kw
	  (regexp-opt 
	   '("event" "port" "provides" "requires" "thread") t)
	  "[ \t]+"
	  ;; 2nd kw
	  (regexp-opt 
	   '("group" "data") t)
	  "[ \t]+"
	  ;; 3rd kw
	  (regexp-opt 
	   '("access" "implementation" "port") t)
	  "\\>[ \t]*"
	  "\\(\\sw+\\(\\(\\.\\|::\\)\\sw*\\)*\\)?")
	 '(1 font-lock-keyword-face) 
	 '(2 font-lock-keyword-face)
	 '(3 font-lock-keyword-face)
	 '(4 font-lock-function-name-face nil t))

   ;; Couple of keywords followed by an identifier or a scoped name
   (list (concat
	  ;; 1st kw
	  (regexp-opt 
	   '("bus" "data" "device" "end" "event" "memory" "port" "process" 
	     "processor" "property" "thread" "port" "subprogram" "system") t)
	  "[ \t]+"
	  ;; 2nd kw
	  (regexp-opt 
	   '("group" "implementation" "port" "set") t)
	  "\\>[ \t]*"
	  "\\(\\sw+\\(\\(\\.\\|::\\)\\sw*\\)*\\)?")
	 '(1 font-lock-keyword-face) 
	 '(2 font-lock-keyword-face)
	 '(3 font-lock-function-name-face nil t))

   ;; One single keyword followed by an identifier or a scoped name
   (list (concat
	  "\\<"
	  (regexp-opt
	   '("annex" "bus" "data" "device" "end" "extends" "memory" 
	     "package" "parameter" "process" "processor" "property" "port" 
	     "subprogram" "system" "thread" "with") t)
 	  "\\>[ \t]*"
 	  "\\(\\sw+\\(\\(\\.\\|::\\)\\sw*\\)*\\)?") 
 	 '(1 font-lock-keyword-face) 
	 '(2 font-lock-function-name-face nil t))
   
   ;; Identifier followed by a single colon ':'
   (list (concat
 	  "^[ \t]*\\<\\("
	  "\\sw+"
 	  "\\)\\>[ \t]*:[^:]") 
 	 '(1 font-lock-variable-name-face) 
	 '(2 font-lock-text-face nil t))

   ;; Constants
   (list (concat
	  "\\<"
	  (regexp-opt 
	   '("true" "false" "none" "all") t)
	  "\\>")
	 '(1 font-lock-constant-face))

   ;; Types
   (list (concat
	  "\\<"
	  (regexp-opt 
	   '("aadlboolean" "aadlinteger" "aadlreal" "aadlstring") t)
	  "\\>")
	 '(1 font-lock-type-face))
   "AADL Colors"))

;; specialized indentation functions

(defun aadl-indent-end-statement ()
  (forward-line -1)
  (cond 
   ((looking-at "^[ \t]*end")
    0)

   ((looking-at "^[ \t]*\\<\\(package\\|property[ \t]+set\\)\\> +\\sw+")
    0)
   
   ((looking-at "^[ \t]*\\<\\(bus\\|data\\|port\\)\\>[ \t]+\\<\\(access\\|port\\|group\\)\\>")
    (aadl-indent-end-statement))
   
   ((looking-at "^[ \t]*\\<\\(data\\|thread\\|subprogram\\|process\\|bus\\|processor\\|device\\|memory\\|system\\|port[ \t]+group\\)\\([ \t]+implementation\\)?\\>[ \t]+\\sw+")
    (current-indentation))
   
   ((bobp) 0)
   
   (t (aadl-indent-end-statement))
   )
  )

(defun aadl-indent-component-statement ()
  (forward-line -1)
  (cond 
   ((looking-at "^[ \t]*end")
    (current-indentation))
   
   ((looking-at "^[ \t]*\\<\\(private\\|public\\|is\\|package\\|property[ \t]+set\\)\\>")
    2)
   
   ((bobp) 0)
   
   (t (aadl-indent-component-statement))
   )
  )

(defun aadl-indent-subclause-statement ()
  (forward-line -1)
  (cond
   
   ((bobp) 10) ; this should not happen

   ((looking-at "^[ \t]*\\<\\(bus\\|data\\|port\\)\\>[ \t]+\\<\\(access\\|port\\|group\\)\\>")
    (aadl-indent-subclause-statement))
   
   ((looking-at "^[ \t]*\\<\\(data\\|thread\\|subprogram\\|process\\|bus\\|processor\\|device\\|memory\\|system\\|port +group\\)\\([ \t]+implementation\\)?\\>")
    (current-indentation))
   
   (t (aadl-indent-subclause-statement))
   )
  )

(defun aadl-indent-statement ()
  (forward-line -1)
  (cond 
   ((bobp) 0)

   ((looking-at "^[ \t]*\\<\\(package\\|property[ \t]+set\\)\\>[^;]*$")
    2)

   ((looking-at "^[ \t]*\\<\\(public\\|private\\|is\\)\\>[^;]*$")
    2)

   ((looking-at "^[ \t]*end")
    (current-indentation))

   ((looking-at "^[ \t]*\\<\\(bus\\|data\\|port\\)\\>[ \t]+\\<\\(access\\|port\\|group\\)\\>")
    (aadl-indent-statement))
   
   ((looking-at "^[ \t]*\\<\\(data\\|thread\\|subprogram\\|process\\|bus\\|processor\\|device\\|memory\\|port +group\\|features\\|subcomponents\\|properties\\|flows\\|connections\\|modes\\|calls\\)\\>[^;]*$")
    (+ (current-indentation) 2))
   
   ((looking-at "^[ \t]*$")
    (aadl-indent-statement))

   (t (current-indentation))
   )
  )

;; the main indentation function
(defun aadl-indent-line ()
  "indent current line"
  (interactive)
  ;; save the current point position and declare the indentation value variable
  (let ((aadl-current-column 
	 (max 0 (- (current-column) (current-indentation))))
	aadl-indent-value)
    (beginning-of-line)
    (setq 
     aadl-indent-value
     (max 
      0
      (save-excursion
	(cond 
	 ((bobp) 0)
	 
	 ((looking-at "^[ \t]*\\<\\(package\\|property +set\\|private\\|public\\|is\\)\\>") 0)
	 
	 ((looking-at "^[ \t]*end") 
	  (aadl-indent-end-statement))
	 
	 ((looking-at "^[ \t]*\\<\\(data\\|thread\\|subprogram\\|process\\|bus\\|processor\\|device\\|memory\\|system\\|port +group\\)[ \t]+\\(implementation\\)?\\>")
	  (aadl-indent-component-statement))
	 
	 ((looking-at "^[ \t]*\\<\\(features\\|modes\\|subcomponents\\|properties\\|flows\\|connections\\|calls\\)\\>")
	  (aadl-indent-subclause-statement))
	 
	 (t
	  (aadl-indent-statement))
	 ))))
    (indent-line-to aadl-indent-value)
    
    ;; calculate the new point position
    (setq aadl-current-column (+ aadl-current-column aadl-indent-value))
    (move-to-column aadl-current-column)))

(defun aadl-fill-paragraph (ARG)
  (back-to-indentation)
  (cond 
   ((looking-at comment-start)
    (re-search-forward "---*[ \t]*")
    (set-fill-prefix)
    (fill-paragraph ARG))
   
   (t (error "not in comment")))
  )

(defun aadl-mode ()
  "Major mode for editing AADL descriptions"
  (interactive)
  (kill-all-local-variables)
  (use-local-map aadl-mode-map)
  
  ;; register the syntax table
  (set-syntax-table aadl-mode-syntax-table)

  ;; register the keywords for syntax highlighting
  (set (make-local-variable 'font-lock-defaults) 
       '((aadl-font-lock-keywords) nil t))
     
  ;; register the indenting function
  (set (make-local-variable 'indent-line-function)
       'aadl-indent-line)

  ;; a few definitions necessary for aadl-fill-paragraph
  (set (make-local-variable 'comment-line-break-function)
       (lambda (&optional soft) (indent-new-comment-line)))
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-indent-function) 'aadl-indent-statement)
  
  ;;  Support for ispell : Check only comments
  (set (make-local-variable 'ispell-check-comments) 'exclusive)
  
  ;;  Support for indent-new-comment-line (Especially for XEmacs)
  (setq comment-multi-line nil)
   
  (set (make-local-variable 'use-hard-newlines) t)
  
  (set (make-local-variable 'fill-paragraph-function)
      'aadl-fill-paragraph)   
  
  (setq major-mode 'aadl-mode)
  (setq mode-name "AADL")
  (run-hooks 'aadl-mode-hook))

(provide 'aadl-mode)
