(defvar handle-symbolchars '("!" "$" "%" "&" "*" "+" "-" "." "/" ":" "<" "=" ">" "?" "@" "^" "|" "~"))
(defvar handle-prefixop '("~" "?" "!"))
(defvar handle-infixop '("=" "<" ">" "|" "&" "$" "@" "^" "+" "-" "*" "/" "%" "**"))

(defvar handle-anonymous "_")

;; we can declare: operations, dynamic / static variables, ml-types, constants
;; we can bind: ml vars, dyn vars, tt vars, pvars
;; we can destruct: constructors, judgements, operations, finally, val




(defvar handle-operation-rx "operation")

(defface handle-operation-face '((t (:inherit font-lock-function-name-face)))
  "" :group 'm31)

(defvar handle-cases-keywords
  '("mltype" "operation"
    "handle" "handler"
    "match" "|" "with"
    "val" "finally"
    "end"
    "yield"))

(defface handle-cases-face '((t (:inherit font-lock-keyword-face)))
  "" :group 'm31)

(defvar handle-mltype-rx "mltype")
(defface handle-mltype-face '((t (:inherit handle-cases-face)))
  "" :group 'm31)
;; swap with constant / tt ?

(defvar handle-meta-binder-begin-rx
  (rx symbol-start                               ;FIXME: should be symbol \_< not \<
      (|
       ;; "and"  ; problematic because we use it in mltype declarations
       "dynamic" "now"
       ;; "fun"                               ;static variables
       (seq "let" (? (+ space) "rec")                               ;static variables
            ))
      symbol-end))

(defface handle-meta-binder-begin-face '((t (:inherit font-lock-preprocessor-face)))
  "" :group 'm31)

(defvar handle-meta-variable-rx
  (eval
   `(rx
     (|
      (sequence "(" (* space)
                (* (any ,(mapconcat (lambda (c) c) handle-symbolchars "")))
                (* space) ")")
      (sequence (| (syntax word) "_") (* (| (syntax word) (syntax symbol))))))))
(defface handle-meta-variable-face '((t (:inherit font-lock-function-name-face)))
  "" :group 'm31)

(defvar handle-meta-binder-end-keywords
  '("in"
    "="
    "=>" "⇒" "⟹"
    ))
(defface handle-meta-binder-end-face '((t (:inherit handle-meta-binder-begin-face)))
  "" :group 'm31)

(defvar handle-topdirective-keywords '("do" "fail"))
(defface handle-topdirective-face '((t (:inherit font-lock-keyword-face)))
  "" :group 'm31)

(defvar handle-meta-keywords
  '(
    ;; "judgement"
    ;; "judgment"
    ;; "_"
    "external"
    "#include_once"
    "ref" "!" ":="
    ;;    ";" ","
    ))
(defface handle-meta-face '((t (:inherit font-lock-constant-face)))
  "" :group 'm31)

(defvar handle-tt-keywords
  '("==" "≡"
    "Type"
    "beta_step"
    "congr_apply" "congr_eq" "congr_lambda" "congr_prod" "congr_refl"
    "context"
    "natural"
    "occurs"
    "where"                        ;could be a destructor
    "refl"
    "|-" "⊢"))
(defface handle-tt-face '((t (:inherit font-lock-type-face)))
  "" :group 'm31)

(defvar handle-tt-atom-keywords
  '("assume" "constant"))
(defface handle-tt-atom-face '((t (:inherit font-lock-type-face)))
  "" :group 'm31)

(defvar handle-tt-binder-begin-rx (rx bow (| "forall" "∀" "Π" "∏" "lambda" "λ") eow))
(defface handle-tt-binder-begin-face '((t (:inherit font-lock-type-face)))
  "" :group 'm31)

(defvar handle-tt-binder-end-keywords '("->" "→" ","))

(defface handle-tt-binder-end-face '((t (:inherit handle-tt-binder-begin-face)))
  "" :group 'm31)

(defvar handle-boring-keywords '("," ";" "." "of"))
(defface handle-boring-face '((t (:inherit font-lock-preprocessor-face)))
  "" :group 'm31)

(defvar handle-pvar-rx (eval `(rx "?" (regexp ,handle-meta-variable-rx))))
(defface handle-pvar-face '((t (:inherit font-lock-variable-name-face)))
  "" :group 'm31)

(defvar handle-syntax-classes
  '(
    boring
    cases
    topdirective
    meta tt
    ))

(require 'cl-macs)
(defun handle-font-lock-mk (name)
  (cl-flet ((f (suf)
               (intern (concat "handle-" (symbol-name name) suf))))
    (list (regexp-opt (symbol-value (f "-keywords")) 'symbols) 1 `',(f "-face"))))


(defun handle-font-lock-defaults ()
  "Calculate the font-lock defaults for `handle-mode'."
  (list
   (append

    `((,(rx symbol-start (| "¬" "~") symbol-end) 0 'font-lock-negation-char-face))

    `((,(eval `(rx
                (group-n 1 (regexp ,handle-meta-binder-begin-rx))
                (group-n 2
                         (+ (| space (regexp "\n")))
                         (regexp ,handle-meta-variable-rx))
                (group-n 3 (* (seq
                               (+ (| space (regexp "\n")))
                               (regexp ,handle-meta-variable-rx))))))
       (1 '(handle-meta-binder-begin-face))
       (2 '(handle-meta-variable-face))
       (3 '(handle-pvar-face))))

    `((,(rx symbol-start (| "and" "as" "in") symbol-end) (0 '(handle-meta-binder-begin-face))))

    `((,(eval `(rx
                (group-n 1 (regexp ,(regexp-opt handle-tt-atom-keywords)))
                (group-n 2 (+ (seq
                               (+ (| space (regexp "\n")))
                               (regexp ,handle-meta-variable-rx))))))
       (1 '(handle-tt-atom-face))
       (2 '(handle-pvar-face))))

    `((,(eval `(rx
                (group-n 1 (regexp ,handle-mltype-rx))
                (+ (| space (regexp "\n")))
                (group-n 2 (regexp ,handle-meta-variable-rx))))
       (1 '(handle-boring-face))
       (2 '(handle-cases-face))))

    `((,(eval `(rx
                (group-n 1 (regexp ,handle-operation-rx))
                (+ (| space (regexp "\n")))
                (group-n 2 (regexp ,handle-meta-variable-rx))))
       (1 '(handle-boring-face))
       (2 '(handle-operation-face))))

    `((,(eval `(rx
                (group-n 1 (regexp ,handle-tt-binder-begin-rx))
                (group-n 2 (* (seq
                               (+ (| space (regexp "\n")))
                               (regexp ,handle-meta-variable-rx))))))
       (1 '(handle-tt-binder-begin-face))
       (2 '(handle-pvar-face))))

    `((,(eval `(rx
                (group-n 1 (sequence symbol-start "fun" symbol-end))
                (group-n 2 (* (seq
                               (+ (| space (regexp "\n")))
                               (regexp ,handle-meta-variable-rx))))))
       (1 '(handle-meta-binder-begin-face))
       (2 '(handle-pvar-face))))

    `((,(eval `(rx "!" (regexp ,handle-meta-variable-rx))) (0 '(handle-meta-face))))

    `((,handle-pvar-rx 0 'handle-pvar-face))

    (mapcar 'handle-font-lock-mk handle-syntax-classes))))

(require 'subr-x)
(defvar handle-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?( "()1n" table)
                         (modify-syntax-entry ?* ". 23n" table)
                         (modify-syntax-entry ?) ")(4n" table)
    (modify-syntax-entry ?' "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?# "_" table)
    (modify-syntax-entry ?≡ "_" table)
    (mapc (lambda (c) (modify-syntax-entry c "." table)) ",;")
    (modify-syntax-entry ?¬ "w" table)
    (modify-syntax-entry ?∀ "w" table)
    ;; (modify-syntax-entry ?⟹ "w" table)
    ;; (modify-syntax-entry ?= "w" table)
    ;; (modify-syntax-entry ?> "w" table)
    (mapc (lambda (c) (modify-syntax-entry c "'"  table))
          (string-join handle-prefixop))
    table)
  "The syntax table used in `handle-mode'")



(defvar handle-meta-variable-rx
  (eval
   `(rx
     (|
      (sequence (| (syntax word) "_") (* (| (syntax word) (syntax
                                                           symbol))))))))

(defvar handle--name-re handle-meta-variable-rx)
(defvar handle--reserved
  '(("Type" . "TYPE")
    ("judgement" . "JUDGMENT")
    ("judgment" . "JUDGMENT")
    ("_" . "UNDERSCORE")
    ("_atom" . "UATOM")
    ("_constant" . "UCONSTANT")
    ("and" . "AND")
    ("as" . "AS")
    ("assume" . "ASSUME")
    ("beta_step" . "BETA_STEP")
    ("congr_prod" . "CONGR_PROD")
    ("congr_apply" . "CONGR_APPLY")
    ("congr_lambda" . "CONGR_LAMBDA")
    ("congr_eq" . "CONGR_EQ")
    ("congr_refl" . "CONGR_REFL")
    ("constant" . "CONSTANT")
    ("context" . "CONTEXT")
    ("do" . "DO")
    ("dynamic" . "DYNAMIC")
    ("end" . "END")
    ("external" . "EXTERNAL")
    ("fail" . "DO")
    ("finally" . "FINALLY")
    ("forall" . "PROD")
    ("fun" . "FUNCTION")
    ("handle" . "HANDLE")
    ("handler" . "HANDLER")
    ("in" . "IN")
    ("lambda" . "LAMBDA")
    ("let" . "LLET")
    ("match" . "MATCH")
    ("mlstring" . "MLSTRING")
    ("mltype" . "MLTYPE")
    ("mlunit" . "MLUNIT")
    ("natural" . "NATURAL")
    ("now" . "LLET")
    ("occurs" . "OCCURS")
    ("of" . "OF")
    ("operation" . "OPERATION")
    ("rec" . "REC")
    ("ref" . "REF")
    ("refl" . "REFL")
    ("val" . "VAL")
    ("verbosity" . "VERBOSITY")
    ("where" . "WHERE")
    ("with" . "WITH")
    ("yield" . "YIELD")
    ("Π" . "PROD")
    ("λ" . "LAMBDA")
    ("∀" . "PROD")
    ("∏" . "PROD")
    ("⊢" . "VDASH")))

(defun handle--smie-token (dir)
    (cl-flet ((f (cl-case dir ('forward 'looking-at)
                          ('backward (lambda (r) (looking-back r nil t))))))
      (cond
       ((f "(")             (progn (setq handle--in-list-check-backward t) "LPAREN"))
       ((f ")")             (progn (setq handle--in-list-check-forward  t) "RPAREN"))
       ((f "\\[")           "LBRACK")
       ((f "\\]")           "RBRACK")
       ((f ":=")            "COLONEQ")
       ((f "\\_<=\\_>")     "EQ")
       ((f ":")             "COLON")
       ((f ",")             "COMMA")
       ((f ";")             "SEMICOLON")
       ((f "|")             "BAR")
       ((f "_")             "UNDERSCORE")
       ((f "->\\|⟶")        "ARROW")
       ((f "=>\\|⟹\\|⇒")       "DARROW")
       ((f "==\\|≡")        "EQEQ")

       ((f handle--name-re)
        (let ((s (buffer-substring-no-properties
                  (match-beginning 0) (match-end 0))))
          (or
           (cdr (assoc s handle--reserved))
           "NAME")))

       ((eobp) "EOF")
       ((f ".") "idk"))))


(defcustom handle-indent-basic smie-indent-basic "" :group 'm31)
(defcustom handle-indent-do (/ handle-indent-basic 2) "" :group 'm31)
(defcustom handle-indent-after-with handle-indent-basic "" :group 'm31)

;; sequencing, toplevel-handlers, top-let, #include, dynamic, top-now

(defvar handle-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2

    '((topdirs
                (topdirs "DO" topdirs)
                (topdirs "HANDLE" topdirs)
                ;; (term "WITH" term)
                (pattern)
                )


      (term     (ty_term)
;                ("HANDLE" term "WITH" patterns "END")
                ("MATCH" term "WITH" patterns "END")
                ("LLET" names "EQ" term "IN" term)
                ("ASSUME" "NAME" "COLON" term "IN" term)
                ("AND" names "EQ" term "IN" term)
                (term "NAME" term)
                (term "COLON" ty_term)
                ("LAMBDA" simple_term "COMMA" term)
                )

      (ty_term  (term "EQEQ" term)
                (simple_term))

      (simple_term ("TYPE")
                   ("LPAREN" term "RPAREN"))

      (names    (names "NAME" names))

      (patterns (patterns "BAR" patterns)
                (pattern))

      (pattern  (term "DARROW" term)
                (term "COLON" term "DARROW" term))

      )

    ;; '("IN" < "ascr_COLON")
     '((assoc "DO"
              "TOPDIRECTIVE" "END")
       (assoc "HANDLE"))
     '((right "COLON") (assoc "BAR" "TYBAR")
       (assoc "NAME")
       (assoc "IN" "DARROW")
       )

     )))

(defun handle-smie-rules (kind token)
  (message "looking at %S : %S, pt: %S" token kind (point))
  (pcase (cons kind token)
    (`(:elem . basic) (message "basic") handle-indent-basic)
;    (`(,_ . "COMMA") (message "separator-comma") (smie-rule-separator kind))
    ;; (`(:after . "COLONEQ") handle-indent-basic)


    ;; (`(:before . "BAR")
    ;;  (if (smie-rule-parent-p "MLTYPE")
    ;;      (progn (message "before-BAR parent:MLTYPE")
    ;;             (smie-rule-parent handle-indent-basic))
    ;;    (message "before-BAR, %S" (smie-rule-prev-p "HANDLE"))
    ;;    (if (smie-rule-prev-p "HANDLE" "WITH")
    ;;        handle-indent-after-with
    ;;      0)
    ;;    ))


    ;; (`(:before . "NAME")
    ;; ;;  nil)
    ;; ;; (`(:x . "x")

    ;;  (progn (message "before-NAME") nil)
    ;;  (if (smie-rule-parent-p "MLTYPE")
    ;;      (progn (message "before-NAME parent:MLTYPE , foo: %S" (smie-rule-prev-p "BAR"))
    ;;             (smie-rule-parent (+ 2 handle-indent-basic)))

    ;;    ;; after an IN we should find the corresponding opening LET.

    ;;    (if (and ;(smie-rule-bolp) & prev = in
    ;;         (message "prev: %S" (save-excursion
    ;;                               (handle--smie-backward-token)))
    ;;         (equal (handle--smie-backward-token) "IN") (message "hi"))

    ;;        (save-excursion
    ;;          ;; the IN has an EQ as parent, so go there and find its parent
    ;;          ;; (let (smie--parent) (smie-indent--parent)
    ;;          ;;      (goto-char (cadr (smie-indent--parent)))
    ;;          ;;      (message "parent %S" smie--parent))
    ;;          (let (smie--parent) (smie-indent--parent)
    ;;               (goto-char (cadr (smie-indent--parent)))
    ;;               (message "parent %S" smie--parent))
    ;;          (message "pt %S" (point))
    ;;          (cons 'column (current-column)))))
    ;;  )

    ;; (`(:after . "HANDLE") (message "after-HANDLE") handle-indent-basic)

    ;; (`(:before . "HANDLE")
    ;;  (message "before-HANDLE, par: %S, prev: %S"
    ;;           (let (smie--parent) (smie-indent--parent))
    ;;           (save-excursion (smie-indent-backward-token)))
    ;;  (if (and (smie-rule-parent-p "DO") (smie-rule-prev-p "DO"))
    ;;      (progn (message "hi") handle-indent-do)
    ;;    (when (smie-rule-next-p "BAR")
    ;;      (smie-rule-parent))))

    ;; (`(:before . "DARROW")
    ;;  (message "before-DARROW, hang: %S, p0: %S, pt: %S"
    ;;           (smie-rule-hanging-p) smie--parent (point))
    ;;  (when (smie-rule-hanging-p)
    ;;    (if (smie-rule-parent-p "COLON")
    ;;        (save-excursion
    ;;          ;; goto COLON
    ;;          (goto-char (cadr (smie-indent--parent)))
    ;;          (let (smie--parent)
    ;;            ;; goto BAR
    ;;            (goto-char (cadr (smie-indent--parent)))
    ;;            (smie-rule-parent handle-indent-basic)))
    ;;      (smie-rule-parent handle-indent-basic))))

    ;; (`(:after . "COLON")
    ;;  (when (smie-rule-parent-p "CONSTANT")
    ;;    (message "after-colon parent:cnst")
    ;;    (smie-rule-parent handle-indent-basic)))

    ;; (`(:after . "DO") (message "after-DO") handle-indent-do)
    ;; (`(:after . "WITH") (message "after-WITH") handle-indent-after-with)

    ;; (`(:before . ,(or `"begin" `"LPAREN" `"LBRACK"))
    ;;  (if (smie-rule-hanging-p) (smie-rule-parent)))
    ;; (`(:after . ,(or `"in" `"end" `"RPAREN" `"RBRACK"))
    ;;  (if (smie-rule-hanging-p) (smie-rule-parent)))
    (_ (progn (message "fall-through: %S . %S" kind token) nil))
    ))

(defun handle--smie-forward-token nil
  (forward-comment (point-max))
  (let ((s (handle--smie-token 'forward)))
    (goto-char (match-end 0))
    s))

(defun handle--smie-backward-token nil
  (forward-comment (- (point)))
  (let ((s (handle--smie-token 'backward)))
    (goto-char (match-beginning 0))
    s))

(defun handle-smie-forward-token nil
  (interactive)
  (message "%s" (handle--smie-forward-token)))
(defun handle-smie-backward-token nil
  (interactive)
  (message "%s" (handle--smie-backward-token)))

(defun handle-smie-setup nil
  (smie-setup handle-smie-grammar 'handle-smie-rules
              :forward-token 'handle--smie-forward-token
              :backward-token 'handle--smie-backward-token))

(define-derived-mode handle-mode prog-mode "m31"
  "Major mode for editing Andromeda files.

Useful commands:
C-c C-.          handle-send-buffer-up-to-point
C-c .            handle-send-buffer-up-to-point
C-c C-b          handle-send-buffer
C-c C-l          handle-send-buffer
"
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local compilation-buffer-name-function 'handle-compilation-buffer-name)
  (setq-local require-final-newline t)
  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "(\\*+[ \t]*")
  (setq-local font-lock-defaults (handle-font-lock-defaults))
  (handle-smie-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m31\\'" . handle-mode))

(provide 'mini-handle)
;;; mini-handle.el ends here
