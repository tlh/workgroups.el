;;; workgroups-utils.el --- utilities used by Workgroups

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:     workgroups-utils.el
;; Author:   tlh <thunkout@gmail.com>
;; Created:  2010-07-22
;; Version   0.2.0
;; Keywords: utilities

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; Just a bunch of general purpose-ish utilities used by Workgroups.
;;
;;; Installation:
;;
;;; Usage:
;;



;;; Code:

(eval-and-compile
  (require 'cl))



(defmacro wg-with-gensyms (syms &rest body)
  "Bind all symbols in SYMS to `gensym's, and eval BODY."
  (declare (indent 1))
  `(let (,@(mapcar (lambda (sym) `(,sym (gensym))) syms)) ,@body))

(defmacro wg-when-boundp (symbols &rest body)
  "When all SYMBOLS are bound, `eval' body."
  (declare (indent 1))
  `(when (and ,@(mapcar (lambda (sym) `(boundp ',sym)) symbols))
     ,@body))

(defun wg-partition (list n &optional step)
  "Return list of N-length sublists of LIST, offset by STEP.
Iterative to prevent stack overflow."
  (let (acc)
    (while list
      (push (wg-take list n) acc)
      (setq list (nthcdr (or step n) list)))
    (nreverse acc)))

(defmacro wg-dbind (args expr &rest body)
  "Abbreviation of `destructuring-bind'."
  (declare (indent 2))
  `(destructuring-bind ,args ,expr ,@body))

(defmacro wg-docar (spec &rest body)
  "do-style wrapper for `mapcar'."
  (declare (indent 1))
  `(mapcar (lambda (,(car spec)) ,@body) ,(cadr spec)))

(defmacro wg-dohash (spec &rest body)
  "do-style wrapper for `maphash'."
  (declare (indent 1))
  (wg-dbind (key val table &optional return) spec
    `(progn (maphash (lambda (,key ,val) ,@body) ,table) ,return)))

(defmacro wg-doconcat (spec &rest body)
  "do-style wrapper for `mapconcat'."
  (declare (indent 1))
  (wg-dbind (elt seq &optional sep) spec
    `(mapconcat (lambda (,elt) ,@body) ,seq (or ,sep ""))))

(defmacro wg-when-let (binds &rest body)
  "Like `let*', but only eval BODY when all BINDS are non-nil."
  (declare (indent 1))
  (wg-dbind (bind . binds) binds
    (when (consp bind)
      `(let (,bind)
         (when ,(car bind)
           ,(if (not binds) `(progn ,@body)
              `(wg-when-let ,binds ,@body)))))))

(defmacro wg-until (test &rest body)
  "`while' not."
  (declare (indent 1))
  `(while (not ,test) ,@body))

(defmacro wg-aif (test then &rest else)
  "Anaphoric `if'."
  (declare (indent 2))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro wg-awhen (test &rest body)
  "Anaphoric `when'."
  (declare (indent 1))
  `(wg-aif ,test (progn ,@body)))

(defmacro wg-acond (&rest clauses)
  "Anaphoric `cond'."
  (when clauses
    (wg-dbind ((condition . body) . rest) clauses
      `(wg-aif ,condition (progn ,@body)
         (wg-acond ,@rest)))))

(defmacro wg-aand (&rest args)
  "Anaphoric `and'."
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(wg-aif ,(car args) (aand ,@(cdr args))))))

(defmacro wg-asetf (&rest places-and-values)
  "Anaphoric `setf'."
  `(progn ,@(wg-docar (pv (wg-partition places-and-values 2))
              `(let ((it ,(car pv))) (setf ,@pv)))))

(defmacro wg-removef-p (object place &rest keys)
  "If OBJECT is a `member*' of PLACE, remove it and return t.
Otherwise return nil.  KEYS is passed to both `member*' and `remove*'."
  (wg-with-gensyms (obj)
    `(let ((,obj ,object))
       (when (member* ,obj ,place ,@keys)
         (setf ,place (remove* ,obj ,place ,@keys))
         t))))

(defmacro wg-pushnew-p (object place &rest keys)
  "If OBJECT is not a `member*' of PLACE, push it and return t.
Otherwise return nil.  KEYS is passed to both `member*' and `pushnew'."
  (wg-with-gensyms (obj)
    `(let ((,obj ,object))
       (unless (member* ,obj ,place ,@keys)
         (pushnew ,obj ,place ,@keys)
         t))))

(defun wg-toggle (symbol)
  "Toggle SYMBOL's truthiness."
  (set symbol (not (symbol-value symbol))))

(defun wg-step-to (n m step)
  "Increment or decrement N toward M by STEP.
Return M when the difference between N and M is less than STEP."
  (cond ((= n m) n)
        ((< n m) (min (+ n step) m))
        ((> n m) (max (- n step) m))))

(defun wg-within (num lo hi &optional hi-inclusive)
  "Return t when NUM is within bounds LO and HI.
HI-INCLUSIVE non-nil means the HI bound is inclusive."
  (and (>= num lo) (if hi-inclusive (<= num hi) (< num hi))))

(defun wg-filter (pred seq)
  "Return a list of elements in SEQ on which PRED returns non-nil."
  (let (acc)
    (mapc (lambda (elt) (and (funcall pred elt) (push elt acc))) seq)
    (nreverse acc)))

(defun wg-last1 (list)
  "Return the last element of LIST."
  (car (last list)))

(defun wg-take (list n)
  "Return a list of the first N elts in LIST."
  (butlast list (- (length list) n)))

(defun wg-leave (list n)
  "Return a list of the last N elts in LIST."
  (nthcdr (- (length list) n) list))

(defun wg-rnth (n list)
  "Return the Nth element of LIST, counting from the end."
  (nth (- (length list) n 1) list))

(defun wg-take-until-fail (pred list)
  "Take elements from LISP up to the first element on which PRED fails."
  (let (taken)
    (catch 'result
      (dolist (elt list (nreverse taken))
        (if (funcall pred elt) (push elt taken)
          (throw 'result (nreverse taken)))))))

(defun wg-range (start end)
  "Return a list of integers from START up to but not including END."
  (let (accum)
    (dotimes (i (- end start) (nreverse accum))
      (push (+ start i) accum))))

(defun wg-rotate-list (list &optional offset)
  "Rotate LIST by OFFSET.  Positive OFFSET rotates left, negative right."
  (when list
    (let ((split (mod (or offset 1) (length list))))
      (append (nthcdr split list) (wg-take list split)))))

(defun wg-center-rotate-list (list)
  "Rotate LIST so it's first elt is in the center.  When LIST's
length is even, the first elt is left nearer the front."
  (wg-rotate-list list (- (/ (1- (length list)) 2))))

(defun wg-cyclic-nth (list n)
  "Return the Nth element of LIST, modded by the length of list."
  (nth (mod n (length list)) list))

(defun wg-insert-after (elt list index)
  "Insert ELT into LIST after INDEX."
  (let ((new-list (copy-list list)))
    (push elt (cdr (nthcdr index new-list)))
    new-list))

(defun wg-insert-before (elt list index)
  "Insert ELT into LIST before INDEX."
  (if (zerop index) (cons elt list)
    (wg-insert-after elt list (1- index))))

(defun wg-move-elt (elt list index &rest keys)
  "Move ELT before INDEX in LIST.
KEYS is passed to `remove*'."
  (wg-insert-before elt (apply 'remove* elt list keys) index))

(defun wg-cyclic-offset-elt (elt list n)
  "Cyclically offset ELT's position in LIST by N."
  (wg-when-let ((pos (position elt list)))
    (wg-move-elt elt list (mod (+ n pos) (length list)))))

(defun wg-cyclic-nth-from-elt (elt list n &rest keys)
  "Return the elt in LIST N places cyclically from ELT.
If ELT is not present is LIST, return nil.
KEYS is passed to `position'."
  (wg-when-let ((pos (apply 'position elt list keys)))
    (wg-cyclic-nth list (+ pos n))))

(defun wg-util-swap (elt1 elt2 list)
  "Return a copy of LIST with ELT1 and ELT2 swapped.
Return nil when ELT1 and ELT2 aren't both present."
  (wg-when-let ((p1 (position elt1 list))
                (p2 (position elt2 list)))
    (wg-move-elt elt1 (wg-move-elt elt2 list p1) p2)))

(defmacro wg-make-alist (&rest kvps)
  "Return a new alist from KVPS."
  `(list ,@(wg-docar (pair (wg-partition kvps 2))
             `(cons ,@pair))))

(defun wg-aget (alist key &optional default)
  "Return the value of KEY in ALIST. Uses `assq'.
If PARAM is not found, return DEFAULT which defaults to nil."
  (wg-aif (assq key alist) (cdr it) default))

(defun wg-acopy (alist)
  "Return a copy of ALIST's toplevel list structure."
  (wg-docar (kvp alist) (cons (car kvp) (cdr kvp))))

(defun wg-aput (alist key value)
  "Return a new alist from ALIST with KEY's value set to VALUE."
  (let* ((found nil)
         (new (wg-docar (kvp alist)
                (if (not (eq key (car kvp))) kvp
                  (setq found (cons key value))))))
    (if found new (cons (cons key value) new))))

(defun wg-aremove (alist key)
  "`remove' KEY's key-value-pair from alist."
  (remove (assoc key alist) alist))

(defmacro wg-abind (alist binds &rest body)
  "Bind values in ALIST to symbols in BINDS, then eval BODY.
If an elt of BINDS is a symbol, use it as both the bound variable
and the key in ALIST.  If it is a cons, use the car as the bound
variable, and the cadr as the key."
  (declare (indent 2))
  (wg-with-gensyms (asym)
    `(let* ((,asym ,alist)
            ,@(wg-docar (bind binds)
                (let ((c (consp bind)))
                  `(,(if c (car bind) bind)
                    (wg-aget ,asym ',(if c (cadr bind) bind))))))
       ,@body)))

(defun wg-fill-hash-table (table &rest key-value-pairs)
  "Fill TABLE with KEY-VALUE-PAIRS and return TABLE."
  (while key-value-pairs
    (puthash (car key-value-pairs) (cadr key-value-pairs) table)
    (setq key-value-pairs (cddr key-value-pairs)))
  table)

(defun wg-fill-keymap (keymap &rest binds)
  "Return KEYMAP after defining in it all keybindings in BINDS."
  (while binds
    (define-key keymap (car binds) (cadr binds))
    (setq binds (cddr binds)))
  keymap)

(defun wg-write-sexp-to-file (sexp file)
  "Write the printable representation of SEXP to FILE."
  (with-temp-buffer
    (let ((print-level nil)  (print-length nil))
      (insert (format "%S" sexp)))
    (write-file file)))

(defun wg-read-sexp-from-file (file)
  "Read and return an sexp from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun wg-read-object (prompt test warning &optional initial-contents keymap
                              read hist default-value inherit-input-method)
  "PROMPT for an object that satisfies TEST, WARNING if necessary.
ARGS are `read-from-minibuffer's args, after PROMPT."
  (flet ((read () (read-from-minibuffer
                   prompt initial-contents keymap read hist
                   default-value inherit-input-method)))
    (let ((obj (read)))
      (when (and (equal obj "") default-value) (setq obj default-value))
      (while (not (funcall test obj))
        (message warning)
        (sit-for wg-minibuffer-message-timeout)
        (setq obj (read)))
      obj)))

(defun wg-file-under-root-path-p (root-path file-path)
  "Return t when FILE-PATH is under ROOT-PATH, nil otherwise."
  (string-match (concat "^" (regexp-quote (expand-file-name root-path)))
                (expand-file-name file-path)))

(defun wg-cyclic-nth-from-frame (&optional n frame)
  "Return the frame N places away from FRAME in `frame-list' cyclically.
N defaults to 1, and FRAME defaults to `selected-frame'."
  (wg-cyclic-nth-from-elt
   (or frame (selected-frame)) (frame-list) (or n 1)))

(defun wg-symcat (&rest symbols-and-strings)
  "Return a new interned symbol by concatenating SYMBOLS-AND-STRINGS."
  (intern (mapconcat (lambda (obj) (if (symbolp obj) (symbol-name obj) obj))
                     symbols-and-strings "")))

(defun wg-make-string (times string &optional separator)
  "Like `make-string', but includes a separator."
  (mapconcat 'identity (make-list times string) (or separator "")))

(defun wg-get-buffer (buffer-or-name)
  "Return BUFFER-OR-NAME's buffer, or error."
  (or (get-buffer buffer-or-name)
      (error "%S does not identify a buffer" buffer-or-name)))

(defun wg-buffer-name (buffer-or-name)
  "Return BUFFER-OR-NAME's `buffer-name', or error."
  (buffer-name (wg-get-buffer buffer-or-name)))

(defun wg-buffer-file-name (buffer-or-name)
  "Return BUFFER-OR-NAME's `buffer-file-name', or error."
  (buffer-file-name (wg-get-buffer buffer-or-name)))

(defun wg-interesting-buffers ()
  "Return a list of only the interesting buffers in `buffer-list'."
  (remove-if (lambda (bname) (string-match "^ " bname))
             (buffer-list) :key 'buffer-name))

(defun wg-add-or-remove-hooks (remove &rest pairs)
  "Add FUNCTION to or remove it from HOOK, depending on REMOVE."
  (dolist (pair (wg-partition pairs 2))
    (funcall (if remove 'remove-hook 'add-hook)
             (car pair) (cadr pair))))

(defun wg-get-first-buffer-matching-regexp (regexp &optional buffer-list)
  "Return the first buffer in BUFFER-LIST with a name matching REGEXP.
BUFFER-LIST should contain buffer objects and/or buffer names."
  (find regexp (or buffer-list (buffer-list))
        :test 'string-match :key 'wg-buffer-name))

(defun wg-buffer-major-mode (buffer-or-name)
  "Return BUFFER's major-mode."
  (with-current-buffer buffer-or-name major-mode))

(defmacro wg-buffer-local-setq (buffer var value)
  "`setq' VAR to VALUE while BUFFER is current.
Note that this won't make VAR buffer-local if it isn't already."
  `(with-current-buffer ,buffer (setq ,var ,value)))

(defmacro wg-defstruct (prefix name-form &rest slot-defs)
  "`defstruct' wrapper that namespace-prefixes all generated functions.
Note: this doesn't yet work with :conc-name, and possibly other
options."
  (declare (indent 2))
  (let* ((name (if (consp name-form) (car name-form) name-form))
         (prefixed-name (wg-symcat prefix "-" name)))
    (flet ((rebind (opstr)
                   (let ((oldfnsym (wg-symcat opstr "-" prefix "-" name)))
                     `((fset ',(wg-symcat prefix "-" opstr "-" name)
                             (symbol-function ',oldfnsym))
                       (fmakunbound ',oldfnsym)))))
      ;; `eval-and-compile' gets rid of byte-comp warnings ("function `foo' not
      ;; known to be defined").  We can accomplish this with `declare-function'
      ;; too, but it annoyingly requires inclusion of the function's arglist,
      ;; which gets ugly.
      `(eval-and-compile
         (defstruct ,(if (symbolp name-form) prefixed-name
                       `(,prefixed-name ,@(cdr name-form)))
           ,@slot-defs)
         ,@(rebind "make")
         ,@(rebind "copy")
         ',prefixed-name))))

(defmacro wg-with-slots (obj slot-bindings &rest body)
  "Bind OBJ's slot values to symbols in BINDS, then eval BODY.
The car of each element of SLOT-BINDINGS is the bound symbol, and
the cadr as the accessor function."
  (declare (indent 2))
  (wg-with-gensyms (objsym)
    `(let* ((,objsym ,obj)
            ,@(wg-docar (slot slot-bindings)
                `(,(car slot) (,(cadr slot) ,objsym))))
       ,@body)))



;;; provide

(provide 'workgroups-utils)



;;; workgroups-utils.el ends here
