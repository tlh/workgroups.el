;;; workgroups.el --- workgroups for windows (for Emacs)

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:     workgroups.el
;; Author:   tlh <thunkout@gmail.com>
;; Created:  2010-07-22
;; Version   0.2.0
;; Keywords: session management window-configuration persistence

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
;;  See README.md in the workgroups directory
;;

;;; TODO:
;;
;;  - enable truncate lines during restore
;;  - split and join
;;


;;; Code:

(require 'cl)


;;; consts

(defconst wg-version "0.2.0"
  "Current version number of workgroups.")

(defconst wg-list-id '-*-workgroups-*-
  "The car of any list containing persisted workgroups.")


;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- Emacs session manager"
  :group 'convenience
  :version wg-version)

(defcustom workgroups-mode-hook nil
  "Hook run when workgroups-mode is turned on."
  :type 'hook
  :group 'workgroups)

(defcustom wg-prefix-key (kbd "C-z")
  "Workgroups' prefix key."
  :type 'string
  :group 'workgroups
  :set (lambda (sym val)
         (when (boundp 'wg-prefix-key)
           (custom-set-default sym val)
           (wg-set-prefix-key)
           val)))

(defcustom wg-switch-hook nil
  "Hook run whenever a workgroup is switched to."
  :type 'hook
  :group 'workgroups)

(defcustom wg-restore-position nil
  "Non-nil means restore the frame's position on workgroup
restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-size t
  "Non-nil means restore the frame's width and height on
workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-scroll-bars t
  "Non-nil means restore scroll-bar settings on workgroup
restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-fringes t
  "Non-nil means restore fringe settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-margins t
  "Non-nil means restore margin settings on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-selected-window t
  "Non-nil means restore `selected-window' on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-mbs-window t
  "Non-nil means restore `minibuffer-scroll-window' on workgroup
restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-default-buffer "*scratch*"
  "Buffer to switch to when a new workgroup is created.
Also used when a workgroup's window's buffer can't be retrieved."
  :type 'string
  :group 'workgroups)

(defcustom wg-use-faces t
  "Non-nil means use faces in various displays."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-switch-on-load t
  "Non-nil means automatically switch to the first workgroup in a
file when the file is loaded."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-query-for-save-on-emacs-exit t
  "Non-nil means query for save before exiting Emacs when there
are unsaved changes.  Exiting workgroups removes its
`kill-emacs-query-functions' hook, so if you set this to nil, you
may want to set `wg-query-for-save-on-workgroups-exit' to t."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-query-for-save-on-workgroups-exit t
  "Non-nil means query for save before exiting workgroups when
there are unsaved changes. Exiting workgroups removes its
`kill-emacs-query-functions' hook, which is why this variable
exists."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-no-confirm nil
  "Non-nil means don't request confirmation before various
destructive operations.  This doesn't modify query-for-save
behavior.  Use `wg-query-for-save-on-workgroups-exit' and
`wg-query-for-save-on-emacs-exit' for that."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-kill-ring-size 20
  "Maximum length of the workgroups kill-ring."
  :type 'integer
  :group 'workgroups)

(defcustom wg-time-format "%H:%M:%S %A, %B %d %Y"
  "Format string for time display.
Passed to `format-time-string'."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-battery t
  "Non-nil mean include battery info in the time display."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-frame-morph-on t
  "Non-nil means use the frame wipe animation when switching to a
workgroup."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-frame-morph-hsteps 15
  "Windows are enlarged horizontally in increments of this
number of columns during frame morphing."
  :type 'integer
  :group 'workgroups)

(defcustom wg-frame-morph-vsteps 5
  "Windows are enlarged vertically in increments of this number
of rows during frame morphing."
  :type 'integer
  :group 'workgroups)

(defcustom wg-warning-timeout 0.7
  "Seconds to display minibuffer warning messages."
  :type 'float
  :group 'workgroups)

(defcustom wg-mode-line-on t
  "Toggles workgroups' mode-line display."
  :type 'boolean
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (force-mode-line-update)))

(defcustom wg-mode-line-left-brace "("
  "String on the left of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-right-brace ")"
  "String on the right of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-divider ":"
  "String between workgroups names in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-left-brace "( "
  "String on the left of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-right-brace " )"
  "String on the right of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-divider " | "
  "String between workgroups names in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-current-workgroup-left-decor "-<{ "
  "String displayed to the left of the current workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-current-workgroup-right-decor " }>-"
  "String displayed to the right of the current workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-previous-workgroup-left-decor "*"
  "String displayed to the left of the previous workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-previous-workgroup-right-decor "*"
  "String displayed to the right of the previous workgroup in the
list display."
  :type 'string
  :group 'workgroups)


;;; vars

(defvar wg-file nil
  "Current workgroups file.")

(defvar wg-list nil
  "List of currently defined workgroups.")

(defvar wg-frame-table (make-hash-table)
  "Hash table keyed by frame storing that frame's state.")

(defvar wg-dirty nil
  "Non-nil when there are unsaved changes.")

(defvar wg-kill-ring nil
  "Kill ring of saved configs.")

(defvar wg-face-abbrevs nil
  "Assoc list mapping face abbreviations to face names.")

(defvar wg-window-min-width 2 ;; 4
  "Bound to `window-min-width' when restoring configs. ")

(defvar wg-window-min-height 1 ;; 3
  "Bound to `window-min-height' when restoring configs.")

(defvar wg-window-pad 2
  "Added to the window-mins to prevent split size errors.")

(defvar wg-max-frame-morph-iterations 200
  "Max frame-morph iterations before exiting without finishing.")

(defvar wg-last-message nil
  "Holds the last message workgroups sent to the echo area.")


;;; faces

(defmacro wg-defface (face key spec doc &rest args)
  "`defface' wrapper adding a lookup key used by `wg-fontify'."
  (declare (indent 2))
  `(progn
     (pushnew (cons ,key ',face) wg-face-abbrevs :test 'equal)
     (defface ,face ,spec ,doc ,@args)))

(wg-defface wg-current-workgroup-face :cur
  '((((class color)) (:foreground "white")))
  "Face used for the current workgroup in the list display."
  :group 'workgroups)

(wg-defface wg-previous-workgroup-face :prev
  '((((class color)) (:foreground "light sky blue")))
  "Face used for the previous workgroup in the list display."
  :group 'workgroups)

(wg-defface wg-other-workgroup-face :other
  '((((class color)) (:foreground "light slate grey")))
  "Face used for other workgroups in the list display."
  :group 'workgroups)

(wg-defface wg-command-face :cmd
  '((((class color)) (:foreground "aquamarine")))
  "Face used for command/operation names."
  :group 'workgroups)

(wg-defface wg-divider-face :div
  '((((class color)) (:foreground "light slate blue")))
  "Face used for dividers."
  :group 'workgroups)

(wg-defface wg-brace-face :brace
  '((((class color)) (:foreground "light slate blue")))
  "Face used for left and right braces."
  :group 'workgroups)

(wg-defface wg-message-face :msg
  '((((class color)) (:foreground "light sky blue")))
  "Face used for messages."
  :group 'workgroups)

(wg-defface wg-mode-line-face :mode
  '((((class color)) (:foreground "light sky blue")))
  "Face used for mode-line display."
  :group 'workgroups)

(wg-defface wg-filename-face :file
  '((((class color)) (:foreground "light sky blue")))
  "Face used for the filenames."
  :group 'workgroups)

(wg-defface wg-frame-face :frame
  '((((class color)) (:foreground "white")))
  "Face used for frame names."
  :group 'workgroups)


;;; utils

(defmacro wg-dbind (args expr &rest body)
  "Wrapper to shorten `destructuring-bind'."
  (declare (indent 2))
  `(destructuring-bind ,args ,expr ,@body))

(defmacro wg-dohash (spec &rest body)
  "do-style wrapper for `maphash'."
  (declare (indent 1))
  (wg-dbind (key val table &optional ret) spec
    `(progn (maphash (lambda (,key ,val) ,@body) ,table)
            ,ret)))

(defmacro wg-doconcat (spec &rest body)
  "do-style wrapper for `mapconcat'."
  (declare (indent 1))
  (wg-dbind (elt seq &optional sep) spec
    `(mapconcat (lambda (,elt) ,@body) ,seq (or ,sep ""))))

(defmacro wg-docar (spec &rest body)
  "do-style wrapper for `mapcar'."
  (declare (indent 1))
  `(mapcar (lambda (,(car spec)) ,@body) ,(cadr spec)))

(defmacro wg-when-let (binds &rest body)
  "Like `let*', but only eval BODY when all BINDS are non-nil."
  (declare (indent 1))
  (let ((bind (car binds)))
    (when (consp bind)
      `(let (,bind)
         (when ,(car bind)
           ,(if (not (cdr binds)) `(progn ,@body)
              `(wg-when-let ,(cdr binds) ,@body)))))))

(defmacro until (test &rest body)
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

(defun wg-take (list n)
  "Return a list of the first N elts in LIST."
  (butlast list (- (length list) n)))

(defun wg-partition (list n &optional step)
  "Return list of N-length sublists of LIST, offset by STEP.
Iterative to prevent stack overflow."
  (let (acc)
    (while list
      (push (wg-take list n) acc)
      (setq list (nthcdr (or step n) list)))
    (nreverse acc)))

(defun wg-insert-elt (elt list &optional pos)
  "Insert ELT into LIST at POS or the end."
  (let* ((len (length list)) (pos (or pos len)))
    (when (and (>= pos 0) (<= pos len))
      (append (wg-take list pos) (cons elt (nthcdr pos list))))))

(defun wg-move-elt (elt list pos)
  "Move ELT to position POS in LIST."
  (when (member elt list)
    (wg-insert-elt elt (remove elt list) pos)))

(defun wg-cyclic-offset-elt (elt list n)
  "Cyclically offset ELT's position in LIST by N."
  (wg-when-let ((pos (position elt list)))
    (wg-move-elt elt list (mod (+ n pos) (length list)))))

(defun wg-cyclic-nth-from-elt (elt list n)
  "Return the elt in LIST N places cyclically from ELT.
If ELT is not present is LIST, return nil."
  (wg-when-let ((pos (position elt list)))
    (nth (mod (+ pos n) (length list)) list)))

(defun wg-util-swap (elt1 elt2 list)
  "Return a copy of LIST with ELT1 with ELT2 swapped.
Return nil when ELT1 and ELT2 aren't both present."
  (wg-when-let ((p1 (position elt1 list))
                (p2 (position elt2 list)))
    (wg-move-elt elt1 (wg-move-elt elt2 list p1) p2)))

(defun wg-aget (alist key)
  "Return the value of KEY in ALIST. Uses `assq'."
  (cdr (assq key alist)))

(defun wg-aput (alist key val)
  "Return a copy of ALIST with a KEY VAL pair added.
First removes KEY's previous key-value-pair if it's present."
  (cons (cons key val) (remove (assq key alist) alist)))

(defun wg-aset (alist key val)
  "Set KEY's value to VAL in ALIST.
Error if KEY doesn't exist in ALIST."
  (setcdr (assq key alist) val))

(defun wg-get-alist (key val alist-list)
  "Return the first alist in ALIST-LIST containing KEY and VAL."
  (catch 'res
    (dolist (alist alist-list)
      (when (equal val (cdr (assoc key alist)))
        (throw 'res alist)))))

(defmacro wg-abind (alist binds &rest body)
  "Bind values in ALIST to symbols in BINDS, then eval BODY.
If an elt of BINDS is a symbol, it is used as both the bound var
and the key.  If it is a cons, the car is the bound var, and the
cadr is the key."
  (declare (indent 2))
  (let ((asym (gensym)))
    `(let* ((,asym ,alist)
            ,@(mapcar (lambda (bind)
                        (let ((c (consp bind)))
                          `(,(if c (car bind) bind)
                            (wg-aget ,asym ',(if c (cadr bind) bind)))))
                      binds))
       ,@body)))

(defun wg-step-to (n m step)
  "Increment or decrement N toward M by STEP."
  (cond ((= n m) n)
        ((< n m) (min (+ n step) m))
        ((> n m) (max (- n step) m))))

(defmacro wg-fill-keymap (keymap &rest binds)
  "Fill KEYMAP with BINDS."
  (declare (indent 1))
  (let ((km (gensym)))
    `(let ((,km ,keymap))
       ,@(mapcar (lambda (b) `(define-key ,km (kbd ,(car b)) ,(cadr b)))
                 (wg-partition binds 2))
       ,km)))

(defun wg-save-sexp-to-file (sexp file)
  "Write the printable representation of SEXP to FILE."
  (with-temp-buffer
    (insert (format "%S" sexp))
    (write-file file)))

(defun wg-read-sexp-from-file (file)
  "Read and s-expression from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))


;;; workgroups utils

(defun wg-type-of (obj)
  "Return the (workgroups) type of OBJ."
  (wg-aget obj 'type))

(defun wg-type-p (type obj)
  "Return t if OBJ is of type TYPE, nil otherwise."
  (and (consp obj) (eq type (wg-type-of obj))))

(defun wg-type-check (type obj &optional noerror)
  "Throw an error if OBJ is not of type TYPE."
  (or (wg-type-p type obj)
      (unless noerror
        (error "%s is not of type %s" obj type))))

(defun wg-frame-offset (&optional n frame)
  "Return the frame N frames away from FRAME cyclically.
N defaults to 1, and FRAME defaults to `selected-frame'."
  (let ((fl (frame-list)))
    (nth (mod (+ (position (or frame (selected-frame)) fl)
                 (or n 1))
              (length fl))
         fl)))

(defun wg-window-list (&optional frame)
  "Flatten `window-tree' into a stable list.
`window-list' can't be used because its order isn't stable."
  (flet ((inner (obj) (if (atom obj) (list obj)
                        (mapcan 'inner (cddr obj)))))
    (inner (car (window-tree (or frame (selected-frame)))))))

(defun wg-add-face (facekey str)
  "Return a copy of STR fontified according to FACEKEY.
FACEKEY must be a key in `wg-face-abbrevs'."
  (let ((face (wg-aget wg-face-abbrevs facekey))
        (str  (copy-seq str)))
    (unless face (error "No face with key %s" facekey))
    (if (not wg-use-faces) str
      (put-text-property 0 (length str) 'face face str)
      str)))

(defmacro wg-fontify (&rest format)
  "A small fontification DSL."
  `(concat
    ,@(mapcar
       (lambda (spec)
         (typecase spec
           (cons (if (keywordp (car spec))
                     `(wg-add-face
                       ,(car spec)
                       ,(if (stringp (cadr spec))
                            (cadr spec)
                          `(format "%s" ,(cadr spec))))
                   `(progn ,spec)))
           (string `(progn ,spec))
           (atom `(format "%s" ,spec))))
       format)))


;;; types

(defun wg-window-p (obj)
  "Return t if OBJ is a workgroups window, nil otherwise."
  (wg-type-p 'window obj))

(defun wg-wtree-p (obj)
  "Return t if OBJ is a workgroups window tree, nil otherwise."
  (wg-type-p 'wtree obj))

(defun wg-wconfig-p (obj)
  "Return t if OBJ is a workgroups window config, nil otherwise."
  (wg-type-p 'wconfig obj))

(defun wg-workgroup-p (obj)
  "Return t if OBJ is a workgroup, nil otherwise."
  (wg-type-p 'workgroup obj))


;;; frame ops

(defmacro wg-with-frame-state (frame state &rest body)
  "Bind FRAME and STATE in BODY.
FRAME is bound to `wg-frame', and STATE is bound to
FRAME's value in `wg-frame-table'."
  (declare (indent 2))
  `(let* ((,frame (selected-frame))
          (,state (or (gethash ,frame wg-frame-table)
                      (puthash ,frame (make-hash-table)
                               wg-frame-table))))
     ,@body))

(defun wg-frame-val (key)
  "Return KEY's value in `wg-frame-table'."
  (wg-with-frame-state frame state
    (gethash key state)))

(defun wg-set-frame-val (key val)
  "Set KEY to VAL in `wg-frame-table'."
  (wg-with-frame-state frame state
    (puthash key val state)))

(defun wg-delete-frame-key (key)
  "Remove KEY from frame's entry in `wg-frame-table'."
  (wg-with-frame-state frame state
    (remhash key state)))

(defun wg-delete-frame (frame)
  "Remove FRAME from `wg-frame-table'."
  (remhash frame wg-frame-table))


;; window config utils

(defun wg-min-hsplit ()
  "Return the sum of `wg-window-pad' and `wg-window-min-width'.
This is the minimum width when splitting."
  (+ wg-window-pad wg-window-min-width))

(defun wg-min-vsplit ()
  "Return the sum of `wg-window-pad' and `wg-window-min-height'.
This is the minimum height when splitting."
  (+ wg-window-pad wg-window-min-height))

(defun wg-min-split (dir)
  "Return the minimum split size in direction DIR."
  (if dir (wg-min-vsplit) (wg-min-hsplit)))

(defvar wg-min-win
  `((type . window)
    (edges 0 0 ,(wg-min-hsplit) ,(wg-min-vsplit)))
  "Smallest possible window.")

(defun wg-set-edges (w edges)
  "Set the edges of W to EDGES."
  (wg-aput w 'edges edges))

(defun wg-bounds (w dir)
  "Return the edges of W in bounds-form from direction DIR.
W should be a window or a wtree."
  (wg-dbind (l1 t1 r1 b1) (wg-aget w 'edges)
    (if dir (list l1 r1 t1 b1) (list t1 b1 l1 r1))))

(defun wg-set-bounds (w dir ls hs lb hb)
  "Set W's edges in bounds-form to LS HS LB HB in direction DIR.
W should be a window or a wtree."
  (wg-set-edges w (if dir (list ls lb hs hb) (list lb ls hb hs))))

(defun wg-edges-equal (w1 w2)
  "Return t when W1's edges `equal' W2's, nil otherwise.
W1 and W2 should be windows or wtrees."
  (equal (wg-aget w1 'edges) (wg-aget w2 'edges)))

(defun wg-first-win (w)
  "Return the first actual window in W.
W should be a window or a wtree."
  (if (wg-window-p w) w
    (wg-first-win (car (wg-aget w 'wlist)))))

(defun wg-wsize (w &optional width)
  "Return the width or height of W, calculated from its edge list.
W should be a window or a wtree."
  (wg-dbind (ls hs lb hb) (wg-bounds w (not width))
    (- hb lb)))

(defun wg-actual-size-of-w (w &optional width)
  "Return the actual size of W, calculated from its components.
W should be a window or a wtree."
  (if (wg-window-p w) (wg-wsize w width)
    (wg-abind w (dir wlist)
      (if (or (and width (not dir)) (and (not width) dir))
          (reduce '+ wlist :key (lambda (w) (wg-actual-size-of-w w width)))
        (apply 'max (mapcar (lambda (w) (wg-actual-size-of-w w width))
                            wlist))))))

(defun wg-wtrees-equal (w1 w2)
  "Return t when W1 and W2 have equal structure.
W1 and W2 should be windows or wtrees."
  (cond ((and (wg-window-p w1) (wg-window-p w2))
         (wg-edges-equal w1 w2))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (and (eq (wg-aget w1 'dir) (wg-aget w2 'dir))
              (equal (wg-aget w1 'edges) (wg-aget w2 'edges))
              (every 'wg-wtrees-equal
                     (wg-aget w1 'wlist) (wg-aget w2 'wlist))))))

(defmacro wg-map-wtree (binds &rest body)
  "Prettifies mapping the window list of a wtree."
  (declare (indent 1))
  (wg-dbind (subw wtree &optional mapfn) binds
    (let ((wl (gensym)))
      `(let ((,wl (remove nil (,(or mapfn 'mapcar) (lambda (,subw) ,@body)
                               (wg-aget ,wtree 'wlist)))))
         (if (not (cdr ,wl)) (car ,wl)
           (wg-aput ,wtree 'wlist ,wl))))))

(defun wg-set-frame-param (wconfig param &optional frame)
  "Set WCONFIG parameter PARAM in FRAME."
  (set-frame-parameter frame param (wg-aget wconfig param)))

(defun wg-set-frame-position-from-wconfig (wconfig &optional force frame)
  "Set FRAME's position from WCONFIG's position."
  (when (or force wg-restore-position)
    (wg-set-frame-param wconfig 'left)
    (wg-set-frame-param wconfig 'top)))

(defun wg-set-frame-size-from-wconfig (wconfig &optional force frame)
  "Set FRAME's size from WCONFIG's size."
  (when (or force wg-restore-size)
    (wg-set-frame-param wconfig 'width)
    (wg-set-frame-param wconfig 'height)))


;;; window config making

(defun wg-make-window (winobj)
  "Return a workgroups window from WINOBJ."
  (with-current-buffer (window-buffer winobj)
    `((type     .   window)
      (edges    .  ,(window-edges winobj))
      (bname    .  ,(buffer-name))
      (fname    .  ,(buffer-file-name))
      (point    .  ,(if (= (point) (point-max)) :max (point)))
      (mark     .  ,(mark))
      (markx    .  ,mark-active)
      (wstart   .  ,(window-start winobj))
      (sbars    .  ,(window-scroll-bars winobj))
      (margins  .  ,(window-margins winobj))
      (fringes  .  ,(window-fringes winobj)))))

(defun wg-make-wtree (dir edges wlist)
  "Return a workgroups window tree from WTREE."
  `((type   .   wtree)
    (dir    .  ,dir)
    (edges  .  ,edges)
    (wlist  .  ,wlist)))

(defun wg-make-wtree-from-ewtree (ewtree)
  "Return a workgroups wtree from EWTREE.
EWTREE is an Emacs `window-tree'."
  (etypecase ewtree
    (window (wg-make-window ewtree))
    (cons   (wg-dbind (dir edges . wins) ewtree
              (wg-make-wtree
               dir edges (mapcar 'wg-make-wtree-from-ewtree
                                 (cddr ewtree)))))))

(defun wg-make-wconfig ()
  "Return a workgroups window config."
  (when (active-minibuffer-window)
    (error "Workgroups can't make window configs \
while the minibuffer is active"))
  (let ((wl (wg-window-list)))
    (message nil)
    `((type     .   wconfig)
      (left     .  ,(frame-parameter nil 'left))
      (top      .  ,(frame-parameter nil 'top))
      (width    .  ,(frame-parameter nil 'width))
      (height   .  ,(frame-parameter nil 'height))
      (sbars    .  ,(frame-parameter nil 'vertical-scroll-bars))
      (sbwid    .  ,(frame-parameter nil 'scroll-bar-width))
      (swidx    .  ,(position (selected-window) wl))
      (mbswidx  .  ,(position minibuffer-scroll-window wl))
      (wtree    .  ,(wg-make-wtree-from-ewtree (car (window-tree)))))))

(defun wg-make-blank-wconfig (&optional buffer)
  "Return a new default config.
The config displays BUFFER or `wg-default-buffer'."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer wg-default-buffer))
    (wg-make-wconfig)))


;;; window config restoring

(defun wg-restore-window (window)
  "Restore WINDOW's state in `selected-window'."
  (wg-abind window
      (fname bname point mark markx wstart sbars fringes margins)
    (switch-to-buffer
     (or (and fname (file-exists-p fname) (find-file-noselect fname))
         (get-buffer bname)
         wg-default-buffer))
    (unless (equal (buffer-name) wg-default-buffer)
      (rename-buffer bname))
    (when wg-restore-scroll-bars
      (wg-dbind (w c v h) sbars
        (set-window-scroll-bars nil w v h)))
    (when wg-restore-fringes
      (wg-dbind (lwid rwid outside) fringes
        (set-window-fringes nil lwid rwid outside)))
    (when wg-restore-margins
      (wg-dbind (left . right) margins
        (set-window-margins nil left right)))
    (set-window-start nil wstart t)
    (goto-char (if (eq point :max) (point-max) point))
    (set-mark mark)
    (unless markx (deactivate-mark))
    (when (>= wstart (point-max)) (recenter))))

(defun wg-restore-wtree (wtree)
  "Restore WTREE's window layout in FRAME."
  (cond ((wg-window-p wtree)
         (wg-restore-window wtree)
         (other-window 1))
        ((wg-wtree-p wtree)
         (let* ((hor (not (wg-aget wtree 'dir)))
                (wlist (wg-aget wtree 'wlist))
                (lastwin (car (last wlist))))
           (dolist (win wlist)
             (unless (eq win lastwin)
               (split-window nil (wg-wsize win hor) hor))
             (wg-restore-wtree win))))))

(defun wg-restore-wconfig (wconfig)
  "Restore WCONFIG."
  (when (active-minibuffer-window)
    (error "Can't restore configs while the minibuffer is active"))
  (let ((window-min-width  wg-window-min-width)
        (window-min-height wg-window-min-height))
    (wg-abind wconfig (sbars sbwid swidx mbswidx wtree)
      (wg-set-frame-position-from-wconfig wconfig)
      (wg-set-frame-size-from-wconfig wconfig)
      (when wg-restore-scroll-bars
        (set-frame-parameter nil 'vertical-scroll-bars sbars)
        (set-frame-parameter nil 'scroll-bar-width sbwid))
      (delete-other-windows)
      (wg-restore-wtree wtree)
      (when (or wg-restore-selected-window wg-restore-mbs-window)
        (let ((wl (wg-window-list)))
          (when wg-restore-selected-window
            (set-frame-selected-window nil (nth swidx wl)))
          (when (and wg-restore-mbs-window mbswidx)
            (setq minibuffer-scroll-window (nth mbswidx wl))))))))

(defun wg-restore-blank-wconfig ()
  "Restore the default config."
  (wg-restore-wconfig (wg-make-blank-wconfig)))


;;; frame morph

(defun wg-step-edges (w1 w2)
  "Return a new edge list stepped from  W1 toward W2.
W1 and W2 can be either windows or wtrees."
  (let ((h wg-frame-morph-hsteps)
        (v wg-frame-morph-vsteps))
    (mapcar* 'wg-step-to
             (wg-aget w1 'edges)
             (wg-aget w2 'edges)
             (list h v h v))))

(defun wg-step-wins (w1 w2 &optional swap)
  "Return a copy of W1 with its edges stepped toward W2.
When SWAP is non-nil, return a copy of W2 instead."
  (wg-set-edges (if swap w2 w1) (wg-step-edges w1 w2)))

(defun wg-minify-win (win)
  "Return a copy of WIN with the smallest allowable dimensions."
  (wg-set-edges
   win (wg-dbind (left top right bottom) (wg-aget win 'edges)
         `(,left ,top
                 ,(+ left (wg-min-hsplit))
                 ,(+ top  (wg-min-vsplit))))))

(defun wg-trim-wtree (w)
  "Return a new wtree from W with all subwins within W's bounds.
Windows that begin outside of W's bounds are removed. W should be
a window or a wtree."
  (if (wg-window-p w) w
    (wg-abind w (dir)
      (wg-dbind (ls1 hs1 hi hb1) (wg-bounds w dir)
        (let ((lim (- hb1 (wg-min-split dir))))
          (wg-map-wtree (sw w)
            (when (< hi lim)
              (wg-trim-wtree
               (wg-dbind (ls2 hs2 lb2 hb2) (wg-bounds sw dir)
                 (let ((low hi))
                   (setq hi (min lim (+ hi (- hb2 lb2))))
                   (wg-set-bounds sw dir ls1 hs1 low hi)))))))))))

(defun wg-wipe-wtree (w win)
  "Grow the first window of W and its subtrees one step toward WIN.
Shrink windows and wtrees that aren't first in a wlist.  This
eventually wipes the rest of W's components, leaving only a
window.  W should be window or a wtree, and WIN should be a
window."
  (if (wg-window-p w) (wg-step-wins w win t)
    (wg-make-wtree
     (wg-aget w 'dir)
     (wg-step-edges w win)
     (wg-dbind (fwin . wins) (wg-aget w 'wlist)
       (cons (wg-wipe-wtree fwin win)
             (wg-docar (sw wins)
               (if (wg-window-p sw) (wg-step-wins sw wg-min-win)
                 (wg-wipe-wtree sw win))))))))

(defun wg-lengthen-wlist (wl1 wl2)
  "Return a copy of WL1 equal in length to WL2.
Pad WL1 with corresponding minified windows from WL2 as
necessary.  WL1 and WL2 should be window lists."
  (when wl2
    (cons (or (car wl1) (wg-minify-win (wg-first-win (car wl2))))
          (wg-lengthen-wlist (cdr wl1) (cdr wl2)))))

(defun wg-win->wtree (win wt)
  "Return a new wtree from WIN with WT's toplevel structure.
WIN should be a window, and WT should be a wtree."
  (wg-make-wtree
   (wg-aget wt 'dir)
   (wg-step-edges win wt)
   (mapcar (lambda (w) (wg-minify-win (wg-first-win w)))
           (wg-aget wt 'wlist))))

(defun wg-wtree->win (wt win)
  "Step WT toward WIN with `wg-wipe-wtree' and `wg-trim-wtree'.
WT should be a wtree, and WIN should be a window.  Return a wtree
on intermediate steps, or a window on the last step."
  (wg-trim-wtree (wg-wipe-wtree wt win)))

(defun wg-wtree->wtree (wt1 wt2)
  "Return a new wtree morphed one step from WT1 to WT2.
Mutually recursive with `wg-fm-dispatch' to traverse the
structures of WT1 and WT2 looking for discrepancies.  WT1 and WT2
should be wtrees."
  (wg-abind wt1 ((d1 dir) (wl1 wlist))
    (wg-abind wt2 ((d2 dir) (wl2 wlist))
      (wg-make-wtree
       d2 (wg-step-edges wt1 wt2)
       (if (eq d1 d2)
           (mapcar* 'wg-fm-dispatch (wg-lengthen-wlist wl1 wl2) wl2)
         (list (wg-minify-win (wg-first-win wt2)) wt1))))))

(defun wg-fm-dispatch (w1 w2)
  "Return a wtree morphed one step from W1 toward W2.
W1 and W2 should be windows or wtrees.  Dispatch on each possible
combination of types."
  (cond ((and (wg-window-p w1) (wg-window-p w2))
         (wg-step-wins w1 w2 t))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (wg-wtree->wtree w1 w2))
        ((and (wg-window-p w1) (wg-wtree-p w2))
         (wg-win->wtree w1 w2))
        ((and (wg-wtree-p w1) (wg-window-p w2))
         (wg-wtree->win w1 w2))))

(defun wg-frame-morph (to-wconfig)
  "Morph the current window config into TO-WCONFIG.
TO-WCONFIG should be a wconfig."
  (wg-set-frame-size-from-wconfig     to-wconfig)
  (wg-set-frame-position-from-wconfig to-wconfig)
  (let* ((from-wconfig (wg-make-wconfig))
         (from-wt (wg-aget from-wconfig 'wtree))
         (to-wt (wg-aget to-wconfig 'wtree))
         (wg-restore-margins           nil)
         (wg-restore-fringes           nil)
         (wg-restore-scroll-bars       nil)
         (wg-restore-selected-window   nil)
         (wg-restore-mbs-window        nil)
         (watchdog-counter 0))
    (condition-case err
        (until (or (wg-wtrees-equal from-wt to-wt)
                   (> (incf watchdog-counter)
                      wg-max-frame-morph-iterations))
          (setq from-wt (wg-fm-dispatch from-wt to-wt))
          (wg-aset from-wconfig 'wtree from-wt)
          (wg-restore-wconfig from-wconfig)
          (redisplay t))
      (error (message "workgroups frame-morph: %s" err)))))


;;; global error wrappers

(defun wg-file (&optional noerror)
  "Return `wg-file'."
  (or wg-file
      (unless noerror
        (error "Workgroups isn't visiting a file"))))

(defun wg-list (&optional noerror)
  "Return `wg-list'."
  (or wg-list
      (unless noerror
        (error "No workgroups are defined."))))

(defun wg-get-workgroup (key val &optional noerror)
  "Return the workgroup whose KEY equals VAL."
  (or (wg-get-alist key val (wg-list noerror))
      (unless noerror
        (error "There is no workgroup with an %S of %S" key val))))


;;; workgroup property ops

(defun wg-get-workgroup-prop (prop workgroup)
  "Return PROP's value in WORKGROUP."
  (wg-type-check 'workgroup workgroup)
  (wg-aget workgroup prop))

(defun wg-set-workgroup-prop (prop val workgroup &optional nodirty)
  "Set PROP to VAL in WORKGROUP."
  (wg-type-check 'workgroup workgroup)
  (wg-aset workgroup prop val)
  (unless nodirty (setq wg-dirty t)))

(defun wg-uid (workgroup)
  "Return WORKGROUP's uid."
  (wg-get-workgroup-prop 'uid workgroup))

(defun wg-set-uid (workgroup uid)
  "Set the uid of WORKGROUP to UID."
  (wg-set-workgroup-prop 'uid uid workgroup))

(defun wg-uids (&optional noerror)
  "Return a list of workgroups uids."
  (mapcar 'wg-uid (wg-list noerror)))

(defun wg-new-uid ()
  "Return a uid greater than any in `wg-list'."
  (let ((uids (wg-uids t)) (new -1))
    (dolist (uid uids (1+ new))
      (setq new (max uid new)))))

(defun wg-name (workgroup)
  "Return the name of WORKGROUP."
  (wg-get-workgroup-prop 'name workgroup))

(defun wg-set-name (workgroup name)
  "Set the name of WORKGROUP to NAME."
  (wg-set-workgroup-prop 'name name workgroup))

(defun wg-names (&optional noerror)
  "Return a list of workgroup names."
  (mapcar 'wg-name (wg-list noerror)))


;;; current and previous workgroup ops

(defun wg-get-frame-workgroup (key &optional noerror)
  "Return the workgroup under KEY in `wg-frame-table'."
  (or (wg-frame-val key)
      (unless noerror
        (error "There's no %s in the frame" key))))

(defun wg-current-workgroup (&optional noerror)
  "Return the current workgroup."
  (wg-get-frame-workgroup 'current-workgroup noerror))

(defun wg-set-current-workgroup (workgroup)
  "Set the current workgroup to WORKGROUP."
  (wg-set-frame-val 'current-workgroup workgroup))

(defun wg-previous-workgroup (&optional noerror)
  "Return the previous workgroup."
  (wg-get-frame-workgroup 'previous-workgroup noerror))

(defun wg-set-previous-workgroup (workgroup)
  "Set the previous workgroup to WORKGROUP."
  (wg-set-frame-val 'previous-workgroup workgroup))


;;; base and working configs

(defun wg-set-base-config (workgroup config)
  "Set the base config of WORKGROUP to CONFIG."
  (wg-set-workgroup-prop 'wconfig config workgroup))

(defun wg-base-config (workgroup)
  "Return the base config of WORKGROUP."
  (wg-get-workgroup-prop 'wconfig workgroup))

(defun wg-set-working-config (workgroup config)
  "Set the working config of WORKGROUP to CONFIG."
  (wg-set-frame-val (wg-uid workgroup) config))

(defun wg-update-working-config (workgroup)
  "Set WORKGROUP's working config to the current window config."
  (wg-set-working-config workgroup (wg-make-wconfig)))

(defun wg-working-config (workgroup)
  "Return the working config of WORKGROUP.
If WORKGROUP is the current workgroup, update it first."
  (when (eq workgroup (wg-current-workgroup t))
    (wg-update-working-config workgroup))
  (or (wg-frame-val (wg-uid workgroup))
      (wg-base-config workgroup)))


;;; workgroup making and restoring

(defun wg-make-workgroup (uid name wconfig)
  "Make a workgroup named NAME from BASE and WORKING."
  `((type     .   workgroup)
    (uid      .  ,uid)
    (name     .  ,name)
    (wconfig  .  ,wconfig)))

(defun wg-make-default-workgroup (name)
  "Return a new workgroup named NAME from the current wconfig."
  (wg-make-workgroup nil name (wg-make-wconfig)))

(defun wg-make-blank-workgroup (name &optional buffer)
  "Return a new default workgroup named NAME."
  (wg-make-workgroup
   nil name (wg-make-blank-wconfig buffer)))

(defun wg-restore-workgroup (workgroup &optional base)
  "Restore WORKGROUP."
  (wg-restore-wconfig
   (if base (wg-base-config workgroup)
     (wg-working-config workgroup))))


;;; workgroups list ops

(defun wg-delete (workgroup)
  "Remove WORKGROUP from `wg-list'.
Also delete all references to it in `wg-frame-table'."
  (wg-dohash (frame state wg-frame-table)
    (with-selected-frame frame
      (wg-delete-frame-key (wg-uid workgroup))
      (when (eq workgroup (wg-current-workgroup t))
        (wg-set-current-workgroup nil))
      (when (eq workgroup (wg-previous-workgroup t))
        (wg-set-previous-workgroup nil))))
  (setq wg-dirty t wg-list (remove workgroup (wg-list))))

(defun wg-add (new &optional pos)
  "Add WORKGROUP to `wg-list'.
If a workgroup with the same name exists, overwrite it."
  (wg-awhen (wg-get-workgroup 'name (wg-name new) t)
    (unless pos (setq pos (position it wg-list)))
    (wg-delete it))
  (wg-set-uid new (wg-new-uid))
  (setq wg-dirty t wg-list (wg-insert-elt new wg-list pos)))

(defun wg-check-and-add (workgroup)
  "Add WORKGROUP to `wg-list'.
Query to overwrite if a workgroup with the same name exists."
  (let ((name (wg-name workgroup)))
    (when (wg-get-workgroup 'name name t)
      (unless (or wg-no-confirm
                  (y-or-n-p (format "%S exists. Overwrite? " name)))
        (error "Cancelled"))))
  (wg-add workgroup))

(defun wg-cyclic-offset-workgroup (workgroup n)
  "Offset WORKGROUP's position in `wg-list' by n."
  (wg-aif (wg-cyclic-offset-elt workgroup (wg-list) n)
      (setq wg-list it wg-dirty t)
    (error "Workgroup isn't present in `wg-list'.")))

(defun wg-list-swap (w1 w2)
  "Swap W1 and W2 in `wg-list'."
  (when (eq w1 w2) (error "Can't swap a workgroup with itself"))
  (wg-aif (wg-util-swap w1 w2 (wg-list))
      (setq wg-list it wg-dirty t)
    (error "Both workgroups aren't present in `wg-list'.")))


;;; buffer list ops

(defun wg-config-buffer-list (wconfig)
  "Return the names of all unique buffers in WTREE."
  (let (bufs)
    (flet ((inner
            (win)
            (cond ((wg-window-p win)
                   (pushnew (wg-aget win 'bname) bufs :test 'equal))
                  ((wg-wtree-p win)
                   (mapc 'inner (wg-aget win 'wlist))))))
      (inner (wg-aget wconfig 'wtree))
      bufs)))

(defun wg-workgroup-buffer-list (workgroup)
  "Return the names of all unique buffers in WORKGROUP."
  (let ((bufs (wg-config-buffer-list
               (wg-working-config workgroup))))
    (dolist (buf (wg-config-buffer-list
                  (wg-base-config workgroup)) bufs)
      (unless (member buf bufs) (push buf bufs)))))

(defun wg-buffer-list ()
  "Return the names of all unique buffers in `wg-list'."
  (let (bufs)
    (dolist (w (wg-list) (nreverse bufs))
      (dolist (buf (wg-workgroup-buffer-list w))
        (unless (member buf bufs) (push buf bufs))))))

(defun wg-find-buffer (buf)
  "Return the workgroup and config flag that contains BUF."
  (catch 'result
    (dolist (w (wg-list))
      (cond ((member buf (wg-config-buffer-list
                          (wg-working-config w)))
             (throw 'result (list w nil)))
            ((member buf (wg-config-buffer-list
                          (wg-base-config w)))
             (throw 'result (list w t)))))))


;;; mode-line

(defun wg-mode-line-string ()
  "Return the string to be displayed in the mode-line."
  (let ((cur (wg-current-workgroup t)))
    (wg-fontify
     (:div wg-mode-line-left-brace)
     (:mode (when cur (position cur (wg-list t))))
     (:div wg-mode-line-divider)
     (:mode (when cur (wg-name cur)))
     (:div wg-mode-line-right-brace))))

(defun wg-mode-line-add-display ()
  "Add workgroups' mode-line format to `mode-line-format'."
  (unless (assq 'wg-mode-line-on mode-line-format)
    (let ((format `(wg-mode-line-on (:eval (wg-mode-line-string))))
          (pos (1+ (position 'mode-line-position mode-line-format))))
      (set-default 'mode-line-format
                   (wg-insert-elt format mode-line-format pos)))))

(defun wg-mode-line-remove-display ()
  "Remove workgroups' mode-line format from `mode-line-format'."
  (wg-awhen (assq 'wg-mode-line-on mode-line-format)
    (set-default 'mode-line-format (remove it mode-line-format))
    (force-mode-line-update)))


;;; minibuffer reading

(defun wg-completing-read (prompt choices &rest args)
  "Call `ido-completing-read' or `completing-read'."
  (apply (if (and (boundp 'ido-mode) ido-mode)
             'ido-completing-read
           'completing-read) prompt choices args))

(defun wg-read-workgroup (&optional noerror)
  "Read a workgroup with `wg-completing-read'."
  (wg-get-workgroup
   'name (wg-completing-read "Workgroup: " (wg-names))
   noerror))

(defun wg-read-name (&optional prompt)
  "Read a non-empty name from the minibuffer."
  (let ((prompt (or prompt "Name: ")) (name nil)
        (warning (wg-fontify :msg "Name must be non-empty")))
    (while (and (setq name (read-from-minibuffer prompt))
                (equal name ""))
      (message warning)
      (sit-for wg-warning-timeout))
    name))

(defun wg-read-idx (low high &optional prompt)
  "Read and return a valid workgroup index."
  (let ((prompt (or prompt (format "[%d-%d]: " low high))) i
        (warning (wg-fontify
                  :msg (format "Enter an integer [%d-%d]" low high))))
    (while (and (setq i (eval-minibuffer prompt))
                (or (not (integerp i)) (< i low) (> i high)))
      (message warning)
      (sit-for wg-warning-timeout))
    i))

(defun wg-read-buffer ()
  "Read and return a buffer from `wg-buffer-list'."
  (wg-completing-read "Workgroup buffers: " (wg-buffer-list)))


;;; messaging

(defun wg-msg (format-string &rest args)
  "`message' and save the msg to `wg-last-message'."
  (setq wg-last-message (apply 'message format-string args)))

(defmacro wg-fontified-msg (&rest format)
  "`wg-fontify' FORMAT and call `wg-msg' on it."
  (declare (indent defun))
  `(wg-msg (wg-fontify ,@format)))


;;; command utils

(defun wg-arg (&optional reverse noerror)
  "Return a workgroup one way or another.
For use in interactive forms.  If `current-prefix-arg' is nil
return the current workgroups.  Otherwise read a workgroup from
the minibuffer.  If REVERSE is non-nil, `current-prefix-arg''s
begavior is reversed."
  (wg-list noerror)
  (if (if reverse (not current-prefix-arg) current-prefix-arg)
      (wg-read-workgroup noerror)
    (wg-current-workgroup noerror)))

(defun wg-add-to-kill-ring (config)
  "Add CONFIG to `wg-kill-ring'."
  (push config wg-kill-ring)
  (setq wg-kill-ring (wg-take wg-kill-ring wg-kill-ring-size)))

(defun wg-disp ()
  "Return a string of the names of all workgroups."
  (let ((wl    (wg-list t))
        (cur   (wg-current-workgroup  t))
        (prev  (wg-previous-workgroup t))
        (div   (wg-add-face :div wg-display-divider))
        (cld   wg-display-current-workgroup-left-decor)
        (crd   wg-display-current-workgroup-right-decor)
        (pld   wg-display-previous-workgroup-left-decor)
        (prd   wg-display-previous-workgroup-right-decor)
        (i     -1))
    (wg-fontify
     (:brace wg-display-left-brace)
     (if (not wl) (wg-fontify (:msg "No workgroups are defined"))
       (wg-doconcat (w wl div)
         (let ((str (format "%d: %s" (incf i) (wg-name w))))
           (cond ((eq w cur)
                  (wg-fontify (:cur (concat cld str crd))))
                 ((eq w prev)
                  (wg-fontify (:prev (concat pld str prd))))
                 (t (wg-fontify (:other str)))))))
     (:brace wg-display-right-brace))))

(defun wg-cyclic-nth-from-workgroup (&optional workgroup n)
  "Return the workgroup N places from WORKGROUP."
  (wg-when-let ((wg (or workgroup (wg-current-workgroup t))))
    (wg-cyclic-nth-from-elt wg (wg-list) (or n 1))))


;;; commands

(defun wg-switch-to-workgroup (workgroup &optional base)
  "Switch to WORKGROUP.
BASE nil means restore WORKGROUP's working config.
BASE non-nil means restore WORKGROUP's base config."
  (interactive (list (wg-read-workgroup) current-prefix-arg))
  (wg-awhen (wg-current-workgroup t)
    (when (eq it workgroup) (error "Already on: %s" (wg-name it)))
    (wg-update-working-config it))
  (when (and wg-frame-morph-on after-init-time)
    (wg-frame-morph (wg-working-config workgroup)))
  (wg-restore-workgroup workgroup base)
  (wg-set-previous-workgroup (wg-current-workgroup t))
  (wg-set-current-workgroup workgroup)
  (run-hooks 'wg-switch-hook)
  (wg-fontified-msg (:cmd "Switched:  ") (wg-disp)))

(defun wg-create-workgroup (name)
  "Create and add a workgroup named NAME."
  (interactive (list (wg-read-name)))
  (let ((w (if (wg-current-workgroup t) (wg-make-blank-workgroup name)
             (wg-make-default-workgroup name))))
    (wg-check-and-add w)
    (wg-switch-to-workgroup w)
    (wg-fontified-msg (:cmd "Created: ") (:cur name) "  " (wg-disp))))

(defun wg-clone-workgroup (workgroup name)
  "Create and add a clone of WORKGROUP named NAME."
  (interactive (list (wg-arg) (wg-read-name)))
  (let ((new (wg-make-workgroup nil name (wg-base-config workgroup))))
    (wg-check-and-add new)
    (wg-set-working-config new (wg-working-config workgroup))
    (wg-switch-to-workgroup new)
    (wg-fontified-msg
      (:cmd "Cloned: ") (:cur (wg-name workgroup))
      (:msg " to ") (:cur name) "  " (wg-disp))))

(defun wg-kill-workgroup (workgroup)
  "Kill WORKGROUP, saving its working config to the kill ring."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-working-config workgroup))
  (let ((to (or (wg-previous-workgroup t)
                (wg-cyclic-nth-from-workgroup workgroup))))
    (wg-delete workgroup)
    (if (eq to workgroup) (wg-restore-blank-wconfig)
      (wg-switch-to-workgroup to))
    (wg-fontified-msg
      (:cmd "Killed: ") (:cur (wg-name workgroup)) "  " (wg-disp))))

(defun wg-kill-ring-save-base-config (workgroup)
  "Save WORKGROUP's working config to `wg-kill-ring'."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-base-config workgroup))
  (wg-fontified-msg
    (:cmd "Saved: ") (:cur (wg-name workgroup))
    (:cur "'s ") (:msg "base config to the kill ring")))

(defun wg-kill-ring-save-working-config (workgroup)
  "Save WORKGROUP's working config to `wg-kill-ring'."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-working-config workgroup))
  (wg-fontified-msg
    (:cmd "Saved: ") (:cur (wg-name workgroup))
    (:cur "'s ") (:msg "working config to the kill ring")))

(defun wg-yank-config ()
  "Restore a config from `wg-kill-ring'.
Successive yanks restore configs sequentially from the kill ring,
starting at the front."
  (interactive)
  (unless wg-kill-ring (error "The kill-ring is empty"))
  (let ((pos (if (not (eq real-last-command 'wg-yank-config)) 0
               (mod (1+ (or (get 'wg-yank-config :position) 0))
                    (length wg-kill-ring)))))
    (put 'wg-yank-config :position pos)
    (wg-restore-wconfig (nth pos wg-kill-ring))
    (wg-fontified-msg (:cmd "Yanked: ") (:msg pos) "  " (wg-disp))))

(defun wg-kill-workgroup-and-buffers (workgroup)
  "Kill WORKGROUP and the buffers in its working config."
  (interactive (list (wg-arg)))
  (let ((bufs (save-window-excursion
                (wg-restore-workgroup workgroup)
                (mapcar 'window-buffer (window-list)))))
    (wg-kill-workgroup workgroup)
    (mapc 'kill-buffer bufs)
    (wg-fontified-msg
      (:cmd "Killed: ") (:cur (wg-name workgroup))
      (:msg " and its buffers ") "\n" (wg-disp))))

(defun wg-delete-other-workgroups (workgroup)
  "Delete all workgroups but WORKGROUP."
  (interactive (list (wg-arg)))
  (unless (or wg-no-confirm
              (y-or-n-p "Really delete all other workgroups? "))
    (error "Cancelled"))
  (let ((cur (wg-current-workgroup)))
    (mapc 'wg-delete (remove workgroup (wg-list)))
    (unless (eq workgroup cur) (wg-switch-to-workgroup workgroup))
    (wg-fontified-msg
      (:cmd "Deleted: ") (:msg "All workgroups but ")
      (:cur (wg-name workgroup)))))

(defun wg-update-workgroup (workgroup)
  "Set the base config of WORKGROUP to its current config."
  (interactive (list (wg-arg)))
  (wg-set-base-config workgroup (wg-working-config workgroup))
  (wg-fontified-msg
    (:cmd "Updated: ") (:cur (wg-name workgroup))))

(defun wg-update-all-workgroups ()
  "Update all workgroups' base configs.
Worgroups are updated with their working configs in the
`selected-frame'."
  (interactive)
  (mapc 'wg-update-workgroup (wg-list))
  (wg-fontified-msg (:cmd "Updated: ") (:msg "All")))

(defun wg-revert-workgroup (workgroup)
  "Set the working config of WORKGROUP to its base config."
  (interactive (list (wg-arg)))
  (wg-set-working-config
   workgroup (wg-base-config workgroup))
  (when (eq workgroup (wg-current-workgroup))
    (wg-restore-workgroup workgroup t))
  (wg-fontified-msg
    (:cmd "Reverted: ") (:cur (wg-name workgroup))))

(defun wg-revert-all-workgroups ()
  "Revert all workgroups to their base configs."
  (interactive)
  (mapc 'wg-revert-workgroup (wg-list))
  (wg-fontified-msg (:cmd "Reverted: ") (:msg "All")))

(defun wg-index-prompt ()
  "Prompt string for `wg-jump'."
  (let* ((max (1- (length (wg-list))))
         (pr (format "%s\n\nEnter [0-%d]: " (wg-disp) max)))
    (wg-read-idx 0 max pr)))

(defun wg-switch-to-index (n)
  "Switch to Nth workgroup in `wg-list'."
  (interactive (list (or current-prefix-arg (wg-index-prompt))))
  (let ((wl (wg-list)))
    (wg-switch-to-workgroup
     (or (nth n wl) (error "There are only %d workgroups" (length wl))))))

(defun wg-switch-to-index-0 () (interactive) (wg-switch-to-index 0))
(defun wg-switch-to-index-1 () (interactive) (wg-switch-to-index 1))
(defun wg-switch-to-index-2 () (interactive) (wg-switch-to-index 2))
(defun wg-switch-to-index-3 () (interactive) (wg-switch-to-index 3))
(defun wg-switch-to-index-4 () (interactive) (wg-switch-to-index 4))
(defun wg-switch-to-index-5 () (interactive) (wg-switch-to-index 5))
(defun wg-switch-to-index-6 () (interactive) (wg-switch-to-index 6))
(defun wg-switch-to-index-7 () (interactive) (wg-switch-to-index 7))
(defun wg-switch-to-index-8 () (interactive) (wg-switch-to-index 8))
(defun wg-switch-to-index-9 () (interactive) (wg-switch-to-index 9))

(defun wg-switch-left (&optional workgroup n)
  "Switch to the workgroup before WORKGROUP in `wg-list'."
  (interactive (list (wg-arg nil t) current-prefix-arg))
  (wg-switch-to-workgroup
   (or (wg-cyclic-nth-from-workgroup workgroup (or n -1))
       (car (wg-list)))))

(defun wg-switch-right (&optional workgroup n)
  "Switch to the workgroup after WORKGROUP in `wg-list'."
  (interactive (list (wg-arg nil t) current-prefix-arg))
  (wg-switch-to-workgroup
   (or (wg-cyclic-nth-from-workgroup workgroup n)
       (car (wg-list)))))

(defun wg-switch-left-other-frame (&optional n)
  "Like `wg-switch-left', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-frame-offset (or n 1))
    (wg-switch-left)))

(defun wg-switch-right-other-frame (&optional n)
  "Like `wg-switch-right', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-frame-offset (or n -1))
    (wg-switch-right)))

(defun wg-switch-to-previous-workgroup ()
  "Switch to the previous workgroup."
  (interactive)
  (wg-switch-to-workgroup (wg-previous-workgroup)))

(defun wg-swap-workgroups ()
  "Swap the previous and current workgroups."
  (interactive)
  (wg-list-swap (wg-current-workgroup) (wg-previous-workgroup))
  (wg-fontified-msg (:cmd "Swapped ") (wg-disp)))

(defun wg-offset-left (workgroup &optional n)
  "Offset WORKGROUP leftward cyclically."
  (interactive (list (wg-arg) current-prefix-arg))
  (wg-cyclic-offset-workgroup workgroup (or n -1))
  (wg-fontified-msg (:cmd "Offset left: ") (wg-disp)))

(defun wg-offset-right (workgroup &optional n)
  "Offset WORKGROUP rightward cyclically."
  (interactive (list (wg-arg) current-prefix-arg))
  (wg-cyclic-offset-workgroup workgroup (or n 1))
  (wg-fontified-msg (:cmd "Offset right: ") (wg-disp)))

(defun wg-rename-workgroup (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list (wg-arg) (wg-read-name "New name: ")))
  (let ((oldname (wg-name workgroup)))
    (wg-set-name workgroup newname)
    (wg-fontified-msg
      (:cmd "Renamed: ") (:cur oldname) (:msg " to ")
      (:cur (wg-name workgroup)))))

(defun wg-reset (&optional force)
  "Reset workgroups.
Deletes saved state in `wg-frame-table' and nulls out
`wg-list', `wg-file' and `wg-kill-ring'."
  (interactive "P")
  (unless (or force wg-no-confirm (y-or-n-p "Are you sure? "))
    (error "Canceled"))
  (clrhash wg-frame-table)
  (setq wg-list nil
        wg-file nil
        wg-dirty nil)
  (wg-fontified-msg (:cmd "Reset: ") (:msg "Workgroups")))


;;; file commands

(defun wg-save (file)
  "Save workgroups to FILE.
Called interactively with a prefix arg, or if `wg-file'
is nil, read a filename.  Otherwise use `wg-file'."
  (interactive
   (list (if (or current-prefix-arg (not (wg-file t)))
             (read-file-name "File: ") (wg-file))))
  (wg-save-sexp-to-file
   (cons wg-list-id (wg-list)) file)
  (setq wg-dirty nil wg-file file)
  (wg-fontified-msg (:cmd "Wrote: ") (:file file)))

(defun wg-load (file)
  "Load workgroups from FILE.
Called interactively with a prefix arg, and if `wg-file'
is non-nil, use `wg-file'. Otherwise read a filename."
  (interactive
   (list (if (and current-prefix-arg (wg-file t))
             (wg-file) (read-file-name "File: "))))
  (wg-dbind (id . workgroups) (wg-read-sexp-from-file file)
    (unless (eq id wg-list-id)
      (error "%S is not a workgroups file." file))
    (wg-reset t)
    (setq wg-list workgroups wg-file file))
  (when wg-switch-on-load
    (wg-awhen (wg-list t) (wg-switch-to-workgroup (car it))))
  (wg-fontified-msg (:cmd "Loaded: ") (:file file)))

(defun wg-find-file (file)
  "Create a new workgroup and find file FILE in it."
  (interactive "FFile: ")
  (wg-create-workgroup (file-name-nondirectory file))
  (find-file file))

(defun wg-find-file-read-only (file)
  "Create a new workgroup and find FILE read-only in it."
  (interactive "FFile: ")
  (wg-create-workgroup (file-name-nondirectory file))
  (find-file-read-only file))

(defun wg-get-by-buffer (buf)
  "Switch to the workgroup that contains BUF."
  (interactive (list (wg-read-buffer)))
  (wg-aif (wg-find-buffer buf)
      (apply 'wg-switch-to-workgroup it)
    (error "No workgroup contains %S" buf)))

(defun wg-dired (dir &optional switches)
  "Create a workgroup and open DIR in dired with SWITCHES."
  (interactive (list (read-directory-name "Dired: ") current-prefix-arg))
  (wg-create-workgroup dir)
  (dired dir switches))


;;; toggle commands

(defun wg-toggle-mode-line ()
  "Toggle workgroups' mode-line display."
  (interactive)
  (setq wg-mode-line-on (not wg-mode-line-on))
  (force-mode-line-update)
  (wg-fontified-msg
    (:cmd "mode-line: ") (:msg (if wg-mode-line-on "on" "off"))))

(defun wg-toggle-frame-morph ()
  "Toggle frame morphing."
  (interactive)
  (setq wg-frame-morph-on (not wg-frame-morph-on))
  (wg-fontified-msg
    (:cmd "Frame morph: ") (:msg (if wg-frame-morph-on "on" "off"))))


;;; echo commands

(defun wg-echo-current-workgroup ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (wg-fontified-msg
    (:cmd "Current: ") (:cur (wg-name (wg-current-workgroup)))))

(defun wg-echo-all-workgroups ()
  "Display the names of all workgroups in the echo area."
  (interactive)
  (wg-fontified-msg (:cmd "Workgroups: ") (wg-disp)))

(defun wg-echo-time ()
  "Echo the current time."
  (interactive)
  (wg-msg ;; Pass through a format to escape the % in `battery'
   "%s" (wg-fontify
         (:cmd "Current time: ")
         (:msg (format-time-string wg-time-format))
         (when (and wg-display-battery (fboundp 'battery))
           (wg-fontify "\n" (:cmd "Battery: ") (:msg (battery)))))))

(defun wg-echo-version ()
  "Echo the current version number."
  (interactive)
  (wg-fontified-msg
    (:cmd "Workgroups version: ") (:msg wg-version)))

(defun wg-echo-last-message ()
  "Echo the last message workgroups sent to the echo area.
The string is passed through a format arg to escape %'s."
  (interactive)
  (message "%s" wg-last-message))


;;; keymap

(defvar wg-map
  (wg-fill-keymap (make-sparse-keymap)
    "C-c"        'wg-create-workgroup
    "c"          'wg-create-workgroup
    "C-'"        'wg-switch-to-workgroup
    "'"          'wg-switch-to-workgroup
    "C-v"        'wg-switch-to-workgroup
    "v"          'wg-switch-to-workgroup
    "C"          'wg-clone-workgroup
    "C-k"        'wg-kill-workgroup
    "k"          'wg-kill-workgroup
    "C-y"        'wg-yank-config
    "y"          'wg-yank-config
    "M-W"        'wg-kill-ring-save-base-config
    "M-w"        'wg-kill-ring-save-working-config
    "M-k"        'wg-kill-workgroup-and-buffers
    "K"          'wg-delete-other-workgroups
    "C-r"        'wg-revert-workgroup
    "r"          'wg-revert-workgroup
    "C-S-r"      'wg-revert-all-workgroups
    "R"          'wg-revert-all-workgroups
    "C-u"        'wg-update-workgroup
    "u"          'wg-update-workgroup
    "C-S-u"      'wg-update-all-workgroups
    "U"          'wg-update-all-workgroups
    "C-s"        'wg-save
    "C-l"        'wg-load
    "A"          'wg-rename-workgroup
    "C-p"        'wg-switch-left
    "p"          'wg-switch-left
    "C-n"        'wg-switch-right
    "n"          'wg-switch-right
    "M-p"        'wg-switch-left-other-frame
    "M-n"        'wg-switch-right-other-frame
    "C-a"        'wg-switch-to-previous-workgroup
    "a"          'wg-switch-to-previous-workgroup
    "C-j"        'wg-switch-to-index
    "0"          'wg-switch-to-index-0
    "1"          'wg-switch-to-index-1
    "2"          'wg-switch-to-index-2
    "3"          'wg-switch-to-index-3
    "4"          'wg-switch-to-index-4
    "5"          'wg-switch-to-index-5
    "6"          'wg-switch-to-index-6
    "7"          'wg-switch-to-index-7
    "8"          'wg-switch-to-index-8
    "9"          'wg-switch-to-index-9
    "C-x"        'wg-swap-workgroups
    "C-,"        'wg-offset-left
    "C-."        'wg-offset-right
    "C-e"        'wg-echo-all-workgroups
    "e"          'wg-echo-all-workgroups
    "S-C-e"      'wg-echo-current-workgroup
    "E"          'wg-echo-current-workgroup
    "C-t"        'wg-echo-time
    "t"          'wg-echo-time
    "V"          'wg-echo-version
    "C-m"        'wg-echo-last-message
    "m"          'wg-echo-last-message
    "C-b"        'wg-get-by-buffer
    "b"          'wg-get-by-buffer
    "C-f"        'wg-find-file
    "S-C-f"      'wg-find-file-read-only
    "d"          'wg-dired
    "C-i"        'wg-toggle-mode-line
    "C-w"        'wg-toggle-frame-morph
    "!"          'wg-reset
    "?"          'wg-help)
  "Workgroups' keymap.")


;;; help

(defvar wg-help
  '("\\[wg-create-workgroup]"
    "Create a new workgroup and switch to it"
    "\\[wg-switch-to-workgroup]"
    "Switch to a workgroup"
    "\\[wg-clone-workgroup]"
    "Create a clone of the current workgroug and switch to it"
    "\\[wg-kill-workgroup]"
    "Kill a workgroup"
    "\\[wg-yank-config]"
    "Set the working config to a config from the kill ring"
    "\\[wg-kill-ring-save-working-config]"
    "Save the current config to the kill ring"
    "\\[wg-kill-workgroup-and-buffers]"
    "Kill a workgroup and its buffer"
    "\\[wg-delete-other-workgroups]"
    "Delete all but the specified workgroup"
    "\\[wg-revert-workgroup]"
    "Set a workgroup's working config to its base config"
    "\\[wg-update-workgroup]"
    "Set a workgroup's base config to its working config"
    "\\[wg-save]"
    "Save workgroups to a file"
    "\\[wg-load]"
    "Load workgroups from a file"
    "\\[wg-rename-workgroup]"
    "Rename a workgroup"
    "\\[wg-switch-left]"
    "Cycle leftward in the workgroups list"
    "\\[wg-switch-right]"
    "Cycle rightward in the workgroups list"
    "\\[wg-switch-left-other-frame]"
    "Cycle leftward in the workgroups list in another frame"
    "\\[wg-switch-right-other-frame]"
    "Cycle rightward in the workgroups list in another frame"
    "\\[wg-offset-left]"
    "Offset a workgroup leftward cyclically"
    "\\[wg-offset-right]"
    "Offset a workgroup rightward cyclically"
    "\\[wg-swap-workgroups]"
    "Swap the previous and current workgroups"
    "\\[wg-switch-to-previous-workgroup]"
    "Switch to the previously selected workgroup"
    "\\[wg-switch-to-index]"
    "Jump to a workgroup by number"
    "\\[wg-switch-to-index-0]"
    "Switch to the workgroup at index 0"
    "\\[wg-switch-to-index-1]"
    "Switch to the workgroup at index 1"
    "\\[wg-switch-to-index-2]"
    "Switch to the workgroup at index 2"
    "\\[wg-switch-to-index-3]"
    "Switch to the workgroup at index 3"
    "\\[wg-switch-to-index-4]"
    "Switch to the workgroup at index 4"
    "\\[wg-switch-to-index-5]"
    "Switch to the workgroup at index 5"
    "\\[wg-switch-to-index-6]"
    "Switch to the workgroup at index 6"
    "\\[wg-switch-to-index-7]"
    "Switch to the workgroup at index 7"
    "\\[wg-switch-to-index-8]"
    "Switch to the workgroup at index 8"
    "\\[wg-switch-to-index-9]"
    "Switch to the workgroup at index 9"
    "\\[wg-get-by-buffer]"
    "Switch to the workgroup and config which contains the specified buffer"
    "\\[wg-find-file]"
    "Create a new workgroup and find a file in it"
    "\\[wg-find-file-read-only]"
    "Create a new workgroup and find-file-read-only in it"
    "\\[wg-dired]"
    "Create a new workgroup and open a dired buffer in it"
    "\\[wg-toggle-mode-line]"
    "Toggle workgroups mode-line display"
    "\\[wg-toggle-frame-morph]"
    "Toggle frame-morph animation on workgroups switch"
    "\\[wg-echo-all-workgroups]"
    "Display the names of all workgroups in the echo area"
    "\\[wg-echo-current-workgroup]"
    "Display the name of the current workgroup in the echo area"
    "\\[wg-echo-time]"
    "Display the current time in the echo area"
    "\\[wg-echo-version]"
    "Display the version in the echo area"
    "\\[wg-help]"
    "Show this help message")
  "List of commands and their help messages. Used by `wg-help'.")

(defun wg-help ()
  "Show the workgroups commands help buffer."
  (interactive)
  (let ((hline (concat (make-string 80 ?-) "\n")))
    (with-output-to-temp-buffer "*workroups help*"
      (princ  "Workgroups For Windows keybindings:\n")
      (princ hline)
      (dolist (elt (wg-partition wg-help 2))
        (princ (format "%15s  |  %s\n"
                       (substitute-command-keys (car elt))
                       (cadr elt))))
      (princ hline)
      (help-print-return-message))))


;;; mode definition

(defun wg-unset-prefix-key ()
  "Restore the original definition of `wg-prefix-key'."
  (wg-awhen (get 'wg-prefix-key :original)
    (wg-dbind (key . def) it
      (when (eq wg-map (lookup-key global-map key))
        (global-set-key key def))
      (put 'wg-prefix-key :original nil))))

(defun wg-set-prefix-key ()
  "Define `wg-prefix-key' as `wg-map' in `global-map'."
  (wg-unset-prefix-key)
  (let ((key wg-prefix-key))
    (put 'wg-prefix-key :original (cons key (lookup-key global-map key)))
    (global-set-key key wg-map)))

(defun wg-query-for-save ()
  "Query for save when `wg-dirty' is non-nil."
  (or (not wg-dirty)
      (not (y-or-n-p "Save modified workgroups? "))
      (call-interactively 'wg-save)
      t))

(defun wg-emacs-exit-query ()
  "Call `wg-query-for-save' when
`wg-query-for-save-on-emacs-exit' is non-nil."
  (or (not wg-query-for-save-on-emacs-exit)
      (wg-query-for-save)))

(defun wg-workgroups-mode-exit-query ()
  "Call `wg-query-for-save' when
`wg-query-for-save-on-workgroups-exit' is non-nil."
  (or (not wg-query-for-save-on-workgroups-exit)
      (wg-query-for-save)))

(defun wg-enable (enable)
  "Enable `workgroups' when ENABLE is non-nil.
Disable otherwise."
  (cond (enable
         (add-hook 'kill-emacs-query-functions 'wg-emacs-exit-query)
         (add-hook 'delete-frame-functions 'wg-delete-frame)
         (wg-set-prefix-key)
         (wg-mode-line-add-display))
        (t
         (wg-workgroups-mode-exit-query)
         (remove-hook 'kill-emacs-query-functions 'wg-emacs-exit-query)
         (remove-hook 'delete-frame-functions 'wg-delete-frame)
         (wg-unset-prefix-key)
         (wg-mode-line-remove-display))))

(define-minor-mode workgroups-mode
  "This turns `workgroups-mode' on and off.
If ARG is null, toggle `workgroups-mode'.
If ARG is an integer greater than zero, turn on `workgroups-mode'.
If ARG is an integer less one, turn off `workgroups-mode'.
If ARG is anything else, turn on `workgroups-mode'."
  :lighter     " wg"
  :init-value  nil
  :global      t
  :group       'workgroups
  (wg-enable workgroups-mode))


;;; provide

(provide 'workgroups)


;;; workgroups.el ends here
