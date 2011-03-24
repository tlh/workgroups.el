;;; workgroups.el --- Workgroups For Windows (for Emacs)
;;
;; Workgroups is an Emacs session manager providing window-configuration
;; switching, persistence, undo/redo, killing/yanking, animated morphing,
;; per-workgroup buffer-lists, and more.

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
;; See the file README.md in `workgroups.el's directory
;;
;;; Installation:
;;
;;; Usage:
;;

;;; Symbol naming conventions:
;;
;; W always refers to a Workgroups window or window tree.
;; WT always refers to a Workgroups window tree.
;; SW always refers to a sub-window or sub-window-tree of a wtree.
;; WL always refers to the window list of a wtree.
;; LN, TN, RN and BN always refer to the LEFT, TOP, RIGHT and BOTTOM
;;   edges of an edge list, where N is a differentiating integer.
;; LS, HS, LB and HB always refer to the LOW-SIDE, HIGH-SIDE, LOW-BOUND
;;   and HIGH-BOUND of a bounds list.  See `wg-with-bounds'.
;; WGBUF always refers to a workgroups buffer object.
;; EBUF always refers to an Emacs buffer object.
;;



;;; Code:

(require 'cl)

(eval-when-compile
  ;; This prevents "assignment to free variable"
  ;; and "function not known to exist" warnings.
  (require 'ido nil t)
  (require 'iswitchb nil t))



;;; consts

(defconst wg-version "0.2.1"
  "Current version of workgroups.")

(defconst wg-persisted-workgroups-tag 'workgroups
  "Tag appearing at the beginning of any list of persisted workgroups.")

(defconst wg-persisted-workgroups-format-version "2.0"
  "Current version number of the format of persisted workgroup lists.")



;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- Emacs session manager"
  :group 'convenience
  :version wg-version)

(defcustom workgroups-mode-hook nil
  "Hook run when `workgroups-mode' is turned on."
  :type 'hook
  :group 'workgroups)

(defcustom workgroups-mode-exit-hook nil
  "Hook run when `workgroups-mode' is turned off."
  :type 'hook
  :group 'workgroups)

;; FIXME:
(defcustom wg-prefix-key (kbd "C-z")
  "Workgroups' prefix key."
  :type 'string
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (when (and (boundp 'workgroups-mode) workgroups-mode)
           (wg-set-prefix-key))
         val))

(defcustom wg-switch-hook nil
  "Hook run by `wg-switch-to-workgroup'."
  :type 'hook
  :group 'workgroups)

(defcustom wg-no-confirm nil
  "Non-nil means don't request confirmation before various
destructive operations, like `wg-reset'.  This doesn't modify
query-for-save behavior.  Use
`wg-query-for-save-on-workgroups-mode-exit' and
`wg-query-for-save-on-emacs-exit' for that."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-on t
  "Toggles Workgroups' mode-line display."
  :type 'boolean
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (force-mode-line-update)))

(defcustom wg-kill-ring-size 20
  "Maximum length of the `wg-kill-ring'."
  :type 'integer
  :group 'workgroups)

(defcustom wg-warning-timeout 1.0
  "Seconds to `sit-for' after a warning message."
  :type 'float
  :group 'workgroups)


;; save and load customization

(defcustom wg-switch-on-load t
  "Non-nil means switch to the first workgroup in a file when it's loaded."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-query-for-save-on-emacs-exit t
  "Non-nil means query to save changes before exiting Emacs.
Exiting workgroups removes its `kill-emacs-query-functions' hook,
so if you set this to nil, you may want to set
`wg-query-for-save-on-workgroups-exit' to t."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-query-for-save-on-workgroups-mode-exit t
  "Non-nil means query to save changes before exiting `workgroups-mode'.
Exiting workgroups removes its `kill-emacs-query-functions' hook,
which is why this variable exists."
  :type 'boolean
  :group 'workgroups)


;; workgroup restoration customization

(defcustom wg-restore-assigned-buffers t
  "Non-nil means restore all buffers assigned to the workgroup
on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-default-buffer "*scratch*"
  "Buffer switched to when a blank workgroup is created.
Also used when a window's buffer can't be restored."
  :type 'string
  :group 'workgroups)

(defcustom wg-barf-on-nonexistent-file nil
  "Non-nil means error when an attempt is made to restore a
buffer pointing to a nonexistent file."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-position nil
  "Non-nil means restore frame position on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-scroll-bars t
  "Non-nil means restore scroll-bar settings on workgroup restore."
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

(defcustom wg-restore-mbs-window t
  "Non-nil means restore `minibuffer-scroll-window' on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point t
  "Non-nil means restore `point' on workgroup restore.
This is included mainly so point restoration can be suspended
during `wg-morph' -- you probably want this on."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point-max t
  "Controls point restoration when point is at `point-max'.
If `point' is at `point-max' when a wconfig is created, put
`point' back at `point-max' when the wconfig is restored, even if
`point-max' has increased in the meantime.  This is useful
in (say) irc buffers where `point-max' is constantly increasing."
  :type 'boolean
  :group 'workgroups)


;; undo/redo customization

(defcustom wg-wconfig-undo-list-max 30
  "Number of past window configs for undo to retain.
A value of nil means no limit."
  :type 'integer
  :group 'workgroups)

(defcustom wg-commands-that-alter-window-configs (make-hash-table)
  "Table of commands before which to save up-to-date undo
information about the current window-configuration.  Because some
window-configuration changes -- like changes to selected-window,
window scrolling, point, mark, fringes, margins and scroll-bars
-- don't trigger `window-configuration-change-hook', they are
effectively invisible to undoification.  Workgroups solves this
problem by checking in `pre-command-hook' whether `this-command'
is in this table, and saving undo info if it is.  Since it's just
a single hash-table lookup per-command, there's no noticeable
slowdown.  Add commands to this table if they aren't already
present, and you want up-to-date undo info saved prior to their
invocation."
  :type 'hash-table
  :group 'workgroups)


;; buffer set customization

(defcustom wg-buffer-sets-on t
  "Non-nil means the completions sets feature is on.
Nil means ido, iswitchb and switch-to-buffer behave normally.
See `wg-default-buffer-set-order'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-default-buffer-set-order
  '(all assigned filtered assigned-and-filtered fallback)
  "Order in which `wg-switch-to-buffer' presents buffer sets.
The value should be a list containing any combination of `all',
`assigned', `filtered', `assigned-and-filtered' and `fallback'.
The meanings of these are as follows:

all: all buffer names

assigned: only on the names of those live buffers that have been
  assigned to the current workgroup

filtered: only on the list of buffer names resulting from running
  the current workgroup's buffer-list filters over the entire
  buffer-list

assigned-and-filtered: the union of the `assigned' and `filtered'
  sets

fallback: fallback to the non-ido or non-iswitchb version of the
  command, completing on all buffer names
"
  :type 'list
  :group 'workgroups)

(defcustom wg-switch-to-buffer-buffer-set-order nil
  "Non-nil overrides `wg-default-buffer-set-order' in
`wg-switch-to-buffer'. "
  :type 'list
  :group 'workgroups)

(defcustom wg-kill-buffer-buffer-set-order nil
  "Non-nil overrides `wg-default-buffer-set-order' in
`wg-kill-buffer'. "
  :type 'list
  :group 'workgroups)

(defcustom wg-next-buffer-buffer-set-order nil
  "Non-nil overrides `wg-default-buffer-set-order' in
`wg-next-buffer' and `wg-previous-buffer'."
  :type 'list
  :group 'workgroups)

(defcustom wg-barf-on-filter-error nil
  "Nil means catch all filter errors and `message' them,
rather than leaving them uncaught."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-auto-add-buffer-on-find-file nil
  "Non-nil means automatically add to the current workgroup those
buffers created by `find-file' and friends."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-auto-remove-buffer-on-kill-buffer nil
  "Non-nil means automatically remove from the current workgroup
buffers killed with `kill-buffer' and friends."
  :type 'boolean
  :group 'workgroups)


;; morph customization

(defcustom wg-morph-on t
  "Non-nil means use `wg-morph' when restoring wconfigs."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-morph-hsteps 9
  "Columns/iteration to step window edges during `wg-morph'.
Values lower than 1 are invalid."
  :type 'integer
  :group 'workgroups)

(defcustom wg-morph-vsteps 3
  "Rows/iteration to step window edges during `wg-morph'.
Values lower than 1 are invalid."
  :type 'integer
  :group 'workgroups)

(defcustom wg-morph-terminal-hsteps 3
  "Used instead of `wg-morph-hsteps' in terminal frames.
If nil, `wg-morph-hsteps' is used."
  :type 'integer
  :group 'workgroups)

(defcustom wg-morph-terminal-vsteps 1
  "Used instead of `wg-morph-vsteps' in terminal frames.
If nil, `wg-morph-vsteps' is used."
  :type 'integer
  :group 'workgroups)

(defcustom wg-morph-sit-for-seconds 0
  "Seconds to `sit-for' between `wg-morph' iterations.
Should probably be zero unless `redisplay' is *really* fast on
your machine, and `wg-morph-hsteps' and `wg-morph-vsteps' are
already set as low as possible."
  :type 'float
  :group 'workgroups)

(defcustom wg-morph-truncate-partial-width-windows t
  "Bound to `truncate-partial-width-windows' during `wg-morph'.
Non-nil, this prevents weird-looking continuation line behavior,
and can speed up morphing a little.  Lines jump back to their
wrapped status when `wg-morph' is complete."
  :type 'boolean
  :group 'workgroups)


;; display customization

(defcustom wg-use-faces t
  "Nil means don't use faces in various displays."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-left-brace "("
  "String to the left of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-right-brace ")"
  "String to the right of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-divider ":"
  "String between workgroup position and name in the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-left-brace "( "
  "String to the left of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-right-brace " )"
  "String to the right of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-divider " | "
  "String between workgroup names in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-current-workgroup-left-decor "-<{ "
  "String to the left of the current workgroup name in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-current-workgroup-right-decor " }>-"
  "String to the right of the current workgroup name in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-previous-workgroup-left-decor "< "
  "String to the left of the previous workgroup name in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-previous-workgroup-right-decor " >"
  "String to the right of the previous workgroup name in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-time-format "%H:%M:%S %A, %B %d %Y"
  "Format string for time display.  Passed to `format-time-string'."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-battery t
  "Non-nil means include `battery', when available, in the time display."
  :type 'boolean
  :group 'workgroups)



;;; vars

(defvar wg-minor-mode-map-entry nil
  "Contains workgroups' minor-mode-map entry.")

(defvar wg-kill-ring nil
  "Ring of killed or kill-ring-saved wconfigs.")

(defvar wg-last-message nil
  "Holds the last message Workgroups sent to the echo area.")

(defvar wg-face-abbrevs nil
  "Assoc list mapping face abbreviations to face names.")


;; file and dirty flag vars

(defvar wg-file nil
  "Current workgroups file.")

(defvar wg-list nil
  "List of currently defined workgroups.")

(defvar wg-dirty nil
  "Global dirty flag.  Non-nil when there are unsaved changes.")

(defvar wg-nodirty nil
  "Dynamically bound around workgroup-modifying operations to
temporarily disably dirty flagging.")


;; undo vars

(defvar wg-flag-wconfig-changes t
  "Non-nil means window config changes should be flagged for undoification.")

(defvar wg-window-config-changed-p nil
  "Flag set by `window-configuration-change-hook'.")

(defvar wg-just-exited-minibuffer nil
  "Flag set by `minibuffer-exit-hook' to exempt from
undoification those window-configuration changes caused by
exiting the minibuffer .")


;; buffer set vars

(defvar wg-temp-buffer-list nil
  "Temporary buffer list filtered by `wg-run-buffer-list-filters'.")

(defvar wg-current-buffer-set nil
  "Bound to a buffer-set symbol during buffer list contruction.")

(defvar wg-current-buffer-method nil
  "Bound to the current buffer method in `wg-with-buffer-sets'.")


;; wconfig restoration and morph vars

(defvar wg-window-min-width 2
  "Bound to `window-min-width' when restoring wtrees. ")

(defvar wg-window-min-height 1
  "Bound to `window-min-height' when restoring wtrees.")

(defvar wg-window-min-pad 2
  "Added to `wg-window-min-foo' to produce the actual minimum window size.")

(defvar wg-actual-min-width (+ wg-window-min-width wg-window-min-pad)
  "Actual minimum window width when creating windows.")

(defvar wg-actual-min-height (+ wg-window-min-height wg-window-min-pad)
  "Actual minimum window height when creating windows.")

(defvar wg-min-edges `(0 0 ,wg-actual-min-width ,wg-actual-min-height)
  "Smallest allowable edge list of windows created by Workgroups.")

(defvar wg-null-edges '(0 0 0 0)
  "Null edge list.")

(defvar wg-morph-max-steps 200
  "Maximum `wg-morph' iterations before forcing exit.")

(defvar wg-morph-no-error t
  "Non-nil means ignore errors during `wg-morph'.
The error message is sent to *messages* instead.  This was added
when `wg-morph' was unstable, so that the screen wouldn't be left
in an inconsistent state.  It's unnecessary now, as `wg-morph' is
stable, but is left here for the time being.")

(defvar wg-selected-window nil
  "Used during wconfig restoration to hold the selected window.")



;;; faces

(defmacro wg-defface (face key spec doc &rest args)
  "`defface' wrapper adding a lookup key used by `wg-fontify'."
  (declare (indent 2))
  `(progn
     (pushnew (cons ,key ',face) wg-face-abbrevs :test #'equal)
     (defface ,face ,spec ,doc ,@args)))

(wg-defface wg-current-workgroup-face :cur
  '((((class color)) (:foreground "white")))
  "Face used for the name of the current workgroup in the list display."
  :group 'workgroups)

(wg-defface wg-previous-workgroup-face :prev
  '((((class color)) (:foreground "light sky blue")))
  "Face used for the name of the previous workgroup in the list display."
  :group 'workgroups)

(wg-defface wg-other-workgroup-face :other
  '((((class color)) (:foreground "light slate grey")))
  "Face used for the names of other workgroups in the list display."
  :group 'workgroups)

(wg-defface wg-command-face :cmd
  '((((class color)) (:foreground "aquamarine")))
  "Face used for command/operation strings."
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
  "Face used for workgroup position and name in the mode-line display."
  :group 'workgroups)

(wg-defface wg-filename-face :file
  '((((class color)) (:foreground "light sky blue")))
  "Face used for filenames."
  :group 'workgroups)

(wg-defface wg-frame-face :frame
  '((((class color)) (:foreground "white")))
  "Face used for frame names."
  :group 'workgroups)



;;; utils

;; functions used in macros:
(eval-and-compile

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
  )

(defmacro wg-with-gensyms (syms &rest body)
  "Bind all symbols in SYMS to `gensym's, and eval BODY."
  (declare (indent 1))
  `(let (,@(mapcar (lambda (sym) `(,sym (gensym))) syms)) ,@body))

(defmacro wg-dbind (args expr &rest body)
  "Abbreviation of `destructuring-bind'."
  (declare (indent 2))
  `(destructuring-bind ,args ,expr ,@body))

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

(defmacro wg-docar (spec &rest body)
  "do-style wrapper for `mapcar'."
  (declare (indent 1))
  `(mapcar (lambda (,(car spec)) ,@body) ,(cadr spec)))

(defmacro wg-get1 (spec &rest body)
  "Return the first elt in SPEC's list on which BODY returns non-nil.
SPEC is a list with the var as the car and the list as the cadr."
  (declare (indent 1))
  (wg-dbind (sym list) spec
    `(some (lambda (,sym) (when (progn ,@body) ,sym)) ,list)))

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

(defmacro wg-aand (&rest args)
  "Anaphoric `and'."
  (declare (indent defun))
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

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

(defun wg-filter-map (pred seq)
  "Call PRED on each element in SEQ, returning a list of the non-nil results."
  (let (acc)
    (mapc (lambda (elt) (wg-awhen (funcall pred elt) (push it acc))) seq)
    (nreverse acc)))

(defun wg-last1 (list)
  "Return the last element of LIST."
  (car (last list)))

(defun wg-leave (list n)
  "Return a list of the last N elts in LIST."
  (nthcdr (- (length list) n) list))

(defun wg-rnth (n list)
  "Return the Nth element of LIST, counting from the end."
  (nth (- (length list) n 1) list))

(defun wg-rotlst (list &optional offset)
  "Rotate LIST by OFFSET.  Positive OFFSET rotates left, negative right."
  (let ((split (mod (or offset 1) (length list))))
    (append (nthcdr split list) (wg-take list split))))

(defun wg-cyclic-nth (list n)
  "Return the Nth element of LIST, modded by the length of list."
  (nth (mod n (length list)) list))

(defun wg-insert-elt (elt list &optional pos)
  "Insert ELT into LIST at POS or the end."
  (let* ((len (length list)) (pos (or pos len)))
    (when (wg-within pos 0 len t)
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
    (wg-cyclic-nth list (+ pos n))))

(defun wg-util-swap (elt1 elt2 list)
  "Return a copy of LIST with ELT1 and ELT2 swapped.
Return nil when ELT1 and ELT2 aren't both present."
  (wg-when-let ((p1 (position elt1 list))
                (p2 (position elt2 list)))
    (wg-move-elt elt1 (wg-move-elt elt2 list p1) p2)))

(defun wg-aget (alist key)
  "Return the value of KEY in ALIST. Uses `assq'."
  (cdr (assq key alist)))

(defun wg-acopy (alist)
  "Return a copy of ALIST's toplevel list structure."
  (wg-docar (kvp alist) (cons (car kvp) (cdr kvp))))

(defun wg-aset! (alist key val)
  "Set KEY to VALUE in ALIST strictly by side-effect.  ALIST must be a cons."
  (unless alist (error "wg-aset: ALIST must be a cons."))
  (wg-aif (assq key alist) (setcdr it val)
    (setcdr (last alist) (list (cons key val))))
  val)

(defun wg-aset (alist key val)
  "Set KEY's value to VAL in ALIST.
If KEY already exists in ALIST, destructively set its value.
Otherwise, cons a new key-value-pair onto ALIST."
  (wg-aif (assq key alist) (progn (setcdr it val) alist)
    (cons (cons key val) alist)))

(defun wg-aput (alist &rest key-value-pairs)
  "Add all KEY-VALUE-PAIRS to a copy of ALIST, and return the copy."
  (flet ((rec (alist kvps) (if (not kvps) alist
                             (wg-dbind (k v . rest) kvps
                               (wg-aset (rec alist rest) k v)))))
    (rec (wg-acopy alist) key-value-pairs)))

(defun wg-get-alist (key val list-of-alists)
  "Return the first alist in LIST-OF-ALISTS containing KEY and VAL."
  (catch 'found
    (dolist (alist list-of-alists)
      (when (equal val (cdr (assoc key alist)))
        (throw 'found alist)))))

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

(defmacro wg-fill-hash-table (table &rest key-value-pairs)
  "Fill TABLE with KEY-VALUE-PAIRS and return TABLE."
  (declare (indent 1))
  (wg-with-gensyms (tabsym)
    `(let ((,tabsym ,table))
       ,@(wg-docar (pair (wg-partition key-value-pairs 2))
           `(puthash ,@pair ,tabsym))
       ,tabsym)))

(defmacro wg-fill-keymap (keymap &rest binds)
  "Return KEYMAP after defining in it all keybindings in BINDS."
  (declare (indent 1))
  (wg-with-gensyms (km)
    `(let ((,km ,keymap))
       ,@(wg-docar (b (wg-partition binds 2))
           `(define-key ,km (kbd ,(car b)) ,(cadr b)))
       ,km)))

(defun wg-write-sexp-to-file (sexp file)
  "Write the printable representation of SEXP to FILE."
  (with-temp-buffer
    (let (print-level print-length)
      (insert (format "%S" sexp))
      (write-file file))))

(defun wg-read-sexp-from-file (file)
  "Read and return an sexp from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun wg-read-object (prompt test warning &rest args)
  "PROMPT for an object that satisfies TEST, WARNING if necessary.
ARGS are `read-from-minibuffer's args, after PROMPT."
  (let ((obj (apply #'read-from-minibuffer prompt args)))
    (wg-until (funcall test obj)
      (message warning)
      (sit-for wg-warning-timeout)
      (setq obj (apply #'read-from-minibuffer prompt args)))
    obj))

(defun wg-file-under-root-path-p (root-path file-path)
  "Return t when FILE-PATH is under ROOT-PATH, nil otherwise."
  (string-match (concat "^" (regexp-quote (expand-file-name root-path)))
                (expand-file-name file-path)))

(defun wg-cyclic-nth-from-frame (&optional n frame)
  "Return the frame N places away from FRAME in `frame-list' cyclically.
N defaults to 1, and FRAME defaults to `selected-frame'."
  (wg-cyclic-nth-from-elt
   (or frame (selected-frame)) (frame-list) (or n 1)))

(defun wg-make-string (times string &optional separator)
  "Like `make-string', but includes a separator."
  (mapconcat 'identity (make-list times string) (or separator "")))

(defun wg-emacs-buffer-names ()
  "Return a list of the names of all buffers in `buffer-list'."
  (mapcar 'buffer-name (buffer-list)))



;;; workgroups utils

(defun wg-type-of (obj)
  "Return workgroups' object type of OBJ."
  (wg-aget obj 'type))

(defun wg-type-p (type obj)
  "Return t if OBJ is of type TYPE, nil otherwise."
  (and (consp obj) (eq type (wg-type-of obj))))

(defun wg-type-check (type obj &optional noerror)
  "Throw an error if OBJ is not of type TYPE."
  (or (wg-type-p type obj)
      (unless noerror
        (error "%s is not of type %s" obj type))))

(defun wg-add-face (facekey str)
  "Return a copy of STR fontified according to FACEKEY.
FACEKEY must be a key in `wg-face-abbrevs'."
  (let ((face (wg-aget wg-face-abbrevs facekey))
        (str  (copy-seq str)))
    (unless face (error "No face with key %s" facekey))
    (if (not wg-use-faces) str
      (put-text-property 0 (length str) 'face face str)
      str)))

(defmacro wg-fontify (&rest specs)
  "A small fontification DSL. *WRITEME*"
  (declare (indent defun))
  `(concat
    ,@(wg-docar (spec specs)
        (typecase spec
          (cons (if (keywordp (car spec))
                    `(wg-add-face
                      ,(car spec)
                      ,(if (stringp (cadr spec))
                           (cadr spec)
                         `(format "%s" ,(cadr spec))))
                  `(progn ,spec)))
          (string `(progn ,spec))
          (atom `(format "%s" ,spec))))))

(defun wg-barf-on-active-minibuffer ()
  "Throw an error when the minibuffer is active."
  (when (active-minibuffer-window)
    (error "Workgroup operations aren't permitted while the \
minibuffer is active.")))

(defun wg-current-read-buffer-mode ()
  "Return the buffer switching package (ido or iswitchb) to use, or nil."
  (let ((ido-p (and (boundp 'ido-mode) (memq ido-mode '(both buffer))))
        (iswitchb-p (and (boundp 'iswitchb-mode) iswitchb-mode)))
    (cond ((eq wg-current-buffer-set 'fallback) 'fallback)
          ((and ido-p iswitchb-p)
           (if (< (position 'iswitchb-mode minor-mode-map-alist :key 'car)
                  (position 'ido-mode minor-mode-map-alist :key 'car))
               'iswitchb 'ido))
          (ido-p 'ido)
          (iswitchb-p 'iswitchb)
          (t 'fallback))))



;;; type predicates

(defun wg-buffer-p (obj)
  "Return t if OBJ is a Workgroups buffer, nil otherwise."
  (wg-type-p 'buffer obj))

(defun wg-window-p (obj)
  "Return t if OBJ is a Workgroups window, nil otherwise."
  (wg-type-p 'window obj))

(defun wg-wtree-p (obj)
  "Return t if OBJ is a Workgroups window tree, nil otherwise."
  (wg-type-p 'wtree obj))

(defun wg-wconfig-p (obj)
  "Return t if OBJ is a Workgroups window config, nil otherwise."
  (wg-type-p 'wconfig obj))

(defun wg-workgroup-p (obj)
  "Return t if OBJ is a workgroup, nil otherwise."
  (wg-type-p 'workgroup obj))



;;; window config utils

(defun wg-dir (w) (wg-aget w 'dir))

(defun wg-edges (w) (wg-aget w 'edges))

(defun wg-wlist (w) (wg-aget w 'wlist))

(defun wg-wtree (w) (wg-aget w 'wtree))

(defun wg-min-size (dir)
  "Return the minimum window size in split direction DIR."
  (if dir wg-window-min-height wg-window-min-width))

(defun wg-actual-min-size (dir)
  "Return the actual minimum window size in split direction DIR."
  (if dir wg-actual-min-height wg-actual-min-width))

(defmacro wg-with-edges (w spec &rest body)
  "Bind W's edge list to SPEC and eval BODY."
  (declare (indent 2))
  `(wg-dbind ,spec (wg-edges ,w) ,@body))

(defun wg-put-edges (w left top right bottom)
  "Return a copy of W with an edge list of LEFT TOP RIGHT and BOTTOM."
  (wg-aput w 'edges (list left top right bottom)))

(defmacro wg-with-bounds (w dir spec &rest body)
  "Bind SPEC to W's bounds in DIR, and eval BODY.
\"Bounds\" are a direction-independent way of dealing with edge lists."
  (declare (indent 3))
  (wg-with-gensyms (dir-sym l1 t1 r1 b1)
    (wg-dbind (ls1 hs1 lb1 hb1) spec
      `(wg-with-edges ,w (,l1 ,t1 ,r1 ,b1)
         (cond (,dir (let ((,ls1 ,l1) (,hs1 ,r1) (,lb1 ,t1) (,hb1 ,b1))
                       ,@body))
               (t    (let ((,ls1 ,t1) (,hs1 ,b1) (,lb1 ,l1) (,hb1 ,r1))
                       ,@body)))))))

(defun wg-put-bounds (w dir ls hs lb hb)
  "Set W's edges in DIR with bounds LS HS LB and HB."
  (if dir (wg-put-edges w ls lb hs hb) (wg-put-edges w lb ls hb hs)))

(defun wg-step-edges (edges1 edges2 hstep vstep)
  "Return W1's edges stepped once toward W2's by HSTEP and VSTEP."
  (wg-dbind (l1 t1 r1 b1) edges1
    (wg-dbind (l2 t2 r2 b2) edges2
      (let ((left (wg-step-to l1 l2 hstep))
            (top  (wg-step-to t1 t2 vstep)))
        (list left top
              (+ left (wg-step-to (- r1 l1) (- r2 l2) hstep))
              (+ top  (wg-step-to (- b1 t1) (- b2 t2) vstep)))))))

(defun wg-w-edge-operation (w edges op)
  "Return a copy of W with its edges mapped against EDGES through OP."
  (wg-aput w 'edges (mapcar* op (wg-aget w 'edges) edges)))

(defun wg-first-win (w)
  "Return the first actual window in W."
  (if (wg-window-p w) w (wg-first-win (car (wg-wlist w)))))

(defun wg-last-win (w)
  "Return the last actual window in W."
  (if (wg-window-p w) w (wg-last-win (wg-last1 (wg-wlist w)))))

(defun wg-minify-win (w)
  "Return a copy of W with the smallest allowable dimensions."
  (let* ((edges (wg-edges w))
         (left (car edges))
         (top (cadr edges)))
    (wg-put-edges w left top
                  (+ left wg-actual-min-width)
                  (+ top  wg-actual-min-height))))

(defun wg-minify-last-win (w)
  "Minify the last actual window in W."
  (wg-minify-win (wg-last-win w)))

(defun wg-wsize (w &optional height)
  "Return the width or height of W, calculated from its edge list."
  (wg-with-edges w (l1 t1 r1 b1)
    (if height (- b1 t1) (- r1 l1))))

(defun wg-adjust-wsize (w width-fn height-fn &optional new-left new-top)
  "Adjust W's width and height with WIDTH-FN and HEIGHT-FN."
  (wg-with-edges w (left top right bottom)
    (let ((left (or new-left left)) (top (or new-top top)))
      (wg-put-edges w left top
                    (+ left (funcall width-fn  (- right  left)))
                    (+ top  (funcall height-fn (- bottom top)))))))

(defun wg-scale-wsize (w width-scale height-scale)
  "Scale W's size by WIDTH-SCALE and HEIGHT-SCALE."
  (flet ((wscale (width)  (truncate (* width  width-scale)))
         (hscale (height) (truncate (* height height-scale))))
    (wg-adjust-wsize w #'wscale #'hscale)))

(defun wg-equal-wtrees (w1 w2)
  "Return t when W1 and W2 have equal structure."
  (cond ((and (wg-window-p w1) (wg-window-p w2))
         (equal (wg-edges w1) (wg-edges w2)))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (and (eq (wg-dir w1) (wg-dir w2))
              (equal (wg-edges w1) (wg-edges w2))
              (every #'wg-equal-wtrees (wg-wlist w1) (wg-wlist w2))))))

;; FIXME: Require a minimum size to fix wscaling
(defun wg-normalize-wtree (wtree)
  "Clean up and return a new wtree from WTREE.
Recalculate the edge lists of all subwins, and remove subwins
outside of WTREE's bounds.  If there's only one element in the
new wlist, return it instead of a new wtree."
  (if (wg-window-p wtree) wtree
    (wg-abind wtree (dir wlist)
      (wg-with-bounds wtree dir (ls1 hs1 lb1 hb1)
        (let* ((min-size (wg-min-size dir))
               (max (- hb1 1 min-size))
               (lastw (wg-last1 wlist)))
          (flet ((mapwl
                  (wl)
                  (wg-dbind (sw . rest) wl
                    (cons (wg-normalize-wtree
                           (wg-put-bounds
                            sw dir ls1 hs1 lb1
                            (setq lb1 (if (eq sw lastw) hb1
                                        (let ((hb2 (+ lb1 (wg-wsize sw dir))))
                                          (if (>= hb2 max) hb1 hb2))))))
                          (when (< lb1 max) (mapwl rest))))))
            (let ((new (mapwl wlist)))
              (if (cdr new) (wg-aput wtree 'wlist new)
                (car new)))))))))

(defun wg-scale-wtree (wtree wscale hscale)
  "Return a copy of WTREE with its dimensions scaled by WSCALE and HSCALE.
All WTREE's subwins are scaled as well."
  (let ((scaled (wg-scale-wsize wtree wscale hscale)))
    (if (wg-window-p wtree) scaled
      (wg-aput scaled
               'wlist (wg-docar (sw (wg-wlist scaled))
                        (wg-scale-wtree sw wscale hscale))))))

(defun wg-scale-wconfigs-wtree (wconfig new-width new-height)
  "Scale WCONFIG's wtree with NEW-WIDTH and NEW-HEIGHT.
Return a copy WCONFIG's wtree scaled with `wg-scale-wtree' by the
ratio or NEW-WIDTH to WCONFIG's width, and NEW-HEIGHT to
WCONFIG's height."
  (wg-normalize-wtree
   (wg-scale-wtree
    (wg-wtree wconfig)
    (/ (float new-width)  (wg-aget wconfig 'width))
    (/ (float new-height) (wg-aget wconfig 'height)))))

(defun w-set-frame-size-and-scale-wtree (wconfig &optional frame)
  "Set FRAME's size to WCONFIG's, returning a possibly scaled wtree.
If the frame size was set correctly, return WCONFIG's wtree
unchanged.  If it wasn't, return a copy of WCONFIG's wtree scaled
with `wg-scale-wconfigs-wtree' to fit the frame as it exists."
  (let ((frame (or frame (selected-frame))))
    (wg-abind wconfig ((wcwidth width) (wcheight height))
      (when window-system (set-frame-size frame wcwidth wcheight))
      (let ((fwidth  (frame-parameter frame 'width))
            (fheight (frame-parameter frame 'height)))
        (if (and (= wcwidth fwidth) (= wcheight fheight))
            (wg-wtree wconfig)
          (wg-scale-wconfigs-wtree wconfig fwidth fheight))))))

(defun wg-reverse-wlist (w &optional dir)
  "Reverse W's wlist and those of all its sub-wtrees in direction DIR.
If DIR is nil, reverse WTREE horizontally.
If DIR is 'both, reverse WTREE both horizontally and vertically.
Otherwise, reverse WTREE vertically."
  (flet ((inner (w) (if (wg-window-p w) w
                      (wg-abind w ((d1 dir) edges wlist)
                        (wg-make-wtree-node
                         d1 edges
                         (let ((wl2 (mapcar #'inner wlist)))
                           (if (or (eq dir 'both)
                                   (and (not dir) (not d1))
                                   (and dir d1))
                               (nreverse wl2) wl2)))))))
    (wg-normalize-wtree (inner w))))

(defun wg-reverse-wconfig (&optional dir wconfig)
  "Reverse WCONFIG's wtree's wlist in direction DIR."
  (let ((wc (or wconfig (wg-make-wconfig))))
    (wg-aput wc 'wtree (wg-reverse-wlist (wg-aget wc 'wtree) dir))))

(defun wg-wtree-move-window (wtree offset)
  "Offset `selected-window' OFFSET places in WTREE."
  (flet ((inner (w) (if (wg-window-p w) w
                      (wg-abind w ((d1 dir) edges wlist)
                        (wg-make-wtree-node
                         d1 edges
                         (wg-aif (wg-get1 (sw wlist) (wg-aget sw 'selwin))
                             (wg-cyclic-offset-elt it wlist offset)
                           (mapcar #'inner wlist)))))))
    (wg-normalize-wtree (inner wtree))))

(defun wg-wconfig-move-window (offset &optional wconfig)
  "Offset `selected-window' OFFSET places in WCONFIG."
  (let ((wc (or wconfig (wg-make-wconfig))))
    (wg-aput wc 'wtree (wg-wtree-move-window (wg-aget wc 'wtree) offset))))



;;; wconfig construction

(defun wg-window-point (ewin)
  "Return `point' or :max.  See `wg-restore-point-max'.
EWIN should be an Emacs window object."
  (let ((p (window-point ewin)))
    (if (and wg-restore-point-max (= p (point-max))) :max p)))

(defun wg-serialize-buffer (buffer)
  "Return a serialized buffer from BUFFER."
  (with-current-buffer buffer
    `((type        .    buffer)
      (bname       .   ,(buffer-name))
      (fname       .   ,(buffer-file-name))
      (major-mode  .   ,major-mode)
      (mark        .   ,(mark))
      (markx       .   ,mark-active))))

(defun wg-serialize-window (window)
  "Return a serialized window from WINDOW."
  (let ((selwin (eq window (selected-window))))
    (with-selected-window window
      `((type     .   window)
        (buffer   .  ,(wg-serialize-buffer (window-buffer window)))
        (edges    .  ,(window-edges window))
        (point    .  ,(wg-window-point window))
        (wstart   .  ,(window-start window))
        (hscroll  .  ,(window-hscroll window))
        (sbars    .  ,(window-scroll-bars window))
        (margins  .  ,(window-margins window))
        (fringes  .  ,(window-fringes window))
        (selwin   .  ,selwin)
        (mbswin   .  ,(eq window minibuffer-scroll-window))))))

(defun wg-make-wtree-node (dir edges wlist)
  "Return a new serialized wtree node from DIR EDGES and WLIST."
  `((type   .   wtree)
    (dir    .  ,dir)
    (edges  .  ,edges)
    (wlist  .  ,wlist)))

(defun wg-serialize-window-tree (window-tree)
  "Return a serialized window-tree from WINDOW-TREE."
  (wg-barf-on-active-minibuffer)
  (flet ((inner (w) (if (windowp w) (wg-serialize-window w)
                      (wg-dbind (dir edges . wins) w
                        (wg-make-wtree-node
                         dir edges (mapcar #'inner wins))))))
    (let ((w (car window-tree)))
      (when (and (windowp w) (window-minibuffer-p w))
        (error "Workgroups can't operate on minibuffer-only frames."))
      (inner w))))

(defun wg-make-wconfig ()
  "Return a new Workgroups window config from `selected-frame'."
  `((type    .   wconfig)
    (left    .  ,(frame-parameter nil 'left))
    (top     .  ,(frame-parameter nil 'top))
    (width   .  ,(frame-parameter nil 'width))
    (height  .  ,(frame-parameter nil 'height))
    (sbars   .  ,(frame-parameter nil 'vertical-scroll-bars))
    (sbwid   .  ,(frame-parameter nil 'scroll-bar-width))
    (wtree   .  ,(wg-serialize-window-tree (window-tree)))))

(defun wg-make-blank-wconfig (&optional buffer)
  "Return a new blank wconfig.
BUFFER or `wg-default-buffer' is visible in the only window."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer wg-default-buffer))
    (wg-make-wconfig)))



;;; wconfig restoration

(defun wg-restore-buffer (wg-buffer)
  "Restore WG-BUFFER and return it."
  (wg-abind wg-buffer (fname bname (mm major-mode) mark markx)
    (let ((buffer (cond ((not fname) (get-buffer bname))
                        ((file-exists-p fname)
                         (with-current-buffer (find-file-noselect fname)
                           (when (and (fboundp mm) (not (eq mm major-mode)))
                             (funcall mm))
                           (rename-buffer bname t)
                           (current-buffer)))
                        (wg-barf-on-nonexistent-file
                         (error "%S doesn't exist" fname)))))
      (when buffer
        (with-current-buffer buffer
          (set-mark mark)
          (unless markx (deactivate-mark))
          buffer)))))

(defun wg-restore-window (wg-window)
  "Restore WG-WINDOW in `selected-window'."
  (wg-abind wg-window ((wg-buffer buffer) point wstart hscroll
                       sbars fringes margins selwin mbswin)
    (let ((sw (selected-window))
          (buffer (wg-restore-buffer wg-buffer)))
      (when selwin (setq wg-selected-window sw))
      (if (not buffer) (switch-to-buffer wg-default-buffer t)
        (switch-to-buffer buffer t)
        (set-window-start sw wstart t)
        (set-window-point
         sw (cond ((not wg-restore-point) wstart)
                  ((eq point :max) (point-max))
                  (t point)))
        (when (>= wstart (point-max)) (recenter))
        (when (and wg-restore-mbs-window mbswin)
          (setq minibuffer-scroll-window sw))
        (when wg-restore-scroll-bars
          (wg-dbind (width cols vtype htype) sbars
            (set-window-scroll-bars sw width vtype htype)))
        (when wg-restore-fringes
          (apply #'set-window-fringes sw fringes))
        (when wg-restore-margins
          (set-window-margins sw (car margins) (cdr margins)))
        (set-window-hscroll sw hscroll)))))

(defun wg-restore-window-tree (wg-window-tree)
  "Restore WG-WINDOW-TREE in `selected-frame'."
  (flet ((inner (w) (if (wg-wtree-p w)
                        (wg-abind w ((d dir) wlist)
                          (let ((lastw (wg-last1 wlist)))
                            (dolist (sw wlist)
                              (unless (eq sw lastw)
                                (split-window nil (wg-wsize sw d) (not d)))
                              (inner sw))))
                      (wg-restore-window w)
                      (other-window 1))))
    (let ((window-min-width  wg-window-min-width)
          (window-min-height wg-window-min-height))
      (delete-other-windows)
      (setq wg-selected-window nil)
      (inner wg-window-tree)
      (wg-awhen wg-selected-window (select-window it)))))

(defun wg-restore-wconfig (wconfig)
  "Restore WCONFIG in `selected-frame'."
  (wg-barf-on-active-minibuffer)
  (let ((frame (selected-frame)) (wtree nil))
    (wg-abind wconfig (left top sbars sbwid)
      (setq wtree (w-set-frame-size-and-scale-wtree wconfig frame))
      (when (and wg-restore-position left top)
        (set-frame-position frame left top))
      (when (and wg-morph-on after-init-time)
        (wg-morph (wg-serialize-window-tree (window-tree))
                  wtree wg-morph-no-error))
      (wg-restore-window-tree wtree)
      (when wg-restore-scroll-bars
        (set-frame-parameter frame 'vertical-scroll-bars sbars)
        (set-frame-parameter frame 'scroll-bar-width sbwid)))))

(defun wg-restore-wconfig-undoably (wconfig &optional noflag noupdate)
  "Restore WCONFIG in `selected-frame', saving undo information."
  (let ((wg-flag-wconfig-changes nil))
    (unless noflag (setq wg-window-config-changed-p t))
    (unless noupdate (wg-update-workgroup-working-config))
    (wg-restore-wconfig wconfig)))



;;; morph

(defun wg-morph-step-edges (w1 w2)
  "Step W1's edges toward W2's by `wg-morph-hsteps' and `wg-morph-vsteps'."
  (wg-step-edges (wg-edges w1) (wg-edges w2)
                 wg-morph-hsteps wg-morph-vsteps))

(defun wg-morph-determine-steps (gui-steps &optional term-steps)
  (max 1 (if (and (not window-system) term-steps) term-steps gui-steps)))

(defun wg-morph-match-wlist (wt1 wt2)
  "Return a wlist by matching WT1's wlist to WT2's.
When wlist1's and wlist2's lengths are equal, return wlist1.
When wlist1 is shorter than wlist2, add a window at the front of wlist1.
When wlist1 is longer than wlist2, package up wlist1's excess windows
into a wtree, so it's the same length as wlist2."
  (let* ((wl1 (wg-wlist wt1)) (l1 (length wl1)) (d1 (wg-dir wt1))
         (wl2 (wg-wlist wt2)) (l2 (length wl2)))
    (cond ((= l1 l2) wl1)
          ((< l1 l2)
           (cons (wg-minify-last-win (wg-rnth (1+ l1) wl2))
                 (if (< (wg-wsize (car wl1) d1)
                        (* 2 (wg-actual-min-size d1)))
                     wl1
                   (cons (wg-w-edge-operation (car wl1) wg-min-edges #'-)
                         (cdr wl1)))))
          ((> l1 l2)
           (append (wg-take wl1 (1- l2))
                   (list (wg-make-wtree-node d1 wg-null-edges
                                             (nthcdr (1- l2) wl1))))))))

(defun wg-morph-win->win (w1 w2 &optional swap)
  "Return a copy of W1 with its edges stepped once toward W2.
When SWAP is non-nil, return a copy of W2 instead."
  (wg-aput (if swap w2 w1) 'edges (wg-morph-step-edges w1 w2)))

(defun wg-morph-win->wtree (win wt)
  "Return a new wtree with WIN's edges and WT's last two windows."
  (wg-make-wtree-node
   (wg-dir wt)
   (wg-morph-step-edges win wt)
   (let ((wg-morph-hsteps 2) (wg-morph-vsteps 2))
     (wg-docar (w (wg-leave (wg-wlist wt) 2))
       (wg-morph-win->win (wg-minify-last-win w) w)))))

(defun wg-morph-wtree->win (wt win &optional noswap)
  "Grow the first window of WT and its subtrees one step toward WIN.
This eventually wipes WT's components, leaving only a window.
Swap WT's first actual window for WIN, unless NOSWAP is non-nil."
  (if (wg-window-p wt) (wg-morph-win->win wt win (not noswap))
    (wg-make-wtree-node
     (wg-dir wt)
     (wg-morph-step-edges wt win)
     (wg-dbind (fwin . wins) (wg-wlist wt)
       (cons (wg-morph-wtree->win fwin win noswap)
             (wg-docar (sw wins)
               (if (wg-window-p sw) sw
                 (wg-morph-wtree->win sw win t))))))))

(defun wg-morph-wtree->wtree (wt1 wt2)
  "Return a new wtree morphed one step toward WT2 from WT1.
Mutually recursive with `wg-morph-dispatch' to traverse the
structures of WT1 and WT2 looking for discrepancies."
  (let ((d1 (wg-dir wt1)) (d2 (wg-dir wt2)))
    (wg-make-wtree-node
     d2 (wg-morph-step-edges wt1 wt2)
     (if (not (eq (wg-dir wt1) (wg-dir wt2)))
         (list (wg-minify-last-win wt2) wt1)
       (mapcar* #'wg-morph-dispatch
                (wg-morph-match-wlist wt1 wt2)
                (wg-wlist wt2))))))

(defun wg-morph-dispatch (w1 w2)
  "Return a wtree morphed one step toward W2 from W1.
Dispatches on each possible combination of types."
  (cond ((and (wg-window-p w1) (wg-window-p w2))
         (wg-morph-win->win w1 w2 t))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (wg-morph-wtree->wtree w1 w2))
        ((and (wg-window-p w1) (wg-wtree-p w2))
         (wg-morph-win->wtree w1 w2))
        ((and (wg-wtree-p w1) (wg-window-p w2))
         (wg-morph-wtree->win w1 w2))))

(defun wg-morph (from to &optional noerror)
  "Morph from wtree FROM to wtree TO.
Assumes both FROM and TO fit in `selected-frame'."
  (let ((wg-morph-hsteps
         (wg-morph-determine-steps wg-morph-hsteps wg-morph-terminal-hsteps))
        (wg-morph-vsteps
         (wg-morph-determine-steps wg-morph-vsteps wg-morph-terminal-vsteps))
        (wg-flag-wconfig-changes nil)
        (wg-restore-scroll-bars nil)
        (wg-restore-fringes nil)
        (wg-restore-margins nil)
        (wg-restore-point nil)
        (truncate-partial-width-windows
         wg-morph-truncate-partial-width-windows)
        (watchdog 0))
    (condition-case err
        (wg-until (wg-equal-wtrees from to)
          (when (> (incf watchdog) wg-morph-max-steps)
            (error "`wg-morph-max-steps' exceeded"))
          (setq from (wg-normalize-wtree (wg-morph-dispatch from to)))
          (wg-restore-window-tree from)
          (redisplay)
          (unless (zerop wg-morph-sit-for-seconds)
            (sit-for wg-morph-sit-for-seconds t)))
      (error (if noerror (message "%S" err) (error "%S" err))))))



;;; global error wrappers

(defun wg-file (&optional noerror)
  "Return `wg-file' or error."
  (or wg-file
      (unless noerror
        (error "Workgroups isn't visiting a file"))))

(defun wg-list (&optional noerror)
  "Return `wg-list' or error."
  (or wg-list
      (unless noerror
        (error "No workgroups are defined."))))

(defun wg-get-workgroup (key val &optional error)
  "Return the workgroup whose KEY equals VAL or error."
  (or (wg-get-alist key val (wg-list))
      (when error
        (error "There's no workgroup with key %S and value %S" key val))))

(defun wg-get-workgroup-by-name (name &optional noerror)
  "Return the workgroup with name NAME."
  (or (wg-get-workgroup 'name name)
      (unless noerror
        (error "There's no workgroup named %S" name))))

(defun wg-get-workgroup-by-uid (uid &optional noerror)
  "Return the workgroup with uid UID."
  (or (wg-get-workgroup 'uid uid)
      (unless noerror
        (error "There's no workgroup with a UID of %S" uid))))

(defun wg-get-workgroup-flexibly (obj &optional noerror)
  "Return a workgroup from OBJ.
If OBJ is a workgroup, return it.
If OBJ is a string, return the workgroup with that name.
If OBJ is an integer, return the workgroup with that uid.
If OBJ is null, return the current workgroup."
  (cond ((stringp obj) (wg-get-workgroup-by-name obj noerror))
        ((integerp obj) (wg-get-workgroup-by-uid obj noerror))
        ((null obj) (wg-current-workgroup noerror))
        ((wg-type-check 'workgroup obj noerror) obj)))



;;; workgroup parameters

(defun wg-workgroup-parameter (workgroup parameter)
  "Return WORKGROUP's value for PARAMETER.
WORKGROUP should be accepted by `wg-get-workgroup-flexibly'."
  (wg-aget (wg-get-workgroup-flexibly workgroup) parameter))

(defun wg-set-workgroup-parameter (workgroup parameter value)
  "Set WORKGROUP's value of PARAMETER to VALUE.
Sets `wg-dirty' to t unless NODIRTY.  WORKGROUP should be a value
accepted by `wg-get-workgroup-flexibly'.  Return VALUE."
  (let ((wg (wg-get-workgroup-flexibly workgroup)))
    (unless wg-nodirty
      (wg-mark-workgroup-dirty wg)
      (setq wg-dirty t))
    (wg-aset! wg parameter value)
    value))

;; (defun wg-remove-workgroup-parameter (workgroup parameter)
;;   ""
;;   ;; FIXME: write this
;;   )


;; dirty flags

(defun wg-workgroup-dirty-p (workgroup)
  "Return the value of WORKGROUP's dirty flag."
  (wg-workgroup-parameter workgroup 'dirty))

(defun wg-dirty-p ()
  "Return t when `wg-dirty' or any workgroup dirty parameter all is non-nil."
  (or wg-dirty (some 'wg-workgroup-dirty-p wg-list)))

(defun wg-mark-workgroup-dirty (workgroup)
  "Set WORKGROUP's dirty flag to t."
  (wg-aset! workgroup 'dirty t))

(defun wg-mark-workgroup-clean (workgroup)
  "Set WORKGROUP's dirty flag to nil."
  (wg-aset! workgroup 'dirty nil))

(defun wg-mark-everything-clean ()
  "Set `wg-dirty' and all workgroup dirty flags to nil."
  (mapc 'wg-mark-workgroup-clean wg-list)
  (setq wg-dirty nil))


;; workgroup uid

(defun wg-uid (workgroup)
  "Return WORKGROUP's uid."
  (wg-workgroup-parameter workgroup 'uid))

(defun wg-set-uid (workgroup uid)
  "Set the uid of WORKGROUP to UID."
  (wg-set-workgroup-parameter workgroup 'uid uid))

(defun wg-uids (&optional noerror)
  "Return a list of workgroups uids."
  (mapcar 'wg-uid (wg-list noerror)))

(defun wg-new-uid ()
  "Return a uid greater than any in `wg-list'."
  (let ((uids (wg-uids t)) (new -1))
    (dolist (uid uids (1+ new))
      (setq new (max uid new)))))


;; workgroup name

(defun wg-workgroup-name (workgroup)
  "Return the name of WORKGROUP."
  (wg-workgroup-parameter workgroup 'name))

(defun wg-set-workgroup-name (workgroup name)
  "Set the name of WORKGROUP to NAME."
  (wg-set-workgroup-parameter workgroup 'name name))

(defun wg-workgroup-names (&optional noerror)
  "Return a list of workgroup names."
  (mapcar 'wg-workgroup-name (wg-list noerror)))


;; workgroup assigned buffers

(defun wg-workgroup-assigned-buffers (workgroup)
  "Return the assigned-buffers of WORKGROUP."
  (wg-workgroup-parameter workgroup 'assigned-buffers))

(defun wg-set-workgroup-assigned-buffers (workgroup assigned-buffers)
  "Set the assigned-buffers of WORKGROUP to ASSIGNED-BUFFERS."
  (wg-set-workgroup-parameter workgroup 'assigned-buffers assigned-buffers))

(defun wg-wgbuf-refers-to-p (wgbuf buffer-or-name)
  "Return t if WGBUF refers to BUFFER-OR-NAME, nil otherwise."
  (let ((wg-fname (wg-aget wgbuf 'fname))
        (ebuf (get-buffer buffer-or-name)))
    (cond (wg-fname (equal wg-fname (buffer-file-name ebuf)))
          ((buffer-file-name ebuf) nil)
          ((equal (wg-aget wgbuf 'bname) (buffer-name ebuf))))))

(defun wg-get-buffer (wg-buffer &optional buffer-list)
  "Return the buffer in BUFFER-LIST corresponding to WG-BUFFER, or nil."
  (wg-abind wg-buffer (fname bname)
    (let ((buffer (if fname (get-file-buffer fname) (get-buffer bname))))
      (when buffer (buffer-name buffer)))))

(defun wg-get-wg-buffer (buffer-or-name &optional workgroup)
  "Return the wg-buffer in WORKGROUP corresponding to BUFFER-OR-NAME, or nil."
  (wg-get1 (wgbuf (wg-workgroup-assigned-buffers
                   (wg-get-workgroup-flexibly workgroup)))
    (wg-wgbuf-refers-to-p wgbuf buffer-or-name)))

;; FIXME: refactor this into add-buffer, update-buffer, and add-or-update-buffer
(defun wg-workgroup-add-buffer
  (workgroup buffer-or-name &optional append overwrite noerror)
  "Add BUFFER-OR-NAME to WORKGROUP."
  (let* ((wg (wg-get-workgroup-flexibly workgroup))
         (buffer (get-buffer buffer-or-name))
         (assigned (wg-workgroup-assigned-buffers wg))
         (old-wg-buffer (wg-get-wg-buffer buffer wg))
         (new-wg-buffer (wg-serialize-buffer buffer)))
    (cond ((not old-wg-buffer)
           (wg-set-workgroup-assigned-buffers
            wg (if append (append assigned (list new-wg-buffer))
                 (cons new-wg-buffer assigned))))
          (overwrite
           (wg-set-workgroup-assigned-buffers
            wg (subst-if new-wg-buffer
                         (lambda (elt) (eq elt old-wg-buffer))
                         assigned))
           nil)
          (noerror nil)
          (t (error "%S has already been added to %S"
                    (buffer-name buffer) (wg-workgroup-name wg))))))

(defun wg-workgroup-remove-buffer (workgroup buffer-or-name &optional noerror)
  "Remove BUFFER-OR-NAME from WORKGROUP."
  (let* ((wg (wg-get-workgroup-flexibly workgroup))
         (buffer (get-buffer buffer-or-name))
         (wg-buffer (wg-get-wg-buffer buffer workgroup)))
    (if (not wg-buffer)
        (unless noerror (error "%S has not been assigned to %S"
                               (buffer-name buffer) (wg-workgroup-name wg)))
      (wg-set-workgroup-assigned-buffers
       wg (remove wg-buffer (wg-workgroup-assigned-buffers wg))))))

(defun wg-workgroup-live-buffers (workgroup &optional names)
  "Return a list of WORKGROUP's live assigned buffers."
  (let ((wg (wg-get-workgroup-flexibly workgroup)))
    (wg-filter-map (lambda (buffer)
                     (when (wg-get-wg-buffer buffer wg)
                       (if names (buffer-name buffer) buffer)))
                   (buffer-list))))

(defun wg-auto-remove-buffer-hook ()
  "`kill-buffer-hook' that automatically removes buffers from workgroups."
  (when wg-auto-remove-buffer-on-kill-buffer
    (wg-awhen (wg-current-workgroup t)
      (wg-workgroup-remove-buffer it (current-buffer) t))))

(defun wg-auto-add-buffer-hook ()
  "`find-file-hook' that automatically adds buffers to workgroups."
  (when wg-auto-add-buffer-on-find-file
    (wg-awhen (wg-current-workgroup t)
      (wg-workgroup-add-buffer it (current-buffer) t t))))


;; workgroup buffer list filters

(defun wg-workgroup-buffer-list-filters (workgroup)
  "Return the buffer-list-filters of WORKGROUP."
  (wg-workgroup-parameter workgroup 'buffer-list-filters))

(defun wg-set-workgroup-buffer-list-filters (workgroup buffer-list-filters)
  "Set the buffer-list-filters of WORKGROUP to BUFFER-LIST-FILTERS."
  (wg-set-workgroup-parameter
   workgroup 'buffer-list-filters buffer-list-filters))

;; FIXME: determine consistent arg ordering for this and `add-buffer'
(defun wg-add-workgroup-buffer-list-filter
  (workgroup name form &optional append overwrite)
  "Add a filter with name NAME and form FORM to WORKGROUP's filter list.
If a filter named NAME already exists, and OVERWRITE is non-nil,
overwrite it.  Otherwise error."
  (let ((filters (wg-workgroup-buffer-list-filters workgroup))
        (filter (list name form)))
    (wg-awhen (assoc name filters)
      (if overwrite (setq filters (remove it filters))
        (error "%S already has a filter named %S"
               (wg-workgroup-name workgroup) name)))
    (wg-set-workgroup-buffer-list-filters
     workgroup (if append (append filters (list filter))
                 (cons filter filters)))))

(defun wg-remove-workgroup-buffer-list-filter (workgroup filter-name)
  "Remove the filter named NAME from WORKGROUP's filter list."
  (let* ((filters (wg-workgroup-buffer-list-filters workgroup))
         (filter (assoc filter-name filters)))
    (wg-set-workgroup-buffer-list-filters workgroup (remove filter filters))))

(defun wg-workgroup-filtered-buffer-list (workgroup &optional buffer-list)
  "Run WORKGROUP's filters on BUFFER-LIST or `buffer-list'."
  (let ((wg-temp-buffer-list (or buffer-list (wg-emacs-buffer-names)))
        (filters (wg-workgroup-buffer-list-filters workgroup)))
    (dolist (filter filters wg-temp-buffer-list)
      (wg-dbind (name form) filter
        (condition-case err
            (cond ((symbolp form) (funcall form))
                  ((consp form) (eval form))
                  (t (error "Invalid filter type %S" form)))
          (error
           (let ((msg (format "Error in filter in %S - %S" name err)))
             (if wg-barf-on-filter-error (error msg)
               (message msg)
               (sit-for wg-warning-timeout)))))))))


;; standard filters

;; FIXME: Add wg-filter-and-assign to filter the buffer-list and assign the results to a workgroup
;; FIXME: write a recent buffers filter

(defun wg-filter-temp-buffer-list (pred)
  "Remove from `wg-temp-buffer-list' those buffers that don't satisfy PRED."
  (setq wg-temp-buffer-list (remove-if-not pred wg-temp-buffer-list)))

(defun wg-filter-buffer-list-by-regexp (regexp)
  "Filter `wg-temp-buffer-list' for buffer-names matching REGEXP."
  (wg-filter-temp-buffer-list (lambda (bname) (string-match regexp bname))))

(defun wg-filter-buffer-list-by-root-path (root-path)
  "Filter `wg-temp-buffer-list' for buffer-file-names under ROOT-PATH."
  (wg-filter-temp-buffer-list
   (lambda (bname)
     (let ((fname (buffer-file-name (get-buffer bname))))
       (when fname (wg-file-under-root-path-p root-path fname))))))

(defun wg-filter-buffer-list-by-major-mode (major-mode-symbol)
  "Filter `wg-temp-buffer-list' for buffers buffer-file-names under ROOT-PATH."
  (wg-filter-temp-buffer-list
   (lambda (bname)
     (eq major-mode-symbol (with-current-buffer (get-buffer bname) major-mode)))))


;; workgroup completion set order

;; (defun wg-workgroup-buffer-set-order (workgroup)
;;   "Return the buffer-set-order of WORKGROUP."
;;   (wg-workgroup-parameter workgroup 'buffer-set-order))

;; (defun wg-set-workgroup-buffer-set-order (workgroup buffer-set-order)
;;   "Set the buffer-set-order of WORKGROUP."
;;   (wg-set-workgroup-parameter
;;    workgroup 'buffer-set-order buffer-set-order))


;; buffer-set construction

(defun wg-default-buffer-set-order ()
  "Wrapper for if `wg-default-buffer-set-order' gets set to nil."
  (or wg-default-buffer-set-order '(all fallback)))

(defcustom wg-all-buffer-set-designator "all"
  "Prepended to read-buffer prompts to designate the
\"all\" buffer-set."
  :type 'string
  :group 'workgroups)

(defcustom wg-assigned-buffer-set-designator "a--"
  "Prepended to read-buffer prompts to designate the
\"assigned\" buffer-set."
  :type 'string
  :group 'workgroups)

(defcustom wg-filtered-buffer-set-designator "--f"
  "Prepended to read-buffer prompts to designate the
\"filtered\" buffer-set."
  :type 'string
  :group 'workgroups)

(defcustom wg-assigned-and-filtered-buffer-set-designator "a+f"
  "Prepended to read-buffer prompts to designate the
\"assigned-and-filtered\" buffer-set."
  :type 'string
  :group 'workgroups)

(defcustom wg-fallback-buffer-set-designator "<--"
  "Prepended to read-buffer prompts to designate the
\"all\" buffer-set."
  :type 'string
  :group 'workgroups)

(defun wg-buffer-method-string ()
  "Return a string indicating current buffer method."
  (case wg-current-buffer-method
    (selected-window "Buffer")
    (other-window "Switch to buffer in other window")
    (other-frame "Switch to buffer in other frame")
    (insert "Insert buffer")
    (display "Display buffer")
    (kill "Kill buffer")))

(defun wg-buffer-set-string (&optional workgroup)
  "Return a string indicating WORKGROUP's name and the current buffer set."
  (format
   "(%s %s)"
   (wg-workgroup-name (wg-get-workgroup-flexibly workgroup))
   (case wg-current-buffer-set
     (all wg-all-buffer-set-designator)
     (assigned wg-assigned-buffer-set-designator)
     (filtered wg-filtered-buffer-set-designator)
     (assigned-and-filtered wg-assigned-and-filtered-buffer-set-designator)
     (fallback wg-fallback-buffer-set-designator))))

(defun wg-buffer-set-prompt (&optional workgroup)
  "Return a prompt string indicating WORKGROUP and buffer set."
  (format "%s %s: " (wg-buffer-method-string) (wg-buffer-set-string workgroup)))

;; FIXME: assigned-then-filtered, filtered-then-assigned and recent
;; FIXME: generalize all this stuff, so there aren't so many special cases
(defun wg-make-buffer-set (&optional initial-buffer-list workgroup buffer-set)
  "Return the final list of buffer name completions."
  (let ((wg (wg-get-workgroup-flexibly workgroup))
        (buffer-set (or buffer-set wg-current-buffer-set))
        (initial (or initial-buffer-list (wg-emacs-buffer-names)))
        (final nil))
    (when (eq buffer-set 'all)
      (setq final initial))
    (when (memq buffer-set '(filtered assigned-and-filtered))
      (setq final (wg-workgroup-filtered-buffer-list wg initial)))
    (when (memq buffer-set '(assigned assigned-and-filtered))
      (dolist (buf (nreverse (wg-workgroup-live-buffers wg t)))
        (pushnew buf final)))
    (case wg-current-buffer-method
      ((selected-window other-window other-frame insert display)
       (when (equal (car final) (buffer-name (current-buffer)))
         (setq final (wg-rotlst final)))))
    final))


;; buffer-set context

(defun wg-determine-buffer-set-order (&optional workgroup)
  "Return WORKGROUP's buffer-set order."
  (or (wg-workgroup-parameter workgroup 'switch-to-buffer-buffer-set-order)
      wg-switch-to-buffer-buffer-set-order
      (wg-default-buffer-set-order)))

(defmacro wg-with-buffer-sets (method &rest body)
  "Create context for buffer method METHOD, and eval BODY."
  (declare (indent 1))
  (wg-with-gensyms (order status)
    `(let* ((wg-current-buffer-method ,method)
            (,order (wg-determine-buffer-set-order)))
       (if (or (not wg-buffer-sets-on) (not (wg-current-workgroup t)))
           (progn ,@body)
         (catch 'result
           (while 'your-mom
             (let* ((wg-current-buffer-set (car ,order))
                    (,status (catch 'status (list 'done (progn ,@body)))))
               (case (car ,status)
                 (done (throw 'result (cadr ,status)))
                 (next (setq ,order (wg-rotlst ,order (cadr ,status))))
                 (prev (setq ,order (wg-rotlst ,order (cadr ,status))))))))))))



;;; current and previous workgroup ops

(defun wg-current-workgroup (&optional noerror frame)
  "Return the current workgroup."
  (let ((uid (frame-parameter frame 'wg-current-workgroup-uid)))
    (if uid (wg-get-workgroup-by-uid uid noerror)
      (unless noerror
        (error "There's no current workgroup in this frame.")))))

(defun wg-previous-workgroup (&optional noerror frame)
  "Return the previous workgroup."
  (let ((uid (frame-parameter frame 'wg-previous-workgroup-uid)))
    (if uid (wg-get-workgroup-by-uid uid noerror)
      (unless noerror
        (error "There's no previous workgroup in this frame.")))))

(defun wg-set-current-workgroup (workgroup &optional frame)
  "Set the current workgroup to WORKGROUP."
  (set-frame-parameter
   frame 'wg-current-workgroup-uid (when workgroup (wg-uid workgroup))))

(defun wg-set-previous-workgroup (workgroup &optional frame)
  "Set the previous workgroup to WORKGROUP."
  (set-frame-parameter
   frame 'wg-previous-workgroup-uid (when workgroup (wg-uid workgroup))))

(defun wg-current-workgroup-p (workgroup &optional noerror)
  "Return t when WORKGROUP is the current workgroup, nil otherwise."
  (wg-awhen (wg-current-workgroup noerror)
    (eq it workgroup)))

(defun wg-previous-workgroup-p (workgroup &optional noerror)
  "Return t when WORKGROUP is the previous workgroup, nil otherwise."
  (wg-awhen (wg-previous-workgroup noerror)
    (eq it workgroup)))



;;; workgroup base config

(defun wg-set-workgroup-base-config (workgroup base-config)
  "Set the base-config of WORKGROUP to BASE-CONFIG."
  (wg-set-workgroup-parameter workgroup 'base-config base-config))

(defun wg-workgroup-base-config (workgroup)
  "Return the base-config of WORKGROUP."
  (wg-workgroup-parameter workgroup 'base-config))



;;; workgroup working config and window config undo/redo

(defun wg-workgroup-table (&optional frame)
  "Return FRAME's workgroup table, creating it first if necessary."
  (or (frame-parameter frame 'wg-workgroup-table)
      (let ((wt (make-hash-table)))
        (set-frame-parameter frame 'wg-workgroup-table wt)
        wt)))

(defun wg-workgroup-state-table (workgroup &optional frame)
  "Return FRAME's WORKGROUP's state table."
  (let ((uid (wg-uid workgroup)) (wt (wg-workgroup-table frame)))
    (or (gethash uid wt)
        (let ((wst (make-hash-table)))
          (puthash 'undo-pointer 0 wst)
          (puthash 'undo-list (list (wg-workgroup-base-config workgroup)) wst)
          (puthash uid wst wt)
          wst))))

(defmacro wg-with-undo (workgroup spec &rest body)
  "Bind WORKGROUP's undo state to SPEC and eval BODY."
  (declare (indent 2))
  (wg-dbind (state-table undo-pointer undo-list) spec
    `(let* ((,state-table (wg-workgroup-state-table ,workgroup))
            (,undo-pointer (gethash 'undo-pointer ,state-table))
            (,undo-list (gethash 'undo-list ,state-table)))
       ,@body)))

(defun wg-set-workgroup-working-config (workgroup config)
  "Set the working config of WORKGROUP to CONFIG."
  (wg-with-undo workgroup (state-table undo-pointer undo-list)
    (setcar (nthcdr undo-pointer undo-list) config)))

(defun wg-update-workgroup-working-config (&optional workgroup)
  "Update WORKGROUP's working config with CONFIG or `wg-make-wconfig'."
  (let ((cur (wg-current-workgroup t)))
    (when (and cur (or (not workgroup) (eq workgroup cur)))
      (wg-set-workgroup-working-config cur (wg-make-wconfig)))))

(defun wg-workgroup-working-config (workgroup)
  "Return WORKGROUP's working config, which is its current undo state."
  (or (wg-update-workgroup-working-config workgroup)
      (wg-with-undo workgroup (state-table undo-pointer undo-list)
        (nth undo-pointer undo-list))))

(defun wg-push-new-workgroup-working-config (workgroup config)
  "Push CONFIG onto WORKGROUP's undo list, truncating its future if necessary."
  (wg-with-undo workgroup (state-table undo-pointer undo-list)
    (let ((undo-list (cons config (nthcdr undo-pointer undo-list))))
      (when (and wg-wconfig-undo-list-max
                 (> (length undo-list) wg-wconfig-undo-list-max))
        (setq undo-list (wg-take undo-list wg-wconfig-undo-list-max)))
      (puthash 'undo-list undo-list state-table)
      (puthash 'undo-pointer 0 state-table))))


;; undo/redo hook functions
;;
;; Exempting minibuffer-related window-config changes from undoification is
;; tricky, which is why all the flag-setting hooks.
;;
;; Example hook call order:
;;
;; pre-command-hook called ido-switch-buffer
;; window-configuration-change-hook called
;; minibuffer-setup-hook called
;; post-command-hook called
;; pre-command-hook called self-insert-command
;; post-command-hook called
;; ...
;; pre-command-hook called self-insert-command
;; post-command-hook called
;; pre-command-hook called ido-exit-minibuffer
;; minibuffer-exit-hook called
;; window-configuration-change-hook called [2 times]
;; post-command-hook called

(defun wg-unflag-window-config-changed ()
  "Set `wg-window-config-changed-p' to nil, exempting from
undoification those window-configuration changes caused by
entering the minibuffer."
  (setq wg-window-config-changed-p nil))

(defun wg-flag-just-exited-minibuffer ()
  "Set `wg-just-exited-minibuffer' on minibuffer exit."
  (setq wg-just-exited-minibuffer t))

(defun wg-flag-wconfig-change ()
  "Conditionally set `wg-window-config-changed-p' to t.
Added to `window-configuration-change-hook'."
  (when (and wg-flag-wconfig-changes
             (zerop (minibuffer-depth))
             (not wg-just-exited-minibuffer))
    (setq wg-window-config-changed-p t))
  (setq wg-just-exited-minibuffer nil))

(defun wg-update-working-config-before-command ()
  "Update the current workgroup's working config before
`wg-commands-that-alter-window-configs'. Added to
`pre-command-hook'."
  (when (gethash this-command wg-commands-that-alter-window-configs)
    (wg-update-workgroup-working-config)))

(defun wg-save-undo-after-wconfig-change ()
  "`wg-push-new-workgroup-working-config' when `wg-window-config-changed-p'
is non-nil.  Added to `post-command-hook'."
  (when (and wg-window-config-changed-p (zerop (minibuffer-depth)))
    (wg-awhen (wg-current-workgroup t)
      (wg-push-new-workgroup-working-config it (wg-make-wconfig))))
  (setq wg-window-config-changed-p nil))


;; `wg-commands-that-alter-window-configs' stuff

(defun wg-add-commands-that-modify-window-configs (&rest command-symbols)
  "Add command symbols to `wg-commands-that-alter-window-configs'."
  (dolist (cmdsym command-symbols command-symbols)
    (puthash cmdsym t wg-commands-that-alter-window-configs)))

(defun wg-remove-commands-that-modify-window-configs (&rest command-symbols)
  "Remove command symbols from `wg-commands-that-alter-window-configs'."
  (dolist (cmdsym command-symbols command-symbols)
    (remhash cmdsym wg-commands-that-alter-window-configs)))

(wg-add-commands-that-modify-window-configs
 'split-window
 'split-window-horizontally
 'split-window-vertically
 'delete-window
 'delete-other-windows
 'delete-other-windows-vertically
 'enlarge-window
 'enlarge-window-horizontally
 'shrink-window
 'shrink-window-horizontally
 'shrink-window-if-larger-than-buffer
 'switch-to-buffer
 'switch-to-buffer-other-window
 'switch-to-buffer-other-frame
 'ido-switch-buffer
 'ido-switch-buffer-other-window
 'ido-switch-buffer-other-frame
 'iswitchb-buffer
 'iswitchb-buffer-other-window
 'iswitchb-buffer-other-frame
 'balance-windows
 'balance-windows-area
 'help-with-tutorial
 'jump-to-register
 'erc-complete-word)



;;; workgroup construction and restoration

;; FIXME: working configs should include assigned-buffers ... this needs thinking through
;; FIXME: Many of these parameters don't need to be explicit
;; FIXME: Make workgroups immutable?
(defun wg-make-workgroup (&optional uid name wconfig assigned-buffers
                                    buffer-list-filters buffer-set-order)
  "Return a new workgroup from UID, NAME and WCONFIG."
  `((type                  .   workgroup)
    (uid                   .  ,uid)
    (name                  .  ,name)
    (wconfig               .  ,wconfig)
    (assigned-buffers      .  ,assigned-buffers)
    (buffer-list-filters   .  ,buffer-list-filters)
    (buffer-set-order      .  ,buffer-set-order)))

(defun wg-restore-workgroup-buffers-internal (workgroup)
  "Restore all the buffers assigned to WORKGROUP that can be restored."
  (wg-filter-map 'wg-restore-buffer (wg-workgroup-assigned-buffers workgroup)))

(defun wg-restore-workgroup (workgroup)
  "Restore WORKGROUP in `selected-frame'."
  (when wg-restore-assigned-buffers
    (wg-restore-workgroup-buffers-internal workgroup))
  (wg-restore-wconfig-undoably (wg-workgroup-working-config workgroup) t))



;;; workgroups list ops

(defun wg-delete-workgroup (workgroup)
  "Remove WORKGROUP from `wg-list'.
Also delete all references to it by `wg-workgroup-table',
`wg-current-workgroup' and `wg-previous-workgroup'."
  (dolist (frame (frame-list))
    (remhash (wg-uid workgroup) (wg-workgroup-table frame))
    (when (eq workgroup (wg-current-workgroup t frame))
      (wg-set-current-workgroup nil frame))
    (when (eq workgroup (wg-previous-workgroup t frame))
      (wg-set-previous-workgroup nil frame)))
  (setq wg-list (remove workgroup (wg-list)) wg-dirty t))

(defun wg-add-workgroup (new &optional pos)
  "Add WORKGROUP to `wg-list'.
If a workgroup with the same name exists, overwrite it."
  (wg-awhen (wg-get-workgroup-by-name (wg-workgroup-name new) t)
    (unless pos (setq pos (position it wg-list)))
    (wg-delete-workgroup it))
  (wg-set-uid new (wg-new-uid))
  (setq wg-list (wg-insert-elt new wg-list pos) wg-dirty t))

(defun wg-check-and-add-workgroup (workgroup)
  "Add WORKGROUP to `wg-list'.
Query to overwrite if a workgroup with the same name exists."
  (let ((name (wg-workgroup-name workgroup)))
    (when (wg-get-workgroup-by-name name t)
      (unless (or wg-no-confirm
                  (y-or-n-p (format "%S exists. Overwrite? " name)))
        (error "Cancelled"))))
  (wg-add-workgroup workgroup))

(defun wg-cyclic-offset-workgroup (workgroup n)
  "Offset WORKGROUP's position in `wg-list' by N."
  (wg-aif (wg-cyclic-offset-elt workgroup (wg-list) n)
      (setq wg-list it wg-dirty t)
    (error "Workgroup isn't present in `wg-list'.")))

(defun wg-list-swap (w1 w2)
  "Swap the positions of W1 and W2 in `wg-list'."
  (when (eq w1 w2) (error "Can't swap a workgroup with itself"))
  (wg-aif (wg-util-swap w1 w2 (wg-list))
      (setq wg-list it wg-dirty t)
    (error "Both workgroups aren't present in `wg-list'.")))



;;; mode-line

(defun wg-mode-line-string ()
  "Return the string to be displayed in the mode-line."
  (let ((wg (wg-current-workgroup t)))
    (cond (wg (wg-fontify " "
                (:div wg-mode-line-left-brace)
                (:mode (if wg-dirty "*" "-"))
                (:mode (if (wg-workgroup-dirty-p wg) "*" "-"))
                (:div wg-mode-line-divider)
                ;; (:mode (position wg (wg-list t)))
                ;; (:div wg-mode-line-divider)
                (:mode (wg-workgroup-name wg))
                (:div wg-mode-line-right-brace)))
          (t  (wg-fontify " "
                (:div wg-mode-line-left-brace)
                (:mode "No workgroups")
                (:div wg-mode-line-right-brace))))))

(defun wg-mode-line-add-display ()
  "Add Workgroups' mode-line format to `mode-line-format'."
  (unless (assq 'wg-mode-line-on mode-line-format)
    (let ((format `(wg-mode-line-on (:eval (wg-mode-line-string))))
          (pos (1+ (position 'mode-line-position mode-line-format))))
      (set-default 'mode-line-format
                   (wg-insert-elt format mode-line-format pos)))))

(defun wg-mode-line-remove-display ()
  "Remove Workgroups' mode-line format from `mode-line-format'."
  (wg-awhen (assq 'wg-mode-line-on mode-line-format)
    (set-default 'mode-line-format (remove it mode-line-format))
    (force-mode-line-update)))



;;; minibuffer reading

(defun wg-read-buffer (prompt &optional default require-match)
  "Workgroups' version of `read-buffer'."
  (wg-with-buffer-sets 'read
    (let ((prompt (format "%s %s" (wg-buffer-set-string) prompt)))
      (case (wg-current-read-buffer-mode)
        (ido (ido-read-buffer prompt default require-match))
        (iswitchb (iswitchb-read-buffer prompt default require-match))
        (fallback (let (read-buffer-function)
                    (read-buffer prompt default require-match)))))))

(defun wg-completing-read
  (prompt choices &optional pred rm ii history default)
  "Do a completing read.  The function called depends on what's on."
  (ecase (wg-current-read-buffer-mode)
    (ido
     (ido-completing-read prompt choices pred rm ii history default))
    (iswitchb
     (let* ((iswitchb-use-virtual-buffers nil)
            (iswitchb-make-buflist-hook
             (lambda () (setq iswitchb-temp-buflist choices))))
       (iswitchb-read-buffer prompt default rm)))
    (falback
     (completing-read prompt choices pred rm ii history default))))

(defun wg-read-workgroup (&optional noerror)
  "Read a workgroup with `wg-completing-read'."
  (wg-get-workgroup-by-name
   (wg-completing-read
    "Workgroup: " (wg-workgroup-names) nil nil nil nil
    (wg-awhen (wg-current-workgroup t) (wg-workgroup-name it)))
   noerror))

(defun wg-read-new-workgroup-name (&optional prompt)
  "Read a non-empty name string from the minibuffer."
  (wg-read-object
   (or prompt "Name: ")
   (lambda (obj) (and (stringp obj) (not (equal obj ""))))
   "Please enter a unique, non-empty name"))

(defun wg-read-workgroup-index ()
  "Prompt for the index of a workgroup."
  (let ((max (1- (length (wg-list)))))
    (wg-read-object
     (format "%s\n\nEnter [0-%d]: " (wg-disp) max)
     (lambda (obj) (and (integerp obj) (wg-within obj 0 max t)))
     (format "Please enter an integer [%d-%d]" 0 max)
     nil nil t)))



;;; messaging

(defun wg-msg (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS.
Also save the msg to `wg-last-message'."
  (setq wg-last-message (apply #'message format-string args)))

(defmacro wg-fontified-msg (&rest format)
  "`wg-fontify' FORMAT and call `wg-msg' on it."
  (declare (indent defun))
  `(wg-msg (wg-fontify ,@format)))


;;; fancy display

;; FIXME: cleanup and docstring
(defun wg-element-display (elt elt-string current-p previous-p)
  ""
  (let ((cld wg-display-current-workgroup-left-decor)
        (crd wg-display-current-workgroup-right-decor)
        (pld wg-display-previous-workgroup-left-decor)
        (prd wg-display-previous-workgroup-right-decor))
    (cond ((funcall current-p elt)
           (wg-fontify (:cur (concat cld elt-string crd))))
          ((funcall previous-p elt)
           (wg-fontify (:prev (concat pld elt-string prd))))
          (t (wg-fontify (:other elt-string))))))

;; FIXME: cleanup and docstring
(defun wg-workgroup-display (workgroup index)
  ""
  (wg-element-display
   workgroup
   (format "%d: %s" index (wg-workgroup-name workgroup))
   (lambda (wg) (wg-current-workgroup-p wg t))
   (lambda (wg) (wg-previous-workgroup-p wg t))))

;; FIXME: cleanup and docstring
(defun wg-display-internal (elt-fn list)
  ""
  (let ((i -1))
    (wg-fontify
      (:brace wg-display-left-brace)
      (wg-doconcat (elt list (wg-add-face :div wg-display-divider))
        (funcall elt-fn elt (incf i)))
      (:brace wg-display-right-brace))))



;;; command utils

(defun wg-arg (&optional reverse noerror)
  "Return a workgroup one way or another.
For use in interactive forms.  If `current-prefix-arg' is nil,
return the current workgroup.  Otherwise read a workgroup from
the minibuffer.  If REVERSE is non-nil, `current-prefix-arg's
begavior is reversed."
  (wg-list noerror)
  (if (if reverse (not current-prefix-arg) current-prefix-arg)
      (wg-read-workgroup noerror)
    (wg-current-workgroup noerror)))

(defun wg-add-to-kill-ring (config)
  "Add CONFIG to `wg-kill-ring'."
  (push config wg-kill-ring)
  (setq wg-kill-ring (wg-take wg-kill-ring wg-kill-ring-size)))

(defun wg-cyclic-nth-from-workgroup (&optional workgroup n)
  "Return the workgroup N places from WORKGROUP in `wg-list'."
  (wg-when-let ((wg (or workgroup (wg-current-workgroup t))))
    (wg-cyclic-nth-from-elt wg (wg-list) (or n 1))))

(defun wg-disp ()
  "Return the Workgroups list display string.
The string contains the names of all workgroups in `wg-list',
decorated with faces, dividers and strings identifying the
current and previous workgroups."
  (wg-display-internal 'wg-workgroup-display wg-list))



;;; commands

(defun wg-switch-to-workgroup (workgroup)
  "Switch to WORKGROUP."
  (interactive (list (wg-read-workgroup)))
  (let ((current (wg-current-workgroup t)))
    (when (eq workgroup current)
      (error "Already on: %s" (wg-workgroup-name current)))
    (wg-restore-workgroup workgroup)
    (wg-set-previous-workgroup current)
    (wg-set-current-workgroup workgroup))
  (run-hooks 'wg-switch-hook)
  (wg-fontified-msg
    (:cmd "Switched:  ")
    (wg-disp)))

(defun wg-create-workgroup (name)
  "Create and add a workgroup named NAME.
If there is no current workgroup, use the frame's current
window-config.  Otherwise, use a blank, one window window-config.
The reason for not using the current workgroup's window-config
for the new workgroup is that we don't want to give the
impression that the current workgroup's other parameters (buffer
list filters, assigned buffers, etc.) have been copied to the new
workgroup as well.  For that, use `wg-clone-workgroup'."
  (interactive (list (wg-read-new-workgroup-name)))
  (let ((workgroup (if (wg-current-workgroup t)
                       (wg-make-workgroup nil name (wg-make-blank-wconfig))
                     (wg-make-workgroup nil name (wg-make-wconfig)))))
    (wg-check-and-add-workgroup workgroup)
    (wg-switch-to-workgroup workgroup)
    (wg-fontified-msg
      (:cmd "Created: ")
      (:cur name) "  "
      (wg-disp))))

(defun wg-clone-workgroup (workgroup name)
  "Create and add a clone of WORKGROUP named NAME."
  (interactive (list (wg-arg) (wg-read-new-workgroup-name)))
  (let ((clone (wg-make-workgroup
                nil name (wg-workgroup-base-config workgroup))))
    (wg-check-and-add-workgroup clone)
    (wg-set-workgroup-working-config
     clone (wg-workgroup-working-config workgroup))
    (wg-switch-to-workgroup clone)
    (wg-fontified-msg
      (:cmd "Cloned: ")
      (:cur (wg-workgroup-name workgroup))
      (:msg " to ")
      (:cur name) "  "
      (wg-disp))))

(defun wg-kill-workgroup (workgroup)
  "Kill WORKGROUP, saving its working config to the kill ring."
  (interactive (list (wg-arg)))
  (let ((to (or (wg-previous-workgroup t)
                (wg-cyclic-nth-from-workgroup workgroup))))
    (wg-add-to-kill-ring (wg-workgroup-working-config workgroup))
    (wg-delete-workgroup workgroup)
    (if (eq workgroup to)
        (wg-restore-wconfig-undoably (wg-make-blank-wconfig))
      (wg-switch-to-workgroup to))
    (wg-fontified-msg
      (:cmd "Killed: ")
      (:cur (wg-workgroup-name workgroup)) "  "
      (wg-disp))))

(defun wg-kill-ring-save-base-config (workgroup)
  "Save WORKGROUP's base config to `wg-kill-ring'."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-workgroup-base-config workgroup))
  (wg-fontified-msg
    (:cmd "Saved: ")
    (:cur (wg-workgroup-name workgroup))
    (:cur "'s ")
    (:msg "base config to the kill ring")))

(defun wg-kill-ring-save-working-config (workgroup)
  "Save WORKGROUP's working config to `wg-kill-ring'."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-workgroup-working-config workgroup))
  (wg-fontified-msg
    (:cmd "Saved: ")
    (:cur (wg-workgroup-name workgroup))
    (:cur "'s ")
    (:msg "working config to the kill ring")))

(defun wg-yank-wconfig ()
  "Restore a wconfig from `wg-kill-ring'.
Successive yanks restore wconfigs sequentially from the kill
ring, starting at the front."
  (interactive)
  (unless wg-kill-ring (error "The kill-ring is empty"))
  (let ((pos (if (not (eq real-last-command 'wg-yank-wconfig)) 0
               (mod (1+ (or (get 'wg-yank-wconfig :position) 0))
                    (length wg-kill-ring)))))
    (put 'wg-yank-wconfig :position pos)
    (wg-restore-wconfig-undoably (nth pos wg-kill-ring))
    (wg-fontified-msg
      (:cmd "Yanked: ")
      (:msg pos) "  "
      (wg-disp))))

(defun wg-kill-workgroup-and-buffers (workgroup)
  "Kill WORKGROUP and the buffers in its working config."
  (interactive (list (wg-arg)))
  (let ((bufs (save-window-excursion
                (wg-restore-workgroup workgroup)
                (mapcar #'window-buffer (window-list)))))
    (wg-kill-workgroup workgroup)
    (mapc #'kill-buffer bufs)
    (wg-fontified-msg
      (:cmd "Killed: ")
      (:cur (wg-workgroup-name workgroup))
      (:msg " and its buffers ") "\n"
      (wg-disp))))

(defun wg-delete-other-workgroups (workgroup)
  "Delete all workgroups but WORKGROUP."
  (interactive (list (wg-arg)))
  (unless (or wg-no-confirm (y-or-n-p "Really delete all other workgroups? "))
    (error "Cancelled"))
  (let ((cur (wg-current-workgroup)))
    (mapc #'wg-delete-workgroup (remove workgroup (wg-list)))
    (unless (eq workgroup cur) (wg-switch-to-workgroup workgroup))
    (wg-fontified-msg
      (:cmd "Deleted: ")
      (:msg "All workgroups but ")
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-update-workgroup (workgroup)
  "Set the base config of WORKGROUP to its working config in `selected-frame'."
  (interactive (list (wg-arg)))
  (wg-set-workgroup-base-config
   workgroup (wg-workgroup-working-config workgroup))
  (wg-fontified-msg
    (:cmd "Updated: ")
    (:cur (wg-workgroup-name workgroup))))

(defun wg-update-all-workgroups ()
  "Update all workgroups' base configs.
Worgroups are updated with their working configs in the
`selected-frame'."
  (interactive)
  (mapc #'wg-update-workgroup (wg-list))
  (wg-fontified-msg
    (:cmd "Updated: ")
    (:msg "All")))

(defun wg-revert-workgroup (workgroup)
  "Set the working config of WORKGROUP to its base config in `selected-frame'."
  (interactive (list (wg-arg)))
  (if (wg-current-workgroup-p workgroup t)
      (wg-restore-wconfig-undoably (wg-workgroup-base-config workgroup))
    (wg-push-new-workgroup-working-config
     workgroup (wg-workgroup-base-config workgroup)))
  (wg-fontified-msg
    (:cmd "Reverted: ")
    (:cur (wg-workgroup-name workgroup))))

(defun wg-revert-all-workgroups ()
  "Revert all workgroups to their base configs."
  (interactive)
  (mapc #'wg-revert-workgroup (wg-list))
  (wg-fontified-msg
    (:cmd "Reverted: ")
    (:msg "All")))

(defun wg-switch-to-workgroup-at-index (n)
  "Switch to Nth workgroup in `wg-list'."
  (interactive (list (or current-prefix-arg (wg-read-workgroup-index))))
  (let ((wl (wg-list)))
    (wg-switch-to-workgroup
     (or (nth n wl) (error "There are only %d workgroups" (length wl))))))

;; Define wg-switch-to-workgroup-[0-9]:
(macrolet
    ((defi (n)
       `(defun ,(intern (format "wg-switch-to-workgroup-at-index-%d" n)) ()
          ,(format "Switch to the workgroup at index %d in the list." n)
          (interactive) (wg-switch-to-workgroup-at-index ,n))))
  (defi 0) (defi 1) (defi 2) (defi 3) (defi 4)
  (defi 5) (defi 6) (defi 7) (defi 8) (defi 9))

(defun wg-switch-left (&optional workgroup n)
  "Switch to the workgroup left of WORKGROUP in `wg-list'."
  (interactive (list (wg-arg nil t) current-prefix-arg))
  (wg-switch-to-workgroup
   (or (wg-cyclic-nth-from-workgroup workgroup (or n -1))
       (car (wg-list)))))

(defun wg-switch-right (&optional workgroup n)
  "Switch to the workgroup right of WORKGROUP in `wg-list'."
  (interactive (list (wg-arg nil t) current-prefix-arg))
  (wg-switch-to-workgroup
   (or (wg-cyclic-nth-from-workgroup workgroup n)
       (car (wg-list)))))

(defun wg-switch-left-other-frame (&optional n)
  "Like `wg-switch-left', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-cyclic-nth-from-frame (or n 1))
    (wg-switch-left)))

(defun wg-switch-right-other-frame (&optional n)
  "Like `wg-switch-right', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-cyclic-nth-from-frame (or n -1))
    (wg-switch-right)))

(defun wg-switch-to-previous-workgroup ()
  "Switch to the previous workgroup."
  (interactive)
  (wg-switch-to-workgroup (wg-previous-workgroup)))

(defun wg-swap-workgroups ()
  "Swap the previous and current workgroups."
  (interactive)
  (wg-list-swap (wg-current-workgroup) (wg-previous-workgroup))
  (wg-fontified-msg
    (:cmd "Swapped ")
    (wg-disp)))

(defun wg-offset-left (workgroup &optional n)
  "Offset WORKGROUP leftward in `wg-list' cyclically."
  (interactive (list (wg-arg) current-prefix-arg))
  (wg-cyclic-offset-workgroup workgroup (or n -1))
  (wg-fontified-msg
    (:cmd "Offset left: ")
    (wg-disp)))

(defun wg-offset-right (workgroup &optional n)
  "Offset WORKGROUP rightward in `wg-list' cyclically."
  (interactive (list (wg-arg) current-prefix-arg))
  (wg-cyclic-offset-workgroup workgroup (or n 1))
  (wg-fontified-msg
    (:cmd "Offset right: ")
    (wg-disp)))

(defun wg-rename-workgroup (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list (wg-arg) (wg-read-new-workgroup-name "New name: ")))
  (let ((oldname (wg-workgroup-name workgroup)))
    (wg-set-workgroup-name workgroup newname)
    (wg-fontified-msg
      (:cmd "Renamed: ")
      (:cur oldname)
      (:msg " to ")
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-reset (&optional force)
  "Reset workgroups.
Deletes all state saved in frame parameters, and nulls out
`wg-list', `wg-file' and `wg-kill-ring'."
  (interactive "P")
  (unless (or force wg-no-confirm (y-or-n-p "Are you sure? "))
    (error "Canceled"))
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'wg-workgroup-table nil)
    (set-frame-parameter frame 'wg-current-workgroup-uid nil)
    (set-frame-parameter frame 'wg-previous-workgroup-uid nil))
  (setq wg-list nil wg-file nil wg-dirty nil)
  (wg-fontified-msg
    (:cmd "Reset: ")
    (:msg "Workgroups")))


;; undo/redo commands

;; FIXME: Add wg-undo-once-on-all-workgroups

;; FIXME: use `wg-disp'
(defun wg-timeline-string (position length)
  "Return a timeline visualization string from POSITION and LENGTH."
  (wg-fontify
    (:div "-<{")
    (:other (wg-make-string (- length position) "-" "="))
    (:cur "O")
    (:other (wg-make-string (1+ position) "-" "="))
    (:div "}>-")))

(defun wg-undo-wconfig-change (&optional offset)
  "Undo a change to the current workgroup's window-configuration."
  (interactive "P")
  (wg-with-undo (wg-current-workgroup) (utab upos ulst)
    (let ((upos (+ upos (or offset 1))) (len (length ulst)) (msg ""))
      (if (>= upos len) (setq msg "  Completely undone!")
        (wg-restore-wconfig-undoably (nth upos ulst) t)
        (setf (gethash 'undo-pointer utab) upos))
      (wg-fontified-msg
        (:cmd "Undo: ")
        (wg-timeline-string (gethash 'undo-pointer utab) len)
        (:cur msg)))))

(defun wg-redo-wconfig-change (&optional offset)
  "Redo a change to the current workgroup's window-configuration."
  (interactive "P")
  (wg-with-undo (wg-current-workgroup) (utab upos ulst)
    (let ((upos (- upos (or offset 1))) (len (length ulst)) (msg ""))
      (if (< upos 0) (setq msg "  Completely redone!")
        (wg-restore-wconfig-undoably (nth upos ulst) t)
        (setf (gethash 'undo-pointer utab) upos))
      (wg-fontified-msg
        (:cmd "Redo: ")
        (wg-timeline-string (gethash 'undo-pointer utab) len)
        (:cur msg)))))


;; workgroup assigned buffers commands

(defun wg-add-buffer-to-workgroup (workgroup buffer)
  "Add BUFFER to WORKGROUP."
  (interactive (list (wg-arg) (wg-read-buffer "Buffer to add: ")))
  (let ((wgname (wg-workgroup-name workgroup))
        (bname (buffer-name buffer)))
    (if (wg-workgroup-add-buffer workgroup buffer t t)
        (message "Added %S to %s" bname wgname)
      (message "Updated %S in %s" bname wgname))))

(defun wg-remove-buffer-from-workgroup (workgroup buffer)
  "Remove BUFFER from WORKGROUP."
  (interactive (list (wg-arg) (wg-read-buffer "Buffer to remove: ")))
  (let ((wgname (wg-workgroup-name workgroup))
        (bname (buffer-name buffer)))
    (if (wg-workgroup-remove-buffer workgroup buffer t)
        (message "Removed %S from %s" bname wgname)
      (message "%S isn't assigned to %s" bname wgname))))

(defun wg-add-current-buffer-to-current-workgroup ()
  "Do what the name says."
  (interactive)
  (wg-add-buffer-to-workgroup
   (wg-current-workgroup) (current-buffer)))

(defun wg-remove-current-buffer-from-current-workgroup ()
  "Do what the name says."
  (interactive)
  (wg-remove-buffer-from-workgroup
   (wg-current-workgroup) (current-buffer)))

;; FIXME: Use wg-disp for the msg
(defun wg-restore-workgroup-buffers (workgroup)
  "Restore all the buffers assigned to WORKGROUP that can be restored."
  (interactive (list (wg-arg)))
  (let ((buffers (wg-restore-workgroup-buffers-internal workgroup))
        (div (wg-fontify (:div " | "))))
    (wg-fontified-msg
      (:cmd "Restored buffers (")
      (:msg (format "%s" (wg-workgroup-name workgroup)))
      (:cmd "):   ")
      (mapconcat 'buffer-name buffers div))))



;;; ibuffer commands

;; FIXME: add ibuffer commands here



;;; wg-switch-to-buffer

(defun wg-set-ido-buffer-list ()
  "Set `ido-temp-list' to the return value of `wg-make-buffer-set'.
Added to `ido-make-buffer-list-hook'."
  (when (and wg-current-buffer-set (boundp 'ido-temp-list))
    (setq ido-temp-list (wg-make-buffer-set ido-temp-list))))

(defun wg-set-iswitchb-buffer-list ()
  "Set `iswitchb-temp-buflist' to the return value of `wg-make-buffer-set'.
Added to `iswitchb-make-buflist-hook'."
  (when (and wg-current-buffer-set (boundp 'iswitchb-temp-buflist))
    (setq iswitchb-temp-buflist (wg-make-buffer-set iswitchb-temp-buflist))))


;; iswitchb compatibility

(defun wg-translate-ido-method-to-iswitchb-method (method)
  "Translate iswitchb's buffer-methods into ido buffer-methods."
  (case method
    (selected-window   'samewindow)
    (insert            'samewindow)
    (kill              'samewindow)
    (other-window      'otherwindow)
    (display           'display)
    (other-frame       'otherframe)
    (maybe-frame       'maybe-frame)
    (raise-frame       'always-frame)))

(defun wg-iswitchb-internal (method &optional prompt default init)
  "This provides the buffer switching interface to
`iswitchb-read-buffer' (analogous to ido's `ido-buffer-internal')
that iswitchb *should* have had.  A lot of this code is
duplicated from `iswitchb', so is similarly shitty."
  (let* ((iswitchb-method (wg-translate-ido-method-to-iswitchb-method method))
         (iswitchb-invalid-regexp nil)
         (prompt (or prompt "iswitch "))
         (buffer (iswitchb-read-buffer prompt default nil init)))
    (cond ((eq iswitchb-exit 'findfile)
           (call-interactively 'find-file))
          (iswitchb-invalid-regexp
           (message "Won't make invalid regexp named buffer"))
          ((not buffer) nil)
          ((not (get-buffer buffer))
           (iswitchb-possible-new-buffer buffer))
          ((eq method 'insert)
           (call-interactively 'insert-buffer buffer))
          ((eq method 'kill)
           (kill-buffer buffer))
          (t (iswitchb-visit-buffer buffer)))))


;; completion map

(defun wg-next-buffer-set (&optional num)
  "Trigger a forward cycling through the buffer-set order.
Call `backward-char' unless `point' is right after the prompt.
NUM is the number of buffer sets to cycle forward."
  (interactive "P")
  (if (> (point) (minibuffer-prompt-end)) (backward-char)
    (throw 'status (list 'next (or num 1)))))

(defun wg-previous-buffer-set (&optional num)
  "Trigger a backward cycling through the buffer-set order.
Call `backward-char' unless `point' is right after the prompt.
NUM is the number of states to cycle backward."
  (interactive "P")
  (if (> (point) (minibuffer-prompt-end)) (backward-char)
    (throw 'status (list 'prev (- (or num 1))))))

(defun wg-make-buffer-keyset-map ()
  "Return completion map with parent `minibuffer-local-completion-map'.
This works in ido, iswitchb and plain `switch-to-buffer' because
they all locally bind `minibuffer-local-completion-map'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-b")   'wg-next-buffer-set)
    (define-key map (kbd "C-S-b") 'wg-previous-buffer-set)
    (set-keymap-parent map minibuffer-local-completion-map)
    map))

(defun wg-setup-minibuffer-commands ()
  "Conditionally bind various commands in `minibuffer-local-map'.
Added to `minibuffer-setup-hook'."
  (when wg-current-buffer-set
    (use-local-map (wg-make-buffer-keyset-map))))


;; buffer-set commands

(defun wg-fallback-internal (method)
  "Completion filtration interface to `switch-to-buffer'."
  (let (read-buffer-function)
    (call-interactively
     (case method
       (selected-window  'switch-to-buffer)
       (other-window     'switch-to-buffer-other-window)
       (display          'display-buffer)
       (other-frame      'switch-to-buffer-other-frame)
       (kill             'kill-buffer)
       (insert           'insert-buffer)))))

(defun wg-buffer-internal (method &optional prompt default init)
  "Return a switch-to-buffer fn from `wg-current-read-buffer-mode'."
  (wg-with-buffer-sets method
    (let ((prompt (or prompt (wg-buffer-set-prompt))))
      (ecase (wg-current-read-buffer-mode)
        (ido       (ido-buffer-internal method nil prompt default init))
        (iswitchb  (wg-iswitchb-internal method prompt default init))
        (fallback  (wg-fallback-internal method))))))

;; FIXME: add pretty messaging?
(defun wg-switch-to-buffer ()
  "Workgroups' version of `switch-to-buffer'."
  (interactive)
  (wg-buffer-internal 'selected-window))

(defun wg-switch-to-buffer-other-window ()
  "Workgroups' version of `switch-to-buffer-other-window'."
  (interactive)
  (wg-buffer-internal 'other-window))

(defun wg-switch-to-buffer-other-frame ()
  "Workgroups' version of `switch-to-buffer-other-frame'."
  (interactive)
  (wg-buffer-internal 'other-frame))

(defun wg-kill-buffer ()
  "Workgroups' version of `kill-buffer'."
  (interactive)
  (wg-buffer-internal 'kill))

(defun wg-display-buffer ()
  "Workgroups' version of `display-buffer'."
  (interactive)
  (wg-buffer-internal 'display))

(defun wg-insert-buffer ()
  "Workgroups' version of `insert-buffer'."
  (interactive)
  (wg-buffer-internal 'insert))

(defun wg-determine-next-buffer-buffer-set-order (&optional workgroup)
  "Determine WORKGROUP's next-buffer-buffer-set-order."
  (let ((wg (wg-get-workgroup-flexibly workgroup)))
    (or (wg-workgroup-parameter wg 'next-buffer-buffer-set-order)
        (wg-workgroup-parameter wg 'default-buffer-set-order)
        wg-next-buffer-buffer-set-order
        (wg-default-buffer-set-order))))

(defun wg-buffer-display (buffer index)
  "Return BUFFER's fancy display string.  INDEX is ignored."
  (wg-element-display
   buffer (format "%S" (buffer-name buffer))
   (lambda (b) (eq b (current-buffer)))
   (lambda (b) nil)))

;; ;; FIXME: Ugly as shit.
;; ;; FIXME: And broken
;; (defun wg-next-buffer (&optional fallback prev)
;;   "Switch to the next buffer in Workgroups' filtered buffer list."
;;   (interactive "P")
;;   (if fallback (next-buffer)
;;     (let* ((wg (wg-current-workgroup))
;;            (set (car (wg-determine-next-buffer-buffer-set-order wg)))
;;            (wg-current-buffer-set set) ;; FIXME:
;;            (live-buffer-names (wg-make-buffer-set wg set))
;;            (buffer-names (cdr (wg-emacs-buffer-names)))
;;            (buffer-names (if prev (nreverse buffer-names) buffer-names))
;;            (next (wg-get1 (b buffer-names) (member b live-buffer-names))))
;;       (unless next (error "No buffer to switch to."))
;;       (let ((cur (current-buffer)))
;;         (switch-to-buffer next)
;;         (unless prev (bury-buffer cur)))
;;       (wg-fontified-msg
;;         (:cmd (wg-buffer-set-prompt wg))
;;         (wg-display-internal
;;          'wg-buffer-display
;;          (mapcar 'get-buffer (wg-make-buffer-set wg set)))))))

;; FIXME: Ugly as shit.
;; FIXME: And broken
(defun wg-next-buffer (&optional fallback prev)
  "Switch to the next buffer in Workgroups' filtered buffer list."
  (interactive "P")
  (if fallback (next-buffer)
    (let* ((wg (wg-current-workgroup))
           (buffer-set (car (wg-determine-next-buffer-buffer-set-order wg)))
           (wg-current-buffer-set buffer-set) ;; FIXME:
           (live-buffer-names (wg-make-buffer-set nil wg buffer-set))
           (buffer-names (cdr (wg-emacs-buffer-names)))
           (buffer-names (if prev (nreverse buffer-names) buffer-names))
           (next (wg-get1 (b buffer-names) (member b live-buffer-names))))
      (unless next (error "No buffer to switch to."))
      (let ((cur (current-buffer)))
        (switch-to-buffer next)
        (unless prev (bury-buffer cur)))
      (wg-fontified-msg
        (:cmd (wg-buffer-set-prompt wg))
        (wg-display-internal
         'wg-buffer-display
         (mapcar 'get-buffer (wg-make-buffer-set nil wg buffer-set)))))))

(defun wg-previous-buffer (&optional fallback)
  "Switch to the next buffer in Workgroups' filtered buffer list."
  (interactive "P")
  (wg-next-buffer nil t))



;;; file commands

(defun wg-save (file &optional force)
  "Save workgroups to FILE.
Called interactively with a prefix arg, or if `wg-file'
is nil, read a filename.  Otherwise use `wg-file'."
  (interactive
   (list (if (or current-prefix-arg (not (wg-file t)))
             (read-file-name "File: ") (wg-file))))
  (if (and (not force) (not (wg-dirty-p)))
      (message "(No workgroups need to be saved)")
    (wg-mark-everything-clean)
    (setq wg-file file)
    (wg-write-sexp-to-file
     `(,wg-persisted-workgroups-tag
       ,wg-persisted-workgroups-format-version
       ,@(wg-list))
     file)
    (wg-fontified-msg (:cmd "Wrote: ") (:file file))))

(defun wg-load (filename)
  "Load workgroups from FILENAME.
Called interactively with a prefix arg, and if `wg-file'
is non-nil, use `wg-file'. Otherwise read a filename."
  (interactive
   (list (if (and current-prefix-arg (wg-file t)) (wg-file)
           (read-file-name "File: "))))
  (setq filename (expand-file-name filename))
  (if (not (file-exists-p filename))
      (when (wg-query-for-save)
        (wg-reset t)
        (setq wg-file filename))
    (let ((contents (wg-read-sexp-from-file filename)))
      (unless (consp contents)
        (error "%S is not a workgroups file." filename))
      (wg-dbind (tag version . workgroups-list) contents
        (unless (eq tag wg-persisted-workgroups-tag)
          (error "%S is not a workgroups file." filename))
        (unless (and (stringp version)
                     (string= version wg-persisted-workgroups-format-version))
          (error "%S is incompatible with this version of Workgroups.  \
Please create a new workgroups file." filename))
        (wg-reset t)
        (setq wg-list workgroups-list wg-file filename)))
    (when wg-switch-on-load
      (wg-awhen (wg-list t)
        (wg-switch-to-workgroup (car it))))
    (wg-fontified-msg
      (:cmd "Loaded: ")
      (:file filename))))

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

(defun wg-dired (dir &optional switches)
  "Create a workgroup and open DIR in dired with SWITCHES."
  (interactive (list (read-directory-name "Dired: ") current-prefix-arg))
  (wg-create-workgroup dir)
  (dired dir switches))

(defun wg-update-all-workgroups-and-save ()
  "Call `wg-update-all-workgroups', the `wg-save'.
Keep in mind that workgroups will be updated with their
working-config in the current frame."
  (interactive)
  (wg-update-all-workgroups)
  (call-interactively 'wg-save))


;;; mode-line commands

(defun wg-toggle-mode-line ()
  "Toggle Workgroups' mode-line display."
  (interactive)
  (setq wg-mode-line-on (not wg-mode-line-on))
  (force-mode-line-update)
  (wg-fontified-msg
    (:cmd "mode-line: ")
    (:msg (if wg-mode-line-on "on" "off"))))


;;; morph commands

(defun wg-toggle-morph ()
  "Toggle `wg-morph', Workgroups' morphing animation."
  (interactive)
  (setq wg-morph-on (not wg-morph-on))
  (wg-fontified-msg
    (:cmd "Morph: ")
    (:msg (if wg-morph-on "on" "off"))))


;;; Window movement commands

(defun wg-backward-transpose-window (offset)
  "Move `selected-window' backward by OFFSET in its wlist."
  (interactive (list (or current-prefix-arg -1)))
  (wg-restore-wconfig-undoably (wg-wconfig-move-window offset)))

(defun wg-transpose-window (offset)
  "Move `selected-window' forward by OFFSET in its wlist."
  (interactive (list (or current-prefix-arg 1)))
  (wg-restore-wconfig-undoably (wg-wconfig-move-window offset)))

(defun wg-reverse-frame-horizontally ()
  "Reverse the order of all horizontally split wtrees."
  (interactive)
  (wg-restore-wconfig-undoably (wg-reverse-wconfig)))

(defun wg-reverse-frame-vertically ()
  "Reverse the order of all vertically split wtrees."
  (interactive)
  (wg-restore-wconfig-undoably (wg-reverse-wconfig t)))

(defun wg-reverse-frame-horizontally-and-vertically ()
  "Reverse the order of all wtrees."
  (interactive)
  (wg-restore-wconfig-undoably (wg-reverse-wconfig 'both)))


;;; echo commands

(defun wg-echo-current-workgroup ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (wg-fontified-msg
    (:cmd "Current: ")
    (:cur (wg-workgroup-name (wg-current-workgroup)))))

(defun wg-echo-all-workgroups ()
  "Display the names of all workgroups in the echo area."
  (interactive)
  (wg-fontified-msg
    (:cmd "Workgroups: ")
    (wg-disp)))

(defun wg-echo-time ()
  "Echo the current time.  Optionally includes `battery' info."
  (interactive)
  (wg-msg ;; Pass through format to escape the % in `battery'
   "%s" (wg-fontify
          (:cmd "Current time: ")
          (:msg (format-time-string wg-time-format))
          (when (and wg-display-battery (fboundp 'battery))
            (wg-fontify "\n" (:cmd "Battery: ") (:msg (battery)))))))

(defun wg-echo-version ()
  "Echo Workgroups' current version number."
  (interactive)
  (wg-fontified-msg
    (:cmd "Workgroups version: ")
    (:msg wg-version)))

(defun wg-echo-last-message ()
  "Echo the last message Workgroups sent to the echo area.
The string is passed through a format arg to escape %'s."
  (interactive)
  (message "%s" wg-last-message))


;;; help

(defvar wg-help
  '("\\[wg-switch-to-workgroup]"
    "Switch to a workgroup"
    "\\[wg-create-workgroup]"
    "Create a new workgroup and switch to it"
    "\\[wg-clone-workgroup]"
    "Create a clone of the current workgroug and switch to it"
    "\\[wg-kill-workgroup]"
    "Kill a workgroup"
    "\\[wg-kill-ring-save-base-config]"
    "Save the current workgroup's base config to the kill ring"
    "\\[wg-kill-ring-save-working-config]"
    "Save the current workgroup's working config to the kill ring"
    "\\[wg-yank-wconfig]"
    "Yank a wconfig from the kill ring into the current frame"
    "\\[wg-kill-workgroup-and-buffers]"
    "Kill a workgroup and all buffers visible in it"
    "\\[wg-delete-other-workgroups]"
    "Delete all but the specified workgroup"
    "\\[wg-update-workgroup]"
    "Update a workgroup's base config with its working config"
    "\\[wg-update-all-workgroups]"
    "Update all workgroups' base configs with their working configs"
    "\\[wg-revert-workgroup]"
    "Revert a workgroup's working config to its base config"
    "\\[wg-revert-all-workgroups]"
    "Revert all workgroups' working configs to their base configs"
    "\\[wg-switch-to-workgroup-at-index]"
    "Jump to a workgroup by its index in the workgroups list"
    "\\[wg-switch-to-workgroup-at-index-0]"
    "Switch to the workgroup at index 0"
    "\\[wg-switch-to-workgroup-at-index-1]"
    "Switch to the workgroup at index 1"
    "\\[wg-switch-to-workgroup-at-index-2]"
    "Switch to the workgroup at index 2"
    "\\[wg-switch-to-workgroup-at-index-3]"
    "Switch to the workgroup at index 3"
    "\\[wg-switch-to-workgroup-at-index-4]"
    "Switch to the workgroup at index 4"
    "\\[wg-switch-to-workgroup-at-index-5]"
    "Switch to the workgroup at index 5"
    "\\[wg-switch-to-workgroup-at-index-6]"
    "Switch to the workgroup at index 6"
    "\\[wg-switch-to-workgroup-at-index-7]"
    "Switch to the workgroup at index 7"
    "\\[wg-switch-to-workgroup-at-index-8]"
    "Switch to the workgroup at index 8"
    "\\[wg-switch-to-workgroup-at-index-9]"
    "Switch to the workgroup at index 9"
    "\\[wg-switch-left]"
    "Switch to the workgroup leftward cyclically in the workgroups list"
    "\\[wg-switch-right]"
    "Switch to the workgroup rightward cyclically in the workgroups list"
    "\\[wg-switch-left-other-frame]"
    "Like `wg-switch-left', but operates in the next frame"
    "\\[wg-switch-right-other-frame]"
    "Like `wg-switch-right', but operates in the next frame"
    "\\[wg-switch-to-previous-workgroup]"
    "Switch to the previously selected workgroup"
    "\\[wg-swap-workgroups]"
    "Swap the positions of the current and previous workgroups"
    "\\[wg-offset-left]"
    "Offset a workgroup's position leftward cyclically in the workgroups list"
    "\\[wg-offset-right]"
    "Offset a workgroup's position rightward cyclically in the workgroups list"
    "\\[wg-rename-workgroup]"
    "Rename a workgroup"
    "\\[wg-reset]"
    "Reset Workgroups' entire state."
    "\\[wg-save]"
    "Save the workgroup list to a file"
    "\\[wg-load]"
    "Load a workgroups list from a file"
    "\\[wg-find-file]"
    "Create a new blank workgroup and find a file in it"
    "\\[wg-find-file-read-only]"
    "Create a new blank workgroup and find a file read-only in it"
    ;; "\\[wg-get-by-buffer]"
    ;; "Switch to the workgroup and config in which the specified buffer is visible"
    "\\[wg-dired]"
    "Create a new blank workgroup and open a dired buffer in it"
    "\\[wg-backward-transpose-window]"
    "Move `selected-window' backward in its wlist"
    "\\[wg-transpose-window]"
    "Move `selected-window' forward in its wlist"
    "\\[wg-reverse-frame-horizontally]"
    "Reverse the order of all horizontall window lists."
    "\\[wg-reverse-frame-vertically]"
    "Reverse the order of all vertical window lists."
    "\\[wg-reverse-frame-horizontally-and-vertically]"
    "Reverse the order of all window lists."
    "\\[wg-toggle-mode-line]"
    "Toggle Workgroups' mode-line display"
    "\\[wg-toggle-morph]"
    "Toggle the morph animation on any wconfig change"
    "\\[wg-echo-current-workgroup]"
    "Display the name of the current workgroup in the echo area"
    "\\[wg-echo-all-workgroups]"
    "Display the names of all workgroups in the echo area"
    "\\[wg-echo-time]"
    "Display the current time in the echo area"
    "\\[wg-echo-version]"
    "Display the current version of Workgroups in the echo area"
    "\\[wg-echo-last-message]"
    "Display the last message Workgroups sent to the echo area in the echo area."
    "\\[wg-help]"
    "Show this help message")
  "List of commands and their help messages. Used by `wg-help'.")

(defun wg-help ()
  "Display Workgroups' help buffer."
  (interactive)
  (with-output-to-temp-buffer "*workroups help*"
    (princ  "Workgroups' keybindings:\n\n")
    (dolist (elt (wg-partition wg-help 2))
      (wg-dbind (cmd help-string) elt
        (princ (format "%15s   %s\n"
                       (substitute-command-keys cmd)
                       help-string))))))


;;; keymap


(defvar wg-map
  (wg-fill-keymap (make-sparse-keymap)

    ;; workgroup creation

    "C-c"        'wg-create-workgroup
    "c"          'wg-create-workgroup
    "C"          'wg-clone-workgroup


    ;; killing and yanking

    "C-k"        'wg-kill-workgroup
    "k"          'wg-kill-workgroup
    "M-W"        'wg-kill-ring-save-base-config
    "M-w"        'wg-kill-ring-save-working-config
    "C-y"        'wg-yank-wconfig
    "y"          'wg-yank-wconfig
    "M-k"        'wg-kill-workgroup-and-buffers
    "K"          'wg-delete-other-workgroups


    ;; updating and reverting

    "C-u"        'wg-update-workgroup
    "u"          'wg-update-workgroup
    "C-S-u"      'wg-update-all-workgroups
    "U"          'wg-update-all-workgroups
    "C-r"        'wg-revert-workgroup
    "r"          'wg-revert-workgroup
    "C-S-r"      'wg-revert-all-workgroups
    "R"          'wg-revert-all-workgroups


    ;; workgroup switching

    "C-'"        'wg-switch-to-workgroup
    "'"          'wg-switch-to-workgroup
    "C-v"        'wg-switch-to-workgroup
    "v"          'wg-switch-to-workgroup
    "C-j"        'wg-switch-to-workgroup-at-index
    "j"          'wg-switch-to-workgroup-at-index
    "0"          'wg-switch-to-workgroup-at-index-0
    "1"          'wg-switch-to-workgroup-at-index-1
    "2"          'wg-switch-to-workgroup-at-index-2
    "3"          'wg-switch-to-workgroup-at-index-3
    "4"          'wg-switch-to-workgroup-at-index-4
    "5"          'wg-switch-to-workgroup-at-index-5
    "6"          'wg-switch-to-workgroup-at-index-6
    "7"          'wg-switch-to-workgroup-at-index-7
    "8"          'wg-switch-to-workgroup-at-index-8
    "9"          'wg-switch-to-workgroup-at-index-9
    "C-p"        'wg-switch-left
    "p"          'wg-switch-left
    "C-n"        'wg-switch-right
    "n"          'wg-switch-right
    "M-p"        'wg-switch-left-other-frame
    "M-n"        'wg-switch-right-other-frame
    "C-a"        'wg-switch-to-previous-workgroup
    "a"          'wg-switch-to-previous-workgroup


    ;; undo/redo

    "<left>"     'wg-undo-wconfig-change
    "<right>"    'wg-redo-wconfig-change
    "["          'wg-undo-wconfig-change
    "]"          'wg-redo-wconfig-change


    ;; buffer-list

    "o"          'wg-add-current-buffer-to-current-workgroup
    "O"          'wg-remove-current-buffer-from-current-workgroup
    "M-o"        'wg-toggle-per-workgroup-buffer-lists
    "("          'wg-next-buffer
    ")"          'wg-previous-buffer


    ;; workgroup movement

    "C-x"        'wg-swap-workgroups
    "C-,"        'wg-offset-left
    "C-."        'wg-offset-right


    ;; file and buffer

    "C-s"        'wg-save
    "C-l"        'wg-load
    "C-f"        'wg-find-file
    "S-C-f"      'wg-find-file-read-only
    ;; "C-b"        'wg-get-by-buffer
    ;; "b"          'wg-get-by-buffer
    "C-b"        'wg-switch-to-buffer
    "b"          'wg-switch-to-buffer
    "d"          'wg-dired


    ;; window moving and frame reversal

    "<"          'wg-backward-transpose-window
    ">"          'wg-transpose-window
    "|"          'wg-reverse-frame-horizontally
    "-"          'wg-reverse-frame-vertically
    "+"          'wg-reverse-frame-horizontally-and-vertically


    ;; toggling

    "C-i"        'wg-toggle-mode-line
    "C-w"        'wg-toggle-morph


    ;; echoing

    "S-C-e"      'wg-echo-current-workgroup
    "E"          'wg-echo-current-workgroup
    "C-e"        'wg-echo-all-workgroups
    "e"          'wg-echo-all-workgroups
    "C-t"        'wg-echo-time
    "t"          'wg-echo-time
    "V"          'wg-echo-version
    "C-m"        'wg-echo-last-message
    "m"          'wg-echo-last-message


    ;; misc

    "A"          'wg-rename-workgroup
    "!"          'wg-reset
    "?"          'wg-help

    )
  "Workgroups' keymap.")


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
  (or (not (wg-dirty-p))
      (not (y-or-n-p "Save modified workgroups? "))
      (call-interactively 'wg-save)
      t))

(defun wg-emacs-exit-query ()
  "Conditionally call `wg-query-for-save'.
Call `wg-query-for-save' when `wg-query-for-save-on-emacs-exit'
is non-nil."
  (or (not wg-query-for-save-on-emacs-exit)
      (wg-query-for-save)))

(defun wg-workgroups-mode-exit-query ()
  "Conditionally call `wg-query-for-save'.
Call `wg-query-for-save' when
`wg-query-for-save-on-workgroups-mode-exit' is non-nil."
  (or (not wg-query-for-save-on-workgroups-mode-exit)
      (wg-query-for-save)))

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
  (cond
   (workgroups-mode
    (add-hook 'kill-emacs-query-functions 'wg-emacs-exit-query)
    (add-hook 'window-configuration-change-hook 'wg-flag-wconfig-change)
    (add-hook 'pre-command-hook 'wg-update-working-config-before-command)
    (add-hook 'post-command-hook 'wg-save-undo-after-wconfig-change)
    (add-hook 'minibuffer-setup-hook 'wg-unflag-window-config-changed)
    (add-hook 'minibuffer-setup-hook 'wg-setup-minibuffer-commands)
    (add-hook 'minibuffer-exit-hook 'wg-flag-just-exited-minibuffer)
    (add-hook 'ido-make-buffer-list-hook 'wg-set-ido-buffer-list)
    (add-hook 'iswitchb-make-buflist-hook 'wg-set-iswitchb-buffer-list)
    (add-hook 'kill-buffer-hook 'wg-auto-remove-buffer-hook)
    (add-hook 'find-file-hook 'wg-auto-add-buffer-hook)
    (wg-set-prefix-key)
    (wg-mode-line-add-display)
    (let ((map (make-sparse-keymap)))
      (define-key map
        [remap switch-to-buffer] 'wg-switch-to-buffer)
      (define-key map
        [remap switch-to-buffer-other-window] 'wg-switch-to-buffer-other-window)
      (define-key map
        [remap switch-to-buffer-other-frame] 'wg-switch-to-buffer-other-frame)
      (define-key map
        [remap next-buffer] 'wg-next-buffer)
      (define-key map
        [remap previous-buffer] 'wg-previous-buffer)
      (define-key map
        [remap kill-buffer] 'wg-kill-buffer)
      (define-key map
        [remap display-buffer] 'wg-display-buffer)
      (define-key map
        [remap insert-buffer] 'wg-insert-buffer)
      (if wg-minor-mode-map-entry
          (setcdr wg-minor-mode-map-entry map)
        (setq wg-minor-mode-map-entry (cons 'workgroups-mode map))
        (add-to-list 'minor-mode-map-alist wg-minor-mode-map-entry)))
    (run-hooks 'workgroups-mode-hook))
   (t
    (wg-workgroups-mode-exit-query)
    (remove-hook 'kill-emacs-query-functions 'wg-emacs-exit-query)
    (remove-hook 'window-configuration-change-hook 'wg-flag-wconfig-change)
    (remove-hook 'pre-command-hook 'wg-update-working-config-before-command)
    (remove-hook 'post-command-hook 'wg-save-undo-after-wconfig-change)
    (remove-hook 'minibuffer-setup-hook 'wg-unflag-window-config-changed)
    (remove-hook 'minibuffer-setup-hook 'wg-setup-minibuffer-commands)
    (remove-hook 'minibuffer-exit-hook 'wg-flag-just-exited-minibuffer)
    (remove-hook 'ido-make-buffer-list-hook 'wg-set-ido-buffer-list)
    (remove-hook 'iswitchb-make-buflist-hook 'wg-set-iswitchb-buffer-list)
    (remove-hook 'kill-buffer-hook 'wg-auto-remove-buffer-hook)
    (remove-hook 'find-file-hook 'wg-auto-add-buffer-hook)
    (wg-unset-prefix-key)
    (wg-mode-line-remove-display)
    (run-hooks 'workgroups-mode-exit-hook))))

(define-minor-mode workgroups-everywhere
  "Use Workgroups' buffer-sets for all buffer selection with `read-buffer'."
  :global t
  :group 'workgroups
  (wg-awhen (get 'workgroups-everywhere 'read-buffer-fn)
    (when (eq read-buffer-function 'wg-read-buffer)
      (setq read-buffer-function it))
    (put 'workgroups-everywhere 'read-buffer-fn nil))
  (when workgroups-everywhere
    (put 'workgroups-everywhere 'read-buffer-fn read-buffer-function)
    (setq read-buffer-function 'wg-read-buffer)))



;;; provide

(provide 'workgroups)



;;; workgroups.el ends here
