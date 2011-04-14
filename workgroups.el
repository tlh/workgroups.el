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
;; WGBUF always refers to a Workgroups buffer object.

;; FIXME: Make sure it compiles without warning in an "emacs -Q" session

;;; Code:

(require 'cl)

(eval-when-compile
  ;; This prevents "assignment to free variable" and "function not known to
  ;; exist" warnings on ido and iswitchb variables and functions.
  (require 'ido nil t)
  (require 'iswitchb nil t))



;;; consts

(defconst wg-version "1.0.0"
  "Current version of workgroups.")



;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- Emacs session manager"
  :group 'convenience
  :version wg-version)



;; keybinding customization

(defcustom wg-prefix-key (kbd "C-z")
  "Workgroups' prefix key."
  :type 'string
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (when (and (boundp 'workgroups-mode) workgroups-mode)
           (wg-set-prefix-key))
         val))


;; hooks

(defcustom workgroups-mode-hook nil
  "Hook run when `workgroups-mode' is turned on."
  :type 'hook
  :group 'workgroups)

(defcustom workgroups-mode-exit-hook nil
  "Hook run when `workgroups-mode' is turned off."
  :type 'hook
  :group 'workgroups)

(defcustom wg-switch-to-workgroup-hook nil
  "Hook run by `wg-switch-to-workgroup'."
  :type 'hook
  :group 'workgroups)


;; save and load customization

(defcustom wg-switch-to-first-workgroup-on-find-workgroups-file t
  "Non-nil means switch to the first workgroup in a file when
it's found with `wg-find-workgroups-file'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-emacs-exit-save-behavior 'query
  "The way save is handled when Emacs exits.
Possible values:
`save'     Call `wg-save-workgroups' when there are unsaved changes
`nosave'   Exit Emacs without saving changes
`query'    Query the user if there are unsaved changes"
  :type 'symbol
  :group 'workgroups)

(defcustom wg-workgroups-mode-exit-save-behavior 'query
  "The way save is handled when Workgroups exits.
Possible values:
`save'     Call `wg-save-workgroups' when there are unsaved changes
`nosave'   Exit `workgroups-mode' without saving changes
`query'    Query the user if there are unsaved changes"
  :type 'symbol
  :group 'workgroups)


;; minibuffer customization

(defcustom wg-no-confirm nil
  "Non-nil means don't request confirmation before various
destructive operations, like `wg-reset'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-warning-timeout 0.75
  "Seconds to `sit-for' after a warning message in the minibuffer."
  :type 'float
  :group 'workgroups)


;; workgroup restoration customization

(defcustom wg-default-buffer "*scratch*"
  "Buffer made visible a window when the window's actual buffer
can't be restored.  Also used when a blank workgroup is created."
  :type 'string
  :group 'workgroups)

(defcustom wg-barf-on-nonexistent-file nil
  "Non-nil means error when an attempt is made to restore a
buffer visiting a nonexistent file."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-associated-buffers t
  "Non-nil means restore all buffers associated with the
workgroup on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-frame-position nil
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

(defcustom wg-restore-minibuffer-scroll-window t
  "Non-nil means restore `minibuffer-scroll-window' on workgroup restore."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point t
  "Non-nil means restore `point' on workgroup restore.
This is included mainly so point restoration can be suspended
during `wg-morph' -- you probably want this non-nil."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-restore-point-max t
  "Controls point restoration when point is at `point-max'.
If `point' is at `point-max' when a wconfig is created, put
`point' back at `point-max' when the wconfig is restored, even if
`point-max' has increased in the meantime.  This is useful in,
say, irc buffers where `point-max' is constantly increasing."
  :type 'boolean
  :group 'workgroups)


;; wconfig undo/redo customization

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
a single hash-table lookup per command, there's no noticeable
slowdown.  Add commands to this table if they aren't already
present, and you want up-to-date undo info saved prior to their
invocation."
  :type 'hash-table
  :group 'workgroups)


;; wconfig kill-ring customization

(defcustom wg-wconfig-kill-ring-max 20
  "Maximum length of the `wg-wconfig-kill-ring'."
  :type 'integer
  :group 'workgroups)


;; buffer-list filtration customization

(defcustom wg-buffer-list-filtration-on t
  "Non-nil means Workgroups' buffer-list filtration feature is on.
Nil means ido and iswitchb behave normally.  See
`wg-buffer-list-filter-definitions' for more info."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-buffer-list-filter-definitions
  '((all "all" wg-buffer-list-filter-all)
    (associated "associated" wg-buffer-list-filter-associated)
    (unassociated "unassociated" wg-buffer-list-filter-unassociated)
    (fallback "fallback" nil))
  "List of buffer-list-filter definitions.
Each entry should be a list containing an identifier symbol, a
prompt string, and a function form that's funcall'd to produce
the filtered buffer-list.

The prompt string is displayed in the minibuffer-prompt when its
filter is active.

The function form should be either a function-symbol or a lambda, and
should take two arguments: a workgroup and an initial
buffer-list.  The workgroup is the current workgroup, and the
initial buffer-list is the return value of
`wg-interesting-buffers'.  You may not need these arguments in
your custom filters, but they're just necessary for some of the
built-in filters.

Default buffer-list-filters include:

`all'           All buffer names

`associated'    Only the names of those live buffers that have
                been associated with the current workgroup

`unassociated'  Only the names of those live buffers that are
                unassociated with the current workgroup

`fallback'      A special case used to fallback to the
                original (non-ido/iswitchb) Emacs command.
                `fallback' isn't actually a buffer-list-filter
                itself, but can be used in
                `wg-buffer-list-filter-order-alist' just the
                same.

A few example custom buffer-list filtration functions are
included, like `wg-buffer-list-filter-home-dir',
`wg-buffer-list-filter-irc' and `wg-buffer-list-filter-elisp'.
See their definitions for more info on how they're defined, and
the utilities they're built on.

Here's an example of how to add an `elisp' buffer-list-filter
definition to `wg-buffer-list-filter-definitions' using the
example function `wg-buffer-list-filter-elisp':

(add-to-list
 'wg-buffer-list-filter-definitions
 '(elisp \"elisp\" wg-buffer-list-filter-elisp))

After this form has been evaluated, `elisp' can be used wherever
other buffer-list-filter identifiers are used, like in
`wg-buffer-list-filter-order-alist'."
  :type 'list
  :group 'workgroups)

(defcustom wg-buffer-list-filter-order-alist
  '((default associated unassociated all fallback))
  "Alist defining the order in which filtered buffer-lists are presented.

The car of each entry should be the symbol of the original Emacs
command (not the ido or iswitchb remappings) -- i.e. one of
`switch-to-buffer', `switch-to-buffer-other-window',
`switch-to-buffer-other-frame', `kill-buffer', `next-buffer',
`previous-buffer', `display-buffer', `insert-buffer',
`read-buffer', or the special symbol `default', which defines the
buffer-list-filter order for all commands not present in this
alist.

The cdr of each entry should be a list of buffer-list-filter
identifiers defining the order in which filtered buffer-lists are
presented for the command.  See
`wg-buffer-list-filter-definitions'."
  :type 'alist
  :group 'workgroups)

(defcustom wg-center-rotate-buffer-list-display t
  "Non-nil means rotate the buffer list display so that the
current buffer is in the center of the list.  This makes it
easier to see the where `wg-previous-buffer' will take you."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-associate-buffer-on-switch-to-buffer t
  "Non-nil means automatically associate buffers with the current
workgroup on `switch-to-buffer'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-associate-buffer-on-set-window-buffer t
  "Non-nil means automatically associate buffers with the current
workgroup on `set-window-buffer'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-dissociate-buffer-on-kill-buffer t
  "Non-nil means automatically dissociate from the current
workgroup buffers killed with `kill-buffer'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-dissociate-buffer-on-bury-buffer t
  "Non-nil means automatically dissociate from the current
workgroup buffers buried with `bury-buffer'."
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


;; mode-line customization

(defcustom wg-mode-line-display-on t
  "Toggles Workgroups' mode-line display."
  :type 'boolean
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (force-mode-line-update)))

(defcustom wg-mode-line-use-faces nil
  "Non-nil means use faces in the mode-line display.
Provided because it can be difficult to read the fontified
mode-line under certain color settings."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-decor-alist
  '((left-brace          . "(")
    (right-brace         . ")")
    (divider             . ":")
    (manually-associated . "@")
    (auto-associated     . "~")
    (unassociated        . "-")
    (dirty               . "*")
    (clean               . "-"))
  "Alist mapping mode-line decoration symbols to mode-line
decoration strings."
  :type 'alist
  :group 'workgroups)


;; display customization

(defcustom wg-use-faces t
  "Non-nil means use faces in various displays."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-list-display-decor-alist
  '((left-brace     . "( ")
    (right-brace    . " )")
    (divider        . " | ")
    (current-left   . "-<{ ")
    (current-right  . " }>-")
    (previous-left  . "< ")
    (previous-right . " >"))
  "Alist mapping decoration symbols with decoration strings"
  :type 'alist
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

(defvar workgroups-mode nil "")

(defvar wg-workgroup-set nil
  "Current workgroup-set object.")

(defvar wg-minor-mode-map-entry nil
  "Workgroups' minor-mode-map entry.")

(defvar wg-wconfig-kill-ring nil
  "Ring of killed or kill-ring-saved wconfigs.")

(defvar wg-last-message nil
  "Holds the last message Workgroups sent to the echo area.")

(defvar wg-face-abbrevs nil
  "Assoc list mapping face abbreviations to face names.")


;; file and dirty flag vars

(defvar wg-visited-file-name nil
  "Current workgroups file.")

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


;; buffer-list-filter vars

(defvar wg-current-workgroup nil
  "Bound to the current workgroup in `wg-with-buffer-list-filters'.")

(defvar wg-current-buffer-method nil
  "Bound to the current buffer method in `wg-with-buffer-list-filters'.")

(defvar wg-current-buffer-list-filter-id nil
  "Bound to the current buffer-list-filter symbol in `wg-with-buffer-list-filters'.")

(defvar wg-minibuffer-contents nil
  "Holds the previous minibuffer contents for re-insertion when
the buffer-list-filter is cycled.")

(defvar wg-ido-translations
  `((switch-to-buffer              . selected-window)
    (switch-to-buffer-other-window . other-window)
    (switch-to-buffer-other-frame  . other-frame)
    (kill-buffer                   . kill)
    (insert-buffer                 . insert)
    (display-buffer                . display))
  "Alist mapping buffer methods to ido buffer methods.")

(defvar wg-iswitchb-translations
  `((switch-to-buffer              . samewindow)
    (switch-to-buffer-other-window . otherwindow)
    (switch-to-buffer-other-frame  . otherframe)
    (kill-buffer                   . kill)
    (insert-buffer                 . insert)
    (display-buffer                . display))
  "Alist mapping buffer methods to iswitchb buffer methods.")


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

(defun wg-filter-map (pred seq)
  "Call PRED on each element in SEQ, returning a list of the non-nil results."
  (let (acc)
    (mapc (lambda (elt) (wg-awhen (funcall pred elt) (push it acc))) seq)
    (nreverse acc)))

(defun wg-stable-union (test &rest lists)
  "Return the stable-ordered `union' of LISTS, testing membership with TEST."
  (let (result)
    (nreverse
     (dolist (list lists result)
       (dolist (elt list)
         (pushnew elt result :test test))))))

(defun wg-last1 (list)
  "Return the last element of LIST."
  (car (last list)))

(defun wg-leave (list n)
  "Return a list of the last N elts in LIST."
  (nthcdr (- (length list) n) list))

(defun wg-rnth (n list)
  "Return the Nth element of LIST, counting from the end."
  (nth (- (length list) n 1) list))

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

(defun wg-cyclic-nth-from-elt (elt list n &optional test)
  "Return the elt in LIST N places cyclically from ELT.
If ELT is not present is LIST, return nil."
  (wg-when-let ((pos (position elt list :test test)))
    (wg-cyclic-nth list (+ pos n))))

(defun wg-util-swap (elt1 elt2 list)
  "Return a copy of LIST with ELT1 and ELT2 swapped.
Return nil when ELT1 and ELT2 aren't both present."
  (wg-when-let ((p1 (position elt1 list))
                (p2 (position elt2 list)))
    (wg-move-elt elt1 (wg-move-elt elt2 list p1) p2)))

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
        (sit-for wg-warning-timeout)
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

(defun wg-make-string (times string &optional separator)
  "Like `make-string', but includes a separator."
  (mapconcat 'identity (make-list times string) (or separator "")))

(defun wg-interesting-buffers (&optional names)
  "Return a list of only the interesting buffers in `buffer-list'.
NAME non-nil means return their names instead."
  (wg-filter-map (lambda (buffer)
                   (let ((bname (buffer-name buffer)))
                     (unless (string-match "^ " bname)
                       (if names bname buffer))))
                 (buffer-list)))

(defun wg-add-or-remove-hooks (remove &rest pairs)
  "Add FUNCTION to or remove it from HOOK, depending on REMOVE."
  (dolist (pair (wg-partition pairs 2))
    (funcall (if remove 'remove-hook 'add-hook)
             (car pair) (cadr pair))))



;;; workgroups utils

(defun wg-type-of (obj)
  "Return the type of Workgroups object OBJ."
  (and (consp obj) (car obj)))

(defun wg-type-p (obj type)
  "Return t if OBJ is of type TYPE, nil otherwise."
  (eq type (wg-type-of obj)))

(defun wg-type-check (type obj &optional noerror)
  "Throw an error if OBJ is not of type TYPE."
  (or (wg-type-p obj type)
      (unless noerror
        (error "%s is not of type %s" obj type))))

(defun wg-get (obj param &optional default)
  "Return OBJ's value for PARAM.
If PARAM is not found, return DEFAULT which defaults to nil."
  (unless (wg-type-of obj) (error "%S is not a Workgroups object" obj))
  (wg-aget (cdr obj) param default))

(defun wg-put (obj param val)
  "Return a new object from OBJ with PARAM set to VAL."
  (cons (car obj) (wg-aput (cdr obj) param val)))

(defun wg-set (obj param val)
  "Set OBJ's value for PARAM to VAL."
  (setcdr obj (wg-aput (cdr obj) param val)))

(defun wg-copy (obj)
  "Return a copy of Workgroups object OBJ.
Only the the first cons of OBJ and the alist structure of the cdr
of OBJ are copied."
  (cons (car obj) (wg-acopy (cdr obj))))

(defmacro wg-bind-params (obj binds &rest body)
  "Bind OBJ's according to BINDS, and eval BODY. See `wg-abind'."
  (declare (indent 2))
  `(wg-abind (cdr ,obj) ,binds ,@body))

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
        (cond ((and (consp spec) (keywordp (car spec)))
               `(wg-add-face ,@spec))
              ((consp spec) spec)
              ((stringp spec) spec)
              (t `(format "%s" ,spec))))))

(defun wg-barf-on-active-minibuffer ()
  "Throw an error when the minibuffer is active."
  (when (active-minibuffer-window)
    (error "Workgroup operations aren't permitted while the \
minibuffer is active.")))



;;; type predicates

(defun wg-wgbuf-p (obj)
  "Return t if OBJ is a Workgroups buffer, nil otherwise."
  (wg-type-p obj 'buffer))

(defun wg-window-p (obj)
  "Return t if OBJ is a Workgroups window, nil otherwise."
  (wg-type-p obj 'window))

(defun wg-wtree-p (obj)
  "Return t if OBJ is a Workgroups window tree, nil otherwise."
  (wg-type-p obj 'wtree))

(defun wg-wconfig-p (obj)
  "Return t if OBJ is a Workgroups window config, nil otherwise."
  (wg-type-p obj 'wconfig))

(defun wg-workgroup-p (obj)
  "Return t if OBJ is a workgroup, nil otherwise."
  (wg-type-p obj 'workgroup))

(defun wg-workgroup-set-p (obj)
  "Return t if OBJ is a workgroup-set, nil otherwise."
  (wg-type-p obj 'workgroup-set))



;;; wconfig construction

(defun wg-window-point (ewin)
  "Return `point' or :max.  See `wg-restore-point-max'.
EWIN should be an Emacs window object."
  (let ((p (window-point ewin)))
    (if (and wg-restore-point-max (= p (point-max))) :max p)))

(defun wg-serialize-buffer (buffer)
  "Return a serialized buffer from BUFFER."
  (with-current-buffer buffer
    `(buffer
      (bname       .   ,(buffer-name))
      (fname       .   ,(buffer-file-name))
      (major-mode  .   ,major-mode)
      (point       .   ,(point))
      (mark        .   ,(mark))
      (markx       .   ,mark-active))))

(defun wg-serialize-window (window)
  "Return a serialized window from WINDOW."
  (let ((selwin (eq window (selected-window))))
    (with-selected-window window
      `(window
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
  "Return a new wtree node from DIR EDGES and WLIST."
  `(wtree
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

(defun wg-serialize-frame (&optional frame)
  "Return a serialized wconfig from FRAME or `selected-frame'."
  (let ((frame (or frame (selected-frame))))
    `(wconfig
      (left    .  ,(frame-parameter frame 'left))
      (top     .  ,(frame-parameter frame 'top))
      (width   .  ,(frame-parameter frame 'width))
      (height  .  ,(frame-parameter frame 'height))
      (sbars   .  ,(frame-parameter frame 'vertical-scroll-bars))
      (sbwid   .  ,(frame-parameter frame 'scroll-bar-width))
      (wtree   .  ,(wg-serialize-window-tree (window-tree frame))))))

(defun wg-make-blank-wconfig (&optional buffer)
  "Return a new blank wconfig.
BUFFER or `wg-default-buffer' is visible in the only window."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer wg-default-buffer))
    (wg-serialize-frame)))



;;; window config utils

(defun wg-dir (w) (wg-get w 'dir))

(defun wg-edges (w) (wg-get w 'edges))

(defun wg-wlist (w) (wg-get w 'wlist))

(defun wg-wtree (w) (wg-get w 'wtree))

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
  (wg-put w 'edges (list left top right bottom)))

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
  (wg-put w 'edges (mapcar* op (wg-get w 'edges) edges)))

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
    (wg-bind-params wtree (dir wlist)
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
              (if (cdr new) (wg-put wtree 'wlist new)
                (car new)))))))))

(defun wg-scale-wtree (wtree wscale hscale)
  "Return a copy of WTREE with its dimensions scaled by WSCALE and HSCALE.
All WTREE's subwins are scaled as well."
  (let ((scaled (wg-scale-wsize wtree wscale hscale)))
    (if (wg-window-p wtree) scaled
      (wg-put scaled 'wlist
              (wg-docar (sw (wg-wlist scaled))
                (wg-scale-wtree sw wscale hscale))))))

(defun wg-scale-wconfigs-wtree (wconfig new-width new-height)
  "Scale WCONFIG's wtree with NEW-WIDTH and NEW-HEIGHT.
Return a copy WCONFIG's wtree scaled with `wg-scale-wtree' by the
ratio or NEW-WIDTH to WCONFIG's width, and NEW-HEIGHT to
WCONFIG's height."
  (wg-normalize-wtree
   (wg-scale-wtree
    (wg-wtree wconfig)
    (/ (float new-width)  (wg-get wconfig 'width))
    (/ (float new-height) (wg-get wconfig 'height)))))

(defun wg-set-frame-size-and-scale-wtree (wconfig &optional frame)
  "Set FRAME's size to WCONFIG's, returning a possibly scaled wtree.
If the frame size was set correctly, return WCONFIG's wtree
unchanged.  If it wasn't, return a copy of WCONFIG's wtree scaled
with `wg-scale-wconfigs-wtree' to fit the frame as it exists."
  (let ((frame (or frame (selected-frame))))
    (wg-bind-params wconfig ((wcwidth width) (wcheight height))
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
                      (wg-bind-params w ((d1 dir) edges wlist)
                        (wg-make-wtree-node
                         d1 edges
                         (let ((wl2 (mapcar #'inner wlist)))
                           (if (or (eq dir 'both)
                                   (and (not dir) (not d1))
                                   (and dir d1))
                               (nreverse wl2) wl2)))))))
    (wg-normalize-wtree (inner w))))

(defun wg-wtree-move-window (wtree offset)
  "Offset `selected-window' OFFSET places in WTREE."
  (flet ((inner (w) (if (wg-window-p w) w
                      (wg-bind-params w ((d1 dir) edges wlist)
                        (wg-make-wtree-node
                         d1 edges
                         (wg-aif (wg-get1 (sw wlist) (wg-get sw 'selwin))
                             (wg-cyclic-offset-elt it wlist offset)
                           (mapcar #'inner wlist)))))))
    (wg-normalize-wtree (inner wtree))))

(defun wg-reverse-wconfig (wconfig &optional dir)
  "Reverse WCONFIG's wtree's wlist in direction DIR."
  (wg-put wconfig 'wtree (wg-reverse-wlist (wg-get wconfig 'wtree) dir)))

(defun wg-wconfig-move-window (wconfig offset)
  "Offset `selected-window' OFFSET places in WCONFIG."
  (wg-put wconfig 'wtree (wg-wtree-move-window (wg-get wconfig 'wtree) offset)))



;;; wconfig restoration

;; TODO: Add serialization and restoration of *Info* and *Help* buffers as a
;;       first stab at complex buffer restoration

(defun wg-restore-buffer (wgbuf)
  "Restore WGBUF and return it."
  (wg-bind-params wgbuf (fname bname (mm major-mode) point mark markx)
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
          ;; (goto-char point) ;; this causes problems
          (set-mark mark)
          (unless markx (deactivate-mark))
          buffer)))))

(defun wg-restore-window (wg-window)
  "Restore WG-WINDOW in `selected-window'."
  (wg-bind-params wg-window ((wgbuf buffer) point wstart hscroll
                             sbars fringes margins selwin mbswin)
    (let ((sw (selected-window))
          (buffer (wg-restore-buffer wgbuf)))
      (when selwin (setq wg-selected-window sw))
      (if (not buffer) (set-window-buffer sw wg-default-buffer)
        (with-current-buffer buffer
          (set-window-buffer sw buffer)
          (set-window-start sw wstart t)
          (set-window-point
           sw (cond ((not wg-restore-point) wstart)
                    ((eq point :max) (point-max))
                    (t point)))
          (when (>= wstart (point-max)) (recenter))
          (when (and wg-restore-minibuffer-scroll-window mbswin)
            (setq minibuffer-scroll-window sw))
          (when wg-restore-scroll-bars
            (wg-dbind (width cols vtype htype) sbars
              (set-window-scroll-bars sw width vtype htype)))
          (when wg-restore-fringes
            (apply #'set-window-fringes sw fringes))
          (when wg-restore-margins
            (set-window-margins sw (car margins) (cdr margins)))
          (set-window-hscroll sw hscroll))))))

(defun wg-restore-window-tree (wg-window-tree)
  "Restore WG-WINDOW-TREE in `selected-frame'."
  (flet ((inner (w) (if (wg-wtree-p w)
                        (wg-bind-params w ((d dir) wlist)
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
  (let ((frame (selected-frame))
        wg-associate-buffer-on-set-window-buffer
        wg-associate-buffer-on-switch-to-buffer
        wtree)
    (wg-bind-params wconfig (left top sbars sbwid)
      (setq wtree (wg-set-frame-size-and-scale-wtree wconfig frame))
      (when (and wg-restore-frame-position left top)
        (set-frame-position frame left top))
      (when (wg-morph-p)
        (wg-morph (wg-serialize-window-tree (window-tree))
                  wtree wg-morph-no-error))
      (wg-restore-window-tree wtree)
      (when wg-restore-scroll-bars
        (set-frame-parameter frame 'vertical-scroll-bars sbars)
        (set-frame-parameter frame 'scroll-bar-width sbwid)))))



;;; morph
;;
;; TODO: Add upward and left morphing.  And once it's been added, add selection
;;       of a random morph direction.
;;

(defun wg-morph-p ()
  "Return t when it's ok to morph, nil otherwise."
  (and after-init-time wg-morph-on))

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
                   (list (wg-make-wtree-node
                          d1 wg-null-edges (nthcdr (1- l2) wl1))))))))

(defun wg-morph-win->win (w1 w2 &optional swap)
  "Return a copy of W1 with its edges stepped once toward W2.
When SWAP is non-nil, return a copy of W2 instead."
  (wg-put (if swap w2 w1) 'edges (wg-morph-step-edges w1 w2)))

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
  (let ((wg-morph-hsteps (wg-morph-determine-steps
                          wg-morph-hsteps wg-morph-terminal-hsteps))
        (wg-morph-vsteps (wg-morph-determine-steps
                          wg-morph-vsteps wg-morph-terminal-vsteps))
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

(defun wg-visited-file-name (&optional noerror)
  "Return `wg-visited-file-name' or error."
  (or wg-visited-file-name
      (unless noerror
        (error "Workgroups isn't visiting a file"))))

(defun wg-workgroup-set ()
  "Return `wg-workgroup-set', creating it if necessary."
  (or wg-workgroup-set (setq wg-workgroup-set '(workgroup-set))))

(defun wg-workgroup-list (&optional noerror)
  "Return `wg-workgroup-set's `workgroup-list' param."
  (or (wg-get (wg-workgroup-set) 'workgroup-list)
      (unless noerror
        (error "No workgroups are defined."))))



;;; buffer object utils

(defun wg-bufobj-file-name (bufobj)
  "Return BUFOBJ's filename.
See `wg-get-bufobj' for allowable values for BUFOBJ."
  (cond ((bufferp bufobj) (buffer-file-name bufobj))
        ((wg-wgbuf-p bufobj) (wg-get bufobj 'fname))
        ((and (stringp bufobj) (get-buffer bufobj))
         (buffer-file-name (get-buffer bufobj)))
        (t (error "%S is not a valid buffer identifier." bufobj))))

(defun wg-bufobj-name (bufobj)
  "Return BUFOBJ's buffer name.
See `wg-get-bufobj' for allowable values for BUFOBJ."
  (cond ((bufferp bufobj) (buffer-name bufobj))
        ((wg-wgbuf-p bufobj) (wg-get bufobj 'bname))
        ((and (stringp bufobj) (get-buffer bufobj)) bufobj)
        (t (error "%S is not a valid buffer identifier." bufobj))))

(defun wg-get-wgbuf (bufobj)
  "Return a wgbuf from BUFOBJ.
See `wg-get-bufobj' for allowable values for BUFOBJ."
  (cond ((bufferp bufobj) (wg-serialize-buffer bufobj))
        ((wg-wgbuf-p bufobj) bufobj)
        ((and (stringp bufobj) (get-buffer bufobj))
         (wg-serialize-buffer (get-buffer bufobj)))
        (t (error "%S is not a valid buffer identifier." bufobj))))

(defun wg-bufobjs-equal (bufobj1 bufobj2)
  "Return t if BUFOBJ1 is \"equal\" to BUFOBJ2.
See `wg-get-bufobj' for allowable values for BUFOBJ1 and BUFOBJ2."
  (let ((fname (wg-bufobj-file-name bufobj1)))
    (cond (fname (equal fname (wg-bufobj-file-name bufobj2)))
          ((wg-bufobj-file-name bufobj2) nil)
          ((equal (wg-bufobj-name bufobj1) (wg-bufobj-name bufobj2))))))

(defun wg-get-bufobj (bufobj bufobj-list)
  "Return the bufobj in BUFOBJ-LIST corresponding to BUFOBJ, or nil.
BUFOBJ should be a Workgroups buffer, an Emacs buffer, or an
Emacs buffer-name string."
  (wg-get1 (bufobj2 bufobj-list) (wg-bufobjs-equal bufobj bufobj2)))




;;; workgroup utils

(defun wg-get-workgroup (key val &optional noerror)
  "Return the workgroup whose KEY equals VAL or error."
  (or (wg-get1 (wg (wg-workgroup-list noerror))
        (equal val (wg-get wg key)))
      (unless noerror
        (error "No workgroups defined with key %S and value %S" key val))))

(defun wg-get-workgroup-by-name (name &optional noerror)
  "Return the workgroup with name NAME."
  (or (wg-get-workgroup 'name name noerror)
      (unless noerror
        (error "There's no workgroup named %S" name))))

(defun wg-get-workgroup-by-uid (uid &optional noerror)
  "Return the workgroup with uid UID."
  (or (wg-get-workgroup 'uid uid noerror)
      (unless noerror
        (error "There's no workgroup with a UID of %S" uid))))

(defun wg-get-workgroup-flexibly (obj &optional noerror)
  "Return a workgroup from OBJ.
If OBJ is a workgroup, return it.
If OBJ is a string, return the workgroup with that name.
If OBJ is an integer, return the workgroup with that uid.
If OBJ is nil, return the current workgroup.
Otherwise error unless NOERROR.  All the above cases will also
error unless NOERROR if no workgroup is found."
  (cond ((wg-workgroup-p obj) obj)
        ((null obj) (wg-current-workgroup noerror))
        ((stringp obj) (wg-get-workgroup-by-name obj noerror))
        ((integerp obj) (wg-get-workgroup-by-uid obj noerror))
        (t (unless noerror
             (error "Unable to get workgroup from %S" obj)))))

(defun wg-copy-workgroup (workgroup)
  "Copy WORKGROUP.  This is just a wrapper for `wg-copy' that
resolves WORKGROUP with `wg-get-workgroup-flexibly'."
  (wg-copy (wg-get-workgroup-flexibly workgroup)))

(defun wg-workgroups-eq (workgroup1 workgroup2)
  "Return t when WORKGROUP1 and WORKGROUP2 are `eq'.
WORKGROUP1 and WORKGROUP2 can be any value accepted by
`wg-get-workgroup-flexibly'."
  (eq (wg-get-workgroup-flexibly workgroup1)
      (wg-get-workgroup-flexibly workgroup2)))



;;; workgroup parameters

(defun wg-mark-workgroup-dirty (workgroup)
  "Set WORKGROUP's dirty flag to t."
  (wg-set workgroup 'dirty t))

(defun wg-mark-workgroup-clean (workgroup)
  "Set WORKGROUP's dirty flag to nil."
  (wg-set workgroup 'dirty nil))

(defun wg-mark-everything-clean ()
  "Set `wg-dirty' and all workgroup dirty flags to nil."
  (mapc 'wg-mark-workgroup-clean (wg-workgroup-list t))
  (setq wg-dirty nil))

(defun wg-workgroup-parameter (workgroup parameter &optional default)
  "Return WORKGROUP's value for PARAMETER.
If PARAMETER is not found, return DEFAULT which defaults to nil.
WORKGROUP should be accepted by `wg-get-workgroup-flexibly'."
  (wg-get (wg-get-workgroup-flexibly workgroup) parameter default))

(defun wg-set-workgroup-parameter (workgroup parameter value)
  "Set WORKGROUP's value of PARAMETER to VALUE.
Sets `wg-dirty' to t unless `wg-nodirty' is bound to t.
WORKGROUP should be a value accepted by
`wg-get-workgroup-flexibly'.  Return VALUE."
  (let ((wg (wg-get-workgroup-flexibly workgroup)))
    (unless wg-nodirty
      (wg-mark-workgroup-dirty wg)
      (setq wg-dirty t))
    (wg-set wg parameter value)
    value))

(defun wg-remove-workgroup-parameter (workgroup parameter)
  "Remove parameter PARAMETER from WORKGROUP destructively."
  (let ((alist (cdr workgroup)))
    (setcdr workgroup (remove (assq parameter alist) alist))
    workgroup))

(defun wg-workgroup-dirty-p (workgroup)
  "Return the value of WORKGROUP's dirty flag."
  (wg-workgroup-parameter workgroup 'dirty))

(defun wg-dirty-p ()
  "Return t when `wg-dirty' or any workgroup dirty parameter all is non-nil."
  (or wg-dirty (some 'wg-workgroup-dirty-p (wg-workgroup-list t))))



;; workgroup uid

(defun wg-workgroup-uid (workgroup)
  "Return WORKGROUP's uid."
  (wg-workgroup-parameter workgroup 'uid))

(defun wg-set-workgroup-uid (workgroup uid)
  "Set the uid of WORKGROUP to UID."
  (wg-set-workgroup-parameter workgroup 'uid uid))

(defun wg-workgroup-uids (&optional noerror)
  "Return a list of workgroups uids."
  (mapcar 'wg-workgroup-uid (wg-workgroup-list noerror)))

(defun wg-new-workgroup-uid ()
  "Return a uid greater than any in `wg-workgroup-list'."
  (let ((uids (wg-workgroup-uids t)) (new -1))
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
  (mapcar 'wg-workgroup-name (wg-workgroup-list noerror)))


;; workgroup associated buffers

(defun wg-workgroup-associated-buffers (workgroup)
  "Return the associated-buffers of WORKGROUP."
  (wg-workgroup-parameter workgroup 'associated-buffers))

(defun wg-set-workgroup-associated-buffers (workgroup wgbufs)
  "Set WORKGROUP's `associated-buffers' to WGBUFS."
  (wg-set-workgroup-parameter workgroup 'associated-buffers wgbufs))

(defun wg-workgroup-get-wgbuf (workgroup bufobj &optional noerror)
  "Return the wgbuf corresponding to BUFOBJ in WORKGROUP's
associated-buffers list, or error unless NOERROR."
  (or (wg-get-bufobj bufobj (wg-workgroup-associated-buffers workgroup))
      (unless noerror
        (error "%S is not associated with %S"
               (wg-bufobj-name bufobj)
               (wg-workgroup-name workgroup)))))

(defun wg-workgroup-buffer-association-type (workgroup bufobj)
  "Return BUFOBJ's association-type in WORKGROUP, or nil if unassociated."
  (wg-awhen (wg-workgroup-get-wgbuf workgroup bufobj t)
    (wg-get it 'association-type)))

(defun wg-wgbuf-auto-associated-p (wgbuf)
  "Return t if WGBUF's `association-type' is `auto'."
  (eq (wg-get wgbuf 'association-type) 'auto))

(defun wg-wgbuf-manually-associated-p (wgbuf)
  "Return t if WGBUF's `association-type' is `manual'."
  (eq (wg-get wgbuf 'association-type) 'manual))

(defun wg-buffer-auto-associated-p (workgroup bufobj)
  "Return t if BUFOBJ was automatically associated with WORKGROUP."
  (wg-awhen (wg-workgroup-get-wgbuf workgroup bufobj t)
    (wg-wgbuf-auto-associated-p it)))

(defun wg-buffer-manually-associated-p (workgroup bufobj)
  "Return t if BUFOBJ was manually associated with WORKGROUP."
  (wg-awhen (wg-workgroup-get-wgbuf workgroup bufobj t)
    (wg-wgbuf-manually-associated-p it)))

(defun wg-workgroup-associate-buffer (workgroup bufobj type &optional noerror)
  "Add BUFOBJ to WORKGROUP.
If TYPE is non-nil, set BUFOBJ's `association-type' parameter to it.
See `wg-get-bufobj' for allowable values for BUFOBJ."
  (if (wg-workgroup-get-wgbuf workgroup bufobj t)
      (unless noerror
        (error "%S is already associated with %S"
               (wg-bufobj-name bufobj)
               (wg-workgroup-name workgroup)))
    (let ((wgbuf (wg-get-wgbuf bufobj)))
      (wg-set wgbuf 'association-type type)
      (wg-set-workgroup-associated-buffers
       workgroup (cons wgbuf (wg-workgroup-associated-buffers workgroup)))
      wgbuf)))

(defun wg-workgroup-dissociate-buffer (workgroup bufobj &optional noerror)
  "Dissociate BUFOBJ from WORKGROUP.
See `wg-get-bufobj' for allowable values for BUFOBJ."
  (wg-awhen (wg-workgroup-get-wgbuf workgroup bufobj noerror)
    (wg-set-workgroup-associated-buffers
     workgroup (remove it (wg-workgroup-associated-buffers workgroup)))
    it))

(defun wg-workgroup-update-buffer (workgroup bufobj type &optional noerror)
  "Update BUFOBJ in WORKGROUP.
If TYPE is non-nil, set BUFOBJ's `association-type' parameter to it.
See `wg-get-bufobj' for allowable values for BUFOBJ."
  (when (wg-workgroup-dissociate-buffer workgroup bufobj noerror)
    (wg-workgroup-associate-buffer workgroup bufobj type)))

(defun wg-workgroup-update-or-associate-buffer (workgroup bufobj type)
  "Update BUFOBJ in or add it to WORKGROUP.
If TYPE is non-nil, set BUFOBJ's `association-type' parameter to it.
See `wg-get-bufobj' for allowable values for BUFOBJ."
  (or (wg-workgroup-update-buffer workgroup bufobj type t)
      (progn (wg-workgroup-associate-buffer workgroup bufobj type t) nil)))

(defun wg-workgroup-live-buffers (workgroup)
  "Return a list of WORKGROUP's live associated buffers."
  (let ((assoc-bufs (wg-workgroup-associated-buffers workgroup)))
    (wg-filter-map (lambda (bname) (when (wg-get-bufobj bname assoc-bufs) bname))
                   (wg-interesting-buffers t))))

(defun wg-workgroup-cycle-buffer-association-type (workgroup bufobj)
  "Cycle the BUFOBJ's association type in WORKGROUP.
If it's not associated with the workgroup, mark it as manually associated.
If it's auto-associated with the workgroup, remove it from the workgroup.
If it's manually associated with the workgroup, mark it as auto-associated."
  (case (wg-workgroup-buffer-association-type workgroup bufobj)
    (manual (wg-workgroup-update-buffer workgroup bufobj 'auto))
    (auto (wg-workgroup-dissociate-buffer workgroup bufobj))
    (otherwise (wg-workgroup-associate-buffer workgroup bufobj 'manual))))

(defun wg-workgroup-purge-auto-associated-buffers (workgroup)
  "Remove from WORKGROUP all wgbufs with `association-type' `auto'."
  (wg-set-workgroup-associated-buffers
   workgroup (remove-if 'wg-wgbuf-auto-associated-p
                        (wg-workgroup-associated-buffers workgroup))))

(defun wg-auto-dissociate-buffer-hook ()
  "`kill-buffer-hook' that automatically removes buffers from workgroups."
  (when wg-dissociate-buffer-on-kill-buffer
    (wg-awhen (wg-current-workgroup t)
      (wg-workgroup-dissociate-buffer it (current-buffer) t))))


;; filtered buffer-list construction

(defun wg-get-buffer-list-filter-id-flexibly (blf-id)
  "Return a buffer-list-filter-id one way or another."
  (or blf-id wg-current-buffer-list-filter-id 'all))

(defun wg-get-buffer-list-filter-val (id key &optional noerror)
  "Return ID's KEY's value in `wg-buffer-list-filter-definitions'.
Lots of possible errors here because
`wg-buffer-list-filter-definitions' can be modified by the user."
  (let ((slot-num (case key (symbol 0) (indicator 1) (constructor 2))))
    (if (not slot-num)
        (unless noerror
          (error "`%S' is not a valid buffer-list-filter definition slot" key))
      (let* ((id (wg-get-buffer-list-filter-id-flexibly id))
             (entry (assq id wg-buffer-list-filter-definitions)))
        (if (not entry)
            (unless noerror
              (error "`%S' is an undefined buffer-list-filter" id))
          (or (nth slot-num entry)
              (unless noerror
                (error "Slot `%S' is undefined in `%S's definition"
                       key id))))))))

(defun wg-filtered-buffer-list (&optional workgroup bfl-id initial)
  "Return the final list of buffer name completions."
  (let ((bfl-id (wg-get-buffer-list-filter-id-flexibly bfl-id)))
    (condition-case err
        (funcall (wg-get-buffer-list-filter-val bfl-id 'constructor)
                 (wg-get-workgroup-flexibly workgroup)
                 (or initial (wg-interesting-buffers t)))
      (error (error "Error in buffer-list-filter `%S's constructor: %S"
                    bfl-id err)))))


;; buffer-list filters

(defun wg-buffer-list-filter-all (workgroup initial)
  "Return all buffers in INITIAL."
  initial)

(defun wg-buffer-list-filter-associated (workgroup initial)
  "Return only those buffers associated with WORKGROUP."
  (wg-workgroup-live-buffers workgroup))

(defun wg-buffer-list-filter-unassociated (workgroup initial)
  "Return only those buffer unassociated with WORKGROUP."
  (let ((buflist (wg-workgroup-live-buffers workgroup)))
    (remove-if (lambda (buf) (member buf buflist)) initial)))


;; buffer-list filtration utils

(defun wg-filter-buffer-list (pred &optional buffer-list)
  "Return only those buffers in BUFFER-LIST on which PRED returns non-nil."
  (remove-if-not pred (or buffer-list (wg-interesting-buffers t))))

(defun wg-filter-buffer-list-by-regexp (regexp &optional buffer-list)
  "Return only those buffers in BUFFER-LIST with names matching REGEXP."
  (wg-filter-buffer-list (lambda (bname) (string-match regexp bname)) buffer-list))

(defun wg-filter-buffer-list-by-root-dir (root-dir &optional buffer-list)
  "Return only those buffers in BUFFER-LIST visiting files undo ROOT-DIR."
  (wg-filter-buffer-list
   (lambda (bname)
     (wg-awhen (buffer-file-name (get-buffer bname))
       (wg-file-under-root-path-p (expand-file-name root-dir) it)))
   buffer-list))

(defun wg-filter-buffer-list-by-major-mode (mode-symbol &optional buffer-list)
  "Return only those buffers in BUFFER-LIST in major-mode MODE-SYMBOL."
  (wg-filter-buffer-list
   (lambda (bname)
     (with-current-buffer (get-buffer bname)
       (eq mode-symbol major-mode)))
   buffer-list))


;; Example custom buffer-list-filters

(defun wg-buffer-list-filter-irc (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST with names starting in \"#\"."
  (wg-filter-buffer-list-by-regexp "^#" buffer-list))

(defun wg-buffer-list-filter-home-dir (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST visiting files under ~/."
  (wg-filter-buffer-list-by-root-dir "~/" buffer-list))

(defun wg-buffer-list-filter-elisp (workgroup buffer-list)
  "Return only those buffers in BUFFER-LIST in `emacs-lisp-mode'."
  (wg-filter-buffer-list-by-major-mode 'emacs-lisp-mode buffer-list))

;; TODO: look into `apply-partially'
(defun wg-buffer-list-filter-home-dir->elisp (workgroup buffer-list)
  "Example of chaining buffer-list filters.
This returns all buffers under \"~/\" that are also in `emacs-lisp-mode'."
  (wg-buffer-list-filter-elisp
   nil (wg-buffer-list-filter-home-dir nil buffer-list)))


;; buffer-list-filter context

(defun wg-buffer-list-filter-order (workgroup method)
  "Return WORKGROUP's buffer-list-filter order for METHOD, or a default."
  (let ((bso (wg-workgroup-parameter workgroup 'buffer-list-filter-order-alist)))
    (or (wg-aget bso  method)
        (wg-aget bso 'default)
        (wg-aget wg-buffer-list-filter-order-alist  method)
        (wg-aget wg-buffer-list-filter-order-alist 'default))))

(defmacro wg-prior-mapping (mode command)
  "Return whatever COMMAND would call if MODE wasn't on."
  `(or (let (,mode) (command-remapping ,command)) ,command))

(defun wg-filter-buffer-list-p ()
  "Return the current workgroup when buffer-list-filters are on."
  (and workgroups-mode wg-buffer-list-filtration-on (wg-current-workgroup t)))

(defmacro wg-with-buffer-list-filters (method &rest body)
  "Establish buffer-list-filter context for buffer-method METHOD, and eval BODY.
Binds `wg-current-workgroup', `wg-current-buffer-method' and
`wg-current-buffer-list-filter-id' in BODY."
  (declare (indent 1))
  (wg-with-gensyms (order status)
    `(let* ((wg-current-workgroup (wg-current-workgroup t))
            (wg-current-buffer-method ,method)
            (,order (wg-buffer-list-filter-order wg-current-workgroup ,method))
            (wg-minibuffer-contents nil))
       (catch 'result
         (while 'your-mom
           (let* ((wg-current-buffer-list-filter-id (car ,order))
                  (,status (catch 'action (list 'done (progn ,@body)))))
             (case (car ,status)
               (done (throw 'result (cadr ,status)))
               (next (setq ,order (wg-rotate-list ,order 1))
                     (setq wg-minibuffer-contents (cadr ,status)))
               (prev (setq ,order (wg-rotate-list ,order -1))
                     (setq wg-minibuffer-contents (cadr ,status))))))))))



;;; current and previous workgroup ops

(defun wg-current-workgroup (&optional noerror frame)
  "Return the current workgroup."
  (or wg-current-workgroup
      (let ((uid (frame-parameter frame 'wg-current-workgroup-uid)))
        (if uid (wg-get-workgroup-by-uid uid noerror)
          (unless noerror
            (error "There's no current workgroup in this frame."))))))

(defun wg-previous-workgroup (&optional noerror frame)
  "Return the previous workgroup."
  (let ((uid (frame-parameter frame 'wg-previous-workgroup-uid)))
    (if uid (wg-get-workgroup-by-uid uid noerror)
      (unless noerror
        (error "There's no previous workgroup in this frame.")))))

(defun wg-set-current-workgroup (workgroup &optional frame)
  "Set the current workgroup to WORKGROUP."
  (set-frame-parameter
   frame 'wg-current-workgroup-uid
   (when workgroup (wg-workgroup-uid (wg-get-workgroup-flexibly workgroup)))))

(defun wg-set-previous-workgroup (workgroup &optional frame)
  "Set the previous workgroup to WORKGROUP."
  (set-frame-parameter
   frame 'wg-previous-workgroup-uid
   (when workgroup (wg-workgroup-uid (wg-get-workgroup-flexibly workgroup)))))

;; (defun wg-current-workgroup-p (workgroup &optional noerror)
;;   "Return t when WORKGROUP is the current workgroup, nil otherwise."
;;   (wg-awhen (wg-current-workgroup noerror)
;;     (eq it workgroup)))

(defun wg-current-workgroup-p (workgroup &optional noerror frame)
  "Return t when WORKGROUP is the current workgroup, nil otherwise."
  (wg-awhen (wg-current-workgroup noerror frame)
    (wg-workgroups-eq workgroup it)))

;; (defun wg-previous-workgroup-p (workgroup &optional noerror)
;;   "Return t when WORKGROUP is the previous workgroup, nil otherwise."
;;   (wg-awhen (wg-previous-workgroup noerror)
;;     (eq it workgroup)))

(defun wg-previous-workgroup-p (workgroup &optional noerror frame)
  "Return t when WORKGROUP is the previous workgroup, nil otherwise."
  (wg-awhen (wg-previous-workgroup noerror frame)
    (wg-workgroups-eq workgroup it)))

(defmacro wg-with-current-workgroup (workgroup &rest body)
  "Execute forms in BODY with WORKGROUP temporarily current.
WORKGROUP should be any workgroup identifier accepted by
`wg-get-workgroup-flexibly'.  The value returned is the last form
in BODY."
  (declare (indent 1))
  `(let ((wg-current-workgroup (wg-get-workgroup-flexibly ,workgroup)))
     ,@body))



;;; workgroup base wconfig

(defun wg-set-workgroup-base-wconfig (workgroup base-wconfig)
  "Set the base-wconfig of WORKGROUP to BASE-WCONFIG."
  (wg-set-workgroup-parameter workgroup 'base-wconfig base-wconfig))

(defun wg-workgroup-base-wconfig (workgroup)
  "Return the base-wconfig of WORKGROUP."
  (wg-workgroup-parameter workgroup 'base-wconfig))



;;; workgroup working-wconfig and wconfig undo/redo

(defun wg-workgroup-table (&optional frame)
  "Return FRAME's workgroup table, creating it first if necessary."
  (or (frame-parameter frame 'wg-workgroup-table)
      (let ((wt (make-hash-table)))
        (set-frame-parameter frame 'wg-workgroup-table wt)
        wt)))

(defun wg-workgroup-state-table (workgroup &optional frame)
  "Return FRAME's WORKGROUP's state table."
  (let ((uid (wg-workgroup-uid workgroup)) (wt (wg-workgroup-table frame)))
    (or (gethash uid wt)
        (let ((wst (make-hash-table))
              (working-wconfig
               (or (wg-workgroup-parameter workgroup 'working-wconfig)
                   (wg-workgroup-base-wconfig workgroup))))
          (puthash 'undo-pointer 0 wst)
          (puthash 'undo-list (list working-wconfig) wst)
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

(defun wg-set-workgroup-working-wconfig (workgroup wconfig)
  "Set the working-wconfig of WORKGROUP to WCONFIG."
  (wg-set-workgroup-parameter workgroup 'working-wconfig wconfig)
  (wg-with-undo workgroup (state-table undo-pointer undo-list)
    (setcar (nthcdr undo-pointer undo-list) wconfig)))

(defun wg-update-workgroup-working-wconfig (workgroup)
  "Update WORKGROUP's working-wconfig with `wg-serialize-frame'."
  (when (wg-current-workgroup-p workgroup t)
    (wg-set-workgroup-working-wconfig workgroup (wg-serialize-frame))))

(defun wg-workgroup-working-wconfig (workgroup)
  "Return WORKGROUP's working-wconfig, which is its current undo state.
If WORKGROUP is the current workgroup in `selected-frame',
`wg-update-workgroup-working-wconfig' will do its thing and
return WORKGROUP's updated working wconfig.  Otherwise, return
the current undo state unupdated."
  (or (wg-update-workgroup-working-wconfig workgroup)
      (wg-with-undo workgroup (state-table undo-pointer undo-list)
        (nth undo-pointer undo-list))))

(defun wg-add-wconfig-to-undo-list (workgroup wconfig)
  "Add WCONFIG to WORKGROUP's undo list, truncating its future if necessary."
  (wg-with-undo workgroup (state-table undo-pointer undo-list)
    (wg-set-workgroup-parameter workgroup 'working-wconfig wconfig)
    (let ((undo-list (cons wconfig (nthcdr undo-pointer undo-list))))
      (when (and wg-wconfig-undo-list-max
                 (> (length undo-list) wg-wconfig-undo-list-max))
        (setq undo-list (wg-take undo-list wg-wconfig-undo-list-max)))
      (puthash 'undo-list undo-list state-table)
      (puthash 'undo-pointer 0 state-table))))

(defun wg-restore-wconfig-undoably (wconfig &optional noflag noupdate)
  "Restore WCONFIG in `selected-frame', saving undo information."
  (let ((wg-flag-wconfig-changes nil))
    (unless noflag (setq wg-window-config-changed-p t))
    (unless noupdate (wg-update-workgroup-working-wconfig nil))
    (wg-restore-wconfig wconfig)))

(defun wg-increment-workgroup-undo-pointer (workgroup increment)
  "Increment WORKGROUP's undo-pointer by INCREMENT.
Also restore the wconfig at the incremented undo-pointer if
WORKGROUP is current."
  (wg-with-undo workgroup (state-table undo-pointer undo-list)
    (let ((new-pointer (+ undo-pointer increment)))
      (when (wg-within new-pointer 0 (length undo-list))
        (when (wg-current-workgroup-p workgroup t)
          (wg-restore-wconfig-undoably (nth new-pointer undo-list) t))
        (setf (gethash 'undo-pointer state-table) new-pointer)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun wg-update-working-wconfig-before-command ()
  "Update the current workgroup's working-wconfig before
`wg-commands-that-alter-window-configs'. Added to
`pre-command-hook'."
  (when (gethash this-command wg-commands-that-alter-window-configs)
    (wg-update-workgroup-working-wconfig nil)))

(defun wg-save-undo-after-wconfig-change ()
  "`wg-add-wconfig-to-undo-list' when `wg-window-config-changed-p'
is non-nil.  Added to `post-command-hook'."
  (when (and wg-window-config-changed-p (zerop (minibuffer-depth)))
    (wg-awhen (wg-current-workgroup t)
      (wg-add-wconfig-to-undo-list it (wg-serialize-frame))))
  (setq wg-window-config-changed-p nil))


;; commands that alter window-configs

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

(defun wg-make-workgroup (&rest key-value-pairs)
  "Return a new workgroup with KEY-VALUE-PAIRS."
  `(workgroup ,@(mapcar (lambda (kvp) (cons (car kvp) (cadr kvp)))
                        (wg-partition key-value-pairs 2))))

(defun wg-restore-workgroup-associated-buffers-internal (workgroup)
  "Restore all the buffers associated with WORKGROUP that can be restored."
  (wg-filter-map 'wg-restore-buffer (wg-workgroup-associated-buffers workgroup)))

(defun wg-restore-workgroup (workgroup)
  "Restore WORKGROUP in `selected-frame'."
  (when wg-restore-associated-buffers
    (wg-restore-workgroup-associated-buffers-internal workgroup))
  (let ((wg-nodirty 'seems-insane-to-flag-dirty-every-time))
    (wg-restore-wconfig-undoably (wg-workgroup-working-wconfig workgroup) t)))



;;; workgroup-list ops

(defun wg-set-workgroup-list (new-workgroup-list)
  "Set `wg-workgroup-set's workgroup-list param to NEW-WORKGROUP-LIST."
  (wg-set (wg-workgroup-set) 'workgroup-list new-workgroup-list))

(defun wg-delete-workgroup (workgroup)
  "Remove WORKGROUP from `wg-workgroup-list'.
Also delete all references to it by `wg-workgroup-table',
`wg-current-workgroup' and `wg-previous-workgroup'."
  (let ((workgroup (wg-get-workgroup-flexibly workgroup)))
    (dolist (frame (frame-list))
      (remhash (wg-workgroup-uid workgroup) (wg-workgroup-table frame))
      (when (wg-current-workgroup-p workgroup t frame)
        (wg-set-current-workgroup nil frame))
      (when (wg-previous-workgroup-p workgroup t frame)
        (wg-set-previous-workgroup nil frame)))
    (wg-set-workgroup-list (remove workgroup (wg-workgroup-list)))
    (setq wg-dirty t)))

(defun wg-add-workgroup (workgroup &optional pos)
  "Add WORKGROUP to `wg-workgroup-list'.
If a workgroup with the same name exists, overwrite it."
  (wg-awhen (wg-get-workgroup-by-name (wg-workgroup-name workgroup) t)
    (unless pos (setq pos (position it (wg-workgroup-list))))
    (wg-delete-workgroup it))
  (wg-set-workgroup-uid workgroup (wg-new-workgroup-uid))
  (wg-set-workgroup-list (wg-insert-elt workgroup (wg-workgroup-list t) pos))
  (setq wg-dirty t))

(defun wg-check-and-add-workgroup (workgroup)
  "Add WORKGROUP to `wg-workgroup-list'.
Query to overwrite if a workgroup with the same name exists."
  (let ((name (wg-workgroup-name workgroup)))
    (when (wg-get-workgroup-by-name name t)
      (unless (or wg-no-confirm
                  (y-or-n-p (format "%S exists. Overwrite? " name)))
        (error "Cancelled"))))
  (wg-add-workgroup workgroup))

(defun wg-cyclic-offset-workgroup (workgroup n)
  "Offset WORKGROUP's position in `wg-workgroup-list' by N."
  (let ((workgroup (wg-get-workgroup-flexibly workgroup))
        (workgroup-list (wg-workgroup-list)))
    (unless (member workgroup workgroup-list)
      (error "Workgroup isn't present in `wg-workgroup-list'."))
    (wg-set-workgroup-list
     (wg-cyclic-offset-elt workgroup workgroup-list n))
    (setq wg-dirty t)))

(defun wg-swap-workgroups-in-workgroup-list (workgroup1 workgroup2)
  "Swap the positions of WORKGROUP1 and WORKGROUP2 in `wg-workgroup-list'."
  (let ((workgroup1 (wg-get-workgroup-flexibly workgroup1))
        (workgroup2 (wg-get-workgroup-flexibly workgroup2))
        (workgroup-list (wg-workgroup-list)))
    (when (eq workgroup1 workgroup2)
      (error "Can't swap a workgroup with itself"))
    (unless (and (member workgroup1 workgroup-list)
                 (member workgroup2 workgroup-list))
      (error "Both workgroups aren't present in `wg-workgroup-list'."))
    (wg-set-workgroup-list
     (wg-util-swap workgroup1 workgroup2 workgroup-list))
    (setq wg-dirty t)))

(defun wg-cyclic-nth-from-workgroup (workgroup &optional n)
  "Return the workgroup N places from WORKGROUP in `wg-workgroup-list'."
  (wg-cyclic-nth-from-elt
   (wg-get-workgroup-flexibly workgroup) (wg-workgroup-list) (or n 1)))



;;; mode-line

(defun wg-mode-line-decor (decor-symbol)
  "Return DECOR-SYMBOL's decoration string in `wg-mode-line-decor-alist'."
  (wg-aget wg-mode-line-decor-alist decor-symbol))

(defun wg-mode-line-buffer-association-indicator (workgroup)
  "Return a string indicating `current-buffer's association-type in WORKGROUP."
  (case (wg-workgroup-buffer-association-type workgroup (current-buffer))
    (manual (wg-mode-line-decor 'manually-associated))
    (auto (wg-mode-line-decor 'auto-associated))
    (otherwise (wg-mode-line-decor 'unassociated))))

(defun wg-mode-line-string ()
  "Return the string to be displayed in the mode-line."
  (let ((wg (wg-current-workgroup t))
        (wg-use-faces wg-mode-line-use-faces)
        (divider (wg-mode-line-decor 'divider)))
    (cond (wg (wg-fontify " "
                (:div (wg-mode-line-decor 'left-brace))
                (:mode (wg-workgroup-name wg))
                (:div divider)
                (:mode (wg-mode-line-buffer-association-indicator wg))
                (:div divider)
                (:mode (wg-mode-line-decor (if wg-dirty 'dirty 'clean)))
                (:mode (wg-mode-line-decor
                        (if (wg-workgroup-dirty-p wg) 'dirty 'clean)))
                (:div (wg-mode-line-decor 'right-brace))))
          (t  (wg-fontify " "
                (:div (wg-mode-line-decor 'left-brace))
                (:mode "workgroups")
                (:div (wg-mode-line-decor 'right-brace)))))))

(defun wg-mode-line-add-display ()
  "Add Workgroups' mode-line format to `mode-line-format'."
  (unless (assq 'wg-mode-line-display-on mode-line-format)
    (let ((format `(wg-mode-line-display-on (:eval (wg-mode-line-string))))
          (pos (1+ (position 'mode-line-position mode-line-format))))
      (set-default 'mode-line-format
                   (wg-insert-elt format mode-line-format pos)))))

(defun wg-mode-line-remove-display ()
  "Remove Workgroups' mode-line format from `mode-line-format'."
  (wg-awhen (assq 'wg-mode-line-display-on mode-line-format)
    (set-default 'mode-line-format (remove it mode-line-format))
    (force-mode-line-update)))



;;; messaging

(defun wg-message (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS.
Also save the msg to `wg-last-message'."
  (setq wg-last-message (apply #'message format-string args)))

(defmacro wg-fontified-message (&rest format)
  "`wg-fontify' FORMAT and call `wg-message' on it."
  (declare (indent defun))
  `(wg-message (wg-fontify ,@format)))



;;; fancy displays

(defun wg-element-display (elt elt-string current-p previous-p)
  "Return display string for ELT."
  (wg-abind wg-list-display-decor-alist
      ((cl current-left)
       (cr current-right)
       (pl previous-left)
       (pr previous-right))
    (cond ((funcall current-p elt)
           (wg-fontify (:cur (concat cl elt-string cr))))
          ((funcall previous-p elt)
           (wg-fontify (:prev (concat pl elt-string pr))))
          (t (wg-fontify (:other elt-string))))))

(defun wg-workgroup-display (workgroup index)
  "Return display string for WORKGROUP at INDEX."
  (if (not workgroup) "No workgroups"
    (wg-element-display
     workgroup
     (format "%d: %s" index (wg-workgroup-name workgroup))
     (lambda (wg) (wg-current-workgroup-p wg t))
     (lambda (wg) (wg-previous-workgroup-p wg t)))))

(defun wg-buffer-display (buffer index)
  "Return display string for BUFFER. INDEX is ignored."
  (if (not buffer) "No buffers"
    (let ((buffer (get-buffer buffer)))
      (wg-element-display
       buffer (format "%s" (buffer-name buffer))
       (lambda (b) (eq (get-buffer b) (current-buffer)))
       (lambda (b) nil)))))

(defun wg-display-internal (elt-fn list)
  "Return display string built by calling ELT-FN on each element of LIST."
  (let ((div (wg-add-face :div (wg-aget wg-list-display-decor-alist 'divider)))
        (i -1))
    (wg-fontify
      (:brace (wg-aget wg-list-display-decor-alist 'left-brace))
      (if (not list) (funcall elt-fn nil nil)
        (wg-doconcat (elt list div) (funcall elt-fn elt (incf i))))
      (:brace (wg-aget wg-list-display-decor-alist 'right-brace)))))

(defun wg-workgroup-list-display (&optional workgroup-list)
  "Return the Workgroups list display string.
The string contains the names of all workgroups in `wg-workgroup-list',
decorated with faces, dividers and strings identifying the
current and previous workgroups."
  (wg-display-internal
   'wg-workgroup-display (or workgroup-list (wg-workgroup-list t))))

;; TODO: Scroll animation for the buffer list display during `wg-next-buffer'
;; and `wg-previous-buffer'?
(defun wg-buffer-list-display (buffer-list)
  "Return the buffer-list display string."
  (wg-display-internal
   'wg-buffer-display
   (if wg-center-rotate-buffer-list-display
       (wg-center-rotate-list buffer-list) buffer-list)))

(defun wg-buffer-list-filter-display (&optional workgroup blf-id)
  "Return a buffer-list-filter display string from WORKGROUP and BLF-ID."
  (wg-fontify
    (:div "(")
    (:msg (wg-workgroup-name (wg-get-workgroup-flexibly workgroup)))
    (:div ":")
    (:msg (wg-get-buffer-list-filter-val blf-id 'indicator))
    (:div ")")))

(defun wg-buffer-list-filter-prompt (prompt &optional workgroup method blf-id)
  "Return a prompt string indicating WORKGROUP and buffer-list-filter BLF-ID."
  (wg-fontify
    (:cmd prompt) " "
    (wg-buffer-list-filter-display workgroup blf-id)
    (:msg ": ")))

(defun wg-buffer-command-display (&optional buffer-list)
  "Return the buffer command display string."
  (concat
   (wg-buffer-list-filter-display) ": "
   (wg-buffer-list-display (or buffer-list (wg-filtered-buffer-list)))))

(defun wg-timeline-display (position length)
  "Return a timeline visualization string from POSITION and LENGTH."
  (wg-fontify
    (:div "-<{")
    (:other (wg-make-string (- length position) "-" "="))
    (:cur "O")
    (:other (wg-make-string (1+ position) "-" "="))
    (:div "}>-")))

(defun wg-undo-timeline-display (workgroup)
  "Return WORKGROUP's undo timeline string."
  (wg-with-undo workgroup (state-table undo-pointer undo-list)
    (wg-timeline-display undo-pointer (length undo-list))))



;;; ido and iswitchb compatibility

(defun wg-read-buffer-mode ()
  "Return the buffer switching package (ido or iswitchb) to use, or nil."
  (if (eq wg-current-buffer-list-filter-id 'fallback) 'fallback
    (case (let (workgroups-mode) (command-remapping 'switch-to-buffer))
      (ido-switch-buffer 'ido)
      (iswitchb-buffer 'iswitchb)
      (otherwise 'fallback))))

(defun wg-read-buffer-function (&optional mode)
  "Return MODE's or `wg-read-buffer-mode's `read-buffer' function."
  (case (or mode (wg-read-buffer-mode))
    (ido 'ido-read-buffer)
    (iswitchb 'iswitchb-read-buffer)
    (fallback (lambda (prompt &optional default require-match)
                (let (read-buffer-function)
                  (read-buffer prompt default require-match))))))

(defun wg-completing-read
  (prompt choices &optional pred require-match initial-input history default)
  "Do a completing read.  The function called depends on what's on."
  (ecase (wg-read-buffer-mode)
    (ido
     (ido-completing-read prompt choices pred require-match
                          initial-input history default))
    (iswitchb
     (let* ((iswitchb-use-virtual-buffers nil)
            (iswitchb-make-buflist-hook
             (lambda () (setq iswitchb-temp-buflist choices))))
       (iswitchb-read-buffer prompt default require-match)))
    (fallback
     (completing-read prompt choices pred require-match
                      initial-input history default))))

(defun wg-current-matches (&optional read-buffer-mode)
  "Return READ-BUFFER-MODE's current matches."
  (ecase (or read-buffer-mode (wg-read-buffer-mode))
    (ido (when (boundp 'ido-cur-list) ido-cur-list))
    (iswitchb (when (boundp 'iswitchb-buflist) iswitchb-buflist))
    (fallback (list minibuffer-default))))

(defun wg-current-match (&optional read-buffer-mode)
  "Return READ-BUFFER-MODE's current match."
  (car (wg-current-matches read-buffer-mode)))

(defun wg-set-current-matches (match-list &optional read-buffer-mode)
  "Set READ-BUFFER-MODE's current matches, and flag a rescan."
  (case (or read-buffer-mode (wg-read-buffer-mode))
    (ido
     (when (boundp 'ido-cur-list)
       (setq ido-cur-list match-list ido-rescan t)))
    (iswitchb
     (when (boundp 'iswitchb-buflist)
       (setq iswitchb-buflist match-list iswitchb-rescan t)))
    (fallback nil)))

(defun wg-iswitchb-internal (method &optional prompt default init)
  "This provides the buffer switching interface to
`iswitchb-read-buffer' (analogous to ido's `ido-buffer-internal')
that iswitchb *should* have had.  A lot of this code is
duplicated from `iswitchb', so is similarly shitty."
  (let ((iswitchb-method (if (memq method '(insert kill)) 'samewindow method))
        (iswitchb-invalid-regexp nil)
        (buffer (iswitchb-read-buffer (or prompt "iswitch ") default nil init)))
    (cond ((eq iswitchb-exit 'findfile)
           (call-interactively 'find-file))
          (iswitchb-invalid-regexp
           (message "Won't make invalid regexp named buffer"))
          ((not buffer) nil)
          ((not (get-buffer buffer))
           (iswitchb-possible-new-buffer buffer))
          ((eq method 'insert)
           (insert-buffer-substring buffer))
          ((eq method 'kill)
           (kill-buffer buffer))
          (t (iswitchb-visit-buffer buffer)))))

(defun wg-buffer-internal (method &optional prompt default)
  "Return a switch-to-buffer fn from `wg-read-buffer-mode'."
  (if (not (wg-filter-buffer-list-p))
      (call-interactively (wg-prior-mapping workgroups-mode method))
    (wg-with-buffer-list-filters method
      (ecase (wg-read-buffer-mode)
        (ido
         (ido-buffer-internal
          (wg-aget wg-ido-translations method) nil
          (wg-buffer-list-filter-prompt prompt)
          default wg-minibuffer-contents))
        (iswitchb
         (wg-iswitchb-internal
          (wg-aget wg-iswitchb-translations method)
          (wg-buffer-list-filter-prompt prompt)
          default wg-minibuffer-contents))
        (fallback
         (let (read-buffer-function) (call-interactively method))))
      (wg-message (wg-buffer-command-display)))))

(defun wg-rotate-buflist-if-necessary (buflist &optional buffer-command)
  "Conditionally move the first buffer in BUFLIST to the end.
Currently `kill-buffer' is the only command for which BUFLIST isn't rotated."
  (if (eq (or buffer-command wg-current-buffer-method) 'kill-buffer)
      buflist (wg-rotate-list buflist)))

(defun wg-set-buflist-symbol (symbol)
  "`set' SYMBOL to the (possibly rotated) filtered buffer-list."
  (when (and wg-current-buffer-list-filter-id (boundp symbol))
    (set symbol (wg-rotate-buflist-if-necessary (wg-filtered-buffer-list)))))

(defun wg-set-ido-buffer-list ()
  "Set `ido-temp-list' with `wg-set-buflist-symbol'.
Added to `ido-make-buffer-list-hook'."
  (wg-set-buflist-symbol 'ido-temp-list))

(defun wg-set-iswitchb-buffer-list ()
  "Set `iswitchb-temp-buflist' with `wg-set-buflist-symbol'.
Added to `iswitchb-make-buflist-hook'."
  (wg-set-buflist-symbol 'iswitchb-temp-buflist))



;;; minibuffer reading

(defun wg-read-buffer (prompt &optional default require-match)
  "Workgroups' version of `read-buffer'."
  (if (not (wg-filter-buffer-list-p))
      (funcall (wg-read-buffer-function) prompt default require-match)
    (wg-with-buffer-list-filters 'read-buffer
      (funcall (wg-read-buffer-function)
               (wg-buffer-list-filter-prompt
                (wg-aif (string-match ": *$" prompt)
                    (substring prompt 0 it) prompt))
               default require-match))))

;; TODO: Add minibuffer commands for killing, cloning, etc.
(defun wg-read-workgroup (&optional noerror)
  "Read a workgroup with `wg-completing-read'."
  (wg-get-workgroup-by-name
   (wg-completing-read
    "Workgroup: " (wg-workgroup-names) nil nil nil nil
    (wg-awhen (wg-current-workgroup t) (wg-workgroup-name it)))
   noerror))

(defun wg-new-default-workgroup-name ()
  "Return a new, unique, default workgroup name."
  (let ((names (wg-workgroup-names t)) (index -1) result)
    (while (not result)
      (let ((new-name (format "workgroup-%s" (incf index))))
        (unless (member new-name names)
          (setq result new-name))))
    result))

(defun wg-unique-workgroup-name-p (new-name)
  "Return t if NEW-NAME is unique in `wg-workgroup-list', nil otherwise."
  (every (lambda (existing-name) (not (equal new-name existing-name)))
         (wg-workgroup-names t)))

(defun wg-read-new-workgroup-name (&optional prompt)
  "Read a non-empty name string from the minibuffer."
  (let ((default (wg-new-default-workgroup-name)))
    (wg-read-object
     (or prompt (format "Name (default: %S): " default))
     (lambda (new) (and (stringp new)
                   (not (equal new ""))
                   (wg-unique-workgroup-name-p new)))
     "Please enter a unique, non-empty name"
     nil nil nil nil default)))

(defun wg-read-workgroup-index ()
  "Prompt for the index of a workgroup."
  (let ((max (1- (length (wg-workgroup-list)))))
    (wg-read-object
     (format "%s\n\nEnter [0-%d]: " (wg-workgroup-list-display) max)
     (lambda (obj) (and (integerp obj) (wg-within obj 0 max t)))
     (format "Please enter an integer [%d-%d]" 0 max)
     nil nil t)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                          ;;;
;;;                                                                          ;;;
;;;                                Commands                                  ;;;
;;;                                                                          ;;;
;;;                                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; workgroup switching commands

(defun wg-switch-to-workgroup (workgroup &optional noerror)
  "Switch to WORKGROUP."
  (interactive (list (wg-read-workgroup)))
  (let ((workgroup (wg-get-workgroup-flexibly workgroup))
        (current (wg-current-workgroup t)))
    (unless (or (not (eq workgroup current)) noerror)
      (error "Already on: %s" (wg-workgroup-name current)))
    (wg-restore-workgroup workgroup)
    (wg-set-previous-workgroup current)
    (wg-set-current-workgroup workgroup))
  (run-hooks 'wg-switch-to-workgroup-hook)
  (wg-fontified-message
    (:cmd "Switched:  ")
    (wg-workgroup-list-display)))

;; (defun wg-switch-to-workgroup (workgroup &optional noerror)
;;   "Switch to WORKGROUP."
;;   (interactive (list (wg-read-workgroup)))
;;   (when (and (wg-current-workgroup-p workgroup t) (not noerror))
;;     (error "Already on: %s" (wg-workgroup-name workgroup)))
;;   (wg-restore-workgroup workgroup)
;;   (wg-set-previous-workgroup (wg-current-workgroup t))
;;   (wg-set-current-workgroup workgroup)
;;   (run-hooks 'wg-switch-to-workgroup-hook)
;;   (wg-fontified-message
;;     (:cmd "Switched:  ")
;;     (wg-workgroup-list-display)))

(defun wg-switch-to-workgroup-other-frame (workgroup &optional n)
  "Switch to WORKGROUP in the frame N places cyclically from `selected-frame'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list (wg-read-workgroup) current-prefix-arg))
  (with-selected-frame (wg-cyclic-nth-from-frame (or n 1))
    (wg-switch-to-workgroup workgroup)))

(defun wg-switch-to-workgroup-at-index (index)
  "Switch to the workgroup at INDEX in `wg-workgroup-list'."
  (interactive (list (or current-prefix-arg (wg-read-workgroup-index))))
  (let ((wl (wg-workgroup-list)))
    (wg-switch-to-workgroup
     (or (nth index wl) (error "There are only %d workgroups" (length wl))))))

;; Define wg-switch-to-workgroup-at-index-[0-9]:
(macrolet
    ((d (n)
        `(defun ,(intern (format "wg-switch-to-workgroup-at-index-%d" n)) ()
           ,(format "Switch to the workgroup at index %d." n)
           (interactive) (wg-switch-to-workgroup-at-index ,n))))
  (d 0) (d 1) (d 2) (d 3) (d 4) (d 5) (d 6) (d 7) (d 8) (d 9))

(defun wg-switch-to-cyclic-nth-from-workgroup (workgroup n)
  "Switch N workgroups cyclically from WORKGROUP in `wg-workgroup-list.'"
  (wg-switch-to-workgroup
   (wg-aif (wg-get-workgroup-flexibly workgroup t)
       (wg-cyclic-nth-from-workgroup it n)
     (car (wg-workgroup-list)))))

(defun wg-switch-to-workgroup-left (&optional workgroup n)
  "Switch to the workgroup (- N) places from WORKGROUP in `wg-workgroup-list'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list nil current-prefix-arg))
  (wg-switch-to-cyclic-nth-from-workgroup workgroup (- (or n 1))))

(defun wg-switch-to-workgroup-right (&optional workgroup n)
  "Switch to the workgroup N places from WORKGROUP in `wg-workgroup-list'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list nil current-prefix-arg))
  (wg-switch-to-cyclic-nth-from-workgroup workgroup (or n 1)))

(defun wg-switch-to-workgroup-left-other-frame (&optional n)
  "Like `wg-switch-to-workgroup-left', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-cyclic-nth-from-frame (or n 1))
    (call-interactively 'wg-switch-to-workgroup-left)))

(defun wg-switch-to-workgroup-right-other-frame (&optional n)
  "Like `wg-switch-to-workgroup-right', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-cyclic-nth-from-frame (or n -1))
    (call-interactively 'wg-switch-to-workgroup-right)))

(defun wg-switch-to-previous-workgroup ()
  "Switch to the previous workgroup."
  (interactive)
  (wg-switch-to-workgroup (wg-previous-workgroup)))



;;; workgroup creation commands

(defun wg-create-workgroup (name)
  "Create and add a workgroup named NAME.
If there is no current workgroup, use the frame's current
window-config.  Otherwise, use a blank, one window window-config.
The reason for not using the current workgroup's window-config
for the new workgroup is that we don't want to give the
impression that the current workgroup's other parameters (like
associated buffers) have been copied to the new workgroup
as well.  For that, use `wg-clone-workgroup'."
  (interactive (list (wg-read-new-workgroup-name)))
  (let ((workgroup (wg-make-workgroup
                    'name name
                    'base-wconfig (if (wg-current-workgroup t)
                                      (wg-make-blank-wconfig)
                                    (wg-serialize-frame)))))
    (wg-check-and-add-workgroup workgroup)
    (wg-switch-to-workgroup workgroup)
    (wg-fontified-message
      (:cmd "Created: ")
      (:cur name) "  "
      (wg-workgroup-list-display))))

(defun wg-clone-workgroup (workgroup name)
  "Create and add a clone of WORKGROUP named NAME.
Keep in mind that only WORKGROUP's top-level alist structure is
copied, so destructive operations on the keys or values of
WORKGROUP will be reflected in the clone, and vice-versa.  Be
safe -- be immutable."
  (interactive (list (wg-current-workgroup) (wg-read-new-workgroup-name)))
  (let ((clone (wg-copy-workgroup workgroup)))
    (wg-set-workgroup-name clone name)
    (wg-check-and-add-workgroup clone)
    (wg-set-workgroup-working-wconfig
     clone (wg-workgroup-working-wconfig workgroup))
    (wg-switch-to-workgroup clone)
    (wg-fontified-message
      (:cmd "Cloned: ")
      (:cur (wg-workgroup-name workgroup))
      (:msg " to ")
      (:cur name) "  "
      (wg-workgroup-list-display))))



;;; workgroup killing commands

(defun wg-add-to-wconfig-kill-ring (wconfig)
  "Add WCONFIG to `wg-wconfig-kill-ring'."
  (push wconfig wg-wconfig-kill-ring)
  (setq wg-wconfig-kill-ring
        (wg-take wg-wconfig-kill-ring wg-wconfig-kill-ring-max)))

(defun wg-kill-workgroup (workgroup)
  "Kill WORKGROUP, saving its working-wconfig to the kill ring."
  (interactive (list (wg-current-workgroup)))
  (let ((to (or (wg-previous-workgroup t)
                (wg-cyclic-nth-from-workgroup workgroup))))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-working-wconfig workgroup))
    (wg-delete-workgroup workgroup)
    (if (wg-workgroups-eq workgroup to)
        (wg-restore-wconfig-undoably (wg-make-blank-wconfig))
      (wg-switch-to-workgroup to))
    (wg-fontified-message
      (:cmd "Killed: ")
      (:cur (wg-workgroup-name workgroup)) "  "
      (wg-workgroup-list-display))))

(defun wg-kill-ring-save-base-wconfig (workgroup)
  "Save WORKGROUP's base wconfig to `wg-wconfig-kill-ring'."
  (interactive (list (wg-current-workgroup)))
  (wg-add-to-wconfig-kill-ring (wg-workgroup-base-wconfig workgroup))
  (wg-fontified-message
    (:cmd "Saved: ")
    (:cur (wg-workgroup-name workgroup))
    (:cur "'s ")
    (:msg "base wconfig to the kill ring")))

(defun wg-kill-ring-save-working-wconfig (workgroup)
  "Save WORKGROUP's working-wconfig to `wg-wconfig-kill-ring'."
  (interactive (list (wg-current-workgroup)))
  (wg-add-to-wconfig-kill-ring (wg-workgroup-working-wconfig workgroup))
  (wg-fontified-message
    (:cmd "Saved: ")
    (:cur (wg-workgroup-name workgroup))
    (:cur "'s ")
    (:msg "working-wconfig to the kill ring")))

(defun wg-yank-wconfig ()
  "Restore a wconfig from `wg-wconfig-kill-ring'.
Successive yanks restore wconfigs sequentially from the kill
ring, starting at the front."
  (interactive)
  (unless wg-wconfig-kill-ring (error "The kill-ring is empty"))
  (let ((pos (if (not (eq real-last-command 'wg-yank-wconfig)) 0
               (mod (1+ (or (get 'wg-yank-wconfig :position) 0))
                    (length wg-wconfig-kill-ring)))))
    (put 'wg-yank-wconfig :position pos)
    (wg-restore-wconfig-undoably (nth pos wg-wconfig-kill-ring))
    (wg-fontified-message
      (:cmd "Yanked: ")
      (:msg (format "%S" pos)) "  "
      (wg-workgroup-list-display))))

(defun wg-kill-workgroup-and-buffers (workgroup)
  "Kill WORKGROUP and the buffers in its working-wconfig."
  (interactive (list (wg-current-workgroup)))
  (let ((bufs (save-window-excursion
                (wg-restore-workgroup workgroup)
                (mapcar #'window-buffer (window-list)))))
    (wg-kill-workgroup workgroup)
    (mapc #'kill-buffer bufs)
    (wg-fontified-message
      (:cmd "Killed: ")
      (:cur (wg-workgroup-name workgroup))
      (:msg " and its buffers ") "\n"
      (wg-workgroup-list-display))))

(defun wg-delete-other-workgroups (workgroup)
  "Delete all workgroups but WORKGROUP."
  (interactive (list (wg-current-workgroup)))
  (unless (or wg-no-confirm (y-or-n-p "Really delete all other workgroups? "))
    (error "Cancelled"))
  (dolist (w (wg-workgroup-list))
    (unless (wg-workgroups-eq w workgroup)
      (wg-delete-workgroup w)))
  (unless (wg-current-workgroup-p workgroup t)
    (wg-switch-to-workgroup workgroup))
  (wg-fontified-message
    (:cmd "Deleted: ")
    (:msg "All workgroups but ")
    (:cur (wg-workgroup-name workgroup))))



;;; workgroup updating and reverting commands

(defun wg-update-workgroup (workgroup)
  "Set the base wconfig of WORKGROUP to its working-wconfig in `selected-frame'."
  (interactive (list (wg-current-workgroup)))
  (wg-set-workgroup-base-wconfig
   workgroup (wg-workgroup-working-wconfig workgroup))
  (wg-fontified-message
    (:cmd "Updated: ")
    (:cur (wg-workgroup-name workgroup))))

(defun wg-update-all-workgroups ()
  "Update all workgroups' base wconfigs.
Worgroups are updated with their working-wconfigs in the
`selected-frame'."
  (interactive)
  (mapc #'wg-update-workgroup (wg-workgroup-list))
  (wg-fontified-message
    (:cmd "Updated: ")
    (:msg "All")))

(defun wg-revert-workgroup (workgroup)
  "Set the working-wconfig of WORKGROUP to its base wconfig in `selected-frame'."
  (interactive (list (wg-current-workgroup)))
  (let ((base (wg-workgroup-base-wconfig workgroup)))
    (if (wg-current-workgroup-p workgroup t)
        (wg-restore-wconfig-undoably base)
      (wg-add-wconfig-to-undo-list workgroup base)))
  (wg-fontified-message
    (:cmd "Reverted: ")
    (:cur (wg-workgroup-name workgroup))))

(defun wg-revert-all-workgroups ()
  "Revert all workgroups to their base wconfigs."
  (interactive)
  (mapc #'wg-revert-workgroup (wg-workgroup-list))
  (wg-fontified-message
    (:cmd "Reverted: ")
    (:msg "All")))



;;; workgroup-list reorganization commands

(defun wg-swap-workgroups ()
  "Swap the previous and current workgroups."
  (interactive)
  (wg-swap-workgroups-in-workgroup-list
   (wg-current-workgroup) (wg-previous-workgroup))
  (wg-fontified-message
    (:cmd "Swapped:  ")
    (wg-workgroup-list-display)))

(defun wg-offset-workgroup-left (workgroup &optional n)
  "Offset WORKGROUP leftward in `wg-workgroup-list' cyclically."
  (interactive (list nil current-prefix-arg))
  (wg-cyclic-offset-workgroup workgroup (or n -1))
  (wg-fontified-message
    (:cmd "Offset left: ")
    (wg-workgroup-list-display)))

(defun wg-offset-workgroup-right (workgroup &optional n)
  "Offset WORKGROUP rightward in `wg-workgroup-list' cyclically."
  (interactive (list nil current-prefix-arg))
  (wg-cyclic-offset-workgroup workgroup (or n 1))
  (wg-fontified-message
    (:cmd "Offset right: ")
    (wg-workgroup-list-display)))



;;; undo/redo commands

(defun wg-undo-wconfig-change (workgroup)
  "Undo a change to the current workgroup's window-configuration."
  (interactive (list nil))
  (let ((undone? (wg-increment-workgroup-undo-pointer workgroup 1)))
    (wg-fontified-message
      (:cmd "Undo: ")
      (wg-undo-timeline-display workgroup)
      (:cur (if undone? "" "  No more undo info")))))

(defun wg-redo-wconfig-change (workgroup)
  "Redo a change to the current workgroup's window-configuration."
  (interactive (list nil))
  (let ((redone? (wg-increment-workgroup-undo-pointer workgroup -1)))
    (wg-fontified-message
      (:cmd "Redo: ")
      (wg-undo-timeline-display workgroup)
      (:cur (if redone? "" "  No more redo info")))))

(defun wg-undo-once-all-workgroups ()
  "Do what the name says.  Useful for instance when you
accidentally call `wg-revert-all-workgroups' and want to return
all workgroups to their un-reverted state."
  (interactive)
  (mapc 'wg-undo-wconfig-change (wg-workgroup-list))
  (wg-message "Undid once on all workgroups."))

(defun wg-redo-once-all-workgroups ()
  "Do what the name says.  Probably useless.  Included for
symetry with `wg-undo-once-all-workgroups'."
  (interactive)
  (mapc 'wg-redo-wconfig-change (wg-workgroup-list))
  (wg-message "Redid once on all workgroups."))



;;; buffer-list-filter commands

(defun wg-switch-to-buffer ()
  "Workgroups' version of `switch-to-buffer'."
  (interactive)
  (wg-buffer-internal 'switch-to-buffer "Buffer"))

(defun wg-switch-to-buffer-other-window ()
  "Workgroups' version of `switch-to-buffer-other-window'."
  (interactive)
  (wg-buffer-internal
   'switch-to-buffer-other-window "Switch to buffer in other window"))

(defun wg-switch-to-buffer-other-frame ()
  "Workgroups' version of `switch-to-buffer-other-frame'."
  (interactive)
  (wg-buffer-internal
   'switch-to-buffer-other-frame "Switch to buffer in other frame"))

(defun wg-kill-buffer ()
  "Workgroups' version of `kill-buffer'."
  (interactive)
  (wg-buffer-internal
   'kill-buffer "Kill buffer" (buffer-name (current-buffer))))

(defun wg-display-buffer ()
  "Workgroups' version of `display-buffer'."
  (interactive)
  (wg-buffer-internal 'display-buffer "Display buffer"))

(defun wg-insert-buffer ()
  "Workgroups' version of `insert-buffer'."
  (interactive)
  (wg-buffer-internal 'insert-buffer "Insert buffer"))

;; FIXME: If you C-h i for info, then wg-next-buffer, you occasionally don't
;; switch to the buffer you were on previously.
(defun wg-next-buffer-internal (buffer-list &optional prev noerror)
  "Switch to the next buffer in Workgroups' filtered buffer list."
  (when buffer-list
    (let* ((cur (buffer-name (current-buffer)))
           (next (or (wg-cyclic-nth-from-elt cur buffer-list (if prev -1 1))
                     (car buffer-list))))
      (unless (equal cur next)
        (switch-to-buffer next)
        (unless prev
          (let (wg-dissociate-buffer-on-bury-buffer)
            (bury-buffer cur)))
        next))))

(defun wg-next-buffer (&optional prev)
  "Switch to the next buffer in Workgroups' filtered buffer list.
In the post-command message the current buffer is rotated to the
middle of the list to more easily see where `wg-previous-buffer'
will take you."
  (interactive)
  (let ((method (if prev 'previous-buffer 'next-buffer)))
    (if (not (wg-filter-buffer-list-p))
        (call-interactively (wg-prior-mapping workgroups-mode method))
      (wg-with-buffer-list-filters method
        (wg-awhen (wg-filtered-buffer-list) (wg-next-buffer-internal it prev))
        (wg-message (wg-buffer-command-display))))))

(defun wg-previous-buffer (&optional fallback)
  "Switch to the next buffer in Workgroups' filtered buffer list."
  (interactive "P")
  (wg-next-buffer t))

(defun wg-bury-buffer (&optional buffer-or-name)
  "Remove BUFFER-OR-NAME from the current workgroup, bury it,
and switch to the next buffer in the buffer-list-filter."
  (interactive (list (current-buffer)))
  (if (not (wg-filter-buffer-list-p))
      (call-interactively (wg-prior-mapping workgroups-mode 'bury-buffer))
    (wg-with-buffer-list-filters 'bury-buffer
      (let ((buffer (get-buffer buffer-or-name)))
        (wg-next-buffer-internal (wg-filtered-buffer-list))
        (bury-buffer buffer)
        (wg-message (wg-buffer-command-display))))))

(defun wg-associate-buffer-with-workgroup (workgroup buffer)
  "Associate BUFFER with WORKGROUP."
  (interactive (list nil (current-buffer)))
  (wg-message
   (if (wg-workgroup-update-or-associate-buffer workgroup buffer 'manual)
       "Updated %S in %s" "Associated %S with %s")
   (buffer-name (get-buffer buffer))
   (wg-workgroup-name workgroup)))

(defun wg-dissociate-buffer-from-workgroup (workgroup buffer)
  "Dissociate BUFFER from WORKGROUP."
  (interactive (list nil (current-buffer)))
  (wg-message
   (if (wg-workgroup-dissociate-buffer workgroup buffer t)
       "Dissociated %S from %s" "%S isn't associated with %s")
   (buffer-name (get-buffer buffer))
   (wg-workgroup-name workgroup)))

(defun wg-restore-workgroup-associated-buffers (workgroup)
  "Restore all the buffers associated with WORKGROUP that can be restored."
  (interactive (list nil))
  (let ((restored-buffers (wg-restore-workgroup-associated-buffers-internal
                           workgroup)))
    (wg-fontified-message
      (:cmd "Restored: ")
      (wg-buffer-list-display restored-buffers))))

(defun wg-cycle-buffer-association-type ()
  "Cycle the current buffer's association type in the current workgroup.
See `wg-workgroup-cycle-buffer-association-type' for details."
  (interactive)
  (let ((workgroup (wg-current-workgroup))
        (buffer (current-buffer)))
    (wg-workgroup-cycle-buffer-association-type workgroup buffer)
    (force-mode-line-update)
    (wg-fontified-message
      (:cur (buffer-name buffer))
      (:cmd (case (wg-workgroup-buffer-association-type workgroup buffer)
              (manual " manually associated with ")
              (auto " automatically associated with ")
              (otherwise " dissociated from ")))
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-purge-auto-associated-buffers (workgroup)
  "Remove buffers from the current workgroup that were added automatically."
  (interactive (list nil))
  (wg-workgroup-purge-auto-associated-buffers workgroup)
  (wg-fontified-message
    (:cmd "Remaining buffers: ")
    (wg-buffer-list-display (wg-workgroup-live-buffers workgroup))))



;;; misc commands

(defun wg-rename-workgroup (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list nil (wg-read-new-workgroup-name "New name: ")))
  (let ((oldname (wg-workgroup-name workgroup)))
    (wg-set-workgroup-name workgroup newname)
    (wg-fontified-message
      (:cmd "Renamed: ")
      (:cur oldname)
      (:msg " to ")
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-reset-frame (frame)
  "Reset Workgroups' frame-parameters in FRAME to nil."
  (set-frame-parameter frame 'wg-workgroup-table nil)
  (set-frame-parameter frame 'wg-current-workgroup-uid nil)
  (set-frame-parameter frame 'wg-previous-workgroup-uid nil))

(defun wg-reset (&optional force)
  "Reset workgroups.
Deletes all state saved in frame parameters, and nulls out
`wg-workgroup-list', `wg-visited-file-name' and `wg-wconfig-kill-ring'."
  (interactive "P")
  (unless (or force wg-no-confirm (y-or-n-p "Are you sure? "))
    (error "Canceled"))
  (mapc 'wg-reset-frame (frame-list))
  (setq wg-workgroup-set nil
        wg-visited-file-name nil
        wg-dirty nil)
  (wg-fontified-message
    (:cmd "Reset: ")
    (:msg "Workgroups")))



;;; file commands

(defun wg-save-workgroups (file &optional force)
  "Save workgroups to FILE.
Called interactively with a prefix arg, or if `wg-visited-file-name'
is nil, read a filename.  Otherwise use `wg-visited-file-name'."
  (interactive
   (list (if (or current-prefix-arg (not (wg-visited-file-name t)))
             (read-file-name "File: ") (wg-visited-file-name))))
  (if (and (not force) (not (wg-dirty-p)))
      (wg-message "(No workgroups need to be saved)")
    (let ((workgroup-set (wg-workgroup-set)))
      (setq wg-visited-file-name file)
      (wg-set workgroup-set 'version wg-version)
      (wg-update-workgroup-working-wconfig nil)
      (wg-write-sexp-to-file workgroup-set file)
      (wg-mark-everything-clean)
      (wg-fontified-message (:cmd "Wrote: ") (:file file)))))

(defun wg-find-new-workgroups-file (filename)
  "Reset Workgroups and set `wg-visited-file-name' to FILENAME."
  (interactive "FNew workgroups file: ")
  (when (file-exists-p filename)
    (error "%S already exists" filename))
  (when (wg-query-for-save)
    (wg-reset t)
    (setq wg-visited-file-name filename)
    (wg-fontified-message
      (:cmd "Visited new workgroups file: ")
      (:file (format "%S" filename)))))

(defun wg-find-workgroups-file (filename)
  "Load workgroups from FILENAME."
  (interactive "FWorkgroups file: ")
  (if (not (file-exists-p filename))
      (wg-find-new-workgroups-file filename)
    (let ((sexp (wg-read-sexp-from-file filename)))
      (unless (wg-workgroup-set-p sexp)
        (error "%S is not a workgroups file." filename))
      (wg-reset t)
      (setq wg-workgroup-set sexp wg-visited-file-name filename))
    (wg-awhen (and wg-switch-to-first-workgroup-on-find-workgroups-file
                   (wg-workgroup-list t))
      (wg-switch-to-workgroup (car it)))
    (wg-fontified-message
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
  "Call `wg-update-all-workgroups', the `wg-save-workgroups'.
Keep in mind that workgroups will be updated with their
working-wconfig in the current frame."
  (interactive)
  (wg-update-all-workgroups)
  (call-interactively 'wg-save-workgroups))



;;; toggle commands

(defun wg-toggle-and-message (symbol)
  "Toggle SYMBOL's truthiness and message the new value."
  (wg-fontified-message
    (:cmd (format "%s: " symbol))
    (:msg (format "%s" (wg-toggle symbol)))))

(defun wg-toggle-buffer-list-filtration ()
  "Toggle `wg-buffer-list-filtration-on'."
  (interactive)
  (wg-toggle-and-message 'wg-buffer-list-filtration-on))

(defun wg-toggle-mode-line-display ()
  "Toggle `wg-mode-line-display-on'."
  (interactive)
  (wg-toggle-and-message 'wg-mode-line-display-on))

(defun wg-toggle-morph ()
  "Toggle `wg-morph-on'."
  (interactive)
  (wg-toggle-and-message 'wg-morph-on))



;;; window movement commands

(defun wg-backward-transpose-window (offset)
  "Move `selected-window' backward by OFFSET in its wlist."
  (interactive (list (or current-prefix-arg -1)))
  (wg-restore-wconfig-undoably
   (wg-wconfig-move-window
    (wg-workgroup-working-wconfig (wg-current-workgroup))
    offset)))

(defun wg-transpose-window (offset)
  "Move `selected-window' forward by OFFSET in its wlist."
  (interactive (list (or current-prefix-arg 1)))
  (wg-restore-wconfig-undoably
   (wg-wconfig-move-window
    (wg-workgroup-working-wconfig (wg-current-workgroup))
    offset)))

(defun wg-reverse-frame-horizontally (workgroup)
  "Reverse the order of all horizontally split wtrees."
  (interactive (list nil))
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig (wg-workgroup-working-wconfig workgroup))))

(defun wg-reverse-frame-vertically (workgroup)
  "Reverse the order of all vertically split wtrees."
  (interactive (list nil))
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig (wg-workgroup-working-wconfig workgroup) t)))

(defun wg-reverse-frame-horizontally-and-vertically (workgroup)
  "Reverse the order of all wtrees."
  (interactive (list nil))
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig (wg-workgroup-working-wconfig workgroup) 'both)))



;;; echo commands

(defun wg-echo-current-workgroup ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (wg-fontified-message
    (:cmd "Current: ")
    (:cur (wg-workgroup-name (wg-current-workgroup)))))

(defun wg-echo-all-workgroups ()
  "Display the names of all workgroups in the echo area."
  (interactive)
  (wg-fontified-message
    (:cmd "Workgroups: ")
    (wg-workgroup-list-display)))

(defun wg-echo-time ()
  "Echo the current time.  Optionally includes `battery' info."
  (interactive)
  (wg-message ;; Pass through format to escape the % in `battery'
   "%s" (wg-fontify
          (:cmd "Current time: ")
          (:msg (format-time-string wg-time-format))
          (when (and wg-display-battery (fboundp 'battery))
            (wg-fontify "\n" (:cmd "Battery: ") (:msg (battery)))))))

(defun wg-echo-version ()
  "Echo Workgroups' current version number."
  (interactive)
  (wg-fontified-message
    (:cmd "Workgroups version: ")
    (:msg wg-version)))

(defun wg-echo-last-message ()
  "Echo the last message Workgroups sent to the echo area.
The string is passed through a format arg to escape %'s."
  (interactive)
  (message "%s" wg-last-message))



;;; wg-minibuffer-mode commands

(defun wg-next-buffer-list-filter ()
  "Trigger a switch to the next buffer-list-filter."
  (interactive)
  (throw 'action (list 'next (minibuffer-contents))))

(defun wg-previous-buffer-list-filter ()
  "Trigger a switch to the previous buffer-list-filter."
  (interactive)
  (throw 'action (list 'prev (minibuffer-contents))))

(defun wg-magic-C-b ()
  "Call `backward-char' unless `point' is right after the prompt,
in which case call `wg-next-buffer-list-filter'."
  (interactive)
  (if (> (point) (minibuffer-prompt-end)) (backward-char)
    (wg-next-buffer-list-filter)))

(defun wg-magic-C-S-b (&optional num)
  "Call `backward-char' unless `point' is right after the prompt,
in which case call `wg-previous-buffer-list-filter'."
  (interactive)
  (if (> (point) (minibuffer-prompt-end)) (backward-char)
    (wg-previous-buffer-list-filter)))

(defun wg-dissociate-first-match ()
  "Dissociate the first match from current workgroup."
  (interactive)
  (wg-when-let ((mode (wg-read-buffer-mode))
                (buffer (wg-current-match mode))
                (pos (position buffer (wg-filtered-buffer-list) :test 'equal)))
    (wg-workgroup-dissociate-buffer nil buffer t)
    (wg-set-current-matches
     (wg-rotate-list (wg-filtered-buffer-list) pos) mode)))

(defun wg-associate-first-match ()
  "Associate the first match with or update it in the current workgroup."
  (interactive)
  (wg-when-let ((mode (wg-read-buffer-mode))
                (buffer (wg-current-match mode))
                (pos (position buffer (wg-filtered-buffer-list) :test 'equal)))
    (wg-workgroup-update-or-associate-buffer nil buffer 'manual)
    (wg-set-current-matches
     (wg-rotate-list (wg-filtered-buffer-list) pos) mode)))

(defvar wg-minibuffer-mode-map
  (wg-fill-keymap (make-sparse-keymap)
    "C-b"       'wg-magic-C-b
    "C-c n"     'wg-next-buffer-list-filter
    "C-c C-n"   'wg-next-buffer-list-filter
    "C-S-b"     'wg-magic-C-S-b
    "C-c p"     'wg-previous-buffer-list-filter
    "C-c C-p"   'wg-previous-buffer-list-filter
    "C-c a"     'wg-associate-first-match
    "C-c C-a"   'wg-associate-first-match
    "C-c d"     'wg-dissociate-first-match
    "C-c C-d"   'wg-dissociate-first-match
    )
  "`wg-minibuffer-mode's keymap.")

(defvar wg-minibuffer-mode-minor-mode-map-entry
  (cons 'wg-minibuffer-mode wg-minibuffer-mode-map)
  "`wg-minibuffer-mode's entry in `minor-mode-map-alist'.")

(define-minor-mode wg-minibuffer-mode
  "Minor mode for Workgroups' minibuffer commands."
  :global t
  :group 'workgroups
  (when wg-minibuffer-mode
    (add-to-list 'minor-mode-map-alist
                 wg-minibuffer-mode-minor-mode-map-entry)))

(defun wg-turn-on-minibuffer-mode ()
  "`minibuffer-setup-hook' to turn on `wg-minibuffer-mode'."
  (when wg-current-buffer-list-filter-id
    (wg-minibuffer-mode 1)))

(defun wg-turn-off-minibuffer-mode ()
  "`minibuffer-exit-hook' to turn off `wg-minibuffer-mode'."
  (when wg-current-buffer-list-filter-id
    (wg-minibuffer-mode -1)))



;;; help commands

(defun wg-help ()
  "Just call `apropos-command' on \"^wg-\".
There used to be a bunch of help-buffer construction stuff here,
including a `wg-help' variable that basically duplicated every
command's docstring;  But why, when there's `apropos-command'?"
  (interactive)
  (apropos-command "^wg-"))



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
    "M-W"        'wg-kill-ring-save-base-wconfig
    "M-w"        'wg-kill-ring-save-working-wconfig
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
    "M-v"        'wg-switch-to-workgroup-other-frame
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
    "C-p"        'wg-switch-to-workgroup-left
    "p"          'wg-switch-to-workgroup-left
    "C-n"        'wg-switch-to-workgroup-right
    "n"          'wg-switch-to-workgroup-right
    "M-p"        'wg-switch-to-workgroup-left-other-frame
    "M-n"        'wg-switch-to-workgroup-right-other-frame
    "C-a"        'wg-switch-to-previous-workgroup
    "a"          'wg-switch-to-previous-workgroup


    ;; wconfig undo/redo

    "<left>"     'wg-undo-wconfig-change
    "<right>"    'wg-redo-wconfig-change
    "["          'wg-undo-wconfig-change
    "]"          'wg-redo-wconfig-change
    "{"          'wg-undo-once-all-workgroups
    "}"          'wg-redo-once-all-workgroups


    ;; buffer-list

    "+"          'wg-associate-buffer-with-workgroup
    "-"          'wg-dissociate-buffer-from-workgroup
    "="          'wg-cycle-buffer-association-type
    "*"          'wg-restore-workgroup-associated-buffers
    "_"          'wg-purge-auto-associated-buffers
    "M-b"        'wg-toggle-buffer-list-filtration
    "("          'wg-next-buffer
    ")"          'wg-previous-buffer


    ;; workgroup movement

    "C-x"        'wg-swap-workgroups
    "C-,"        'wg-offset-workgroup-left
    "C-."        'wg-offset-workgroup-right


    ;; file and buffer

    "C-s"        'wg-save-workgroups
    "C-l"        'wg-find-workgroups-file
    "M-l"        'wg-find-new-workgroups-file
    "S"          'wg-update-all-workgroups-and-save
    "C-f"        'wg-find-file
    "S-C-f"      'wg-find-file-read-only
    "C-b"        'wg-switch-to-buffer
    "b"          'wg-switch-to-buffer
    "d"          'wg-dired


    ;; window moving and frame reversal

    "<"          'wg-backward-transpose-window
    ">"          'wg-transpose-window
    "|"          'wg-reverse-frame-horizontally
    "\\"         'wg-reverse-frame-vertically
    "/"          'wg-reverse-frame-horizontally-and-vertically


    ;; toggling

    "C-i"        'wg-toggle-mode-line-display
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


(defun wg-make-minor-mode-map ()
  "Return Workgroups' minor-mode-map, which contains all its
remappings of standard commands."
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
    (define-key map
      [remap bury-buffer] 'wg-bury-buffer)
    map))

(defun wg-setup-minor-mode-map-entry ()
  "Setup `wg-minor-mode-map-entry', and add it to
`minor-mode-map-alist' if necessary."
  (let ((map (wg-make-minor-mode-map)))
    (if wg-minor-mode-map-entry
        (setcdr wg-minor-mode-map-entry map)
      (setq wg-minor-mode-map-entry (cons 'workgroups-mode map))
      (add-to-list 'minor-mode-map-alist wg-minor-mode-map-entry))))



;;; prefix key

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



;; advice

(defadvice switch-to-buffer (after wg-auto-associate-buffer)
  "Automatically associate the buffer with the current workgroup."
  (when (and wg-buffer-list-filtration-on
             wg-associate-buffer-on-switch-to-buffer)
    (wg-awhen (wg-current-workgroup t)
      (wg-workgroup-associate-buffer it ad-return-value 'auto t))))

(defadvice set-window-buffer (after wg-auto-associate-buffer)
  "Automatically associate the buffer with the current workgroup."
  (when (and wg-buffer-list-filtration-on
             wg-associate-buffer-on-set-window-buffer)
    (let ((frame (window-frame (ad-get-arg 0))))
      (wg-awhen (wg-current-workgroup t frame)
        (wg-workgroup-associate-buffer it (ad-get-arg 1) 'auto t)))))

(defadvice bury-buffer (before wg-auto-dissociate-buffer)
  "Automatically dissociate the buffer from the current workgroup."
  (when (and wg-buffer-list-filtration-on
             wg-dissociate-buffer-on-bury-buffer)
    (wg-awhen (wg-current-workgroup t)
      (wg-workgroup-dissociate-buffer
       it (or (ad-get-arg 0) (current-buffer)) t))))

(defun wg-enable-all-advice ()
  "Enable and activate all of Workgroups' advice."
  (ad-enable-advice 'switch-to-buffer 'after 'wg-auto-associate-buffer)
  (ad-activate 'switch-to-buffer)
  (ad-enable-advice 'set-window-buffer 'after 'wg-auto-associate-buffer)
  (ad-activate 'set-window-buffer)
  (ad-enable-advice 'bury-buffer 'before 'wg-auto-dissociate-buffer)
  (ad-activate 'bury-buffer))

(defun wg-disable-all-advice ()
  "Disable and deactivate all of Workgroups' advice."
  (ad-disable-advice 'switch-to-buffer 'after 'wg-auto-associate-buffer)
  (ad-deactivate 'switch-to-buffer)
  (ad-disable-advice 'set-window-buffer 'after 'wg-auto-associate-buffer)
  (ad-deactivate 'set-window-buffer)
  (ad-disable-advice 'bury-buffer 'before 'wg-auto-dissociate-buffer)
  (ad-deactivate 'bury-buffer))



;;; mode definition

(defun wg-query-for-save ()
  "Query for save when `wg-dirty' is non-nil."
  (or (not (wg-dirty-p))
      (not (y-or-n-p "Save modified workgroups? "))
      (call-interactively 'wg-save-workgroups)
      t))

(defun wg-save-workgroups-on-emacs-exit ()
  "Conditionally call `wg-query-for-save'.
Added to `kill-emacs-query-functions'."
  (case wg-emacs-exit-save-behavior
    (save (call-interactively 'wg-save-workgroups) t)
    (query (wg-query-for-save) t)
    (nosave t)))

(defun wg-save-workgroups-on-workgroups-mode-exit ()
  "Conditionally call `wg-query-for-save'.
Called when `workgroups-mode' is turned off."
  (case wg-workgroups-mode-exit-save-behavior
    (save (call-interactively 'wg-save-workgroups) t)
    (query (wg-query-for-save) t)
    (nosave t)))

(defun wg-add-or-remove-workgroups-hooks (remove)
  "Add or remove all of Workgroups' hooks, depending on REMOVE."
  (wg-add-or-remove-hooks
   remove
   'kill-emacs-query-functions       'wg-save-workgroups-on-emacs-exit
   'window-configuration-change-hook 'wg-flag-wconfig-change
   'pre-command-hook                 'wg-update-working-wconfig-before-command
   'post-command-hook                'wg-save-undo-after-wconfig-change
   'minibuffer-setup-hook            'wg-unflag-window-config-changed
   'minibuffer-setup-hook            'wg-turn-on-minibuffer-mode
   'minibuffer-exit-hook             'wg-flag-just-exited-minibuffer
   'minibuffer-exit-hook             'wg-turn-off-minibuffer-mode
   'ido-make-buffer-list-hook        'wg-set-ido-buffer-list
   'iswitchb-make-buflist-hook       'wg-set-iswitchb-buffer-list
   'kill-buffer-hook                 'wg-auto-dissociate-buffer-hook))

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
    (wg-add-or-remove-workgroups-hooks nil)
    (wg-enable-all-advice)
    (wg-set-prefix-key)
    (wg-mode-line-add-display)
    (wg-setup-minor-mode-map-entry)
    (run-hooks 'workgroups-mode-hook))
   (t
    (wg-save-workgroups-on-workgroups-mode-exit)
    (wg-add-or-remove-workgroups-hooks t)
    (wg-disable-all-advice)
    (wg-unset-prefix-key)
    (wg-mode-line-remove-display)
    (run-hooks 'workgroups-mode-exit-hook))))

(define-minor-mode workgroups-everywhere
  "Use Workgroups' buffer-list-filters for all buffer selection with `read-buffer'."
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
