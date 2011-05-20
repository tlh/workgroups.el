;;; workgroups.el --- Workgroups For Windows (for Emacs)
;;
;; FIXME: some text got deleted here:
;; Workgroups is an Emacs session manager providing window-configuration
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
;; See the file README.md in this directory
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


;; FIXME: convert alist objects to structs.  Write a defstruct wrapper to
;; namespace-prefix the generated `copy-NAME' and `make-NAME' fns


;;; Code:

(require 'cl)

(eval-when-compile ;; byte-comp warnings begone!
  (require 'ido nil t)
  (require 'iswitchb nil t))



;;; consts

(defconst wg-version "1.0.0" "This version of Workgroups.")



;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- Emacs session manager"
  :group 'convenience
  :version wg-version)

(defcustom workgroups-mode nil
  "Non-nil if Workgroups mode is enabled."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :group 'workgroups
  :type 'boolean)


;; keybinding customization

(defcustom wg-prefix-key (kbd "C-z")
  "Workgroups' prefix key.
Setting this variable requires that `workgroups-mode' be turned
off and then on again to take effect."
  :type 'string
  :group 'workgroups)


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

(defcustom wg-buffer-list-finalization-hook nil
  "Functions in this hook can modify `wg-temp-buffer-list'
arbitrarily, provided its final value is still a list of the
names of live buffer.  Any final adjustments the user wishes to
make to the filtered buffer list before ido/iswitchb get ahold of
it should be made here."
  :type 'hook
  :group 'workgroups)


;; save and load customization

;; TODO: add default directory and file customizations, and a save customization
;; like: <jlf> or maybe have wg-save-file-name be one of 'auto (as above),
;; 'confirm-auto (as above, but overridable), or 'ask (always ask) [10:30]


(defcustom wg-switch-to-first-workgroup-on-find-workgroups-file t
  "Non-nil means switch to the first workgroup in a file when
it's found with `wg-find-workgroups-file'."
  :type 'boolean
  :group 'workgroups)

;; FIXME: add update-all-and-save and query-update-all-and-save, possibly
(defcustom wg-emacs-exit-save-behavior 'query
  "Determines save behavior on Emacs exit.
Possible values:
`nosave'   Exit Emacs without saving changes
`save'     Call `wg-save-workgroups' when there are unsaved changes
`query'    Query the user if there are unsaved changes"
  :type 'symbol
  :group 'workgroups)

(defcustom wg-workgroups-mode-exit-save-behavior 'query
  "Determines save behavior on `workgroups-mode' exit.
Possible values:
`nosave'   Exit `workgroups-mode' without saving changes
`save'     Call `wg-save-workgroups' when there are unsaved changes
`query'    Query the user if there are unsaved changes"
  :type 'symbol
  :group 'workgroups)


;; minibuffer customization

(defcustom wg-no-confirm-on-create-workgroup nil
  "Non-nil means don't request confirmation before creating a new
workgroup when `wg-get-or-create-workgroup' is called with a
string that doesn't name an existing workgroup."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-no-confirm-on-destructive-operation nil
  "Non-nil means don't request confirmation before various
destructive operations, like `wg-reset'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-minibuffer-message-timeout 0.75
  "Bound to `minibuffer-message-timeout' when messaging while the
minibuffer is active."
  :type 'float
  :group 'workgroups)


;; workgroup restoration customization

;; FIXME: major-mode isn't sufficient to determine whether a serdes is defined
;; for a fileless buffer.  It needs to be a predicate on the buffer.
(defcustom wg-mode-serdes-definitions
  '((Info-mode wg-serialize-Info-mode-buffer wg-deserialize-Info-mode-buffer)
    (help-mode wg-serialize-help-mode-buffer wg-deserialize-help-mode-buffer))
  "Alist mapping major-mode symbols to a list containint a
serialization and a deserialization function.
FIXME: docstring this"
  :type 'alist
  :group 'workgroups)

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

(defcustom wg-restore-mark t
  "Non-nil means restore mark data on workgroup restore."
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
buffer-list.  The workgroup may be any workgroup identifier
accepted by `wg-get-workgroup-flexibly', and the initial
buffer-list is a list of live Emacs buffers and/or the names of
live Emacs buffers.  The function should return the filtered list
of buffer objects and/or buffer names.  You may not need these
arguments in your custom filters, but they're necessary for some
of the built-in filters.

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

(defcustom wg-center-rotate-buffer-list-display nil
  "Non-nil means rotate the buffer list display so that the
current buffer is in the center of the list.  This can make it
easier to see the where `wg-previous-buffer' will take you, but
it doesn't look right if the buffer list display is long enough
to wrap in the miniwindow."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-buffer-auto-association-on t
  "Non-nil means buffer auto-association is on.
nil means it's off.  See `wg-buffer-auto-association'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-buffer-auto-association 'soft
  "Specifies the behavior for auto-associating buffers with workgroups.

Buffers can be auto-associated with workgroups whenever they are
made visible in windows (by any function that bottoms out in a
call to `switch-to-buffer' or `set-window-buffer').  This setting
determines whether and how that happens.  The workgroup with
which to associate the buffer is the current workgroup in the
window's frame.

`no-assoc' means don't associate buffers with workgroups.
`soft' means soft associate buffers with workgroups.
`hard' means hard associate buffers with workgroups.

If the value is a function specifier (a function-symbol or a lambda),
it will be `funcall'd to determine whether and how to associate
the buffer.  The function should accept two arguments -- a
workgroup and a buffer -- and should return one of the above
values.

Any other value means don't associate buffers with workgroups.

Also, if the workgroup's `buffer-auto-association' parameter is
non-nil, it overrides this setting.  Allowable values of the
parameter are the same as above, except that nil defers to
`wg-buffer-auto-association's value.  See
`wg-set-workgroup-parameter'."
  :type 'sexp
  :group 'workgroups)

(defcustom wg-dissociate-buffer-on-kill-buffer t
  "Non-nil means dissociate from the current workgroup buffers
killed with `kill-buffer'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-switch-to-buffer t
  "Non-nil means remap `switch-to-buffer' to `wg-switch-to-buffer'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-switch-to-buffer-other-window t
  "Non-nil means remap `switch-to-buffer-other-window' to
`wg-switch-to-buffer-other-window'.  Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-switch-to-buffer-other-frame t
  "Non-nil means remap `switch-to-buffer-other-frame' to
`wg-switch-to-buffer-other-frame'.  Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-kill-buffer t
  "Non-nil means remap `kill-buffer' to `wg-kill-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-display-buffer t
  "Non-nil means remap `display-buffer' to `wg-display-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-insert-buffer t
  "Non-nil means remap `insert-buffer' to `wg-insert-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-next-buffer t
  "Non-nil means remap `next-buffer' to `wg-next-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-previous-buffer t
  "Non-nil means remap `previous-buffer' to `wg-previous-buffer'.
Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-remap-bury-buffer 'bury
  "Non-nil means remap `bury-buffer'.
`banish' means remap `bury-buffer' to `wg-banish-buffer'.
`bury' or other non-nil means remap `bury-buffer' to
`wg-bury-buffer'.  Otherwise, don't remap."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-ido-entry-buffer-replacement-regexp "^ .*Minibuf.*$"
  "Regexp matching the name of a buffer to replace `ido-entry-buffer'.
The regexp should match the name of a live buffer that will never
be a completion candidate under normal circumstances.  You
probably don't want to change this.  See
`wg-get-sneaky-ido-entry-buffer-replacement'."
  :type 'regexp
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

(defcustom wg-mode-line-use-faces t
  "Non-nil means use faces in the mode-line display."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-decor-alist
  '((left-brace          . "(")
    (right-brace         . ")")
    (divider             . ":")
    (hard-associated . "@")
    (soft-associated     . "~")
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

(defvar workgroups-mode-map nil
  "Workgroups Mode's keymap")

(defvar wg-workgroup-set nil
  "Current workgroup-set object.")

(defvar wg-workgroups-mode-minor-mode-map-entry nil
  "Workgroups' minor-mode-map entry.")

(defvar wg-wconfig-kill-ring nil
  "Ring of killed or kill-ring-saved wconfigs.")

(defvar wg-last-message nil
  "Holds the last message Workgroups sent to the echo area.")

(defvar wg-face-abbrevs nil
  "Assoc list mapping face abbreviations to face names.")

(defvar wg-readable-types
  '(integer float cons symbol vector string char-table bool-vector)
  "List of types with readable printed representations.")


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

(defvar wg-current-buffer-command nil
  "Bound to the current buffer command in `wg-with-buffer-list-filters'.")

(defvar wg-current-buffer-list-filter-id nil
  "Bound to the current buffer-list-filter symbol in `wg-with-buffer-list-filters'.")

(defvar wg-minibuffer-contents nil
  "Holds the previous minibuffer contents for re-insertion when
the buffer-list-filter is cycled.")

(defvar wg-ido-method-translations
  `((switch-to-buffer              . selected-window)
    (switch-to-buffer-other-window . other-window)
    (switch-to-buffer-other-frame  . other-frame)
    (kill-buffer                   . kill)
    (insert-buffer                 . insert)
    (display-buffer                . display))
  "Alist mapping buffer commands to ido buffer methods.")

(defvar wg-iswitchb-method-translations
  `((switch-to-buffer              . samewindow)
    (switch-to-buffer-other-window . otherwindow)
    (switch-to-buffer-other-frame  . otherframe)
    (kill-buffer                   . kill)
    (insert-buffer                 . insert)
    (display-buffer                . display))
  "Alist mapping buffer commands to iswitchb buffer methods.")

(defvar wg-buffer-internal-default-buffer nil
  "Bound to `wg-buffer-internal's optional DEFAULT argument for
use by buffer list filtration hooks.")

(defvar wg-temp-buffer-list nil
  "Dynamically bound to the filtered buffer list in
`wg-finalize-buffer-list'.  Functions in
`wg-buffer-list-finalization-hook' should modify this variable.")


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
  "Bound to the `selected-window' by `wg-restore-window'.")

(defvar wg-window-tree-selected-window nil
  "Used during wconfig restoration to hold the selected window.")



;;; faces

;; (defun font-lock-function-name-face () "font-lock-doc-face")
;; default
;; font-lock-keyword-face (defun ())
;; :font-lock-builtin-face
;; "font-lock-string-face"
;; ;; font-lock-comment-delimiter-face
;; `font-lock-constant-face'
;; (defvar font-lock-variable-name-face 1 )


(defmacro wg-defface (face key spec doc &rest args)
  "`defface' wrapper adding a lookup key used by `wg-fontify'."
  (declare (indent 2))
  `(progn
     (pushnew (cons ,key ',face) wg-face-abbrevs :test #'equal)
     (defface ,face ,spec ,doc ,@args)))

;; (wg-defface wg-current-workgroup-face :cur
;;   '((((class color)) (:foreground "white")))
;;   "Face used for the name of the current workgroup in the list display."
;;   :group 'workgroups)

(wg-defface wg-current-workgroup-face :cur
  '((t :inherit font-lock-constant-face :bold nil))
  "Face used for current elements in list displays."
  :group 'workgroups)

;; (wg-defface wg-previous-workgroup-face :prev
;;   '((((class color)) (:foreground "light sky blue")))
;;   "Face used for the name of the previous workgroup in the list display."
;;   :group 'workgroups)

(wg-defface wg-previous-workgroup-face :prev
  '((t :inherit font-lock-keyword-face :bold nil))
  "Face used for the name of the previous workgroup in the list display."
  :group 'workgroups)

;; (wg-defface wg-other-workgroup-face :other
;;   '((((class color)) (:foreground "light slate grey")))
;;   "Face used for the names of other workgroups in the list display."
;;   :group 'workgroups)

(wg-defface wg-other-workgroup-face :other
  '((t :inherit font-lock-string-face :bold nil))
  "Face used for the names of other workgroups in the list display."
  :group 'workgroups)

;; (wg-defface wg-command-face :cmd
;;   '((((class color)) (:foreground "aquamarine")))
;;   "Face used for command/operation strings."
;;   :group 'workgroups)

(wg-defface wg-command-face :cmd
  '((t :inherit font-lock-function-name-face :bold nil))
  "Face used for command/operation strings."
  :group 'workgroups)

;; (wg-defface wg-divider-face :div
;;   '((((class color)) (:foreground "light slate blue")))
;;   "Face used for dividers."
;;   :group 'workgroups)

(wg-defface wg-divider-face :div
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face used for dividers."
  :group 'workgroups)

;; (wg-defface wg-brace-face :brace
;;   '((((class color)) (:foreground "light slate blue")))
;;   "Face used for left and right braces."
;;   :group 'workgroups)

(wg-defface wg-brace-face :brace
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face used for left and right braces."
  :group 'workgroups)

;; (wg-defface wg-message-face :msg
;;   '((((class color)) (:foreground "light sky blue")))
;;   "Face used for messages."
;;   :group 'workgroups)

(wg-defface wg-message-face :msg
  '((t :inherit font-lock-string-face :bold nil))
  "Face used for messages."
  :group 'workgroups)

;; (wg-defface wg-mode-line-face :mode
;;   '((((class color)) (:foreground "light sky blue")))
;;   "Face used for workgroup position and name in the mode-line display."
;;   :group 'workgroups)

(wg-defface wg-mode-line-face :mode
  '((t :inherit font-lock-doc-face :bold nil))
  "Face used for workgroup position and name in the mode-line display."
  :group 'workgroups)

;; (wg-defface wg-filename-face :file
;;   '((((class color)) (:foreground "light sky blue")))
;;   "Face used for filenames."
;;   :group 'workgroups)

(wg-defface wg-filename-face :file
  '((t :inherit font-lock-keyword-face :bold nil))
  "Face used for filenames."
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

  (defun wg-range (start end)
    "Return a list of integers from START up to but not including END."
    (let (accum)
      (dotimes (i (- end start) (nreverse accum))
        (push (+ start i) accum))))

  (defun symcat (&rest symbols-and-strings)
    "Return a new interned symbol by concatenating SYMBOLS-AND-STRINGS."
    (intern (mapconcat (lambda (obj) (if (symbolp obj) (symbol-name obj) obj))
                       symbols-and-strings "")))

  )

(defmacro wg-with-gensyms (syms &rest body)
  "Bind all symbols in SYMS to `gensym's, and eval BODY."
  (declare (indent 1))
  `(let (,@(mapcar (lambda (sym) `(,sym (gensym))) syms)) ,@body))

(defmacro wg-when-boundp (symbols &rest body)
  "When all SYMBOLS are bound, `eval' body."
  (declare (indent 1))
  `(when (and ,@(mapcar (lambda (sym) `(boundp ',sym)) symbols))
     ,@body))

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

(defmacro wg-acond (&rest forms)
  "Anaphoric `cond'."
  (if (not forms) nil
    (wg-dbind ((test . then) . forms) forms
      `(wg-aif ,test (progn ,@then)
         (wg-acond ,@forms)))))

(defmacro wg-aand (&rest args)
  "Anaphoric `and'."
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(wg-aif ,(car args) (aand ,@(cdr args))))))

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

(defun wg-take-until-fail (pred list)
  "Take elements from LISP up to the first element on which PRED fails."
  (let (taken)
    (catch 'result
      (dolist (elt list (nreverse taken))
        (if (funcall pred elt) (push elt taken)
          (throw 'result (nreverse taken)))))))

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
  (wg-insert-elt elt (remove elt list) pos))

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

;; (defmacro wg-fill-keymap (keymap &rest binds)
;;   "Return KEYMAP after defining in it all keybindings in BINDS."
;;   (declare (indent 1))
;;   (wg-with-gensyms (km)
;;     `(let ((,km ,keymap))
;;        ,@(wg-docar (b (wg-partition binds 2))
;;            `(define-key ,km ,@b))
;;        ,km)))

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

(defun wg-get-first-buffer-matching-regexp (regexp &optional buffer-list)
  "Return the first buffer in BUFFER-LIST with a name matching REGEXP.
BUFFER-LIST should contain buffer objects and/or buffer names."
  (wg-get1 (buffer (or buffer-list (buffer-list)))
    (string-match regexp (if (stringp buffer) buffer (buffer-name buffer)))))

;; (defun wg-get-first-buffer-matching-regexp (regexp &optional buffer-list)
;;   "Return the first buffer in BUFFER-LIST with a name matching REGEXP.
;; BUFFER-LIST should contain buffer objects and/or buffer names."
;;   (find t (or buffer-list (buffer-list))
;;         :key (lambda (b) (when (string-match regexp (if (stringp b) b (buffer-name b)))
;;                       t))))
;; (wg-get-first-buffer-matching-regexp wg-ido-entry-buffer-replacement-regexp)


(defun wg-buffer-major-mode (buffer)
  "Return BUFFER's major-mode."
  (with-current-buffer buffer major-mode))

(defun wg-get-buffer (buffer-name &optional file-name)
  "Get a buffer with `get-file-buffer' if FILE-NAME is non-nil.
Otherwise, get a buffer named BUFFER-NAME with `get-buffer'."
  (if file-name (get-file-buffer file-name)
    (get-buffer buffer-name)))

(defmacro wg-buffer-local-setq (buffer var value)
  "`setq' VAR to VALUE while BUFFER is current.
Note that this won't make VAR buffer-local if it isn't already."
  `(with-current-buffer ,buffer (setq ,var ,value)))

(defun wg-defstruct-helper (prefix fn-name obj-name)
  "Helper for `wg-defstruct' that does the function rebinding."
  (let ((oldfn (symcat fn-name "-" prefix "-" obj-name)))
    (fset (symcat prefix "-" fn-name "-" obj-name) (symbol-function oldfn))
    (fmakunbound oldfn)))

(defmacro wg-defstruct (prefix name-form &rest slot-forms)
  "`defstruct' wrapper that namespace-prefixes all generated functions."
  (let ((name (if (consp name-form) (car name-form) name-form)))
    `(progn
       (defstruct ,(symcat prefix "-" name) ,@slot-forms)
       (wg-defstruct-rebinder ',prefix "make" ',name)
       (wg-defstruct-rebinder ',prefix "copy" ',name)
       ',name)))




;;; workgroups utils

(defun wg-is-readable-p (obj)
  "Return non-nil if OBJ's printed representation is readable."
  (memq (type-of obj) wg-readable-types))

(defmacro wg-make-object (type &rest kvps)
  "Return a new Workgroups object of TYPE, and key-value-pairs KVPS."
  (declare (indent 1))
  `(cons ,type (wg-make-alist ,@kvps)))

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

(defun wg-add-face (facekey string)
  "Return a copy of STRING fontified according to FACEKEY.
FACEKEY must be a key in `wg-face-abbrevs'."
  (let ((face (wg-aget wg-face-abbrevs facekey))
        (string  (copy-seq string)))
    (unless face (error "No face with key %s" facekey))
    (if (not wg-use-faces) string
      (put-text-property 0 (length string) 'face face string)
      string)))

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



;;; uid construction

(defun wg-int-to-b36-one-digit (i)
  "Return a character in 0..9 or A..Z from I, and integer 0<=I<32.
Similar to `org-id-int-to-b36-one-digit'."
  (cond ((not (wg-within i 0 36))
         (error "%s out of range" i))
        ((< i 10) (+ ?0 i))
        ((< i 36) (+ ?A i -10))))

(defun wg-int-to-b36 (i &optional length)
  "Return a base 36 string from I.
Similar to `org-id-int-to-b36'."
  (let ((base 36) b36)
    (flet ((add-digit () (push (wg-int-to-b36-one-digit (mod i base)) b36)
                      (setq i (/ i base))))
      (add-digit)
      (while (> i 0) (add-digit))
      (setq b36 (map 'string 'identity b36))
      (if (not length) b36
        (concat (make-string (max 0 (- length (length b36))) ?0) b36)))))

(defun wg-time-to-b36 (&optional time)
  "FIXME: docstring this"
  (apply 'concat
         (mapcar (lambda (time) (wg-int-to-b36 time 4))
                 (or time (current-time)))))

;; (wg-time-to-b36)
;; (prin1-to-string (current-buffer))
;; (wg-int-to-b36 123123 1)
;; (format "%06d is padded on the left with zeros" 123)
;; (let ((org-id-method 'org)) (org-id-new))
;; (current-time)



;;; workgroup-set ops

;; FIXME: call these something other than "workgroup-sets" -- It's too easily
;; confused with other uses of "set", and seems to imply that the workgroup-list
;; isn't ordered.

;; FIXME: factor out the similarities between workgroup-set-parameters and
;; workgroup-parameters

(defun wg-make-workgroup-set ()
  "Return a new workgroup-set."
  (wg-make-object 'workgroup-set
    'version  wg-version))

(defun wg-workgroup-set ()
  "Return `wg-workgroup-set', creating it if necessary."
  (or wg-workgroup-set
      (setq wg-workgroup-set (wg-make-workgroup-set))))

(defun wg-workgroup-set-parameter (workgroup-set parameter &optional default)
  "Return WORKGROUP-SET's value for PARAMETER.
If PARAMETER is not found, return DEFAULT which defaults to nil.
WORKGROUP-SET nil defaults to `wg-workgroup-set'."
  (wg-get (or workgroup-set (wg-workgroup-set)) parameter default))

(defun wg-set-workgroup-set-parameter (workgroup-set parameter value)
  "FIXME: docstring this"
  (let ((workgroup-set (or workgroup-set (wg-workgroup-set))))
    ;; FIXME: move wg-dirty and related into the workgroup-set object?
    (unless wg-nodirty (setq wg-dirty t))
    (wg-set workgroup-set parameter value)
    value))

(defun wg-workgroup-list (&optional noerror)
  "Return the value of `wg-workgroup-set's 'workgroup-list param."
  (or (wg-get (wg-workgroup-set) 'workgroup-list)
      (unless noerror
        (error "No workgroups are defined."))))

(defun wg-workgroup-set-tracked-buffers ()
  "Return the value of `wg-workgroup-set's 'tracked-buffers param."
  (wg-get (wg-workgroup-set) 'tracked-buffers))

(defun wg-set-workgroup-set-tracked-buffers (workgroup-set tracked-buffers)
  "FIXME: docstring this"
  (wg-set-workgroup-set-parameter
   workgroup-set 'tracked-buffers tracked-buffers))

(defvar wg-buffer-uid nil "FIXME: docstring this")
(make-variable-buffer-local 'wg-buffer-uid)

(defun wg-new-buffer-uid ()
  "Return a buffer uid greater than any in `wg-workgroup-set-tracked-buffers'."
  (concat (wg-time-to-b36) "-" (wg-int-to-b36 string-chars-consed)))

(defun wg-buffer-uid (bufobj)
  "Return BUFOBJ's uid."
  (cond ((bufferp bufobj)
         (buffer-local-value 'wg-buffer-uid bufobj))
        ((and (stringp bufobj) (get-buffer bufobj))
         (wg-buffer-uid (get-buffer bufobj)))
        ((wg-wgbuf-p bufobj)
         (wg-get bufobj 'uid))))

(defun wg-get-tracked-buffer-by-uid (uid)
  "FIXME: docstring this"
  (wg-get1 (wgbuf (wg-workgroup-set-tracked-buffers))
    (equal uid (wg-buffer-uid wgbuf))))

;; FIXME: `wg-get-bufobj' should be moved above this point

;; FIXME: remove the error after things stabilize, since the call to
;; `wg-get-tracked-buffer-by-uid' is inefficient
(defun wg-track-buffer (buffer)
  "FIXME: docstring this"
  (wg-acond
   ((wg-buffer-uid buffer)
    (if (wg-get-tracked-buffer-by-uid it) it
      (error "This shouldn't happen.")))
   ((wg-get-bufobj buffer (wg-workgroup-set-tracked-buffers))
    (wg-buffer-local-setq buffer wg-buffer-uid (wg-get it 'uid)))
   (t
    (let* ((wgbuf (wg-serialize-buffer buffer))
           (uid (wg-get wgbuf 'uid)))
      (wg-buffer-local-setq buffer wg-buffer-uid uid)
      (wg-set-workgroup-set-tracked-buffers
       nil (cons wgbuf (wg-workgroup-set-tracked-buffers)))
      uid))))



;;; wconfig construction

(defun wg-mode-serdes-entry (major-mode)
  "Return BUFFER's major-mode's entry in `wg-mode-serdes-definitions'."
  (wg-aget wg-mode-serdes-definitions major-mode))

(defun wg-mode-serializer (major-mode)
  "Return BUFFER's serialization function, or nil."
  (car (wg-mode-serdes-entry major-mode)))

(defun wg-mode-deserializer (major-mode)
  "Return BUFFER's deserialization function, or nil."
  (cadr (wg-mode-serdes-entry major-mode)))

(defun wg-extended-buffer-serialization (buffer &optional major-mode)
  "Return the MAJOR-MODE's extended serialization of BUFFER, if any."
  (wg-awhen (wg-mode-serializer (or major-mode (wg-buffer-major-mode buffer)))
    (funcall it buffer)))

(defun wg-window-point (ewin)
  "Return `point' or :max.  See `wg-restore-point-max'.
EWIN should be an Emacs window object."
  (let ((p (window-point ewin)))
    (if (and wg-restore-point-max (= p (point-max))) :max p)))

;; FIXME: rename all workgroup object parameters to more accurately reflect what
;; they are
(defun wg-serialize-buffer (buffer)
  "Return a serialized buffer from Emacs buffer BUFFER."
  (with-current-buffer buffer
    (wg-make-object 'buffer
      'uid          (wg-new-buffer-uid)
      'bname        (buffer-name)
      'fname        (buffer-file-name)
      'major-mode   major-mode
      'point        (point)
      'mark         (mark)
      'markx        mark-active
      'extended     (wg-extended-buffer-serialization buffer major-mode))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Notes on buffer and window properties:
;;
;; fringes, margins and scroll-bars are properly properties of buffers, but
;; their settings can be forced ephemerally in a window with the set-window-foo
;; functions.
;;
;; window-point is a property of a buffer/window pair, but won't set properly
;; unless the buffer is current -- i.e. (set-window-buffer some-window
;; some-buffer) (set-window-point some-window 0)) won't set some-buffer's point
;; in some-window unless some-buffer is also current.
;;
;; window-start and window-hscroll are properties of buffer/window pairs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FIXME: tracked buffers should be updated in
;; `wg-workgroups-set-tracked-buffers' wg-buffer-list when they're killed.
;; `wg-workgroups-set-tracked-buffers' should also be filtered on `wg-save' to
;; only contain wgbufs that are referred to either by at least one window in any
;; workgroup's base or working wconfig, or by any workgroup's associated-buffers
;; list.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; FIXME: These shouldn't be called `wg-serialize-foo', as they're not
;; serializing anything.  They just make Workgroups objects.

;; (defun wg-serialize-window (window)
;;   "Return a serialized window from Emacs window WINDOW."
;;   (let ((selwin (eq window (selected-window))))
;;     (with-selected-window window
;;       (wg-make-object 'window
;;         'edges      (window-edges window)
;;         'point      (wg-window-point window)
;;         'wstart     (window-start window)
;;         'hscroll    (window-hscroll window)
;;         'sbars      (window-scroll-bars window)
;;         'margins    (window-margins window)
;;         'fringes    (window-fringes window)
;;         'selwin     selwin
;;         'mbswin     (eq window minibuffer-scroll-window)
;;         'buffer     (wg-serialize-buffer (window-buffer window))))))



(defun wg-serialize-window (window)
  "Return a serialized window from Emacs window WINDOW."
  (let ((selwin (eq window (selected-window))))
    (with-selected-window window
      (wg-make-object 'window
        'edges      (window-edges window)
        'point      (wg-window-point window)
        'wstart     (window-start window)
        'hscroll    (window-hscroll window)
        'sbars      (window-scroll-bars window)
        'margins    (window-margins window)
        'fringes    (window-fringes window)
        'selwin     selwin
        'mbswin     (eq window minibuffer-scroll-window)
        'buffer     (wg-track-buffer (window-buffer window))))))

(defun wg-make-wtree-node (dir edges wlist)
  "Return a new wtree node from DIR EDGES and WLIST."
  (wg-make-object 'wtree
    'dir      dir
    'edges    edges
    'wlist    wlist))

(defun wg-serialize-window-tree (window-tree)
  "Return a serialized window-tree from Emacs window tree WINDOW-TREE."
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
  "Return a serialized window-configuration from FRAME or `selected-frame'."
  (let ((frame (or frame (selected-frame))))
    (wg-make-object 'wconfig
      'left      (frame-parameter frame 'left)
      'top       (frame-parameter frame 'top)
      'width     (frame-parameter frame 'width)
      'height    (frame-parameter frame 'height)
      'sbars     (frame-parameter frame 'vertical-scroll-bars)
      'sbwid     (frame-parameter frame 'scroll-bar-width)
      'wtree     (wg-serialize-window-tree (window-tree frame)))))

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

(defun wg-resize-frame-scale-wtree (wconfig &optional frame)
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



;;; non-file buffer serdes functions

;; Add ielm, shell, eshell, term, ansi-term

;; Info-mode serdes

(defun wg-serialize-Info-mode-buffer (buffer)
  "Return `Info-mode's extended serialization of BUFFER."
  (with-current-buffer buffer
    (wg-when-boundp (Info-current-file Info-current-node)
      (list Info-current-file Info-current-node))))

(defun wg-deserialize-Info-mode-buffer ()
  "Deserialize an `Info-mode' buffer."
  (require 'info)
  (wg-aif wg-buffer-extended-serialization
      (apply #'Info-find-node it) (info)))


;; help-mode serdes

(defun wg-serialize-help-mode-buffer (buffer)
  "Return `help-mode's extended serialization of BUFFER."
  (with-current-buffer buffer
    (wg-when-boundp (help-xref-stack-item)
      (wg-take-until-fail 'wg-is-readable-p help-xref-stack-item))))

(defun wg-deserialize-help-mode-buffer ()
  "Deserialize a `help-mode' buffer."
  (require 'help-mode)
  (wg-dbind (method . args) wg-buffer-extended-serialization
    (apply method args)))



;;; wconfig restoration

;; FIXME: docstring these, or eliminate them
(defvar wg-current-wgbuf-uid nil "")
(defvar wg-current-wgbuf nil "")
(defvar wg-buffer-name nil "")
(defvar wg-buffer-file-name nil "")
(defvar wg-buffer-major-mode nil "")
(defvar wg-buffer-mark nil "")
(defvar wg-buffer-mark-active nil "")
(defvar wg-buffer-extended-serialization nil "")
(defvar wg-window-point nil "")
(defvar wg-window-start nil "")
(defvar wg-window-hscroll nil "")

(defun wg-set-selected-window-buffer (buffer)
  "Set `wg-selected-window's buffer to BUFFER."
  (set-window-buffer wg-selected-window buffer))

(defun wg-restore-window-buffer-positions ()
  "FIXME: docstring this"
  (with-current-buffer wg-buffer-name
    (when wg-window-start
      (set-window-start wg-selected-window wg-window-start t))
    (when wg-window-hscroll
      (set-window-hscroll wg-selected-window wg-window-hscroll))
    (when wg-window-point
      (set-window-point
       wg-selected-window
       (cond ((not wg-restore-point) wg-window-start)
             ((eq wg-window-point :max) (point-max))
             (t wg-window-point))))
    (when (and wg-window-start (>= wg-window-start (point-max)))
      (recenter))
    (if (not wg-restore-mark) (deactivate-mark)
      (set-mark wg-buffer-mark)
      (unless wg-buffer-mark-active (deactivate-mark)))))

(defun wg-restore-default-buffer ()
  "Set `wg-selected-window's buffer to `wg-default-buffer'."
  (wg-set-selected-window-buffer wg-default-buffer))

(defun wg-restore-existing-buffer (buffer)
  "Set `wg-selected-window's buffer to BUFFER, and restore various positions."
  (wg-set-selected-window-buffer buffer)
  (wg-restore-window-buffer-positions))

(defun wg-restore-file-buffer ()
  "Restore a file buffer."
  (if (not (file-exists-p wg-buffer-file-name))
      (if wg-barf-on-nonexistent-file
          (error "%S doesn't exist" wg-buffer-file-name)
        (wg-restore-default-buffer))
    (find-file wg-buffer-file-name)
    (rename-buffer wg-buffer-name t)
    (wg-restore-window-buffer-positions)))

(defun wg-restore-extended-buffer (deserializer-fn)
  "Restore a buffer with DESERIALIZER-FN."
  (condition-case err
      (funcall deserializer-fn)
    (error (message "Error deserializing %S: %S" wg-buffer-name err))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FIXME:
;;
;; <jlf> ok, not sure i'll remember the exact sequence of events but it was
;;       something like this:                                              [10:47]
;; <jlf> first, i had ~/.emacs.d/google.el and ~/.emacs.d/erbot/contrib/google.el
;;       open in side by side windows with the "irc" workgroup current, the only
;;       wg existent.  i finished an ediff-buffers session and went looking for
;;       wg-create-wg-from-current-window-layout in C-h m but didn't see it, so i
;;       went ahead and did wg-create-workgroup thinking that might do it, but it
;;       created a new wg with a single *scratch* window.  at the same time it
;;       gave an                                                           [10:50]
;; <jlf> error about buffer google.el:
;; <jlf> Mark saved where search started
;; <jlf> Switched:  ( < 0: irc > | -<{ 1: google }>- )
;; <jlf> Created: google  ( < 0: irc > | -<{ 1: google }>- )
;; <jlf> save-current-buffer: No buffer named google.el
;; <jlf>
;; <jlf> (from *Messages*)
;; <jlf> so the apparent bug is that it seems to have been confused by the
;;       existence of two identically named google.el buffers visiting different
;;       files                                                             [10:51]
;; <jlf> i also use uniquify.el if that matters
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; FIXME: Add restore-extended-serialization customization option

(defun wg-restore-buffer (wgbuf)
  "Restore WGBUF and return it."
  (wg-bind-params wgbuf
      ((wg-buffer-name bname)
       (wg-buffer-file-name fname)
       (wg-buffer-major-mode major-mode)
       (wg-buffer-mark mark)
       (wg-buffer-mark-active markx)
       (wg-buffer-extended-serialization extended)
       (wg-current-wgbuf-uid uid))
    (wg-acond ((wg-get-buffer wg-buffer-name wg-buffer-file-name)
               (wg-restore-existing-buffer it))
              ((wg-mode-deserializer wg-buffer-major-mode)
               (wg-restore-extended-buffer it))
              (wg-buffer-file-name
               (wg-restore-file-buffer))
              (t (wg-restore-default-buffer)))
    (let ((cb (window-buffer wg-selected-window)))
      (wg-aif (buffer-local-value 'wg-buffer-uid cb)
          (unless (equal it wg-current-wgbuf-uid)
            (error "This shouldn't happen"))
        (wg-buffer-local-setq
         cb wg-buffer-uid wg-current-wgbuf-uid)))))

;; (buffer-local-value 'wg-buffer-uid (current-buffer))
;; (wg-workgroup-set-tracked-buffers)

;; (defun wg-restore-window (wg-window)
;;   "Restore WG-WINDOW in `selected-window'."
;;   (wg-bind-params wg-window
;;       ((wg-current-wgbuf buffer)
;;        (wg-window-point point)
;;        (wg-window-start wstart)
;;        (wg-window-hscroll hscroll)
;;        sbars fringes margins selwin mbswin)
;;     (let ((wg-selected-window (selected-window)))
;;       (wg-restore-buffer wg-current-wgbuf)
;;       (when selwin
;;         (setq wg-window-tree-selected-window wg-selected-window))
;;       (when (and wg-restore-minibuffer-scroll-window mbswin)
;;         (setq minibuffer-scroll-window wg-selected-window))
;;       (wg-dbind (width cols vtype htype)
;;           (if wg-restore-scroll-bars sbars '(nil nil nil nil))
;;         (set-window-scroll-bars wg-selected-window width vtype htype))
;;       (wg-dbind (left-width right-width outside-margins)
;;           (if wg-restore-fringes fringes '(nil nil nil))
;;         (set-window-fringes
;;          wg-selected-window left-width right-width outside-margins))
;;       (wg-dbind (left-width . right-width)
;;           (if wg-restore-margins margins '(nil . nil))
;;         (set-window-margins wg-selected-window left-width right-width)))))

;; FIXME: fix varnames here to reflect buffer uids
;; FIXME: fringes, margins, scroll-bars, etc. shouldn't be restored with the
;; buffer, not the window
(defun wg-restore-window (wg-window)
  "Restore WG-WINDOW in `selected-window'."
  (wg-bind-params wg-window
      ((wg-window-point point)
       (wg-window-start wstart)
       (wg-window-hscroll hscroll)
       sbars fringes margins selwin mbswin)
    (let ((wg-selected-window (selected-window))
          (wg-current-wgbuf (wg-get-tracked-buffer-by-uid
                             (wg-get wg-window 'buffer))))
      (wg-restore-buffer wg-current-wgbuf)
      (when selwin
        (setq wg-window-tree-selected-window wg-selected-window))
      (when (and wg-restore-minibuffer-scroll-window mbswin)
        (setq minibuffer-scroll-window wg-selected-window))
      (wg-dbind (width cols vtype htype)
          (if wg-restore-scroll-bars sbars '(nil nil nil nil))
        (set-window-scroll-bars wg-selected-window width vtype htype))
      (wg-dbind (left-width right-width outside-margins)
          (if wg-restore-fringes fringes '(nil nil nil))
        (set-window-fringes
         wg-selected-window left-width right-width outside-margins))
      (wg-dbind (left-width . right-width)
          (if wg-restore-margins margins '(nil . nil))
        (set-window-margins wg-selected-window left-width right-width)))))

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
          (window-min-height wg-window-min-height)
          (wg-window-tree-selected-window nil))
      (delete-other-windows)
      (inner wg-window-tree)
      (wg-awhen wg-window-tree-selected-window (select-window it)))))

(defun wg-restore-wconfig (wconfig)
  "Restore WCONFIG in `selected-frame'."
  (wg-barf-on-active-minibuffer)
  (let ((wg-buffer-auto-association-on nil)
        (frame (selected-frame)))
    (wg-bind-params wconfig (left top sbars sbwid)
      (when (and wg-restore-frame-position left top)
        (set-frame-position frame left top))
      (let ((wtree (wg-resize-frame-scale-wtree wconfig frame)))
        (wg-restore-window-tree
         (if (not (wg-morph-p)) wtree (wg-morph wtree))))
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

(defun wg-morph (to &optional from)
  "Morph from wtree FROM to wtree TO.
Assumes both FROM and TO fit in `selected-frame'."
  (let ((from (or from (wg-serialize-window-tree (window-tree))))
        (wg-morph-hsteps
         (wg-morph-determine-steps wg-morph-hsteps wg-morph-terminal-hsteps))
        (wg-morph-vsteps
         (wg-morph-determine-steps wg-morph-vsteps wg-morph-terminal-vsteps))
        (wg-flag-wconfig-changes nil)
        (wg-restore-scroll-bars nil)
        (wg-restore-fringes nil)
        (wg-restore-margins nil)
        (wg-restore-point nil)
        (wg-restore-mark nil)
        (truncate-partial-width-windows
         wg-morph-truncate-partial-width-windows)
        (watchdog 0))
    (wg-until (wg-equal-wtrees from to)
      (condition-case err
          (if (> (incf watchdog) wg-morph-max-steps)
              (error "`wg-morph-max-steps' exceeded")
            (setq from (wg-normalize-wtree (wg-morph-dispatch from to)))
            (wg-restore-window-tree from)
            (redisplay))
        (error (wg-dbind (sym data) err
                 (unless (or (and (stringp data)
                                  (string-match "too small" data))
                             (not wg-morph-no-error))
                   (signal sym data))))))
    to))



;;; global error wrappers
;; FIXME: This no longer needs to be a section.  There used to be a number of
;; things here, but now it's just `wg-visited-file-name'.

(defun wg-visited-file-name (&optional noerror)
  "Return `wg-visited-file-name' or error."
  (or wg-visited-file-name
      (unless noerror
        (error "Workgroups isn't visiting a file"))))



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

;; FIXME: dispatch to type-specific functions to speed this up
;; FIXME: needs a smarter heuristic to determine wgbuf->buffer reference
(defun wg-get-bufobj (bufobj bufobj-list)
  "Return the bufobj in BUFOBJ-LIST corresponding to BUFOBJ, or nil.
BUFOBJ should be a Workgroups buffer, an Emacs buffer, or an
Emacs buffer-name string."
  (wg-get1 (bufobj2 bufobj-list)
    (wg-bufobjs-equal bufobj bufobj2)))




;;; workgroup utils

(defun wg-make-workgroup (&rest key-value-pairs)
  "Return a new workgroup with KEY-VALUE-PAIRS."
  `(workgroup ,@(wg-docar (kvp (wg-partition key-value-pairs 2))
                  (cons (car kvp) (cadr kvp)))))

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
  (let* ((workgroup (wg-get-workgroup-flexibly workgroup))
         (alist (cdr workgroup)))
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

;; FIXME: convert this to return b36 uids
(defun wg-new-workgroup-uid ()
  "Return a uid greater than any in `wg-workgroup-list'."
  (wg-aif (wg-workgroup-uids t) (1+ (apply 'max it)) 0))


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

;; FIXME: change soft/hard to weakly/strongly

(defun wg-workgroup-associated-buffer-entries (workgroup)
  "FIXME: docstring this"
  (wg-workgroup-parameter workgroup 'associated-buffer-entries))

(defun wg-set-workgroup-associated-buffer-entries (workgroup entries)
  "Set WORKGROUP's `strongly-associated-buffer-uids' to ENTRIES."
  (wg-set-workgroup-parameter workgroup 'associated-buffer-entries entries))

(defun wg-workgroup-associated-buffers (workgroup)
  "FIXME: docstring this"
  (mapcar (lambda (entry) (wg-get-tracked-buffer-by-uid (car entry)))
          (wg-workgroup-associated-buffer-entries workgroup)))

(defun wg-workgroup-get-wgbuf (workgroup bufobj &optional noerror)
  "Return WORKGROUP's associated wgbuf corresponding to BUFOBJ."
  (or (wg-get-bufobj bufobj (wg-workgroup-associated-buffers workgroup))
      (unless noerror
        (error "%S is not associated with %S"
               (wg-bufobj-name bufobj)
               (wg-workgroup-name workgroup)))))

(defun wg-get-tracked-buffer (bufobj)
  "FIXME: docstring this"
  (wg-get-bufobj bufobj (wg-workgroup-set-tracked-buffers)))

(defun wg-workgroup-associated-buffer-entry (workgroup bufobj)
  "FIXME: docstring this"
  (assoc (wg-buffer-uid (wg-get-tracked-buffer bufobj))
         (wg-workgroup-associated-buffer-entries workgroup)))

(defun wg-workgroup-buffer-association-type (workgroup bufobj)
  "Return BUFOBJ's association-type in WORKGROUP, or nil if unassociated."
  (wg-awhen (wg-workgroup-associated-buffer-entry workgroup bufobj)
    (if (cdr it) 'soft 'hard)))

(defun wg-hard-associated-buffer-entry-p (entry)
  "Return non-nil if ENTRY is hard associated."
  (not (cdr entry)))

(defun wg-soft-associated-buffer-entry-p (entry)
  "Return non-nil if ENTRY is soft associated."
  (cdr entry))

;; FIXME: clean this up
(defun wg-workgroup-associate-buffer (workgroup bufobj &optional soft)
  "Add BUFOBJ to WORKGROUP. FIXME: docstring this"
  (when (bufferp bufobj) (wg-track-buffer bufobj))
  (let* ((uid (wg-buffer-uid bufobj))
         (entries (wg-workgroup-associated-buffer-entries workgroup))
         (entry (assoc uid entries)))
    (unless (and entry (eq soft (cdr entry)))
      (wg-set-workgroup-associated-buffer-entries
       workgroup (acons uid soft (if entry (remove entry entries) entries)))
      uid)))

;; FIXME: clean this up
(defun wg-workgroup-dissociate-buffer (workgroup bufobj)
  "Dissociate BUFOBJ from WORKGROUP.
See `wg-get-bufobj' for allowable values for BUFOBJ."
  (when (bufferp bufobj) (wg-track-buffer bufobj))
  (let ((uid (wg-buffer-uid bufobj))
        (entries (wg-workgroup-associated-buffer-entries workgroup)))
    (wg-awhen (assoc uid entries)
      (wg-set-workgroup-associated-buffer-entries
       workgroup (remove it entries))
      uid)))

(defun wg-workgroup-live-buffers (workgroup &optional initial names)
  "Return a list of WORKGROUP's live associated buffers."
  (let ((assoc-bufs (wg-workgroup-associated-buffers workgroup)))
    (wg-filter-map (lambda (buf) (when (wg-get-bufobj buf assoc-bufs) buf))
                   (or initial (wg-interesting-buffers names)))))

(defun wg-workgroup-cycle-buffer-association-type (workgroup bufobj)
  "Cycle the BUFOBJ's association type in WORKGROUP.
If it's hard-associated with the workgroup, mark it as soft-associated.
If it's soft-associated with the workgroup, dissociate it from the workgroup.
If it's not associated with the workgroup, mark it as hard-associated."
  (case (wg-workgroup-buffer-association-type workgroup bufobj)
    (hard (wg-workgroup-associate-buffer workgroup bufobj t))
    (soft (wg-workgroup-dissociate-buffer workgroup bufobj))
    (otherwise (wg-workgroup-associate-buffer workgroup bufobj))))

(defun wg-workgroup-purge-soft-associated-buffers (workgroup)
  "Remove from WORKGROUP all wgbufs with `association-type' `soft'."
  (wg-set-workgroup-associated-buffer-entries
   workgroup (remove-if 'wg-soft-associated-buffer-entry-p
                        (wg-workgroup-associated-buffer-entries workgroup))))

(defun wg-auto-dissociate-buffer-hook ()
  "`kill-buffer-hook' that automatically dissociates buffers from workgroups."
  (when wg-dissociate-buffer-on-kill-buffer
    (wg-awhen (wg-current-workgroup t)
      (wg-workgroup-dissociate-buffer it (current-buffer)))))


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

(defun wg-filtered-buffer-list (&optional names workgroup bfl-id initial)
  "Return a filtered buffer-list from NAMES, WORKGROUP, BLF-ID and INITIAL.
NAMES non-nil means return a list of buffer-names instead of buffer objects.
WORKGROUP non-nil should be any workgroup identifier accepted by
`wg-get-workgroup-flexibly'.
BLF-ID non-nil should be the identifier of a defined buffer-list-filter.
It defaults to `wg-get-buffer-list-filter-val'.
INITIAL non-nil should be an initial buffer-list to filter.  It defaults to
`wg-interesting-buffers'."
  (let ((bfl-id (wg-get-buffer-list-filter-id-flexibly bfl-id)))
    (condition-case err
        (mapcar (lambda (buf)
                  (let ((buffer (get-buffer buf)))
                    (if names (buffer-name buffer) buffer)))
                (funcall (wg-get-buffer-list-filter-val bfl-id 'constructor)
                         (wg-get-workgroup-flexibly workgroup)
                         (or initial (wg-interesting-buffers t))))
      (error
       (signal (car err)
               (format "Error in buffer-list-filter `%S's constructor:\n%S"
                       bfl-id (cadr err)))))))



;; buffer-list filters

(defun wg-buffer-list-filter-all (workgroup initial)
  "Return all buffers in INITIAL."
  initial)

(defun wg-buffer-list-filter-associated (workgroup initial)
  "Return only those buffers associated with WORKGROUP."
  (wg-workgroup-live-buffers workgroup initial))

(defun wg-buffer-list-filter-unassociated (workgroup initial)
  "Return only those buffer unassociated with WORKGROUP."
  (let ((buffers (wg-workgroup-live-buffers workgroup initial)))
    (remove-if (lambda (buf) (wg-get-bufobj buf buffers)) initial)))


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

(defun wg-buffer-list-filter-order (workgroup command)
  "Return WORKGROUP's buffer-list-filter order for COMMAND, or a default."
  (let ((bso (wg-workgroup-parameter workgroup 'buffer-list-filter-order-alist)))
    (or (wg-aget bso  command)
        (wg-aget bso 'default)
        (wg-aget wg-buffer-list-filter-order-alist  command)
        (wg-aget wg-buffer-list-filter-order-alist 'default))))

(defmacro wg-prior-mapping (mode command)
  "Return whatever COMMAND would call if MODE wasn't on."
  `(or (let (,mode) (command-remapping ,command)) ,command))

(defun wg-filter-buffer-list-p ()
  "Return the current workgroup when buffer-list-filters are on."
  (and workgroups-mode wg-buffer-list-filtration-on (wg-current-workgroup t)))

(defmacro wg-with-buffer-list-filters (command &rest body)
  "Establish buffer-list-filter context for buffer command COMMAND, and eval BODY.
Binds `wg-current-workgroup', `wg-current-buffer-command' and
`wg-current-buffer-list-filter-id' in BODY."
  (declare (indent 1))
  (wg-with-gensyms (order status)
    `(let* ((wg-current-workgroup (wg-current-workgroup t))
            (wg-current-buffer-command ,command)
            (wg-minibuffer-contents nil)
            (,order (wg-buffer-list-filter-order wg-current-workgroup ,command)))
       (catch 'wg-result
         (while 'your-mom
           (let* ((wg-current-buffer-list-filter-id (car ,order))
                  (,status (catch 'wg-action (list 'done (progn ,@body)))))
             (case (car ,status)
               (done (throw 'wg-result (cadr ,status)))
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

(defun wg-current-workgroup-p (workgroup &optional noerror frame)
  "Return t when WORKGROUP is the current workgroup, nil otherwise."
  (wg-awhen (wg-current-workgroup noerror frame)
    (wg-workgroups-eq workgroup it)))

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



;;; workgroup restoration

(defun wg-restore-workgroup-associated-buffers-internal (workgroup)
  "Restore all the buffers associated with WORKGROUP that can be restored."
  (save-window-excursion
    (wg-filter-map 'wg-restore-buffer
                   (wg-workgroup-associated-buffers workgroup))))

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
    (setq wg-dirty t)
    workgroup))

(defun wg-add-workgroup (workgroup &optional pos)
  "Add WORKGROUP to `wg-workgroup-list'.
If a workgroup with the same name exists, overwrite it."
  (wg-awhen (wg-get-workgroup-by-name (wg-workgroup-name workgroup) t)
    (unless pos (setq pos (position it (wg-workgroup-list))))
    (wg-delete-workgroup it))
  (wg-set-workgroup-uid workgroup (wg-new-workgroup-uid))
  (wg-set-workgroup-list (wg-insert-elt workgroup (wg-workgroup-list t) pos))
  (setq wg-dirty t)
  workgroup)

(defun wg-check-and-add-workgroup (workgroup)
  "Add WORKGROUP to `wg-workgroup-list'.
Query to overwrite if a workgroup with the same name exists."
  (let ((name (wg-workgroup-name workgroup)))
    (when (wg-get-workgroup-by-name name t)
      (unless (or wg-no-confirm-on-destructive-operation
                  (y-or-n-p (format "%S exists. Overwrite? " name)))
        (error "Cancelled"))))
  (wg-add-workgroup workgroup))

(defun wg-make-and-add-workgroup (name)
  "Create a workgroup named NAME and add it with `wg-check-and-add-workgroup'."
  (wg-check-and-add-workgroup
   (wg-make-workgroup
    'name name
    'base-wconfig (if (wg-current-workgroup t)
                      (wg-make-blank-wconfig)
                    (wg-serialize-frame)))))

(defun wg-get-or-create-workgroup (workgroup)
  "Return the workgroup identified by WORKGROUP.
If WORKGROUP is a string that isn't the name of an
existing workgroup, offer to create it."
  (or (wg-get-workgroup-flexibly workgroup t)
      (if (stringp workgroup)
          (when (or wg-no-confirm-on-create-workgroup
                    (y-or-n-p (format "%S doesn't exist.  Create it? "
                                      workgroup)))
            (wg-make-and-add-workgroup workgroup))
        (wg-get-workgroup-flexibly workgroup))))

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

;; FIXME: add mouse-over text explaining mode-line elements

(defun wg-mode-line-decor (decor-symbol)
  "Return DECOR-SYMBOL's decoration string in `wg-mode-line-decor-alist'."
  (wg-aget wg-mode-line-decor-alist decor-symbol))

(defun wg-mode-line-buffer-association-indicator (workgroup)
  "Return a string indicating `current-buffer's association-type in WORKGROUP."
  (wg-mode-line-decor
   (case (wg-workgroup-buffer-association-type workgroup (current-buffer))
     (hard    'hard-associated)
     (soft      'soft-associated)
     (otherwise 'unassociated))))

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
                (:mode "no workgroups")
                (:div (wg-mode-line-decor 'right-brace)))))))

(defun wg-add-mode-line-display ()
  "Add Workgroups' mode-line format to `mode-line-format'."
  (unless (assq 'wg-mode-line-display-on mode-line-format)
    (let ((format '(wg-mode-line-display-on (:eval (wg-mode-line-string))))
          (pos (1+ (position 'mode-line-position mode-line-format))))
      (set-default 'mode-line-format
                   (wg-insert-elt format mode-line-format pos))
      (force-mode-line-update))))

(defun wg-remove-mode-line-display ()
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

;; FIXME: add jlf's `wg-display-max-lines' suggestion to chop long display
;; strings at max-line and element-name boundaries

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

(defun wg-buffer-list-filter-prompt (prompt &optional workgroup blf-id)
  "Return a prompt string from PROMPT indicating WORKGROUP and BLF-ID."
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
    (ido (wg-when-boundp (ido-cur-list) ido-cur-list))
    (iswitchb (wg-when-boundp (iswitchb-buflist) iswitchb-buflist))
    (fallback (list minibuffer-default))))

(defun wg-current-match (&optional read-buffer-mode)
  "Return READ-BUFFER-MODE's current match."
  (car (wg-current-matches read-buffer-mode)))

(defun wg-set-current-matches (match-list &optional read-buffer-mode)
  "Set READ-BUFFER-MODE's current matches, and flag a rescan."
  (case (or read-buffer-mode (wg-read-buffer-mode))
    (ido
     (wg-when-boundp (ido-cur-list)
       (setq ido-cur-list match-list ido-rescan t)))
    (iswitchb
     (wg-when-boundp (iswitchb-buflist)
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

(defun wg-buffer-internal (command &optional prompt default)
  "Buffer list filtration interface to the current remapping of COMMAND.
PROMPT non-nil specifies the prompt.
DEFAULT non-nil specifies the first completion candidate."
  (if (not (wg-filter-buffer-list-p))
      (call-interactively (wg-prior-mapping workgroups-mode command))
    (wg-with-buffer-list-filters command
      (let ((wg-buffer-internal-default-buffer default))
        (ecase (wg-read-buffer-mode)
          (ido
           (ido-buffer-internal
            (wg-aget wg-ido-method-translations command) nil
            (wg-buffer-list-filter-prompt prompt)
            nil wg-minibuffer-contents))
          (iswitchb
           (wg-iswitchb-internal
            (wg-aget wg-iswitchb-method-translations command)
            (wg-buffer-list-filter-prompt prompt)
            nil wg-minibuffer-contents))
          (fallback
           (let (read-buffer-function)
             (call-interactively command))))
        (wg-message (wg-buffer-command-display))))))

(defun wg-get-sneaky-ido-entry-buffer-replacement (&optional regexp)
  "Return a live buffer to replace `ido-entry-buffer'.
This is a workaround for an ido misfeature.  IMHO, ido should
respect the value of `ido-temp-list' after
`ido-make-buffer-list-hook' has been run, since the user's
preference for the final value of `ido-temp-list', if any, has
been expressed in that hook.  But ido conditionally rotates the
first match to the end after the hook has been run, based on the
value of `ido-entry-buffer'.  So as a workaround, set
`ido-entry-buffer' to a buffer that will never be a completion
candidate under normal circumstances.  See
`wg-ido-entry-buffer-replacement-regexp'."
  (wg-get-first-buffer-matching-regexp
   (or regexp wg-ido-entry-buffer-replacement-regexp)))

(defun wg-adjust-buffer-list-default (buflist &optional default)
  "Adjust BUFLIST based on DEFAULT.
DEFAULT is the default completion candidate, and defaults to
`wg-buffer-internal-default-buffer'.  Non-nil, this gets placed
at the beginning of BUFLIST.  Otherwise rotate BUFLIST."
  (wg-aif (or default wg-buffer-internal-default-buffer)
      (wg-move-elt it buflist 0)
    (wg-rotate-list buflist)))

(defun wg-finalize-buffer-list (buflist)
  "Run `wg-buffer-list-finalization-hook' and return
`wg-temp-buffer-list'."
  (let ((wg-temp-buffer-list buflist))
    (run-hooks 'wg-buffer-list-finalization-hook)
    wg-temp-buffer-list))

(defun wg-set-buffer-list-symbol (symbol)
  "Set SYMBOL to the filtered buffer-list."
  (when (and wg-current-buffer-list-filter-id (boundp symbol))
    (set symbol
         (wg-finalize-buffer-list
          (wg-adjust-buffer-list-default
           (wg-filtered-buffer-list t))))))

(defun wg-set-ido-buffer-list ()
  "Set `ido-temp-list' with `wg-set-buffer-list-symbol'.
Added to `ido-make-buffer-list-hook'."
  (wg-set-buffer-list-symbol 'ido-temp-list)
  (wg-when-boundp (ido-entry-buffer)
    (setq ido-entry-buffer (wg-get-sneaky-ido-entry-buffer-replacement))))

(defun wg-set-iswitchb-buffer-list ()
  "Set `iswitchb-temp-buflist' with `wg-set-buffer-list-symbol'.
Added to `iswitchb-make-buflist-hook'."
  (wg-set-buffer-list-symbol 'iswitchb-temp-buflist))



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
(defun wg-read-workgroup-name (&optional require-match)
  "Read a workgroup with `wg-completing-read'."
  (wg-completing-read
   "Workgroup: " (wg-workgroup-names) nil require-match nil nil
   (wg-awhen (wg-current-workgroup t) (wg-workgroup-name it))))

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
  (interactive (list (wg-read-workgroup-name)))
  (let ((workgroup (wg-get-or-create-workgroup workgroup))
        (current (wg-current-workgroup t)))
    (unless (or (not (eq workgroup current)) noerror)
      (error "Already on: %s" (wg-workgroup-name current)))
    (wg-restore-workgroup workgroup)
    (wg-set-previous-workgroup current)
    (wg-set-current-workgroup workgroup)
    (run-hooks 'wg-switch-to-workgroup-hook)
    (wg-fontified-message
      (:cmd "Switched:  ")
      (wg-workgroup-list-display))))

(defun wg-switch-to-workgroup-other-frame (workgroup &optional n)
  "Switch to WORKGROUP in the frame N places cyclically from `selected-frame'.
Use `current-prefix-arg' for N if non-nil.  Otherwise N defaults to 1."
  (interactive (list (wg-read-workgroup-name) current-prefix-arg))
  (with-selected-frame (wg-cyclic-nth-from-frame (or n 1))
    (wg-switch-to-workgroup workgroup)))

(defun wg-switch-to-workgroup-at-index (index)
  "Switch to the workgroup at INDEX in `wg-workgroup-list'."
  (interactive (list (or current-prefix-arg (wg-read-workgroup-index))))
  (let ((wl (wg-workgroup-list)))
    (wg-switch-to-workgroup
     (or (nth index wl) (error "There are only %d workgroups" (length wl))))))

(macrolet
    ((define-wg-switch-to-workgroup-at-index-range (num)
       `(progn
          ,@(wg-docar (i (wg-range 0 num))
              `(defun ,(intern (format "wg-switch-to-workgroup-at-index-%d" i)) ()
                 ,(format "Switch to the workgroup at index %d." i)
                 (interactive)
                 (wg-switch-to-workgroup-at-index ,i))))))
  (define-wg-switch-to-workgroup-at-index-range 10))

;; FIXME: If there's only one workgroup, give a more informative error msg.
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
  (wg-switch-to-workgroup (wg-make-and-add-workgroup name))
  (wg-fontified-message
    (:cmd "Created: ")
    (:cur name) "  "
    (wg-workgroup-list-display)))

(defun wg-clone-workgroup (workgroup name)
  "Create and add a clone of WORKGROUP named NAME.
Keep in mind that only WORKGROUP's top-level alist structure is
copied, so destructive operations on the keys or values of
WORKGROUP will be reflected in the clone, and vice-versa.  Be
safe -- don't mutate them."
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
  (unless (or wg-no-confirm-on-destructive-operation
              (y-or-n-p "Really delete all other workgroups? "))
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
    (let* ((cur (current-buffer))
           (next (or (wg-cyclic-nth-from-elt cur buffer-list (if prev -1 1))
                     (car buffer-list))))
      (unless (eq cur next)
        (switch-to-buffer next)
        (unless prev (bury-buffer cur))
        next))))

(defun wg-next-buffer (&optional prev)
  "Switch to the next buffer in Workgroups' filtered buffer list.
In the post-command message the current buffer is rotated to the
middle of the list to more easily see where `wg-previous-buffer'
will take you."
  (interactive)
  (let ((command (if prev 'previous-buffer 'next-buffer)))
    (if (not (wg-filter-buffer-list-p))
        (call-interactively (wg-prior-mapping workgroups-mode command))
      (wg-with-buffer-list-filters command
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
      (wg-next-buffer-internal (wg-filtered-buffer-list))
      (bury-buffer buffer-or-name)
      (wg-message (wg-buffer-command-display)))))

(defun wg-banish-buffer (&optional buffer-or-name)
  "Dissociate BUFFER-OR-NAME from the current workgroup, and bury it."
  (interactive)
  (let ((buffer (or buffer-or-name (current-buffer))))
    (wg-workgroup-dissociate-buffer (wg-current-workgroup) buffer)
    (wg-bury-buffer buffer)))

;; (defun wg-associate-buffer-with-workgroup (workgroup buffer &optional soft)
;;   "Hard-associate BUFFER with WORKGROUP.  SOFT non-nil means soft-associate."
;;   (interactive (list nil (current-buffer) current-prefix-arg))
;;   (let ((bname (buffer-name (get-buffer buffer)))
;;         (wgname (wg-workgroup-name workgroup)))
;;     (if (wg-workgroup-update-or-associate-buffer
;;          workgroup buffer (if soft 'soft 'hard))
;;         (wg-message "Updated %S in %s" bname wgname)
;;       (wg-message "%s-associated %S with %s"
;;                   (if soft "Soft" "Hard") bname wgname))))

;; (defun wg-associate-buffer-with-workgroup (workgroup buffer &optional soft)
;;   "Hard-associate BUFFER with WORKGROUP.  SOFT non-nil means soft-associate."
;;   (interactive (list nil (current-buffer) current-prefix-arg))
;;   (let ((bname (buffer-name (get-buffer buffer)))
;;         (wgname (wg-workgroup-name workgroup)))
;;     (if (wg-workgroup-associate-buffer workgroup buffer soft)
;;         (wg-message "Updated %S in %s" bname wgname)
;;       (wg-message "%s-associated %S with %s"
;;                   (if soft "Soft" "Hard") bname wgname))))

;; (defun wg-associate-buffer-with-workgroup (workgroup buffer &optional soft)
;;   "Hard-associate BUFFER with WORKGROUP.  SOFT non-nil means soft-associate."
;;   (interactive (list nil (current-buffer) current-prefix-arg))
;;   (when (wg-workgroup-associate-buffer workgroup buffer soft)
;;     (wg-message "%s-associated %S with %s"
;;                 (if soft "Soft" "Hard")
;;                 (buffer-name (get-buffer buffer))
;;                 (wg-workgroup-name workgroup))))

(defun wg-associate-buffer-with-workgroup (workgroup buffer &optional soft)
  "Hard-associate BUFFER with WORKGROUP.  SOFT non-nil means soft-associate."
  (interactive (list nil (current-buffer) current-prefix-arg))
  (let ((bname (buffer-name (get-buffer buffer)))
        (wgname (wg-workgroup-name workgroup)))
    (if (wg-workgroup-associate-buffer workgroup buffer soft)
        (wg-message "%s-associated %S with %s"
                    (if soft "Soft" "Hard") bname wgname)
      (wg-message "%S is already associated with %s" bname wgname))))

(defun wg-dissociate-buffer-from-workgroup (workgroup buffer)
  "Dissociate BUFFER from WORKGROUP."
  (interactive (list nil (current-buffer)))
  (wg-message
   (if (wg-workgroup-dissociate-buffer workgroup buffer)
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
              (hard " hard-associated with ")
              (soft " soft-associated with ")
              (otherwise " dissociated from ")))
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-purge-soft-associated-buffers (workgroup)
  "Dissociate from the current workgroup soft-associated buffers."
  (interactive (list nil))
  (wg-workgroup-purge-soft-associated-buffers workgroup)
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
  (unless (or force wg-no-confirm-on-destructive-operation
              (y-or-n-p "Are you sure? "))
    (error "Canceled"))
  (mapc 'wg-reset-frame (frame-list))
  (setq wg-workgroup-set nil
        wg-visited-file-name nil
        wg-dirty nil)
  (wg-fontified-message
    (:cmd "Reset: ")
    (:msg "Workgroups")))



;;; file commands

;; FIXME: decide whether to use pickel for serialization
(defun wg-save-workgroups (file &optional force)
  "Save workgroups to FILE.
Called interactively with a prefix arg, or if `wg-visited-file-name'
is nil, read a filename.  Otherwise use `wg-visited-file-name'."
  (interactive
   (list (if (or current-prefix-arg (not (wg-visited-file-name t)))
             (read-file-name "File to save in: ") (wg-visited-file-name))))
  (if (and (not force) (not (wg-dirty-p)))
      (wg-message "(No workgroups need to be saved)")
    (setq wg-visited-file-name file)
    (wg-update-workgroup-working-wconfig nil)
    (wg-write-sexp-to-file (wg-workgroup-set) file)
    (wg-mark-everything-clean)
    (wg-fontified-message (:cmd "Wrote: ") (:file file))))

(defun wg-query-for-save ()
  "Query for save when `wg-dirty' is non-nil."
  (or (not (wg-dirty-p))
      (not (y-or-n-p "Save modified workgroups? "))
      (call-interactively 'wg-save-workgroups)
      t))

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
Keep in mind that workgroups will be updated with their working
wconfigs in the current frame."
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



;;; window-tree commands
;;
;; FIXME: Add complex window creation commands
;;
;; FIXME: Add window splitting, deletion and locking commands
;;
;; FIXME: These are half-hearted.  Clean them up, and allow specification of the
;; window-tree depth at which to operate.

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



;;; help commands

(defun wg-help ()
  "Just call `apropos-command' on \"^wg-\".
There used to be a bunch of help-buffer construction stuff here,
including a `wg-help' variable that basically duplicated every
command's docstring;  But why, when there's `apropos-command'?"
  (interactive)
  (apropos-command "^wg-"))



;;; wg-minibuffer-mode commands

;; FIXME: These commands should give a more informative error msg when no
;; workgroups are defined than "C-x C-n is undefined", etc.
;; `wg-minibuffer-mode' isn't currently turned on when no workgroups are
;; defined.

(defun wg-next-buffer-list-filter ()
  "Trigger a switch to the next buffer-list-filter."
  (interactive)
  (throw 'wg-action (list 'next (minibuffer-contents))))

(defun wg-previous-buffer-list-filter ()
  "Trigger a switch to the previous buffer-list-filter."
  (interactive)
  (throw 'wg-action (list 'prev (minibuffer-contents))))

(defun wg-backward-char-or-next-buffer-list-filter ()
  "Call `backward-char' unless `point' is right after the prompt,
in which case call `wg-next-buffer-list-filter'."
  (interactive)
  (if (> (point) (minibuffer-prompt-end)) (backward-char)
    (wg-next-buffer-list-filter)))

(defun wg-backward-char-or-previous-buffer-list-filter (&optional num)
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
                (pos (position buffer (wg-filtered-buffer-list t) :test 'equal)))
    (wg-workgroup-dissociate-buffer nil buffer)
    (wg-set-current-matches
     (wg-rotate-list (wg-filtered-buffer-list t) pos) mode)))

;; (defun wg-associate-first-match ()
;;   "Associate the first match with or update it in the current workgroup."
;;   (interactive)
;;   (wg-when-let ((mode (wg-read-buffer-mode))
;;                 (buffer (wg-current-match mode))
;;                 (pos (position buffer (wg-filtered-buffer-list t) :test 'equal)))
;;     (wg-workgroup-update-or-associate-buffer nil buffer 'hard)
;;     (wg-set-current-matches
;;      (wg-rotate-list (wg-filtered-buffer-list t) pos) mode)))

(defun wg-associate-first-match ()
  "Associate the first match with or update it in the current workgroup."
  (interactive)
  (wg-when-let ((mode (wg-read-buffer-mode))
                (buffer (wg-current-match mode))
                (pos (position buffer (wg-filtered-buffer-list t) :test 'equal)))
    (wg-workgroup-associate-buffer nil buffer)
    (wg-set-current-matches
     (wg-rotate-list (wg-filtered-buffer-list t) pos) mode)))

(defun wg-minibuffer-mode-purge-soft-associated-buffers ()
  "Purge soft-associated buffers and update the current matches."
  (interactive)
  (wg-workgroup-purge-soft-associated-buffers nil)
  (wg-set-current-matches
   (let ((unpurged (wg-filtered-buffer-list t)) new-matches)
     (dolist (match (wg-current-matches) (nreverse new-matches))
       (when (member match unpurged)
         (push match new-matches))))))



;;; advice

;; (defun wg-auto-associate-buffer-helper (workgroup buffer assoc)
;;   "Associate BUFFER with WORKGROUP based on ASSOC.
;; See `wg-buffer-auto-association' for allowable values of ASSOC."
;;   (cond ((memq assoc '(soft hard))
;;          (wg-workgroup-associate-buffer workgroup buffer assoc t))
;;         ((functionp assoc)
;;          (wg-auto-associate-buffer-helper
;;           workgroup buffer (funcall assoc workgroup buffer)))
;;         (t nil)))

(defun wg-auto-associate-buffer-helper (workgroup buffer assoc)
  "Associate BUFFER with WORKGROUP based on ASSOC.
See `wg-buffer-auto-association' for allowable values of ASSOC."
  (cond ((memq assoc '(soft hard))
         (wg-workgroup-associate-buffer workgroup buffer (eq assoc 'soft)))
        ((functionp assoc)
         (wg-auto-associate-buffer-helper
          workgroup buffer (funcall assoc workgroup buffer)))
        (t nil)))

(defun wg-auto-associate-buffer (buffer &optional frame)
  "Conditionally associate BUFFER with the current workgroup in FRAME.
Frame defaults to `selected-frame'.  See `wg-buffer-auto-association'."
  (when (and wg-buffer-list-filtration-on wg-buffer-auto-association-on)
    (wg-when-let ((wg (wg-current-workgroup t frame)))
      (wg-auto-associate-buffer-helper
       wg buffer (or (wg-workgroup-parameter wg 'buffer-auto-association)
                     wg-buffer-auto-association)))))

(defadvice switch-to-buffer (after wg-auto-associate-buffer)
  "Automatically associate the buffer with the current workgroup."
  (wg-auto-associate-buffer ad-return-value))

(defadvice set-window-buffer (after wg-auto-associate-buffer)
  "Automatically associate the buffer with the current workgroup."
  (wg-auto-associate-buffer
   (ad-get-arg 1)
   (window-frame (or (ad-get-arg 0) (selected-window)))))

(defun wg-enable-all-advice ()
  "Enable and activate all of Workgroups' advice."
  (ad-define-subr-args
   'switch-to-buffer '(buffer-or-name &optional norecord))
  (ad-enable-advice 'switch-to-buffer 'after 'wg-auto-associate-buffer)
  (ad-activate 'switch-to-buffer)
  (ad-define-subr-args
   'set-window-buffer '(window buffer-or-name &optional keep-margins))
  (ad-enable-advice 'set-window-buffer 'after 'wg-auto-associate-buffer)
  (ad-activate 'set-window-buffer))

(defun wg-disable-all-advice ()
  "Disable and deactivate all of Workgroups' advice."
  (ad-disable-advice 'switch-to-buffer 'after 'wg-auto-associate-buffer)
  (ad-deactivate 'switch-to-buffer)
  (ad-disable-advice 'set-window-buffer 'after 'wg-auto-associate-buffer)
  (ad-deactivate 'set-window-buffer))



;;; keymaps

(defvar wg-prefixed-map
  (wg-fill-keymap (make-sparse-keymap)

    ;; workgroup creation

    (kbd "C-c")        'wg-create-workgroup
    (kbd "c")          'wg-create-workgroup
    (kbd "C")          'wg-clone-workgroup


    ;; killing and yanking

    (kbd "C-k")        'wg-kill-workgroup
    (kbd "k")          'wg-kill-workgroup
    (kbd "M-W")        'wg-kill-ring-save-base-wconfig
    (kbd "M-w")        'wg-kill-ring-save-working-wconfig
    (kbd "C-y")        'wg-yank-wconfig
    (kbd "y")          'wg-yank-wconfig
    (kbd "M-k")        'wg-kill-workgroup-and-buffers
    (kbd "K")          'wg-delete-other-workgroups


    ;; updating and reverting

    (kbd "C-u")        'wg-update-workgroup
    (kbd "u")          'wg-update-workgroup
    (kbd "C-S-u")      'wg-update-all-workgroups
    (kbd "U")          'wg-update-all-workgroups
    (kbd "C-r")        'wg-revert-workgroup
    (kbd "r")          'wg-revert-workgroup
    (kbd "C-S-r")      'wg-revert-all-workgroups
    (kbd "R")          'wg-revert-all-workgroups


    ;; workgroup switching

    (kbd "C-'")        'wg-switch-to-workgroup
    (kbd "'")          'wg-switch-to-workgroup
    (kbd "C-v")        'wg-switch-to-workgroup
    (kbd "v")          'wg-switch-to-workgroup
    (kbd "M-v")        'wg-switch-to-workgroup-other-frame
    (kbd "C-j")        'wg-switch-to-workgroup-at-index
    (kbd "j")          'wg-switch-to-workgroup-at-index
    (kbd "0")          'wg-switch-to-workgroup-at-index-0
    (kbd "1")          'wg-switch-to-workgroup-at-index-1
    (kbd "2")          'wg-switch-to-workgroup-at-index-2
    (kbd "3")          'wg-switch-to-workgroup-at-index-3
    (kbd "4")          'wg-switch-to-workgroup-at-index-4
    (kbd "5")          'wg-switch-to-workgroup-at-index-5
    (kbd "6")          'wg-switch-to-workgroup-at-index-6
    (kbd "7")          'wg-switch-to-workgroup-at-index-7
    (kbd "8")          'wg-switch-to-workgroup-at-index-8
    (kbd "9")          'wg-switch-to-workgroup-at-index-9
    (kbd "C-p")        'wg-switch-to-workgroup-left
    (kbd "p")          'wg-switch-to-workgroup-left
    (kbd "C-n")        'wg-switch-to-workgroup-right
    (kbd "n")          'wg-switch-to-workgroup-right
    (kbd "M-p")        'wg-switch-to-workgroup-left-other-frame
    (kbd "M-n")        'wg-switch-to-workgroup-right-other-frame
    (kbd "C-a")        'wg-switch-to-previous-workgroup
    (kbd "a")          'wg-switch-to-previous-workgroup


    ;; wconfig undo/redo

    (kbd "<left>")     'wg-undo-wconfig-change
    (kbd "<right>")    'wg-redo-wconfig-change
    (kbd "[")          'wg-undo-wconfig-change
    (kbd "]")          'wg-redo-wconfig-change
    (kbd "{")          'wg-undo-once-all-workgroups
    (kbd "}")          'wg-redo-once-all-workgroups


    ;; buffer-list

    (kbd "+")          'wg-associate-buffer-with-workgroup
    (kbd "-")          'wg-dissociate-buffer-from-workgroup
    (kbd "=")          'wg-cycle-buffer-association-type
    (kbd "*")          'wg-restore-workgroup-associated-buffers
    (kbd "_")          'wg-purge-soft-associated-buffers
    (kbd "M-b")        'wg-toggle-buffer-list-filtration
    (kbd "(")          'wg-next-buffer
    (kbd ")")          'wg-previous-buffer


    ;; workgroup movement

    (kbd "C-x")        'wg-swap-workgroups
    (kbd "C-,")        'wg-offset-workgroup-left
    (kbd "C-.")        'wg-offset-workgroup-right


    ;; file and buffer

    (kbd "C-s")        'wg-save-workgroups
    (kbd "C-l")        'wg-find-workgroups-file
    (kbd "M-l")        'wg-find-new-workgroups-file
    (kbd "S")          'wg-update-all-workgroups-and-save
    (kbd "C-f")        'wg-find-file
    (kbd "S-C-f")      'wg-find-file-read-only
    (kbd "C-b")        'wg-switch-to-buffer
    (kbd "b")          'wg-switch-to-buffer
    (kbd "d")          'wg-dired


    ;; window moving and frame reversal

    (kbd "<")          'wg-backward-transpose-window
    (kbd ">")          'wg-transpose-window
    (kbd "|")          'wg-reverse-frame-horizontally
    (kbd "\\")         'wg-reverse-frame-vertically
    (kbd "/")          'wg-reverse-frame-horizontally-and-vertically


    ;; toggling

    (kbd "C-i")        'wg-toggle-mode-line-display
    (kbd "C-w")        'wg-toggle-morph


    ;; echoing

    (kbd "S-C-e")      'wg-echo-current-workgroup
    (kbd "E")          'wg-echo-current-workgroup
    (kbd "C-e")        'wg-echo-all-workgroups
    (kbd "e")          'wg-echo-all-workgroups
    (kbd "C-t")        'wg-echo-time
    (kbd "t")          'wg-echo-time
    (kbd "V")          'wg-echo-version
    (kbd "C-m")        'wg-echo-last-message
    (kbd "m")          'wg-echo-last-message


    ;; misc

    (kbd "A")          'wg-rename-workgroup
    (kbd "!")          'wg-reset
    (kbd "?")          'wg-help

    )
  "Make and return the keymap that sits on `wg-prefix-key'.")

(defun wg-make-workgroups-mode-map ()
  "Return Workgroups' minor-mode-map.
This map includes `wg-prefixed-map' on `wg-prefix-key', as well
as Workgroups' command remappings."
  (let ((map (make-sparse-keymap)))
    (define-key map wg-prefix-key
      wg-prefixed-map)
    (when wg-remap-switch-to-buffer
      (define-key map [remap switch-to-buffer]
        'wg-switch-to-buffer))
    (when wg-remap-switch-to-buffer-other-window
      (define-key map [remap switch-to-buffer-other-window]
        'wg-switch-to-buffer-other-window))
    (when wg-remap-switch-to-buffer-other-frame
      (define-key map [remap switch-to-buffer-other-frame]
        'wg-switch-to-buffer-other-frame))
    (when wg-remap-next-buffer
      (define-key map [remap next-buffer]
        'wg-next-buffer))
    (when wg-remap-previous-buffer
      (define-key map [remap previous-buffer]
        'wg-previous-buffer))
    (when wg-remap-kill-buffer
      (define-key map [remap kill-buffer]
        'wg-kill-buffer))
    (when wg-remap-display-buffer
      (define-key map [remap display-buffer]
        'wg-display-buffer))
    (when wg-remap-insert-buffer
      (define-key map [remap insert-buffer]
        'wg-insert-buffer))
    (cond ((eq wg-remap-bury-buffer 'banish)
           (define-key map [remap bury-buffer]
             'wg-banish-buffer))
          (wg-remap-bury-buffer
           (define-key map [remap bury-buffer]
             'wg-bury-buffer)))
    (setq workgroups-mode-map map)))

(defvar wg-minibuffer-mode-map
  (wg-fill-keymap (make-sparse-keymap)
    (kbd "C-b")       'wg-backward-char-or-next-buffer-list-filter
    (kbd "C-c n")     'wg-next-buffer-list-filter
    (kbd "C-c C-n")   'wg-next-buffer-list-filter
    (kbd "C-S-b")     'wg-backward-char-or-previous-buffer-list-filter
    (kbd "C-c p")     'wg-previous-buffer-list-filter
    (kbd "C-c C-p")   'wg-previous-buffer-list-filter
    (kbd "C-c a")     'wg-associate-first-match
    (kbd "C-c C-a")   'wg-associate-first-match
    (kbd "C-c d")     'wg-dissociate-first-match
    (kbd "C-c C-d")   'wg-dissociate-first-match
    (kbd "C-c _")     'wg-minibuffer-mode-purge-soft-associated-buffers
    )
  "`wg-minibuffer-mode's keymap.")



;;; workgroups-everywhere

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



;;; wg-minibuffer-mode

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



;;; workgroups-mode

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

(defun wg-add-workgroups-mode-minor-mode-entries ()
  "Add Workgroups' minor-mode entries.
Adds entries to `minor-mode-list', `minor-mode-alist' and
`minor-mode-map-alist'."
  (pushnew 'workgroups-mode minor-mode-list)
  (pushnew '(workgroups-mode " wg") minor-mode-alist :test 'equal)
  (setq minor-mode-map-alist
        (cons (cons 'workgroups-mode (wg-make-workgroups-mode-map))
              (delete (assoc 'workgroups-mode minor-mode-map-alist)
                      minor-mode-map-alist))))

(defun workgroups-mode (&optional arg)
  "Turns `workgroups-mode' on and off.
If ARG is null, toggle `workgroups-mode'.
If ARG is an integer greater than zero, turn on `workgroups-mode'.
If ARG is an integer less one, turn off `workgroups-mode'.
If ARG is anything else, turn on `workgroups-mode'."
  (interactive (list current-prefix-arg))
  (setq workgroups-mode
        (cond ((not arg) (not workgroups-mode))
              ((integerp arg) (if (> arg 0) t nil))
              (t)))
  (cond
   (workgroups-mode
    (wg-add-workgroups-mode-minor-mode-entries)
    (wg-enable-all-advice)
    (wg-add-or-remove-workgroups-hooks nil)
    (wg-add-mode-line-display)
    (run-hooks 'workgroups-mode-hook))
   (t
    (wg-save-workgroups-on-workgroups-mode-exit)
    (wg-disable-all-advice)
    (wg-add-or-remove-workgroups-hooks t)
    (wg-remove-mode-line-display)
    (run-hooks 'workgroups-mode-exit-hook)))
  (wg-fontified-message
    (:cmd "Workgroups Mode: ")
    (:msg (if workgroups-mode "on" "off")))
  workgroups-mode)



;;; provide

(provide 'workgroups)



;;; workgroups.el ends here
