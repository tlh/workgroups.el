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
;; See the file README.md in this directory
;;
;;; Installation:
;;
;;; Usage:
;;

;;; Symbol naming conventions:
;;
;; bufobj always refers to a Workgroups buffer (wg-buf) or an Emacs buffer
;; W always refers to a Workgroups window (wg-win) or window tree (wg-wtree).
;; SW always refers to a sub-window or sub-window-tree of a wtree.
;; WL always refers to the window list of a wtree.
;; LN, TN, RN and BN always refer to the LEFT, TOP, RIGHT and BOTTOM
;;   edges of an edge list, where N is a differentiating integer.
;; LS, HS, LB and HB always refer to the LOW-SIDE, HIGH-SIDE, LOW-BOUND
;;   and HIGH-BOUND of a bounds list.  See `wg-with-bounds'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO:
;;
;; * gc of of tracked bufs
;;
;; ** gc a buf when its refcount is 0 and there's no corresponding live buffer
;;
;; ** Tracked buffers should be updated in `wg-tracked-buffers' when they're
;;    killed.
;;
;; ** `wg-tracked-buffers' should also be filtered on `wg-save' to only contain
;;    bufs that are referred to either by at least one window in any workgroup's
;;    base or working wconfig, or by any workgroup's associated-buffers list.
;;
;; ** Don't gc a buf when there's an existing buffer with the same uid
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Code:

(eval-and-compile
  (require 'cl))

(eval-when-compile ;; bytecomp warnings begone!
  (require 'ido nil t)
  (require 'iswitchb nil t))



;;; consts

(defconst wg-version "1.0.0")



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

(defcustom wg-special-buffer-serdes-functions
  '(wg-serialize-Info-buffer
    wg-serialize-help-buffer)
  "FIXME: docstring this"
  :type 'alist
  :group 'workgroups)

(defcustom wg-default-buffer "*scratch*"
  "Buffer made visible a window when the window's actual buffer
can't be restored.  Also used when a blank workgroup is created."
  :type 'string
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

(defcustom wg-restore-dedicated t
  "Non-nil means restore `window-dedicated-p' on workgroup restore."
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
should take two arguments: a workgroup and a list of live Emacs
buffers and/or the names of live Emacs buffers.  The function
should return the filtered list of buffer objects and/or buffer
names.  You may not need to use these arguments in your custom
filters.

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

(defcustom wg-buffer-auto-association 'weak
  "Specifies the behavior for auto-associating buffers with workgroups.

Buffers can be auto-associated with workgroups whenever they are
made visible in windows (by any function that bottoms out in a
call to `switch-to-buffer' or `set-window-buffer').  This setting
determines whether and how that happens.  The workgroup with
which to associate the buffer is the current workgroup in the
window's frame.

`no-assoc' means don't associate buffers with workgroups.
`weak' means weakly associate buffers with workgroups.
`strong' means strongly associate buffers with workgroups.

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
    (strongly-associated . "@")
    (weakly-associated   . "~")
    (unassociated        . "-")
    (modified            . "*")
    (unmodified          . "-"))
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

(defvar wg-current-workgroup-set nil
  "Current workgroup-set object.")

(defvar wg-workgroups-mode-minor-mode-map-entry nil
  "Workgroups' minor-mode-map entry.")

(defvar wg-wconfig-kill-ring nil
  "Ring of killed or kill-ring-saved wconfigs.")

(defvar wg-last-message nil
  "Holds the last message Workgroups sent to the echo area.")

(defvar wg-face-abbrevs nil
  "Assoc list mapping face abbreviations to face names.")

(defvar wg-buffer-uid nil
  "Symbol for the current buffer's wg-buf's uid.
Every Workgroups buffer object (wg-buf) has a uid.  When
Workgroups creates or encounters an Emacs buffer object
corresponding to a wg-buf, it tags it with the wg-buf's uid to
unambiguously pair the two.")
(make-variable-buffer-local 'wg-buffer-uid)


;; file and modified flag vars

(defvar wg-visited-file-name nil
  "Current workgroups file.")

(defvar wg-flag-modified t
  "Dynamically bound to nil around destructive operations to
temporarily disable flagging `modified'.")


;; undo vars

(defvar wg-window-configuration-changed nil
  "Flag set by `window-configuration-change-hook'.")

(defvar wg-undoify-window-configuration-change t
  "Flag unset when changes to the window config shouldn't cause
workgroups' undo info to be updated.")

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

;; (defvar wg-selected-window nil
;;   "Bound to the `selected-window' by `wg-restore-window'.")

(defvar wg-window-tree-selected-window nil
  "Used during wconfig restoration to hold the selected window.")



;;; faces

(defmacro wg-defface (face key spec doc &rest args)
  "`defface' wrapper adding a lookup key used by `wg-fontify'."
  (declare (indent 2))
  `(progn
     (pushnew (cons ,key ',face) wg-face-abbrevs :test #'equal)
     (defface ,face ,spec ,doc ,@args)))

(wg-defface wg-current-workgroup-face :cur
  '((t :inherit font-lock-constant-face :bold nil))
  "Face used for current elements in list displays."
  :group 'workgroups)

(wg-defface wg-previous-workgroup-face :prev
  '((t :inherit font-lock-keyword-face :bold nil))
  "Face used for the name of the previous workgroup in the list display."
  :group 'workgroups)

(wg-defface wg-other-workgroup-face :other
  '((t :inherit font-lock-string-face :bold nil))
  "Face used for the names of other workgroups in the list display."
  :group 'workgroups)

(wg-defface wg-command-face :cmd
  '((t :inherit font-lock-function-name-face :bold nil))
  "Face used for command/operation strings."
  :group 'workgroups)

(wg-defface wg-divider-face :div
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face used for dividers."
  :group 'workgroups)

(wg-defface wg-brace-face :brace
  '((t :inherit font-lock-builtin-face :bold nil))
  "Face used for left and right braces."
  :group 'workgroups)

(wg-defface wg-message-face :msg
  '((t :inherit font-lock-string-face :bold nil))
  "Face used for messages."
  :group 'workgroups)

(wg-defface wg-mode-line-face :mode
  '((t :inherit font-lock-doc-face :bold nil))
  "Face used for workgroup position and name in the mode-line display."
  :group 'workgroups)

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

  (defun wg-symcat (&rest symbols-and-strings)
    "Return a new interned symbol by concatenating SYMBOLS-AND-STRINGS."
    (intern (mapconcat (lambda (obj) (if (symbolp obj) (symbol-name obj) obj))
                       symbols-and-strings "")))

  (defmacro wg-docar (spec &rest body)
    "do-style wrapper for `mapcar'."
    (declare (indent 1))
    `(mapcar (lambda (,(car spec)) ,@body) ,(cadr spec)))

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

(defmacro wg-removef-p (object place &rest keys)
  "If OBJECT is a `member*' of PLACE, remove it and return t.
Otherwise return nil.  KEYS are the standard CL seq lib keys."
  (wg-with-gensyms (obj)
    `(let ((,obj ,object))
       (when (member* ,obj ,place ,@keys)
         (setf ,place (remove* ,obj ,place ,@keys))
         t))))

(defmacro wg-pushnew-p (object place &rest keys)
  "If OBJECT is not a `member*' of PLACE, push it and return t.
Otherwise return nil.  KEYS are the standard CL seq lib keys."
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

(defun wg-make-string (times string &optional separator)
  "Like `make-string', but includes a separator."
  (mapconcat 'identity (make-list times string) (or separator "")))

(defun wg-get-buffer (buffer-or-name)
  "Return BUFFER-OR-NAME's buffer, or error."
  (or (get-buffer buffer-or-name)
      (error "%S does not identify a bufferis not a buffer" buffer-or-name)))

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



;;; workgroups utils

(defvar wg-readable-types
  '(integer float cons symbol vector string char-table bool-vector)
  "List of types with readable printed representations.")

(defun wg-is-readable-p (obj)
  "Return non-nil if OBJ's printed representation is readable."
  (memq (type-of obj) wg-readable-types))

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
  (apply 'concat (wg-docar (time (or time (current-time)))
                   (wg-int-to-b36 time 4))))

(defun wg-generate-uid (&optional prefix)
  "FIXME: docstring this"
  (concat (when prefix (concat prefix "-"))
          (wg-time-to-b36)
          "-"
          (wg-int-to-b36 string-chars-consed)))



;;; structure types

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FIXME: Add text-scale-mode-amount
;;
;; FIXME: finges, margins, scroll-bars and some other stuff should be set as
;; buffer-local variables, not properties of windows.  variables to be set:
;;
;; fringes:
;;   fringes-outside-margins
;;   left-fringe-width
;;   right-fringe-width
;;
;; margins:
;;   left-margin-width
;;   right-margin-width
;;
;; scroll-bars (can be set at the frame, window and buffer levels - gah):
;;   buffer: vertical-scroll-bar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(wg-defstruct wg buf
  (uid (wg-generate-uid))
  (name)
  (file-name)
  (major-mode)
  (point)
  (mark)
  (mark-active)
  (special-data))

(wg-defstruct wg win
  (edges)
  (point)
  (start)
  (hscroll)
  (scroll-bars)
  (fringes)
  (margins)
  (dedicated)
  (selected)
  (minibuffer-scroll)
  (buf-uid))

(wg-defstruct wg wtree
  (dir)
  (edges)
  (wlist))

(wg-defstruct wg wconfig
  (left)
  (top)
  (width)
  (height)
  (vertical-scroll-bars)
  (scroll-bar-width)
  (wtree))

(wg-defstruct wg workgroup
  (uid (wg-generate-uid))
  (name)
  (modified)
  (base-wconfig)
  (most-recent-working-wconfig)
  (weakly-associated-buffer-uids)
  (strongly-associated-buffer-uids)
  (parameters))

(wg-defstruct wg workgroup-set
  (version wg-version)
  (modified)
  (workgroup-list)
  (tracked-buffers)
  (parameters))



;;; workgroup-set ops

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FIXME: call these something other than "workgroup-sets" -- It's too easily
;; confused with other uses of "set", and seems to imply that the workgroup-list
;; isn't ordered.
;;
;; FIXME: factor out the similarities between workgroup-set-parameters and
;; workgroup-parameters
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun wg-set-workgroup-set-parameter (workgroup-set parameter value)
;;   (let ((workgroup-set (or workgroup-set (wg-current-workgroup-set))))
;;     (unless wg-no-flag-modified (setq wg-modified t))
;;     (wg-set workgroup-set parameter value)
;;     value))

(defun wg-current-workgroup-set ()
  "Return `wg-current-workgroup-set', setting it first if necessary."
  (or wg-current-workgroup-set
      (setq wg-current-workgroup-set (wg-make-workgroup-set))))

(defun wg-workgroup-list (&optional noerror)
  "Return the value of `wg-current-workgroup-set's :workgroup-list slot."
  (or (wg-workgroup-set-workgroup-list (wg-current-workgroup-set))
      (unless noerror
        (error "No workgroups are defined."))))

(defmacro wg-modified ()
  "setf'ably expands to `wg-current-workgroup-set's modified slot."
  `(wg-workgroup-set-modified (wg-current-workgroup-set)))

(defmacro wg-tracked-buffers ()
  "setf'ably expands to `wg-current-workgroup-set's tracked-buffers slot."
  `(wg-workgroup-set-tracked-buffers (wg-current-workgroup-set)))



;;; buffer object utils

(defun wg-buffer-uid (buffer-or-name)
  "Return BUFFER-OR-NAME's buffer-local value of `wg-buffer-uid'."
  (buffer-local-value 'wg-buffer-uid (wg-get-buffer buffer-or-name)))

(defun wg-bufobj-uid (bufobj)
  "Return BUFOBJ's uid."
  (etypecase bufobj
    (buffer (wg-buffer-uid bufobj))
    (wg-buf (wg-buf-uid bufobj))
    (string (wg-bufobj-uid (wg-get-buffer bufobj)))))

(defun wg-bufobj-name (bufobj)
  "Return BUFOBJ's buffer name."
  (etypecase bufobj
    (buffer (buffer-name bufobj))
    (wg-buf (wg-buf-name bufobj))
    (string (wg-buffer-name bufobj))))

(defun wg-bufobj-file-name (bufobj)
  "Return BUFOBJ's filename."
  (etypecase bufobj
    (buffer (buffer-file-name bufobj))
    (wg-buf (wg-buf-file-name bufobj))
    (string (wg-bufobj-file-name (wg-get-buffer bufobj)))))

;; FIXME: beef this up
(defun wg-equal-bufobjs (bufobj1 bufobj2)
  "Return t if BUFOBJ1 is \"equal\" to BUFOBJ2."
  (let ((fname1 (wg-bufobj-file-name bufobj1))
        (fname2 (wg-bufobj-file-name bufobj2)))
    (cond ((and fname1 fname2) (string= fname1 fname2))
          ((or fname1 fname2) nil)
          ((string= (wg-bufobj-name bufobj1) (wg-bufobj-name bufobj2)) t))))

(defun wg-find-bufobj (bufobj bufobj-list)
  "Find BUFOBJ in BUFOBJ-LIST, testing with `wg-equal-bufobjs'."
  (find bufobj bufobj-list :test 'wg-equal-bufobjs))

(defun wg-find-bufobj-by-uid (uid bufobj-list)
  "Find the bufobj in BUFOBJ-LIST with uid UID."
  (find uid bufobj-list :test 'string= :key 'wg-bufobj-uid))

(defun wg-find-buf-in-buf-list (buf buf-list)
  "Find BUF in BUF-LIST.
This is only here for completeness."
  (find buf buf-list))

(defun wg-find-buffer-in-buffer-list (buffer-or-name buffer-list)
  "Find BUFFER-OR-NAME in BUFFER-LIST."
  (find (wg-get-buffer buffer-or-name) buffer-list :key 'wg-get-buffer))

(defun wg-find-buffer-in-buf-list (buffer-or-name buf-list)
  "Find BUFFER-OR-NAME in BUF-LIST."
  (wg-aif (wg-buffer-uid buffer-or-name)
      (wg-find-bufobj-by-uid it buf-list)
    (wg-find-bufobj buffer-or-name buf-list)))

(defun wg-find-buf-in-buffer-list (buf buffer-list)
  "Find BUF in BUFFER-LIST."
  (or (wg-find-bufobj-by-uid (wg-buf-uid buf) buffer-list)
      (wg-find-bufobj buf buffer-list)))

(defun wg-find-tracked-buf-by-uid (uid)
  "Find a buf in `wg-tracked-buffers' by UID."
  (wg-find-bufobj-by-uid uid (wg-tracked-buffers)))

(defun wg-set-buffer-uid-or-error (uid &optional buffer)
  "Set BUFFER's buffer local value of `wg-buffer-uid' to UID.
If BUFFER already has a buffer local value of `wg-buffer-uid',
and it's not equal to UID, error."
  (if wg-buffer-uid
      (if (string= wg-buffer-uid uid) uid
        (error "uids don't match %S and %S" uid wg-buffer-uid))
    (setq wg-buffer-uid uid)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; wconfig construction

(defun wg-buffer-special-data (buffer)
  "Return BUFFER's auxiliary serialization, or nil."
  (some (lambda (fn) (funcall fn buffer)) wg-special-buffer-serdes-functions))

(defun wg-window-point (ewin)
  "Return `point' or :max.  See `wg-restore-point-max'.
EWIN should be an Emacs window object."
  (let ((p (window-point ewin)))
    (if (and wg-restore-point-max (= p (point-max))) :max p)))

(defun wg-buffer-to-buf (buffer)
  "Return a serialized buffer from Emacs buffer BUFFER."
  (with-current-buffer buffer
    (wg-make-buf
     :name          (buffer-name)
     :file-name     (buffer-file-name)
     :major-mode    major-mode
     :point         (point)
     :mark          (mark)
     :mark-active   mark-active
     :special-data  (wg-buffer-special-data buffer))))

(defun wg-track-buffer (buffer)
  "Make a buf from BUFFER, and add it to `wg-tracked-buffers' if necessary.
If there isn't already a buf corresponding to BUFFER in
`wg-tracked-buffers', make one and add it.  Return BUFFER's uid
in either case."
  (with-current-buffer buffer
    (setq wg-buffer-uid
          (wg-aif (wg-find-buffer-in-buf-list buffer (wg-tracked-buffers))
              (wg-buf-uid it)
            (let* ((buf (wg-buffer-to-buf buffer))
                   (uid (wg-buf-uid buf)))
              (push buf (wg-tracked-buffers))
              uid)))))

(defun wg-buffer-uid-or-track (buffer)
  "If there isn't already a buf corresponding to BUFFER in
`wg-tracked-buffers', make one and add it.  Return BUFFER's uid
in either case."
  (or (wg-buffer-uid buffer) (wg-track-buffer buffer)))

(defun wg-bufobj-uid-or-track (bufobj)
  "If BUFOBJ is a wg-buf, return its uid.
If BUFOBJ is a buffer or a buffer name, see `wg-buffer-uid-or-track'."
  (etypecase bufobj
    (wg-buf (wg-buf-uid bufobj)) ;; TODO: possibly also track bufs
    (buffer (wg-buffer-uid-or-track bufobj))
    (string (wg-bufobj-uid-or-track (wg-get-buffer bufobj)))))



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


(defun wg-window-to-win (window)
  "Return a serialized window from Emacs window WINDOW."
  (let ((selected (eq window (selected-window))))
    (with-selected-window window
      (wg-make-win
       :edges              (window-edges window)
       :point              (wg-window-point window)
       :start              (window-start window)
       :hscroll            (window-hscroll window)
       :scroll-bars        (window-scroll-bars window)
       :margins            (window-margins window)
       :fringes            (window-fringes window)
       :selected           selected
       :minibuffer-scroll  (eq window minibuffer-scroll-window)
       :dedicated          (window-dedicated-p window)
       :buf-uid            (wg-buffer-uid-or-track (window-buffer window))))))

(defun wg-window-tree-to-wtree (window-tree)
  "Return a serialized window-tree from Emacs window tree WINDOW-TREE."
  (wg-barf-on-active-minibuffer)
  (flet ((inner (w) (if (windowp w) (wg-window-to-win w)
                      (wg-dbind (dir edges . wins) w
                        (wg-make-wtree
                         :dir    dir
                         :edges  edges
                         :wlist  (mapcar #'inner wins))))))
    (let ((w (car window-tree)))
      (when (and (windowp w) (window-minibuffer-p w))
        (error "Workgroups can't operate on minibuffer-only frames."))
      (inner w))))

(defun wg-frame-to-wconfig (&optional frame)
  "Return a serialized window-configuration from FRAME or `selected-frame'."
  (let ((frame (or frame (selected-frame))))
    (wg-make-wconfig
     :left                  (frame-parameter frame 'left)
     :top                   (frame-parameter frame 'top)
     :width                 (frame-parameter frame 'width)
     :height                (frame-parameter frame 'height)
     :vertical-scroll-bars  (frame-parameter frame 'vertical-scroll-bars)
     :scroll-bar-width      (frame-parameter frame 'scroll-bar-width)
     :wtree                 (wg-window-tree-to-wtree (window-tree frame)))))

(defun wg-make-blank-wconfig (&optional buffer)
  "Return a new blank wconfig.
BUFFER or `wg-default-buffer' is visible in the only window."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer wg-default-buffer))
    (wg-frame-to-wconfig)))



;;; win/wtree/wconfig utils

(defun wg-edges (w)
  "Return W's edge list."
  (etypecase w
    (wg-win (wg-win-edges w))
    (wg-wtree (wg-wtree-edges w))))

(defun wg-set-edges (w edges)
  "Set W's edge list, and return W."
  (etypecase w
    (wg-win (setf (wg-win-edges w) edges))
    (wg-wtree (setf (wg-wtree-edges w) edges)))
  w)

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

(defmacro wg-with-bounds (wtree dir spec &rest body)
  "Bind SPEC to W's bounds in DIR, and eval BODY.
\"bounds\" are a direction-independent way of dealing with edge lists."
  (declare (indent 3))
  (wg-with-gensyms (dir-sym l1 t1 r1 b1)
    (wg-dbind (ls1 hs1 lb1 hb1) spec
      `(wg-with-edges ,wtree (,l1 ,t1 ,r1 ,b1)
         (cond (,dir (let ((,ls1 ,l1) (,hs1 ,r1) (,lb1 ,t1) (,hb1 ,b1))
                       ,@body))
               (t    (let ((,ls1 ,t1) (,hs1 ,b1) (,lb1 ,l1) (,hb1 ,r1))
                       ,@body)))))))

(defun wg-set-bounds (w dir ls hs lb hb)
  "Set W's edges in DIR with bounds LS HS LB and HB."
  (wg-set-edges w (if dir (list ls lb hs hb) (list lb ls hb hs))))

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
  (wg-set-edges w (mapcar* op (wg-edges w) edges)))

(defun wg-first-win (w)
  "Return the first actual window in W."
  (if (wg-win-p w) w
    (wg-first-win (car (wg-wtree-wlist w)))))

(defun wg-last-win (w)
  "Return the last actual window in W."
  (if (wg-win-p w) w
    (wg-last-win (wg-last1 (wg-wtree-wlist w)))))

(defun wg-minify-win (w)
  "Set W's edges to the smallest allowable."
  (let* ((edges (wg-edges w))
         (left (car edges))
         (top (cadr edges)))
    (wg-set-edges w (list left top
                          (+ left wg-actual-min-width)
                          (+ top  wg-actual-min-height)))))

(defun wg-minified-copy-of-last-win (w)
  "Minify a copy of the last actual window in W."
  (wg-minify-win (wg-copy-win (wg-last-win w))))

(defun wg-w-size (w &optional height)
  "Return the width or height of W, calculated from its edge list."
  (wg-with-edges w (l1 t1 r1 b1)
    (if height (- b1 t1) (- r1 l1))))

(defun wg-adjust-w-size (w width-fn height-fn &optional new-left new-top)
  "Adjust W's width and height with WIDTH-FN and HEIGHT-FN."
  (wg-with-edges w (left top right bottom)
    (let ((left (or new-left left)) (top (or new-top top)))
      (wg-set-edges w (list left
                            top
                            (+ left (funcall width-fn  (- right  left)))
                            (+ top  (funcall height-fn (- bottom top))))))))

(defun wg-scale-w-size (w width-scale height-scale)
  "Scale W's size by WIDTH-SCALE and HEIGHT-SCALE."
  (flet ((wscale (width)  (truncate (* width  width-scale)))
         (hscale (height) (truncate (* height height-scale))))
    (wg-adjust-w-size w #'wscale #'hscale)))

(defun wg-equal-wtrees (w1 w2)
  "Return t when W1 and W2 have equal structure."
  (cond ((and (wg-win-p w1) (wg-win-p w2))
         (equal (wg-edges w1) (wg-edges w2)))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (and (eq (wg-wtree-dir w1) (wg-wtree-dir w2))
              (equal (wg-wtree-edges w1) (wg-wtree-edges w2))
              (every #'wg-equal-wtrees
                     (wg-wtree-wlist w1)
                     (wg-wtree-wlist w2))))))

(defun wg-normalize-wtree (wtree)
  "Clean up and return a new wtree from WTREE.
Recalculate the edge lists of all subwins, and remove subwins
outside of WTREE's bounds.  If there's only one element in the
new wlist, return it instead of a new wtree."
  (if (wg-win-p wtree) wtree
    (wg-with-slots wtree ((dir wg-wtree-dir)
                          (wlist wg-wtree-wlist))
      (wg-with-bounds wtree dir (ls1 hs1 lb1 hb1)
        (let* ((min-size (wg-min-size dir))
               (max (- hb1 1 min-size))
               (lastw (wg-last1 wlist)))
          (flet ((mapwl
                  (wl)
                  (wg-dbind (sw . rest) wl
                    (cons (wg-normalize-wtree
                           (wg-set-bounds
                            sw dir ls1 hs1 lb1
                            (setq lb1 (if (eq sw lastw) hb1
                                        (let ((hb2 (+ lb1 (wg-w-size sw dir))))
                                          (if (>= hb2 max) hb1 hb2))))))
                          (when (< lb1 max) (mapwl rest))))))
            (let ((new (mapwl wlist)))
              (if (not (cdr new)) (car new)
                (setf (wg-wtree-wlist wtree) new)
                wtree))))))))

(defun wg-scale-wtree (wtree wscale hscale)
  "Return a copy of WTREE with its dimensions scaled by WSCALE and HSCALE.
All WTREE's subwins are scaled as well."
  (let ((scaled (wg-scale-w-size wtree wscale hscale)))
    (if (wg-win-p wtree) scaled
      (setf (wg-wtree-wlist scaled)
            (wg-docar (sw (wg-wtree-wlist scaled))
              (wg-scale-wtree sw wscale hscale)))
      scaled)))

(defun wg-scale-wconfigs-wtree (wconfig new-width new-height)
  "Scale WCONFIG's wtree with NEW-WIDTH and NEW-HEIGHT.
Return a copy WCONFIG's wtree scaled with `wg-scale-wtree' by the
ratio or NEW-WIDTH to WCONFIG's width, and NEW-HEIGHT to
WCONFIG's height."
  (wg-normalize-wtree
   (wg-scale-wtree
    (wg-wconfig-wtree wconfig)
    (/ (float new-width)  (wg-wconfig-width wconfig))
    (/ (float new-height) (wg-wconfig-height wconfig)))))

(defun wg-resize-frame-scale-wtree (wconfig &optional frame)
  "Set FRAME's size to WCONFIG's, returning a possibly scaled wtree.
If the frame size was set correctly, return WCONFIG's wtree
unchanged.  If it wasn't, return a copy of WCONFIG's wtree scaled
with `wg-scale-wconfigs-wtree' to fit the frame as it exists."
  (let ((frame (or frame (selected-frame))))
    (wg-with-slots wconfig ((wcwidth wg-wconfig-width)
                            (wcheight wg-wconfig-height))
      (when window-system (set-frame-size frame wcwidth wcheight))
      (let ((fwidth  (frame-parameter frame 'width))
            (fheight (frame-parameter frame 'height)))
        (if (and (= wcwidth fwidth) (= wcheight fheight))
            (wg-wconfig-wtree wconfig)
          (wg-scale-wconfigs-wtree wconfig fwidth fheight))))))

(defun wg-reverse-wlist (w &optional dir)
  "Reverse W's wlist and those of all its sub-wtrees in direction DIR.
If DIR is nil, reverse WTREE horizontally.
If DIR is 'both, reverse WTREE both horizontally and vertically.
Otherwise, reverse WTREE vertically."
  (flet ((inner (w) (if (wg-win-p w) w
                      (wg-with-slots w ((d1 wg-wtree-dir))
                        (wg-make-wtree
                         :dir d1
                         :edges (wg-wtree-edges w)
                         :wlist (let ((wl2 (mapcar #'inner (wg-wtree-wlist w))))
                                  (if (or (eq dir 'both) (eq dir d1))
                                      (nreverse wl2)
                                    wl2)))))))
    (wg-normalize-wtree (inner w))))

(defun wg-wtree-move-window (wtree offset)
  "Offset `selected-window' OFFSET places in WTREE."
  (flet ((inner (w) (if (wg-win-p w) w
                      (wg-with-slots w ((wlist wg-wtree-wlist))
                        (wg-make-wtree
                         :dir (wg-wtree-dir w)
                         :edges (wg-wtree-edges w)
                         :wlist (wg-aif (find t wlist :key 'wg-win-selected)
                                    (wg-cyclic-offset-elt it wlist offset)
                                  (mapcar #'inner wlist)))))))
    (wg-normalize-wtree (inner wtree))))

(defun wg-reverse-wconfig (wconfig &optional dir)
  "Reverse WCONFIG's wtree's wlist in direction DIR."
  (setf (wg-wconfig-wtree wconfig)
        (wg-reverse-wlist (wg-wconfig-wtree wconfig) dir))
  wconfig)

(defun wg-wconfig-move-window (wconfig offset)
  "Offset `selected-window' OFFSET places in WCONFIG."
  (setf (wg-wconfig-wtree wconfig)
        (wg-wtree-move-window (wg-wconfig-wtree wconfig) offset)))

(defun wg-flatten-wtree (wtree &optional key)
  "Flatten wtree into a list of wins, or, if KEY is non-nil, a
list of the returns values of calling KEY on all wins."
  (flet ((inner (w) (if (wg-win-p w)
                        (list (if key (funcall key w) w))
                      (mapcan 'inner (wg-wtree-wlist w)))))
    (inner wtree)))

(defun wg-win-list (wtree)
  "Construct and return a list of all wg-wins in WTREE."
  (wg-flatten-wtree wtree))

(defun wg-wtree-buf-uids (wtree)
  "Return a list of the buf uids of all wins in wtree."
  (wg-flatten-wtree wtree 'wg-win-buf-uid))



;;; non-file buffer serdes functions
;;
;; FIXME: Add restore-special-data customization option
;;
;; TODO: Add ielm, shell, eshell, term, ansi-term
;;

;; Info buffer serdes

(defun wg-deserialize-Info-buffer (buf)
  "Deserialize an Info buffer."
  (require 'info)
  (wg-aif (cdr (wg-buf-special-data buf))
      (apply #'Info-find-node it)
    (info)))

(defun wg-serialize-Info-buffer (buffer)
  "Serialize an Info buffer."
  (with-current-buffer buffer
    (when (eq major-mode 'Info-mode)
      (wg-when-boundp (Info-current-file Info-current-node)
        (list 'wg-deserialize-Info-buffer
              Info-current-file
              Info-current-node)))))


;; help buffer serdes

(defun wg-deserialize-help-buffer (buf)
  "Deserialize a `help-mode' buffer."
  (require 'help-mode)
  (wg-dbind (method . args) (cdr (wg-buf-special-data buf))
    (apply method args)))

(defun wg-serialize-help-buffer (buffer)
  "Return `help-mode's extended serialization of BUFFER."
  (with-current-buffer buffer
    (wg-when-boundp (help-xref-stack-item)
      (when (and (eq major-mode 'help-mode)
                 (assq 'help-xref-stack-item (buffer-local-variables)))
        (cons 'wg-deserialize-help-buffer
              (wg-take-until-fail 'wg-is-readable-p help-xref-stack-item))))))



;;; wconfig restoration

(defun wg-restore-buffer-mark (buf)
  "Set mark and `mark-active' from BUF."
  (if (not wg-restore-mark) (deactivate-mark)
    (set-mark (wg-buf-mark buf))
    (unless (wg-buf-mark-active buf)
      (deactivate-mark))))

(defun wg-restore-default-buffer ()
  "Switch to `wg-default-buffer'."
  (switch-to-buffer wg-default-buffer t))

(defun wg-restore-existing-buffer (buf)
  "Switch to and return BUF's referrent (some live buffer) if it exists."
  (wg-awhen (wg-find-buf-in-buffer-list buf (buffer-list))
    (switch-to-buffer it t)
    (wg-set-buffer-uid-or-error (wg-buf-uid buf))
    it))

(defun wg-restore-file-buffer (buf)
  "Restore BUF by finding its file.  Return the created buffer.
If BUF's file doesn't exist, call `wg-restore-default-buffer'"
  (wg-when-let ((file-name (wg-buf-file-name buf)))
    (cond ((file-exists-p file-name)
           (find-file file-name)
           (rename-buffer (wg-buf-name buf) t)
           (wg-set-buffer-uid-or-error (wg-buf-uid buf))
           (current-buffer))
          (t ;; FIXME: flag BUF for gc
           (message "Attempt to restore nonexistent file %S" file-name)
           (wg-restore-default-buffer)))))

;; (defun wg-restore-special-buffer (buf)
;;   "Restore a buffer with DESERIALIZER-FN."
;;   (wg-when-let ((deserializer (wg-mode-deserializer (wg-buf-major-mode buf))))
;;     (condition-case err
;;         (funcall deserializer)
;;       (error (message "Error deserializing %S: %S" (wg-buf-name buf) err)))
;;     (current-buffer)))

(defun wg-restore-special-buffer (buf)
  "Restore a buffer with DESERIALIZER-FN."
  (wg-when-let ((special-data (wg-buf-special-data buf)))
    (condition-case err
        (funcall (car special-data) buf)
      (error (message "Error deserializing %S: %S" (wg-buf-name buf) err)))
    (wg-set-buffer-uid-or-error (wg-buf-uid buf))
    (current-buffer)))

(defun wg-restore-buffer (buf)
  "Restore BUF and return it."
  (or (wg-restore-existing-buffer buf)
      (wg-restore-special-buffer buf)
      (wg-restore-file-buffer buf)
      (progn (wg-restore-default-buffer) nil)))

(defun wg-restore-window-positions (win &optional window)
  "Restore various positions in WINDOW from their values in WIN."
  (let ((window (or window (selected-window))))
    (wg-with-slots win
        ((win-point wg-win-point)
         (win-start wg-win-start)
         (win-hscroll wg-win-hscroll))
      (set-window-start window win-start t)
      (set-window-hscroll window win-hscroll)
      (set-window-point
       window
       (cond ((not wg-restore-point) win-start)
             ((eq win-point :max) (point-max))
             (t win-point)))
      (when (>= win-start (point-max)) (recenter)))))

(defun wg-restore-window (win)
  "Restore WIN in `selected-window'."
  (let ((buf (wg-find-tracked-buf-by-uid (wg-win-buf-uid win)))
        (selected (selected-window)))
    (when (wg-restore-buffer buf)
      (wg-restore-window-positions win selected)
      (wg-restore-buffer-mark buf))
    (when (wg-win-selected win)
      (setq wg-window-tree-selected-window selected))
    (when (and wg-restore-minibuffer-scroll-window
               (wg-win-minibuffer-scroll win))
      (setq minibuffer-scroll-window selected))
    (wg-dbind (width cols vtype htype)
        (if wg-restore-scroll-bars (wg-win-scroll-bars win)
          '(nil nil nil nil))
      (set-window-scroll-bars selected width vtype htype))
    (wg-dbind (left-width right-width outside-margins)
        (if wg-restore-fringes (wg-win-fringes win)
          '(nil nil nil))
      (set-window-fringes
       selected left-width right-width outside-margins))
    (wg-dbind (left-width . right-width)
        (if wg-restore-margins (wg-win-margins win)
          '(nil . nil))
      (set-window-margins selected left-width right-width))
    (when wg-restore-dedicated
      (set-window-dedicated-p selected (wg-win-dedicated win)))))

(defun wg-restore-window-tree (wtree)
  "Restore WTREE in `selected-frame'."
  (flet ((inner (w) (if (wg-wtree-p w)
                        (wg-with-slots w ((d wg-wtree-dir)
                                          (wlist wg-wtree-wlist))
                          (let ((lastw (wg-last1 wlist)))
                            (dolist (sw wlist) ;; convert to while over cdr
                              (unless (eq sw lastw)
                                (split-window nil (wg-w-size sw d) (not d)))
                              (inner sw))))
                      (wg-restore-window w)
                      (other-window 1))))
    (let ((window-min-width wg-window-min-width)
          (window-min-height wg-window-min-height)
          (wg-window-tree-selected-window nil))
      (delete-other-windows)
      (inner wtree)
      (wg-awhen wg-window-tree-selected-window (select-window it)))))

(defun wg-restore-wconfig (wconfig)
  "Restore WCONFIG in `selected-frame'."
  (wg-barf-on-active-minibuffer)
  (let ((wg-buffer-auto-association-on nil)
        (frame (selected-frame)))
    (wg-with-slots wconfig
        ((left wg-wconfig-left)
         (top wg-wconfig-top)
         (sbars wg-wconfig-vertical-scroll-bars)
         (sbwid wg-wconfig-scroll-bar-width))
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
  (let* ((wl1 (wg-wtree-wlist wt1))
         (l1 (length wl1))
         (d1 (wg-wtree-dir wt1))
         (wl2 (wg-wtree-wlist wt2))
         (l2 (length wl2)))
    (cond ((= l1 l2) wl1)
          ((< l1 l2)
           (cons (wg-minified-copy-of-last-win (wg-rnth (1+ l1) wl2))
                 (if (< (wg-w-size (car wl1) d1)
                        (* 2 (wg-actual-min-size d1)))
                     wl1
                   (cons (wg-w-edge-operation (car wl1) wg-min-edges #'-)
                         (cdr wl1)))))
          ((> l1 l2)
           (append (wg-take wl1 (1- l2))
                   (list (wg-make-wtree
                          :dir d1
                          :edges wg-null-edges
                          :wlist (nthcdr (1- l2) wl1))))))))

(defun wg-morph-win->win (w1 w2 &optional swap)
  "Return a copy of W1 with its edges stepped once toward W2.
When SWAP is non-nil, return a copy of W2 instead."
  (wg-set-edges (wg-copy-win (if swap w2 w1)) (wg-morph-step-edges w1 w2)))

(defun wg-morph-win->wtree (win wtree)
  "Return a new wtree with WIN's edges and WTREE's last two windows."
  (wg-make-wtree
   :dir (wg-wtree-dir wtree)
   :edges (wg-morph-step-edges win wtree)
   :wlist (let ((wg-morph-hsteps 2) (wg-morph-vsteps 2))
            (wg-docar (w (wg-leave (wg-wtree-wlist wtree) 2))
              (wg-morph-win->win (wg-minified-copy-of-last-win w) w)))))

(defun wg-morph-wtree->win (wtree win &optional noswap)
  "Grow the first window of WTREE and its subtrees one step toward WIN.
This eventually wipes WTREE's components, leaving only a window.
Swap WTREE's first actual window for WIN, unless NOSWAP is non-nil."
  (if (wg-win-p wtree) (wg-morph-win->win wtree win (not noswap))
    (wg-make-wtree
     :dir (wg-wtree-dir wtree)
     :edges (wg-morph-step-edges wtree win)
     :wlist (wg-dbind (fwin . wins) (wg-wtree-wlist wtree)
              (cons (wg-morph-wtree->win fwin win noswap)
                    (wg-docar (sw wins)
                      (if (wg-win-p sw) sw
                        (wg-morph-wtree->win sw win t))))))))

(defun wg-morph-wtree->wtree (wt1 wt2)
  "Return a new wtree morphed one step toward WT2 from WT1.
Mutually recursive with `wg-morph-dispatch' to traverse the
structures of WT1 and WT2 looking for discrepancies."
  (let ((d1 (wg-wtree-dir wt1)) (d2 (wg-wtree-dir wt2)))
    (wg-make-wtree
     :dir d2
     :edges (wg-morph-step-edges wt1 wt2)
     :wlist (if (not (eq (wg-wtree-dir wt1) (wg-wtree-dir wt2)))
                (list (wg-minified-copy-of-last-win wt2) wt1)
              (mapcar* #'wg-morph-dispatch
                       (wg-morph-match-wlist wt1 wt2)
                       (wg-wtree-wlist wt2))))))

(defun wg-morph-dispatch (w1 w2)
  "Return a wtree morphed one step toward W2 from W1.
Dispatches on each possible combination of types."
  (cond ((and (wg-win-p w1) (wg-win-p w2))
         (wg-morph-win->win w1 w2 t))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (wg-morph-wtree->wtree w1 w2))
        ((and (wg-win-p w1) (wg-wtree-p w2))
         (wg-morph-win->wtree w1 w2))
        ((and (wg-wtree-p w1) (wg-win-p w2))
         (wg-morph-wtree->win w1 w2))))

(defun wg-morph (to &optional from)
  "Morph from wtree FROM to wtree TO.
Assumes both FROM and TO fit in `selected-frame'."
  (let ((from (or from (wg-window-tree-to-wtree (window-tree))))
        (wg-morph-hsteps
         (wg-morph-determine-steps wg-morph-hsteps wg-morph-terminal-hsteps))
        (wg-morph-vsteps
         (wg-morph-determine-steps wg-morph-vsteps wg-morph-terminal-vsteps))
        (wg-restore-scroll-bars nil)
        (wg-restore-fringes nil)
        (wg-restore-margins nil)
        (wg-restore-point nil)
        (wg-restore-mark nil)
        (truncate-partial-width-windows
         wg-morph-truncate-partial-width-windows)
        (watchdog 0))
    (wg-until (wg-equal-wtrees from to)
      ;; FIXME: remove this `condition-case' form
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



;;; workgroup utils

(defmacro wg-set-workgroup-slot (workgroup accessor value)
  "wg-workgroup slot `setf' wrapper that also sets various 'modified' flags.
WORKGROUP should be a wg-workgroup struct; ACCESSOR should be the
unquoted slot accessor function symbol; And VALUE is the value to
which to set the slot."
  (wg-with-gensyms (wg val)
    `(let ((,wg (wg-get-workgroup ,workgroup))
           (,val ,value))
       (setf (,accessor ,wg) ,val)
       (when wg-flag-modified
         (setf (wg-workgroup-modified ,wg) t)
         (setf (wg-modified) t))
       ,val)))

(defun wg-find-workgroup-by (slotkey value &optional noerror)
  "Return the workgroup on which ACCESSOR returns VALUE or error."
  (let ((accessor (ecase slotkey
                    (:name 'wg-workgroup-name)
                    (:uid  'wg-workgroup-uid))))
    (or (find value (wg-workgroup-list noerror) :test 'equal :key accessor)
        (unless noerror
          (error "No are no workgroups with a %S of %S"
                 accessor value)))))

(defun wg-current-workgroup (&optional noerror frame)
  "Return the current workgroup in FRAME, or error unless NOERROR."
  (or wg-current-workgroup
      (wg-aif (frame-parameter frame 'wg-current-workgroup-uid)
          (wg-find-workgroup-by :uid it noerror)
        (unless noerror (error "No current workgroup in this frame")))))

(defun wg-previous-workgroup (&optional noerror frame)
  "Return the previous workgroup in FRAME, or error unless NOERROR."
  (wg-aif (frame-parameter frame 'wg-previous-workgroup-uid)
      (wg-find-workgroup-by :uid it noerror)
    (unless noerror (error "No previous workgroup in this frame"))))

(defun wg-set-current-workgroup (workgroup &optional frame)
  "Set the current workgroup to WORKGROUP.
WORKGROUP should be a workgroup or nil."
  (set-frame-parameter frame 'wg-current-workgroup-uid
                       (when workgroup (wg-workgroup-uid workgroup))))

(defun wg-set-previous-workgroup (workgroup &optional frame)
  "Set the previous workgroup to WORKGROUP.
WORKGROUP should be a workgroup or nil."
  (set-frame-parameter frame 'wg-previous-workgroup-uid
                       (when workgroup (wg-workgroup-uid workgroup))))

(defun wg-current-workgroup-p (workgroup &optional noerror frame)
  "Return t when WORKGROUP is the current workgroup, nil otherwise."
  (wg-awhen (wg-current-workgroup noerror frame)
    (eq workgroup it)))

(defun wg-previous-workgroup-p (workgroup &optional noerror frame)
  "Return t when WORKGROUP is the previous workgroup, nil otherwise."
  (wg-awhen (wg-previous-workgroup noerror frame)
    (eq workgroup it)))

(defmacro wg-with-current-workgroup (workgroup &rest body)
  "Execute forms in BODY with WORKGROUP temporarily current.
WORKGROUP should be any workgroup identifier accepted by
`wg-get-workgroup'.  The value returned is the last form
in BODY."
  (declare (indent 1))
  `(let ((wg-current-workgroup (wg-get-workgroup ,workgroup)))
     ,@body))

(defun wg-get-workgroup (obj &optional noerror)
  "Return a workgroup from OBJ.
If OBJ is a workgroup, return it.
If OBJ is a string, return the workgroup named OBJ, or error unless NOERROR.
If OBJ is nil, return the current workgroup, or error unless NOERROR."
  (cond ((wg-workgroup-p obj) obj)
        ((stringp obj) (wg-find-workgroup-by :name obj noerror))
        ((null obj) (wg-current-workgroup noerror))
        (t (error "Invalid type: %S" (type-of obj)))))



;;; workgroup parameters

(defun wg-mark-everything-unmodified ()
  "Mark the workgroup-set and all workgroups as unmodified."
  (setf (wg-modified) nil)
  (mapc (lambda (wg) (setf (wg-workgroup-modified wg) nil))
        (wg-workgroup-list t)))

(defun wg-workgroup-parameter (workgroup parameter &optional default)
  "Return WORKGROUP's value for PARAMETER.
If PARAMETER is not found, return DEFAULT which defaults to nil.
WORKGROUP should be accepted by `wg-get-workgroup'."
  (wg-aget (wg-workgroup-parameters (wg-get-workgroup workgroup))
           parameter default))

(defun wg-set-workgroup-parameter (workgroup parameter value)
  "Set WORKGROUP's value of PARAMETER to VALUE.
WORKGROUP should be a value accepted by `wg-get-workgroup'.
Return VALUE."
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-set-workgroup-slot
     workgroup wg-workgroup-parameters
     (wg-aput (wg-workgroup-parameters workgroup) parameter value))
    value))

(defun wg-remove-workgroup-parameter (workgroup parameter)
  "Remove parameter PARAMETER from WORKGROUP destructively."
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-set-workgroup-slot
     workgroup wg-workgroup-parameters
     (wg-aremove (wg-workgroup-parameters workgroup) parameter))))

(defun wg-modified-p ()
  "Return t when `wg-modified' or any workgroup modified parameter all is non-nil."
  (or (wg-modified) (some 'wg-workgroup-modified (wg-workgroup-list t))))

(defun wg-workgroup-names (&optional noerror)
  "Return a list of workgroup names."
  (mapcar 'wg-workgroup-name (wg-workgroup-list noerror)))



;;; workgroup associated buffers

(defun wg-workgroup-associated-buffer-uids (workgroup)
  "Return a new list containing all of WORKGROUP's associated buf uids."
  (append (wg-workgroup-strongly-associated-buffer-uids workgroup)
          (wg-workgroup-weakly-associated-buffer-uids workgroup)))

(defun wg-workgroup-associated-bufs (workgroup)
  "Return a new list containing all of WORKGROUP's associated bufs."
  (mapcar 'wg-find-tracked-buf-by-uid
          (wg-workgroup-associated-buffer-uids workgroup)))

(defun wg-workgroup-bufobj-association-type (workgroup bufobj)
  "Return BUFOBJ's association-type in WORKGROUP, or nil if unassociated."
  (let ((uid (wg-bufobj-uid bufobj)))
    (cond ((member uid (wg-workgroup-strongly-associated-buffer-uids workgroup))
           'strong)
          ((member uid (wg-workgroup-weakly-associated-buffer-uids workgroup))
           'weak)
          (t nil))))

(defun wg-workgroup-strongly-associate-bufobj (workgroup bufobj)
  "Strongly associate BUFOBJ with WORKGROUP."
  (let ((uid (wg-bufobj-uid-or-track bufobj)))
    (wg-removef-p uid (wg-workgroup-weakly-associated-buffer-uids workgroup))
    (wg-pushnew-p uid (wg-workgroup-strongly-associated-buffer-uids workgroup))))

(defun wg-workgroup-weakly-associate-bufobj (workgroup bufobj)
  "Weakly associate BUFOBJ with WORKGROUP."
  (let ((uid (wg-bufobj-uid-or-track bufobj)))
    (wg-removef-p uid (wg-workgroup-strongly-associated-buffer-uids workgroup))
    (wg-pushnew-p uid (wg-workgroup-weakly-associated-buffer-uids workgroup))))

(defun wg-workgroup-associate-bufobj (workgroup bufobj &optional weak)
  "Associate BUFOBJ with WORKGROUP.
WEAK non-nil means weakly associate it.  Otherwise strongly associate it."
  (if weak (wg-workgroup-weakly-associate-bufobj workgroup bufobj)
    (wg-workgroup-strongly-associate-bufobj workgroup bufobj)))

(defun wg-workgroup-dissociate-bufobj (workgroup bufobj)
  "Dissociate BUFOBJ from WORKGROUP."
  (let ((uid (wg-bufobj-uid-or-track bufobj)))
    (or (wg-removef-p uid (wg-workgroup-strongly-associated-buffer-uids workgroup))
        (wg-removef-p uid (wg-workgroup-weakly-associated-buffer-uids workgroup)))))

(defun wg-workgroup-cycle-bufobj-association-type (workgroup bufobj)
  "Cycle the BUFOBJ's association type in WORKGROUP.
If it's strongly associated with the workgroup, weakly associate it.
If it's weakly associated with the workgroup, dissociate it.
If it's unassociated with the workgroup, mark it as strongly associated."
  (case (wg-workgroup-bufobj-association-type workgroup bufobj)
    (strong (wg-workgroup-weakly-associate-bufobj workgroup bufobj))
    (weak (wg-workgroup-dissociate-bufobj workgroup bufobj))
    (otherwise (wg-workgroup-strongly-associate-bufobj workgroup bufobj))))

(defun wg-workgroup-live-buffers (workgroup &optional initial names)
  "Return a list of WORKGROUP's live associated buffers."
  (let ((assoc-bufs (wg-workgroup-associated-bufs workgroup)))
    (remove-if-not
     (lambda (buffer) (wg-find-buffer-in-buf-list buffer assoc-bufs))
     (or initial (buffer-list)))))

(defun wg-workgroup-purge-weakly-associated-buffers (workgroup)
  "Dissociate from WORKGROUP all weakly associated buffers."
  (wg-set-workgroup-slot
   workgroup wg-workgroup-weakly-associated-buffer-uids nil))

(defun wg-auto-dissociate-buffer-hook ()
  "`kill-buffer-hook' that automatically dissociates buffers from workgroups."
  (when wg-dissociate-buffer-on-kill-buffer
    (wg-awhen (wg-current-workgroup t)
      (wg-workgroup-dissociate-bufobj it (current-buffer)))))



;;; filtered buffer-list construction

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

;; FIXME: generalize consumers of this so NAMES isn't necessary
(defun wg-filtered-buffer-list (&optional names workgroup bfl-id initial)
  "Return a filtered buffer-list from NAMES, WORKGROUP, BLF-ID and INITIAL.
NAMES non-nil means return a list of buffer-names instead of buffer objects.
WORKGROUP non-nil should be any workgroup identifier accepted by
`wg-get-workgroup'.
BLF-ID non-nil should be the identifier of a defined buffer-list-filter.
It defaults to `wg-get-buffer-list-filter-val'.
INITIAL non-nil should be an initial buffer-list to filter.  It defaults to
`wg-interesting-buffers'."
  (let ((buffer-list (funcall (wg-get-buffer-list-filter-val
                               (wg-get-buffer-list-filter-id-flexibly bfl-id)
                               'constructor)
                              (wg-get-workgroup workgroup)
                              (or initial (wg-interesting-buffers)))))
    (if names (mapcar 'wg-buffer-name buffer-list)
      (mapcar 'wg-get-buffer buffer-list))))


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
    (remove-if (lambda (b) (wg-find-buffer-in-buffer-list b buffers))
               initial)))


;; buffer-list filtration utils

(defun wg-filter-buffer-list-by-regexp (regexp buffer-list)
  "Return only those buffers in BUFFER-LIST with names matching REGEXP."
  (remove-if-not (lambda (bname) (string-match regexp bname))
                 buffer-list :key 'wg-buffer-name))

(defun wg-filter-buffer-list-by-root-dir (root-dir buffer-list)
  "Return only those buffers in BUFFER-LIST visiting files undo ROOT-DIR."
  (remove-if-not (lambda (f) (when f (wg-file-under-root-path-p root-dir f)))
                 buffer-list :key 'wg-buffer-file-name))

(defun wg-filter-buffer-list-by-major-mode (major-mode buffer-list)
  "Return only those buffers in BUFFER-LIST in major-mode MAJOR-MODE."
  (remove-if-not (lambda (mm) (eq mm major-mode))
                 buffer-list :key 'wg-buffer-major-mode))


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
    (or (wg-aget bso command)
        (wg-aget bso 'default)
        (wg-aget wg-buffer-list-filter-order-alist command)
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



;;; workgroup working-wconfig and wconfig undo/redo

(wg-defstruct wg workgroup-state
  (undo-pointer)
  (undo-list))

(defun wg-workgroup-state-table (&optional frame)
  "Return FRAME's workgroup table, creating it first if necessary."
  (or (frame-parameter frame 'wg-workgroup-state-table)
      (let ((wtree (make-hash-table :test 'equal)))
        (set-frame-parameter frame 'wg-workgroup-state-table wtree)
        wtree)))

(defun wg-get-workgroup-state (workgroup &optional frame)
  "Return FRAME's WORKGROUP's state table."
  (let ((uid (wg-workgroup-uid workgroup))
        (wt (wg-workgroup-state-table frame)))
    (or (gethash uid wt)
        (puthash
         uid
         (wg-make-workgroup-state
          :undo-pointer 0
          :undo-list
          (list (or (wg-workgroup-most-recent-working-wconfig workgroup)
                    (wg-workgroup-base-wconfig workgroup))))
         wt))))

(defmacro wg-with-undo (workgroup spec &rest body)
  "Bind WORKGROUP's undo state to SPEC and eval BODY."
  (declare (indent 2))
  (wg-dbind (state undo-pointer undo-list) spec
    `(let* ((,state (wg-get-workgroup-state ,workgroup))
            (,undo-pointer (wg-workgroup-state-undo-pointer ,state))
            (,undo-list (wg-workgroup-state-undo-list ,state)))
       ,@body)))

(defun wg-flag-just-exited-minibuffer ()
  "Added to `minibuffer-exit-hook'."
  (setq wg-just-exited-minibuffer t))

(defun wg-flag-window-configuration-changed ()
  "Set `wg-window-configuration-changed' to t unless the
minibuffer was just exited.  Added to
`window-configuration-change-hook'."
  (if wg-just-exited-minibuffer
      (setq wg-just-exited-minibuffer nil)
    (setq wg-window-configuration-changed t)))

(defun wg-unflag-undoify-window-configuration-change ()
  "Set `wg-undoify-window-configuration-change' to nil, exempting
from undoification any window-configuration changes caused by the
current command."
  (setq wg-undoify-window-configuration-change nil))

(defun wg-set-workgroup-working-wconfig (workgroup wconfig)
  "Set the working-wconfig of WORKGROUP to WCONFIG."
  (wg-set-workgroup-slot
   workgroup wg-workgroup-most-recent-working-wconfig wconfig)
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (setcar (nthcdr undo-pointer undo-list) wconfig)))

(defun wg-add-wconfig-to-undo-list (workgroup wconfig)
  "Add WCONFIG to WORKGROUP's undo list, truncating its future if necessary."
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (wg-set-workgroup-slot
     workgroup wg-workgroup-most-recent-working-wconfig wconfig)
    (let ((undo-list (cons wconfig (nthcdr undo-pointer undo-list))))
      (when (and wg-wconfig-undo-list-max
                 (> (length undo-list) wg-wconfig-undo-list-max))
        (setq undo-list (wg-take undo-list wg-wconfig-undo-list-max)))
      (setf (wg-workgroup-state-undo-list state) undo-list)
      (setf (wg-workgroup-state-undo-pointer state) 0))))

(defun wg-workgroup-working-wconfig (workgroup)
  "Return WORKGROUP's working-wconfig, which is its current undo state.
If WORKGROUP is the current workgroup in `selected-frame', set
its working wconfig to `wg-frame-to-wconfig' and return the
updated wconfig.  Otherwise, return the current undo state
unupdated."
  (if (wg-current-workgroup-p workgroup t)
      (wg-set-workgroup-working-wconfig workgroup (wg-frame-to-wconfig))
    (wg-with-undo workgroup (state undo-pointer undo-list)
      (nth undo-pointer undo-list))))

(defun wg-update-current-workgroup-working-wconfig ()
  "Update WORKGROUP's working-wconfig with `wg-frame-to-wconfig'."
  (wg-awhen (wg-current-workgroup t)
    (wg-set-workgroup-working-wconfig it (wg-frame-to-wconfig))))

(defun wg-restore-wconfig-undoably (wconfig &optional noundo)
  "Restore WCONFIG in `selected-frame', saving undo information."
  (when noundo (wg-unflag-undoify-window-configuration-change))
  (wg-update-current-workgroup-working-wconfig)
  (wg-restore-wconfig wconfig))

(defun wg-increment-workgroup-undo-pointer (workgroup increment)
  "Increment WORKGROUP's undo-pointer by INCREMENT.
Also restore the wconfig at the incremented undo-pointer if
WORKGROUP is current."
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (let ((new-pointer (+ undo-pointer increment)))
      (when (wg-within new-pointer 0 (length undo-list))
        (when (wg-current-workgroup-p workgroup t)
          (wg-restore-wconfig-undoably (nth new-pointer undo-list) t))
        (setf (wg-workgroup-state-undo-pointer state) new-pointer)))))

(defun wg-undoify-window-configuration-change ()
  "Conditionally `wg-add-wconfig-to-undo-list'.
Added to `post-command-hook'."
  (when (and
         ;; When the window config has changed,
         wg-window-configuration-changed
         ;; and undoification is still on for the current command
         wg-undoify-window-configuration-change
         ;; and the change didn't occur while the minibuffer is active,
         (zerop (minibuffer-depth)))
    ;; and there's a current workgroup,
    (wg-when-let ((wg (wg-current-workgroup t)))
      ;; add the current wconfig to that workgroup's undo list:
      (wg-add-wconfig-to-undo-list wg (wg-frame-to-wconfig))))
  ;; Reset both flags no matter what:
  (setq wg-window-configuration-changed nil
        wg-undoify-window-configuration-change t))

(defun wg-update-working-wconfig-before-command ()
  "Update the current workgroup's working-wconfig before
`wg-commands-that-alter-window-configs'. Added to
`pre-command-hook'."
  (when (gethash this-command wg-commands-that-alter-window-configs)
    (wg-update-current-workgroup-working-wconfig)))

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
    (delete nil (mapcar 'wg-restore-buffer
                        (wg-workgroup-associated-bufs workgroup)))))

(defun wg-restore-workgroup (workgroup)
  "Restore WORKGROUP in `selected-frame'."
  (when wg-restore-associated-buffers
    (wg-restore-workgroup-associated-buffers-internal workgroup))
  (let (wg-flag-modified)
    (wg-restore-wconfig-undoably
     (wg-workgroup-working-wconfig workgroup) t)))



;;; workgroup-list ops

(defun wg-set-workgroup-list (new-workgroup-list)
  "Set `wg-workgroup-set's workgroup-list param to NEW-WORKGROUP-LIST."
  (setf (wg-workgroup-set-workgroup-list (wg-current-workgroup-set))
        new-workgroup-list))

(defun wg-delete-workgroup (workgroup)
  "Remove WORKGROUP from `wg-workgroup-list'.
Also delete all references to it by `wg-workgroup-state-table',
`wg-current-workgroup' and `wg-previous-workgroup'."
  (dolist (frame (frame-list))
    (remhash (wg-workgroup-uid workgroup) (wg-workgroup-state-table frame))
    (when (wg-current-workgroup-p workgroup t frame)
      (wg-set-current-workgroup nil frame))
    (when (wg-previous-workgroup-p workgroup t frame)
      (wg-set-previous-workgroup nil frame)))
  (wg-set-workgroup-list (remove workgroup (wg-workgroup-list)))
  (setf (wg-modified) t)
  workgroup)

(defun wg-add-workgroup (workgroup &optional pos)
  "Add WORKGROUP to `wg-workgroup-list'.
If a workgroup with the same name exists, overwrite it."
  (wg-awhen (wg-find-workgroup-by :name (wg-workgroup-name workgroup) t)
    (unless pos (setq pos (position it (wg-workgroup-list))))
    (wg-delete-workgroup it))
  (wg-set-workgroup-list (wg-insert-elt workgroup (wg-workgroup-list t) pos))
  (setf (wg-modified) t)
  workgroup)

(defun wg-check-and-add-workgroup (workgroup)
  "Add WORKGROUP to `wg-workgroup-list'.
Query to overwrite if a workgroup with the same name exists."
  (let ((name (wg-workgroup-name workgroup))
        (uid (wg-workgroup-uid workgroup)))
    (when (wg-find-workgroup-by :uid uid t)
      (error "A workgroup with uid %S already exists" uid))
    (when (wg-find-workgroup-by :name name t)
      (unless (or wg-no-confirm-on-destructive-operation
                  (y-or-n-p (format "%S exists. Overwrite? " name)))
        (error "Cancelled"))))
  (wg-add-workgroup workgroup))

(defun wg-make-and-add-workgroup (name)
  "Create a workgroup named NAME and add it with `wg-check-and-add-workgroup'."
  (wg-check-and-add-workgroup
   (wg-make-workgroup
    :name name
    :base-wconfig (if (wg-current-workgroup t)
                      (wg-make-blank-wconfig)
                    (wg-frame-to-wconfig)))))

(defun wg-get-or-create-workgroup (workgroup)
  "Return the workgroup identified by WORKGROUP.
If WORKGROUP is a string that isn't the name of an
existing workgroup, offer to create it."
  (or (wg-get-workgroup workgroup t)
      (if (stringp workgroup)
          (when (or wg-no-confirm-on-create-workgroup
                    (y-or-n-p (format "%S doesn't exist.  Create it? "
                                      workgroup)))
            (wg-make-and-add-workgroup workgroup))
        ;; Call this again for its informative error message:
        (wg-get-workgroup workgroup))))

(defun wg-cyclic-offset-workgroup (workgroup n)
  "Offset WORKGROUP's position in `wg-workgroup-list' by N."
  (let ((workgroup-list (wg-workgroup-list)))
    (unless (member workgroup workgroup-list)
      (error "Workgroup isn't present in `wg-workgroup-list'."))
    (wg-set-workgroup-list
     (wg-cyclic-offset-elt workgroup workgroup-list n))
    (setf (wg-modified) t)))

(defun wg-swap-workgroups-in-workgroup-list (workgroup1 workgroup2)
  "Swap the positions of WORKGROUP1 and WORKGROUP2 in `wg-workgroup-list'."
  (let ((workgroup-list (wg-workgroup-list)))
    (when (eq workgroup1 workgroup2)
      (error "Can't swap a workgroup with itself"))
    (unless (and (memq workgroup1 workgroup-list)
                 (memq workgroup2 workgroup-list))
      (error "Both workgroups aren't present in `wg-workgroup-list'."))
    (wg-set-workgroup-list (wg-util-swap workgroup1 workgroup2 workgroup-list))
    (setf (wg-modified) t)))

(defun wg-cyclic-nth-from-workgroup (workgroup &optional n)
  "Return the workgroup N places from WORKGROUP in `wg-workgroup-list'."
  (wg-cyclic-nth-from-elt workgroup (wg-workgroup-list) (or n 1)))



;;; mode-line

;; FIXME: add mouse-over text explaining mode-line elements

(defun wg-mode-line-decor (decor-symbol)
  "Return DECOR-SYMBOL's decoration string in `wg-mode-line-decor-alist'."
  (wg-aget wg-mode-line-decor-alist decor-symbol))

(defun wg-mode-line-buffer-association-indicator (workgroup)
  "Return a string indicating `current-buffer's association-type in WORKGROUP."
  (wg-mode-line-decor
   (case (wg-workgroup-bufobj-association-type workgroup (current-buffer))
     (strong 'strongly-associated)
     (weak 'weakly-associated)
     (otherwise 'unassociated))))

;; asdf

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
                (:mode (wg-mode-line-decor (if (wg-modified) 'modified 'unmodified)))
                (:mode (wg-mode-line-decor
                        (if (wg-workgroup-modified wg) 'modified 'unmodified)))
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

;; FIXME: also send messages to a *workgroups log* buffer

;; FIXME: add a `wg-debug-mode' var and a `wg-error' wrapper such that when
;; `wg-debug-mode' is on `wg-error' throws the error, but when it's off,
;; `wg-error' just logs the error to *workgroups log*

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

;; (defun wg-buffer-display (buffer index)
;;   "Return display string for BUFFER. INDEX is ignored."
;;   (if (not buffer) "No buffers"
;;     (let ((buffer (wg-get-buffer buffer)))
;;       (wg-element-display
;;        buffer (format "%s" (buffer-name buffer))
;;        (lambda (b) (eq (wg-get-buffer b) (current-buffer)))
;;        (lambda (b) nil)))))

(defun wg-buffer-display (buffer index)
  "Return display string for BUFFER. INDEX is ignored."
  (if (not buffer) "No buffers"
    (wg-element-display
     (wg-get-buffer buffer)
     (format "%s" (wg-buffer-name buffer))
     (lambda (b) (eq (wg-get-buffer b) (current-buffer)))
     (lambda (b) nil))))

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
    (:msg (wg-workgroup-name (wg-get-workgroup workgroup)))
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
  (wg-with-undo workgroup (state undo-pointer undo-list)
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
          ((not (wg-get-buffer buffer))
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
      (let ((new-name (format "wg%s" (incf index))))
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
    (when (and (eq workgroup current) (not noerror))
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
   (wg-aif (wg-get-workgroup workgroup t)
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
  (interactive (list nil (wg-read-new-workgroup-name)))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (clone (wg-copy-workgroup workgroup)))
    (wg-set-workgroup-slot clone wg-workgroup-name name)
    (wg-set-workgroup-slot clone wg-workgroup-uid (wg-generate-uid))
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
  (interactive (list nil))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (to (or (wg-previous-workgroup t)
                 (wg-cyclic-nth-from-workgroup workgroup))))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-working-wconfig workgroup))
    (wg-delete-workgroup workgroup)
    (if (eq workgroup to) (wg-restore-wconfig (wg-make-blank-wconfig))
      (wg-switch-to-workgroup to))
    (wg-fontified-message
      (:cmd "Killed: ")
      (:cur (wg-workgroup-name workgroup)) "  "
      (wg-workgroup-list-display))))

(defun wg-kill-ring-save-base-wconfig (workgroup)
  "Save WORKGROUP's base wconfig to `wg-wconfig-kill-ring'."
  (interactive (list nil))
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-base-wconfig workgroup))
    (wg-fontified-message
      (:cmd "Saved: ")
      (:cur (wg-workgroup-name workgroup))
      (:cur "'s ")
      (:msg "base wconfig to the kill ring"))))

(defun wg-kill-ring-save-working-wconfig (workgroup)
  "Save WORKGROUP's working-wconfig to `wg-wconfig-kill-ring'."
  (interactive (list nil))
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-working-wconfig workgroup))
    (wg-fontified-message
      (:cmd "Saved: ")
      (:cur (wg-workgroup-name workgroup))
      (:cur "'s ")
      (:msg "working-wconfig to the kill ring"))))

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
  (interactive (list nil))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (bufs (save-window-excursion
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
  (interactive (list nil))
  (let ((workgroup (wg-get-workgroup workgroup)))
    (unless (or wg-no-confirm-on-destructive-operation
                (y-or-n-p "Really delete all other workgroups? "))
      (error "Cancelled"))
    (dolist (w (wg-workgroup-list))
      (unless (eq w workgroup)
        (wg-delete-workgroup w)))
    (unless (wg-current-workgroup-p workgroup t)
      (wg-switch-to-workgroup workgroup))
    (wg-fontified-message
      (:cmd "Deleted: ")
      (:msg "All workgroups but ")
      (:cur (wg-workgroup-name workgroup)))))



;;; workgroup updating and reverting commands

(defun wg-update-workgroup (workgroup)
  "Set WORKGROUP's base wconfig to its working-wconfig in `selected-frame'."
  (interactive (list nil))
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-set-workgroup-slot
     workgroup wg-workgroup-base-wconfig
     (wg-workgroup-working-wconfig workgroup))
    (wg-fontified-message
      (:cmd "Updated: ")
      (:cur (wg-workgroup-name workgroup)))))

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
  (interactive (list nil))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (base (wg-workgroup-base-wconfig workgroup)))
    (if (wg-current-workgroup-p workgroup t)
        (wg-restore-wconfig-undoably base)
      (wg-add-wconfig-to-undo-list workgroup base))
    (wg-fontified-message
      (:cmd "Reverted: ")
      (:cur (wg-workgroup-name workgroup)))))

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
  (wg-cyclic-offset-workgroup (wg-get-workgroup workgroup) (or n -1))
  (wg-fontified-message
    (:cmd "Offset left: ")
    (wg-workgroup-list-display)))

(defun wg-offset-workgroup-right (workgroup &optional n)
  "Offset WORKGROUP rightward in `wg-workgroup-list' cyclically."
  (interactive (list nil current-prefix-arg))
  (wg-cyclic-offset-workgroup (wg-get-workgroup workgroup) (or n 1))
  (wg-fontified-message
    (:cmd "Offset right: ")
    (wg-workgroup-list-display)))



;;; undo/redo commands

(defun wg-undo-wconfig-change (workgroup)
  "Undo a change to the current workgroup's window-configuration."
  (interactive (list nil))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (undone? (wg-increment-workgroup-undo-pointer workgroup 1)))
    (wg-fontified-message
      (:cmd "Undo: ")
      (wg-undo-timeline-display workgroup)
      (:cur (if undone? "" "  No more undo info")))))

(defun wg-redo-wconfig-change (workgroup)
  "Redo a change to the current workgroup's window-configuration."
  (interactive (list nil))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (redone? (wg-increment-workgroup-undo-pointer workgroup -1)))
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
    (wg-workgroup-dissociate-bufobj (wg-current-workgroup) buffer)
    (wg-bury-buffer buffer)))

(defun wg-associate-buffer-with-workgroup (workgroup buffer &optional weak)
  "Associate BUFFER with WORKGROUP.
WEAK non-nil means weakly associate BUFFER."
  (interactive (list nil (current-buffer) current-prefix-arg))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (bname (wg-buffer-name buffer))
         (wgname (wg-workgroup-name workgroup)))
    (if (wg-workgroup-associate-bufobj workgroup buffer weak)
        (wg-message "%s-associated %S with %s"
                    (if weak "Weakly" "Strongly") bname wgname)
      (wg-message "%S is already associated with %s" bname wgname))))

(defun wg-dissociate-buffer-from-workgroup (workgroup buffer)
  "Dissociate BUFFER from WORKGROUP."
  (interactive (list nil (current-buffer)))
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-message
     (if (wg-workgroup-dissociate-bufobj workgroup buffer)
         "Dissociated %S from %s" "%S isn't associated with %s")
     (wg-buffer-name buffer)
     (wg-workgroup-name workgroup))))

(defun wg-restore-workgroup-associated-buffers (workgroup)
  "Restore all the buffers associated with WORKGROUP that can be restored."
  (interactive (list nil))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (restored-buffers (wg-restore-workgroup-associated-buffers-internal
                            workgroup)))
    (wg-fontified-message
      (:cmd "Restored: ")
      (wg-buffer-list-display restored-buffers))))

(defun wg-cycle-buffer-association-type ()
  "Cycle the current buffer's association type in the current workgroup.
See `wg-workgroup-cycle-bufobj-association-type' for details."
  (interactive)
  (let ((workgroup (wg-current-workgroup))
        (buffer (current-buffer)))
    (wg-workgroup-cycle-bufobj-association-type workgroup buffer)
    (force-mode-line-update)
    (wg-fontified-message
      (:cur (buffer-name buffer))
      (:cmd (case (wg-workgroup-bufobj-association-type workgroup buffer)
              (strong " strongly associated with ")
              (weak " weakly associated with ")
              (otherwise " unassociated with ")))
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-purge-weakly-associated-buffers (workgroup)
  "Dissociate from the current workgroup all weakly associated buffers."
  (interactive (list nil))
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-workgroup-purge-weakly-associated-buffers workgroup)
    (wg-fontified-message
      (:cmd "Remaining buffers: ")
      (wg-buffer-list-display (wg-workgroup-live-buffers workgroup)))))



;;; misc commands

(defun wg-rename-workgroup (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list nil (wg-read-new-workgroup-name "New name: ")))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (oldname (wg-workgroup-name workgroup)))
    (wg-set-workgroup-slot workgroup wg-workgroup-name newname)
    (wg-fontified-message
      (:cmd "Renamed: ")
      (:cur oldname)
      (:msg " to ")
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-reset-frame (frame)
  "Reset Workgroups' frame-parameters in FRAME to nil."
  (set-frame-parameter frame 'wg-workgroup-state-table nil)
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
  (setq wg-current-workgroup-set nil
        wg-visited-file-name nil)
  (setf (wg-modified) nil)
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
   (list (if (or current-prefix-arg (not wg-visited-file-name))
             (read-file-name "File to save in: ")
           wg-visited-file-name)))
  (if (and (not force) (not (wg-modified-p)))
      (wg-message "(No workgroups need to be saved)")
    (setq wg-visited-file-name file)
    (wg-update-current-workgroup-working-wconfig)
    (wg-write-sexp-to-file (wg-current-workgroup-set) file)
    (wg-mark-everything-unmodified)
    (wg-fontified-message (:cmd "Wrote: ") (:file file))))

(defun wg-query-for-save ()
  "Query for save when `wg-modified' is non-nil."
  (or (not (wg-modified-p))
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
        (error "%S is not a Workgroups file." filename))
      (wg-reset t)
      (setq wg-current-workgroup-set sexp wg-visited-file-name filename))
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
;; TODO: Add complex window creation commands
;;
;; TODO: Add window splitting, deletion and locking commands
;;
;; FIXME: These are half-hearted.  Clean them up, and allow specification of the
;; window-tree depth at which to operate.

(defun wg-backward-transpose-window (workgroup offset)
  "Move `selected-window' backward by OFFSET in its wlist."
  (interactive (list nil (or current-prefix-arg -1)))
  (wg-restore-wconfig-undoably
   (wg-wconfig-move-window
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    offset)))

(defun wg-transpose-window (workgroup offset)
  "Move `selected-window' forward by OFFSET in its wlist."
  (interactive (list nil (or current-prefix-arg 1)))
  (wg-restore-wconfig-undoably
   (wg-wconfig-move-window
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    offset)))

(defun wg-reverse-frame-horizontally (workgroup)
  "Reverse the order of all horizontally split wtrees."
  (interactive (list nil))
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup)))))

(defun wg-reverse-frame-vertically (workgroup)
  "Reverse the order of all vertically split wtrees."
  (interactive (list nil))
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    t)))

(defun wg-reverse-frame-horizontally-and-vertically (workgroup)
  "Reverse the order of all wtrees."
  (interactive (list nil))
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    'both)))



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
    (wg-workgroup-dissociate-bufobj (wg-current-workgroup) buffer)
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
    (wg-workgroup-associate-bufobj (wg-current-workgroup) buffer)
    (wg-set-current-matches
     (wg-rotate-list (wg-filtered-buffer-list t) pos) mode)))

(defun wg-minibuffer-mode-purge-weakly-associated-buffers ()
  "Purge weakly associated buffers and update the current matches."
  (interactive)
  (wg-workgroup-purge-weakly-associated-buffers (wg-current-workgroup))
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
;;          (wg-workgroup-associate-bufobj workgroup buffer assoc t))
;;         ((functionp assoc)
;;          (wg-auto-associate-buffer-helper
;;           workgroup buffer (funcall assoc workgroup buffer)))
;;         (t nil)))

(defun wg-auto-associate-buffer-helper (workgroup buffer assoc)
  "Associate BUFFER with WORKGROUP based on ASSOC.
See `wg-buffer-auto-association' for allowable values of ASSOC."
  (cond ((memq assoc '(weak strong))
         (wg-workgroup-associate-bufobj workgroup buffer (eq assoc 'weak)))
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
                  (kbd "_")          'wg-purge-weakly-associated-buffers
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
                  (kbd "C-c _")     'wg-minibuffer-mode-purge-weak-associated-buffers
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

;; (defun wg-add-or-remove-workgroups-hooks (remove)
;;   "Add or remove all of Workgroups' hooks, depending on REMOVE."
;;   (wg-add-or-remove-hooks
;;    remove
;;    'kill-emacs-query-functions 'wg-save-workgroups-on-emacs-exit
;;    'window-configuration-change-hook 'wg-flag-window-configuration-changed
;;    'pre-command-hook 'wg-update-working-wconfig-before-command
;;    'post-command-hook 'wg-undoify-window-configuration-change
;;    'minibuffer-setup-hook 'wg-turn-on-minibuffer-mode
;;    'minibuffer-exit-hook 'wg-unflag-undoify-window-configuration-change
;;    'minibuffer-exit-hook 'wg-turn-off-minibuffer-mode
;;    'ido-make-buffer-list-hook 'wg-set-ido-buffer-list
;;    'iswitchb-make-buflist-hook 'wg-set-iswitchb-buffer-list
;;    'kill-buffer-hook 'wg-auto-dissociate-buffer-hook))

(defun wg-add-or-remove-workgroups-hooks (remove)
  "Add or remove all of Workgroups' hooks, depending on REMOVE."
  (wg-add-or-remove-hooks
   remove
   'kill-emacs-query-functions 'wg-save-workgroups-on-emacs-exit
   'window-configuration-change-hook 'wg-flag-window-configuration-changed
   'pre-command-hook 'wg-update-working-wconfig-before-command
   'post-command-hook 'wg-undoify-window-configuration-change
   'minibuffer-setup-hook 'wg-turn-on-minibuffer-mode
   'minibuffer-exit-hook 'wg-flag-just-exited-minibuffer
   'minibuffer-exit-hook 'wg-turn-off-minibuffer-mode
   'ido-make-buffer-list-hook 'wg-set-ido-buffer-list
   'iswitchb-make-buflist-hook 'wg-set-iswitchb-buffer-list
   'kill-buffer-hook 'wg-auto-dissociate-buffer-hook))

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
