;;; workgroups.el --- Workgroups For Windows (for Emacs)
;;
;; Copyright (C) 2010, 2011 tlh
;;
;; Author: tlh <thunkout at gmail dot com>
;; Keywords: session management window-configuration persistence
;; Homepage: https://github.com/tlh/workgroups.el
;; Version   1.0.0

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
;; Workgroups is an Emacs session manager providing window-configuration
;; persistence, switching, undo/redo, killing/yanking, animated morphing,
;; per-workgroup buffer-lists, and more.
;;
;;
;; Installation and Usage
;; ----------------------
;; See the file README.md in this directory, or at
;;
;;   https://github.com/tlh/workgroups.el
;;
;;
;; Symbol naming conventions
;; -------------------------
;; * bufobj always refers to a Workgroups buffer (wg-buf) or an Emacs buffer
;; * W always refers to a Workgroups window (wg-win) or window tree (wg-wtree).
;; * SW always refers to a sub-window or sub-window-tree of a wtree.
;; * WL always refers to the window list of a wtree.
;; * LN, TN, RN and BN always refer to the LEFT, TOP, RIGHT and BOTTOM
;;   edges of an edge list, where N is a differentiating integer.
;; * LS, HS, LB and HB always refer to the LOW-SIDE, HIGH-SIDE, LOW-BOUND
;;   and HIGH-BOUND of a bounds list.  See `wg-with-bounds'.
;;
;;
;;; Code:


(require 'cl)
(require 'workgroups-utils)
(require 'workgroups-pickel)

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

;; FIXME: add default directory and file customizations, and a save
;; customization like: "<jlf> or maybe have wg-save-file-name be one of 'auto
;; (as above), 'confirm-auto (as above, but overridable), or 'ask (always ask)
;; [10:30]"

;; FIXME: add autosave, and an autosave timer.  Autosaves should not save over
;; the currently visited file.  And if there's an autosave file newer than the
;; visited file, it should ask the user if they want to load the autosave.  Can
;; this be piggybacked on Emacs' existing autosave?

(defcustom wg-default-session-file
  (concat user-emacs-directory "workgroups-session")
  "FIXME: nix or docstring this"
  :type 'file
  :group 'workgroups)

(defcustom wg-switch-to-first-workgroup-on-find-session-file t
  "Non-nil means switch to the first workgroup in a session file
when it's found with `wg-find-session-file'."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-emacs-exit-save-behavior 'query
  "Determines save behavior on Emacs exit.
Possible values:
`nosave'   Exit Emacs without saving changes
`save'     Call `wg-save-session' when there are unsaved changes
`query'    Query the user if there are unsaved changes"
  :type 'symbol
  :group 'workgroups)

(defcustom wg-workgroups-mode-exit-save-behavior 'query
  "Determines save behavior on `workgroups-mode' exit.
Possible values:
`nosave'   Exit `workgroups-mode' without saving changes
`save'     Call `wg-save-session' when there are unsaved changes
`query'    Query the user if there are unsaved changes"
  :type 'symbol
  :group 'workgroups)


;; minibuffer customization

(defcustom wg-confirm-on-get-workgroup-create nil
  "Non-nil means request confirmation before creating a new
workgroup when `wg-get-workgroup-create' is called with a string
that doesn't name an existing workgroup."
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FIXME:
;;
;; Only set `wg-workgroup-base-wconfig' on `wg-write-session-file' or
;; `delete-frame' and only with the most recently changed working-wconfig.
;; Then, since it's not overwritten on every call to
;; `wg-workgroup-working-wconfig', its restoration can be retried after manually
;; recreating buffers that couldn't be restored.  So it takes over the
;; 'incorrect restoration' portion of the base wconfig's duty.  All that leaves
;; to base wconfigs is that they're a saved wconfig the user felt was important.
;; So why not allow more of of them?  A workgroup could store an unlimited
;; number of saved wconfigs.
;;
;; TODO:
;;
;;   * Write new commands for restoring stashed wconfigs
;;
;;   * Add this message on improper restoration of `base-wconfig':
;;
;;       "Unable to restore 'buf1', 'buf2'... Hit C-whatever to retry after
;;        manually recreating these buffers."
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; workgroup restoration customization

;; TODO: possibly add `buffer-file-coding-system', `text-scale-mode-amount'
(defcustom wg-buffer-local-variables-alist
  `((major-mode nil wg-deserialize-buffer-major-mode)
    (mark-ring wg-serialize-buffer-mark-ring wg-deserialize-buffer-mark-ring)
    (left-fringe-width nil nil)
    (right-fringe-width nil nil)
    (fringes-outside-margins nil nil)
    (left-margin-width nil nil)
    (right-margin-width nil nil)
    (vertical-scroll-bar nil nil))
  "Alist mapping buffer-local variable symbols to serdes functions.

The `car' of each entry should be a buffer-local variable symbol.

The `cadr' of the entry should be either nil or a function of no
arguments.  If nil, the variable's value is used as-is, and
should have a readable printed representation.  If a function,
`funcall'ing it should yield a serialization of the value of the
variable.

The `caddr' of the entry should be either nil or a function of
one argument.  If nil, the serialized value from above is
assigned to the variable as-is.  It a function, `funcall'ing it
on the serialized value from above should do whatever is
necessary to properly restore the original value of the variable.
For example, in the case of `major-mode' it should funcall the
value (a major-mode function symbol) rather than just assigning
it to `major-mode'."
  :type 'alist
  :group 'workgroups)

(defcustom wg-special-buffer-serdes-functions
  '(wg-serialize-Info-buffer
    wg-serialize-help-buffer
    wg-serialize-ielm-buffer
    wg-serialize-shell-buffer
    wg-serialize-eshell-buffer
    wg-serialize-term-buffer)
  "List of functions providing special buffer serialization/deserialization.
An entry should be either a function symbol or a lambda, and should
accept a single Emacs buffer object as an argument.

When a buffer is to be serialized, it is passed to each of these
functions in turn until one returns non-nil, or the list ends.  A
return value of nil indicates that the function can't handle
buffers of that type.  A non-nil return value indicates that it
can.  The first non-nil return value becomes the buffer's special
serialization data.  The return value should be a cons, with a
deserialization function (a function symbol or a lambda) as the car,
and any other serialization data as the cdr.

When it comes time to deserialize the buffer, the deserialization
function (the car of the cons mentioned above) is passed the
wg-buf object, from which it should restore the buffer.  The
special serialization data itself can be accessed
with (cdr (wg-buf-special-data <wg-buf>)).  The deserialization
function must return the restored Emacs buffer object.

See the definitions of the functions in this list for examples of
how to write your own."
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

(defcustom wg-restore-window-dedicated-p t
  "Non-nil means restore `window-dedicated-p' on workgroup restore."
  :type 'boolean
  :group 'workgroups)


;; wconfig undo/redo customization

(defcustom wg-wconfig-undo-list-max 20
  "Number of past window configs to retain for undo."
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
  "List of buffer list filter definitions.
Each entry should be a list containing an identifier symbol, a
prompt string, and a function form that's funcall'd to produce
the filtered buffer-list.

The prompt string is displayed as part of the minibuffer prompt
when its filter is active.

The function form should be either a function-symbol or a lambda, and
should take two arguments: a workgroup and a list of live Emacs
buffers.  The function should return a new list of live buffers,
typically by filtering its second argument in some way.

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

(defcustom wg-mode-line-use-faces nil
  "Non-nil means use faces in the mode-line display.
It can be tricky to choose faces that are visible in both active
and inactive mode-lines, so this feature defaults to off."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-mode-line-decor-left-brace "("
  "String displayed at the left of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-right-brace ")"
  "String displayed at the right of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-divider ":"
  "String displayed between elements of the mode-line display."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-strongly-associated
  #("@" 0 1 (help-echo "This buffer is strongly associated with the \
current workgroup"))
  "Indicates that a buffer is strongly associated with the current workgroup."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-weakly-associated
  #("~" 0 1 (help-echo "This buffer is weakly associated with the \
current workgroup"))
  "Indicates that a buffer is weakly associated with the current workgroup."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-unassociated
  #("-" 0 1 (help-echo "This buffer is unassociated with the \
current workgroup"))
  "Indicates that a buffer is unassociated with the current workgroup."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-session-modified
  #("*" 0 1 (help-echo "The session is modified"))
  "Indicates that the session is modified."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-session-unmodified
  #("-" 0 1 (help-echo "The session is unmodified"))
  "Indicates that the session is unmodified."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-workgroup-modified
  #("*" 0 1 (help-echo "The current workgroup is modified"))
  "Indicates that the current workgroup is modified."
  :type 'string
  :group 'workgroups)

(defcustom wg-mode-line-decor-workgroup-unmodified
  #("-" 0 1 (help-echo "The current workgroup is unmodified"))
  "Indicates that the current workgroup is unmodified."
  :type 'string
  :group 'workgroups)


;; display customization

(defcustom wg-use-faces t
  "Non-nil means use faces in various displays."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-time-format "%H:%M:%S %A, %B %d %Y"
  "Format string for time display.  Passed to `format-time-string'."
  :type 'string
  :group 'workgroups)

(defcustom wg-display-battery t
  "Non-nil means include `battery', when available, in the time display."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-list-display-decor-left-brace "( "
  "String displayed to the left of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-right-brace " )"
  "String displayed to the right of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-divider " | "
  "String displayed between elements of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-current-left "-<{ "
  "String displayed to the left of the current element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-current-right " }>-"
  "String displayed to the right of the current element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-previous-left "< "
  "String displayed to the left of the previous element of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-list-display-decor-previous-right " >"
  "String displayed to the right of the previous element of the list display."
  :type 'string
  :group 'workgroups)



;;; vars

(defvar workgroups-mode-map nil
  "Workgroups Mode's keymap")

(defvar wg-current-session nil
  "Current session object.")

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
exiting the minibuffer.  This is ugly, but necessary.  It may
seem like we could just null out
`wg-undoify-window-configuration-change' in
`minibuffer-exit-hook', but that also prevents undoification of
window configuration changes triggered by commands called with
`execute-extended-command' -- i.e. it's just too coarse.")


;; buffer-list-filter vars

(defvar wg-current-workgroup nil
  "Bound to the current workgroup in `wg-with-buffer-list-filters'.")

;; (defvar wg-current-buffer-command nil
;;   "Bound to the current buffer command in `wg-with-buffer-list-filters'.")

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

(defvar wg-current-wconfig-internal nil
  "Binding this to a wconfig causes `wg-current-wconfig' to
return that wconfig, rather than `wg-frame-to-wconfig'.")

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



;;; workgroups utils

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

(defvar wg-readable-types
  '(integer float cons symbol vector string char-table bool-vector)
  "List of types with readable printed representations.")

(defun wg-is-readable-p (obj)
  "Return non-nil if OBJ's printed representation is readable."
  (memq (type-of obj) wg-readable-types))

(defun wg-take-until-unreadable (list)
  "Return a new list of elements of LIST up to the first unreadable element."
  (wg-take-until-fail 'wg-is-readable-p list))

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
  "A small fontification DSL.
The results of all SPECS are `concat'd together.
If a spec is a cons with a keyword car, apply `wg-add-face' to it.
Other conses get evaluated, and should produce a strings.
It a spec is a string it is used unmodified.
Anything else is formatted with %s to produce a string."
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
  (unless (zerop (minibuffer-depth))
    (error "Workgroup operations aren't permitted while the \
minibuffer is active.")))

(defmacro wg-set-parameter (place parameter value)
  "Set PARAMETER to VALUE at PLACE.
This needs to be a macro to allow specification of a setf'able place."
  (wg-with-gensyms (p v)
    `(let ((,p ,parameter) (,v ,value))
       (wg-pickelable-or-error ,p)
       (wg-pickelable-or-error ,v)
       (setf ,place (wg-aput ,place ,p ,v))
       ,v)))



;;; uid utils

(defun wg-time-to-b36 (&optional time)
  "Convert `current-time' into a b36 string."
  (apply 'concat (wg-docar (time (or time (current-time)))
                   (wg-int-to-b36 time 4))))

(defun wg-b36-to-time (b36)
  "Parse the time from UID."
  (loop for i from 0 to 8 by 4
        collect (wg-b36-to-int (subseq b36 i (+ i 4)))))

(defalias 'wg-uid-to-time 'wg-b36-to-time)

(defun wg-generate-uid (&optional prefix)
  "Return a new uid, optionally prefixed by PREFIX."
  (concat prefix
          (wg-time-to-b36)
          "-"
          (wg-int-to-b36 string-chars-consed)))

(defun wg-uid-to-seconds (uid)
  "Return the `float-time' parsed from UID with `wg-uid-to-time'."
  (float-time (wg-uid-to-time uid)))



;;; structure types

(wg-defstruct wg buf
  (uid (wg-generate-uid))
  (name)
  (file-name)
  (point)
  (mark)
  (local-vars)
  (special-data)
  (gc))

(wg-defstruct wg win
  (uid)
  (parameters)
  (edges)
  (point)
  (start)
  (hscroll)
  (dedicated)
  (selected)
  (minibuffer-scroll)
  (buf-uid))

(wg-defstruct wg wtree
  (uid)
  (dir)
  (edges)
  (wlist))

(wg-defstruct wg wconfig
  (uid (wg-generate-uid))
  (name)
  (parameters)
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
  (parameters)
  (base-wconfig)
  (saved-wconfigs)
  (strong-buf-uids)
  (weak-buf-uids))

(wg-defstruct wg session
  (uid (wg-generate-uid))
  (name)
  (modified)
  (parameters)
  (file-name)
  (version wg-version)
  (workgroup-list)
  (buf-list))

(wg-defstruct wg workgroup-state
  (undo-pointer)
  (undo-list))



;;; session ops

(defun wg-current-session ()
  "Return `wg-current-session', setting it first if necessary."
  (or wg-current-session
      (setq wg-current-session (wg-make-session))))

(defmacro wg-buf-list ()
  "setf'able `wg-current-session' buf-list slot accessor."
  `(wg-session-buf-list (wg-current-session)))

(defmacro wg-workgroup-list ()
  "setf'able `wg-current-session' modified slot accessor."
  `(wg-session-workgroup-list (wg-current-session)))

(defun wg-workgroup-list-or-error (&optional noerror)
  "Return the value of `wg-current-session's :workgroup-list slot."
  (or (wg-workgroup-list)
      (unless noerror
        (error "No workgroups are defined."))))

(defun wg-modified-p ()
  "Return t when the current session or any of its workgroups are modified."
  (or (wg-session-modified (wg-current-session))
      (some 'wg-workgroup-modified (wg-workgroup-list))))

(defun wg-mark-everything-unmodified ()
  "Mark the session and all workgroups as unmodified."
  (setf (wg-session-modified (wg-current-session)) nil)
  (dolist (workgroup (wg-workgroup-list))
    (setf (wg-workgroup-modified workgroup) nil)))

(defun wg-workgroup-names (&optional noerror)
  "Return a list of workgroup names."
  (mapcar 'wg-workgroup-name (wg-workgroup-list-or-error noerror)))



;;; session parameters

(defun wg-session-parameter (session parameter &optional default)
  "Return SESSION's value for PARAMETER.
If PARAMETER is not found, return DEFAULT which defaults to nil.
SESSION nil defaults to the current session."
  (wg-aget (wg-session-parameters (or session (wg-current-session)))
           parameter default))

(defun wg-set-session-parameter (session parameter value)
  "Set SESSION's value of PARAMETER to VALUE.
SESSION nil means use the current session.
Return value."
  (let ((session (or session (wg-current-session))))
    (wg-set-parameter (wg-session-parameters session) parameter value)
    (setf (wg-session-modified session) t)
    value))

(defun wg-remove-session-parameter (session parameter)
  "Remove parameter PARAMETER from SESSION's parameters."
  (let ((session (or session (wg-current-session))))
    (wg-asetf (wg-session-parameters session) (wg-aremove it parameter))
    (setf (wg-session-modified session) t)))



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

(defun wg-buf-major-mode (buf)
  "Return BUF's `major-mode'.
It's stored in BUF's local-vars list, since it's a local variable."
  (wg-aget (wg-buf-local-vars buf) 'major-mode))

(defun wg-bufobj-major-mode (bufobj)
  "Return BUFOBJ's major-mode."
  (etypecase bufobj
    (buffer (wg-buffer-major-mode bufobj))
    (wg-buf (wg-buf-major-mode bufobj))
    (string (wg-buffer-major-mode bufobj))))

;; `wg-equal-bufobjs' and `wg-find-bufobj' may need to be made a lot smarter
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

(defun wg-find-buf-by-uid (uid)
  "Find a buf in `wg-buf-list' by UID."
  (wg-find-bufobj-by-uid uid (wg-buf-list)))

(defun wg-set-buffer-uid-or-error (uid &optional buffer)
  "Set BUFFER's buffer local value of `wg-buffer-uid' to UID.
If BUFFER already has a buffer local value of `wg-buffer-uid',
and it's not equal to UID, error."
  (if wg-buffer-uid
      (if (string= wg-buffer-uid uid) uid
        (error "uids don't match %S and %S" uid wg-buffer-uid))
    (setq wg-buffer-uid uid)))




;;; wconfig construction

(defun wg-buffer-special-data (buffer)
  "Return BUFFER's auxiliary serialization, or nil."
  (some (lambda (fn) (funcall fn buffer)) wg-special-buffer-serdes-functions))

(defun wg-window-point (ewin)
  "Return `point' or :max.  See `wg-restore-point-max'.
EWIN should be an Emacs window object."
  (let ((p (window-point ewin)))
    (if (and wg-restore-point-max (= p (point-max))) :max p)))

(defun wg-serialize-buffer-local-variables ()
  "Return an alist of buffer-local variable symbols and their values.
See `wg-buffer-local-variables-alist' for details."
  (wg-docar (entry wg-buffer-local-variables-alist)
    (wg-dbind (var ser des) entry
      (when (local-variable-p var)
        (cons var (if ser (funcall ser) (symbol-value var)))))))

(defun wg-buffer-to-buf (buffer)
  "Return the serialization (a wg-buf) of Emacs buffer BUFFER."
  (with-current-buffer buffer
    (wg-make-buf
     :name           (buffer-name)
     :file-name      (buffer-file-name)
     :point          (point)
     :mark           (mark)
     :local-vars     (wg-serialize-buffer-local-variables)
     :special-data   (wg-buffer-special-data buffer))))

(defun wg-add-buffer-to-buf-list (buffer)
  "Make a buf from BUFFER, and add it to `wg-buf-list' if necessary.
If there isn't already a buf corresponding to BUFFER in
`wg-buf-list', make one and add it.  Return BUFFER's uid
in either case."
  (with-current-buffer buffer
    (setq wg-buffer-uid
          (wg-aif (wg-find-buffer-in-buf-list buffer (wg-buf-list))
              (wg-buf-uid it)
            (let ((buf (wg-buffer-to-buf buffer)))
              (push buf (wg-buf-list))
              (wg-buf-uid buf))))))

(defun wg-buffer-uid-or-add (buffer)
  "If there isn't already a buf corresponding to BUFFER in
`wg-buf-list', make one and add it.  Return BUFFER's uid
in either case."
  (or (wg-buffer-uid buffer) (wg-add-buffer-to-buf-list buffer)))

(defun wg-bufobj-uid-or-add (bufobj)
  "If BUFOBJ is a wg-buf, return its uid.
If BUFOBJ is a buffer or a buffer name, see `wg-buffer-uid-or-add'."
  (etypecase bufobj
    (wg-buf (wg-buf-uid bufobj)) ;; possibly also add to `wg-buf-list'
    (buffer (wg-buffer-uid-or-add bufobj))
    (string (wg-bufobj-uid-or-add (wg-get-buffer bufobj)))))



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
  "Return the serialization (a wg-win) of Emacs window WINDOW."
  (let ((selected (eq window (selected-window))))
    (with-selected-window window
      (wg-make-win
       :edges              (window-edges window)
       :point              (wg-window-point window)
       :start              (window-start window)
       :hscroll            (window-hscroll window)
       :selected           selected
       :minibuffer-scroll  (eq window minibuffer-scroll-window)
       :dedicated          (window-dedicated-p window)
       :buf-uid            (wg-buffer-uid-or-add (window-buffer window))))))

(defun wg-window-tree-to-wtree (window-tree)
  "Return the serialization (a wg-wtree) of Emacs window tree WINDOW-TREE."
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
  "Return the serialization (a wg-wconfig) of Emacs frame FRAME.
FRAME nil defaults to `selected-frame'."
  (let ((frame (or frame (selected-frame))))
    (wg-make-wconfig
     :left                  (frame-parameter frame 'left)
     :top                   (frame-parameter frame 'top)
     :width                 (frame-parameter frame 'width)
     :height                (frame-parameter frame 'height)
     :vertical-scroll-bars  (frame-parameter frame 'vertical-scroll-bars)
     :scroll-bar-width      (frame-parameter frame 'scroll-bar-width)
     :wtree                 (wg-window-tree-to-wtree (window-tree frame)))))

(defun wg-current-wconfig ()
  "Return the current wconfig.
If `wg-current-wconfig' is non-nil, return it.  Otherwise return
`wg-frame-to-wconfig'."
  (or wg-current-wconfig-internal (wg-frame-to-wconfig)))

(defmacro wg-with-current-wconfig (wconfig &rest body)
  "Eval BODY with WCONFIG current."
  (declare (indent 1))
  `(let ((wg-current-wconfig-internal ,wconfig)) ,@body))

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
      (wg-asetf (wg-wtree-wlist scaled)
                (wg-docar (sw it) (wg-scale-wtree sw wscale hscale)))
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

(defun wg-resize-frame-scale-wtree (wconfig)
  "Set FRAME's size to WCONFIG's, returning a possibly scaled wtree.
If the frame size was set correctly, return WCONFIG's wtree
unchanged.  If it wasn't, return a copy of WCONFIG's wtree scaled
with `wg-scale-wconfigs-wtree' to fit the frame as it exists."
  (let ((frame (selected-frame)))
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
  (wg-asetf (wg-wconfig-wtree wconfig) (wg-reverse-wlist it dir))
  wconfig)

(defun wg-wconfig-move-window (wconfig offset)
  "Offset `selected-window' OFFSET places in WCONFIG."
  (wg-asetf (wg-wconfig-wtree wconfig) (wg-wtree-move-window it offset))
  wconfig)

(defun wg-flatten-wtree (wtree &optional key)
  "Return a new list by flattening WTREE.
KEY non returns returns a list of WTREE's wins.
KEY non-nil returns a list of the results of calling KEY on each win."
  (flet ((inner (w) (if (wg-win-p w) (list (if key (funcall key w) w))
                      (mapcan 'inner (wg-wtree-wlist w)))))
    (inner wtree)))

(defun wg-win-list (wtree)
  "Construct and return a list of all wg-wins in WTREE."
  (wg-flatten-wtree wtree))


;;; special buffer serdes functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO: Possibly add restore-special-data customization option
;; TODO: These could be a little more thorough
;;

;; Info buffer serdes

(defun wg-deserialize-Info-buffer (buf)
  "Deserialize an Info buffer."
  (require 'info)
  (wg-aif (cdr (wg-buf-special-data buf))
      (apply #'Info-find-node it)
    (info))
  (current-buffer))

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
  "Deserialize a help buffer.
See `wg-serialize-help-buffer'."
  (require 'help-mode)
  (wg-dbind (this-function item stack forward-stack) (wg-buf-special-data buf)
    (let ((help-window-select t))
      (apply (car item) (cdr item))
      (set-buffer (window-buffer (selected-window)))
      (wg-when-boundp (help-xref-stack help-xref-forward-stack)
        (setq help-xref-stack stack
              help-xref-forward-stack forward-stack)))))

(defun wg-serialize-help-buffer (buffer)
  "Serialize a help buffer.
Since `help-mode' is used by many buffers that aren't actually
*Help* buffers (e.g. *Process List*), we also check that
`help-xref-stack-item' has a local binding."
  (with-current-buffer buffer
    (when (and (eq major-mode 'help-mode)
               (local-variable-p 'help-xref-stack-item)
               (boundp 'help-xref-stack-item)
               (boundp 'help-xref-stack)
               (boundp 'help-xref-forward-stack))
      (list 'wg-deserialize-help-buffer
            (wg-take-until-unreadable help-xref-stack-item)
            (mapcar 'wg-take-until-unreadable help-xref-stack)
            (mapcar 'wg-take-until-unreadable help-xref-forward-stack)))))


;; ielm buffer serdes

(defun wg-deserialize-ielm-buffer (buf)
  "Deserialize an `inferior-emacs-lisp-mode' buffer."
  (ielm)
  (current-buffer))

(defun wg-serialize-ielm-buffer (buffer)
  "Serialize an `inferior-emacs-lisp-mode' buffer."
  (with-current-buffer buffer
    (when (eq major-mode 'inferior-emacs-lisp-mode)
      (list 'wg-deserialize-ielm-buffer))))


;; shell buffer serdes

(defun wg-deserialize-shell-buffer (buf)
  "Deserialize a `shell-mode' buffer."
  (shell (wg-buf-name buf)))

(defun wg-serialize-shell-buffer (buffer)
  "Serialize a `shell-mode' buffer."
  (with-current-buffer buffer
    (when (eq major-mode 'shell-mode)
      (list 'wg-deserialize-shell-buffer))))


;; eshell buffer serdes

(defun wg-deserialize-eshell-buffer (buf)
  "Deserialize an `eshell-mode' buffer."
  (prog1 (eshell t)
    (rename-buffer (wg-buf-name buf) t)))

(defun wg-serialize-eshell-buffer (buffer)
  "Serialize an `eshell-mode' buffer."
  (with-current-buffer buffer
    (when (eq major-mode 'eshell-mode)
      (list 'wg-deserialize-eshell-buffer))))


;; term and ansi-term buffer serdes

(defun wg-deserialize-term-buffer (buf)
  "Deserialize a `term-mode' buffer."
  (require 'term)
  ;; flet'ing these prevents scrunched up wrapping when restoring during morph
  (flet ((term-window-width () 80)
         (window-height () 24))
    (prog1 (term (nth 1 (wg-buf-special-data buf)))
      (rename-buffer (wg-buf-name buf) t))))

(defun wg-serialize-term-buffer (buffer)
  "Serialize a `term-mode' buffer.
This should work for `ansi-term's, too, as there doesn't seem to
be any difference between the two except how the name of the
buffer is generated."
  (with-current-buffer buffer
    (when (eq major-mode 'term-mode)
      (list 'wg-deserialize-term-buffer
            (wg-last1 (process-command (get-buffer-process buffer)))))))



;;; buffer-local variable serdes

(defun wg-serialize-buffer-mark-ring ()
  "Return a new list of the positions of the marks in `mark-ring'."
  (mapcar 'marker-position mark-ring))

(defun wg-deserialize-buffer-mark-ring (positions)
  "Set `mark-ring' to a new list of markers created from POSITIONS."
  (setq mark-ring
        (mapcar (lambda (pos) (set-marker (make-marker) pos))
                positions)))

(defun wg-deserialize-buffer-major-mode (major-mode-symbol)
  "Conditionally retore MAJOR-MODE-SYMBOL in `current-buffer'."
  (and (fboundp major-mode-symbol)
       (not (eq major-mode-symbol major-mode))
       (funcall major-mode-symbol)))

(defun wg-deserialize-buffer-local-variables (buf)
  "Restore BUF's buffer local variables in `current-buffer'."
  (loop for ((var . val) . rest) on (wg-buf-local-vars buf)
        do (wg-awhen (assq var wg-buffer-local-variables-alist)
             (wg-dbind (var ser des) it
               (if des (funcall des val)
                 (set var val))))))



;;; wconfig restoration

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
           (when wg-restore-mark
             (set-mark (wg-buf-mark buf))
             (deactivate-mark))
           (wg-deserialize-buffer-local-variables buf)
           (current-buffer))
          (t
           (message "Attempt to restore nonexistent file: %S" file-name)
           nil))))

(defun wg-restore-special-buffer (buf)
  "Restore a buffer with DESERIALIZER-FN."
  (wg-when-let ((special-data (wg-buf-special-data buf)))
    (switch-to-buffer
     (save-window-excursion
       (condition-case err
           (funcall (car special-data) buf)
         (error (message "Error deserializing %S: %S"
                         (wg-buf-name buf) err)))) t)
    (wg-set-buffer-uid-or-error (wg-buf-uid buf))
    (current-buffer)))

(defun wg-set-buf-gc-flag (buf value)
  "Set BUF's gc flag to VALUE unless it's currently set to `never'."
  (unless (eq 'never (wg-buf-gc buf))
    (setf (wg-buf-gc buf) value)))

(defun wg-restore-buffer (buf)
  "Restore BUF and return it."
  (let* ((wg-buffer-auto-association-on nil)
         (buffer (or (wg-restore-existing-buffer buf)
                     (wg-restore-special-buffer buf)
                     (wg-restore-file-buffer buf)
                     (progn (wg-restore-default-buffer) nil))))
    (wg-set-buf-gc-flag buf (if buffer nil t))
    buffer))

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

;; FIXME: nix these or move them to the vars section
(defvar wg-incorrectly-restored-bufs nil
  "FIXME: docstring this")

(defvar wg-record-incorrectly-restored-bufs nil
  "FIXME: docstring this")

(defun wg-restore-window (win)
  "Restore WIN in `selected-window'."
  (let ((selwin (selected-window))
        (buf (wg-find-buf-by-uid (wg-win-buf-uid win))))
    (cond ((not buf)
           (wg-restore-default-buffer))
          ((wg-restore-buffer buf)
           (wg-restore-window-positions win selwin)
           (when wg-restore-window-dedicated-p
             (set-window-dedicated-p selwin (wg-win-dedicated win))))
          (t (wg-restore-default-buffer)
             (when wg-record-incorrectly-restored-bufs
               (pushnew buf wg-incorrectly-restored-bufs))))))

(defun wg-reset-window-tree ()
  "Delete all but one window in `selected-frame', and reset
various parameters of that window in preparation for restoring
a wtree."
  (delete-other-windows)
  (set-window-dedicated-p nil nil))

;; (defun wg-restore-window-tree-helper (w)
;;   "Recursion helper for `wg-restore-window-tree'."
;;   (if (wg-wtree-p w)
;;       (loop with dir = (wg-wtree-dir w)
;;             for (win . rest) on (wg-wtree-wlist w)
;;             do (when rest (split-window nil (wg-w-size win dir) (not dir)))
;;             do (wg-restore-window-tree-helper win))
;;     (wg-restore-window w)
;;     (other-window 1)))

(defun wg-restore-window-tree-helper (w)
  "Recursion helper for `wg-restore-window-tree'."
  (if (wg-wtree-p w)
      (loop with dir = (wg-wtree-dir w)
            for (win . rest) on (wg-wtree-wlist w)
            do (when rest (split-window nil (wg-w-size win dir) (not dir)))
            do (wg-restore-window-tree-helper win))
    (wg-restore-window w)
    (when (wg-win-selected w)
      (setq wg-window-tree-selected-window (selected-window)))
    (when (wg-win-minibuffer-scroll w)
      (setq minibuffer-scroll-window (selected-window)))
    (other-window 1)))

;; (defun wg-restore-window-tree (wtree)
;;   "Restore WTREE in `selected-frame'."
;;   (let ((window-min-width wg-window-min-width)
;;         (window-min-height wg-window-min-height)
;;         (wg-window-tree-selected-window nil))
;;     (wg-reset-window-tree)
;;     (wg-restore-window-tree-helper wtree)
;;     (wg-awhen wg-window-tree-selected-window (select-window it))))

(defun wg-restore-window-tree (wtree)
  "Restore WTREE in `selected-frame'."
  (let ((window-min-width wg-window-min-width)
        (window-min-height wg-window-min-height)
        (wg-window-tree-selected-window nil))
    (wg-reset-window-tree)
    (wg-restore-window-tree-helper wtree)
    (wg-awhen wg-window-tree-selected-window (select-window it))))

(defun wg-wconfig-restore-frame-position (wconfig)
  "Restore `selected-frame's position from WCONFIG."
  (wg-when-let ((left (wg-wconfig-left wconfig))
                (top (wg-wconfig-top wconfig)))
    (set-frame-position (selected-frame) left top)))

(defun wg-wconfig-restore-scroll-bars (wconfig)
  "Restore `selected-frame's scroll-bar settings from WCONFIG."
  (set-frame-parameter
   nil 'vertical-scroll-bars (wg-wconfig-vertical-scroll-bars wconfig))
  (set-frame-parameter
   nil 'scroll-bar-width (wg-wconfig-scroll-bar-width wconfig)))

;; FIXME: throw a specific error condition if the restoration was unsuccessful
(defun wg-restore-wconfig (wconfig)
  "Restore WCONFIG in `selected-frame'."
  (let ((wg-record-incorrectly-restored-bufs t)
        (wg-incorrectly-restored-bufs nil))
    (wg-barf-on-active-minibuffer)
    (when wg-restore-frame-position
      (wg-wconfig-restore-frame-position wconfig))
    (let ((wtree (wg-resize-frame-scale-wtree wconfig)))
      (wg-restore-window-tree
       (if (not (wg-morph-p)) wtree (wg-morph wtree))))
    (when wg-restore-scroll-bars
      (wg-wconfig-restore-scroll-bars wconfig))
    (when wg-incorrectly-restored-bufs
      (message "Unable to restore these buffers: %S\n\
If you want, restore them manually and try again."
               (mapcar 'wg-buf-name wg-incorrectly-restored-bufs)))))



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

(defun wg-morph-win-to-win (w1 w2 &optional swap)
  "Return a copy of W1 with its edges stepped once toward W2.
When SWAP is non-nil, return a copy of W2 instead."
  (wg-set-edges (wg-copy-win (if swap w2 w1)) (wg-morph-step-edges w1 w2)))

(defun wg-morph-win-to-wtree (win wtree)
  "Return a new wtree with WIN's edges and WTREE's last two windows."
  (wg-make-wtree
   :dir (wg-wtree-dir wtree)
   :edges (wg-morph-step-edges win wtree)
   :wlist (let ((wg-morph-hsteps 2) (wg-morph-vsteps 2))
            (wg-docar (w (wg-leave (wg-wtree-wlist wtree) 2))
              (wg-morph-win-to-win (wg-minified-copy-of-last-win w) w)))))

(defun wg-morph-wtree-to-win (wtree win &optional noswap)
  "Grow the first window of WTREE and its subtrees one step toward WIN.
This eventually wipes WTREE's components, leaving only a window.
Swap WTREE's first actual window for WIN, unless NOSWAP is non-nil."
  (if (wg-win-p wtree) (wg-morph-win-to-win wtree win (not noswap))
    (wg-make-wtree
     :dir (wg-wtree-dir wtree)
     :edges (wg-morph-step-edges wtree win)
     :wlist (wg-dbind (fwin . wins) (wg-wtree-wlist wtree)
              (cons (wg-morph-wtree-to-win fwin win noswap)
                    (wg-docar (sw wins)
                      (if (wg-win-p sw) sw
                        (wg-morph-wtree-to-win sw win t))))))))

(defun wg-morph-wtree-to-wtree (wt1 wt2)
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
         (wg-morph-win-to-win w1 w2 t))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (wg-morph-wtree-to-wtree w1 w2))
        ((and (wg-win-p w1) (wg-wtree-p w2))
         (wg-morph-win-to-wtree w1 w2))
        ((and (wg-wtree-p w1) (wg-win-p w2))
         (wg-morph-wtree-to-win w1 w2))))

;; (defun wg-morph (to &optional from)
;;   "Morph from wtree FROM to wtree TO.
;; Assumes both FROM and TO fit in `selected-frame'."
;;   (let ((from (or from (wg-window-tree-to-wtree (window-tree))))
;;         (wg-morph-hsteps
;;          (wg-morph-determine-steps wg-morph-hsteps wg-morph-terminal-hsteps))
;;         (wg-morph-vsteps
;;          (wg-morph-determine-steps wg-morph-vsteps wg-morph-terminal-vsteps))
;;         (truncate-partial-width-windows wg-morph-truncate-partial-width-windows)
;;         (wg-restore-scroll-bars nil)
;;         (wg-restore-fringes nil)
;;         (wg-restore-margins nil)
;;         (wg-restore-point nil)
;;         (wg-restore-mark nil)
;;         (watchdog 0))
;;     (wg-until (wg-equal-wtrees from to)
;;       (condition-case err
;;           (if (> (incf watchdog) wg-morph-max-steps)
;;               (error "`wg-morph-max-steps' exceeded")
;;             (setq from (wg-normalize-wtree (wg-morph-dispatch from to)))
;;             (wg-restore-window-tree from)
;;             (redisplay))
;;         (error (wg-dbind (sym data) err
;;                  (unless (and (stringp data)
;;                               (string-match "too small" data))
;;                    (signal sym data))))))
;;     to))

(defun wg-morph (to &optional from)
  "Morph from wtree FROM to wtree TO.
Assumes both FROM and TO fit in `selected-frame'."
  (let ((from (or from (wg-window-tree-to-wtree (window-tree))))
        (wg-morph-hsteps
         (wg-morph-determine-steps wg-morph-hsteps wg-morph-terminal-hsteps))
        (wg-morph-vsteps
         (wg-morph-determine-steps wg-morph-vsteps wg-morph-terminal-vsteps))
        (truncate-partial-width-windows wg-morph-truncate-partial-width-windows)
        (wg-record-incorrectly-restored-bufs nil)
        (wg-restore-scroll-bars nil)
        (wg-restore-fringes nil)
        (wg-restore-margins nil)
        (wg-restore-point nil)
        (wg-restore-mark nil)
        (watchdog 0))
    (wg-until (wg-equal-wtrees from to)
      (condition-case err
          (if (> (incf watchdog) wg-morph-max-steps)
              (error "`wg-morph-max-steps' exceeded")
            (setq from (wg-normalize-wtree (wg-morph-dispatch from to)))
            (wg-restore-window-tree from)
            (redisplay))
        (error (wg-dbind (sym data) err
                 (unless (and (stringp data)
                              (string-match "too small" data))
                   (signal sym data))))))
    to))



;;; workgroup utils

(defun wg-flag-workgroup-modified (workgroup)
  "Set WORKGROUP's and the current session's modified flags."
  (when wg-flag-modified
    (setf (wg-workgroup-modified workgroup) t)
    (setf (wg-session-modified (wg-current-session)) t)))

(defun wg-find-workgroup-by (slotkey value &optional noerror)
  "Return the workgroup on which ACCESSOR returns VALUE or error."
  (let ((accessor (ecase slotkey
                    (:name 'wg-workgroup-name)
                    (:uid  'wg-workgroup-uid))))
    (or (find value (wg-workgroup-list-or-error noerror) :test 'equal :key accessor)
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

(defun wg-current-workgroup-p (workgroup &optional frame)
  "Return t when WORKGROUP is the current workgroup, nil otherwise."
  (wg-awhen (wg-current-workgroup t frame)
    (eq workgroup it)))

(defun wg-previous-workgroup-p (workgroup &optional frame)
  "Return t when WORKGROUP is the previous workgroup, nil otherwise."
  (wg-awhen (wg-previous-workgroup t frame)
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
        (t (error "Can't get workgroup from type:: %S" (type-of obj)))))



;;; workgroup parameters

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
    (wg-set-parameter (wg-workgroup-parameters workgroup) parameter value)
    (wg-flag-workgroup-modified workgroup)
    value))

(defun wg-remove-workgroup-parameter (workgroup parameter)
  "Remove PARAMETER from WORKGROUP's parameters."
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-flag-workgroup-modified workgroup)
    (wg-asetf (wg-workgroup-parameters workgroup) (wg-aremove it parameter))))



;;; workgroup associated buffers

(defun wg-workgroup-associated-buf-uids (workgroup)
  "Return a new list containing all of WORKGROUP's associated buf uids."
  (append (wg-workgroup-strong-buf-uids workgroup)
          (wg-workgroup-weak-buf-uids workgroup)))

(defun wg-workgroup-associated-bufs (workgroup)
  "Return a new list containing all of WORKGROUP's associated bufs."
  (delete nil (mapcar 'wg-find-buf-by-uid
                      (wg-workgroup-associated-buf-uids workgroup))))

(defun wg-workgroup-associated-buffers (workgroup &optional initial names)
  "Return a list of WORKGROUP's live associated buffers."
  (let ((assoc-bufs (wg-workgroup-associated-bufs workgroup)))
    (remove-if-not
     (lambda (buffer) (wg-find-buffer-in-buf-list buffer assoc-bufs))
     (or initial (buffer-list)))))

(defun wg-workgroup-bufobj-association-type (workgroup bufobj)
  "Return BUFOBJ's association-type in WORKGROUP, or nil if unassociated."
  (let ((uid (wg-bufobj-uid-or-add bufobj)))
    (or (and (member uid (wg-workgroup-strong-buf-uids workgroup)) 'strong)
        (and (member uid (wg-workgroup-weak-buf-uids workgroup)) 'weak))))

(defun wg-workgroup-strongly-associate-bufobj (workgroup bufobj)
  "Strongly associate BUFOBJ with WORKGROUP."
  (let* ((uid (wg-bufobj-uid-or-add bufobj))
         (remp (wg-removef-p uid (wg-workgroup-weak-buf-uids workgroup)
                             :test 'string=))
         (addp (wg-pushnew-p uid (wg-workgroup-strong-buf-uids workgroup)
                             :test 'string=)))
    (when (or remp addp)
      (wg-flag-workgroup-modified workgroup)
      bufobj)))

(defun wg-workgroup-weakly-associate-bufobj (workgroup bufobj)
  "Weakly associate BUFOBJ with WORKGROUP."
  (let* ((uid (wg-bufobj-uid-or-add bufobj))
         (remp (wg-removef-p uid (wg-workgroup-strong-buf-uids workgroup)
                             :test 'string=))
         (addp (wg-pushnew-p uid (wg-workgroup-weak-buf-uids workgroup)
                             :test 'string=)))
    (when (or remp addp)
      (wg-flag-workgroup-modified workgroup)
      bufobj)))

(defun wg-workgroup-dissociate-bufobj (workgroup bufobj)
  "Dissociate BUFOBJ from WORKGROUP."
  (let* ((uid (wg-bufobj-uid-or-add bufobj))
         (rem1p (wg-removef-p uid (wg-workgroup-strong-buf-uids workgroup)
                              :test 'string=))
         (rem2p (wg-removef-p uid (wg-workgroup-weak-buf-uids workgroup)
                              :test 'string=)))
    (wg-awhen (or rem1p rem2p)
      (wg-flag-workgroup-modified workgroup)
      bufobj)))

(defun wg-workgroup-associate-bufobj (workgroup bufobj &optional weak)
  "Associate BUFOBJ with WORKGROUP.
WEAK non-nil means weakly associate it.  Otherwise strongly associate it."
  (if weak (wg-workgroup-weakly-associate-bufobj workgroup bufobj)
    (wg-workgroup-strongly-associate-bufobj workgroup bufobj)))

(defun wg-workgroup-cycle-bufobj-association-type (workgroup bufobj)
  "Cycle the BUFOBJ's association type in WORKGROUP.
If it's strongly associated with the workgroup, weakly associate it.
If it's weakly associated with the workgroup, dissociate it.
If it's unassociated with the workgroup, mark it as strongly associated."
  (case (wg-workgroup-bufobj-association-type workgroup bufobj)
    (strong (wg-workgroup-weakly-associate-bufobj workgroup bufobj) 'weak)
    (weak (wg-workgroup-dissociate-bufobj workgroup bufobj) nil)
    (otherwise (wg-workgroup-strongly-associate-bufobj workgroup bufobj) 'strong)))

(defun wg-workgroup-dissociate-weakly-associated-buffers (workgroup)
  "Dissociate from WORKGROUP all weakly associated buffers."
  (when (wg-workgroup-weak-buf-uids workgroup)
    (wg-flag-workgroup-modified workgroup)
    (setf (wg-workgroup-weak-buf-uids workgroup) nil)))

(defun wg-workgroup-dissociate-strongly-associated-buffers (workgroup)
  "Dissociate from WORKGROUP all strongly associated buffers."
  (when (wg-workgroup-strong-buf-uids workgroup)
    (wg-flag-workgroup-modified workgroup)
    (setf (wg-workgroup-strong-buf-uids workgroup) nil)))

(defun wg-workgroup-dissociate-all-buffers (workgroup)
  "Dissociate from WORKGROUP all its associated buffers."
  (wg-workgroup-dissociate-weakly-associated-buffers workgroup)
  (wg-workgroup-dissociate-strongly-associated-buffers workgroup))

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
    (if names (mapcar 'wg-buffer-name buffer-list) buffer-list)))


;; buffer-list filters

(defun wg-buffer-list-filter-all (workgroup initial)
  "Return all buffers in INITIAL."
  initial)

(defun wg-buffer-list-filter-associated (workgroup initial)
  "Return only those buffers associated with WORKGROUP."
  (wg-workgroup-associated-buffers workgroup initial))

(defun wg-buffer-list-filter-unassociated (workgroup initial)
  "Return only those buffer unassociated with WORKGROUP."
  (let ((buffers (wg-workgroup-associated-buffers workgroup initial)))
    (remove-if (lambda (buffer) (member buffer buffers)) initial)))


;; buffer-list filtration utils

(defun wg-filter-buffer-list-by-regexp (regexp buffer-list)
  "Return only those buffers in BUFFER-LIST with names matching REGEXP."
  (remove-if-not (lambda (bname) (string-match regexp bname))
                 buffer-list :key 'buffer-name))

(defun wg-filter-buffer-list-by-root-dir (root-dir buffer-list)
  "Return only those buffers in BUFFER-LIST visiting files undo ROOT-DIR."
  (remove-if-not (lambda (f) (when f (wg-file-under-root-path-p root-dir f)))
                 buffer-list :key 'buffer-file-name))

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

(defun wg-buffer-list-filter-home-dir->elisp (workgroup buffer-list)
  "Example of chaining buffer-list filters.
This returns all buffers under \"~/\" that are also in `emacs-lisp-mode'."
  (wg-buffer-list-filter-elisp
   nil (wg-buffer-list-filter-home-dir nil buffer-list)))



;;; workgroup and session local variables
;;
;; FIXME: move this elsewhere
;;

(defun wg-session-local-value (variable &optional session)
  "Return the value of VARIABLE in SESSION.
SESSION nil defaults to the current session.  If VARIABLE does
not have a session-local binding in SESSION, the value is
resolved by Emacs."
  (let ((value (wg-session-parameter session variable 'default)))
    (if (not (eq value 'default)) value
      (symbol-value variable))))

(defun wg-workgroup-local-value (variable &optional workgroup)
  "Return the value of VARIABLE in WORKGROUP.
WORKGROUP nil defaults to the current workgroup.  If there is no
current workgroup, or if VARIABLE does not have a workgroup-local
binding in WORKGROUP, resolve VARIABLE with `wg-session-local-value'."
  (let ((workgroup (wg-get-workgroup workgroup t)))
    (if (not workgroup) (wg-session-local-value variable)
      (let ((value (wg-workgroup-parameter workgroup variable 'default)))
        (if (not (eq value 'default)) value
          (wg-session-local-value variable))))))

(defalias 'wg-local-value 'wg-workgroup-local-value)


;; buffer-list-filter context

(defun wg-buffer-list-filter-order (command)
  "Return WORKGROUP's buffer-list-filter order for COMMAND, or a default."
  (let ((bfo (wg-local-value 'wg-buffer-list-filter-order-alist)))
    (or (wg-aget bfo command) (wg-aget bfo 'default))))

(defmacro wg-prior-mapping (mode command)
  "Return whatever COMMAND would call if MODE wasn't on."
  `(or (let (,mode) (command-remapping ,command)) ,command))

(defun wg-filter-buffer-list-p ()
  "Return the current workgroup when buffer-list-filters are on."
  (and workgroups-mode wg-buffer-list-filtration-on (wg-current-workgroup t)))

(defmacro wg-with-buffer-list-filters (command &rest body)
  "Establish buffer-list-filter context for buffer command COMMAND, and eval BODY.
Binds `wg-current-buffer-list-filter-id' in BODY."
  (declare (indent 1))
  (wg-with-gensyms (order status)
    `(let* ((wg-minibuffer-contents nil)
            (,order (wg-buffer-list-filter-order ,command)))
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

(defun wg-workgroup-state-table (&optional frame)
  "Return FRAME's workgroup table, creating it first if necessary."
  (or (frame-parameter frame 'wg-workgroup-state-table)
      (let ((wtree (make-hash-table :test 'equal)))
        (set-frame-parameter frame 'wg-workgroup-state-table wtree)
        wtree)))

(defun wg-get-workgroup-state (workgroup &optional frame)
  "Return FRAME's WORKGROUP's state table."
  (let ((uid (wg-workgroup-uid workgroup))
        (state-table (wg-workgroup-state-table frame)))
    (or (gethash uid state-table)
        (let ((wgs (wg-make-workgroup-state
                    :undo-pointer 0
                    :undo-list (list (wg-workgroup-base-wconfig workgroup)))))
          (puthash uid wgs state-table)
          wgs))))

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
  (wg-flag-workgroup-modified workgroup)
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (setcar (nthcdr undo-pointer undo-list) wconfig)))

(defun wg-add-wconfig-to-undo-list (workgroup wconfig)
  "Add WCONFIG to WORKGROUP's undo list, truncating its future if necessary."
  (let ((state (wg-get-workgroup-state workgroup)))
    (wg-flag-workgroup-modified workgroup)
    (push wconfig (wg-workgroup-state-undo-list state))
    (setf (wg-workgroup-state-undo-pointer state) 0)
    (wg-awhen (nthcdr wg-wconfig-undo-list-max
                      (wg-workgroup-state-undo-list state))
      (setcdr it nil))))

(defun wg-workgroup-working-wconfig (workgroup &optional noupdate)
  "Return WORKGROUP's working-wconfig.
If WORKGROUP is the current workgroup in `selected-frame', and
NOUPDATE is nil, set its working wconfig in `selected-frame' to
`wg-current-wconfig' and return the updated wconfig.  Otherwise
return WORKGROUP's current undo state."
  (if (and (not noupdate) (wg-current-workgroup-p workgroup))
      (wg-set-workgroup-working-wconfig workgroup (wg-current-wconfig))
    (wg-with-undo workgroup (state undo-pointer undo-list)
      (nth undo-pointer undo-list))))

(defun wg-update-current-workgroup-working-wconfig ()
  "Update WORKGROUP's working-wconfig with `wg-current-wconfig'."
  (wg-awhen (wg-current-workgroup t)
    (wg-set-workgroup-working-wconfig it (wg-current-wconfig))))

(defun wg-restore-wconfig-undoably (wconfig &optional noundo)
  "Restore WCONFIG in `selected-frame', saving undo information."
  (when noundo (wg-unflag-undoify-window-configuration-change))
  (wg-update-current-workgroup-working-wconfig)
  (wg-restore-wconfig wconfig))

(defun wg-workgroup-offset-position-in-undo-list (workgroup increment)
  "Increment WORKGROUP's undo-pointer by INCREMENT.
Also restore the wconfig at the incremented undo-pointer if
WORKGROUP is current."
  (wg-with-undo workgroup (state undo-pointer undo-list)
    (let ((new-pointer (+ undo-pointer increment)))
      (when (wg-within new-pointer 0 (length undo-list))
        (when (wg-current-workgroup-p workgroup)
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
      (wg-add-wconfig-to-undo-list wg (wg-current-wconfig))))
  ;; Reset both flags no matter what:
  (setq wg-window-configuration-changed nil
        wg-undoify-window-configuration-change t))


;; FIXME: replace `wg-commands-that-alter-window-configs' with advice on every
;; window-config altering function.
(defun wg-update-working-wconfig-before-command ()
  "Update the current workgroup's working-wconfig before
`wg-commands-that-alter-window-configs'. Added to
`pre-command-hook'."
  (when (gethash this-original-command wg-commands-that-alter-window-configs)
    (wg-update-current-workgroup-working-wconfig)))

(defun wg-add-commands-that-modify-window-configs (&rest commands)
  "Add COMMANDS to `wg-commands-that-alter-window-configs'."
  (dolist (command commands commands)
    (puthash command t wg-commands-that-alter-window-configs)))

(defun wg-remove-commands-that-modify-window-configs (&rest commands)
  "Remove command symbols from `wg-commands-that-alter-window-configs'."
  (dolist (command commands commands)
    (remhash command wg-commands-that-alter-window-configs)))

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
 'balance-windows
 'balance-windows-area
 'help-with-tutorial
 'jump-to-register
 'erc-complete-word)



;;; base wconfig updating

(defun wg-set-workgroup-base-wconfig (workgroup wconfig)
  "Set WORKGROUP's base wconfig to WCONFIG, and set WORKGROUP's modified flag."
  (setf (wg-workgroup-base-wconfig workgroup) wconfig
        (wg-workgroup-modified workgroup) t))

(defun wg-workgroup-most-recent-working-wconfig (workgroup)
  "Return WORKGROUP's most recently created working wconfig by frame."
  (reduce
   (lambda (wconfig1 &optional wconfig2)
     (if (or (not wconfig2)
             (>= (wg-uid-to-seconds (wg-wconfig-uid wconfig1))
                 (wg-uid-to-seconds (wg-wconfig-uid wconfig2))))
         wconfig1
       wconfig2))
   (frame-list)
   :key (lambda (frame)
          (with-selected-frame frame
            (wg-workgroup-working-wconfig workgroup t)))
   :initial-value (wg-workgroup-base-wconfig workgroup)))

(defun wg-workgroup-update-base-wconfig (workgroup)
  "Update WORKGROUP's base wconfig with
`wg-workgroup-most-recent-working-wconfig'."
  (let ((wconfig (wg-workgroup-most-recent-working-wconfig workgroup)))
    (unless (eq (wg-workgroup-base-wconfig workgroup) wconfig)
      (wg-set-workgroup-base-wconfig workgroup wconfig))))

(defun wg-update-all-base-wconfigs ()
  "Update every workgroup's base wconfig with
`wg-workgroup-update-base-wconfig'."
  (mapc 'wg-workgroup-update-base-wconfig (wg-workgroup-list)))

(defun wg-update-base-wconfigs-from-frame-if-necessary (frame)
  "Added to `delete-frame-hook'.  Conditionally update each
workgroup's base wconfig with its working wconfig in FRAME.  Only
update if FRAME's working wconfig is the most recently created of
the working wconfigs and the base wconfig."
  (dolist (wg (wg-workgroup-list))
    (let ((wconfig (lambda (frame)
                     (with-selected-frame frame
                       (wg-workgroup-working-wconfig wg t)))))
      (when (and (not (eq wconfig (wg-workgroup-base-wconfig wg)))
                 (eq wconfig (wg-workgroup-most-recent-working-wconfig wg)))
        (wg-set-workgroup-base-wconfig wg wconfig)))))



;;; workgroup saved wconfigs

(defun wg-workgroup-get-saved-wconfig (workgroup wconfig)
  "FIXME: docstring this"
  (let ((saved (wg-workgroup-saved-wconfigs workgroup)))
    (etypecase wconfig
      (wg-wconfig (car (memq wconfig saved)))
      (string (find wconfig saved :key 'wg-wconfig-name :test 'string=)))))

(defun wg-workgroup-save-wconfig (workgroup wconfig)
  "FIXME: docstring this"
  (let ((name (wg-wconfig-name wconfig)))
    (unless name (error "Attempt to save a nameless wconfig"))
    (setf (wg-workgroup-modified workgroup) t)
    (wg-asetf (wg-workgroup-saved-wconfigs workgroup)
              (cons wconfig (remove* name it
                                     :key 'wg-wconfig-name
                                     :test 'string=)))))

(defun wg-workgroup-delete-saved-wconfig (workgroup wconfig)
  "FIXME: docstring this"
  (when (wg-removef-p wconfig (wg-workgroup-saved-wconfigs workgroup))
    (setf (wg-workgroup-modified workgroup) t)))



;;; garbage collection

;; update buf list

(defun wg-update-buffer-in-buf-list (&optional buffer)
  "Update BUFFER's corresponding buf in `wg-buf-list'.
BUFFER nil defaults to `current-buffer'."
  (let ((buffer (or buffer (current-buffer))))
    (wg-when-let ((uid (wg-buffer-uid buffer))
                  (old-buf (wg-find-buf-by-uid uid))
                  (new-buf (wg-buffer-to-buf buffer)))
      (setf (wg-buf-uid new-buf) (wg-buf-uid old-buf))
      (wg-asetf (wg-buf-list) (cons new-buf (remove old-buf it))))))

(defun wg-update-buf-list (&optional buffer-list)
  "Update all bufs in `wg-buf-list' corresponding to buffers in BUFFER-LIST."
  (mapc 'wg-update-buffer-in-buf-list (or buffer-list (buffer-list))))


;; gc buf uids

(defun wg-workgroup-gc-buf-uids (workgroup)
  "Remove buf uids from WORKGROUP that have no referent in `wg-buf-list'."
  (wg-asetf (wg-workgroup-strong-buf-uids workgroup)
            (remove-if-not 'wg-find-buf-by-uid it)
            (wg-workgroup-weak-buf-uids workgroup)
            (remove-if-not 'wg-find-buf-by-uid it)))

(defun wg-gc-buf-uids ()
  "Remove from all workgroups those buf uids that have no referent in `wg-buf-list'."
  (mapc 'wg-workgroup-gc-buf-uids (wg-workgroup-list)))


;; gc bufs

(defun wg-wtree-buf-uids (wtree)
  "Return a new list of the buf uids of all wins in wtree."
  (wg-flatten-wtree wtree 'wg-win-buf-uid))

(defun wg-wtree-unique-buf-uids (wtree)
  "Return a list of the unique buf uids of all wins in wtree."
  (remove-duplicates (wg-wtree-buf-uids wtree) :test 'string=))

(defun wg-wconfig-buf-uids (wconfig)
  "Return WCONFIG's wtree's `wg-wtree-buf-uids'."
  (wg-wtree-unique-buf-uids (wg-wconfig-wtree wconfig)))

(defun wg-workgroup-base-wconfig-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP's working wconfig."
  (wg-wconfig-buf-uids (wg-workgroup-base-wconfig workgroup)))

(defun wg-workgroup-saved-wconfigs-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP's base wconfig."
  (reduce 'wg-string-list-union
          (wg-workgroup-saved-wconfigs workgroup)
          :key 'wg-wconfig-buf-uids))

(defun wg-workgroup-all-wconfig-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP's wconfigs."
  (union (wg-workgroup-base-wconfig-buf-uids workgroup)
         (wg-workgroup-saved-wconfigs-buf-uids workgroup)
         :test 'string=))

(defun wg-workgroup-all-buf-uids (workgroup)
  "Return a new list of all unique buf uids in WORKGROUP."
  (reduce 'wg-string-list-union
          (list (wg-workgroup-base-wconfig-buf-uids workgroup)
                (wg-workgroup-saved-wconfigs-buf-uids workgroup)
                (wg-workgroup-associated-buf-uids workgroup))))

(defun wg-session-all-extant-buf-uids (&optional session)
  "Return a new list of all unique buf uids in SESSION.
SESSION nil defaults to `wg-current-session'."
  (reduce 'wg-string-list-union
          (wg-session-workgroup-list (or session (wg-current-session)))
          :key 'wg-workgroup-all-buf-uids))

(defun wg-buffer-uids (&optional buffer-list)
  "Return a list of the uids of all buffers in BUFFER-LIST in
which `wg-buffer-uid' is locally bound.
BUFFER-LIST nil defaults to `buffer-list'."
  (delq nil (mapcar 'wg-buffer-uid (or buffer-list (buffer-list)))))

(defun wg-all-extant-buf-uids (&optional session buffer-list)
  "Return the union of all workgroups' `wg-workgroup-all-buf-uids'."
  (union (wg-session-all-extant-buf-uids session)
         (wg-buffer-uids buffer-list)
         :test 'string=))

(defun wg-buf-uids-not-to-gc (&optional session)
  "Return a new list of all unique buf uids whose bufs should not be gc'd.
SESSION nil defaults to `wg-current-session'."
  (reduce 'wg-string-list-union
          (wg-session-workgroup-list (or session (wg-current-session)))
          :key 'wg-workgroup-all-wconfig-buf-uids))

(defun wg-gc-bufs ()
  "gc bufs from `wg-buf-list' that are no longer needed."
  (let ((buf-uids-not-to-gc (wg-buf-uids-not-to-gc))
        (all-buf-uids (wg-all-extant-buf-uids)))
    (wg-asetf (wg-buf-list)
              (remove-if (lambda (buf)
                           (let ((uid (wg-buf-uid buf))
                                 (gc (wg-buf-gc buf)))
                             (unless (or (eq gc 'never)
                                         (member uid buf-uids-not-to-gc))
                               (or gc (not (member uid all-buf-uids))))))
                         it))))

(defun wg-perform-session-maintenance ()
  "Perform various maintenance operations on the current Workgroups session."
  (wg-update-all-base-wconfigs)
  (wg-gc-bufs)
  (wg-gc-buf-uids)
  (wg-update-buf-list))


;; session consistency testing

(defun wg-session-uids-consistent-p ()
  "Return t if there are no duplicate bufs or buf uids in the wrong places,
nil otherwise."
  (and (every (lambda (wg)
                (not (wg-dups-p (wg-workgroup-associated-buf-uids wg)
                                :test 'string=)))
              (wg-workgroup-list))
       (not (wg-dups-p (wg-buf-list) :key 'wg-buf-uid :test 'string=))
       (not (wg-dups-p (wg-workgroup-list) :key 'wg-workgroup-uid :test 'string=))))



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

(defun wg-delete-workgroup (workgroup)
  "Remove WORKGROUP from `wg-workgroup-list'.
Also delete all references to it by `wg-workgroup-state-table',
`wg-current-workgroup' and `wg-previous-workgroup'."
  (dolist (frame (frame-list))
    (remhash (wg-workgroup-uid workgroup) (wg-workgroup-state-table frame))
    (when (wg-current-workgroup-p workgroup frame)
      (wg-set-current-workgroup nil frame))
    (when (wg-previous-workgroup-p workgroup frame)
      (wg-set-previous-workgroup nil frame)))
  (setf (wg-workgroup-list) (remove workgroup (wg-workgroup-list-or-error)))
  (setf (wg-session-modified (wg-current-session)) t)
  workgroup)

(defun wg-add-workgroup (workgroup &optional index)
  "Add WORKGROUP to `wg-workgroup-list' at INDEX or the end.
If a workgroup with the same name exists, overwrite it."
  (wg-awhen (wg-find-workgroup-by :name (wg-workgroup-name workgroup) t)
    (unless index (setq index (position it (wg-workgroup-list-or-error))))
    (wg-delete-workgroup it))
  (wg-asetf (wg-workgroup-list)
            (wg-insert-before workgroup it (or index (length it))))
  (setf (wg-session-modified (wg-current-session)) t)
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

(defun wg-make-and-add-workgroup (name &optional blank)
  "Create a workgroup named NAME and add it with `wg-check-and-add-workgroup'."
  (wg-check-and-add-workgroup
   (wg-make-workgroup
    :name name
    :base-wconfig (if blank (wg-make-blank-wconfig)
                    (wg-current-wconfig)))))

(defun wg-get-workgroup-create (workgroup)
  "Return the workgroup specified by WORKGROUP, creating a new one if needed.
If `wg-get-workgroup' on WORKGROUP returns a workgroup, return it.
Otherwise, if WORKGROUP is a string, create a new workgroup with
that name and return it. Otherwise error."
  (or (wg-get-workgroup workgroup t)
      (if (stringp workgroup)
          (when (or (not wg-confirm-on-get-workgroup-create)
                    (y-or-n-p (format "%S doesn't exist.  Create it? "
                                      workgroup)))
            (wg-make-and-add-workgroup workgroup))
        ;; Call this again for its error message
        (wg-get-workgroup workgroup))))

(defun wg-cyclic-offset-workgroup (workgroup n)
  "Offset WORKGROUP's position in `wg-workgroup-list' by N."
  (let ((workgroup-list (wg-workgroup-list-or-error)))
    (unless (member workgroup workgroup-list)
      (error "Workgroup isn't present in `wg-workgroup-list'."))
    (setf (wg-workgroup-list) (wg-cyclic-offset-elt workgroup workgroup-list n)
          (wg-session-modified (wg-current-session)) t)))

(defun wg-swap-workgroups-in-workgroup-list (workgroup1 workgroup2)
  "Swap the positions of WORKGROUP1 and WORKGROUP2 in `wg-workgroup-list'."
  (let ((workgroup-list (wg-workgroup-list-or-error)))
    (when (eq workgroup1 workgroup2)
      (error "Can't swap a workgroup with itself"))
    (unless (and (memq workgroup1 workgroup-list)
                 (memq workgroup2 workgroup-list))
      (error "Both workgroups aren't present in `wg-workgroup-list'."))
    (setf (wg-workgroup-list) (wg-util-swap workgroup1 workgroup2 workgroup-list)
          (wg-session-modified (wg-current-session)) t)))

(defun wg-cyclic-nth-from-workgroup (workgroup &optional n)
  "Return the workgroup N places from WORKGROUP in `wg-workgroup-list'."
  (wg-cyclic-nth-from-elt workgroup (wg-workgroup-list-or-error) (or n 1)))



;;; mode-line

(defun wg-mode-line-buffer-association-indicator (workgroup)
  "Return a string indicating `current-buffer's association-type in WORKGROUP."
  (case (wg-workgroup-bufobj-association-type workgroup (current-buffer))
    (strong wg-mode-line-decor-strongly-associated)
    (weak wg-mode-line-decor-weakly-associated)
    (otherwise wg-mode-line-decor-unassociated)))

(defun wg-mode-line-string ()
  "Return the string to be displayed in the mode-line."
  (let ((wg (wg-current-workgroup t))
        (wg-use-faces wg-mode-line-use-faces))
    (cond (wg (wg-fontify " "
                (:div wg-mode-line-decor-left-brace)
                (:mode (wg-workgroup-name wg))
                (:div wg-mode-line-decor-divider)
                (:mode (wg-mode-line-buffer-association-indicator wg))
                (:div wg-mode-line-decor-divider)
                (:mode (if (wg-session-modified (wg-current-session))
                           wg-mode-line-decor-session-modified
                         wg-mode-line-decor-session-unmodified))
                (:mode (if (wg-workgroup-modified wg)
                           wg-mode-line-decor-workgroup-modified
                         wg-mode-line-decor-workgroup-unmodified))
                (:div wg-mode-line-decor-right-brace)))
          (t  (wg-fontify " "
                (:div wg-mode-line-decor-left-brace)
                (:mode "no workgroups")
                (:div wg-mode-line-decor-right-brace))))))

(defun wg-add-mode-line-display ()
  "Add Workgroups' mode-line format to `mode-line-format'."
  (unless (assq 'wg-mode-line-display-on mode-line-format)
    (let ((format '(wg-mode-line-display-on (:eval (wg-mode-line-string))))
          (pos (position 'mode-line-position mode-line-format)))
      (set-default 'mode-line-format
                   (wg-insert-after format mode-line-format pos))
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

;; FIXME: add `wg-display-max-lines' to chop long display strings at max-line
;; and element-name boundaries

(defun wg-element-display (elt elt-string current-p previous-p)
  "Return display string for ELT."
  (cond ((funcall current-p elt)
         (wg-fontify (:cur (concat wg-list-display-decor-current-left
                                   elt-string
                                   wg-list-display-decor-current-right))))
        ((funcall previous-p elt)
         (wg-fontify (:prev (concat wg-list-display-decor-previous-left
                                    elt-string
                                    wg-list-display-decor-previous-right))))
        (t (wg-fontify (:other elt-string)))))

(defun wg-workgroup-display (workgroup index)
  "Return display string for WORKGROUP at INDEX."
  (if (not workgroup) "No workgroups"
    (wg-element-display
     workgroup
     (format "%d: %s" index (wg-workgroup-name workgroup))
     (lambda (wg) (wg-current-workgroup-p wg))
     (lambda (wg) (wg-previous-workgroup-p wg)))))

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
  (let ((div (wg-add-face :div wg-list-display-decor-divider))
        (i -1))
    (wg-fontify
      (:brace wg-list-display-decor-left-brace)
      (if (not list) (funcall elt-fn nil nil)
        (wg-doconcat (elt list div) (funcall elt-fn elt (incf i))))
      (:brace wg-list-display-decor-right-brace))))

(defun wg-workgroup-list-display (&optional workgroup-list)
  "Return the Workgroups list display string.
The string contains the names of all workgroups in `wg-workgroup-list',
decorated with faces, dividers and strings identifying the
current and previous workgroups."
  (wg-display-internal
   'wg-workgroup-display (or workgroup-list (wg-workgroup-list))))

;; TODO: Possibly add scroll animation for the buffer list display during
;; `wg-next-buffer' and `wg-previous-buffer'
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

;; TODO: clean this up
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



;;; parameter pickeling

(defun wg-pickel-workgroup-parameters (workgroup)
  "If WORKGROUP's parameters are non-nil, return a copy of
WORKGROUP after pickeling its parameters. Otherwise return
WORKGROUP."
  (if (not (wg-workgroup-parameters workgroup)) workgroup
    (let ((copy (wg-copy-workgroup workgroup)))
      (wg-asetf (wg-workgroup-parameters copy) (wg-pickel it))
      copy)))

(defun wg-unpickel-workgroup-parameters (workgroup)
  "If WORKGROUP's parameters are non-nil, return a copy of
WORKGROUP after unpickeling its parameters. Otherwise return
WORKGROUP."
  (if (not (wg-workgroup-parameters workgroup)) workgroup
    (let ((copy (wg-copy-workgroup workgroup)))
      (wg-asetf (wg-workgroup-parameters copy) (wg-unpickel it))
      copy)))

(defun wg-pickel-all-session-parameters (session)
  "Return a copy of SESSION after pickeling its
parameters and the parameters of all its workgroups."
  (let ((copy (wg-copy-session session)))
    (when (wg-session-parameters copy)
      (wg-asetf (wg-session-parameters copy) (wg-pickel it)))
    (wg-asetf (wg-session-workgroup-list copy)
              (mapcar 'wg-pickel-workgroup-parameters it))
    copy))

(defun wg-unpickel-session-parameters (session)
  "Return a copy of SESSION after unpickeling its
parameters and the parameters of all its workgroups."
  (let ((copy (wg-copy-session session)))
    (when (wg-session-parameters copy)
      (wg-asetf (wg-session-parameters copy) (wg-unpickel it)))
    (wg-asetf (wg-session-workgroup-list copy)
              (mapcar 'wg-unpickel-workgroup-parameters it))
    copy))



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
  (let ((max (1- (length (wg-workgroup-list-or-error)))))
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
  (let ((workgroup (wg-get-workgroup-create workgroup))
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
  (let ((wl (wg-workgroup-list-or-error)))
    (wg-switch-to-workgroup
     (or (nth index wl) (error "There are only %d workgroups" (length wl))))))

(macrolet
    ((define-range-of-switch-to-workgroup-at-index (num)
       `(progn
          ,@(wg-docar (i (wg-range 0 num))
              `(defun ,(intern (format "wg-switch-to-workgroup-at-index-%d" i)) ()
                 ,(format "Switch to the workgroup at index %d." i)
                 (interactive)
                 (wg-switch-to-workgroup-at-index ,i))))))
  (define-range-of-switch-to-workgroup-at-index 10))

(defun wg-switch-to-cyclic-nth-from-workgroup (workgroup n)
  "Switch N workgroups cyclically from WORKGROUP in `wg-workgroup-list.'"
  (let ((workgroup-list (wg-workgroup-list-or-error))
        (workgroup (wg-get-workgroup workgroup t)))
    (wg-switch-to-workgroup
     (cond ((not workgroup) (car workgroup-list))
           ((= 1 (length workgroup-list)) (error "There's only one workgroup"))
           (t (wg-cyclic-nth-from-workgroup workgroup n))))))

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

(defun wg-create-workgroup (name &optional blank)
  "Create and add a workgroup named NAME.
Optional argument BLANK non-nil (set interactively with a prefix
arg) means use a blank, one window window-config.  Otherwise use
the current window-configuration.  Keep in mind that even though
the current window-config may be used, other parameters of the
current workgroup are not copied to the created workgroup.  For
that, use `wg-clone-workgroup'."
  (interactive (list (wg-read-new-workgroup-name) current-prefix-arg))
  (wg-switch-to-workgroup (wg-make-and-add-workgroup name blank))
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
    (setf (wg-workgroup-name clone) name
          (wg-workgroup-uid clone) (wg-generate-uid))
    (when (wg-check-and-add-workgroup clone)
      (wg-flag-workgroup-modified clone))
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

(defun wg-wconfig-kill-ring ()
  "Return `wg-wconfig-kill-ring', creating it first if necessary."
  (or wg-wconfig-kill-ring
      (setq wg-wconfig-kill-ring (make-ring wg-wconfig-kill-ring-max))))

(defun wg-add-to-wconfig-kill-ring (wconfig)
  "Add WCONFIG to `wg-wconfig-kill-ring'."
  (ring-insert (wg-wconfig-kill-ring) wconfig))

(defun wg-kill-workgroup (&optional workgroup)
  "Kill WORKGROUP, saving its working-wconfig to the kill ring."
  (interactive)
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

(defun wg-kill-ring-save-base-wconfig (&optional workgroup)
  "Save WORKGROUP's base wconfig to the kill ring."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-add-to-wconfig-kill-ring (wg-workgroup-base-wconfig workgroup))
    (wg-fontified-message
      (:cmd "Saved: ")
      (:cur (wg-workgroup-name workgroup))
      (:cur "'s ")
      (:msg "base wconfig to the kill ring"))))

(defun wg-kill-ring-save-working-wconfig (&optional workgroup)
  "Save WORKGROUP's working-wconfig to `wg-wconfig-kill-ring'."
  (interactive)
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
  (when (zerop (ring-length (wg-wconfig-kill-ring)))
    (error "The kill-ring is empty"))
  (let ((pos (if (not (eq real-last-command 'wg-yank-wconfig)) 0
               (1+ (or (get 'wg-yank-wconfig :position) 0)))))
    (put 'wg-yank-wconfig :position pos)
    (wg-restore-wconfig-undoably (ring-ref (wg-wconfig-kill-ring) pos))
    (wg-fontified-message
      (:cmd "Yanked: ")
      (:msg (format "%S" pos)) "  "
      (wg-workgroup-list-display))))

(defun wg-kill-workgroup-and-buffers (&optional workgroup)
  "Kill WORKGROUP and the buffers in its working-wconfig."
  (interactive)
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

(defun wg-delete-other-workgroups (&optional workgroup)
  "Delete all workgroups but WORKGROUP."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (unless (or wg-no-confirm-on-destructive-operation
                (y-or-n-p "Really delete all other workgroups? "))
      (error "Cancelled"))
    (dolist (w (wg-workgroup-list-or-error))
      (unless (eq w workgroup)
        (wg-delete-workgroup w)))
    (unless (wg-current-workgroup-p workgroup)
      (wg-switch-to-workgroup workgroup))
    (wg-fontified-message
      (:cmd "Deleted: ")
      (:msg "All workgroups but ")
      (:cur (wg-workgroup-name workgroup)))))



;;; workgroup updating and reverting commands

(defun wg-revert-workgroup (&optional workgroup)
  "Restore WORKGROUP's window configuration to its state at the last save."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (base-wconfig (wg-workgroup-base-wconfig workgroup)))
    (if (wg-current-workgroup-p workgroup)
        (wg-restore-wconfig-undoably base-wconfig)
      (wg-add-wconfig-to-undo-list workgroup base-wconfig))
    (wg-fontified-message
      (:cmd "Reverted: ")
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-revert-all-workgroups ()
  "Revert all workgroups to their base wconfigs.
Only workgroups' working-wconfigs in `selected-frame' are
reverted."
  (interactive)
  (mapc #'wg-revert-workgroup (wg-workgroup-list-or-error))
  (wg-fontified-message
    (:cmd "Reverted: ")
    (:msg "All")))



;;; saved wconfig commands

(defun wg-save-wconfig ()
  "FIXME: docstring this"
  (interactive)
  (let* ((workgroup (wg-current-workgroup))
         (name (read-string "Name: "))
         (wconfig (wg-current-wconfig)))
    (setf (wg-wconfig-name wconfig) name)
    (wg-workgroup-save-wconfig workgroup wconfig)
    (wg-fontified-message
      (:cmd "Saved: ")
      (:cur name))))

(defun wg-restore-saved-wconfig ()
  "FIXME: docstring this"
  (interactive)
  (let ((workgroup (wg-current-workgroup)))
    (wg-restore-wconfig-undoably
     (wg-workgroup-get-saved-wconfig
      workgroup
      (wg-completing-read
       "Saved wconfig: "
       (mapcar 'wg-wconfig-name (wg-workgroup-saved-wconfigs workgroup))
       nil t)))))



;;; workgroup-list reorganization commands

(defun wg-swap-workgroups ()
  "Swap the previous and current workgroups."
  (interactive)
  (wg-swap-workgroups-in-workgroup-list
   (wg-current-workgroup) (wg-previous-workgroup))
  (wg-fontified-message
    (:cmd "Swapped:  ")
    (wg-workgroup-list-display)))

(defun wg-offset-workgroup-left (&optional workgroup n)
  "Offset WORKGROUP leftward in `wg-workgroup-list' cyclically."
  (interactive (list nil current-prefix-arg))
  (wg-cyclic-offset-workgroup (wg-get-workgroup workgroup) (or n -1))
  (wg-fontified-message
    (:cmd "Offset left: ")
    (wg-workgroup-list-display)))

(defun wg-offset-workgroup-right (&optional workgroup n)
  "Offset WORKGROUP rightward in `wg-workgroup-list' cyclically."
  (interactive (list nil current-prefix-arg))
  (wg-cyclic-offset-workgroup (wg-get-workgroup workgroup) (or n 1))
  (wg-fontified-message
    (:cmd "Offset right: ")
    (wg-workgroup-list-display)))



;;; undo/redo commands

(defun wg-undo-wconfig-change (&optional workgroup)
  "Undo a change to the current workgroup's window-configuration."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (undid? (wg-workgroup-offset-position-in-undo-list workgroup 1)))
    (wg-fontified-message
      (:cmd "Undo: ")
      (wg-undo-timeline-display workgroup)
      (:cur (if undid? "" "  No more undo info")))))

(defun wg-redo-wconfig-change (&optional workgroup)
  "Redo a change to the current workgroup's window-configuration."
  (interactive)
  (let* ((workgroup (wg-get-workgroup workgroup))
         (redid? (wg-workgroup-offset-position-in-undo-list workgroup -1)))
    (wg-fontified-message
      (:cmd "Redo: ")
      (wg-undo-timeline-display workgroup)
      (:cur (if redid? "" "  No more redo info")))))

(defun wg-undo-once-all-workgroups ()
  "Do what the name says.  Useful for instance when you
accidentally call `wg-revert-all-workgroups' and want to return
all workgroups to their un-reverted state."
  (interactive)
  (mapc 'wg-undo-wconfig-change (wg-workgroup-list-or-error))
  (wg-message "Undid once on all workgroups."))

(defun wg-redo-once-all-workgroups ()
  "Do what the name says.  Probably useless.  Included for
symetry with `wg-undo-once-all-workgroups'."
  (interactive)
  (mapc 'wg-redo-wconfig-change (wg-workgroup-list-or-error))
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

(defun wg-associate-buffer-with-workgroup (&optional workgroup buffer weak)
  "Associate BUFFER with WORKGROUP.
WEAK non-nil means weakly associate BUFFER."
  (interactive (list nil nil current-prefix-arg))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (buffer (or buffer (current-buffer)))
         (bname (buffer-name buffer))
         (wgname (wg-workgroup-name workgroup)))
    (if (wg-workgroup-associate-bufobj workgroup buffer weak)
        (wg-message "%s-associated %S with %s"
                    (if weak "Weakly" "Strongly") bname wgname)
      (wg-message "%S is already associated with %s" bname wgname))))

(defun wg-associate-visible-buffers-with-workgroup (&optional workgroup weak)
  "Associate all buffers visible in `selected-frame' with WORKGROUP.
WEAK non-nil means weakly associate them.  Otherwise strongly
associate them."
  (interactive (list nil current-prefix-arg))
  (let ((workgroup (wg-get-workgroup workgroup))
        (buffers (mapcar 'window-buffer (window-list))))
    (dolist (buffer buffers)
      (wg-workgroup-associate-bufobj workgroup buffer weak))
    (wg-fontified-message
      (:cmd (format "%s associated: " (if weak "Weakly" "Strongly")))
      (wg-buffer-list-display buffers))))

(defun wg-dissociate-buffer-from-workgroup (&optional workgroup buffer)
  "Dissociate BUFFER from WORKGROUP."
  (interactive (list nil nil))
  (let ((workgroup (wg-get-workgroup workgroup))
        (buffer (or buffer (current-buffer))))
    (wg-message
     (if (wg-workgroup-dissociate-bufobj workgroup buffer)
         "Dissociated %S from %s" "%S isn't associated with %s")
     (wg-buffer-name buffer)
     (wg-workgroup-name workgroup))))

(defun wg-restore-workgroup-associated-buffers (&optional workgroup)
  "Restore all the buffers associated with WORKGROUP that can be restored."
  (interactive)
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
  (let* ((workgroup (wg-current-workgroup))
         (buffer (current-buffer))
         (type (wg-workgroup-cycle-bufobj-association-type workgroup buffer)))
    (force-mode-line-update)
    (wg-fontified-message
      (:cur (buffer-name buffer))
      (:cmd (case type
              (strong " strongly associated with ")
              (weak " weakly associated with ")
              (otherwise " unassociated with ")))
      (:cur (wg-workgroup-name workgroup)))))

(defun wg-dissociate-weakly-associated-buffers (&optional workgroup)
  "Dissociate from the current workgroup all weakly associated buffers."
  (interactive)
  (let ((workgroup (wg-get-workgroup workgroup)))
    (wg-workgroup-dissociate-weakly-associated-buffers workgroup)
    (wg-fontified-message
      (:cmd "Remaining buffers: ")
      (wg-buffer-list-display (wg-workgroup-associated-buffers workgroup)))))



;;; window-tree commands
;;
;; TODO: These are half-hearted.  Clean them up; allow specification of the
;; window-tree depth at which to operate; add complex window creation commands;
;; and add window splitting, deletion and locking commands.

(defun wg-transpose-window-internal (workgroup offset)
  "Move `selected-window' by OFFSET in its wlist."
  (wg-restore-wconfig-undoably
   (wg-wconfig-move-window
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    offset)))

(defun wg-backward-transpose-window (&optional workgroup offset)
  "Move `selected-window' backward by OFFSET in its wlist."
  (interactive (list nil current-prefix-arg))
  (wg-transpose-window-internal workgroup (or offset -1)))

(defun wg-transpose-window (&optional workgroup offset)
  "Move `selected-window' forward by OFFSET in its wlist."
  (interactive (list nil current-prefix-arg))
  (wg-transpose-window-internal workgroup (or offset 1)))

(defun wg-reverse-frame-horizontally (&optional workgroup)
  "Reverse the order of all horizontally split wtrees."
  (interactive)
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup)))))

(defun wg-reverse-frame-vertically (&optional workgroup)
  "Reverse the order of all vertically split wtrees."
  (interactive)
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    t)))

(defun wg-reverse-frame-horizontally-and-vertically (&optional workgroup)
  "Reverse the order of all wtrees."
  (interactive)
  (wg-restore-wconfig-undoably
   (wg-reverse-wconfig
    (wg-workgroup-working-wconfig
     (wg-get-workgroup workgroup))
    'both)))

;; FIXME: add dedicated indicator to the mode-line display
(defun wg-toggle-window-dedicated-p ()
  "Toggle `window-dedicated-p' in `selected-window'."
  (interactive)
  (set-window-dedicated-p nil (not (window-dedicated-p)))
  (wg-fontified-message
    (:cmd "Window:")
    (:cur (concat (unless (window-dedicated-p) " not") " dedicated"))))



;;; misc commands

(defun wg-rename-workgroup (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list nil (wg-read-new-workgroup-name "New name: ")))
  (let* ((workgroup (wg-get-workgroup workgroup))
         (oldname (wg-workgroup-name workgroup)))
    (setf (wg-workgroup-name workgroup) newname)
    (wg-flag-workgroup-modified workgroup)
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

(defun wg-reset-buffer (buffer)
  "Return BUFFER.
Currently only sets BUFFER's `wg-buffer-uid' to nil."
  (with-current-buffer buffer
    (setq wg-buffer-uid nil)))

(defun wg-reset (&optional force)
  "Reset Workgroups.
Resets all frame parameters, buffer-local vars, the current
Workgroups session object, etc."
  (interactive "P")
  (unless (or force wg-no-confirm-on-destructive-operation
              (y-or-n-p "Really reset Workgroups? "))
    (error "Canceled"))
  (mapc 'wg-reset-frame (frame-list))
  (mapc 'wg-reset-buffer (buffer-list))
  (setq wg-current-session nil
        wg-wconfig-kill-ring nil)
  (wg-fontified-message
    (:cmd "Reset: ")
    (:msg "Workgroups")))



;;; file commands

(defun wg-write-session-file (filename &optional confirm)
  "Write the current session into file FILENAME.
This makes the session visit that file, and marks it as not modified.

If optional second arg CONFIRM is non-nil, this function asks for
confirmation before overwriting an existing file.  Interactively,
confirmation is required unless you supply a prefix argument.

Think of it as `write-file' for Workgroups sessions."
  (interactive (list (read-file-name "Save session as: ")
                     (not current-prefix-arg)))
  (when (and confirm (file-exists-p filename))
    (unless (y-or-n-p (format "File `%s' exists; overwrite? " filename))
      (error "Cancelled")))
  (wg-perform-session-maintenance)
  (setf (wg-session-file-name (wg-current-session)) filename)
  (wg-write-sexp-to-file
   (wg-pickel-all-session-parameters
    (wg-current-session)) filename)
  (wg-mark-everything-unmodified)
  (wg-fontified-message (:cmd "Wrote: ") (:file filename)))

(defun wg-save-session ()
  "Save the current Workgroups session to its visited file if modified.
Think of it as `save-buffer' for Workgroups sessions."
  (interactive)
  (let ((filename (wg-session-file-name (wg-current-session))))
    (cond ((not (wg-modified-p))
           (wg-message "(This Workgroups session is unmodified)"))
          (filename
           (wg-write-session-file filename))
          (t
           (call-interactively 'wg-write-session-file)))))

(defun wg-query-for-save ()
  "Query for save when `wg-modified-p'."
  (or (not (wg-modified-p))
      (not (y-or-n-p "Save modified workgroups? "))
      (call-interactively 'wg-save-session)
      t))

(defun wg-find-session-file (filename)
  "Load a session visiting FILENAME, creating one if none already exists."
  (interactive "FFind session file: ")
  (cond ((file-exists-p filename)
         (let ((session (wg-read-sexp-from-file filename)))
           (unless (wg-session-p session)
             (error "%S is not a Workgroups session file." filename))
           (wg-reset t)
           (setq wg-current-session (wg-unpickel-session-parameters session)))
         (setf (wg-session-file-name (wg-current-session)) filename)
         (wg-awhen (and wg-switch-to-first-workgroup-on-find-session-file
                        (wg-workgroup-list))
           (wg-switch-to-workgroup (car it)))
         (wg-fontified-message
           (:cmd "Loaded: ")
           (:file filename)))
        (t
         (when (wg-query-for-save)
           (wg-reset t)
           (setf (wg-session-file-name (wg-current-session)) filename)
           (wg-fontified-message
             (:cmd "(New Workgroups session file)"))))))

(defun wg-find-file-in-new-workgroup (filename)
  "Create a new blank workgroup and find file FILENAME in it."
  (interactive "FFind file in new workgroup: ")
  (wg-create-workgroup (file-name-nondirectory filename) t)
  (find-file filename))

(defun wg-find-file-read-only-in-new-workgroup (filename)
  "Create a new workgroup and find file FILENAME read-only in it."
  (interactive "FFind file read only in new workgroup: ")
  (wg-create-workgroup (file-name-nondirectory filename) t)
  (find-file-read-only filename))

(defun wg-dired-in-new-workgroup (dirname &optional switches)
  "Create a workgroup and open DIRNAME in dired with SWITCHES."
  (interactive (list (read-directory-name "Dired (directory): ")
                     current-prefix-arg))
  (wg-create-workgroup dirname)
  (dired dirname switches))



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
  (wg-when-let
      ((mode (wg-read-buffer-mode))
       (buffer (wg-current-match mode))
       (pos (position buffer (wg-filtered-buffer-list t) :test 'equal)))
    (wg-workgroup-dissociate-bufobj (wg-current-workgroup) buffer)
    (wg-set-current-matches
     (wg-rotate-list (wg-filtered-buffer-list t) pos) mode)))

(defun wg-associate-first-match ()
  "Associate the first match with or update it in the current workgroup."
  (interactive)
  (wg-when-let
      ((mode (wg-read-buffer-mode))
       (buffer (wg-current-match mode))
       (pos (position buffer (wg-filtered-buffer-list t) :test 'equal)))
    (wg-workgroup-associate-bufobj (wg-current-workgroup) buffer)
    (wg-set-current-matches
     (wg-rotate-list (wg-filtered-buffer-list t) pos) mode)))

(defun wg-minibuffer-mode-dissociate-weakly-associated-buffers ()
  "Dissociate weakly associated buffers and update the current matches."
  (interactive)
  (wg-workgroup-dissociate-weakly-associated-buffers (wg-current-workgroup))
  (wg-set-current-matches
   (let ((remaining (wg-filtered-buffer-list t)))
     (remove-if-not (lambda (match) (member match remaining))
                    (wg-current-matches)))))


;;; advice

;; buffer auto-association advice

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
  (when wg-buffer-auto-association-on
    (wg-when-let ((wg (wg-current-workgroup t frame)))
      (unless (wg-workgroup-bufobj-association-type wg buffer)
        (wg-auto-associate-buffer-helper
         wg buffer (or (wg-workgroup-parameter wg 'buffer-auto-association)
                       wg-buffer-auto-association))))))

(defadvice switch-to-buffer (after wg-auto-associate-buffer)
  "Automatically associate the buffer with the current workgroup."
  (wg-auto-associate-buffer ad-return-value))

(defadvice set-window-buffer (after wg-auto-associate-buffer)
  "Automatically associate the buffer with the current workgroup."
  (wg-auto-associate-buffer
   (ad-get-arg 1)
   (window-frame (or (ad-get-arg 0) (selected-window)))))


;; save-buffers-kill-emacs advice

(defadvice save-buffers-kill-emacs (around wg-freeze-wconfig)
  "`save-buffers-kill-emacs' calls `list-processes' when active
processes exist, screwing up the window config right before
Workgroups saves it.  This advice freezes `wg-current-wconfig' in
its correct state, prior to any window-config changes caused by
`s-b-k-e'."
  (wg-with-current-wconfig (wg-frame-to-wconfig)
    ad-do-it))


(defun wg-enable-all-advice ()
  "Enable and activate all of Workgroups' advice."

  ;; switch-to-buffer
  (ad-define-subr-args
   'switch-to-buffer '(buffer-or-name &optional norecord))
  (ad-enable-advice 'switch-to-buffer 'after 'wg-auto-associate-buffer)
  (ad-activate 'switch-to-buffer)

  ;; set-window-buffer
  (ad-define-subr-args
   'set-window-buffer '(window buffer-or-name &optional keep-margins))
  (ad-enable-advice 'set-window-buffer 'after 'wg-auto-associate-buffer)
  (ad-activate 'set-window-buffer)

  ;; save-buffers-kill-emacs
  (ad-enable-advice 'save-buffers-kill-emacs 'around 'wg-freeze-wconfig)
  (ad-activate 'save-buffers-kill-emacs)

  )


(defun wg-disable-all-advice ()
  "Disable and deactivate all of Workgroups' advice."

  ;; switch-to-buffer
  (ad-disable-advice 'switch-to-buffer 'after 'wg-auto-associate-buffer)
  (ad-deactivate 'switch-to-buffer)

  ;; set-window-buffer
  (ad-disable-advice 'set-window-buffer 'after 'wg-auto-associate-buffer)
  (ad-deactivate 'set-window-buffer)

  ;; save-buffers-kill-emacs
  (ad-disable-advice 'save-buffers-kill-emacs 'around 'wg-freeze-wconfig)
  (ad-deactivate 'save-buffers-kill-emacs)

  )



;;; keymaps

(defvar wg-prefixed-map
  (wg-fill-keymap
   (make-sparse-keymap)


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
   (kbd "~")          'wg-associate-visible-buffers-with-workgroup
   (kbd "-")          'wg-dissociate-buffer-from-workgroup
   (kbd "=")          'wg-cycle-buffer-association-type
   (kbd "*")          'wg-restore-workgroup-associated-buffers
   (kbd "_")          'wg-dissociate-weakly-associated-buffers
   (kbd "(")          'wg-next-buffer
   (kbd ")")          'wg-previous-buffer


   ;; workgroup movement

   (kbd "C-x")        'wg-swap-workgroups
   (kbd "C-,")        'wg-offset-workgroup-left
   (kbd "C-.")        'wg-offset-workgroup-right


   ;; file and buffer

   (kbd "C-s")        'wg-save-session
   (kbd "C-w")        'wg-write-session-file
   (kbd "C-f")        'wg-find-session-file
   (kbd "F")          'wg-find-file-in-new-workgroup
   (kbd "M-F")        'wg-find-file-read-only-in-new-workgroup
   (kbd "d")          'wg-dired-in-new-workgroup
   (kbd "C-b")        'wg-switch-to-buffer
   (kbd "b")          'wg-switch-to-buffer


   ;; window moving and frame reversal

   (kbd "<")          'wg-backward-transpose-window
   (kbd ">")          'wg-transpose-window
   (kbd "|")          'wg-reverse-frame-horizontally
   (kbd "\\")         'wg-reverse-frame-vertically
   (kbd "/")          'wg-reverse-frame-horizontally-and-vertically


   ;; toggling

   (kbd "C-t C-m")    'wg-toggle-mode-line-display
   (kbd "C-t C-m")    'wg-toggle-morph
   (kbd "C-t C-b")    'wg-toggle-buffer-list-filtration


   ;; echoing

   (kbd "S-C-e")      'wg-echo-current-workgroup
   (kbd "E")          'wg-echo-current-workgroup
   (kbd "C-e")        'wg-echo-all-workgroups
   (kbd "e")          'wg-echo-all-workgroups
   (kbd "C-t")        'wg-echo-time
   (kbd "T")          'wg-echo-time
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
  (wg-fill-keymap
   (make-sparse-keymap)
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
   (kbd "C-c _")     'wg-minibuffer-mode-dissociate-weakly-associated-buffers
   )
  "`wg-minibuffer-mode's keymap.")



;;; workgroups-everywhere

(define-minor-mode workgroups-everywhere
  "Use Workgroups' buffer list filters everywhere `read-buffer' is used."
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
  (wg-minibuffer-mode -1))



;;; workgroups-mode

(defun wg-save-session-on-emacs-exit ()
  "Conditionally call `wg-query-for-save'.
Added to `kill-emacs-query-functions'."
  (case wg-emacs-exit-save-behavior
    (save (call-interactively 'wg-save-session) t)
    (query (wg-query-for-save) t)
    (nosave t)))

(defun wg-save-session-on-workgroups-mode-exit ()
  "Conditionally call `wg-query-for-save'.
Called when `workgroups-mode' is turned off."
  (case wg-workgroups-mode-exit-save-behavior
    (save (call-interactively 'wg-save-session) t)
    (query (wg-query-for-save) t)
    (nosave t)))

(defun wg-add-or-remove-workgroups-hooks (remove)
  "Add or remove all of Workgroups' hooks, depending on REMOVE."
  (wg-add-or-remove-hooks
   remove
   'kill-emacs-query-functions 'wg-save-session-on-emacs-exit
   'delete-frame-hook 'wg-update-base-wconfigs-from-frame-if-necessary
   'window-configuration-change-hook 'wg-flag-window-configuration-changed
   'pre-command-hook 'wg-update-working-wconfig-before-command
   'post-command-hook 'wg-undoify-window-configuration-change
   'minibuffer-setup-hook 'wg-turn-on-minibuffer-mode
   'minibuffer-exit-hook 'wg-flag-just-exited-minibuffer
   'minibuffer-exit-hook 'wg-turn-off-minibuffer-mode
   'ido-make-buffer-list-hook 'wg-set-ido-buffer-list
   'iswitchb-make-buflist-hook 'wg-set-iswitchb-buffer-list
   'kill-buffer-hook 'wg-auto-dissociate-buffer-hook
   'kill-buffer-hook 'wg-update-buffer-in-buf-list))

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
    (wg-save-session-on-workgroups-mode-exit)
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
