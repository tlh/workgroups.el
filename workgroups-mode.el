;;; workgroups-mode.el --- workgroups for windows

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:     workgroups-mode.el
;; Author:   tlh <thunkout@gmail.com>
;; Created:  2010-07-22
;; Version   0.1.7
;; Keywords: session manager window-configuration persistence

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
;;   workgroups-mode.el is a session management package for GNU Emacs.
;;   It allows you to store and restore window configurations, save
;;   them to and load them from disk, and perform various other
;;   operations on them.  workgroups-mode saves the window layout of
;;   the current frame, as well as each window's buffer's filename if
;;   it's visiting a file, or its buffername otherwise.  And that's
;;   it. It doesn't try to save complicated information about the
;;   buffer, like major or minor modes.  If you save workgroups that
;;   include things like erc or gnus buffers, you should launch those
;;   applications and buffers again in your next session before
;;   restoring the workgroup that includes them. Nothing bad will
;;   happen otherwise, of course.  workgroups-mode will just default
;;   to a buffer that already exists, like *scratch*.
;;
;;   `workgroups-list' contains all the currently available
;;   workgroups.  You can switch to workgroups (i.e. restore window
;;   configurations), bury them, go to the previous or next workgroup
;;   circularly, etc.  `workgroups-save' saves `workgroups-list' to a
;;   file, which can then be loaded in another session.  Workgroups
;;   are added to `workgroups-list' by calling `workgroups-create',
;;   removed by calling `workgroups-kill', and can be moved to the end
;;   of `workgroups-list' by calling `workgroups-bury'.  In general,
;;   operations on workgroups and `workgroups-list' behave as
;;   similarly to buffers and buffer-lists as possible.
;;

;;; Installation:
;;
;;   - Put `workgroups-mode.el' somewhere on your emacs load path
;;
;;   - Add this line to your .emacs file:
;;
;;     (require 'workgroups-mode)
;;

;;; Configuration:
;;
;;   To turn on workgroups-mode, either issue the command:
;;
;;     M-x workgroups-mode
;;
;;   Or put this in your .emacs file:
;;
;;     (workgroups-mode t)
;;
;;   To start off, you should add a few workgroups.  When your frame
;;   is in a state that you'd like to save, run the command
;;   `workgroups-create', and give the workgroup a name when prompted.
;;   Once you've added a few workgroups with `workgroups-create', you
;;   should save them to a file with `workgroups-save'.  You can
;;   designate this file to be automatically loaded when
;;   workgroups-mode is started by setting `workgroups-default-file'
;;   like so:
;;
;;     (setq workgroups-default-file "/path/to/workgroups/file")
;;
;;   With these two options set, workgroups mode will automatically
;;   load the default file and switch to the first workgroup in it at
;;   emacs startup.
;;
;;   Check the documentation of the customizable variables below for
;;   more configuration options.
;;

;;; TODO:
;;
;;  - change goddam "workgroups" to something else. "frameup"?
;;  - add minibuffer state to configs
;;  - undo/redo?
;;


;; A "window configuration" records the entire layout of one frame--all
;; windows, their sizes, which buffers they contain, how those buffers are
;; scrolled, and their values of point and the mark; also their fringes,
;; margins, and scroll bar settings.  It also includes the value of
;; `minibuffer-scroll-window'.  As a special exception, the window
;; configuration does not record the value of point in the selected window
;; for the current buffer.  Also, the window configuration does not record
;; the values of window parameters; see *Note Window Parameters::.




;;; Code:

(require 'cl)


;;; consts

(defconst workgroups-version "0.1.7"
  "Current version number of workgroups.")

(defconst workgroups-fid '-*-workgroups-*-
  "Symbol identifying a workgroups file.")


;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- Emacs session manager"
  :group 'convenience
  :version workgroups-version)

;; (defcustom workgroups-prefix-key (kbd "C-z")
;;   "Workgroups' prefix key."
;;   :type 'string
;;   :group 'workgroups
;;   :set (lambda (sym val)
;;          (when (fboundp 'workgroups-set-prefix-key)
;;            (workgroups-set-prefix-key val)
;;            (custom-set-default sym val))))

(defcustom workgroups-prefix-key (kbd "C-z")
  "Workgroups' prefix key."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-switch-hook nil
  "Hook run whenever a workgroup is switched to."
  :type 'hook
  :group 'workgroups)

(defcustom workgroups-restore-position nil
  "Non-nil means restore the frame's position when switching to a
workgroup."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-restore-size t
  "Non-nil means restore the frame's size when switching to a
workgroup."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-default-buffer "*scratch*"
  "Buffer name of the buffer to switch to when a new workgroup is
created."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-use-faces t
  "Non-nil means use faces in various displays."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-switch-on-load t
  "Non-nil means automatically switch to the first workgroup in a
file when the file is loaded."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-query-save-on-exit t
  "Non-nil means query for save before exiting when there are
unsaved changes."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-kill-ring-size 20
  "Maximum length of the workgroups kill-ring."
  :type 'integer
  :group 'workgroups)

(defcustom workgroups-time-format "%H:%M:%S %A, %B %d %Y"
  "Format string for time display.
Passed to `format-time-string'."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-display-battery t
  "Non-nil mean include battery info in the time display."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-frame-wipe-on t
  "Non-nil means use the frame wipe animation when switching to a
workgroup."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-frame-wipe-horizontal-factor 25
  "Windows are enlarged horizontally in increments of this
numbers of columns during frame wiping."
  :type 'integer
  :group 'workgroups)

(defcustom workgroups-frame-wipe-vertical-factor 8
  "Windows are enlarged vertically in increments of this numbers
of rows during frame wiping."
  :type 'integer
  :group 'workgroups)

(defcustom workgroups-frame-wipe-speed 0.001
  "Number of seconds to sit between window enlargement calls
during frame wipe."
  :type 'integer
  :group 'workgroups)

(defcustom workgroups-current-left-decoration "*" ;; "-<{ "
  "String displayed to the left of the current workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-current-right-decoration "" ;; " }>-"
  "String displayed to the right of the current workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-previous-left-decoration "~" ;; "*"
  "String displayed to the left of the previous workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-previous-right-decoration "" ;; "*"
  "String displayed to the right of the previous workgroup in the
list display."
  :type 'string
  :group 'workgroups)


;;; faces

(defface workgroups-command-face
  '((((class color)) (:foreground "aquamarine")))
  "Face used for command/operation names."
  :group 'workgroups)

(defface workgroups-current-workgroup-face
  '((((class color)) (:foreground "white")))
  "Face used for the current workgroup."
  :group 'workgroups)

(defface workgroups-previous-workgroup-face
  '((((class color)) (:foreground "light sky blue")))
  "Face used for the previous workgroup."
  :group 'workgroups)

(defface workgroups-other-workgroup-face
  '((((class color)) (:foreground "light slate grey")))
  "Face used for workgroups that aren't current or previous."
  :group 'workgroups)

(defface workgroups-divider-face
  '((((class color)) (:foreground "light slate blue")))
  "Face used for dividers."
  :group 'workgroups)

(defface workgroups-message-ace
  '((((class color)) (:foreground "light sky blue")))
  "Face used for messages."
  :group 'workgroups)

(defface workgroups-mode-line-face
  '((((class color)) (:foreground "light sky blue")))
  "Face used for mode-line display."
  :group 'workgroups)

(defface workgroups-filename-face
  '((((class color)) (:foreground "light sky blue")))
  "Face used for the filenames."
  :group 'workgroups)

(defvar workgroups-face-abbrevs
  '((cmd  . workgroups-command-face)
    (cur  . workgroups-current-workgroup-face)
    (prev . workgroups-previous-workgroup-face)
    (oth  . workgroups-other-workgroup-face)
    (div  . workgroups-divider-face)
    (msg  . workgroups-message-face)
    (mode . workgroups-mode-line-face)
    (file . workgroups-filename-face))
  "Assoc list mapping face abbreviations to face names.")


;;; vars

(defvar workgroups-file nil
  "Current workgroups file.")

(defvar workgroups-dirty nil
  "Non-nil when there are unsaved changes.")

(defvar workgroups-list nil
  "Current list of workgroups.")

(defvar workgroups-frame-table (make-hash-table)
  "Hash table storing global state for each frame.")

(defvar workgroups-frame nil
  "Bind this to operate on a non-`selected-frame' frame.")

(defvar workgroups-kill-ring nil
  "List of saved configs.")

(defvar workgroups-kill-ring-pos 0
  "Position in `workgroups-kill-ring' during sequential yanks.")

(defvar workgroups-mode-line-on t
  "Toggles workgroups' mode-line display.")

(defvar workgroups-mode-line-string "[unset]"
  "String displayed in the mode-line.")

(defvar workgroups-divider-str "|"
  "Divider used in workgroups list strings.")

(defvar workgroups-warning-timeout 0.7
  "Duration in seconds to display invalid input messages when
  reading from the minibuffer.")


;;; utils

(defmacro workgroups-dohash (bindings &rest body)
  "do-style wrapper for maphash."
  (declare (indent defun))
  (destructuring-bind (key val table &optional ret) bindings
    `(progn (maphash (lambda (,key ,val) ,@body) ,table)
            ,ret)))

(defmacro doconcat (binds &rest body)
  "do-style wrapper for mapconcat."
  (declare (indent defun))
  (destructuring-bind (elt seq &optional sep) binds
    `(mapconcat (lambda (,elt) ,@body) ,seq (or ,sep ""))))

(defmacro workgroups-aif (test then &rest else)
  "Anaphoric if."
  (declare (indent defun))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro workgroups-awhen (test &rest body)
  "Anaphoric when."
  (declare (indent defun))
  `(workgroups-aif ,test (progn ,@body)))

(defmacro workgroups-abind (alst keylist &rest body)
  "Bind keys in KEYLIST to their values in ALST, then eval BODY.
Requires that all keys in ALST are bindable symbols."
  (declare (indent defun))
  (let ((asym (gensym)))
    `(let* ((,asym ,alst)
            ,@(mapcar (lambda (key) `(,key (cdr (assoc ',key ,asym))))
                      keylist))
       ,@body)))

(defun workgroups-lnext (obj lst)
  "Return elt after OBJ in LST, or nil."
  (cadr (member obj lst)))

(defun workgroups-lprev (obj lst)
  "Return elt before OBJ in LST, or nil."
  (let ((prev (car lst)))
    (catch 'res
      (dolist (elt (cdr lst) nil)
        (and (equal obj elt) (throw 'res prev))
        (setq prev elt)))))

(defun workgroups-cnext (elt lst)
  "Return elt after ELT in LIST, or car of LIST."
  (or (workgroups-lnext elt lst) (car lst)))

(defun workgroups-cprev (elt lst)
  "Return elt before ELT in LST, or car of last of LST."
  (or (workgroups-lprev elt lst) (car (last lst))))

(defun workgroups-take (lst n)
  "Return a list of the first N elts in LST."
  (butlast lst (- (length lst) n)))

(defun workgroups-linsert (elt lst &optional pos)
  "Insert ELT into LST at POS or the end."
  (let ((pos (or pos (length lst))))
    (append (workgroups-take lst pos) (cons elt (nthcdr pos lst)))))

(defun workgroups-access (key alst)
  "Return the value portion from associng KEY in ALST."
  (cdr (assoc key alst)))

(defun workgroups-aassoc (key val aalst &optional test)
  "Return the alst in aalst containing KEY with val VAL."
  (let ((test (or test 'equal)))
    (catch 'res
      (dolist (alst aalst)
        (when (funcall test val (workgroups-access key alst))
          (throw 'res alst))))))

(defun workgroups-partition (lst n)
  "Return list of contiguous N-length sublists of LST.
Length of last sublist may be less than N.  Iterative to prevent
stack overflow."
  (when (< n 1) (error "N must be greater than zero."))
  (let (acc)
    (while lst
      (push (workgroups-take lst n) acc)
      (setq lst (nthcdr n lst)))
    (nreverse acc)))

(defun workgroups-util-swap (elt1 elt2 lst)
  "Return a copy of LST with ELT1 with ELT2 swapped.
Return nil when ELT1 and ELT2 aren't both present."
  (let* ((lst (copy-list lst))
         (l1  (member elt1 lst))
         (l2  (member elt2 lst)))
    (if (not (and l1 l2)) nil
      (setcar l1 elt2)
      (setcar l2 elt1)
      lst)))

(defun workgroups-fill-keymap (keymap &rest binds)
  "Fill KEYMAP with BINDS."
  (dolist (bind (workgroups-partition binds 2) keymap)
    (define-key keymap (read-kbd-macro (car bind)) (cadr bind))))

(defun workgroups-save-sexp-to-file (sexp file)
  "Write the printable representation of SEXP to FILE."
  (with-temp-buffer
    (insert (format "%S" sexp))
    (write-file file)))

(defun workgroups-read-sexp-from-file (file)
  "Read and s-expression from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun workgroups-facify (face str)
  "Return a copy of STR fontified with FACE."
  (let ((str (format "%s" str)))
    (if (not workgroups-use-faces) str
      (put-text-property
       0 (length str) 'face
       (workgroups-access face workgroups-face-abbrevs)
       str)
      str)))

(defun workgroups-window-list (&optional frame)
  "Flatten `window-tree' into a stable list.
`window-list' can't be used because its order isn't stable."
  (flet ((inner (obj) (if (atom obj) (list obj)
                        (mapcan 'inner (cddr obj)))))
    (inner (car (window-tree (or frame (selected-frame)))))))


;;; global accessors

(defun workgroups-file (&optional noerror)
  "Return `workgroups-file'."
  (or workgroups-file
      (unless noerror
        (error "Workgroups isn't visiting a file"))))

(defun workgroups-list (&optional noerror)
  "Return `workgroups-list'."
  (or workgroups-list
      (unless noerror
        (error "No workgroups are defined."))))

(defun workgroups-get (key val &optional noerror)
  "Return the workgroup whose KEY equals VAL."
  (or (workgroups-aassoc key val (workgroups-list noerror))
      (unless noerror
        (error "There is no workgroup with an %S of %S" key val))))


;;; frame state ops

(defmacro workgroups-bind-frame (frame &rest body)
  "Bind FRAME to `workgroups-frame' or `selected-frame' in BODY."
  (declare (indent defun))
  `(let ((,frame (or workgroups-frame (selected-frame))))
     ,@body))

(defmacro workgroups-frame-context (frame state &rest body)
  "Bind FRAME and STATE in BODY.
STATE is bound to FRAME's value in `workgroups-frame-table'."
  (declare (indent defun))
  `(workgroups-bind-frame ,frame
     (let ((,state (gethash ,frame workgroups-frame-table)))
       ,@body)))

(defun workgroups-frame-val (key)
  "Return KEY's value in `workgroups-frame-table'."
  (workgroups-frame-context frame state
    (workgroups-access key state)))

(defun workgroups-set-frame-val (key val)
  "Set KEY to VAL in `workgroups-frame-table'."
  (workgroups-frame-context frame state
    (aif (assoc key state) (setcdr it val)
         (push (cons key val)
               (gethash frame workgroups-frame-table)))))

(defun workgroups-delete-frame-key (key)
  "Remove KEY from frame's entry in `workgroups-frame-table'."
  (workgroups-frame-context frame state
    (puthash frame (remove (assoc key state) state)
             workgroups-frame-table)))


;;; current and previous workgroup ops

(defun workgroups-current (&optional noerror)
  "Return the current workgroup."
  (workgroups-aif (workgroups-frame-val :current)
    (workgroups-get :guid it)
    (unless noerror
      (error "There's no current workgroup in this frame"))))

(defun workgroups-set-current (val)
  "Set the current workgroup to VAL."
  (workgroups-set-frame-val :current (workgroups-access :guid val)))

(defun workgroups-previous (&optional noerror)
  "Return `workgroups-previous'."
  (workgroups-aif (workgroups-frame-val :previous)
    (workgroups-get :guid it)
    (unless noerror
      (error "There's no previous workgroup in this frame"))))

(defun workgroups-set-previous (val)
  "Set the previous workgroups."
  (workgroups-set-frame-val :previous (workgroups-access :guid val)))


;;; workgroup property ops

(defun workgroups-new-guid ()
  "Return an id unused in `workgroups-list'."
  (let ((guids (workgroups-guids t)) (i -1))
    (while (memq (incf i) guids)) i))

(defun workgroups-guid (workgroup)
  "Return WORKGROUP's guid."
  (workgroups-access :guid workgroup))

(defun workgroups-set-guid (workgroup guid)
  "Set the guid of WORKGROUP to GUID."
  (setcdr (assoc :guid workgroup) guid)
  (setq workgroups-dirty t))

(defun workgroups-guids (&optional noerror)
  "Return a list of workgroups guids."
  (mapcar 'workgroups-guid (workgroups-list noerror)))

(defun workgroups-name (workgroup)
  "Return the name of WORKGROUP."
  (workgroups-access :name workgroup))

(defun workgroups-set-name (workgroup name)
  "Set the name of WORKGROUP to NAME."
  (setcdr (assoc :name workgroup) name)
  (setq workgroups-dirty t))

(defun workgroups-names (&optional noerror)
  "Return a list of workgroups names."
  (mapcar 'workgroups-name (workgroups-list noerror)))

(defun workgroups-base-config (workgroup)
  "Return the base config of WORKGROUP."
  (workgroups-access :config workgroup))

(defun workgroups-set-base-config (workgroup config)
  "Set the base config of WORKGROUP to CONFIG."
  (setcdr (assoc :config workgroup) config)
  (setq workgroups-dirty t))

(defun workgroups-working-config (workgroup &optional noerror)
  "Return the working config of WORKGROUP."
  (or (workgroups-frame-val (workgroups-access :guid workgroup))
      (unless noerror
        (error "%s's working config is unset." (workgroups-name workgroup)))))

(defun workgroups-set-working-config (workgroup config)
  "Set the working config of WORKGROUP to CONFIG."
  (workgroups-set-frame-val (workgroups-access :guid workgroup) config))


;;; workgroups-list ops

(defun workgroups-delete (wg)
  "Remove WORKGROUP from `workgroups-list'."
  (workgroups-dohash (frame state workgroups-frame-table)
    (let ((workgroups-frame frame))
      (workgroups-delete-frame-key (workgroups-access :guid wg))
      (when (eq wg (workgroups-current t))
        (workgroups-set-current nil))
      (when (eq wg (workgroups-previous t))
        (workgroups-set-previous nil))))
  (setq workgroups-dirty t)
  (setq workgroups-list (remove wg (workgroups-list))))

(defun workgroups-add (new &optional pos)
  "Add WORKGROUP at `workgroups-list'.
If a workgroup with the same name exists, query to overwrite it."
  (workgroups-awhen (workgroups-get :name (workgroups-name new) t)
    (unless pos (setq pos (position it workgroups-list)))
    (workgroups-delete it))
  (workgroups-set-guid new (workgroups-new-guid))
  (setq workgroups-dirty t)
  (setq workgroups-list (workgroups-linsert new workgroups-list pos)))

(defun workgroups-list-swap (w1 w2)
  "Swap W1 and W2 in `workgroups-list'."
  (when (eq w1 w2) (error "Can't swap a workgroup with itself"))
  (workgroups-aif (workgroups-util-swap w1 w2 (workgroups-list))
    (setq workgroups-list it workgroups-dirty t)
    (error "W1 and W2 aren't both present in `workgroups-list'.")))


;;; workgroup making

(defun workgroups-window-p (window)
  "t if WINDOW is a workgroups window, nil otherwise."
  (and (consp window) (assoc 'width window)))

(defun workgroups-make-window (winobj)
  "Return a workgroups window from WINOBJ.
WINOBJ is an Emacs window object."
  (with-current-buffer (window-buffer winobj)
    (let ((p (point)) (edges (window-edges winobj)))
      `((width  .  ,(- (nth 2 edges) (nth 0 edges)))
        (height .  ,(window-height winobj))
        (bname  .  ,(buffer-name))
        (fname  .  ,(buffer-file-name))
        (point  .  ,(if (eq p (point-max)) :max p))
        (mark   .  ,(mark))
        (markx  .  ,mark-active)
        (wstart .  ,(window-start winobj))
        (sbars  .  ,(window-scroll-bars winobj))))))

(defun workgroups-make-window-tree (wtree)
  "Return a workgroups window tree from WTREE."
  (etypecase wtree
    (window (workgroups-make-window wtree))
    (cons `(,@(workgroups-take wtree 2)
            ,@(mapcar 'workgroups-make-window-tree
                      (cddr wtree))))))

(defun workgroups-make-window-config ()
  "Return a workgroups window config."
  (workgroups-bind-frame frame
    (let ((wl (workgroups-window-list frame)))
      (flet ((fparam (param) (frame-parameter frame param)))
        `((left   . ,(fparam 'left))
          (top    . ,(fparam 'top))
          (width  . ,(fparam 'width))
          (height . ,(fparam 'height))
          (sbars  . ,(fparam 'vertical-scroll-bars))
          (sbwid  . ,(fparam 'scroll-bar-width))
          (swin   . ,(position (frame-selected-window frame) wl))
          (mbswidx . ,(position minibuffer-scroll-window wl))
          (wtree  . ,(workgroups-make-window-tree
                      (car (window-tree frame)))))))))

;; (let ((f (selected-frame))) (position (frame-selected-window f) (workgroups-window-list f)))
;; (frame-parameter (selected-frame) 'vertical-scroll-bars)
;; (frame-parameter (selected-frame) 'scroll-bar-width)
;; (frame-current-scroll-bars)
;; (window-current-scroll-bars)
;; (window-scroll-bars)
;; (frame-parameters (selected-frame))
;; (frame-parameter (selected-frame) 'scroll-bars-width)
;; (frame-parameter (selected-frame) 'vertical-scroll-bars)
;; (frame-parameter nil 'left 10)
;; (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
;; (set-frame-parameter (selected-frame) 'width 200)

;; asdf

(defun workgroups-make-default-window-config (&optional buffer)
  "Return a new default config."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer workgroups-default-buffer))
    (workgroups-make-window-config)))

(defun workgroups-make-workgroup (guid name config)
  "Make a workgroup named NAME from BASE and WORKING."
  `((:guid    .  ,guid)
    (:name    .  ,name)
    (:config  .  ,config)))

(defun workgroups-copy-workgroup (workgroup &optional name)
  "Return a copy of WORKGROUP, optionally named NAME."
  (workgroups-make-workgroup
   nil (or name (workgroups-access :name workgroup))
   (workgroups-access :config workgroup)))

(defun workgroups-make-default-workgroup (name &optional buffer)
  "Return a new default workgroup named NAME."
  (workgroups-make-workgroup
   nil name (workgroups-make-default-window-config buffer)))


;;; workgroup restoring

(defun workgroups-wsize (win &optional height)
  "Return the width or height of WIN."
  (if (workgroups-window-p win)
      (workgroups-access (if height 'height 'width) win)
    (destructuring-bind (x0 y0 x1 y1) (cadr win)
      (if height (- y1 y0) (- x1 x0)))))

(defun workgroups-restore-window (window winobj)
  "Restore WINDOW's state in `selected-window'."
  (workgroups-abind
    window (fname bname point mark markx wstart sbars)
    (destructuring-bind (w c v h) sbars
      (set-window-scroll-bars winobj w v h))
    (set-window-start winobj wstart t)
    (let ((buf (if (and fname (file-exists-p fname))
                   (find-file-noselect fname)
                 (or (get-buffer bname) workgroups-default-buffer))))
      (set-window-buffer winobj buf)
      (with-current-buffer buf
        (rename-buffer bname)
        (goto-char (if (eq point :max) (point-max) point))
        (set-mark mark)
        (unless markx (deactivate-mark))
        (when (>= wstart (point-max)) (recenter))))))

(defun workgroups-restore-window-tree (wtree frame)
  "Restore the window layout specified by WTREE."
  (let ((wobj (frame-selected-window frame)))
    (cond ((workgroups-window-p wtree)
           (workgroups-restore-window wtree wobj)
           (other-window 1 frame))
          (t (let ((v (car wtree)) (last (car (last wtree))))
               (dolist (win (cddr wtree))
                 (unless (eq win last)
                   (split-window wobj (workgroups-wsize win v) (not v)))
                 (workgroups-restore-window-tree win frame)))))))

(defun workgroups-restore-window-config (config)
  "Restore CONFIG."
  (workgroups-bind-frame frame
    (flet ((set-fparam (par val) (set-frame-parameter frame par val)))
      (workgroups-abind
        config (left top width height sbars sbwid swin mbswidx wtree)
        (when workgroups-restore-position
          (set-fparam 'left left)
          (set-fparam 'top  top))
        (when workgroups-restore-size
          (set-fparam 'width  width)
          (set-fparam 'height height))
        (set-fparam 'vertical-scroll-bars sbars)
        (set-fparam 'scroll-bar-width sbwid)
        (delete-other-windows (frame-selected-window frame))
        (workgroups-restore-window-tree wtree frame)
        (let ((wlst (workgroups-window-list frame)))
          (set-frame-selected-window frame (nth swin wlst))
          (and mbswidx (setq minibuffer-scroll-window (nth mbswidx wlst))))))))

(defun workgroups-restore-default-window-config ()
  "Restore the default config."
  (workgroups-restore-window-config
   (workgroups-make-default-window-config)))

(defun workgroups-restore-workgroup (workgroup &optional base)
  "Restore WORKGROUP."
  (workgroups-restore-window-config
   (or (and (not base) (workgroups-working-config workgroup t))
       (workgroups-base-config workgroup))))


;;; buffer list ops

(defun workgroups-config-buffer-list (config)
  "Return the names of all unique buffers in CONFIG."
  (let (bufs)
    (flet ((inner (obj)
                  (if (not (workgroups-window-p obj))
                      (mapc 'inner (cddr obj))
                    (workgroups-awhen (workgroups-access :buffer-name obj)
                      (or (member it bufs) (push it bufs))))))
      (inner config)
      bufs)))

(defun workgroups-workgroup-buffer-list (workgroup)
  "Return the names of all unique buffers in WORKGROUP."
  (let ((bufs (workgroups-config-buffer-list
               (workgroups-working-config workgroup))))
    (dolist (buf (workgroups-config-buffer-list
                  (workgroups-base-config workgroup)) bufs)
      (unless (member buf bufs) (push buf bufs)))))

(defun workgroups-buffer-list ()
  "Return the names of all unique buffers in `workgroups-list'."
  (let (bufs)
    (dolist (w (workgroups-list) (nreverse bufs))
      (dolist (buf (workgroups-workgroup-buffer-list w))
        (unless (member buf bufs) (push buf bufs))))))

(defun workgroups-find-buffer (buf)
  "Return the workgroup and config flag that contains BUF."
  (catch 'result
    (dolist (w (workgroups-list))
      (cond ((member buf (workgroups-config-buffer-list
                          (workgroups-working-config w)))
             (throw 'result (list w nil)))
            ((member buf (workgroups-config-buffer-list
                          (workgroups-base-config w)))
             (throw 'result (list w t)))))))


;;; mode-line

(defun workgroups-mode-line-string ()
  "Update the mode-line with current workgroup info."
  (let ((cur (workgroups-current t)))
    (concat (workgroups-facify 'div "(")
            (workgroups-facify
             'mode (format "%s" (position cur (workgroups-list t))))
            (workgroups-facify 'div ":")
            (workgroups-facify 'mode (workgroups-name cur))
            (workgroups-facify 'div ")"))))

(defun workgroups-mode-line-display-add ()
  "Turn on workgroups' mode-line display."
  (unless (assoc 'workgroups-mode-line-on mode-line-format)
    (set-default 'mode-line-format
                 (workgroups-linsert
                  `(workgroups-mode-line-on
                    (:eval (workgroups-mode-line-string)))
                  mode-line-format
                  (1+ (position 'mode-line-position mode-line-format))))))

(defun workgroups-mode-line-display-remove ()
  "Turn off workgroups' mode-line display."
  (workgroups-awhen (assoc 'workgroups-mode-line-on mode-line-format)
    (set-default 'mode-line-format (remove it mode-line-format))
    (force-mode-line-update)))


;;; minibuffer reading

(defun workgroups-completing-read (prompt choices &rest args)
  "Call `ido-completing-read' or `completing-read'."
  (apply (if (and (boundp 'ido-mode) ido-mode)
             'ido-completing-read
           'completing-read) prompt choices args))

(defun workgroups-read-workgroup (&optional noerror)
  "Read a workgroup with `workgroups-completing-read'."
  (workgroups-get
   :name (workgroups-completing-read "Workgroup: " (workgroups-names))
   noerror))

(defun workgroups-read-name (&optional prompt)
  "Read a non-empty name from the minibuffer."
  (let ((prompt (or prompt "Name: ")) (name nil)
        (warning (workgroups-facify 'msg "Name must be non-empty")))
    (while (and (setq name (read-from-minibuffer prompt))
                (equal name ""))
      (message warning)
      (sit-for workgroups-warning-timeout))
    name))

(defun workgroups-read-idx (low high &optional prompt)
  "Read and return a valid workgroup index."
  (let ((prompt (or prompt (format "[%d-%d]: " low high))) i
        (warning (workgroups-facify
                  'msg (format "Enter an integer [%d-%d]" low high))))
    (while (and (setq i (read-from-minibuffer prompt nil nil t))
                (or (not (integerp i)) (< i low) (> i high)))
      (message warning)
      (sit-for workgroups-warning-timeout))
    i))

(defun workgroups-read-buffer ()
  "Read and return a buffer from `workgroups-buffer-list'."
  (workgroups-completing-read "Buffer: " (workgroups-buffer-list)))


;;; command utils

(defun workgroups-current-updated (&optional noerror)
  "Return current workgroup after updating its working config."
  (workgroups-awhen (workgroups-current noerror)
    (workgroups-set-working-config it (workgroups-make-window-config))
    it))

(defun workgroups-arg (&optional reverse noerror)
  "Return a workgroup.  For use in interactive forms.
If `current-prefix-arg' is nil return the current workgroups.
Otherwise read a workgroup from the minibuffer.  If REVERSE is
non-nil, `current-prefix-arg''s begavior is reversed."
  (if (if reverse (not current-prefix-arg) current-prefix-arg)
      (workgroups-read-workgroup noerror)
    (workgroups-current-updated noerror)))

(defun workgroups-add-to-kill-ring (config)
  "Add CONFIG to `workgroups-kill-ring'."
  (push config workgroups-kill-ring)
  (setq workgroups-kill-ring
        (workgroups-take workgroups-kill-ring
                         workgroups-kill-ring-size)))

(defun workgroups-list-string ()
  "Return a string of the names of all workgroups."
  (let* ((cur   (workgroups-current  t))
         (prev  (workgroups-previous t))
         (div   (workgroups-facify 'div workgroups-divider-str))
         (lb    (workgroups-facify 'div "("))
         (rb    (workgroups-facify 'div ")"))
         (clb   workgroups-current-left-decoration)
         (crb   workgroups-current-right-decoration)
         (plb   workgroups-previous-left-decoration)
         (prb   workgroups-previous-right-decoration)
         (i     -1)
         (lst   (doconcat (w (workgroups-list t) (concat " " div " "))
                  (let ((str (format "%d: %s" (incf i) (workgroups-name w))))
                    (cond ((eq w cur)
                           (workgroups-facify
                            'cur (format "%s%s%s" clb str crb)))
                          ((eq w prev)
                           (workgroups-facify
                            'prev (format "%s%s%s" plb str prb)))
                          (t (workgroups-facify 'oth str)))))))
    (format "%s %s %s" lb lst rb)))

(defun workgroups-facified-name (workgroup)
  "Return WORKGROUP's name, with `workgroups-name-face' added."
  (workgroups-facify 'cur (workgroups-name workgroup)))

(defun workgroups-frame-wipe ()
  "Frame-wipe animation."
  (while (> (length (window-list)) 1)
    (ignore-errors
      (enlarge-window
       workgroups-frame-wipe-vertical-factor))
    (ignore-errors
      (enlarge-window-horizontally
       workgroups-frame-wipe-horizontal-factor))
    (sit-for workgroups-frame-wipe-speed)))


;;; commands

(defun workgroups-switch (workgroup &optional base)
  "Switch to WORKGROUP.
If BASE is non-nil, restore WORKGROUP's base config.  Otherwise
restore its working config."
  (interactive (list (workgroups-read-workgroup) current-prefix-arg))
  (let ((current (workgroups-current-updated t)))
    (when (eq workgroup current)
      (error "Already on %s" (workgroups-name workgroup)))
    (workgroups-set-previous current)
    (workgroups-set-current workgroup)
    (and workgroups-frame-wipe-on (workgroups-frame-wipe))
    (workgroups-restore-workgroup workgroup base)
    ;; (workgroups-mode-line-update)
    (run-hooks 'workgroups-switch-hook)
    (message "%s %s" (workgroups-facify 'cmd "Switched:")
             (workgroups-list-string))))

(defun workgroups-add-and-switch (workgroup)
  "Add WORKGROUP to `workgroups-list' and swtich to it."
  (let ((name (workgroups-name workgroup)))
    (when (workgroups-get :name name t)
      (unless (y-or-n-p (format "%S exists. Overwrite? " name))
        (error "Cancelled"))))
  (workgroups-add workgroup)
  (workgroups-switch workgroup))

(defun workgroups-create (name)
  "Create and add a workgroup named NAME."
  (interactive (list (workgroups-read-name)))
  (workgroups-add-and-switch (workgroups-make-default-workgroup name))
  (message "%s %s" (workgroups-facify 'cmd "Created:")
           (workgroups-list-string)))

(defun workgroups-clone (workgroup name)
  "Create and add a clone of WORKGROUP named NAME."
  (interactive (list (workgroups-arg) (workgroups-read-name)))
  (workgroups-add-and-switch (workgroups-copy-workgroup workgroup name))
  (message "%s %s" (workgroups-facify 'cmd "Cloned:")
           (workgroups-list-string)))

(defun workgroups-kill (workgroup)
  "Kill workgroup named NAME.
The working config of WORKGROUP is saved to
`workgroups-kill-ring'."
  (interactive (list (workgroups-arg)))
  (workgroups-add-to-kill-ring (workgroups-working-config workgroup))
  (let ((next (workgroups-cnext workgroup (workgroups-list))))
    (workgroups-delete workgroup)
    (if (eq next workgroup)
        (workgroups-restore-default-window-config)
      (workgroups-switch next))
    (message "%s %s %s" (workgroups-facify 'cmd "Killed:")
             (workgroups-facified-name workgroup)
             (workgroups-list-string))))

(defun workgroups-kill-ring-save (workgroup)
  "Save WORKGROUP's working config to `workgroups-kill-ring'."
  (interactive (list (workgroups-arg)))
  (workgroups-add-to-kill-ring (workgroups-working-config workgroup))
  (message "%s %s" (workgroups-facify 'cmd "Saved:")
           (workgroups-facify 'msg "working config")))

(defun workgroups-yank ()
  "Restore a config from `workgroups-kill-ring'.
Successive yanks restore successive configs, starting from the
beginning of `workgroups-kill-ring'."
  (interactive)
  (workgroups-aif workgroups-kill-ring
    (let ((pos (if (not (eq real-last-command 'workgroups-yank)) 0
                 (mod (1+ workgroups-kill-ring-pos) (length it)))))
      (setq workgroups-kill-ring-pos pos)
      (workgroups-restore-window-config (nth pos it))
      (message "%s %s" (workgroups-facify 'cmd "Yanked:")
               (workgroups-facify 'msg (format "%d" pos))))
    (error "Workgroups' kill-ring is empty")))

(defun workgroups-kill-workgroup-and-buffers (workgroup)
  "Kill WORKGROUP and the buffers in its working config."
  (interactive (list (workgroups-arg)))
  (let ((bufs (save-window-excursion
                (workgroups-restore-workgroup workgroup)
                (mapcar 'window-buffer (window-list)))))
    (workgroups-kill workgroup)
    (mapc 'kill-buffer bufs)
    (message "%s %s and its buffers %s"
             (workgroups-facify 'cmd "Killed:")
             (workgroups-facified-name workgroup)
             (workgroups-list-string))))

(defun workgroups-delete-other-workgroups (workgroup)
  "Delete all but the WORKGROUP."
  (interactive (list (workgroups-arg)))
  (let ((cur (workgroups-current)))
    (mapc 'workgroups-delete (remove workgroup (workgroups-list)))
    (unless (eq workgroup cur) (workgroups-switch workgroup))
    (message "%s All workgroups but %s  %s" (workgroups-facify 'cmd "Deleted:")
             (workgroups-facified-name workgroup)
             (workgroups-list-string))))

(defun workgroups-update (workgroup)
  "Set the base config of WORKGROUP to its current config."
  (interactive (list (workgroups-arg)))
  (workgroups-set-base-config
   workgroup (workgroups-working-config workgroup))
  (message "%s %s" (workgroups-facify 'cmd "Updated:")
           (workgroups-facified-name workgroup)))

(defun workgroups-revert (workgroup)
  "Set the working config of WORKGROUP to its base config."
  (interactive (list (workgroups-arg)))
  (workgroups-set-working-config
   workgroup (workgroups-base-config workgroup))
  (when (eq workgroup (workgroups-current))
    (workgroups-restore-workgroup workgroup t))
  (message "%s %s" (workgroups-facify 'cmd "Reverted:")
           (workgroups-facified-name workgroup)))

(defun workgroups-jump-prompt ()
  "Prompt string for `workgroups-jump'."
  (let* ((max (1- (length (workgroups-list))))
         (pr (format "%s\n\nEnter [0-%d]: " (workgroups-list-string) max)))
    (workgroups-read-idx 0 max pr)))

(defun workgroups-jump (n)
  "Switch to Nth workgroup in `workgroups-list'."
  (interactive (list (or current-prefix-arg (workgroups-jump-prompt))))
  (let ((wl (workgroups-list)))
    (workgroups-switch
     (or (nth n wl) (error "There are only %d workgroups" (length wl))))))

(defun workgroups-jump-0 () (interactive) (workgroups-jump 0))
(defun workgroups-jump-1 () (interactive) (workgroups-jump 1))
(defun workgroups-jump-2 () (interactive) (workgroups-jump 2))
(defun workgroups-jump-3 () (interactive) (workgroups-jump 3))
(defun workgroups-jump-4 () (interactive) (workgroups-jump 4))
(defun workgroups-jump-5 () (interactive) (workgroups-jump 5))
(defun workgroups-jump-6 () (interactive) (workgroups-jump 6))
(defun workgroups-jump-7 () (interactive) (workgroups-jump 7))
(defun workgroups-jump-8 () (interactive) (workgroups-jump 8))
(defun workgroups-jump-9 () (interactive) (workgroups-jump 9))

(defun workgroups-prev ()
  "Switch to the workgroup before WORKGROUP in `workgroups-list'."
  (interactive)
  (let ((wl (workgroups-list)) (cur (workgroups-current t)))
    (workgroups-switch
     (if (not cur) (car wl)
       (workgroups-cprev cur wl)))))

(defun workgroups-next ()
  "Switch to the workgroup after WORKGROUP in `workgroups-list'."
  (interactive)
  (let ((wl (workgroups-list)) (cur (workgroups-current t)))
    (workgroups-switch
     (if (not cur) (car (last wl))
       (workgroups-cnext cur wl)))))

(defun workgroups-prev-other-frame ()
  "Like `workgroups-prev', but operates on the next frame."
  (interactive)
  (let ((workgroups-frame (next-frame))
        (workgroups-restore-position))
    (workgroups-prev (workgroups-current))))

(defun workgroups-next-other-frame ()
  "Like `workgroups-next', but operates on the next frame."
  (interactive)
  (let ((workgroups-frame (next-frame))
        (workgroups-restore-position))
    (workgroups-next (workgroups-current))))

(defun workgroups-toggle ()
  "Switch to the previous workgroup."
  (interactive)
  (workgroups-switch (workgroups-previous)))

(defun workgroups-swap ()
  "Swap the current workgroup with the previous."
  (interactive)
  (workgroups-list-swap (workgroups-current) (workgroups-previous))
  (message "%s: %s" (workgroups-facify 'cmd "Swapped")
           (workgroups-list-string)))

(defun workgroups-transpose-left (workgroup)
  "Swap WORKGROUP toward the beginning of `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (workgroups-aif (workgroups-lprev workgroup (workgroups-list))
    (progn (workgroups-list-swap workgroup it)
           (message "%s %s" (workgroups-facify 'cmd "Transposed:")
                    (workgroups-list-string)))
    (error "Can't transpose %s any further."
           (workgroups-name workgroup))))

(defun workgroups-transpose-right (workgroup)
  "Move WORKGROUP toward the end of `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (workgroups-aif (workgroups-lnext workgroup (workgroups-list))
    (progn (workgroups-list-swap workgroup it)
           (message "%s %s" (workgroups-facify 'cmd "Transposed:")
                    (workgroups-list-string)))
    (error "Can't transpose %s any further."
           (workgroups-name workgroup))))

(defun workgroups-rename (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list (workgroups-arg) (workgroups-read-name "New name: ")))
  (let ((oldname (workgroups-facified-name workgroup)))
    (workgroups-set-name workgroup newname)
    (message "%s %s to %s" (workgroups-facify 'cmd "Renamed:") oldname
             (workgroups-facified-name workgroup))))


;;; file commands

(defun workgroups-save (file)
  "Save workgroups to FILE.
Called interactively with a prefix arg, or if `workgroups-file'
is nil, read a filename.  Otherwise use `workgroups-file'."
  (interactive
   (list (if (or current-prefix-arg (not (workgroups-file t)))
             (read-file-name "File: ") (workgroups-file))))
  (workgroups-save-sexp-to-file
   (cons workgroups-fid (workgroups-list)) file)
  (setq workgroups-dirty nil workgroups-file file)
  (message "%s %s" (workgroups-facify 'cmd "Wrote:")
           (workgroups-facify 'file file)))

(defun workgroups-load (file)
  "Load workgroups from FILE.
Called interactively with a prefix arg, and if `workgroups-file'
is non-nil, use `workgroups-file'. Otherwise read a filename."
  (interactive
   (list (if (and current-prefix-arg (workgroups-file t))
             (workgroups-file) (read-file-name "File: "))))
  (destructuring-bind (fid . workgroups)
      (workgroups-read-sexp-from-file file)
    (unless (eq fid workgroups-fid) (error "%S is not a workgroups file."))
    (clrhash workgroups-frame-table)
    (setq workgroups-list   workgroups
          workgroups-file   file
          workgroups-dirty  nil))
  (workgroups-awhen (workgroups-list t)
    (when workgroups-switch-on-load
      (workgroups-switch (car it))))
  (message "%s %s" (workgroups-facify 'cmd "Loaded:")
           (workgroups-facify 'file file)))

(defun workgroups-find-file (file)
  "Create a new workgroup and find file FILE in it."
  (interactive "FFile: ")
  (workgroups-create (file-name-nondirectory file))
  (find-file file))

(defun workgroups-find-file-read-only (file)
  "Create a new workgroup and find FILE read-only in it."
  (interactive "FFile: ")
  (workgroups-create (file-name-nondirectory file))
  (find-file-read-only file))

(defun workgroups-get-by-buffer (buf)
  "Switch to the workgroup that contains BUF."
  (interactive (list (workgroups-read-buffer)))
  (workgroups-aif (workgroups-find-buffer buf)
    (apply 'workgroups-switch it)
    (error "No workgroup contains %S" buf)))

(defun workgroups-dired (dir &optional switches)
  "Create a workgroup and open DIR in dired with SWITCHES."
  (interactive (list (read-directory-name "Dired: ") current-prefix-arg))
  (workgroups-create dir)
  (dired dir switches))


;;; toggle commands

(defun workgroups-toggle-mode-line ()
  "Toggle workgroups' mode-line display."
  (interactive)
  (setq workgroups-mode-line-on (not workgroups-mode-line-on))
  (force-mode-line-update)
  (message "%s %s" (workgroups-facify 'cmd "mode-line:")
           (workgroups-facify
            'msg (if workgroups-mode-line-on "on" "off"))))

(defun workgroups-toggle-frame-wipe ()
  "Toggle frame-wiping on `workgroups-switch'."
  (interactive)
  (setq workgroups-frame-wipe-on (not workgroups-frame-wipe-on))
  (message "%s %s" (workgroups-facify 'cmd "Frame wipe:")
           (workgroups-facify
            'msg (if workgroups-frame-wipe-on "on" "off"))))


;;; echo commands

(defun workgroups-echo-current ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (message "%s %s" (workgroups-facify 'cmd "Current:")
           (workgroups-facified-name (workgroups-current))))

(defun workgroups-echo-all ()
  "Display the names of all workgroups in the echo area."
  (interactive)
  (message "%s %s" (workgroups-facify 'cmd "Workgroups:")
           (workgroups-list-string)))

(defun workgroups-echo-time ()
  "Echo the current time."
  (interactive)
  (let ((op (workgroups-facify 'cmd "Current time:"))
        (time (workgroups-facify
               'msg (format-time-string workgroups-time-format))))
    (if (and workgroups-display-battery (fboundp 'battery))
        (message "%s %s\n%s %s" op time
                 (workgroups-facify 'cmd "Battery:")
                 (workgroups-facify 'msg (battery)))
      (message "%s %s" op time))))

(defun workgroups-echo-version ()
  "Echo the current version number."
  (interactive)
  (message "%s %s" (workgroups-facify 'cmd "Workgroups version:")
           (workgroups-facify 'msg workgroups-version)))


;;; misc commands

(defun workgroups-reset ()
  "Reset workgroups."
  (interactive)
  (if (not (y-or-n-p "Are you sure? "))
      (error "Canceled")
    (clrhash workgroups-frame-table)
    (setq workgroups-list nil
          workgroups-file nil
          workgroups-kill-ring nil
          workgroups-kill-ring-pos 0)
    (message "Workgroups reset")))



;;; keymap

(defun workgroups-unset-prefix-key ()
  "Restore the original definition of `workgroups-prefix-key'."
  (workgroups-awhen (member :original (symbol-plist 'workgroups-prefix-key))
    (global-set-key workgroups-prefix-key (cadr it))
    (setplist 'workgroups-prefix-key nil)))

(defun workgroups-set-prefix-key (key)
  "Set KEY to `workgroups-map' `global-map'.
Also save KEY's original definition, and set
`workgroups-prefix-key' to KEY."
  (workgroups-unset-prefix-key)
  (put 'workgroups-prefix-key :original (lookup-key global-map key))
  (global-set-key key workgroups-map)
  (setq workgroups-prefix-key key))

(defvar workgroups-map
  (workgroups-fill-keymap
   (make-sparse-keymap)
   "C-c"        'workgroups-create
   "c"          'workgroups-create
   "'"          'workgroups-switch
   "C-'"        'workgroups-switch
   "C-v"        'workgroups-switch
   "v"          'workgroups-switch
   "C"          'workgroups-clone
   "C-k"        'workgroups-kill
   "k"          'workgroups-kill
   "C-y"        'workgroups-yank
   "y"          'workgroups-yank
   "M-w"        'workgroups-kill-ring-save
   "M-k"        'workgroups-kill-workgroup-and-buffers
   "K"          'workgroups-delete-other-workgroups
   "C-r"        'workgroups-revert
   "r"          'workgroups-revert
   "C-u"        'workgroups-update
   "u"          'workgroups-update
   "C-s"        'workgroups-save
   "C-l"        'workgroups-load
   "A"          'workgroups-rename
   "C-p"        'workgroups-prev
   "p"          'workgroups-prev
   "C-n"        'workgroups-next
   "n"          'workgroups-next
   "M-p"        'workgroups-prev-other-frame ;; add help
   "M-n"        'workgroups-next-other-frame ;; add help
   "C-a"        'workgroups-toggle
   "a"          'workgroups-toggle
   "C-j"        'workgroups-jump
   "0"          'workgroups-jump-0
   "1"          'workgroups-jump-1
   "2"          'workgroups-jump-2
   "3"          'workgroups-jump-3
   "4"          'workgroups-jump-4
   "5"          'workgroups-jump-5
   "6"          'workgroups-jump-6
   "7"          'workgroups-jump-7
   "8"          'workgroups-jump-8
   "9"          'workgroups-jump-9
   "C-x"        'workgroups-swap
   "C-,"        'workgroups-transpose-left
   "C-."        'workgroups-transpose-right
   "C-e"        'workgroups-echo-all
   "e"          'workgroups-echo-all
   "S-C-e"      'workgroups-echo-current
   "E"          'workgroups-echo-current
   "C-t"        'workgroups-echo-time
   "t"          'workgroups-echo-time
   "V"          'workgroups-echo-version
   "C-b"        'workgroups-get-by-buffer
   "b"          'workgroups-get-by-buffer
   "C-f"        'workgroups-find-file
   "S-C-f"      'workgroups-find-file-read-only
   "d"          'workgroups-dired
   "C-i"        'workgroups-toggle-mode-line
   "C-w"        'workgroups-toggle-frame-wipe
   "C-M-k"      'workgroups-reset
   "?"          'workgroups-help)
  "Workgroups' keymap.")


;;; help

(defvar workgroups-help "workgroups keybindings:\n
  \\[workgroups-create]    Create a new workgroup and switch to it
  \\[workgroups-switch]    Switch to a workgroup
  \\[workgroups-clone]    Create a clone of the current workgroug and switch to it
  \\[workgroups-kill]    Kill a workgroup
  \\[workgroups-yank]    Set the working config to a config from the kill ring
  \\[workgroups-kill-ring-save]    Save the current config to the kill ring
  \\[workgroups-kill-workgroup-and-buffers]    Kill a workgroup and its buffer
  \\[workgroups-delete-other-workgroups]    Delete all but the specified workgroup
  \\[workgroups-revert]    Set a workgroup's working config to its base config
  \\[workgroups-update]    Set a workgroup's base config to its working config
  \\[workgroups-save]    Save workgroups to a file
  \\[workgroups-load]    Load workgroups from a file
  \\[workgroups-rename]    Rename a workgroup
  \\[workgroups-prev]    Cycle leftward in the workgroups list
  \\[workgroups-next]    Cycle rightward in the workgroups list
  \\[workgroups-transpose-left]    Transpose a workgroup leftward in the workgroups list
  \\[workgroups-transpose-right]    Transpose a workgroup rightward in the workgroups list
  \\[workgroups-swap]    Swap the current and previous workgroups' positions in the workgroups list
  \\[workgroups-toggle]    Switch to the previously selected workgroup
  \\[workgroups-jump]    Jump to a workgroup by number
  \\[workgroups-jump-0]    Switch to the workgroup at position 0 in the workgroups list
  \\[workgroups-jump-1]    Switch to the workgroup at position 1 in the workgroups list
  \\[workgroups-jump-2]    Switch to the workgroup at position 2 in the workgroups list
  \\[workgroups-jump-3]    Switch to the workgroup at position 3 in the workgroups list
  \\[workgroups-jump-4]    Switch to the workgroup at position 4 in the workgroups list
  \\[workgroups-jump-5]    Switch to the workgroup at position 5 in the workgroups list
  \\[workgroups-jump-6]    Switch to the workgroup at position 6 in the workgroups list
  \\[workgroups-jump-7]    Switch to the workgroup at position 7 in the workgroups list
  \\[workgroups-jump-8]    Switch to the workgroup at position 8 in the workgroups list
  \\[workgroups-jump-9]    Switch to the workgroup at position 9 in the workgroups list
  \\[workgroups-get-by-buffer]    Switch to the workgroup and config which contains the specified buffer
  \\[workgroups-find-file]    Create a new workgroup and find a file in it
  \\[workgroups-find-file-read-only]    Create a new workgroup and find-file-read-only in it
  \\[workgroups-dired]    Create a new workgroup and open a dired buffer in it
  \\[workgroups-toggle-mode-line]    Toggle workgroups mode-line display
  \\[workgroups-toggle-frame-wipe]    Toggle frame-wipe animation on workgroups switch
  \\[workgroups-echo-all]    Display the names of all workgroups in the echo area
  \\[workgroups-echo-current]    Display the name of the current workgroup in the echo area
  \\[workgroups-echo-time]    Display the current time in the echo area
  \\[workgroups-echo-version]    Display the version in the echo area
  \\[workgroups-help]    Show this help message"
  "Help shown by elscreen-help-mode")

(defun workgroups-help ()
  "Show information about workgroups commands."
  (interactive)
  (with-output-to-temp-buffer "*workroups help*"
    (princ (substitute-command-keys workgroups-help))
    (help-print-return-message)))


;;; mode definition

(defun workgroups-query-hook ()
  "Query for save on exit if `workgroups-dirty' is non-nil."
  (and workgroups-dirty
       workgroups-query-save-on-exit
       (y-or-n-p "Workgroups have been modified. Save them? ")
       (call-interactively 'workgroups-save))
  t)

(defun workgroups-enable (enable)
  "Enable `workgroups-mode' when ENABLE is non-nil.
Disable otherwise."
  (cond (enable
         (add-hook 'kill-emacs-query-functions 'workgroups-query-hook)
         (workgroups-set-prefix-key workgroups-prefix-key)
         (workgroups-mode-line-display-add)
         (setq workgroups-mode t))
        (t
         (remove-hook 'kill-emacs-query-functions 'workgroups-query-hook)
         (workgroups-unset-prefix-key)
         (workgroups-mode-line-display-remove)
         (setq workgroups-mode nil))))

(define-minor-mode workgroups-mode
  "This toggles workgroups-mode.
If ARG is null, toggle workgroups-mode.
If ARG is a number greater than zero, turn on workgroups-mode.
Otherwise, turn off workgroups-mode."
  :lighter     " wg"
  :init-value  nil
  :global      t
  :group       'workgroups
  (cond (noninteractive   (workgroups-enable nil))
        (workgroups-mode  (workgroups-enable t))
        (t                (workgroups-enable nil))))


;;; provide

(provide 'workgroups-mode)


;;; workgroups-mode.el ends here
