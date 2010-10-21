;;; workgroups-mode.el --- workgroups for windows

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:     workgroups-mode.el
;; Author:   tlh <thunkout@gmail.com>
;; Created:  2010-07-22
;; Version   0.1.9
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
;;   them to a file, load them from a file, and perform various other
;;   operations on them.
;;
;;  Here's what the Elisp info docs have to say about window
;;  configurations (info "(Elisp)Window Configurations"):
;;
;;      "A 'window configuration' records the entire layout of one
;;       frame--all windows, their sizes, which buffers they contain,
;;       how those buffers are scrolled, and their values of point and
;;       the mark; also their fringes, margins, and scroll bar
;;       settings.  It also includes the value of
;;       `minibuffer-scroll-window'.  As a special exception, the
;;       window configuration does not record the value of point in
;;       the selected window for the current buffer.  Also, the window
;;       configuration does not record the values of window
;;       parameters; see *Note Window Parameters::."
;;
;;  The problem with Emacs' window-configuration objects is that
;;  they're opaque, meaning you can't peer inside them.  To get at the
;;  information in a window-configuration, you must restore it in a
;;  frame, then access that frame's parameters.
;;
;;  Workgroups solves this problem by implementing its own complete
;;  window-configuration system in parallel to Emacs' -- one that is
;;  fully translucent and serializable.  Workgroups window
;;  configurations contain all the settings listed above, and in some
;;  ways more.
;;
;;  Workgroups tracks the window configuration lazily: it doesn't
;;  update the current workgroup for when changes are made to the
;;  frame.  It only updates the workgroup when the current config is
;;  accesses, or when it's specifically requested.
;;
;;  workgroups is so thorough, that if you transient-mark-mode
;;  selections will still be highlighted after restarting Emacs.
;;
;;  If you save workgroups that include things like erc or gnus
;;  buffers, you should launch those applications and buffers again in
;;  your next session before restoring the workgroup that includes
;;  them. Nothing bad will happen otherwise, of course.
;;  workgroups-mode will just default to a buffer that already exists,
;;  like *scratch*.
;;
;;  `workgroups-list' contains all the currently available workgroups.
;;  You can switch to workgroups (i.e. restore window configurations),
;;  bury them, go to the previous or next workgroup circularly, etc.
;;  `workgroups-save' saves `workgroups-list' to a file, which can
;;  then be loaded in another session.  Workgroups are added to
;;  `workgroups-list' by calling `workgroups-create', removed by
;;  calling `workgroups-kill', and can be moved to the end of
;;  `workgroups-list' by calling `workgroups-bury'.  In general,
;;  operations on workgroups and `workgroups-list' behave as similarly
;;  to buffers and buffer-lists as possible.
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
;;  - split and join
;;  - fix set-prefix-key
;;  - undo/redo
;;  - add minibuffer persistence
;;


;;; Code:

(require 'cl)


;;; consts

(defconst wg-version "0.1.9"
  "Current version number of workgroups.")

(defconst wg-file-id '-*-workgroups-*-
  "Symbol identifying a wg file.")


;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- Emacs session manager"
  :group 'convenience
  :version wg-version)

(defcustom wg-prefix-key (kbd "C-z")
  "Workgroups' prefix key."
  :type 'string
  :group 'workgroups
  :set (lambda (sym val)
         (when (boundp 'wg-prefix-key)
           (wg-set-prefix-key val)
           (custom-set-default sym val))))

(defcustom wg-switch-hook nil
  "Hook run whenever a workgroup is switched to."
  :type 'hook
  :group 'workgroups)

(defcustom wg-restore-position nil
  "Non-nil means restore the frame's position when switching to a
workgroup."
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
  "Non-nil means query for save before exiting when there are
unsaved changes."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-query-for-save-on-mode-exit t
  "Non-nil means query for save before exiting when there are
unsaved changes."
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

(defcustom wg-frame-wipe-on t
  "Non-nil means use the frame wipe animation when switching to a
workgroup."
  :type 'boolean
  :group 'workgroups)

(defcustom wg-frame-wipe-hfactor 25
  "Windows are enlarged horizontally in increments of this
numbers of columns during frame wiping."
  :type 'integer
  :group 'workgroups)

(defcustom wg-frame-wipe-vfactor 8
  "Windows are enlarged vertically in increments of this numbers
of rows during frame wiping."
  :type 'integer
  :group 'workgroups)

(defcustom wg-frame-wipe-speed 0.001
  "Number of seconds to sit between window enlargement calls
during frame wipe."
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

(defcustom wg-disp-left-brace "( "
  "String on the left of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-disp-right-brace " )"
  "String on the right of the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-disp-divider " | "
  "String between workgroups names in the list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-disp-current-left-decor "-<{ "
  "String displayed to the left of the current workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-disp-current-right-decor " }>-"
  "String displayed to the right of the current workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-disp-previous-left-decor "*"
  "String displayed to the left of the previous workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom wg-disp-previous-right-decor "*"
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

(defmacro wg-dohash (bindings &rest body)
  "do-style wrapper for maphash."
  (declare (indent 1))
  (destructuring-bind (key val table &optional ret) bindings
    `(progn (maphash (lambda (,key ,val) ,@body) ,table)
            ,ret)))

(defmacro wg-doconcat (binds &rest body)
  "do-style wrapper for mapconcat."
  (declare (indent 1))
  (destructuring-bind (elt seq &optional sep) binds
    `(mapconcat (lambda (,elt) ,@body) ,seq (or ,sep ""))))

(defmacro wg-aif (test then &rest else)
  "Anaphoric if."
  (declare (indent 2))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro wg-awhen (test &rest body)
  "Anaphoric when."
  (declare (indent 1))
  `(wg-aif ,test (progn ,@body)))

(defun wg-take (list n)
  "Return a list of the first N elts in LIST."
  (butlast list (- (length list) n)))

(defun wg-partition (list n)
  "Return list of contiguous N-length sublists of LIST.
Length of last sublist may be less than N.  Iterative to prevent
stack overflow."
  (when (< n 1) (error "N must be greater than zero."))
  (let (acc)
    (while list
      (push (wg-take list n) acc)
      (setq list (nthcdr n list)))
    (nreverse acc)))

(defun wg-insert-elt (elt list &optional pos)
  "Insert ELT into LIST at POS or the end."
  (let ((pos (or pos (length list))))
    (append (wg-take list pos) (cons elt (nthcdr pos list)))))

(defun wg-move-elt (elt list pos)
  "Move ELT to position POS in LIST."
  (wg-insert-elt elt (remove elt list) pos))

(defun wg-offset-elt (elt list offset)
  "Offset ELT's position in LIST by OFFSET."
  (wg-move-elt
   elt list (mod (+ offset (position elt list)) (length list))))

(defun wg-nth-from (elt list n &optional nocycle)
  "Return the elt in LIST N places from ELT.
If ELT is not present is LIST, return nil.  NOCYCLE nil means
return elements from LIST cyclically.  NOCYCLE t means return nil
if the requested index is outside the boundaries of LIST."
  (wg-awhen (position elt list)
    (let ((pos (+ it n)) (len (length list)))
      (if (not nocycle)
          (nth (mod pos len) list)
        (unless (or (< pos 0) (>= pos len))
          (nth pos list))))))

(defun wg-util-swap (elt1 elt2 list)
  "Return a copy of LISTwith ELT1 with ELT2 swapped.
Return nil when ELT1 and ELT2 aren't both present."
  (let ((pos1 (position elt1 list))
        (pos2 (position elt2 list)))
    (when (and pos1 pos2)
      (wg-move-elt elt1 (wg-move-elt elt2 list pos1) pos2))))

(defun wg-vassq (key alist)
  "Return cdr of associng KEY in ALIST."
  (cdr (assq key alist)))

(defun wg-alist-set (alist key val)
  "If KEY exists in ALIST, set its val to VAL and return VAL.
Otherwise do nothing and return nil."
  (wg-awhen (assq key alist) (setcdr it val)))

(defun wg-get-alist (key val alist-list)
  "Return the first alist in ALIST-LIST containing KEY and VAL."
  (catch 'res
    (dolist (alist alist-list)
      (when (equal val (cdr (assoc key alist)))
        (throw 'res alist)))))

(defmacro wg-bind-alist (alist keylist &rest body)
  "Bind keys in KEYLIST to their values in ALIST, then eval BODY.
Requires that all keys in KEYLIST are bindable symbols."
  (declare (indent 2))
  (let ((asym (gensym)))
    `(let* ((,asym ,alist)
            ,@(mapcar (lambda (key) `(,key (cdr (assq ',key ,asym))))
                      keylist))
       ,@body)))

(defun wg-fill-keymap (keymap &rest binds)
  "Fill KEYMAP with BINDS."
  (dolist (bind (wg-partition binds 2) keymap)
    (define-key keymap (read-kbd-macro (car bind)) (cadr bind))))

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

(defun wg-window-list (&optional frame)
  "Flatten `window-tree' into a stable list.
`window-list' can't be used because its order isn't stable."
  (flet ((inner (obj) (if (atom obj) (list obj)
                        (mapcan 'inner (cddr obj)))))
    (inner (car (window-tree (or frame (selected-frame)))))))

(defun wg-frame-offset (&optional n frame)
  "Return the frame N frames away from FRAME cyclically.
N defaults to 1, and FRAME defaults to `selected-frame'."
  (let ((fl (frame-list)))
    (nth (mod (+ (position (or frame (selected-frame)) fl)
                 (or n 1))
              (length fl))
         fl)))

(defun wg-add-face (facekey str)
  "Return a copy of STR fontified according to FACEKEY.
FACEKEY must be a key in `wg-face-abbrevs'."
  (let ((face (wg-vassq facekey wg-face-abbrevs))
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


;;; frame ops

(defmacro wg-with-frame-state (frame state &rest body)
  "Bind FRAME and STATE in BODY.
FRAME is bound to `wg-frame', and STATE is bound to
FRAME's value in `wg-frame-table'."
  (declare (indent 2))
  `(let* ((,frame (selected-frame))
          (,state (gethash ,frame wg-frame-table)))
     ,@body))

(defun wg-frame-val (key)
  "Return KEY's value in `wg-frame-table'."
  (wg-with-frame-state frame state
    (wg-vassq key state)))

(defun wg-set-frame-val (key val)
  "Set KEY to VAL in `wg-frame-table'."
  (wg-with-frame-state frame state
    (or (wg-alist-set state key val)
        (push (cons key val)
              (gethash frame wg-frame-table)))))

(defun wg-delete-frame-key (key)
  "Remove KEY from frame's entry in `wg-frame-table'."
  (wg-with-frame-state frame state
    (puthash frame (remove (assq key state) state) wg-frame-table)))


;;; window config making

(defun wg-window-p (window)
  "Return t if WINDOW is a workgroups window, nil otherwise."
  (and (consp window) (assq 'width window)))

(defun wg-window-size (win &optional width)
  "Return the width or height of WIN."
  (if (wg-window-p win)
      (wg-vassq (if width 'width 'height) win)
    (destructuring-bind (x0 y0 x1 y1) (cadr win)
      (if width (- x1 x0) (- y1 y0)))))

(defun wg-make-window (winobj)
  "Return a workgroups window from WINOBJ."
  (with-current-buffer (window-buffer winobj)
    (let ((p (point)) (edges (window-edges winobj)))
      `((width   .  ,(- (nth 2 edges) (nth 0 edges)))
        (height  .  ,(window-height winobj))
        (bname   .  ,(buffer-name))
        (fname   .  ,(buffer-file-name))
        (point   .  ,(if (= p (point-max)) :max p))
        (mark    .  ,(mark))
        (markx   .  ,mark-active)
        (wstart  .  ,(window-start winobj))
        (sbars   .  ,(window-scroll-bars winobj))
        (margins .  ,(window-margins winobj))
        (fringes .  ,(window-fringes winobj))))))

(defun wg-make-window-tree (wtree)
  "Return a workgroups window tree from WTREE."
  (etypecase wtree
    (window  (wg-make-window wtree))
    (cons   `(,@(wg-take wtree 2)
              ,@(mapcar 'wg-make-window-tree (cddr wtree))))))

(defun wg-make-window-config ()
  "Return a workgroups window config."
  (let ((wl (wg-window-list)))
    (message nil)
    `((left    . ,(frame-parameter nil 'left))
      (top     . ,(frame-parameter nil 'top))
      (width   . ,(frame-parameter nil 'width))
      (height  . ,(frame-parameter nil 'height))
      (sbars   . ,(frame-parameter nil 'vertical-scroll-bars))
      (sbwid   . ,(frame-parameter nil 'scroll-bar-width))
      (swidx   . ,(position (selected-window) wl))
      (mbswidx . ,(position minibuffer-scroll-window wl))
      (wtree   . ,(wg-make-window-tree (car (window-tree)))))))

(defun wg-make-default-window-config (&optional buffer)
  "Return a new default config.
The config displays BUFFER or `wg-default-buffer'."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer wg-default-buffer))
    (wg-make-window-config)))


;;; window config restoring

(defun wg-restore-window (window)
  "Restore WINDOW's state in `selected-window'."
  (wg-bind-alist window
      (fname bname point mark markx wstart sbars fringes margins)
    (switch-to-buffer
     (or (and fname (file-exists-p fname) (find-file-noselect fname))
         (get-buffer bname)
         wg-default-buffer))
    (unless (equal (buffer-name) wg-default-buffer)
      (rename-buffer bname))
    (destructuring-bind (w c v h) sbars
      (set-window-scroll-bars nil w v h))
    (destructuring-bind (lwid rwid outside) fringes
      (set-window-fringes nil lwid rwid outside))
    (destructuring-bind (left . right) margins
      (set-window-margins nil left right))
    (set-window-start nil wstart t)
    (goto-char (if (eq point :max) (point-max) point))
    (set-mark mark)
    (unless markx (deactivate-mark))
    (when (>= wstart (point-max)) (recenter))))

(defun wg-restore-window-tree (wtree)
  "Restore WTREE's window layout in FRAME."
  (cond ((wg-window-p wtree)
         (wg-restore-window wtree)
         (other-window 1))
        (t (let ((hor (not (car wtree)))
                 (lastwin (car (last wtree))))
             (dolist (win (cddr wtree))
               (unless (eq win lastwin)
                 (split-window nil (wg-window-size win hor) hor))
               (wg-restore-window-tree win))))))

(defun wg-restore-window-config (config)
  "Restore CONFIG."
  (wg-bind-alist config
      (left top width height sbars sbwid swidx mbswidx wtree)
    (when wg-restore-position
      (set-frame-parameter nil 'left left)
      (set-frame-parameter nil 'top  top))
    (set-frame-parameter nil 'width  width)
    (set-frame-parameter nil 'height height)
    (set-frame-parameter nil 'vertical-scroll-bars sbars)
    (set-frame-parameter nil 'scroll-bar-width sbwid)
    (delete-other-windows)
    (wg-restore-window-tree wtree)
    (let ((wl (wg-window-list)))
      (set-frame-selected-window nil (nth swidx wl))
      (when mbswidx
        (setq minibuffer-scroll-window (nth mbswidx wl))))))

(defun wg-restore-default-window-config ()
  "Restore the default config."
  (wg-restore-window-config (wg-make-default-window-config)))


;;; global accessors

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

(defun wg-workgroup-p (obj)
  "Return t if OBJ is a workgroup, nil otherwise."
  (and (consp obj) (wg-vassq 'wg obj)))

(defun wg-workgroup-type-check (obj)
  "Throw an error if OBJ is not a workgroup."
  (or (wg-workgroup-p obj)
      (error "%s is not a workgroup" obj)))

(defun wg-get-workgroup-prop (workgroup prop)
  "Return PROP's value in WORKGROUP."
  (wg-workgroup-type-check workgroup)
  (wg-vassq prop workgroup))

(defun wg-set-workgroup-prop (workgroup prop val &optional nodirty)
  "Set PROP to VAL in WORKGROUP."
  (wg-workgroup-type-check workgroup)
  (wg-alist-set workgroup prop val)
  (unless nodirty (setq wg-dirty t)))

(defun wg-uid (workgroup)
  "Return WORKGROUP's uid."
  (wg-get-workgroup-prop workgroup 'uid))

(defun wg-set-uid (workgroup uid)
  "Set the uid of WORKGROUP to UID."
  (wg-set-workgroup-prop workgroup 'uid uid))

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
  (wg-get-workgroup-prop workgroup 'name))

(defun wg-set-name (workgroup name)
  "Set the name of WORKGROUP to NAME."
  (wg-set-workgroup-prop workgroup 'name name))

(defun wg-names (&optional noerror)
  "Return a list of workgroup names."
  (mapcar 'wg-name (wg-list noerror)))


;;; current and previous workgroup ops

(defun wg-current (&optional noerror)
  "Return the current workgroup."
  (wg-aif (wg-frame-val :current) (wg-get-workgroup 'uid it)
    (unless noerror
      (error "There's no current workgroup in the frame"))))

(defun wg-set-current (workgroup)
  "Set the current workgroup to WORKGROUP."
  (wg-set-frame-val :current (wg-uid workgroup)))

(defun wg-previous (&optional noerror)
  "Return the previous workgroup."
  (wg-aif (wg-frame-val :previous) (wg-get-workgroup 'uid it)
    (unless noerror
      (error "There's no previous workgroup in the frame"))))

(defun wg-set-previous (workgroup)
  "Set the previous workgroup to WORKGROUP."
  (wg-set-frame-val :previous (wg-uid workgroup)))


;;; base and working configs

(defun wg-base-config (workgroup)
  "Return the base config of WORKGROUP."
  (wg-workgroup-type-check workgroup)
  (wg-vassq 'config workgroup))

(defun wg-set-base-config (workgroup config)
  "Set the base config of WORKGROUP to CONFIG."
  (wg-workgroup-type-check workgroup)
  (wg-alist-set workgroup 'config config)
  (setq wg-dirty t))

(defun wg-set-working-config (workgroup config)
  "Set the working config of WORKGROUP to CONFIG."
  (wg-set-frame-val (wg-uid workgroup) config))

(defun wg-update-working-config (workgroup)
  "Set WORKGROUP's working config to the current window config."
  (wg-set-working-config
   workgroup (wg-make-window-config)))

(defun wg-working-config (workgroup)
  "Return the working config of WORKGROUP.
If WORKGROUP is the current workgroup, first update it with
`wg-set-working-config'."
  (when (eq workgroup (wg-current t))
    (wg-update-working-config workgroup))
  (or (wg-frame-val (wg-uid workgroup))
      (wg-base-config workgroup)))


;;; workgroup making and restoring

(defun wg-make-workgroup (uid name config)
  "Make a workgroup named NAME from BASE and WORKING."
  `((wg     . t)
    (uid    . ,uid)
    (name   . ,name)
    (config . ,config)))

(defun wg-make-default-workgroup (name &optional buffer)
  "Return a new default workgroup named NAME."
  (wg-make-workgroup
   nil name (wg-make-default-window-config buffer)))

(defun wg-restore-workgroup (workgroup &optional base)
  "Restore WORKGROUP."
  (wg-restore-window-config
   (if base (wg-base-config workgroup)
     (wg-working-config workgroup))))


;;; workgroups list ops

(defun wg-delete (workgroup)
  "Remove WORKGROUP from `wg-list'."
  (wg-dohash (frame state wg-frame-table)
    (with-selected-frame frame
      (wg-delete-frame-key (wg-uid workgroup))
      (when (eq workgroup (wg-current t))
        (wg-set-current nil))
      (when (eq workgroup (wg-previous t))
        (wg-set-previous nil))))
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
      (unless (y-or-n-p (format "%S exists. Overwrite? " name))
        (error "Cancelled"))))
  (wg-add workgroup))

(defun wg-offset-workgroup (workgroup offset)
  "Offset WORKGROUP's position in `wg-list' by OFFSET."
  (setq wg-list (wg-offset-elt workgroup (wg-list) offset)
        wg-dirty t))

(defun wg-list-swap (w1 w2)
  "Swap W1 and W2 in `wg-list'."
  (when (eq w1 w2) (error "Can't swap a workgroup with itself"))
  (wg-aif (wg-util-swap w1 w2 (wg-list))
      (setq wg-list it wg-dirty t)
    (error "Both workgroups aren't present.")))


;;; buffer list ops

(defun wg-config-buffer-list (wconfig)
  "Return the names of all unique buffers in WTREE."
  (let (bufs)
    (flet ((inner (win)
                  (if (wg-window-p win)
                      (pushnew (wg-vassq 'bname win) bufs :test 'equal)
                    (mapc 'inner (cddr win)))))
      (inner (wg-vassq 'wtree wconfig))
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
  "Update the mode-line with current workgroup info."
  (let ((cur (wg-current t)))
    (wg-fontify
     (:div "(") (:mode (position cur (wg-list t)))
     (:div ":") (:mode (wg-name cur)) (:div ")"))))

(defun wg-mode-line-display-add ()
  "Turn on workgroups' mode-line display."
  (unless (assq 'wg-mode-line-on mode-line-format)
    (let ((format `(wg-mode-line-on (:eval (wg-mode-line-string))))
          (pos (1+ (position 'mode-line-position mode-line-format))))
      (set-default 'mode-line-format
                   (wg-insert-elt format mode-line-format pos)))))

(defun wg-mode-line-display-remove ()
  "Turn off workgroups' mode-line display."
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
  (wg-completing-read "Buffer: " (wg-buffer-list)))


;;; messaging

(defvar wg-last-message nil
  "The last string workgroups sent to the echo area.")

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
  (if (if reverse (not current-prefix-arg) current-prefix-arg)
      (wg-read-workgroup noerror)
    (wg-current noerror)))

(defun wg-add-to-kill-ring (config)
  "Add CONFIG to `wg-kill-ring'."
  (push config wg-kill-ring)
  (setq wg-kill-ring (wg-take wg-kill-ring wg-kill-ring-size)))

(defun wg-disp ()
  "Return a string of the names of all workgroups."
  (let ((cur   (wg-current  t))
        (prev  (wg-previous t))
        (div   (wg-add-face :div wg-disp-divider))
        (clb   wg-disp-current-left-decor)
        (crb   wg-disp-current-right-decor)
        (plb   wg-disp-previous-left-decor)
        (prb   wg-disp-previous-right-decor)
        (i     -1))
    (wg-fontify
     (:brace wg-disp-left-brace)
     (wg-doconcat (w (wg-list t) div)
       (let ((str (format "%d: %s" (incf i) (wg-name w))))
         (cond ((eq w cur)
                (wg-fontify (:cur (concat clb str crb))))
               ((eq w prev)
                (wg-fontify (:prev (concat plb str prb))))
               (t (wg-fontify (:other str))))))
     (:brace wg-disp-right-brace))))

(defun wg-nth-from-workgroup (&optional workgroup n nocycle)
  "Return the workgroup N places from WORKGROUP."
  (wg-nth-from (or workgroup (wg-current t)) (wg-list) (or n 1) nocycle))

(defun wg-frame-wipe ()
  "Frame-wipe animation."
  (while (> (length (window-list (selected-frame))) 1)
    (ignore-errors
      (enlarge-window wg-frame-wipe-vfactor))
    (ignore-errors
      (enlarge-window wg-frame-wipe-hfactor t))
    (sit-for wg-frame-wipe-speed)))


;;; commands

;; (defun wg-switch (workgroup &optional base)
;;   "Switch to WORKGROUP.
;; BASE nil means restore WORKGROUP's working config.
;; BASE non-nil means restore WORKGROUP's base config."
;;   (interactive (list (wg-read-workgroup) current-prefix-arg))
;;   (let ((current (wg-current t)))
;;     (if (eq workgroup current)
;;         (wg-fontified-msg (:cmd "Already on: ") (:cur (wg-name workgroup)))
;;       (when current (wg-update-working-config current))
;;       (when wg-frame-wipe-on (wg-frame-wipe))
;;       (wg-restore-workgroup workgroup base)
;;       (wg-set-previous current)
;;       (wg-set-current workgroup)
;;       (run-hooks 'wg-switch-hook)
;;       (wg-fontified-msg (:cmd "Switched:  ") (wg-disp)))))

(defun wg-switch (workgroup &optional base)
  "Switch to WORKGROUP.
BASE nil means restore WORKGROUP's working config.
BASE non-nil means restore WORKGROUP's base config."
  (interactive (list (wg-read-workgroup) current-prefix-arg))
  (wg-awhen (wg-current t)
    (when (eq it workgroup) (error "Already on: " (wg-name it)))
    (wg-update-working-config it)
    (wg-set-previous it))
  (when wg-frame-wipe-on (wg-frame-wipe))
  (wg-restore-workgroup workgroup base)
  (wg-set-current workgroup)
  (run-hooks 'wg-switch-hook)
  (wg-fontified-msg (:cmd "Switched:  ") (wg-disp)))

;; asdf

(defun wg-create (name)
  "Create and add a workgroup named NAME."
  (interactive (list (wg-read-name)))
  (let ((workgroup (wg-make-default-workgroup name)))
    (wg-check-and-add workgroup)
    (wg-switch workgroup)
    (wg-fontified-msg
      (:cmd "Created: ") (:cur name) "  " (wg-disp))))

(defun wg-clone (workgroup name)
  "Create and add a clone of WORKGROUP named NAME."
  (interactive (list (wg-arg) (wg-read-name)))
  (let ((new (wg-make-workgroup nil name (wg-base-config workgroup))))
    (wg-check-and-add new)
    (wg-set-working-config new (wg-working-config workgroup))
    (wg-switch new)
    (wg-fontified-msg
      (:cmd "Cloned: ") (:cur (wg-name workgroup))
      (:msg " to ") (:cur name) "  " (wg-disp))))

(defun wg-kill (workgroup)
  "Kill WORKGROUP, saving its working config to the kill ring."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-working-config workgroup))
  (let ((to (or (wg-previous t) (wg-nth-from-workgroup workgroup))))
    (wg-delete workgroup)
    (if (eq to workgroup) (wg-restore-default-window-config)
      (wg-switch to))
    (wg-fontified-msg
      (:cmd "Killed: ") (:cur (wg-name workgroup)) "  " (wg-disp))))

(defun wg-kill-ring-save (workgroup)
  "Save WORKGROUP's working config to `wg-kill-ring'."
  (interactive (list (wg-arg)))
  (wg-add-to-kill-ring (wg-working-config workgroup))
  (wg-fontified-msg
    (:cmd "Saved: ") (:cur (wg-name workgroup))
    (:cur "'s ") (:msg "working config")))

(defun wg-yank ()
  "Restore a config from `wg-kill-ring'.
Successive yanks restore successive configs, starting from the
beginning of `wg-kill-ring'."
  (interactive)
  (unless wg-kill-ring (error "The kill-ring is empty"))
  (let ((pos (if (not (eq real-last-command 'wg-yank)) 0
               (mod (1+ (or (get 'wg-yank :position) 0))
                    (length wg-kill-ring)))))
    (put 'wg-yank :position pos)
    (wg-restore-window-config (nth pos wg-kill-ring))
    (wg-fontified-msg (:cmd "Yanked: ") (:msg pos) "\n" (wg-disp))))

(defun wg-kill-workgroup-and-buffers (workgroup)
  "Kill WORKGROUP and the buffers in its working config."
  (interactive (list (wg-arg)))
  (let ((bufs (save-window-excursion
                (wg-restore-workgroup workgroup)
                (mapcar 'window-buffer (window-list)))))
    (wg-kill workgroup)
    (mapc 'kill-buffer bufs)
    (wg-fontified-msg
      (:cmd "Killed: ") (:cur (wg-name workgroup))
      (:msg " and its buffers ") "\n" (wg-disp))))

(defun wg-delete-other-workgroups (workgroup)
  "Delete all but the WORKGROUP."
  (interactive (list (wg-arg)))
  (let ((cur (wg-current)))
    (mapc 'wg-delete (remove workgroup (wg-list)))
    (unless (eq workgroup cur) (wg-switch workgroup))
    (wg-fontified-msg
      (:cmd "Deleted: ") (:msg "All wg but ")
      (:cur (wg-name workgroup)))))

(defun wg-update (workgroup)
  "Set the base config of WORKGROUP to its current config."
  (interactive (list (wg-arg)))
  (wg-set-base-config
   workgroup (wg-working-config workgroup))
  (wg-fontified-msg
    (:cmd "Updated: ") (:cur (wg-name workgroup))))

(defun wg-update-all ()
  "Update all workgroups' base configs.
Worgroups are updated with their working configs in the
`selected-frame'."
  (interactive)
  (mapc 'wg-update (wg-list))
  (wg-fontified-msg (:cmd "Updated: ") (:msg "All")))

(defun wg-revert (workgroup)
  "Set the working config of WORKGROUP to its base config."
  (interactive (list (wg-arg)))
  (wg-set-working-config
   workgroup (wg-base-config workgroup))
  (when (eq workgroup (wg-current))
    (wg-restore-workgroup workgroup t))
  (wg-fontified-msg
    (:cmd "Reverted: ") (:cur (wg-name workgroup))))

(defun wg-revert-all ()
  "Revert all workgroups to their base configs."
  (interactive)
  (mapc 'wg-revert (wg-list))
  (wg-fontified-msg (:cmd "Reverted: ") (:msg "All")))

(defun wg-jump-prompt ()
  "Prompt string for `wg-jump'."
  (let* ((max (1- (length (wg-list))))
         (pr (format "%s\n\nEnter [0-%d]: " (wg-disp) max)))
    (wg-read-idx 0 max pr)))

(defun wg-jump (n)
  "Switch to Nth workgroup in `wg-list'."
  (interactive (list (or current-prefix-arg (wg-jump-prompt))))
  (let ((wl (wg-list)))
    (wg-switch
     (or (nth n wl) (error "There are only %d workgroups" (length wl))))))

(defun wg-jump-0 () (interactive) (wg-jump 0))
(defun wg-jump-1 () (interactive) (wg-jump 1))
(defun wg-jump-2 () (interactive) (wg-jump 2))
(defun wg-jump-3 () (interactive) (wg-jump 3))
(defun wg-jump-4 () (interactive) (wg-jump 4))
(defun wg-jump-5 () (interactive) (wg-jump 5))
(defun wg-jump-6 () (interactive) (wg-jump 6))
(defun wg-jump-7 () (interactive) (wg-jump 7))
(defun wg-jump-8 () (interactive) (wg-jump 8))
(defun wg-jump-9 () (interactive) (wg-jump 9))

(defun wg-prev (&optional workgroup n)
  "Switch to the workgroup before WORKGROUP in `wg-list'."
  (interactive (list (wg-arg) current-prefix-arg))
  (wg-switch (or (wg-nth-from-workgroup
                  (or workgroup (wg-current)) (or n -1))
                 (car (wg-list)))))

(defun wg-next ()
  "Switch to the workgroup after WORKGROUP in `wg-list'."
  (interactive)
  (wg-switch (or (wg-nth-from-workgroup) (car (wg-list)))))

;; FIXME: args

(defun wg-prev-workgroup-next-frame (&optional n)
  "Like `wg-prev', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-frame-offset (or n 1))
    (wg-prev)))

(defun wg-next-workgroup-next-frame (&optional n)
  "Like `wg-next', but operates on the next frame."
  (interactive "p")
  (with-selected-frame (wg-frame-offset (or n -1))
    (wg-next)))

(defun wg-switch-to-previous ()
  "Switch to the previous workgroup."
  (interactive)
  (wg-switch (wg-previous)))

(defun wg-swap ()
  "Swap the current workgroup with the previous."
  (interactive)
  (wg-list-swap (wg-current) (wg-previous))
  (wg-fontified-msg (:cmd "Swapped ") (wg-disp)))

;; asdf

(defun wg-offset-left (workgroup &optional n)
  "Swap WORKGROUP toward the beginning of `wg-list'."
  (interactive (list (wg-arg) current-prefix-arg))
  (wg-offset-workgroup workgroup (or n -1))
  (wg-fontified-msg (:cmd "Offset left: ") (wg-disp)))

(defun wg-offset-right (workgroup &optional n)
  "Move WORKGROUP toward the end of `wg-list'."
  (interactive (list (wg-arg) current-prefix-arg))
  (wg-offset-workgroup workgroup (or n 1))
  (wg-fontified-msg (:cmd "Offset right: ") (wg-disp)))

(defun wg-rename (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list (wg-arg) (wg-read-name "New name: ")))
  (let ((oldname (wg-name workgroup)))
    (wg-set-name workgroup newname)
    (wg-fontified-msg
      (:cmd "Renamed: ") (:cur oldname) (:msg " to ")
      (:cur (wg-name workgroup)))))

(defun wg-reset ()
  "Reset workgroups.
Deletes saved state in `wg-frame-table' and nulls out
`wg-list', `wg-file' and `wg-killring'."
  (interactive)
  (unless (y-or-n-p "Are you sure? ") (error "Canceled"))
  (clrhash wg-frame-table)
  (setq wg-list nil
        wg-file nil
        wg-kill-ring nil
        wg-kill-ring-pos 0)
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
   (cons wg-file-id (wg-list)) file)
  (setq wg-dirty nil wg-file file)
  (wg-fontified-msg (:cmd "Wrote: ") (:file file)))

(defun wg-load (file)
  "Load workgroups from FILE.
Called interactively with a prefix arg, and if `wg-file'
is non-nil, use `wg-file'. Otherwise read a filename."
  (interactive
   (list (if (and current-prefix-arg (wg-file t))
             (wg-file) (read-file-name "File: "))))
  (destructuring-bind (file-id . workgroups)
      (wg-read-sexp-from-file file)
    (unless (eq file-id wg-file-id)
      (error "%S is not a workgroups file."))
    (clrhash wg-frame-table)
    (setq wg-list   workgroups
          wg-file   file
          wg-dirty  nil))
  (wg-awhen (wg-list t)
    (when wg-switch-on-load
      (wg-switch (car it))))
  (wg-fontified-msg (:cmd "Loaded: ") (:file file)))

(defun wg-find-file (file)
  "Create a new workgroup and find file FILE in it."
  (interactive "FFile: ")
  (wg-create (file-name-nondirectory file))
  (find-file file))

(defun wg-find-file-read-only (file)
  "Create a new workgroup and find FILE read-only in it."
  (interactive "FFile: ")
  (wg-create (file-name-nondirectory file))
  (find-file-read-only file))

(defun wg-get-by-buffer (buf)
  "Switch to the workgroup that contains BUF."
  (interactive (list (wg-read-buffer)))
  (wg-aif (wg-find-buffer buf)
      (apply 'wg-switch it)
    (error "No workgroup contains %S" buf)))

(defun wg-dired (dir &optional switches)
  "Create a workgroup and open DIR in dired with SWITCHES."
  (interactive (list (read-directory-name "Dired: ") current-prefix-arg))
  (wg-create dir)
  (dired dir switches))


;;; toggle commands

(defun wg-toggle-mode-line ()
  "Toggle workgroups' mode-line display."
  (interactive)
  (setq wg-mode-line-on (not wg-mode-line-on))
  (force-mode-line-update)
  (wg-fontified-msg
    (:cmd "mode-line: ") (:msg (if wg-mode-line-on "on" "off"))))

(defun wg-toggle-frame-wipe ()
  "Toggle frame-wiping on `wg-switch'."
  (interactive)
  (setq wg-frame-wipe-on (not wg-frame-wipe-on))
  (wg-fontified-msg
    (:cmd "Frame wipe: ") (:msg (if wg-frame-wipe-on "on" "off"))))


;;; echo commands

(defun wg-echo-current ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (wg-fontified-msg
    (:cmd "Current: ") (:cur (wg-name (wg-current)))))

(defun wg-echo-all ()
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

(defun wg-unset-prefix-key ()
  "Restore the original definition of `wg-prefix-key'."
  (let ((key wg-prefix-key))
    (when (eq wg-map (lookup-key global-map key))
      (global-set-key key (get 'wg-set-prefix-key :original)))))

(defun wg-set-prefix-key (key)
  "Define KEY as `wg-map' in `global-map'."
  (wg-unset-prefix-key)
  (put 'wg-set-prefix-key :original (lookup-key global-map key))
  (setq wg-prefix-key key)
  (global-set-key key wg-map))

(defvar wg-map
  (wg-fill-keymap
   (make-sparse-keymap)
   "C-c"        'wg-create
   "c"          'wg-create
   "'"          'wg-switch
   "C-'"        'wg-switch
   "C-v"        'wg-switch
   "v"          'wg-switch
   "C"          'wg-clone
   "C-k"        'wg-kill
   "k"          'wg-kill
   "C-y"        'wg-yank
   "y"          'wg-yank
   "M-w"        'wg-kill-ring-save
   "M-k"        'wg-kill-workgroup-and-buffers
   "K"          'wg-delete-other-workgroups
   "C-r"        'wg-revert
   "r"          'wg-revert
   "C-S-r"      'wg-revert-all
   "R"          'wg-revert-all
   "C-u"        'wg-update
   "u"          'wg-update
   "C-S-u"      'wg-update-all
   "U"          'wg-update-all
   "C-s"        'wg-save
   "C-l"        'wg-load
   "A"          'wg-rename
   "C-p"        'wg-prev
   "p"          'wg-prev
   "C-n"        'wg-next
   "n"          'wg-next
   "M-p"        'wg-prev-other-frame
   "M-n"        'wg-next-other-frame
   "C-a"        'wg-switch-to-previous
   "a"          'wg-switch-to-previous
   "C-j"        'wg-jump
   "0"          'wg-jump-0
   "1"          'wg-jump-1
   "2"          'wg-jump-2
   "3"          'wg-jump-3
   "4"          'wg-jump-4
   "5"          'wg-jump-5
   "6"          'wg-jump-6
   "7"          'wg-jump-7
   "8"          'wg-jump-8
   "9"          'wg-jump-9
   "C-x"        'wg-swap
   "C-,"        'wg-offset-left
   "C-."        'wg-offset-right
   "C-e"        'wg-echo-all
   "e"          'wg-echo-all
   "S-C-e"      'wg-echo-current
   "E"          'wg-echo-current
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
   "C-w"        'wg-toggle-frame-wipe
   "!"          'wg-reset
   "?"          'wg-help)
  "Workgroups' keymap.")


;;; help

(defvar wg-help
  '("\\[wg-create]"
    "Create a new workgroup and switch to it"
    "\\[wg-switch]"
    "Switch to a workgroup"
    "\\[wg-clone]"
    "Create a clone of the current workgroug and switch to it"
    "\\[wg-kill]"
    "Kill a workgroup"
    "\\[wg-yank]"
    "Set the working config to a config from the kill ring"
    "\\[wg-kill-ring-save]"
    "Save the current config to the kill ring"
    "\\[wg-kill-workgroup-and-buffers]"
    "Kill a workgroup and its buffer"
    "\\[wg-delete-other-workgroups]"
    "Delete all but the specified workgroup"
    "\\[wg-revert]"
    "Set a workgroup's working config to its base config"
    "\\[wg-update]"
    "Set a workgroup's base config to its working config"
    "\\[wg-save]"
    "Save workgroups to a file"
    "\\[wg-load]"
    "Load workgroups from a file"
    "\\[wg-rename]"
    "Rename a workgroup"
    "\\[wg-prev]"
    "Cycle leftward in the workgroups list"
    "\\[wg-next]"
    "Cycle rightward in the workgroups list"
    "\\[wg-offset-left]"
    "Offset a workgroup's position leftward"
    "\\[wg-offset-right]"
    "Offset a workgroup's position rightward"
    "\\[wg-swap]"
    "Swap the current and previous workgroups"
    "\\[wg-switch-to-previous]"
    "Switch to the previously selected workgroup"
    "\\[wg-jump]"
    "Jump to a workgroup by number"
    "\\[wg-jump-0]"
    "Switch to the workgroup at position 0"
    "\\[wg-jump-1]"
    "Switch to the workgroup at position 1"
    "\\[wg-jump-2]"
    "Switch to the workgroup at position 2"
    "\\[wg-jump-3]"
    "Switch to the workgroup at position 3"
    "\\[wg-jump-4]"
    "Switch to the workgroup at position 4"
    "\\[wg-jump-5]"
    "Switch to the workgroup at position 5"
    "\\[wg-jump-6]"
    "Switch to the workgroup at position 6"
    "\\[wg-jump-7]"
    "Switch to the workgroup at position 7"
    "\\[wg-jump-8]"
    "Switch to the workgroup at position 8"
    "\\[wg-jump-9]"
    "Switch to the workgroup at position 9"
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
    "\\[wg-toggle-frame-wipe]"
    "Toggle frame-wipe animation on workgroups switch"
    "\\[wg-echo-all]"
    "Display the names of all workgroups in the echo area"
    "\\[wg-echo-current]"
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

(defun wg-query-for-save ()
  "Query for save when `wg-dirty' is non-nil."
  (and wg-dirty
       (y-or-n-p "Workgroups have been modified. Save them? ")
       (call-interactively 'wg-save)))

(defun wg-query-hook ()
  "Query for save on exit if `wg-dirty' is non-nil."
  (when wg-query-for-save-on-emacs-exit
    (wg-query-for-save))
  t)

(defun wg-enable (enable)
  "Enable `wg-mode' when ENABLE is non-nil.
Disable otherwise."
  (cond (enable
         (add-hook 'kill-emacs-query-functions 'wg-query-hook)
         (wg-set-prefix-key wg-prefix-key)
         (wg-mode-line-display-add)
         (setq wg-mode t))
        (t
         (when wg-query-for-save-on-mode-exit
           (wg-query-for-save))
         (remove-hook 'kill-emacs-query-functions 'wg-query-hook)
         (wg-unset-prefix-key)
         (wg-mode-line-display-remove)
         (setq wg-mode nil))))

(define-minor-mode workgroups-mode
  "This toggles workgroups-mode.
If ARG is null, toggle workgroups-mode.
If ARG is a number greater than zero, turn on workgroups-mode.
Otherwise, turn off workgroups-mode."
  :lighter     " wg"
  :init-value  nil
  :global      t
  :group       'workgroups
  (cond (noninteractive   (wg-enable nil))
        (workgroups-mode  (wg-enable t))
        (t                (wg-enable nil))))


;;; provide

(provide 'workgroups-mode)


;;; workgroups-mode.el ends here
