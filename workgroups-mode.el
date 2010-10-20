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
;;  - add frame identifiers to messages
;;  - split and join
;;  - fix set-prefix-key
;;  - change goddam "workgroups" to something else. "frameup"?
;;  - undo/redo
;;  - add minibuffer persistence
;;


;;; Code:

(require 'cl)


;;; consts

(defconst workgroups-version "0.1.9"
  "Current version number of workgroups.")

(defconst workgroups-fid '-*-workgroups-*-
  "Symbol identifying a workgroups file.")


;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- Emacs session manager"
  :group 'convenience
  :version workgroups-version)

(defcustom workgroups-prefix-key (kbd "C-z")
  "Workgroups' prefix key."
  :type 'string
  :group 'workgroups
  :set (lambda (sym val)
         (when (fboundp 'workgroups-set-prefix-key)
           (workgroups-set-prefix-key val)
           (custom-set-default sym val))))

(defcustom workgroups-switch-hook nil
  "Hook run whenever a workgroup is switched to."
  :type 'hook
  :group 'workgroups)

(defcustom workgroups-restore-position nil
  "Non-nil means restore the frame's position when switching to a
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

(defcustom workgroups-query-save-on-emacs-exit t
  "Non-nil means query for save before exiting when there are
unsaved changes."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-query-save-on-mode-exit t
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

(defcustom workgroups-warning-timeout 0.7
  "Seconds to display minibuffer warning messages."
  :type 'float
  :group 'workgroups)

(defcustom workgroups-mode-line-on t
  "Toggles workgroups' mode-line display."
  :type 'boolean
  :group 'workgroups
  :set (lambda (sym val)
         (custom-set-default sym val)
         (force-mode-line-update)))

(defcustom workgroups-disp-left-brace "( "
  "String on the left of the list display."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-disp-right-brace " )"
  "String on the right of the list display."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-disp-divider " | "
  "String between workgroups names in the list display."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-disp-current-left "*" ;; "-<{ "
  "String displayed to the left of the current workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-disp-current-right "" ;; " }>-"
  "String displayed to the right of the current workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-disp-previous-left "~" ;; "*"
  "String displayed to the left of the previous workgroup in the
list display."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-disp-previous-right "" ;; "*"
  "String displayed to the right of the previous workgroup in the
list display."
  :type 'string
  :group 'workgroups)


;;; vars

(defvar workgroups-list nil
  "Current list of workgroups.")

(defvar workgroups-frame nil
  "Bind when operating on a frame other than `selected-frame'.")

(defvar workgroups-frame-table (make-hash-table)
  "Hash table storing global state for each frame.")

(defvar workgroups-file nil
  "Current workgroups file.")

(defvar workgroups-dirty nil
  "Non-nil when there are unsaved changes.")

(defvar workgroups-kill-ring nil
  "Kill ring of saved configs.")

(defvar workgroups-kill-ring-pos 0
  "Position in `workgroups-kill-ring' during sequential yanks.")

(defvar workgroups-prefix-key-original-def nil
  "Original binding of `workgroups-prefix-key'.")

(defvar workgroups-face-abbrevs
  '((:cur   . workgroups-current-workgroup-face)
    (:prev  . workgroups-previous-workgroup-face)
    (:oth   . workgroups-other-workgroup-face)
    (:cmd   . workgroups-command-face)
    (:div   . workgroups-divider-face)
    (:brace . workgroups-brace-face)
    (:msg   . workgroups-message-face)
    (:mode  . workgroups-mode-line-face)
    (:file  . workgroups-filename-face))
  "Assoc list mapping face abbreviations to face names.")


;;; faces

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

(defface workgroups-command-face
  '((((class color)) (:foreground "aquamarine")))
  "Face used for command/operation names."
  :group 'workgroups)

(defface workgroups-divider-face
  '((((class color)) (:foreground "light slate blue")))
  "Face used for dividers."
  :group 'workgroups)

(defface workgroups-brace-face
  '((((class color)) (:foreground "light slate blue")))
  "Face used for left and right braces."
  :group 'workgroups)

(defface workgroups-message-face
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


;;; utils

(defmacro workgroups-dohash (bindings &rest body)
  "do-style wrapper for maphash."
  (declare (indent defun))
  (destructuring-bind (key val table &optional ret) bindings
    `(progn (maphash (lambda (,key ,val) ,@body) ,table)
            ,ret)))

(defmacro workgroups-doconcat (binds &rest body)
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

(defmacro workgroups-letsoc (keylist alist &rest body)
  "Bind keys in KEYLIST to their values in ALIST, then eval BODY.
Requires that all keys in KEYLIST are bindable symbols."
  (declare (indent defun))
  (let ((asym (gensym)))
    `(let* ((,asym ,alist)
            ,@(mapcar (lambda (key) `(,key (cdr (assq ',key ,asym))))
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

(defun workgroups-vassq (alist key)
  "Return the value portion of KEY's key-value-pair in ALIST."
  (cdr (assq key alist)))

(defun workgroups-alist-set (alist key val)
  "If KEY exists in ALIST, set its val to VAL and return VAL.
Otherwise do nothing and return nil."
  (workgroups-awhen (assq key alist)
    (setcdr it val)))

(defun workgroups-map-assq (key val list)
  "Return the first alist in LIST containing KEY and VAL."
  (catch 'res
    (dolist (alist list)
      (when (eq val (workgroups-vassq alist key))
        (throw 'res alist)))))

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

(defun workgroups-window-list (&optional frame)
  "Flatten `window-tree' into a stable list.
`window-list' can't be used because its order isn't stable."
  (flet ((inner (obj) (if (atom obj) (list obj)
                        (mapcan 'inner (cddr obj)))))
    (inner (car (window-tree (or frame (selected-frame)))))))

(defun workgroups-add-face (facekey str)
  "Return a copy of STR fontified according to FACEKEY.
FACEKEY must be a key in `workgroups-face-abbrevs'."
  (let ((face (workgroups-vassq workgroups-face-abbrevs facekey))
        (str  (copy-seq str)))
    (unless face (error "No face with key %s" facekey))
    (if (not workgroups-use-faces) str
      (put-text-property 0 (length str) 'face face str)
      str)))

(defmacro workgroups-facify (&rest format)
  "A small fontification DSL."
  `(concat
    ,@(mapcar
       (lambda (spec)
         (typecase spec
           (cons (if (keywordp (car spec))
                     `(workgroups-add-face ,@spec)
                   `(progn ,spec)))
           (string `(progn ,spec))
           (atom `(format "%s" ,spec))))
       format)))


;;; frame ops

(defun workgroups-frame ()
  "Return `workgroups-frame' or `selected-frame'.
This should be used wherever a frame is needed."
  (or workgroups-frame (selected-frame)))

(defmacro workgroups-with-frame-state (frame state &rest body)
  "Bind FRAME and STATE in BODY.
FRAME is bound to `workgroups-frame', and STATE is bound to
FRAME's value in `workgroups-frame-table'."
  (declare (indent defun))
  `(let* ((,frame (workgroups-frame))
          (,state (gethash ,frame workgroups-frame-table)))
     ,@body))

(defun workgroups-frame-val (key)
  "Return KEY's value in `workgroups-frame-table'."
  (workgroups-with-frame-state frame state
    (workgroups-vassq state key)))

(defun workgroups-set-frame-val (key val)
  "Set KEY to VAL in `workgroups-frame-table'."
  (workgroups-with-frame-state frame state
    (or (workgroups-alist-set state key val)
        (push (cons key val)
              (gethash frame workgroups-frame-table)))))

(defun workgroups-delete-frame-key (key)
  "Remove KEY from frame's entry in `workgroups-frame-table'."
  (workgroups-with-frame-state frame state
    (puthash frame (remove (assq key state) state)
             workgroups-frame-table)))


;;; window config utils

(defun workgroups-window-p (window)
  "t if WINDOW is a workgroups window, nil otherwise."
  (and (consp window) (assq 'width window)))

(defun workgroups-wsize (win &optional height)
  "Return the width or height of WIN."
  (if (workgroups-window-p win)
      (workgroups-vassq win (if height 'height 'width))
    (destructuring-bind (x0 y0 x1 y1) (cadr win)
      (if height (- y1 y0) (- x1 x0)))))


;;; window config making

(defun workgroups-make-window (winobj)
  "Return a workgroups window from WINOBJ.
WINOBJ is an Emacs window object."
  (with-current-buffer (window-buffer winobj)
    (let ((p (point)) (edges (window-edges winobj)))
      `((width   .  ,(- (nth 2 edges) (nth 0 edges)))
        (height  .  ,(window-height winobj))
        (bname   .  ,(buffer-name))
        (fname   .  ,(buffer-file-name))
        (point   .  ,(if (eq p (point-max)) :max p))
        (mark    .  ,(mark))
        (markx   .  ,mark-active)
        (wstart  .  ,(window-start winobj))
        (sbars   .  ,(window-scroll-bars winobj))
        (margins .  ,(window-margins winobj))
        (fringes .  ,(window-fringes winobj))))))

(defun workgroups-make-window-tree (wtree)
  "Return a workgroups window tree from WTREE."
  (etypecase wtree
    (window (workgroups-make-window wtree))
    (cons `(,@(workgroups-take wtree 2)
            ,@(mapcar 'workgroups-make-window-tree
                      (cddr wtree))))))

(defun workgroups-make-window-config ()
  "Return a workgroups window config."
  (let* ((frame (workgroups-frame)) (wl (workgroups-window-list frame)))
    (flet ((fparam (param) (frame-parameter frame param)))
      `((left    . ,(fparam 'left))
        (top     . ,(fparam 'top))
        (width   . ,(fparam 'width))
        (height  . ,(fparam 'height))
        (sbars   . ,(fparam 'vertical-scroll-bars))
        (sbwid   . ,(fparam 'scroll-bar-width))
        (swidx   . ,(position (frame-selected-window frame) wl))
        (mbswidx . ,(position minibuffer-scroll-window wl))
        (wtree   . ,(workgroups-make-window-tree
                     (car (window-tree frame))))))))

(defun workgroups-make-default-window-config (&optional buffer)
  "Return a new default config."
  (save-window-excursion
    (let ((winobj (frame-selected-window (workgroups-frame))))
      (delete-other-windows winobj)
      (set-window-buffer winobj (or buffer workgroups-default-buffer))
      (workgroups-make-window-config))))


;;; window config restoring

(defun workgroups-restore-window (window winobj)
  "Restore WINDOW's state in `selected-window'."
  (workgroups-letsoc
    (fname bname point mark markx wstart sbars fringes margins) window
    (destructuring-bind (w c v h) sbars
      (set-window-scroll-bars winobj w v h))
    (with-current-buffer
        (or (and fname (file-exists-p fname) (find-file-noselect fname))
            (get-buffer bname) workgroups-default-buffer)
      (unless (equal (buffer-name) workgroups-default-buffer)
        (rename-buffer bname))
      (set-window-buffer winobj (current-buffer))
      (set-window-start winobj wstart t)
      (set-window-margins winobj (car margins) (cdr margins))
      (destructuring-bind (lwid rwid outside) fringes
        (set-window-fringes winobj lwid rwid outside))
      (goto-char (if (eq point :max) (point-max) point))
      (set-mark mark)
      (unless markx (deactivate-mark))
      (when (>= wstart (point-max)) (recenter)))))

(defun workgroups-restore-window-tree (wtree frame)
  "Restore the window layout specified by WTREE."
  (cond ((workgroups-window-p wtree)
         (let ((winobj (frame-selected-window frame)))
           (workgroups-restore-window wtree winobj)
           (set-frame-selected-window
            frame (next-window winobj 'no-mini frame))))
        (t (let ((vertically (car wtree))
                 (lastwin (car (last wtree))))
             (dolist (win (cddr wtree))
               (unless (eq win lastwin)
                 (split-window
                  (frame-selected-window frame)
                  (workgroups-wsize win vertically)
                  (not vertically)))
               (workgroups-restore-window-tree win frame))))))

(defun workgroups-restore-window-config (config)
  "Restore CONFIG."
  (let ((frame (workgroups-frame)))
    (flet ((set-fparam (p v) (set-frame-parameter frame p v)))
      (workgroups-letsoc
        (left top width height sbars sbwid swidx mbswidx wtree) config
        (when workgroups-restore-position
          (set-fparam 'left left)
          (set-fparam 'top  top))
        (set-fparam 'width  width)
        (set-fparam 'height height)
        (set-fparam 'vertical-scroll-bars sbars)
        (set-fparam 'scroll-bar-width sbwid)
        (delete-other-windows (frame-selected-window frame))
        (workgroups-restore-window-tree wtree frame)
        (let ((wlst (workgroups-window-list frame)))
          (set-frame-selected-window frame (nth swidx wlst))
          (and mbswidx (setq minibuffer-scroll-window (nth mbswidx wlst))))))))

(defun workgroups-restore-default-window-config ()
  "Restore the default config."
  (workgroups-restore-window-config
   (workgroups-make-default-window-config)))


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
  (or (workgroups-map-assq key val (workgroups-list noerror))
      (unless noerror
        (error "There is no workgroup with an %S of %S" key val))))


;;; workgroup property ops

(defun workgroups-uid (workgroup)
  "Return WORKGROUP's uid."
  (workgroups-vassq workgroup :uid))

(defun workgroups-set-uid (workgroup uid)
  "Set the uid of WORKGROUP to UID."
  (workgroups-alist-set workgroup :uid uid)
  (setq workgroups-dirty t))

(defun workgroups-uids (&optional noerror)
  "Return a list of workgroups uids."
  (mapcar 'workgroups-uid (workgroups-list noerror)))

(defun workgroups-new-uid ()
  "Return an id unused in `workgroups-list'."
  (let ((uids (workgroups-uids t)) (i -1))
    (while (memq (incf i) uids)) i))

(defun workgroups-name (workgroup)
  "Return the name of WORKGROUP."
  (workgroups-vassq workgroup :name))

(defun workgroups-set-name (workgroup name)
  "Set the name of WORKGROUP to NAME."
  (workgroups-alist-set workgroup :name name)
  (setq workgroups-dirty t))

(defun workgroups-names (&optional noerror)
  "Return a list of workgroups names."
  (mapcar 'workgroups-name (workgroups-list noerror)))


;;; current and previous workgroup ops

(defun workgroups-current (&optional noerror)
  "Return the current workgroup."
  (workgroups-aif (workgroups-frame-val :current)
    (workgroups-get :uid it)
    (unless noerror
      (error "There's no current workgroup in the frame"))))

(defun workgroups-set-current (workgroup)
  "Set the current workgroup to WORKGROUP."
  (workgroups-set-frame-val :current (workgroups-uid workgroup)))

(defun workgroups-previous (&optional noerror)
  "Return `workgroups-previous'."
  (workgroups-aif (workgroups-frame-val :previous)
    (workgroups-get :uid it)
    (unless noerror
      (error "There's no previous workgroup in the frame"))))

(defun workgroups-set-previous (workgroup)
  "Set the previous workgroup to WORKGROUP."
  (workgroups-set-frame-val :previous (workgroups-uid workgroup)))


;;; base and working configs

(defun workgroups-base-config (workgroup)
  "Return the base config of WORKGROUP."
  (workgroups-vassq workgroup :config))

(defun workgroups-set-base-config (workgroup config)
  "Set the base config of WORKGROUP to CONFIG."
  (workgroups-alist-set workgroup :config config)
  (setq workgroups-dirty t))

(defun workgroups-set-working-config (workgroup config)
  "Set the working config of WORKGROUP to CONFIG."
  (workgroups-set-frame-val (workgroups-uid workgroup) config))

(defun workgroups-update-working-config (workgroup)
  "Set WORKGROUP's working config to the current window config."
  (workgroups-set-working-config
   workgroup (workgroups-make-window-config)))

(defun workgroups-working-config (workgroup)
  "Return the working config of WORKGROUP.
If WORKGROUP is the current workgroup, first update it with
`workgroups-set-working-config'."
  (when (eq workgroup (workgroups-current t))
    (workgroups-update-working-config workgroup))
  (or (workgroups-frame-val (workgroups-uid workgroup))
      (workgroups-base-config workgroup)))


;;; workgroup making and restoring

(defun workgroups-make-workgroup (uid name config)
  "Make a workgroup named NAME from BASE and WORKING."
  `((:uid    . ,uid)
    (:name   . ,name)
    (:config . ,config)))

(defun workgroups-copy-workgroup (workgroup &optional name)
  "Return a copy of WORKGROUP, optionally named NAME."
  (workgroups-make-workgroup
   nil (or name (workgroups-name workgroup)) (workgroups-base-config workgroup)))

(defun workgroups-make-default-workgroup (name &optional buffer)
  "Return a new default workgroup named NAME."
  (workgroups-make-workgroup
   nil name (workgroups-make-default-window-config buffer)))

(defun workgroups-restore-workgroup (workgroup &optional base)
  "Restore WORKGROUP."
  (workgroups-restore-window-config
   (if base (workgroups-base-config workgroup)
     (workgroups-working-config workgroup))))


;;; workgroups list ops

(defun workgroups-delete (wg)
  "Remove WORKGROUP from `workgroups-list'."
  (workgroups-dohash (frame state workgroups-frame-table)
    (let ((workgroups-frame frame))
      (workgroups-delete-frame-key (workgroups-uid wg))
      (when (eq wg (workgroups-current t))
        (workgroups-set-current nil))
      (when (eq wg (workgroups-previous t))
        (workgroups-set-previous nil))))
  (setq workgroups-dirty t)
  (setq workgroups-list (remove wg (workgroups-list))))

;;; asdf

(defun workgroups-add (new &optional pos)
  "Add WORKGROUP at `workgroups-list'.
If a workgroup with the same name exists, query to overwrite it."
  (workgroups-awhen (workgroups-get :name (workgroups-name new) t)
    (unless pos (setq pos (position it workgroups-list)))
    (workgroups-delete it))
  (workgroups-set-uid new (workgroups-new-uid))
  (setq workgroups-dirty t)
  (setq workgroups-list (workgroups-linsert new workgroups-list pos)))

(defun workgroups-list-swap (w1 w2)
  "Swap W1 and W2 in `workgroups-list'."
  (when (eq w1 w2) (error "Can't swap a workgroup with itself"))
  (workgroups-aif (workgroups-util-swap w1 w2 (workgroups-list))
    (setq workgroups-list it workgroups-dirty t)
    (error "W1 and W2 aren't both present in `workgroups-list'.")))

(defun workgroups-get-next (workgroup &optional noerror)
  "Return the workgroup after WORKGROUP cyclically."
  (workgroups-cnext workgroup (workgroups-list noerror)))

(defun workgroups-get-prev (workgroup &optional noerror)
  "Return the workgroup before WORKGROUP cyclically."
  (workgroups-cprev workgroup (workgroups-list noerror)))


;;; buffer list ops

(defun workgroups-config-buffer-list (config)
  "Return the names of all unique buffers in CONFIG."
  (let (bufs)
    (flet ((inner (obj)
                  (if (not (workgroups-window-p obj))
                      (mapc 'inner (cddr obj))
                    (workgroups-awhen (workgroups-vassq obj :buffer-name)
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
    (workgroups-facify
     (:div "(") (:mode (position cur (workgroups-list t)))
     (:div ":") (:mode (workgroups-name cur)) (:div ")"))))

(defun workgroups-mode-line-display-add ()
  "Turn on workgroups' mode-line display."
  (unless (assq 'workgroups-mode-line-on mode-line-format)
    (set-default 'mode-line-format
                 (workgroups-linsert
                  `(workgroups-mode-line-on
                    (:eval (workgroups-mode-line-string)))
                  mode-line-format
                  (1+ (position 'mode-line-position mode-line-format))))))

(defun workgroups-mode-line-display-remove ()
  "Turn off workgroups' mode-line display."
  (workgroups-awhen (assq 'workgroups-mode-line-on mode-line-format)
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
        (warning (workgroups-facify :msg "Name must be non-empty")))
    (while (and (setq name (read-from-minibuffer prompt))
                (equal name ""))
      (message warning)
      (sit-for workgroups-warning-timeout))
    name))

(defun workgroups-read-idx (low high &optional prompt)
  "Read and return a valid workgroup index."
  (let ((prompt (or prompt (format "[%d-%d]: " low high))) i
        (warning (workgroups-facify
                  :msg (format "Enter an integer [%d-%d]" low high))))
    (while (and (setq i (read-from-minibuffer prompt nil nil t))
                (or (not (integerp i)) (< i low) (> i high)))
      (message warning)
      (sit-for workgroups-warning-timeout))
    i))

(defun workgroups-read-buffer ()
  "Read and return a buffer from `workgroups-buffer-list'."
  (workgroups-completing-read "Buffer: " (workgroups-buffer-list)))


;;; messaging

(defvar workgroups-last-message nil
  "The last string workgroups sent to the echo area.")

(defun workgroups-message (format-string &rest args)
  "`message' and save the msg to `workgroups-last-message'."
  (setq workgroups-last-message (apply 'message format-string args)))

(defmacro workgroups-facified-msg (&rest format)
  "Facify FORMAT and call `workgroups-message' on it."
  `(workgroups-message (workgroups-facify ,@format)))


;;; command utils

(defun workgroups-arg (&optional reverse noerror)
  "Return a workgroup.  For use in interactive forms.
If `current-prefix-arg' is nil return the current workgroups.
Otherwise read a workgroup from the minibuffer.  If REVERSE is
non-nil, `current-prefix-arg''s begavior is reversed."
  (if (if reverse (not current-prefix-arg) current-prefix-arg)
      (workgroups-read-workgroup noerror)
    (workgroups-current noerror)))

(defun workgroups-add-to-kill-ring (config)
  "Add CONFIG to `workgroups-kill-ring'."
  (push config workgroups-kill-ring)
  (setq workgroups-kill-ring
        (workgroups-take workgroups-kill-ring
                         workgroups-kill-ring-size)))

(defun workgroups-list-string ()
  "Return a string of the names of all workgroups."
  (let ((cur   (workgroups-current  t))
        (prev  (workgroups-previous t))
        (div   (workgroups-add-face :div workgroups-disp-divider))
        (clb   workgroups-disp-current-left)
        (crb   workgroups-disp-current-right)
        (plb   workgroups-disp-previous-left)
        (prb   workgroups-disp-previous-right)
        (i     -1))
    (workgroups-facify
     (:brace workgroups-disp-left-brace)
     (workgroups-doconcat (w (workgroups-list t) div)
       (let ((str (format "%d: %s" (incf i) (workgroups-name w))))
         (cond ((eq w cur)
                (workgroups-facify (:cur (concat clb str crb))))
               ((eq w prev)
                (workgroups-facify (:prev (concat plb str prb))))
               (t (workgroups-facify (:oth str))))))
     (:brace workgroups-disp-right-brace))))

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
  (let ((current (workgroups-current t)))
    (if (eq workgroup current)
        (workgroups-message "Already on %s" (workgroups-name workgroup))
      (workgroups-update-working-config current)
      (when workgroups-frame-wipe-on (workgroups-frame-wipe))
      (workgroups-restore-workgroup workgroup base)
      (workgroups-set-previous current)
      (workgroups-set-current workgroup)
      (run-hooks 'workgroups-switch-hook)
      (workgroups-facified-msg
       (:cmd "Switched: ") (workgroups-list-string)))))

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
  (workgroups-ficified-msg (:cmd "Created: ") (workgroups-list-string)))

(defun workgroups-clone (workgroup name base)
  "Create and add a clone of WORKGROUP named NAME."
  (interactive
   (list (workgroups-arg) (workgroups-read-name) current-prefix-arg))
  (workgroups-add-and-switch
   (workgroups-make-workgroup
    nil name (if base (workgroups-base-config workgroup)
               (workgroups-working-config workgroup))))
  (workgroups-facified-msg (:cmd "Cloned: ") (workgroups-list-string)))

(defun workgroups-kill (workgroup)
  "Kill workgroup named NAME.
The working config of WORKGROUP is saved to
`workgroups-kill-ring'."
  (interactive (list (workgroups-arg)))
  (workgroups-add-to-kill-ring (workgroups-working-config workgroup))
  (let ((switch-to (or (workgroups-previous t)
                       (workgroups-get-next workgroup))))
    (workgroups-delete workgroup)
    (if (eq workgroup switch-to)
        (workgroups-restore-default-window-config)
      (workgroups-switch switch-to))
    (workgroups-facified-msg
     (:cmd "Killed: ") (:cur (workgroups-name workgroup))
     (workgroups-list-string))))

(defun workgroups-kill-ring-save (workgroup)
  "Save WORKGROUP's working config to `workgroups-kill-ring'."
  (interactive (list (workgroups-arg)))
  (workgroups-add-to-kill-ring (workgroups-working-config workgroup))
  (workgroups-facified-msg
   (:cmd "Saved: ") (:cur (workgroups-name workgroup))
   (:cur "'s ") (:msg "working config")))

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
      (workgroups-facified-msg (:cmd "Yanked: ") (:msg pos)))
    (error "Workgroups' kill-ring is empty")))

(defun workgroups-kill-workgroup-and-buffers (workgroup)
  "Kill WORKGROUP and the buffers in its working config."
  (interactive (list (workgroups-arg)))
  (let ((bufs (save-window-excursion
                (workgroups-restore-workgroup workgroup)
                (mapcar 'window-buffer (window-list)))))
    (workgroups-kill workgroup)
    (mapc 'kill-buffer bufs)
    (workgroups-facified-msg
     (:cmd "Killed: ") (:cur (workgroups-name workgroup))
     (:msg " and its buffers ") (workgroups-list-string))))

(defun workgroups-delete-other-workgroups (workgroup)
  "Delete all but the WORKGROUP."
  (interactive (list (workgroups-arg)))
  (let ((cur (workgroups-current)))
    (mapc 'workgroups-delete (remove workgroup (workgroups-list)))
    (unless (eq workgroup cur) (workgroups-switch workgroup))
    (workgroups-facified-msg
     (:cmd "Deleted: ") (:msg "All workgroups but ")
     (:cur (workgroups-name workgroup)))))

(defun workgroups-update (workgroup)
  "Set the base config of WORKGROUP to its current config."
  (interactive (list (workgroups-arg)))
  (workgroups-set-base-config
   workgroup (workgroups-working-config workgroup))
  (workgroups-facified-msg
   (:cmd "Updated: ") (:cur (workgroups-name workgroup))))

(defun workgroups-update-all ()
  "Update all workgroups."
  (interactive)
  (mapc 'workgroups-update (workgroups-list))
  (workgroups-facified-msg (:cmd "Updated: ") (:msg "All")))

(defun workgroups-revert (workgroup)
  "Set the working config of WORKGROUP to its base config."
  (interactive (list (workgroups-arg)))
  (workgroups-set-working-config
   workgroup (workgroups-base-config workgroup))
  (when (eq workgroup (workgroups-current))
    (workgroups-restore-workgroup workgroup t))
  (workgroups-facified-msg
   (:cmd "Reverted: ") (:cur (workgroups-name workgroup))))

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
  (workgroups-switch (workgroups-get-prev (workgroups-current t))))

(defun workgroups-next ()
  "Switch to the workgroup after WORKGROUP in `workgroups-list'."
  (interactive)
  (workgroups-switch (workgroups-get-next (workgroups-current t))))

(defun workgroups-prev-workgroup-next-frame ()
  "Like `workgroups-prev', but operates on the next frame."
  (interactive)
  (let ((workgroups-frame (next-frame))
        (workgroups-restore-position))
    (workgroups-prev)))

(defun workgroups-next-workgroup-next-frame ()
  "Like `workgroups-next', but operates on the next frame."
  (interactive)
  (let ((workgroups-frame (next-frame))
        (workgroups-restore-position))
    (workgroups-next)))

(defun workgroups-toggle ()
  "Switch to the previous workgroup."
  (interactive)
  (workgroups-switch (workgroups-previous)))

(defun workgroups-swap ()
  "Swap the current workgroup with the previous."
  (interactive)
  (workgroups-list-swap (workgroups-current) (workgroups-previous))
  (workgroups-facified-msg (:cmd "Swapped ") (workgroups-list-string)))

(defun workgroups-transpose-left (workgroup)
  "Swap WORKGROUP toward the beginning of `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (workgroups-aif (workgroups-lprev workgroup (workgroups-list))
    (progn (workgroups-list-swap workgroup it)
           (workgroups-facified-msg
            (:cmd "Transposed: ") (workgroups-list-string)))
    (error "Can't transpose %s any further."
           (workgroups-name workgroup))))

(defun workgroups-transpose-right (workgroup)
  "Move WORKGROUP toward the end of `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (workgroups-aif (workgroups-lnext workgroup (workgroups-list))
    (progn (workgroups-list-swap workgroup it)
           (workgroups-facified-msg
            (:cmd "Transposed: ") (workgroups-list-string)))
    (error "Can't transpose %s any further."
           (workgroups-name workgroup))))

(defun workgroups-rename (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list (workgroups-arg) (workgroups-read-name "New name: ")))
  (let ((oldname (workgroups-name workgroup)))
    (workgroups-set-name workgroup newname)
    (workgroups-facified-msg
     (:cmd "Renamed: ") (:cur oldname) (:msg " to ")
     (:cur (workgroups-name workgroup)))))

(defun workgroups-reset ()
  "Reset workgroups.
Deletes saved state in `workgroups-frame-table' and nulls out
`workgroups-list', `workgroups-file' and `workgroups-killring'."
  (interactive)
  (unless (y-or-n-p "Are you sure? ") (error "Canceled"))
  (clrhash workgroups-frame-table)
  (setq workgroups-list nil
        workgroups-file nil
        workgroups-kill-ring nil
        workgroups-kill-ring-pos 0)
  (workgroups-facified-msg (:cmd "Reset: ") (:msg "Workgroups")))


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
  (workgroups-facified-msg (:cmd "Wrote: ") (:file file)))

(defun workgroups-load (file)
  "Load workgroups from FILE.
Called interactively with a prefix arg, and if `workgroups-file'
is non-nil, use `workgroups-file'. Otherwise read a filename."
  (interactive
   (list (if (and current-prefix-arg (workgroups-file t))
             (workgroups-file) (read-file-name "File: "))))
  (destructuring-bind (fid . workgroups)
      (workgroups-read-sexp-from-file file)
    (unless (eq fid workgroups-fid)
      (error "%S is not a workgroups file."))
    (clrhash workgroups-frame-table)
    (setq workgroups-list   workgroups
          workgroups-file   file
          workgroups-dirty  nil))
  (workgroups-awhen (workgroups-list t)
    (when workgroups-switch-on-load
      (workgroups-switch (car it))))
  (workgroups-facified-msg (:cmd "Loaded: ") (:file file)))

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
  (workgroups-facified-message
   (:cmd "mode-line: ") (:msg (if workgroups-mode-line-on "on" "off"))))

(defun workgroups-toggle-frame-wipe ()
  "Toggle frame-wiping on `workgroups-switch'."
  (interactive)
  (setq workgroups-frame-wipe-on (not workgroups-frame-wipe-on))
  (workgroups-facified-msg
   (:cmd "Frame wipe: ") (:msg (if workgroups-frame-wipe-on "on" "off"))))


;;; echo commands

(defun workgroups-echo-current ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (workgroups-facified-msg
   (:cmd "Current: ") (:cur (workgroups-name (workgroups-current)))))

(defun workgroups-echo-all ()
  "Display the names of all workgroups in the echo area."
  (interactive)
  (workgroups-facified-msg (:cmd "Workgroups: ") (workgroups-list-string)))

(defun workgroups-echo-time ()
  "Echo the current time."
  (interactive)
  (workgroups-message
   "%s" (workgroups-facify
         (:cmd "Current time: ")
         (:msg (format-time-string workgroups-time-format))
         (when (and workgroups-display-battery (fboundp 'battery))
           (workgroups-facify "\n" (:cmd "Battery: ") (:msg (battery)))))))

(defun workgroups-echo-version ()
  "Echo the current version number."
  (interactive)
  (workgroups-facified-msg
   (:cmd "Workgroups version: ") (:msg workgroups-version)))

(defun workgroups-echo-last-message ()
  "Echo the last message workgroups sent to the echo area.
The string is passed through a format arg to escape %'s."
  (interactive)
  (message "%s" workgroups-last-message))


;;; keymap

(defun workgroups-unset-prefix-key ()
  "Restore the original definition of `workgroups-prefix-key'."
  (let ((key workgroups-prefix-key))
    (when (eq workgroups-map (lookup-key global-map key))
      (global-set-key key workgroups-prefix-key-original-def))))

(defun workgroups-set-prefix-key (key)
  "Define KEY as `workgroups-map' in `global-map'."
  (workgroups-unset-prefix-key)
  (setq workgroups-prefix-key-original-def (lookup-key global-map key))
  (setq workgroups-prefix-key key)
  (global-set-key key workgroups-map))


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
   "M-u"        'workgroups-update-all
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
   "C-m"        'workgroups-echo-last-message
   "m"          'workgroups-echo-last-message
   "C-b"        'workgroups-get-by-buffer
   "b"          'workgroups-get-by-buffer
   "C-f"        'workgroups-find-file
   "S-C-f"      'workgroups-find-file-read-only
   "d"          'workgroups-dired
   "C-i"        'workgroups-toggle-mode-line
   "C-w"        'workgroups-toggle-frame-wipe
   "!"          'workgroups-reset
   "?"          'workgroups-help)
  "Workgroups' keymap.")


;;; help

(defvar workgroups-help
  '("\\[workgroups-create]"
    "Create a new workgroup and switch to it"
    "\\[workgroups-switch]"
    "Switch to a workgroup"
    "\\[workgroups-clone]"
    "Create a clone of the current workgroug and switch to it"
    "\\[workgroups-kill]"
    "Kill a workgroup"
    "\\[workgroups-yank]"
    "Set the working config to a config from the kill ring"
    "\\[workgroups-kill-ring-save]"
    "Save the current config to the kill ring"
    "\\[workgroups-kill-workgroup-and-buffers]"
    "Kill a workgroup and its buffer"
    "\\[workgroups-delete-other-workgroups]"
    "Delete all but the specified workgroup"
    "\\[workgroups-revert]"
    "Set a workgroup's working config to its base config"
    "\\[workgroups-update]"
    "Set a workgroup's base config to its working config"
    "\\[workgroups-save]"
    "Save workgroups to a file"
    "\\[workgroups-load]"
    "Load workgroups from a file"
    "\\[workgroups-rename]"
    "Rename a workgroup"
    "\\[workgroups-prev]"
    "Cycle leftward in the workgroups list"
    "\\[workgroups-next]"
    "Cycle rightward in the workgroups list"
    "\\[workgroups-transpose-left]"
    "Transpose a workgroup leftward in the workgroups list"
    "\\[workgroups-transpose-right]"
    "Transpose a workgroup rightward in the workgroups list"
    "\\[workgroups-swap]"
    "Swap the current and previous workgroups' positions in the workgroups list"
    "\\[workgroups-toggle]"
    "Switch to the previously selected workgroup"
    "\\[workgroups-jump]"
    "Jump to a workgroup by number"
    "\\[workgroups-jump-0]"
    "Switch to the workgroup at position 0 in the workgroups list"
    "\\[workgroups-jump-1]"
    "Switch to the workgroup at position 1 in the workgroups list"
    "\\[workgroups-jump-2]"
    "Switch to the workgroup at position 2 in the workgroups list"
    "\\[workgroups-jump-3]"
    "Switch to the workgroup at position 3 in the workgroups list"
    "\\[workgroups-jump-4]"
    "Switch to the workgroup at position 4 in the workgroups list"
    "\\[workgroups-jump-5]"
    "Switch to the workgroup at position 5 in the workgroups list"
    "\\[workgroups-jump-6]"
    "Switch to the workgroup at position 6 in the workgroups list"
    "\\[workgroups-jump-7]"
    "Switch to the workgroup at position 7 in the workgroups list"
    "\\[workgroups-jump-8]"
    "Switch to the workgroup at position 8 in the workgroups list"
    "\\[workgroups-jump-9]"
    "Switch to the workgroup at position 9 in the workgroups list"
    "\\[workgroups-get-by-buffer]"
    "Switch to the workgroup and config which contains the specified buffer"
    "\\[workgroups-find-file]"
    "Create a new workgroup and find a file in it"
    "\\[workgroups-find-file-read-only]"
    "Create a new workgroup and find-file-read-only in it"
    "\\[workgroups-dired]"
    "Create a new workgroup and open a dired buffer in it"
    "\\[workgroups-toggle-mode-line]"
    "Toggle workgroups mode-line display"
    "\\[workgroups-toggle-frame-wipe]"
    "Toggle frame-wipe animation on workgroups switch"
    "\\[workgroups-echo-all]"
    "Display the names of all workgroups in the echo area"
    "\\[workgroups-echo-current]"
    "Display the name of the current workgroup in the echo area"
    "\\[workgroups-echo-time]"
    "Display the current time in the echo area"
    "\\[workgroups-echo-version]"
    "Display the version in the echo area"
    "\\[workgroups-help]"
    "Show this help message")
  "List of commands and their help messages. Used by `workgroups-help'.")

(defun workgroups-help ()
  "Show the workgroups commands help buffer."
  (interactive)
  (let ((hline (concat (make-string 80 ?-) "\n")))
    (with-output-to-temp-buffer "*workroups help*"
      (princ  "Workgroups For Windows keybindings:\n")
      (princ hline)
      (dolist (elt (workgroups-partition workgroups-help 2))
        (princ (format "%15s  |  %s\n"
                       (substitute-command-keys (car elt))
                       (cadr elt))))
      (princ hline)
      (help-print-return-message))))


;;; mode definition

(defun workgroups-query-for-save ()
  "Query for save when `workgroups-dirty' is non-nil."
  (and workgroups-dirty
       (y-or-n-p "Workgroups have been modified. Save them? ")
       (call-interactively 'workgroups-save)))

(defun workgroups-query-hook ()
  "Query for save on exit if `workgroups-dirty' is non-nil."
  (when workgroups-query-save-on-emacs-exit
    (workgroups-query-for-save))
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
         (when workgroups-query-save-on-mode-exit
           (workgroups-query-for-save))
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
