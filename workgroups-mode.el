;;; workgroups-mode.el --- workgroups for windows

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:      workgroups-mode.el
;; Author:    tlh <thunkout@gmail.com>
;; Created:   2010-07-22
;; Version:   0.1.0
;; Keywords:  window persistence window-configuration

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
;;   workgroups-mode.el is a window configuration persistence minor
;;   mode for GNU Emacs.  It allows you to persist window
;;   configurations, called "workgroups" because it's shorter and
;;   funnier, between sessions.  workgroups-mode saves the window
;;   layout of the current frame, as well as each window's buffer's
;;   filename if it's visiting a file, or its buffername otherwise.
;;   And that's it. It doesn't try to save complicated information
;;   about the buffer, like major or minor modes.  If you save
;;   workgroups that include things like erc or gnus buffers, you
;;   should launch those applications and buffers again in your next
;;   session before restoring the workgroup that includes
;;   them. Nothing bad will happen otherwise, of course.
;;   workgroups-mode will just default to a buffer that already
;;   exists, like *scratch*.
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
;;  - frame support. Need to update mode-line for this, too.
;;  - add minibuffer state to configs
;;  - generate random window configs?
;;


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

(defcustom workgroups-default-buffer "*scratch*"
  "Buffer name used in `workgroups-create', and other functions."
  :type 'string
  :group 'workgroups)

(defcustom workgroups-restore-position t
  "Toggles whether switching to a workgroup restores a frame's
position."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-restore-size t
  "Toggles whether switching to a workgroup restores a frame's
size."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-use-faces t
  "Toggles face usage in various displays."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-switch-hook nil
  "Hook run whenever a workgroup is switched to."
  :type 'hook
  :group 'workgroups)

(defcustom workgroups-switch-on-load t
  "Non-nil means automatically switch to the first workgroup when
a file is loaded."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-query-save-on-exit t
  "When non-nil, offer to save `workgroups-list' on exit if
`workgroups-dirty' in non-nil."
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
  "Toggles inclusion of battery info in time display."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-frame-wipe-on t
  "Toggles frame wipe animation on workgroups switch."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-frame-wipe-horizontal-factor 25
  "Windows are enlarged horizontally by this numbers of columns
during frame wiping."
  :type 'integer
  :group 'workgroups)

(defcustom workgroups-frame-wipe-vertical-factor 8
  "Windows are enlarged vertically by this numbers of rows during
frame wiping."
  :type 'integer
  :group 'workgroups)

(defcustom workgroups-frame-wipe-speed 0.001
  "Number of seconds to sit between window enlargement calls
during frame wipe."
  :type 'integer
  :group 'workgroups)


;;; faces

(defface workgroups-operation-face
  '((((class color)) (:foreground "aquamarine")))
  "Face used for operation names."
  :group 'workgroups)

(defface workgroups-name-face
  '((((class color)) (:foreground "light sky blue")))
  "Face used for names."
  :group 'workgroups)

(defface workgroups-message-face
  '((((class color)) (:foreground "pale turquoise")))
  "Face used for messages."
  :group 'workgroups)

(defface workgroups-divider-face
  '((((class color)) (:foreground "light slate blue")))
  "Face used for dividers."
  :group 'workgroups)

(defvar workgroups-face-abbrevs
  '((op   . workgroups-operation-face)
    (name . workgroups-name-face)
    (msg  . workgroups-message-face)
    (div  . workgroups-divider-face))
  "Assoc list mapping face abbreviations to face names.")


;;; variables

(defvar workgroups-file nil
  "Current workgroups file.")

(defvar workgroups-dirty nil
  "Non-nil when there are unsaved changes.")

(defvar workgroups-list nil
  "Current list of workgroups.")

(defvar workgroups-frame-table (make-hash-table)
  "Hash table storing global state for each frame.")

(defvar workgroups-kill-ring nil
  "List of saved configs.")

(defvar workgroups-kill-ring-pos 0
  "Position in `workgroups-kill-ring' during sequential yanks.")

(defvar workgroups-mode-line-on t
  "Toggles workgroups' mode-line display.")

;; FIXME: frame-specific
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

(defmacro workgroups-aif (test then &rest else)
  "Anaphoric if."
  (declare (indent defun))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro workgroups-awhen (test &rest body)
  "Anaphoric when."
  (declare (indent defun))
  `(workgroups-aif ,test (progn ,@body)))

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

(defun workgroups-linsert (elt n lst)
  "Insert ELT into LST at N."
  (append (workgroups-take lst n) (cons elt (nthcdr n lst))))

(defun workgroups-access (key alst)
  "Return the value portion from associng KEY in ALST."
  (cdr (assoc key alst)))

(defun workgroups-aput (key val alst)
  "Set KEY to VAL in ALST."
  (cons (cons key val) (remove (assoc key alst) alst)))

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
       0 (length str) 'face (assoc face workgroups-face-abbrevs)
       str)
      str)))

(defun workgroups-window-list (&optional frame)
  "Flatten `window-tree' into a stable list.
`window-list' can't be used because its order isn't stable."
  (flet ((inner (obj) (if (atom obj) (list obj)
                        (mapcan 'inner (cddr obj)))))
    (inner (car (window-tree (or frame (selected-frame)))))))


;;; global getters

(defun workgroups-file (&optional noerror)
  "Return `workgroups-file'."
  (or workgroups-file
      (unless noerror
        (error "`workgroups-file' is unset."))))

(defun workgroups-list (&optional noerror)
  "Return `workgroups-list'."
  (or workgroups-list
      (unless noerror
        (error "No workgroups are defined."))))

(defun workgroups-get-workgroup (key val &optional noerror)
  "Return the workgroup whose KEY equals VAL."
  (or (workgroups-aassoc key val (workgroups-list noerror))
      (unless noerror
        (error "There is no workgroup with an %S of %S" key val))))


;;; frame state ops

(defmacro workgroups-with-frame-state (frame state &rest body)
  "Bind FRAME's state to STATE from `workgroups-frame-table'."
  (declare (indent defun))
  `(let* ((,frame (or ,frame (selected-frame)))
          (,state (or (gethash ,frame workgroups-frame-table)
                      (puthash ,frame nil workgroups-frame-table))))
     ,@body))

(defun workgroups-get-frame-val (key &optional frame noerror)
  (workgroups-with-frame-state frame state
    (or (workgroups-access key state)
        (unless noerror
          (error "Key %s is nil in frame" key)))))

(defun workgroups-set-frame-val (key val frame)
  "Set KEY to VAL in FRAME's entry in `workgroups-frame-table'."
  (workgroups-with-frame-state frame state
    (aif (assoc key state)
         (setcdr it val)
         (push (cons key val)
               (gethash frame workgroups-frame-table)))))

(defun workgroups-del-frame-val (key frame)
  "Remove KEY's key-value-pair in FRAME's state."
  (workgroups-with-frame-state frame state
    (puthash frame (remove (assoc key state) state)
             workgroups-frame-table)))


;;; current and previous workgroups

(defun workgroups-current (&optional frame noerror)
  "Return `workgroups-current'."
  (workgroups-get-workgroup
   :id (workgroups-get-frame-val :current frame noerror) noerror))

;; (workgroups-current)
;; (workgroups-current)

(defun workgroups-set-current (val &optional frame)
  "Set the :current key to VAL in FRAME's entry in
`workgroups-frame-table'."
  (workgroups-set-frame-val :current (workgroups-access :id val) frame))

(defun workgroups-previous (&optional frame noerror)
  "Return `workgroups-previous'."
  (workgroups-get-workgroup
   :id (workgroups-get-frame-val :previous frame noerror) noerror))

(defun workgroups-set-previous (val &optional frame)
  "Set the :previous key to VAL in FRAME's entry in
`workgroups-frame-table'."
  (workgroups-set-frame-val :previous (workgroups-access :id val) frame))


;;; workgroup property ops

(defun workgroups-id (workgroup)
  "Return WORKGROUP's id."
  (workgroups-access :id workgroup))

(defun workgroups-set-id (workgroup id)
  "Set the id of WORKGROUP to ID."
  (setcdr (assoc :id workgroup) id)
  (setq workgroups-dirty t))

(defun workgroups-name (workgroup &optional face)
  "Return the name of WORKGROUP."
  (let ((name (workgroups-access :name workgroup)))
    (if (not face) name
      (workgroups-facify 'name name))))

(defun workgroups-set-name (workgroup name)
  "Set the name of WORKGROUP to NAME."
  (setcdr (assoc :name workgroup) name)
  (setq workgroups-dirty t))

(defun workgroups-names (&optional noerror)
  "Return list of workgroups names."
  (mapcar 'workgroups-name (workgroups-list noerror)))

(defun workgroups-base-config (workgroup)
  "Return the base config of WORKGROUP."
  (workgroups-access :config workgroup))

(defun workgroups-set-base-config (workgroup config)
  "Set the base config of WORKGROUP to CONFIG."
  (setcdr (assoc :config workgroup) config)
  (setq workgroups-dirty t))

(defun workgroups-working-config (workgroup &optional frame noerror)
  "Return the working config of WORKGROUP in FRAME."
  (workgroups-get-frame-val
   (workgroups-access :id workgroup) frame noerror))

(defun workgroups-set-working-config (workgroup config &optional frame)
  "Set the working config of WORKGROUP to CONFIG in FRAME."
  (workgroups-set-frame-val (workgroups-access :id workgroup) config frame))


;;; workgroups-list ops

(defun workgroups-delete (wg)
  "Remove WORKGROUP from `workgroups-list'."
  (workgroups-dohash (frame state workgroups-frame-table)
    (workgroups-del-frame-val (workgroups-access :id wg) frame)
    (when (eq wg (workgroups-current frame noerror))
      (workgroups-set-current nil frame))
    (when (eq wg (workgroups-previous frame noerror))
      (workgroups-set-previous nil frame)))
  (setq workgroups-list (remove wg (workgroups-list)))
  (setq workgroups-dirty t))

;; (defun workgroups-add (workgroup &optional pos)
;;   "Add workgroup NEW.
;; Overwrites a workgroup with the same name, if it exists."
;;   (let ((name (workgroups-name new)) (wl (workgroups-list t)))
;;     (workgroups-awhen (workgroups-get-workgroup :name name t)
;;       (or (y-or-n-p (format "%S exists. Overwrite? " name))
;;           (error "Cancelled"))
;;       (or pos (setq pos (position it wl)))
;;       (workgroups-delete it))
;;     (setq workgroups-dirty t)
;;     (setq workgroups-list
;;           (workgroups-linsert
;;            workgroup (or pos (length wl)) wl))))

(defun workgroups-unique-id ()
  "Return an unused id number."
  (let ((wl (workgroups-list t)) (i -1))
    (catch 'res
      (while t
        (unless (workgroups-aassoc :id (incf i) wl)
          (throw 'res i))))))

(defun workgroups-add (workgroup &optional pos)
  "Add workgroup NEW.
Overwrites a workgroup with the same name, if it exists."
  (let ((name (workgroups-name new)) (wl (workgroups-list t)))
    (workgroups-awhen (workgroups-get-workgroup :name name t)
      (or (y-or-n-p (format "%S exists. Overwrite? " name))
          (error "Cancelled"))
      (or pos (setq pos (position it wl)))
      (workgroups-delete it))
    (setq workgroups-dirty t)
    (setq workgroups-list
          (workgroups-linsert
           (workgroups-aput :id (workgroups-unique-id) workgroup)
           (or pos (length wl)) wl))))

(defun workgroups-list-swap (w1 w2)
  "Swap W1 and W2 in `workgroups-list'."
  (when (eq w1 w2) (error "Can't swap a workgroup with itself"))
  (workgroups-aif (workgroups-util-swap w1 w2 (workgroups-list))
    (setq workgroups-list it workgroups-dirty t)
    (error "W1 and W2 aren't both present in `workgroups-list'.")))


;;; config making

(defun workgroups-wprop (key window)
  "Return value of KEY in workgroups WINDOW, or nil."
  (workgroups-access key (cdr window)))

(defun workgroups-make-window (winobj)
  "Make a workgroups window object from WINOBJ.
WINOBJ is an Emacs window object."
  (with-current-buffer (window-buffer winobj)
    (let ((p (point)) (edges (window-edges winobj)))
      `(:window
        (:fname   .  ,(buffer-file-name))
        (:bname   .  ,(buffer-name))
        (:width   .  ,(- (nth 2 edges) (nth 0 edges)))
        (:height  .  ,(window-height winobj))
        (:point   .  ,(if (eq p (point-max)) :max p))
        (:wstart  .  ,(window-start winobj))))))

(defun workgroups-make-current-config (&optional frame)
  "Make a window config from FRAME or `selected-frame'."
  (let ((f (or frame (selected-frame))))
    (flet ((inner (obj) (etypecase obj
                          (window (workgroups-make-window obj))
                          (cons `(,@(workgroups-take obj 2)
                                  ,@(mapcar 'inner (cddr obj)))))))
      `(,(mapcar (lambda (p) (frame-parameter f p)) '(left top width height))
        ,(position (selected-window) (workgroups-window-list f))
        ,(inner (car (window-tree f)))))))

(defun workgroups-make-default-config (&optional buffer)
  "Return a new default config."
  (save-window-excursion
    (delete-other-windows)
    (switch-to-buffer (or buffer workgroups-default-buffer))
    (workgroups-make-current-config)))


;;; workgroup making

(defun workgroups-make-workgroup (name config)
  "Make a workgroup named NAME from BASE and WORKING."
  `((:name . ,name) (:config . ,config) (:id . nil)))

(defun workgroups-make-default (name &optional buffer)
  "Return a new default workgroup named NAME."
  (workgroups-make-workgroup
   name (workgroups-make-default-config buffer)))

(defun workgroups-copy-workgroup (workgroup &optional name)
  "Return a copy of WORKGROUP, optionally named NAME."
  `((:id     ,(workgroups-access :id workgroup))
    (:name   ,(or name (workgroups-access :name workgroup)))
    (:config ,(workgroups-access :config workgroup))))


;;; window ops

(defun workgroups-window-p (window)
  "t if WINDOW is a workgroups window, nil otherwise."
  (and (consp window) (eq :window (car window))))

(defun workgroups-window-width (window)
  "Return the width of WINDOW."
  (if (workgroups-window-p window)
      (workgroups-wprop :width window)
    (let ((coords (cadr window)))
      (- (nth 2 coords) (nth 0 coords)))))

(defun workgroups-window-height (window)
  "Return the height of WINDOW."
  (if (workgroups-window-p window)
      (workgroups-wprop :height window)
    (let ((coords (cadr window)))
      (- (nth 3 coords) (nth 1 coords)))))


;;; workgroup restoring

(defun workgroups-restore-window (window)
  "Restore WINDOW's state in `selected-window'."
  (let ((fname  (workgroups-wprop :fname  window))
        (bname  (workgroups-wprop :bname  window))
        (point  (workgroups-wprop :point  window))
        (wstart (workgroups-wprop :wstart window)))
    (and fname (file-exists-p fname) (find-file fname))
    (and bname (get-buffer bname) (switch-to-buffer bname))
    (set-window-start (selected-window) wstart t)
    (goto-char (if (eq point :max) (point-max) point))
    (when (>= wstart (point-max)) (recenter))))

(defun workgroups-restore-config (config &optional frame)
  "Restore CONFIG in FRAME or `selected-frame'."
  (flet ((inner (obj)
                (cond ((workgroups-window-p obj)
                       (workgroups-restore-window obj)
                       (other-window 1))
                      (t (dolist (win (cddr obj))
                           (unless (eq win (car (last obj)))
                             (if (car obj)
                                 (split-window-vertically
                                  (workgroups-window-height win))
                               (split-window-horizontally
                                (workgroups-window-width win))))
                           (inner win))))))
    (let ((f (or frame (selected-frame))))
      (destructuring-bind ((x y w h) idx wtree) config
        (when workgroups-restore-position (set-frame-position f x y))
        (when workgroups-restore-size
          (set-frame-width f w)
          (set-frame-height f h))
        (delete-other-windows)
        (inner wtree)
        (set-frame-selected-window
         f (nth idx (workgroups-window-list f)))))))

;; (defun workgroups-restore (workgroup &optional base frame)
;;   "Restore WORKGROUP in FRAME or `selected-frame'."
;;   (workgroups-restore-config
;;    (if base (workgroups-base-config workgroup)
;;      (workgroups-working-config workgroup frame))
;;    frame))

(defun workgroups-restore (workgroup &optional base frame)
  "Restore WORKGROUP in FRAME or `selected-frame'."
  (workgroups-restore-config
   (or (and (not base) (workgroups-working-config workgroup frame t))
       (workgroups-base-config workgroup))
   frame))

(defun workgroups-restore-default-config (&optional frame)
  "Restore the default config in FRAME or `selected-frame'."
  (workgroups-restore-config (workgroups-make-default-config) frame))


;;; buffer lists

(defun workgroups-config-buffer-list (config)
  "Return the names of all unique buffers in CONFIG."
  (let (bufs)
    (flet ((inner (obj)
                  (if (not (workgroups-window-p obj))
                      (mapc 'inner (cddr obj))
                    (workgroups-awhen (workgroups-wprop :bname obj)
                      (or (member it bufs) (push it bufs))))))
      (inner config)
      bufs)))

(defun workgroups-workgroup-buffer-list (workgroup &optional frame)
  "Return the names of all unique buffers in WORKGROUP."
  (let ((bufs (workgroups-config-buffer-list
               (workgroups-working-config workgroup frame))))
    (dolist (buf (workgroups-config-buffer-list
                  (workgroups-base-config workgroup)) bufs)
      (unless (member buf bufs) (push buf bufs)))))

(defun workgroups-buffer-list (&optional frame)
  "Return the names of all unique buffers in `workgroups-list'."
  (let (bufs)
    (dolist (w (workgroups-list) (nreverse bufs))
      (dolist (buf (workgroups-workgroup-buffer-list w frame))
        (unless (member buf bufs) (push buf bufs))))))

(defun workgroups-find-buffer (buf &optional frame)
  "Return the workgroup and config flag that contains BUF."
  (catch 'result
    (dolist (w (workgroups-list))
      (cond ((member buf (workgroups-config-buffer-list
                          (workgroups-working-config w frame)))
             (throw 'result (list w nil)))
            ((member buf (workgroups-config-buffer-list
                          (workgroups-base-config w)))
             (throw 'result (list w t)))))))


;;; mode-line

(defun workgroups-mode-line-update (frame)
  "Update the mode-line with current workgroup info."
  (setq workgroups-mode-line-string
        (format "[%s]" (workgroups-awhen (workgroups-current frame t)
                         (workgroups-name it))))
  (force-mode-line-update))

(defun workgroups-mode-line-add ()
  "Turn on workgroups' mode-line display."
  (let ((format mode-line-format)
        (on 'workgroups-mode-line-on))
    (unless (assoc on format)
      (set-default 'mode-line-format
                   (workgroups-linsert
                    `(,on (" " workgroups-mode-line-string))
                    (1+ (position 'mode-line-position format))
                    format))
      (force-mode-line-update))))

(defun workgroups-mode-line-remove ()
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
  (workgroups-get-workgroup
   :name (workgroups-completing-read
          "Workgroup: " (workgroups-names noerror)) noerror))

(defun workgroups-read-name (&optional prompt)
  "Read a non-empty name from the minibuffer."
  (let ((prompt (or prompt "Name: "))
        (warning (workgroups-facify 'msg "Name must be non-empty"))
        (name nil))
    (while (and (setq name (read-from-minibuffer prompt))
                (equal name ""))
      (message warning)
      (sit-for workgroups-warning-timeout))
    name))

(defun workgroups-read-idx (&optional prompt)
  "Read and return a valid workgroup index."
  (let* ((l (1- (length (workgroups-list))))
         (prompt (or prompt (format "Workgroup index [0-%d]: " l)))
         (warning (workgroups-facify
                   'msg (format "Please enter an integer [0-%d]" l)))
         (i nil))
    (while (and (setq i (read-from-minibuffer prompt nil nil t))
                (or (not (integerp i)) (< i 0) (>= i l)))
      (message warning)
      (sit-for workgroups-warning-timeout))
    i))

(defun workgroups-read-buffer ()
  "Read and return a buffer from `workgroups-buffer-list'."
  (workgroups-completing-read
   "Buffer: " (workgroups-buffer-list)))


;;; command utils

(defun workgroups-current-updated (&optional frame noerror)
  "Return current workgroup after updating its working config."
  (workgroups-awhen (workgroups-current frame noerror)
    (workgroups-set-working-config
     it (workgroups-make-current-config))
    it))

(defun workgroups-arg (&optional reverse frame noerror)
  "Return a workgroup.  For use in interactive forms.
If `current-prefix-arg' is nil return the current workgroups.
Otherwise read a workgroup from the minibuffer.  If REVERSE is
non-nil, `current-prefix-arg''s begavior is reversed."
  (if (if reverse (not current-prefix-arg) current-prefix-arg)
      (workgroups-read-workgroup noerror)
    (workgroups-current-updated frame noerror)))

(defun workgroups-add-to-kill-ring (config)
  "Add CONFIG to `workgroups-kill-ring'."
  (push config workgroups-kill-ring)
  (setq workgroups-kill-ring
        (workgroups-take workgroups-kill-ring
                         workgroups-kill-ring-size)))

(defun workgroups-list-string ()
  "Return a string of the names of all workgroups."
  (let ((cur (workgroups-current))
        (div (workgroups-facify 'div workgroups-divider-str)))
    (format "%s %s %s"
            (workgroups-facify 'div "(")
            (mapconcat (lambda (w) (workgroups-name w (eq w cur)))
                       (workgroups-list) (concat " " div " "))
            (workgroups-facify 'div ")"))))


;;; frame-wipe

(defun workgroups-frame-wipe (&optional window)
  "Frame-wipe animation."
  (when window (set-frame-selected-window window))
  (while (> (length (window-list)) 1)
    (enlarge-window-horizontally
     workgroups-frame-wipe-horizontal-factor)
    (enlarge-window
     workgroups-frame-wipe-vertical-factor)
    (sit-for workgroups-frame-wipe-speed)))


;;; commands

(defun workgroups-switch (workgroup &optional base frame)
  "Switch to WORKGROUP.
If BASE is non-nil, restore WORKGROUP's base config.  Otherwise
restore its working config."
  (interactive (list (workgroups-read-workgroup) current-prefix-arg))
  (let ((current (workgroups-current-updated frame t)))
    (when (eq workgroup current)
      (error "Already on %S" (workgroups-name workgroup)))
    (workgroups-set-previous current)
    (workgroups-set-current  workgroup)
    (when workgroups-frame-wipe-on (workgroups-frame-wipe))
    (workgroups-restore workgroup base)
    (workgroups-mode-line-update frame)
    (run-hooks 'workgroups-switch-hook)
    (message "%s %s" (workgroups-facify 'op "Switched to:")
             (workgroups-name workgroup t))))

(defun workgroups-add-and-switch (workgroup)
  "Add WORKGROUP to `workgroups-list' and swtich to it."
  (workgroups-add workgroup)
  (workgroups-switch workgroup))

(defun workgroups-create (name)
  "Create and add a workgroup named NAME."
  (interactive (list (workgroups-read-name)))
  (let ((new (workgroups-make-default name)))
    (workgroups-add-and-switch new)
    (message "%s %s" (workgroups-facify 'op "Created:")
             (workgroups-name new t))))

(defun workgroups-clone (workgroup name)
  "Create and add a clone of WORKGROUP named NAME."
  (interactive (list (workgroups-arg) (workgroups-read-name)))
  (let ((new (workgroups-copy-workgroup workgroup name)))
    (workgroups-add-and-switch new)
    (message "%s %s to %s" (workgroups-facify 'op "Cloned:")
             (workgroups-name workgroup t)
             (workgroups-name new t))))

(defun workgroups-kill (workgroup)
  "Kill workgroup named NAME.
The working config of WORKGROUP is saved to
`workgroups-kill-ring'."
  (interactive (list (workgroups-arg)))
  (workgroups-add-to-kill-ring (workgroups-working-config workgroup))
  (let ((next (workgroups-cnext workgroup (workgroups-list))))
    (workgroups-delete workgroup)
    (if (eq next workgroup)
        (workgroups-restore-default-config)
      (workgroups-switch next))
    (message "%s %s" (workgroups-facify 'op "Killed:")
             (workgroups-name workgroup t))))

(defun workgroups-kill-ring-save (workgroup)
  "Save WORKGROUP's working config to `workgroups-kill-ring'."
  (interactive (list (workgroups-arg)))
  (workgroups-add-to-kill-ring (workgroups-working-config workgroup))
  (message "%s %s" (workgroups-facify 'op "Saved:")
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
      (workgroups-restore-config (nth pos it))
      (message "%s %s" (workgroups-facify 'op "Yanked:")
               (workgroups-facify 'msg (format "%d" pos))))
    (error "Workgroups' kill-ring is empty")))

(defun workgroups-kill-workgroup-and-buffers (workgroup)
  "Kill WORKGROUP and the buffers in its working config."
  (interactive (list (workgroups-arg)))
  (let ((bufs (save-window-excursion
                (workgroups-restore workgroup)
                (mapcar 'window-buffer (window-list)))))
    (workgroups-kill workgroup)
    (mapc 'kill-buffer bufs)
    (message "%s %s and its buffers"
             (workgroups-facify 'op "Killed:")
             (workgroups-name workgroup t))))

(defun workgroups-delete-other-workgroups (workgroup)
  "Delete all but the WORKGROUP."
  (interactive (list (workgroups-arg)))
  (let ((cur (workgroups-current)))
    (mapc 'workgroups-delete (remove workgroup (workgroups-list)))
    (unless (eq workgroup cur) (workgroups-switch workgroup))
    (message "%s All workgroups but %s" (workgroups-facify 'op "Deleted:")
             (workgroups-name workgroup t))))

(defun workgroups-update (workgroup)
  "Set the base config of WORKGROUP to its current config."
  (interactive (list (workgroups-arg)))
  (workgroups-set-base-config
   workgroup (workgroups-working-config workgroup))
  (message "%s %s" (workgroups-facify 'op "Updated:")
           (workgroups-name workgroup t)))

(defun workgroups-revert (workgroup)
  "Set the working config of WORKGROUP to its base config."
  (interactive (list (workgroups-arg)))
  (workgroups-set-working-config
   workgroup (workgroups-base-config workgroup))
  (when (eq workgroup (workgroups-current))
    (workgroups-restore workgroup t))
  (message "%s %s" (workgroups-facify 'op "Reverted:")
           (workgroups-name workgroup t)))

(defun workgroups-jump (n)
  "Switch to the Nth workgroup in `workgroups-list'."
  (interactive (list (or current-prefix-arg (workgroups-read-idx))))
  (let ((wl (workgroups-list)))
    (workgroups-switch
     (or (nth n wl)
         (error "There are only %d workgroups" (length wl))))))

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

(defun workgroups-prev (workgroup)
  "Switch to the workgroup before WORKGROUP in `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (workgroups-switch (workgroups-cprev workgroup (workgroups-list))))

(defun workgroups-next (workgroup)
  "Switch to the workgroup after WORKGROUP in `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (workgroups-switch (workgroups-cnext workgroup (workgroups-list))))

(defun workgroups-toggle ()
  "Switch to the previous workgroup."
  (interactive)
  (workgroups-switch (workgroups-previous)))

(defun workgroups-swap ()
  "Swap the current workgroup with the previous."
  (interactive)
  (let ((cur (workgroups-current)) (prev (workgroups-previous)))
    (workgroups-list-swap cur prev)
    (message "%s %s with %s  -  %s" (workgroups-facify 'op "Swapped")
             (workgroups-name cur t)
             (workgroups-name prev t)
             (workgroups-list-string))))

(defun workgroups-transpose-left (workgroup)
  "Swap WORKGROUP toward the beginning of `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (workgroups-aif (workgroups-lprev workgroup (workgroups-list))
    (progn (workgroups-list-swap workgroup it)
           (message "%s %s" (workgroups-facify 'op "Transposed:")
                    (workgroups-list-string)))
    (error "Can't transpose %s any further."
           (workgroups-name workgroup))))

(defun workgroups-transpose-right (workgroup)
  "Move WORKGROUP toward the end of `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (workgroups-aif (workgroups-lnext workgroup (workgroups-list))
    (progn (workgroups-list-swap workgroup it)
           (message "%s %s" (workgroups-facify 'op "Transposed:")
                    (workgroups-list-string)))
    (error "Can't transpose %s any further."
           (workgroups-name workgroup))))

(defun workgroups-rename (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list (workgroups-arg) (workgroups-read-name "New name: ")))
  (let ((oldname (workgroups-name workgroup )))
    (workgroups-set-name workgroup newname)
    (message "%s %s to %s" (workgroups-facify 'op "Renamed:") oldname
             (workgroups-name workgroup t))))


;;; file commands

(defun workgroups-save (file)
  "Save workgroups to FILE.
Called interactively with a prefix arg, or if `workgroups-file'
is nil, read a filename.  Otherwise use `workgroups-file'."
  (interactive
   (list (if (or current-prefix-arg (not (workgroups-file t)))
             (read-file-name "File: ") (workgroups-file))))
  (workgroups-current-updated t)
  (workgroups-save-sexp-to-file
   (cons workgroups-fid (workgroups-list)) file)
  (setq workgroups-dirty nil workgroups-file file)
  (message "%s %s" (workgroups-facify 'op "Wrote:")
           (workgroups-facify 'name file)))

(defun workgroups-load (file)
  "Load workgroups from FILE.
Called interactively with a prefix arg, and if `workgroups-file'
is non-nil, use `workgroups-file'. Otherwise read a filename."
  (interactive
   (list (if (and current-prefix-arg (workgroups-file t))
             (workgroups-file) (read-file-name "File: "))))
  (let ((sexp (workgroups-read-sexp-from-file file)))
    (unless (eq workgroups-fid (car sexp))
      (error "%S is not a workgroups file."))
    (clrhash workgroups-frame-table)
    (setq workgroups-list   (cdr sexp)
          workgroups-file   file
          workgroups-dirty  nil))
  (workgroups-awhen (workgroups-list t)
    (and workgroups-switch-on-load
         (workgroups-switch (car it))))
  (message "%s %s" (workgroups-facify 'op "Loaded:")
           (workgroups-facify 'name file)))

(defun workgroups-find-file (file)
  "Create a new workgroup and find file FILE in it."
  (interactive "FFile: ")
  (workgroups-create (file-name-nondirectory file))
  (find-file file))

(defun workgroups-find-file-read-only (file)
  "Create a new workgroup and find-file-read-only FILE in it."
  (interactive "FFile: ")
  (workgroups-create (file-name-nondirectory file))
  (find-file-read-only file))

(defun workgroups-get-by-buffer (buf)
  "Switch to the workgroup that contains BUF."
  (interactive (list (workgroups-read-buffer)))
  (workgroups-aif (workgroups-find-buffer buf)
    (apply 'workgroups-switch it)
    (error "No workgroup contains %S" buf)))

(defun workgroups-dired (dirname &optional switches)
  "Create a new workgroup and dired with DIRNAME and SWITCHES."
  (interactive (list (read-directory-name "Dired: ")
                     current-prefix-arg))
  (workgroups-create dirname)
  (dired dirname switches))


;;; misc commands

(defun workgroups-echo-current ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (message "%s %s" (workgroups-facify 'op "Current:")
           (workgroups-name (workgroups-current) t)))

(defun workgroups-echo-all ()
  "Display the names of all workgroups in the echo area."
  (interactive)
  (message "%s %s" (workgroups-facify 'op "Workgroups:")
           (workgroups-list-string)))

(defun workgroups-toggle-mode-line ()
  "Toggle workgroups' mode-line display."
  (interactive)
  (setq workgroups-mode-line-on (not workgroups-mode-line-on))
  (force-mode-line-update)
  (message "%s %s" (workgroups-facify 'op "mode-line:")
           (workgroups-facify
            'name (if workgroups-mode-line-on "on" "off"))))

(defun workgroups-echo-time ()
  "Echo the current time."
  (interactive)
  (let ((op (workgroups-facify 'op "Current time:"))
        (time (workgroups-facify
               'name (format-time-string workgroups-time-format))))
    (if (and workgroups-display-battery (fboundp 'battery))
        (message "%s %s\n%s %s" op time
                 (workgroups-facify 'op "Battery:")
                 (workgroups-facify 'name (battery)))
      (message "%s %s" op time))))

(defun workgroups-version ()
  "Echo the current version number."
  (interactive)
  (message "%s %s" (workgroups-facify 'op "Workgroups version:")
           (workgroups-facify 'name workgroups-version)))


;;; keymap

(defvar workgroups-map
  (workgroups-fill-keymap
   (make-sparse-keymap)
   "C-c"        'workgroups-create
   "c"          'workgroups-create
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
   "C-p"        'workgroups-prev
   "p"          'workgroups-prev
   "C-n"        'workgroups-next
   "n"          'workgroups-next
   "C-a"        'workgroups-toggle
   "a"          'workgroups-toggle
   "C-v"        'workgroups-switch
   "v"          'workgroups-switch
   "'"          'workgroups-switch
   "C-'"        'workgroups-switch
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
   "C-j"        'workgroups-jump
   "C-x"        'workgroups-swap
   "C-,"        'workgroups-transpose-left
   "C-."        'workgroups-transpose-right
   "C-e"        'workgroups-echo-all
   "e"          'workgroups-echo-all
   "S-C-e"      'workgroups-echo-current
   "E"          'workgroups-echo-current
   "C-b"        'workgroups-get-by-buffer
   "b"          'workgroups-get-by-buffer
   "C-f"        'workgroups-find-file
   "S-C-f"      'workgroups-find-file-read-only
   "d"          'workgroups-dired
   "C-i"        'workgroups-toggle-mode-line
   "C-t"        'workgroups-echo-time
   "t"          'workgroups-echo-time
   "A"          'workgroups-rename
   "C-s"        'workgroups-save
   "C-l"        'workgroups-load
   "V"          'workgroups-version
   "?"          'workgroups-help)
  "workgroups-mode's keymap.")


;;; help

(defvar workgroups-help "workgroups keybindings:\n
  \\[workgroups-create]    Create a new workgroup and switch to it
  \\[workgroups-switch]    Switch to a workgroup
  \\[workgroups-get-by-buffer]    Switch to the workgroup and config which contains the specified buffer
  \\[workgroups-clone]    Clone the current workgroup
  \\[workgroups-kill]    Kill a workgroup
  \\[workgroups-kill-workgroup-and-buffers]    Kill a workgroup and its buffer
  \\[workgroups-delete-other-workgroups]    Delete all but the specified workgroup
  \\[workgroups-yank]    Restore a killed config to the current workgroup
  \\[workgroups-kill-ring-save]    Save the current config to the kill ring
  \\[workgroups-save]    Save workgroups to a file
  \\[workgroups-load]    Load workgroups from a file
  \\[workgroups-find-file]    Create a new workgroup and find a file in it
  \\[workgroups-jump]    Jump to a workgroup by number
  \\[workgroups-jump-0]    Switch to the 0th workgroup
  \\[workgroups-jump-1]    Switch to the 1th workgroup
  \\[workgroups-jump-2]    Switch to the 2th workgroup
  \\[workgroups-jump-3]    Switch to the 3th workgroup
  \\[workgroups-jump-4]    Switch to the 4th workgroup
  \\[workgroups-jump-5]    Switch to the 5th workgroup
  \\[workgroups-jump-6]    Switch to the 6th workgroup
  \\[workgroups-jump-7]    Switch to the 7th workgroup
  \\[workgroups-jump-8]    Switch to the 8th workgroup
  \\[workgroups-jump-9]    Switch to the 9th workgroup
  \\[workgroups-revert]    Revert to a workgroup's base config
  \\[workgroups-update]    Set a workgroups base config to its working config
  \\[workgroups-toggle]    Switch to the previously selected workgroup
  \\[workgroups-next]    Switch to the next workgroup circularly
  \\[workgroups-prev]    Switch to the previous workgroup circularly
  \\[workgroups-swap]    Swap the current and previous workgroups
  \\[workgroups-transpose-left]    Move a workgroup leftward in workgroups-list
  \\[workgroups-transpose-right]    Move a workgroup rightward in workgroups-list
  \\[workgroups-rename]    Rename a workgroup
  \\[workgroups-echo-current]    Display the name of the current workgroup in the echo area
  \\[workgroups-echo-all]    Display the names of all workgroups in the echo area
  \\[workgroups-toggle-mode-line]    Toggle workgroups mode-line display
  \\[workgroups-help]    Show this help message"
  "Help shown by elscreen-help-mode")

(defun workgroups-help ()
  "Show information about workgroups commands."
  (interactive)
  (with-output-to-temp-buffer "*workroups help*"
    (princ (substitute-command-keys workgroups-help))
    (print-help-return-message)))


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
         (workgroups-mode-line-add)
         (setq workgroups-mode t))
        (t
         (remove-hook 'kill-emacs-query-functions 'workgroups-query-hook)
         (workgroups-mode-line-remove)
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
