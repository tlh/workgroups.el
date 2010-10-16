;;; workgroups-mode.el --- workgroups for windows

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:      workgroups-mode.el
;; Author:    tlh <thunkout@gmail.com>
;; Created:   2010-07-22
;; Version:   1.0
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
;;   If `workgroups-switch-on-load' is non-nil, the first workgroup in a
;;   file will automatically be switched to when the file is loaded:
;;
;;     (setq workgroups-switch-on-load t)
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
;;  - Multi-frame support. Need to update mode-line for this, too.
;;
;;  - generate random window configs?
;;


;;; Code:

(defconst workgroups-version "1.0")

(require 'cl)


;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- Emacs session manager"
  :group 'convenience
  :version "1.0")

(defcustom workgroups-default-file nil
  "File to load automatically when `workgroups-mode' is enabled.
If you want this to be loaded at emacs startup, make sure to set
it before calling `workgroups-mode'."
  :type 'file
  :group 'workgroups)

(defcustom workgroups-default-buffer "*scratch*"
  "Name of the default buffer used in `workgroups-create', and
other functions."
  :type 'string
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


;;; non-customizable variables

(defvar workgroups-fid '-*-workgroups-*-
  "Symbol identifying a workgroups file.")

(defvar workgroups-file nil
  "Current workgroups file.")

(defvar workgroups-dirty nil
  "Non-nil when there are unsaved changes.")

(defvar workgroups-list nil
  "Current list of workgroups.")

(defvar workgroups-current nil
  "Current workgroup.")

(defvar workgroups-previous nil
  "Previous workgroup.")

(defvar workgroups-kill-ring nil
  "List of killed configs.")

(defvar workgroups-kill-ring-pos 0
  "Position in `workgroups-kill-ring' during sequential yanks.")

(defvar workgroups-kill-ring-size 20
  "Max length of `workgroups-kill-ring'.")

(defvar workgroups-mode-line-on t
  "Toggles workgroups' mode-line display.")

(defvar workgroups-mode-line-string "[unset]"
  "String displayed in the mode-line.")

(defvar workgroups-time-format "%H:%M:%S %A, %B %d %Y"
  "Format string for `format-time-string'.")


;;; utils

(defmacro workgroups-aif (test then &rest else)
  "Anaphoric if."
  (declare (indent defun))
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro workgroups-awhen (test &rest body)
  "Anaphoric when."
  (declare (indent defun))
  `(workgroups-aif ,test (progn ,@body)))

(defun workgroups-lnext (elt lst)
  "Return elt after ELT in LST, or nil."
  (cadr (member elt lst)))

(defun workgroups-lprev (elt lst)
  "Return elt before ELT in LST, or nil."
  (workgroups-lnext elt (reverse lst)))

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
  "Return a copy of LST with ELT1 with ELT2 swapped."
  (let* ((lst (copy-list lst))
         (l1  (member elt1 lst))
         (l2  (member elt2 lst)))
    (unless (and l1 l2)
      (error "ELT1 and ELT2 aren't present in LST."))
    (setcar l1 elt2)
    (setcar l2 elt1)
    lst))

(defun workgroups-window-list (&optional frame)
  "Flatten `window-tree' into a stable list.
`window-list' can't be used because its order isn't stable."
  (flet ((inner (obj) (if (atom obj) (list obj)
                        (mapcan 'inner (cddr obj)))))
    (inner (car (window-tree (or frame (selected-frame)))))))

(defun workgroups-fill-keymap (keymap &rest binds)
  "Fill KEYMAP with BINDS."
  (dolist (bind (workgroups-partition binds 2) keymap)
    (define-key keymap (read-kbd-macro (car bind)) (cadr bind))))


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

(defun workgroups-get-workgroup (name &optional noerror)
  "Return workgroup named NAME."
  (or (assoc name (workgroups-list noerror))
      (unless noerror
        (error "There is no workgroup named %S" name))))

;; FIXME: add frame support
(defun workgroups-current (&optional noerror)
  "Return `workgroups-current'."
  (or workgroups-current
      (unless noerror
        (error "No current workgroup."))))

;; FIXME: add frame support
(defun workgroups-previous (&optional noerror)
  "Return `workgroups-previous'."
  (or workgroups-previous
      (unless noerror
        (error "No previous workgroups."))))


;;; saving and loading

(defun workgroups-save-file (file)
  "Save FILE."
  (with-temp-buffer
    (insert (format "%s%S" workgroups-fid (workgroups-list)))
    (write-file file))
  (setq workgroups-dirty nil
        workgroups-file  file))

(defun workgroups-load-file (file)
  "Load FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (unless (eq workgroups-fid (read (current-buffer)))
      (error "%S is not a workgroups file."))
    (setq workgroups-list (read (current-buffer))))
  (setq workgroups-current  nil
        workgroups-previous nil
        workgroups-dirty    nil
        workgroups-file     file))


;;; name ops

(defun workgroups-name (workgroup)
  "Return the name of WORKGROUP."
  (car workgroup))

(defun workgroups-names (&optional noerror)
  "Return list of workgroups names."
  (mapcar 'workgroups-name (workgroups-list noerror)))

(defun workgroups-set-name (workgroup name)
  "Rename WORKGROUP to NAME."
  (setcar workgroup name)
  (setq workgroups-dirty t))


;;; workgroups-list ops

(defun workgroups-delete (workgroup)
  "Remove WORKGROUP from `workgroups-list'."
  (let ((wl (workgroups-list)))
    (when (eq workgroup (workgroups-current t))
      (setq workgroups-current nil))
    (when (eq workgroup (workgroups-previous t))
      (setq workgroups-previous nil))
    (setq workgroups-list (remove workgroup wl)
          workgroups-dirty t)))

(defun workgroups-add (workgroup &optional pos)
  "Add workgroup NEW.
Overwrites a workgroup with the same name, if it exists."
  (let ((name (workgroups-name new)) (wl (workgroups-list)))
    (workgroups-awhen (workgroups-get-workgroup name t)
      (or (y-or-n-p (format "%S exists. Overwrite? " name))
          (error "Cancelled"))
      (or pos (setq pos (position it wl)))
      (workgroups-delete it))
    (setq workgroups-dirty t)
    (setq workgroups-list
          (workgroups-linsert
           workgroup (or pos (length wl)) wl))))

(defun workgroups-list-swap (w1 w2)
  "Swap current with previous in the workgroups list."
  (when (eq w1 w2) (error "Can't swap a workgroup with itself"))
  (setq workgroups-list
        (workgroups-util-swap w1 w2 (workgroups-list))
        workgroups-dirty t))


;;; base and working config ops

(defun workgroups-set-base-config (workgroup config)
  "Set the base config of WORKGROUP to CONFIG."
  (setcar (cddr workgroup) config)
  (setq workgroups-dirty t)
  workgroup)

(defun workgroups-base-config (workgroup)
  "Return the base config of WORKGROUP."
  (or (nth 2 workgroup)))

(defun workgroups-set-working-config (workgroup config)
  "Set the working config of WORKGROUP to CONFIG."
  (setcar (cdr workgroup) config)
  workgroup)

(defun workgroups-working-config (workgroup)
  "Return the working config of WORKGROUP."
  (or (nth 1 workgroup)))


;;; config making

(defun workgroups-wprop (key window)
  "Return value of KEY in workgroups WINDOW, or nil."
  (cdr (assoc key (cdr window))))

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

(defun workgroups-make-workgroup (name base &optional working)
  "Make a workgroup named NAME from BASE and WORKING."
  (list name (or working base) base))

(defun workgroups-make-default (name &optional buffer)
  "Return a new default workgroup named NAME."
  (workgroups-make-workgroup
   name (workgroups-make-default-config buffer)))

(defun workgroups-copy-workgroup (workgroup &optional name)
  "Return a copy of WORKGROUP, optionally named NAME."
  (let ((copy (copy-list workgroup)))
    (when name (workgroups-set-name copy name))
    copy))


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
        (set-frame-position f x y)
        (set-frame-width f w)
        (set-frame-height f h)
        (delete-other-windows)
        (inner wtree)
        (set-frame-selected-window
         f (nth idx (workgroups-window-list f)))))))

(defun workgroups-restore (workgroup &optional base frame)
  "Restore WORKGROUP in FRAME or `selected-frame'."
  (workgroups-restore-config
   (if base (workgroups-base-config workgroup)
     (workgroups-working-config workgroup))
   frame))

(defun workgroups-restore-default-config (&optional frame)
  "Restore the default config in FRAME or `selected-frame'."
  (workgroups-restore-config (workgroups-make-default-config)))


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
    (dolist (w (workgroups-list) (list nil nil))
      (cond ((member buf (workgroups-config-buffer-list
                          (workgroups-working-config w)))
             (throw 'result (list w nil)))
            ((member buf (workgroups-config-buffer-list
                          (workgroups-base-config w)))
             (throw 'result (list w t)))))))


;;; mode-line

(defun workgroups-mode-line-update ()
  "Update the mode-line with current workgroup info."
  (setq workgroups-mode-line-string
        (format "[%s]" (workgroups-awhen (workgroups-current t)
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


;;; reading from the minibuffer

(defun workgroups-completing-read (prompt choices &rest args)
  "Call `ido-completing-read' or `completing-read'."
  (apply (if (and (boundp 'ido-mode) ido-mode)
             'ido-completing-read
           'completing-read) prompt choices args))

(defun workgroups-read-workgroup (&optional noerror)
  "Read a workgroup with `workgroups-completing-read'."
  (workgroups-get-workgroup
   (workgroups-completing-read
    "Workgroup: " (workgroups-names noerror)) noerror))

(defun workgroups-read-name (&optional prompt)
  "Read a non-empty name from the minibuffer."
  (let ((prompt (or prompt "Name: ")) name)
    (while (equal "" (setq name (read-from-minibuffer prompt)))
      (setq prompt "Please specify a non-empty name: "))
    name))

(defun workgroups-read-idx ()
  "Read and return a valid workgroup index."
  (let* ((l (1- (length (workgroups-list)))) (i nil)
         (prompt (format "Workgroup index [0-%d]: " l)))
    (while (and (setq i (read-from-minibuffer prompt nil nil t))
                (or (not (integerp i)) (< i 0) (>= i l)))
      (message "Please enter an integer between 0 and %d" l)
      (sit-for .5))
    i))


;;; command utils

(defun workgroups-updated-current (&optional noerror)
  "Return current workgroup after updating its working config."
  (workgroups-awhen (workgroups-current noerror)
    (workgroups-set-working-config
     it (workgroups-make-current-config))))

(defun workgroups-arg (&optional reverse noerror)
  "Return a workgroup.  For use in interactive forms.
If `current-prefix-arg' is nil return the current workgroups.
Otherwise read a workgroup from the minibuffer.  If REVERSE is
non-nil, `current-prefix-arg''s begavior is reversed."
  (if (if reverse (not current-prefix-arg) current-prefix-arg)
      (workgroups-read-workgroup noerror)
    (workgroups-updated-current noerror)))

(defun workgroups-add-to-kill-ring (config)
  "Add CONFIG to `workgroups-kill-ring'."
  (push config workgroups-kill-ring)
  (setq workgroups-kill-ring
        (workgroups-take workgroups-kill-ring
                         workgroups-kill-ring-size)))


;;; commands

(defun workgroups-echo-current ()
  "Display the name of the current workgroup in the echo area."
  (interactive)
  (message "Current: %S" (workgroups-name (workgroups-current))))

(defun workgroups-echo-all ()
  "Display the names of all workgroups in the echo area."
  (interactive)
  (message (mapconcat 'identity (workgroups-names) " | ")))

(defun workgroups-switch (workgroup &optional base)
  "Switch to WORKGROUP.
If BASE is non-nil, restore WORKGROUP's base config.  Otherwise
restore its working config."
  (interactive (list (workgroups-read-workgroup) current-prefix-arg))
  (let ((name (workgroups-name workgroup))
        (current (workgroups-updated-current t)))
    (when (eq workgroup current) (error "Already on %S" name))
    (setq workgroups-previous current
          workgroups-current workgroup)
    (workgroups-restore workgroup base)
    (workgroups-mode-line-update)
    (run-hooks 'workgroups-switch-hook)
    (message "Switched to %S." name)))

(defun workgroups-create (name)
  "Create and add a workgroup named NAME."
  (interactive (list (workgroups-read-name)))
  (let ((new (workgroups-make-default name)))
    (workgroups-add new)
    (workgroups-switch new)
    (message "Added %S" name)))

(defun workgroups-clone (workgroup name)
  "Create and add a clone of WORKGROUP named NAME."
  (interactive (list (workgroups-arg) (workgroups-read-name)))
  (let ((new (workgroups-copy-workgroup workgroup name)))
    (workgroups-add new)
    (workgroups-switch new)
    (message "Cloned %S to %S" (workgroups-name workgroup) name)))

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
    (message "Killed %S." (workgroups-name workgroup))))

(defun workgroups-kill-ring-save (workgroup)
  "Save WORKGROUP's working config to `workgroups-kill-ring'."
  (interactive (list (workgroups-arg)))
  (workgroups-add-to-kill-ring (workgroups-working-config workgroup))
  (message "Saved working config to workgroups-kill-ring"))

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
      (message "Yank!"))
    (error "workgroups-kill-ring empty")))

(defun workgroups-kill-workgroup-and-buffers (workgroup)
  "Kill WORKGROUP and the buffers in its working config."
  (interactive (list (workgroups-arg)))
  (let ((bufs (save-window-excursion
                (workgroups-restore workgroup)
                (mapcar 'window-buffer (window-list)))))
    (workgroups-kill workgroup)
    (mapc 'kill-buffer bufs)
    (message "Killed %S and its buffer" (workgroups-name workgroup))))

(defun workgroups-delete-other-workgroups (workgroup)
  "Delete all but the WORKGROUP."
  (interactive (list (workgroups-arg)))
  (mapc 'workgroups-delete (remove workgroup (workgroups-list)))
  (workgroups-switch workgroup)
  (message "Deleted all workgroups but %S" (workgroups-name workgroup)))

(defun workgroups-update (workgroup)
  "Set the base config of WORKGROUP to the current config."
  (interactive (list (workgroups-arg)))
  (workgroups-set-base-config
   workgroup (workgroups-working-config workgroup))
  (message "Updated %S" (workgroups-name workgroup)))

(defun workgroups-revert (workgroup)
  "Revert to `workgroups-base-config' of WORKGROUP."
  (interactive (list (workgroups-arg)))
  (workgroups-set-working-config
   workgroup (workgroups-base-config workgroup))
  (when (eq workgroup (workgroups-current))
    (workgroups-restore workgroup t))
  (message "Reverted %S" (workgroups-name workgroup)))

(defun workgroups-jump (n)
  "Switch to the Nth workgroup (zero-indexed) in `workgroups-list'.
Try N, then the prefix arg, then prompt for a number."
  (interactive (list (or current-prefix-arg (workgroups-read-idx))))
  (workgroups-switch
   (or (nth n (workgroups-list))
       (error "workgroup index %d out of range" n))))

;; There shorter, uglier ways to do this:
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
  (workgroups-list-swap (workgroups-current) (workgroups-previous))
  (workgroups-echo-all))

(defun workgroups-transpose-left (workgroup)
  "Swap WORKGROUP toward the beginning of `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (let ((prev (workgroups-lprev workgroup (workgroups-list))))
    (unless prev
      (error "%S is all the way left" (workgroups-name workgroup)))
    (workgroups-list-swap workgroup prev)
    (workgroups-echo-all)))

(defun workgroups-transpose-right (workgroup)
  "Move WORKGROUP toward the end of `workgroups-list'."
  (interactive (list (workgroups-arg)))
  (let ((next (workgroups-lnext workgroup (workgroups-list))))
    (unless next
      (error "%S is all the way right" (workgroups-name workgroup)))
    (workgroups-list-swap workgroup next)
    (workgroups-echo-all)))

(defun workgroups-rename (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (interactive (list (workgroups-arg) (workgroups-read-name "New name: ")))
  (let ((oldname (workgroups-name workgroup)))
    (workgroups-set-name workgroup newname)
    (message "Renamed %S to %S." oldname newname)))

(defun workgroups-toggle-mode-line ()
  "Toggle workgroups' mode-line display."
  (interactive)
  (setq workgroups-mode-line-on (not workgroups-mode-line-on))
  (force-mode-line-update)
  (message "mode-line diplay %s"
           (if workgroups-mode-line-on "on" "off")))

(defun workgroups-get-by-buffer ()
  "Switch to the workgroup that contains the specified buffer.
The first workgroup in `workgroups-list' that contains the buffer
is selected."
  (interactive)
  (let ((buf (workgroups-completing-read
              "Buffer: " (workgroups-buffer-list))))
    (destructuring-bind (w base) (workgroups-find-buffer buf)
      (if w (workgroups-switch w base)
        (error "No workgroup contains %S" buf)))))


;; file commands

(defun workgroups-save-workgroups (file)
  "Save workgroups to FILE.
Called interactively with a prefix arg, or if `workgroups-file'
is unset, read a filename.  Otherwise use `workgroups-file'."
  (interactive
   (list (if (or current-prefix-arg (not (workgroups-file t)))
             (read-file-name "File: ") (workgroups-file))))
  (workgroups-updated-current t)
  (workgroups-save-file file)
  (message "Wrote %s" file))

(defun workgroups-load-workgroups (file)
  "Load workgroups from FILE.
Called interactively with a prefix arg, and if `workgroups-file'
is set, use `workgroups-file'. Otherwise read a filename."
  (interactive
   (list (if (and current-prefix-arg (workgroups-file t))
             (workgroups-file) (read-file-name "File: "))))
  (workgroups-load-file file)
  (when workgroups-switch-on-load
    (workgroups-switch (car (workgroups-list))))
  (message "Loaded %s" file))

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

(defun workgroups-dired (dirname &optional switches)
  "Create a new workgroup and dired with DIRNAME and SWITCHES."
  (interactive (list (read-directory-name "Dired: ")
                     current-prefix-arg))
  (workgroups-create dirname)
  (dired dirname switches))


;;; misc commands

(defun workgroups-display-time ()
  "Echo the current time."
  (interactive)
  (message (format-time-string workgroups-time-format)))


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
   "C-e"        'workgroups-echo-current
   "e"          'workgroups-echo-current
   "S-C-e"      'workgroups-echo-all
   "C-b"        'workgroups-get-by-buffer
   "b"          'workgroups-get-by-buffer
   "C-f"        'workgroups-find-file
   "S-C-f"      'workgroups-find-file-read-only
   "d"          'workgroups-dired
   "C-i"        'workgroups-toggle-mode-line
   "C-t"        'workgroups-display-time
   "t"          'workgroups-display-time
   "A"          'workgroups-rename
   "C-s"        'workgroups-save-workgroups
   "C-l"        'workgroups-load-workgroups
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
  \\[workgroups-load-workgroups]    Load a workgroups file
  \\[workgroups-save-workgroups]    Save workgroups to a file
  \\[workgroups-find-file]    Create a new workgroup and find a file in it
  \\[workgroups-jump]    Jump to a numbered workgroup
  \\[workgroups-jump-0]    Jump to the 0th workgroup
    ...
  \\[workgroups-jump-9]    Jump to the 9th workgroup
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

(defun workgroups-query-hook-fn ()
  "Query for save on exit if `workgroups-dirty' is non-nil."
  (and workgroups-dirty
       workgroups-query-save-on-exit
       (y-or-n-p "Workgroups have been modified. Save them? ")
       (workgroups-save))
  t)

(defun workgroups-enable (enable)
  "Enable `workgroups-mode' when ENABLE is t, otherwise disable."
  (cond (enable (add-hook
                 'kill-emacs-query-functions
                 'workgroups-query-hook-fn)
                (workgroups-awhen workgroups-default-file
                  (workgroups-load-workgroups it))
                (workgroups-mode-line-add)
                (setq workgroups-mode t))
        (t      (remove-hook
                 'kill-emacs-query-functions
                 'workgroups-query-hook-fn)
                (workgroups-mode-line-remove)
                (setq workgroups-mode nil))))

;;;###autoload
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
