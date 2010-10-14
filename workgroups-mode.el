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
;;   If `workgroups-autoswitch' is non-nil, the first workgroup in a
;;   file will automatically be switched to when the file is loaded:
;;
;;     (setq workgroups-autoswitch t)
;;
;;   With these two options set, workgroups mode will automatically
;;   load the default file and switch to the first workgroup in it at
;;   emacs startup.
;;
;;   Check the documentation of the customizable variables below for
;;   more configuration options.
;;

;;; Some sample keybindings:
;;
;;   (global-set-key (kbd "C-c w a") 'workgroups-create)
;;   (global-set-key (kbd "C-c w k") 'workgroups-kill)
;;   (global-set-key (kbd "C-c w b") 'workgroups-switch)
;;   (global-set-key (kbd "C-c w s") 'workgroups-save)
;;   (global-set-key (kbd "C-c w f") 'workgroups-find-file)
;;   (global-set-key (kbd "C-c w u") 'workgroups-update)
;;   (global-set-key (kbd "C-c w r") 'workgroups-revert)
;;   (global-set-key (kbd "C-c w i") 'workgroups-raise)
;;   (global-set-key (kbd "C-c w j") 'workgroups-bury)
;;   (global-set-key (kbd "C-c w e") 'workgroups-show-current)
;;   (global-set-key (kbd "C-s-,")   'workgroups-previous)
;;   (global-set-key (kbd "C-s-.")   'workgroups-next)
;;

;;; Or the ido versions if you use ido-mode:
;;
;;   (global-set-key (kbd "C-c w a") 'workgroups-ido-add)
;;   (global-set-key (kbd "C-c w b") 'workgroups-ido-switch)
;;   (global-set-key (kbd "C-c w k") 'workgroups-ido-kill)
;;   (global-set-key (kbd "C-c w i") 'workgroups-ido-raise)
;;

;;; TODO:
;;
;;  - Switch just the window structure, keeping the current buffers,
;;    as far as that is possible with mismatch.
;;
;;  - Multi-frame support. Need to update mode-line for this, too.
;;
;;  - clone, kill-workgroup-and-buffers, kill-others
;;
;;


;;; Code:

(require 'cl)


;;; customization

(defgroup workgroups nil
  "Workgroup for Windows -- A simple window configuration
persistence mode."
  :group 'convenience
  :version "1.0")

(defcustom workgroups-switch-hook nil
  "Hook run whenever a workgroup is switched to."
  :type 'hook
  :group 'workgroups)

(defcustom workgroups-autoswitch t
  "Non-nil means automatically switch to the first workgroup when
a file is loaded."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-autosave t
  "Non-nil means automatically save `workgroups-list' to
`workgroups-file' whenever `workgroups-list' is modified."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-confirm-kill nil
  "Request confirmation before killing a workgroup when non-nil,
don't otherwise."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-default-file nil
  "File to load automatically when `workgroups-mode' is enabled.
If you want this to be loaded at emacs startup, make sure to set
it before calling `workgroups-mode'."
  :type 'file
  :group 'workgroups)

(defcustom workgroups-query-save-on-exit t
  "When non-nil, offer to save `workgroups-list' on exit if
`workgroups-dirty' in non-nil."
  :type 'boolean
  :group 'workgroups)

(defcustom workgroups-default-buffer "*scratch*"
  "Name of the default buffer used in `workgroups-create', and
other functions."
  :type 'string
  :group 'workgroups)


;;; non-customizable variables

(defvar workgroups-file nil
  "Current workgroups file.")

(defvar workgroups-list nil
  "List of current workgroups.")

(defvar workgroups-current nil
  "Current workgroup.")

(defvar workgroups-previous nil
  "Previous workgroup.")

(defvar workgroups-dirty nil
  "Non-nil means workgroups have been added or removed from
`workgroups-list' since the last save.")

(defvar workgroups-clipboard nil
  "Clipboard to which configs can be copied.")

(defvar workgroups-display-current-workgroup t
  "Toggles mode-line display.")

(defvar workgroups-mode-line-string "[unset]"
  "String displayed in the mode-line.")


;;; utils

(defun workgroups-circular-next (elt list)
  "Return elt after ELT in LIST, or car of LIST."
  (let ((next (cdr (member elt list))))
    (if next (car next) (car list))))

(defun workgroups-circular-prev (elt list)
  "Return elt before ELT in LIST, or car of last of LIST."
  (workgroups-circular-next elt (reverse list)))

(defun workgroups-take (lst n)
  "Return a list of the first N elts in LST.
Iterative to prevent stack overflow."
  (let (acc)
    (while (and lst (> n 0))
      (decf n)
      (push (pop lst) acc))
    (nreverse acc)))

(defun workgroups-list-insert (elt n lst)
  "Insert ELT into LST at N."
  (append (take lst n) (list elt) (nthcdr n lst)))

(defun workgroups-window-list (frame)
  "Flatten `window-tree' into a stable list.
`window-list' can't be used because its order isn't stable."
  (flet ((inner (obj) (if (atom obj) (list obj)
                        (mapcan 'inner (cddr obj)))))
    (inner (car (window-tree frame)))))


;;; functions

(defun workgroups-set-list (list)
  "Set `workgroups-list' to LIST."
  (setq workgroups-list list))

(defun workgroups-list ()
  "Return `workgroups-list'."
  workgroups-list)

(defun workgroups-set-cur (workgroup)
  "Set `workgroups-current' to WORKGROUP."
  (setq workgroups-current workgroup))

(defun workgroups-cur (&optional no-error)
  "Return car of `workgroups-list'."
  (or workgroups-current
      (let ((wl (workgroups-list)))
        (if wl (workgroups-set-cur (car wl))
          (unless no-error (error "No workgroups defined"))))))

(defun workgroups-set-prev (workgroup)
  "Set `workgroups-previous' to WORKGROUP."
  (setq workgroups-previous workgroup))

(defun workgroups-prev (&optional no-error)
  (or workgroups-previous
      (let ((wl (workgroups-list)))
        (if wl (workgroups-set-prev (car wl))
          (unless no-error (error "No workgroups defined"))))))

(defun workgroups-get-workgroup (name &optional no-error)
  "Return workgroup named NAME if it exists, otherwise nil."
  (or (assoc name (workgroups-list))
      (unless no-error
        (error "There is no workgroup named %S" name))))

(defun workgroups-name (workgroup)
  "Return the name of WORKGROUP."
  (car workgroup))

(defun workgroups-names ()
  "Return list of workgroups names."
  (mapcar 'workgroups-name (workgroups-list)))

(defun workgroups-rename-workgroup (workgroup newname)
  "Rename WORKGROUP to NEWNAME."
  (setcar workgroup newname))


;;; original and working configs

(defun workgroups-base-config (workgroup)
  ""
  (nth 2 workgroup))

(defun workgroups-working-config (workgroup)
  ""
  (nth 1 workgroup))

(defun workgroups-set-base-config (workgroup &optional config)
  "Set the base config of WORKGROUP to CONFIG."
  (setq workgroups-dirty t)
  (setcar (cddr workgroup) config))

(defun workgroups-set-working-config (workgroup config)
  "Set the working config of WORKGROUP to CONFIG."
  (setcar (cdr workgroup) config))


;;; saving

(defun workgroups-save-file (&optional query)
  "Save `workgroups-list' to `workgroups-file'."
  (let ((file (if (or query (not workgroups-file))
                  (read-file-name "File: ")
                workgroups-file))
        make-backup-files)
    (with-temp-buffer
      (insert ";; saved workgroups\n"
              (format "(workgroups-set-list '%S)"
                      (workgroups-list)))
      (write-file file))
    (setq workgroups-file  file
          workgroups-dirty nil)))

(defun workgroups-autosave ()
  "`workgroups-save-file' when `workgroups-autosave' is non-nil."
  (when workgroups-autosave
    (workgroups-save-file)))


;;; workgroups-list operations

(defun workgroups-add-workgroup (workgroup &optional pos)
  "Add WORKGROUP to the front of `workgroups-list'."
  (let ((wl (workgroups-list)))
    (workgroups-set-list
     (workgroups-list-insert workgroup (length wl) wl))
    (setq workgroups-dirty t)
    (workgroups-autosave)))

(defun workgroups-kill-workgroup (workgroup)
  "Remove WORKGROUP from `workgroups-list'."
  (let ((wl (workgroups-list)))
    (when (eq workgroup (workgroups-cur))
      (workgroups-set-cur nil))
    (when (eq workgroup (workgroups-prev))
      (workgroups-set-prev nil))
    (workgroups-set-list (remove workgroup wl))
    (setq workgroups-dirty t)
    (workgroups-autosave)))


;;; workgroup making

(defun workgroups-wprop (key window)
  "Return value of KEY in ALIST or nil."
  (cdr (assoc key (cdr window))))

(defun workgroups-make-window (winobj)
  "Make printable window object from WINOBJ.
WINOBJ is an Emacs window object."
  (with-current-buffer (window-buffer winobj)
    (list :window
          (cons :fname  (buffer-file-name))
          (cons :bname  (buffer-name))
          (cons :width  (let ((edges (window-edges winobj)))
                          ;; From `window-width' docstring:
                          (- (nth 2 edges) (nth 0 edges))))
          (cons :height (window-height winobj))
          (cons :point  (if (eq (point) (point-max)) :max (point)))
          (cons :wstart (window-start winobj)))))

(defun workgroups-make-config (&optional frame)
  (flet ((inner (wt)
                (if (atom wt)
                    (workgroups-make-window wt)
                  `(,(car wt) ,(cadr wt) ,@(mapcar 'inner (cddr wt))))))
    (let ((frame (or frame (selected-frame))))
      (list (mapcar (lambda (p) (frame-parameter frame p))
                    '(left top width height))
            (position (selected-window)
                      (workgroups-window-list frame))
            (inner (car (window-tree frame)))))))

(defun workgroups-make-workgroup (name &optional frame)
  "Make a workgroup from the `window-tree' of the
`selected-frame'."
  (let ((config (workgroups-make-config frame)))
    (list name config config)))


;;; workgroup restoring

(defun workgroups-leaf-window-p (window)
  "Return t if WINDOW is a workgroups window object."
  (and (consp window) (eq :window (car window))))

(defun workgroups-window-width (window)
  "Return the width of workgroups window WINDOW."
  (if (workgroups-leaf-window-p window)
      (workgroups-wprop :width window)
    (let ((coords (cadr window)))
      (- (nth 2 coords) (nth 0 coords)))))

(defun workgroups-window-height (window)
  "Return the height of workgroups window WINDOW."
  (if (workgroups-leaf-window-p window)
      (workgroups-wprop :height window)
    (let ((coords (cadr window)))
      (- (nth 3 coords) (nth 1 coords)))))

(defun workgroups-restore-window-state (window)
  "Set the state of `selected-window' to the file and/or
buffer-name contained in WINDOW."
  (let ((fname  (workgroups-wprop :fname  window))
        (bname  (workgroups-wprop :bname  window))
        (point  (workgroups-wprop :point  window))
        (wstart (workgroups-wprop :wstart window)))
    (and fname (file-exists-p fname) (find-file fname))
    (and bname (get-buffer bname) (switch-to-buffer bname))
    (set-window-start (selected-window) wstart t)
    (goto-char (if (eq point :max) (point-max) point))))

(defun workgroups-restore-config (config &optional frame)
  "Restore CONFIG in FRAME or `selected-frame'."
  (flet ((inner (wtree)
                (if (workgroups-leaf-window-p wtree)
                    (progn (workgroups-restore-window-state wtree)
                           (other-window 1))
                  (dolist (win (cddr wtree))
                    (unless (eq win (car (last wtree)))
                      (if (car wtree)
                          (split-window-vertically
                           (workgroups-window-height win))
                        (split-window-horizontally
                         (workgroups-window-width win))))
                    (inner win)))))
    (let ((frame (or frame (selected-frame))))
      (destructuring-bind ((left top width height) index wtree) config
        (set-frame-position frame left top)
        (set-frame-width    frame width)
        (set-frame-height   frame height)
        (delete-other-windows)
        (inner wtree)
        (set-frame-selected-window
         frame (nth index (workgroups-window-list frame)))))))

(defun workgroups-restore-workgroup (workgroup &optional orig frame)
  "Restore WORKGROUP in FRAME or `selected-frame'."
  (workgroups-restore-config
   (if (or (not (nth 1 workgroup)) orig)
       (nth 2 workgroup)
     (nth 1 workgroup))
   frame))


;;; command utils

(defun workgroups-completing-read ()
  "Read a workgroup name from the minibuffer.
Uses `ido-completing-read' if ido-mode is loaded and on,
`completing-read' otherwise."
  (workgroups-get-workgroup
   (funcall (if (and (boundp 'ido-mode) ido-mode)
                'ido-completing-read
              'completing-read)
            "Workgroup: " (workgroups-names))))

(defun workgroups-smart-get (&optional workgroup)
  "Return a WORKGROUP, one way or another."
  (or workgroup
      (and current-prefix-arg (workgroups-completing-read))
      (workgroups-cur)))


;;; commands

(defun workgroups-switch (workgroup &optional orig)
  "Switch to workgroup named NAME."
  (interactive (list (workgroups-completing-read) current-prefix-arg))
  (let ((prev (workgroups-cur t)))
    (when prev
      (workgroups-set-working-config
       prev (workgroups-make-config))
      (workgroups-set-prev prev)))
  (workgroups-restore-workgroup workgroup orig)
  (workgroups-set-cur workgroup)
  (run-hooks 'workgroups-switch-hook)
  (workgroups-mode-line-update)
  (message "Switched to %S." (workgroups-name workgroup)))

(defun workgroups-create (name)
  "Add workgroup named NAME."
  (interactive "sName: ")
  (let* ((w  (workgroups-get-workgroup name t))
         (wl (workgroups-list)) (pos (length wl)))
    (when w
      (if (not (yes-or-no-p (format "%S already exists. Overwrite? " name)))
          (error "Overwriting of %S cancelled" name)
        (setq pos (position w wl))
        (workgroups-kill-workgroup w)))
    (let ((new (save-window-excursion
                 (delete-other-windows)
                 (switch-to-buffer workgroups-default-buffer)
                 (workgroups-make-workgroup name))))
      (workgroups-add-workgroup new pos)
      (workgroups-switch new)
      (message "Added %S" name))))

(defun workgroups-clone (name &optional workgroup)
  "Create clone of WORKGROUP named NAME.
Add it at the end of `workgroups-list', and switch to it."
  (interactive "sName of clone: ")
  (let ((w (workgroups-smart-get workgroup)))
    (when (workgroups-get-workgroup name t)
      (error "A workgroup named %S already exists" name))
    (let ((new (workgroups-make-workgroup name)))
      (workgroups-add-workgroup new)
      (workgroups-switch new)
      (message "Cloned %S to %S" (workgroups-name w) name))))

(defun workgroups-copy-config ()
  "Copy current config to `workgroups-clipboard'."
  (interactive)
  (setq workgroups-clipboard (workgroups-make-config))
  (message "Copied!"))

(defun workgroups-kill (&optional workgroup)
  "Kill workgroup named NAME."
  (interactive)
  (let* ((w (workgroups-smart-get workgroup))
         (name (workgroups-name w)))
    (when (or (not workgroups-confirm-kill)
              (yes-or-no-p
               (format "Really kill %S?" name)))
      (workgroups-copy-config)
      (let ((next (workgroups-circular-next w (workgroups-list))))
        (when next (workgroups-restore-workgroup next)))
      (workgroups-kill-workgroup w)
      (message "Killed %S." name))))

(defun workgroups-yank ()
  "Restore `workgroups-clipboard' in `selected-frame'."
  (interactive)
  (unless workgroups-clipboard (error "Clipboard is empty"))
  (workgroups-restore-config workgroups-clipboard))

(defun workgroups-update (&optional workgroup)
  "Set the base config of WORKGROUP to the current config."
  (interactive)
  (let ((w (workgroups-smart-get workgroup)))
    (workgroups-set-working-config
     w (workgroups-make-config))
    (workgroups-set-base-config
     w (workgroups-working-config w))
    (message "Updated %S" (workgroups-name w))))

(defun workgroups-revert (&optional workgroup)
  "Revert to `workgroups-base-config' of WORKGROUP."
  (interactive)
  (let ((w (workgroups-smart-get workgroup)))
    (workgroups-set-working-config
     w (workgroups-base-config w))
    (when (eq w (workgroups-cur))
      (workgroups-restore-workgroup w t))
    (message "Reverted %S" (workgroups-name w))))

(defun workgroups-jump-to-num (&optional n)
  "Switch to the Nth workgroup (zero-indexed) in `workgroups-list'.
Try N, then the prefix arg, then prompt for a number."
  (interactive)
  (let ((wl (workgroups-list)))
    (unless wl (error "There are no workgroups defined"))
    (let* ((len (length wl))
           (n (or n current-prefix-arg
                  (read-from-minibuffer
                   (format "Workgroup number (%s total): " len)
                   nil nil t))))
      (unless (integerp n)
        (error "Argument %s not an integer" n))
      (unless (and (>= n 0) (< n len))
        (error "There are only %s workgroups [0-%s]" len (1- len)))
      (let ((w (nth n wl)))
        (when (eq w (workgroups-cur))
          (error "Workgroup %s already selected" n))
        (workgroups-switch w)))))

(defun workgroups-jump-to-key ()
  "Call `workgroups-Jump' using `last-command-char'.
Numbers 1-9 correspond to workgroups 0-8, and 0 corresponds to
the 9th workgroup."
  (interactive)
  (let ((n (string-to-number (string last-command-char))))
    (workgroups-jump-to-num (if (zerop n) 9 (1- n)))))

(defun workgroups-offset (w offset)
  "OFFSET WORKGROUP's position in `workgroups-list'."
  (let* ((name (workgroups-name w))
         (wl (workgroups-list))
         (pos (+ (position w wl) offset))
         (pos (max 0 (min (1- (length wl)) pos))))
    (workgroups-set-list
     (workgroups-list-insert w pos (remove w wl)))
    (message "Moved %S to position %s" name pos)))

;; (defun workgroups-swap

(defun workgroups-promote (&optional workgroup)
  "Move WORKGROUP toward the beginning of `workgroups-list'."
  (interactive)
  (workgroups-offset (workgroups-smart-get workgroup) -1))

(defun workgroups-demote (&optional workgroup)
  "Move WORKGROUP toward the end of `workgroups-list'."
  (interactive)
  (workgroups-offset (workgroups-smart-get workgroup) 1))

(defun workgroups-next ()
  "Switch to the next workgroup in `workgroups-list'."
  (interactive)
  (workgroups-switch
   (workgroups-circular-next
    (workgroups-cur) (workgroups-list))))

(defun workgroups-previous ()
  "Switch to the previous workgroup in `workgroups-list'."
  (interactive)
  (workgroups-switch
   (workgroups-circular-prev
    (workgroups-cur) (workgroups-list))))

(defun workgroups-toggle ()
  "Switch to the previous workgroup."
  (interactive)
  (workgroups-switch (workgroups-prev)))

(defun workgroups-rename ()
  "Rename the current workgroup. Prompt for new name."
  (interactive)
  (let* ((w (workgroups-cur))
         (oldname (car w))
         (newname (read-from-minibuffer
                   (format "Rename workgroup from %S to: "
                           oldname))))
    (workgroups-rename-workgroup w newname)
    (message "Renamed %S to %S." oldname newname)))

(defun workgroups-show-current ()
  "Message name of `workgroups-cur'."
  (interactive)
  (message "Current workgroup: %S"
           (workgroups-name (workgroups-cur))))


;; file commands

(defun workgroups-save (&optional new)
  "`workgroups-save-file' command."
  (interactive)
  (workgroups-save-file (or current-prefix-arg new))
  (message "Saved workgroups to %s" workgroups-file))

(defun workgroups-find-file (file)
  "Load FILE or `workgroups-file'."
  (interactive "fFile: ")
  (let ((file (or file workgroups-file)))
    (if (not (file-exists-p file))
        (message "File %s does not exist." file)
      (load-file file)
      (setq workgroups-file file)
      (when workgroups-autoswitch
        (workgroups-switch
         (workgroups-cur)))
      (message "Loaded workgroups file %s" file))))


;;; workgroups-map

(defvar workgroups-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a")    'workgroups-create)
    (define-key map (kbd "C-b")    'workgroups-switch)
    (define-key map (kbd "C-c")    'workgroups-clone)
    (define-key map (kbd "C-e")    'workgroups-show-current)
    (define-key map (kbd "C-f")    'workgroups-find-file)
    (define-key map (kbd "C-l")    'workgroups-demote)
    (define-key map (kbd "C-j")    'workgroups-jump-to-num)
    (define-key map (kbd "C-k")    'workgroups-kill)
    (define-key map (kbd "C-h")    'workgroups-promote)
    (define-key map (kbd "C-n")    'workgroups-next)
    (define-key map (kbd "C-p")    'workgroups-previous)
    (define-key map (kbd "C-q")    'workgroups-rename)
    (define-key map (kbd "C-r")    'workgroups-revert)
    (define-key map (kbd "C-s")    'workgroups-save)
    (define-key map (kbd "C-t")    'workgroups-toggle)
    (define-key map (kbd "C-u")    'workgroups-update)
    (define-key map (kbd "C-v")    'workgroups-toggle)
    (define-key map (kbd "C-y")    'workgroups-yank)
    (define-key map (kbd "C-z")    'workgroups-raise)
    (define-key map (kbd "M-w")    'workgroups-copy-config)
    (define-key map (kbd "0")      'workgroups-jump-to-key)
    (define-key map (kbd "1")      'workgroups-jump-to-key)
    (define-key map (kbd "2")      'workgroups-jump-to-key)
    (define-key map (kbd "3")      'workgroups-jump-to-key)
    (define-key map (kbd "4")      'workgroups-jump-to-key)
    (define-key map (kbd "5")      'workgroups-jump-to-key)
    (define-key map (kbd "6")      'workgroups-jump-to-key)
    (define-key map (kbd "7")      'workgroups-jump-to-key)
    (define-key map (kbd "8")      'workgroups-jump-to-key)
    (define-key map (kbd "9")      'workgroups-jump-to-key)
    map)
  "workgroups-mode's keymap.")


;; (defvar workgroups-help "workgroups keys:
;;   \\[elscreen-create]    Create a new screen and switch to it
;;   \\[elscreen-clone]    Create a new screen with the window-configuration of current screen
;;   \\[elscreen-kill]    Kill current screen
;;   \\[elscreen-kill-screen-and-buffers]  Kill current screen and buffers
;;   \\[elscreen-kill-others]    Kill other screens
;;   \\[elscreen-next]    Switch to the \"next\" screen in a cyclic order
;;   \\[elscreen-previous]    Switch to the \"previous\" screen in a cyclic order
;;   \\[elscreen-toggle]    Toggle to the screen selected previously
;;   \\[elscreen-select-and-goto]    Jump to the specified screen
;;   \\[elscreen-jump-0]
;;     :      Jump to the screen #
;;   \\[elscreen-jump-9]
;;   \\[elscreen-swap]  Swap current screen with previous one
;;   \\[elscreen-display-screen-name-list]    Show list of screens
;;   \\[elscreen-screen-nickname]    Name current screen
;;   \\[elscreen-display-last-message]    Show last message
;;   \\[elscreen-display-time]    Show time
;;   \\[elscreen-find-and-goto-by-buffer]    Switch to the screen in which specified buffer is displayed
;;   \\[elscreen-find-file]  Create new screen and open file
;;   \\[elscreen-find-file-read-only]  Create new screen and open file but don't allow changes
;;   \\[elscreen-dired]    Create new screen and run dired
;;   \\[elscreen-execute-extended-command]  Read function name, then call it with new screen
;;   \\[elscreen-toggle-display-screen-number]    Show/hide the screen number in the mode line
;;   \\[elscreen-toggle-display-tab]    Show/hide the tab at the top of screen
;;   \\[elscreen-display-version]    Show ElScreen version
;;   \\[elscreen-help]    Show this help"
;;   "Help shown by elscreen-help-mode")


;;; mode-line

(defun workgroups-mode-line-update ()
  "Update the mode-line with current workgroup info."
  (setq workgroups-mode-line-string
        (format "[%s]" (workgroups-name (workgroups-cur))))
  (force-mode-line-update))

(defun workgroups-mode-line-on ()
  "Turn on workgroups' mode-line display."
  (set-default 'mode-line-format
               (workgroups-list-insert
                '(workgroups-display-current-workgroup
                  (" " workgroups-mode-line-string))
                (1+ (position 'mode-line-position mode-line-format))
                mode-line-format))
  (force-mode-line-update))

(defun workgroups-mode-line-off ()
  "Turn off workgroups' mode-line display."
  (set-default 'mode-line-format
               (remove (assoc 'workgroups-display-current-workgroup
                              mode-line-format)
                       mode-line-format))
  (force-mode-line-update))

;; (workgroups-mode-line-on)
;; (force-mode-line-update)
;; (assoc 'workgroups-current-workgroup mode-line-format)
;; (set-default 'mode-line-format (remove (assoc 'workgroups-current-workgroup mode-line-format) mode-line-format))


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
                (when workgroups-default-file
                  (workgroups-find-file workgroups-default-file))
                (workgroups-mode-line-on)
                (setq workgroups-mode t))
        (t      (remove-hook
                 'kill-emacs-query-functions
                 'workgroups-query-hook-fn)
                (workgroups-mode-line-off)
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
