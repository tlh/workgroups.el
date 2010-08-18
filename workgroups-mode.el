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
;;   are added to `workgroups-list' by calling `workgroups-add',
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
;;   Once you've added a few workgroups with `workgroups-add', you
;;   should save them to a file with `workgroups-save'.  You can
;;   designate a file to be automatically loaded when workgroups-mode
;;   is started by setting `workgroups-default-file' like so:
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
;;   To turn on workgroups-mode, either issue the command:
;;
;;     M-x workgroups-mode
;;
;;   Or put this in your .emacs file:
;;
;;     (workgroups-mode t)
;;
;;   Check the documentation of the customizable variables below for
;;   more configuration options.
;;

;;; Some sample keybindings:
;;
;;   (global-set-key (kbd "C-c w a") 'workgroups-add)
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

;;; Code:

(eval-when-compile
  (require 'cl))

;; Customization

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

;; Non-customizable variables

(defvar workgroups-file nil
  "Current workgroups file.")

(defvar workgroups-list nil
  "List of current workgroups.")

(defvar workgroups-dirty nil
  "Non-nil means workgroups have been added or removed from
`workgroups-list' since the last save.")

;; Functions

(defun workgroups-current-workgroup ()
  "Return car of `workgroups-list'."
  (car workgroups-list))

(defun workgroups-get-workgroup (name)
  "Return workgroup named NAME if it exists, otherwise nil."
  (assoc name workgroups-list))

(defun workgroups-name (workgroup)
  "Return the name of WORKGROUP."
  (car workgroup))

(defun workgroups-names (&optional bury-first)
  "Return list of workgroups names."
  (let ((names (mapcar 'workgroups-name workgroups-list)))
    (if bury-first
        (append (cdr names) (list (car names)))
      names)))

(defun workgroups-current-workgroup-name ()
  "Return the name of the current workgroup."
  (workgroups-name (workgroups-current-workgroup)))

(defun workgroups-completing-read (&optional bury-first)
  "Read a workgroup name from the minibuffer."
  (list (completing-read "Workgroup: " (workgroups-names bury-first))))

(defun workgroups-save-file (&optional query)
  "Save `workgroups-list' to `workgroups-file'."
  (let ((file (if (or query (not workgroups-file))
                  (read-file-name "File: ")
                workgroups-file))
        make-backup-files)
    (with-temp-buffer
      (insert ";; workgroups for windows - saved workgroups\n\n\n"
              (format "(setq workgroups-list '%S)" workgroups-list))
      (write-file file))
    (setq workgroups-file  file
          workgroups-dirty nil)))

(defun workgroups-window-list (frame)
  "Flatten `window-tree' into a stable list by depth-first
traversal.  `window-list' can't be used because its order isn't
stable."
  (labels ((inner (obj)
                  (if (atom obj)
                      (list obj)
                    (mapcan 'inner (cddr obj)))))
    (inner (car (window-tree frame)))))

;; workgroups-list operations

(defun workgroups-autosave ()
  "Save `workgroups-file' when `workgroups-autosave' is non-nil."
  (when workgroups-autosave
    (workgroups-save-file)))

(defun workgroups-raise-workgroup (workgroup)
  "Move WORKGROUP to the front of `workgroups-list'."
  (setq workgroups-list
        (cons workgroup
              (remove workgroup workgroups-list)))
  (workgroups-autosave))

(defun workgroups-add-workgroup (workgroup)
  "Add WORKGROUP to the front of `workgroups-list'."
  (setq workgroups-list (cons workgroup workgroups-list))
  (setq workgroups-dirty t)
  (workgroups-autosave))

(defun workgroups-kill-workgroup (workgroup)
  "Remove WORKGROUP from `workgroups-list'."
  (setq workgroups-list (remove workgroup workgroups-list))
  (setq workgroups-dirty t)
  (workgroups-autosave))

(defun workgroups-bury-workgroup (workgroup)
  "Move WORKGROUP to the end of `workgroups-list'."
  (setq workgroups-list
        (append (remove workgroup workgroups-list)
                (list workgroup)))
  (workgroups-autosave))

;; workgroup making

(defun workgroups-make-window (winobj)
  "Make printable window object from WINOBJ.
WINOBJ is an Emacs window object."
  (let ((buffer (window-buffer winobj)))
    (list :window
          (let ((edges (window-edges winobj)))
            ;; this window-width calculation was found
            ;; in the documentation for window-width.
            (- (nth 2 edges) (nth 0 edges)))
          (window-height winobj)
          (buffer-file-name buffer)
          (buffer-name buffer))))

(defun workgroups-make-workgroup (name &optional frame)
  "Make a workgroup from the `window-tree' of the
`selected-frame'."
  (labels ((inner (wt)
                  (if (atom wt)
                      (workgroups-make-window wt)
                    `(,(car wt) ,(cadr wt) ,@(mapcar 'inner (cddr wt))))))
    (let ((frame (or frame (selected-frame))))
      (list name
            (mapcar (lambda (p) (frame-parameter frame p))
                    '(left top width height))
            (position (selected-window) (workgroups-window-list frame))
            (inner (car (window-tree frame)))))))

;; workgroup restoring

(defun workgroups-leaf-window-p (window)
  "Return t if WINDOW is a workgroups window object."
  (and (consp window)
       (eq (car window) :window)))

(defun workgroups-window-width (window)
  "Return the width of workgroups window WINDOW."
  (if (workgroups-leaf-window-p window)
      (nth 1 window)
    (destructuring-bind (x1 y1 x2 y2) (cadr window)
      (- x2 x1))))

(defun workgroups-window-height (window)
  "Return the height of workgroups window WINDOW."
  (if (workgroups-leaf-window-p window)
      (nth 2 window)
    (destructuring-bind (x1 y1 x2 y2) (cadr window)
      (- y2 y1))))

(defun workgroups-restore-window-state (window)
  "Set the state of `selected-window' to the file and/or
buffer-name contained in WINDOW."
  (destructuring-bind (tag w h filename buffername) window
    (cond ((and filename (file-exists-p filename))
           (find-file filename))
          ((and buffername (get-buffer buffername))
           (switch-to-buffer buffername)))))

(defun workgroups-restore-workgroup (workgroup &optional frame)
  "Restore WORKGROUP in FRAME or `selected-frame'."
  (labels ((inner (wtree)
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
      (destructuring-bind (name (left top width height) index wtree) workgroup
        (set-frame-position frame left top)
        (set-frame-width    frame width)
        (set-frame-height   frame height)
        (delete-other-windows)
        (inner wtree)
        (set-frame-selected-window
         frame (nth index (workgroups-window-list frame)))))
    (run-hooks 'workgroups-switch-hook)))

;; commands

(defun workgroups-save (&optional new)
  "`workgroups-save-file' command."
  (interactive)
  (workgroups-save-file (or current-prefix-arg new))
  (message "Saved workgroups to %s" workgroups-file))

(defun workgroups-switch (name)
  "Switch to workgroup named NAME."
  (interactive (workgroups-completing-read t))
  (let ((workgroup (workgroups-get-workgroup name)))
    (if (not workgroup)
        (message "There is no workgroup named %s." name)
      (workgroups-raise-workgroup workgroup)
      (workgroups-restore-workgroup workgroup)
      (message "Switched to %s." name))))

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
         (workgroups-current-workgroup-name)))
      (message "Loaded workgroups file %s" file))))

(defun workgroups-add (name)
  "Add workgroup named NAME."
  (interactive "sName: ")
  (let ((workgroup (workgroups-get-workgroup name)))
    (when (or (not workgroup)
              (y-or-n-p (format "%s already exists. Overwrite? " name)))
      (workgroups-kill-workgroup workgroup)
      (workgroups-add-workgroup
       (workgroups-make-workgroup name))
      (message "Added workgroup %s" name))))

(defun workgroups-kill (&optional name)
  "Kill workgroup named NAME."
  (interactive (workgroups-completing-read))
  (let* ((workgroup (if name
                        (workgroups-get-workgroup name)
                      (workgroups-current-workgroup)))
         (name (workgroups-name workgroup)))
    (if (not workgroup)
        (message "There is no workgroup named %s." name)
      (workgroups-kill-workgroup workgroup)
      (message "Killed %s." name))))

(defun workgroups-raise (name)
  "Raise workgroup named NAME to the front of `workgroups-list'.
Don't restore it, though."
  (interactive (workgroups-completing-read))
  (let ((workgroup (workgroups-get-workgroup name)))
    (if (not workgroup)
        (message "There is no workgroup named %s." name)
      (workgroups-raise-workgroup workgroup)
      (message "Raised %s." name))))

(defun workgroups-bury ()
  "Move `workgroups-current-workgroup' to the end of
`workgroups-list', but don't restore the new
`workgroup-current-workgroup'."
  (interactive)
  (let ((workgroup (workgroups-current-workgroup)))
    (workgroups-bury-workgroup workgroup)
    (message "Buried %s" (workgroups-name workgroup))))

(defun workgroups-revert ()
  "Revert to `workgroups-current-workgroup'."
  (interactive)
  (workgroups-switch
   (workgroups-current-workgroup-name)))

(defun workgroups-next ()
  "Switch to the next workgroup in `workgroups-list'."
  (interactive)
  (workgroups-bury-workgroup (workgroups-current-workgroup))
  (workgroups-revert))

(defun workgroups-previous ()
  "Switch to the previous workgroup in `workgroups-list'."
  (interactive)
  (workgroups-raise-workgroup (car (last workgroups-list)))
  (workgroups-revert))

(defun workgroups-update ()
  "Update workgroup named NAME."
  (interactive)
  (let ((workgroup (workgroups-current-workgroup)))
    (if (not workgroup)
        (call-interactively 'workgroups-add)
      (workgroups-kill-workgroup workgroup)
      (workgroups-add-workgroup
       (workgroups-make-workgroup
        (workgroups-name workgroup)))
      (message "Updated workgroup %s"
               (workgroups-name workgroup)))))

(defun workgroups-show-current ()
  "Message name of `workgroups-current-workgroup'."
  (interactive)
  (let ((name (workgroups-current-workgroup-name)))
    (if name
        (message "Current workgroup: %s" name)
      (message "No workgroups are currently loaded."))))

;; ido

(defun workgroups-ido-read (&optional bury-first)
  "Read a workgroup name with `ido-completing-read'."
  (ido-completing-read "Workgroup: " (workgroups-names bury-first)))

(defun workgroups-ido-add ()
  "Add to workgroup chosen with `ido-completing-read'."
  (interactive)
  (workgroups-add (workgroups-ido-read)))

(defun workgroups-ido-switch ()
  "Switch to workgroup chosen with `ido-completing-read'."
  (interactive)
  (workgroups-switch (workgroups-ido-read t)))

(defun workgroups-ido-kill ()
  "Kill workgroup chosen with `ido-completing-read'."
  (interactive)
  (workgroups-kill (workgroups-ido-read)))

(defun workgroups-ido-raise ()
  "Raise workgroup chosen with `ido-completing-read'."
  (interactive)
  (workgroups-raise (workgroups-ido-read)))

;; mode definition

(defun workgroups-query-hook-fn ()
  "Query for save on exit if `workgroups-dirty' is non-nil."
  (and workgroups-dirty
       workgroups-query-save-on-exit
       (y-or-n-p "Workgroups have been modified. Do you to save them? ")
       (workgroups-save))
  t)

(defun workgroups-enable (enable)
  "Enable `workgroups-mode' when ENABLE is t, otherwise disable."
  (cond (enable (add-hook 'kill-emacs-query-functions 'workgroups-query-hook-fn)
                (when workgroups-default-file
                  (workgroups-find-file workgroups-default-file))
                (setq workgroups-mode t))
        (t      (remove-hook 'kill-emacs-query-functions 'workgroups-query-hook-fn)
                (setq workgroups-mode nil))))

;;;###autoload
(define-minor-mode workgroups-mode
  "This toggles workgroups-mode.

If ARG is null, toggle workgroups-mode.
If ARG is a number greater than zero, turn on workgroups-mode.
Otherwise, turn off workgroups-mode."
  :lighter     " Workgroups"
  :init-value  nil
  :global      t
  :group       'workgroups
  (cond (noninteractive   (workgroups-enable nil))
        (workgroups-mode  (workgroups-enable t))
        (t                (workgroups-enable nil))))

;; provide

(provide 'workgroups-mode)

;;; workgroups-mode.el ends here
