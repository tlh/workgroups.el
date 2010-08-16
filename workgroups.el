;;; workgroups.el --- workgroups for windows

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:      workgroups.el
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
;;    workgroups.el is a simple window configuration persistence
;;    package.  It saves the window layout of the current frame, and,
;;    if a window's buffer is visiting a file, it saves the filename
;;    as well.  And that's it. It doesn't try to save complicated
;;    information about the buffer, like major or minor modes. If you
;;    save configurations that include things like erc or gnus
;;    buffers, you should launch those applications and buffers again
;;    in your next session before restoring the configuration that
;;    includes them. Nothing bad will happen otherwise, of course --
;;    workgroups will just default to a buffer that already exists.
;;

;;; Features:
;;
;;  - Saving window configurations
;;
;;  - Restoring window configurations
;;
;;  - Persisting window configurations across sessions
;;

;;; Installation:
;;
;;  - Put `workgroups.el' somewhere on your emacs load path
;;
;;  - Add this line to your .emacs file:
;;
;;    (require 'workgroups)
;;

;;; Configuration:
;;
;;  - to change the file that configs are saved in:
;;    (setq workgroups-configs-file "/path/to/new/file")
;;
;;  - workgroups-restore-hook is a hook that's run whenever a
;;    workgroup is restored. You can add functions to it like this:
;;
;;    (add-hook 'workgroups-restore-hook 'foo)
;;

;;; Some sample keybindings:
;;
;;    (global-set-key (kbd "C-c C-g C-a") 'workgroups-add-config)
;;    (global-set-key (kbd "C-c C-g C-r") 'workgroups-restore-config)
;;    (global-set-key (kbd "C-c C-g C-d") 'workgroups-delete-config)
;;    (global-set-key (kbd "C-c C-g C-u") 'workgroups-update-config)
;;    (global-set-key (kbd "C-c C-g C-v") 'workgroups-revert-config)
;;    (global-set-key (kbd "C-s ,")       'workgroups-prev-config)
;;    (global-set-key (kbd "C-s .")       'workgroups-next-config)
;;

;;; Or if you use ido-mode:
;;
;;    (global-set-key (kbd "C-c C-g C-a") 'workgroups-ido-add-config)
;;    (global-set-key (kbd "C-c C-g C-r") 'workgroups-ido-restore-config)
;;    (global-set-key (kbd "C-c C-g C-d") 'workgroups-ido-delete-config)
;;

;;; TODO:
;;
;;  - buffer mode persistence
;;

;;; Code:

(eval-when-compile
  (require 'cl))

;; Customization

(defgroup workgroups nil
  "Workgroup for windows: A simple window configuration
persistence package."
  :group 'convenience
  :version "1.0")

(defcustom workgroups-configs-file
  (expand-file-name "~/.emacs.d/workgroups-configs")
  "File containing saved window configs."
  :type 'file
  :group 'workgroups)

(defcustom workgroups-restore-hook nil
  "Hook run whenever a window config is restored."
  :type 'hook
  :group 'workgroups)

;; Non-customizable variables

(defvar workgroups-current-config nil
  "Name of the current window config.")

;; Functions

(defun workgroups-write-configs (configs)
  "Save `workgroups-window-configs' to
`workgroups-configs-file'."
  (let (make-backup-files)
    (with-temp-buffer
      (insert ";; workgroups for windows - saved window configurations\n"
              ";; Don't edit this file manually\n"
              (format "%S" configs))
      (write-file workgroups-configs-file))))

(defun workgroups-get-configs ()
  "Get configs from `workgroups-configs-file'."
  (let ((filename (expand-file-name workgroups-configs-file))
        make-backup-files)
    (with-temp-buffer
      (cond ((file-exists-p filename)
             (insert-file-contents filename)
             (goto-char (point-min))
             (prog1 (condition-case err
                        (read (current-buffer))
                      (error (message "workgroups: Error in %s: %s" file (car err))))
               (message "workgroups: Loaded %s" filename)))
            (t (workgroups-write-configs nil)
               (message "workgroups: Created file %s" filename)
               nil)))))

(defun workgroups-get-config (name)
  "Return the config named NAME if it exists, otherwise nil."
  (assoc-string name (workgroups-get-configs)))

(defun workgroups-circular-next (elt lst)
  "Return the element after ELT in LST, or the car of LST if ELT
is the lat element of LST or is not present in LST."
  (or (cadr (member elt lst)) (car lst)))


;; TODO
;; workgroups-next-workgroup
;; workgroups-previous-workgroup
;; workgroups-bury-workgroup
;; workgroups-switch-to-workgroup
;; workgroups-kill-workgroup


(defun workgroups-config-names ()
  "Return a list of saved window config names."
  (mapcar 'car (workgroups-get-configs)))

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

(defun workgroups-window-list (frame)
  "Flatten `window-tree' into a stable list by depth-first
traversal.  `window-list' can't be used because its order isn't
stable."
  (labels ((inner (obj)
                  (if (atom obj)
                      (list obj)
                    (mapcan 'inner (cddr obj)))))
    (inner (car (window-tree frame)))))

(defun* workgroups-make-config (&optional (frame (selected-frame)))
  "Make workgroups' printable frame and window representation
from the `window-tree' of the `selected-frame'."
  (labels ((inner (wt)
                  (if (atom wt)
                      (workgroups-make-window wt)
                    `(,(car wt) ,(cadr wt) ,@(mapcar 'inner (cddr wt))))))
    (list (mapcar (lambda (p) (frame-parameter frame p))
                  '(left top width height))
          (position (selected-window) (workgroups-window-list frame))
          (inner (car (window-tree frame))))))

(defun workgroups-add-window-config (name)
  "Add the current window config under NAME, and write the
updated list to `workgroups-configs-file'."
  (workgroups-write-configs
   (cons (list name (workgroups-make-config))
         (remove (workgroups-get-config name)
                 (workgroups-get-configs)))))

(defun workgroups-add-config (name)
  "Call `workgroups-add-window-config' with NAME, and set
`workgroups-current-config' to NAME."
  (interactive "sName: ")
  (let ((config (workgroups-get-config name)))
    (when (or (not config) (y-or-n-p (format "%s already exists. Overwrite? " name)))
      (workgroups-add-window-config name)
      (setq workgroups-current-config name)
      (message "Added config %s" name))))

(defun workgroups-restore-window-state (window)
  "Set the state of `selected-window' to the file and/or
buffer-name contained in WINDOW."
  (destructuring-bind (tag w h filename buffername) window
    (cond (filename (find-file filename))
          ((get-buffer buffername) (switch-to-buffer buffername)))))

(defun* workgroups-restore-helper (wconfig &optional (frame (selected-frame)))
  "Restore WCONFIG in FRAME or `selected-frame'."
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
    (destructuring-bind ((left top width height) index wtree) wconfig
      (set-frame-position frame left top)
      (set-frame-width    frame width)
      (set-frame-height   frame height)
      (delete-other-windows)
      (inner wtree)
      (set-frame-selected-window
       frame (nth index (workgroups-window-list frame))))))

(defun workgroups-restore-config (name)
  "Restore window config named NAME."
  (interactive "sName: ")
  (let ((config (workgroups-get-config name)))
    (cond ((not config)
           (ding)
           (message "There is no config named %s." name))
          (t (workgroups-restore-helper (cadr config))
             (run-hooks 'workgroups-restore-hook)
             (setq workgroups-current-config name)
             (message "Restored config %s." name)))))

(defun workgroups-delete-config (name)
  "Delete window config named NAME."
  (interactive "sName: ")
  (let ((config (workgroups-get-config name)))
    (cond ((not config)
           (ding)
           (message "There is no config named %s." name))
          (t (workgroups-write-configs (remove config (workgroups-get-configs)))
             (when (string= name workgroups-current-config)
               (setq workgroups-current-config nil))
             (message "Deleted config %s." name)))))

(defun workgroups-update-config ()
  "Update the config stored under `workgroups-current-config'."
  (interactive)
  (cond ((null workgroups-current-config)
         (ding)
         (message "There is no current config to update."))
        (t (workgroups-add-window-config workgroups-current-config)
           (message "Updated config %s" workgroups-current-config))))

(defun workgroups-revert-config ()
  "Revert the current config to `workgroups-current-config'."
  (interactive)
  (if workgroups-current-config
      (workgroups-restore-config workgroups-current-config)
    (message "There is no current workgroups configuration.")))

(defun workgroups-echo-current-config ()
  "Print `workgroups-current-config' to the echo area."
  (interactive)
  (message "Current workgroups config: %s" workgroups-current-config))

(defun workgroups-circular-restore (&optional prev)
  "Restore the previous or next window config circularly in
`workgroups-config-names'."
  (workgroups-restore-config
   (workgroups-circular-next workgroups-current-config
                             (let ((names (workgroups-config-names)))
                               (if prev (nreverse names) names)))))

(defun workgroups-next-config ()
  "Restore the next window config circularly."
  (interactive)
  (workgroups-circular-restore))

(defun workgroups-prev-config ()
  "Restore the previous window config circularly."
  (interactive)
  (workgroups-circular-restore t))

(defun workgroups-ido-read ()
  "Get a config name with `ido-completing-read'."
  (ido-completing-read "Config name: " (workgroups-config-names)))

(defun workgroups-ido-add-config ()
  "Add a config with `ido-completing-read'."
  (interactive)
  (workgroups-add-config (workgroups-ido-read)))

(defun workgroups-ido-restore-config ()
  "Restore a config with `ido-completing-read'."
  (interactive)
  (workgroups-restore-config (workgroups-ido-read)))

(defun workgroups-ido-delete-config ()
  "Delete a config with `ido-completing-read'."
  (interactive)
  (workgroups-delete-config (workgroups-ido-read)))

(provide 'workgroups)

;;; workgroups.el ends here
