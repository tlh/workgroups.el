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
;; workgroups.el is a simple window configuration persistence package.
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
;;  - put `workgroups.el' somewhere on your emacs load path
;;
;;  - add these lines to your .emacs file:
;;    (require 'workgroups)
;;    (add-hook 'after-init-hook 'workgroups-load-configs)
;;

;;; Configuration:
;;
;;  - to change the file that window-configs are saved in:
;;    (setq workgroups-configs-file "/path/to/new/file")
;;
;;  - workgroups-restore-hook is a hook that's run whenever a
;;    workgroup is restored. You can add functions to it like this:
;;
;;    (add-hook 'workgroups-restore-hook 'foo)
;;

;;; Some sample keybindings:
;;
;;   (global-set-key (kbd "C-c g a") 'workgroups-add-config)
;;   (global-set-key (kbd "C-c g r") 'workgroups-restore-config)
;;   (global-set-key (kbd "C-c g d") 'workgroups-delete-config)
;;   (global-set-key (kbd "C-c g u") 'workgroups-update-config)
;;   (global-set-key (kbd "C-c g p") 'workgroups-prev-config)
;;   (global-set-key (kbd "C-c g n") 'workgroups-next-config)
;;

;;; And if you use ido-mode:
;;
;;   (global-set-key (kbd "C-c g A") 'workgroups-ido-add-config)
;;   (global-set-key (kbd "C-c g R") 'workgroups-ido-restore-config)
;;   (global-set-key (kbd "C-c g D") 'workgroups-ido-delete-config)
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

(defvar workgroups-window-configs nil
  "List containing all window configs that workgroups is
tracking.")

(defvar workgroups-current-config nil
  "Name of the current window config.")

;; Functions

(defun workgroups-circular-next (elt lst)
  "Return the element after ELT in LST, or the car of LST if ELT
is the last element of LST or is not present in LST."
  (or (cadr (member elt lst)) (car lst)))

(defun workgroups-config-names ()
  "Return a list of saved window config names."
  (mapcar 'car workgroups-window-configs))

(defun workgroups-find-config (name)
  "Find and return a workgroups config from NAME, or nil if it
doesn't exist."
  (assoc-string name workgroups-window-configs))

(defun workgroups-make-window (winobj)
  "Create a printable window object from WINOBJ, an Emacs window
object."
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

(defun* workgroups-get-config (&optional (frame (selected-frame)))
  "Create workgroups' printable frame and window representation
from the `window-tree' of the `selected-frame'."
  (labels ((inner (wt)
                  (if (atom wt)
                      (workgroups-make-window wt)
                    `(,(car wt) ,(cadr wt) ,@(mapcar 'inner (cddr wt))))))
    (list (mapcar (lambda (p) (frame-parameter frame p))
                  '(left top width height))
          (position (selected-window) (workgroups-window-list frame))
          (inner (car (window-tree frame))))))

(defun workgroups-set-window-state (window)
  "Set the state of `selected-window' to the file and/or
buffer-name contained in WINDOW."
  (destructuring-bind (tag w h filename buffername) window
    (cond (filename (find-file filename))
          ((get-buffer buffername) (switch-to-buffer buffername)))))

(defun* workgroups-set-config (wconfig &optional (frame (selected-frame)))
  "Restore WCONFIG in FRAME or `selected-frame'."
  (labels ((inner (wtree)
                  (if (workgroups-leaf-window-p wtree)
                      (progn (workgroups-set-window-state wtree)
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

(defun workgroups-save-configs ()
  "Save `workgroups-window-configs' to
`workgroups-configs-file'."
  (with-temp-buffer
    (let (make-backup-files)
      (insert (format "%S" workgroups-window-configs))
      (write-file workgroups-configs-file))))

(defun workgroups-load-configs (&optional file)
  "Load configs from FILE or `workgroups-configs-file'."
  (interactive)
  (let ((file (expand-file-name (or file workgroups-configs-file))))
    (if (not (file-exists-p file))
        (message "workgroups-configs-file doesn't exist: %s" file)
      (let (make-backup-files)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (progn (read (current-buffer))
                     (message "Workgroups: loaded configs file: %s" file))
            (error (message "There was an error reading workgroups configs file: %s" file))))))))

(defun workgroups-add-window-config (name)
  "Add the current window config to `workgroups-window-configs'
under NAME, and save the updated list to
`workgroups-configs-file'."
  (setq workgroups-window-configs
        (cons (list name (workgroups-get-config))
              (remove (workgroups-find-config name) workgroups-window-configs)))
  (workgroups-save-configs))

(defun workgroups-add-config (name)
  "Call `workgroups-add-window-config' with NAME, and set
`workgroups-current-config' to NAME."
  (interactive "sName: ")
  (let ((config (workgroups-find-config name)))
    (when (or (not config) (y-or-n-p (format "%s already exists. Overwrite? " name)))
      (workgroups-add-window-config name)
      (setq workgroups-current-config name)
      (message "Added config %s" name))))

(defun workgroups-restore-config (name)
  "Find the window config named NAME in
`workgroups-window-configs' and restore it."
  (interactive "sName: ")
  (let ((config (workgroups-find-config name)))
    (cond ((not config)
           (ding)
           (message "There is no config named %s." name))
          (t (workgroups-set-config (cadr config))
             (run-hooks 'workgroups-restore-hook)
             (setq workgroups-current-config name)
             (message "Restored config %s." name)))))

(defun workgroups-delete-config (name)
  "Delete the window config named NAME from
`workgroups-window-configs'."
  (interactive "sName: ")
  (let ((config (workgroups-find-config name)))
    (cond ((not config)
           (ding)
           (message "There is no config named %s." name))
          (t (setq workgroups-window-configs
                   (remove config workgroups-window-configs))
             (when (string= name workgroups-current-config)
               (setq workgroups-current-config nil))
             (workgroups-save-configs)
             (message "Deleted config %s." name)))))

(defun workgroups-update-config ()
  "Update the config stored under `workgroups-current-config'."
  (interactive)
  (cond ((null workgroups-current-config)
         (ding)
         (message "There is no current config to update."))
        (t (workgroups-add-window-config workgroups-current-config)
           (message "Updated config %s" workgroups-current-config))))

(defun workgroups-circular-restore (&optional prev)
  "Restore the previous or next window config circularly in
`workgroups-config-names'."
  (workgroups-restore-config
   (workgroups-circular-next workgroups-current-config
                             (let ((names (workgroups-config-names)))
                               (if prev (nreverse names) names)))))

(defun workgroups-next-config ()
  "Restore the next window config from
`workgroups-window-configs' circularly."
  (interactive)
  (workgroups-circular-restore))

(defun workgroups-prev-config ()
  "Restore the previous window config from
`workgroups-window-configs' circularly."
  (interactive)
  (workgroups-circular-restore t))

(defun* workgroups-ido-read-name (fn &optional prompt)
  "Call FN on the config name returned by
`ido-completing-read'."
  (funcall fn (ido-completing-read (or prompt "Config name: ")
                                   (workgroups-config-names))))

(defun workgroups-ido-add-config ()
  "Add a new config using `ido-completing-read' to suggest
possible completions."
  (interactive)
  (workgroups-ido-read-name 'workgroups-add-config))

(defun workgroups-ido-restore-config ()
  "Present restorable window configs using
`ido-completing-read'."
  (interactive)
  (workgroups-ido-read-name 'workgroups-restore-config))

(defun workgroups-ido-delete-config ()
  "Present deletable window configs using `ido-completing-read'."
  (interactive)
  (workgroups-ido-read-name 'workgroups-delete-config))

(provide 'workgroups)
