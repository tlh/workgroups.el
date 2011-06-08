;;; workgroups-pickel.el --- Elisp object serdes used by Workgroups
;;
;; Copyright (C) 2010, 2011 tlh
;;
;; Author: tlh <thunkout at gmail dot com>
;; Keywords: serialization deserialization serdes
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
;;  FIXME: update this
;;
;;
;;; Code:


(require 'cl)
(require 'workgroups-utils)



;;; vars

(defvar wg-pickel-identifier '~pickel!~
  "Symbol identifying a stream as a pickel.")

(defvar wg-pickel-types
  '(integer float symbol string cons vector hash-table)
  "Types pickel can serialize.")

(defvar wg-pickel-object-serializers
  '((integer    . identity)
    (float      . identity)
    (string     . identity)
    (symbol     . wg-pickel-symbol-serializer)
    (cons       . wg-pickel-cons-serializer)
    (vector     . wg-pickel-vector-serializer)
    (hash-table . wg-pickel-hash-table-serializer))
  "Alist mapping types to object serialization functions.")

(defvar wg-pickel-link-serializers
  '((cons       . wg-pickel-cons-link-serializer)
    (vector     . wg-pickel-vector-link-serializer)
    (hash-table . wg-pickel-hash-table-link-serializer))
  "Alist mapping types to link serialization functions.")

(defvar wg-pickel-object-deserializers
  '((s . wg-pickel-deserialize-uninterned-symbol)
    (c . wg-pickel-deserialize-cons)
    (v . wg-pickel-deserialize-vector)
    (h . wg-pickel-deserialize-hash-table))
  "Alist mapping type keys to object deserialization functions.")

(defvar wg-pickel-link-deserializers
  `((c . wg-pickel-relink-cons)
    (v . wg-pickel-relink-vector)
    (h . wg-pickel-relink-hash-table))
  "Alist mapping type keys to link deserialization functions.")



;;; utils

(defun wg-pickel-p (obj)
  "Return t when OBJ is a pickel, nil otherwise."
  (and (consp obj) (eq (car obj) wg-pickel-identifier)))



;;; bindings generation

(defun wg-pickel-generate-bindings-helper (obj binds)
  "See `wg-pickel-generate-bindings'."
  (unless (memq (type-of obj) wg-pickel-types)
    (error "Can't pickel type: %s" (type-of obj)))
  (unless (gethash obj binds)
    (puthash obj (incf i) binds)
    (case (type-of obj)
      (cons
       (wg-pickel-generate-bindings-helper (car obj) binds)
       (wg-pickel-generate-bindings-helper (cdr obj) binds))
      (vector
       (dotimes (j (length obj))
         (wg-pickel-generate-bindings-helper (aref obj j) binds)))
      (hash-table
       (wg-dohash (key val obj)
         (wg-pickel-generate-bindings-helper key binds)
         (wg-pickel-generate-bindings-helper val binds))))))

(defun wg-pickel-generate-bindings (obj)
  "Return a table binding unique subobjects of OBJ to uids."
  (let ((binds (make-hash-table :test 'eq)) (i -1))
    (wg-pickel-generate-bindings-helper obj binds)
    binds))



;;; object serialization

(defun wg-pickel-symbol-serializer (symbol)
  "Return SYMBOL's serialization."
  (cond ((eq symbol t) t)
        ((eq symbol nil) nil)
        ((intern-soft symbol) symbol)
        (t (list 's (symbol-name symbol)))))

(defun wg-pickel-cons-serializer (cons)
  "Return CONS's serialization."
  (list 'c))

(defun wg-pickel-vector-serializer (vector)
  "Return VECTOR's serialization."
  (list 'v (length vector)))

(defun wg-pickel-hash-table-serializer (table)
  "Return HASH-TABLE's serialization."
  (list 'h
        (hash-table-test table)
        (hash-table-size table)
        (hash-table-rehash-size table)
        (hash-table-rehash-threshold table)
        (hash-table-weakness table)))

(defun wg-pickel-serialize-objects (binds)
  "Return a list of serializations of the objects in BINDS."
  (let (result)
    (wg-dohash (obj id binds result)
      (let ((ser (wg-aget wg-pickel-object-serializers (type-of obj))))
        (unless ser (error "Invalid type: %S" (type-of obj)))
        (push (funcall ser obj) result)
        (push id result)))))



;;; object deserialization

(defun wg-pickel-deserialize-uninterned-symbol (name)
  "Return a new uninterned symbol from NAME."
  (make-symbol name))

(defun wg-pickel-deserialize-cons ()
  "Return a new cons cell initialized to nil."
  (cons nil nil))

(defun wg-pickel-deserialize-vector (length)
  "Return a new vector of length LENGTH."
  (make-vector length nil))

(defun wg-pickel-deserialize-hash-table (test size rsize rthresh weakness)
  "Return a new hash-table with the specified properties."
  (make-hash-table :test test :size size :rehash-size rsize
                   :rehash-threshold rthresh :weakness weakness))

(defun wg-pickel-deserialize-objects (serial-objects)
  "Return a hash-table of objects deserialized from SERIAL-OBJECTS."
  (let ((binds (make-hash-table)))
    (wg-destructuring-dolist ((uid obj . rest) serial-objects binds)
      (puthash uid
               (if (atom obj) obj
                 (wg-dbind (key . data) obj
                   (wg-aif (wg-aget wg-pickel-object-deserializers key)
                       (apply it data)
                     (error "Invalid object deserializer key: %S" key))))
               binds))))



;;; link serialization

(defun wg-pickel-cons-link-serializer (cons binds)
  "Return the serialization of CONS's links in BINDS."
  (list 'c
        (gethash cons binds)
        (gethash (car cons) binds)
        (gethash (cdr cons) binds)))

(defun wg-pickel-vector-link-serializer (vector binds)
  "Return the serialization of VECTOR's links in BINDS."
  (let (result)
    (dotimes (i (length vector) result)
      (push (gethash (aref vector i) binds) result)
      (push i result)
      (push (gethash vector binds) result)
      (push 'v result))))

(defun wg-pickel-hash-table-link-serializer (table binds)
  "Return the serialization of TABLE's links in BINDS."
  (let (result)
    (wg-dohash (key value table result)
      (push (gethash table binds) result)
      (push (gethash value binds) result)
      (push (gethash key binds) result)
      (push 'h result))))

(defun wg-pickel-serialize-links (binds)
  "Return a list of serializations of the links between objects in BINDS."
  (let (result)
    (wg-dohash (obj id binds result)
      (wg-awhen (wg-aget wg-pickel-link-serializers (type-of obj))
        (setq result (nconc (funcall it obj binds) result))))))



;;; link deserialization

(defun wg-pickel-relink-cons (cons-id car-id cdr-id binds)
  "Relink a cons cell with its car and cdr in BINDS."
  (let ((cons (gethash cons-id binds)))
    (setcar cons (gethash car-id binds))
    (setcdr cons (gethash cdr-id binds))))

(defun wg-pickel-relink-vector (vector-id index value-id binds)
  "Relink a vector with its elements in BINDS."
  (aset (gethash vector-id binds) index (gethash value-id binds)))

(defun wg-pickel-relink-hash-table (key-id value-id table-id binds)
  "Relink a hash-table with its keys and values in BINDS."
  (puthash (gethash key-id binds)
           (gethash value-id binds)
           (gethash table-id binds)))

(defun wg-pickel-deserialize-links (serial-links binds)
  "Return BINDS after relinking all its objects according to SERIAL-LINKS."
  (wg-destructuring-dolist
      ((key arg1 arg2 arg3 . rest) serial-links binds)
    (wg-aif (wg-aget wg-pickel-link-deserializers key)
        (funcall it arg1 arg2 arg3 binds)
      (error "Invalid link function key: %S" key))))



;;; pickeling

(defun wg-pickel (obj)
  "Serialize OBJ and return its serialization."
  (let ((binds (wg-pickel-generate-bindings obj)))
    (list wg-pickel-identifier
          (wg-pickel-serialize-objects binds)
          (wg-pickel-serialize-links binds)
          (gethash obj binds))))

(defun wg-pickel-to-string (obj)
  "Serialize OBJ to a string and return the string."
  (format "%S" (wg-pickel obj)))

(defun wg-pickel-to-file (file obj)
  "Serialize OBJ to FILE."
  (wg-write-sexp-to-file (wg-pickel obj) file))



;;; unpickeling

(defun wg-unpickel (pickel)
  "Deserialize PICKEL and return its deserialization."
  (unless (wg-pickel-p pickel)
    (error "Attempt to unpickel a non-pickel."))
  (wg-dbind (id serial-objects serial-links result) pickel
    (gethash
     result
     (wg-pickel-deserialize-links
      serial-links
      (wg-pickel-deserialize-objects serial-objects)))))

(defun wg-unpickel-file (file)
  "`unpickel' an object directly from FILE."
  (wg-unpickel (wg-read-sexp-from-file file)))

(defun wg-unpickel-string (str)
  "`unpickel' and object directly from STR."
  (wg-unpickel (read str)))



;;; provide

(provide 'workgroups-pickel)



;;; end of workgroups-pickel.el
