;;; -*- lexical-binding: t -*-
;;; my common library

(require 'seq)

(defun zsy/plist->alist (pl)
  "convert plist to alist with same order"
  (let (al
	(parted-plist (seq-partition pl 2)))
    (dolist (pair parted-plist al)
      (let ((k (car pair))
	    (v (cadr pair)))
	(setq al (cons (cons k v) al))))
    (reverse al)))

(defun zsy/keyword-name (kw)
  "convert keyword to string without the first colon"
  (substring (symbol-name kw) 1))

(defun zsy/make-keyword (s)
  "convert string to keyword, no need to provide the colon"
  (intern (concat ":" s)))

(defun zsy/write-string-to-file (s filename &optional append)
  "simplify the `write-region' function for writing string to file"
  (write-region s nil filename append))

(provide 'zsy)
