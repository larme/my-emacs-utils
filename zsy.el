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

(provide 'zsy)
