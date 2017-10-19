;;; -*- lexical-binding: t -*-
;;; define parameters of a max/msp system and export them to json files

(require 'json)
(require 'seq)
(require 'zsy)

(defvar *maxsys/-map-id-counter* -1)
(defvar *maxsys/parameter-page-size* 8)

;;; general accessors of map
(defun maxsys/-default-get-scriptname-func (m)
  (or (alist-get :scriptname m)
      (alist-get :name m)))

(defun maxsys/get-scriptname (m)
  (let ((scriptname-func (or (alist-get :scriptname-func m)
			     #'maxsys/-default-get-scriptname-func)))
    (funcall scriptname-func m)))

;;; module and parameters are represented as maps (alists)
;;; they have their own construction functions
;;; but we have the same function and macros to make a list of maps

(defun maxsys/-process-mutli-form (make-map-func form)
  "handling multiple map form, return a list of map(s) from single form"
  (let* ((map-f (lambda (subform)
		  (apply make-map-func subform)))
	 (tag (car form))
	 (subforms (if (consp tag)
		       form
		     (list form))))
    (seq-map map-f subforms)))

(defun maxsys/-make-maps-macro-helper (make-map-func entries)
  (let ((map-f (lambda (entry)
		 ;; entry can be in `(:name ...)' raw parameter form
		 ;; or `variable-term' form
		 ;; or `(func ...)' form
		 (let ((args
			(if (consp entry)
			    (let ((tag (car entry)))
			      (if (or (keywordp tag)
				      (integerp tag))
				  `(list ,@entry)
				entry))
			  entry)))
		   `(maxsys/-process-mutli-form (quote ,make-map-func)
						,args)))))
    (seq-map map-f entries)))

(defmacro maxsys/make-make-maps-macro (ident make-map-func)
  (let* ((macro-name (format "maxsys/mk-%s" ident))
	 (macro-symbol (intern macro-name)))
    `(defmacro ,macro-symbol (&rest entries)
       `(let ((*maxsys/-map-id-counter* -1))
	  (funcall 'seq-concatenate
		   'list
		   ,@(maxsys/-make-maps-macro-helper
		      ,make-map-func
		      entries))))))

;;; parameter part

(defun maxsys/make-parameter (name min max default &rest rest-args)
  (setq *maxsys/-map-id-counter*
	(let ((param-id (plist-get rest-args :id))
	      (page-id (plist-get rest-args :start-page)))
	  (if param-id
	      param-id
	    (if page-id
		(* *maxsys/parameter-page-size* page-id)
	      (+ *maxsys/-map-id-counter* 1)))))
  
  (let ((p `((:id . ,*maxsys/-map-id-counter*)
	     (:name . ,name)
	     (:type . :parameter)
	     (:min . ,min)
	     (:max . ,max)
	     (:default . ,default)))
	(rest-alist (zsy/plist->alist rest-args)))
    ;; filter :id and :start-page key
    (setq rest-alist (assq-delete-all :id rest-alist))
    (setq rest-alist (assq-delete-all :start-page rest-alist))
    (append p (reverse rest-alist))))

(maxsys/make-make-maps-macro params 'maxsys/make-parameter)


;;; modules part

(defun maxsys/make-module (name params &rest rest-args)
  (setq *maxsys/-map-id-counter*
	(let ((module-id (plist-get rest-args :id)))
	  (if module-id
	      module-id
	    (+ *maxsys/-map-id-counter* 1))))
  (let ((p `((:id . ,*maxsys/module-id-counter*)
	     (:name . ,name)
	     (:type . :module)
	     (:params . ,params)))
	(rest-alist (zsy/plist->alist rest-args)))
    ;; filter :id key
    (setq rest-alist (assq-delete-all :id rest-alist))
    (append p (reverse rest-alist))))

(maxsys/make-make-maps-macro modules 'maxsys/make-module)

;;; controllers part

(defun maxsys/make-ctrl (id min max &rest rest-args)
  (let ((ctrl (list
	       (cons :id id)
	       (cons :min min)
	       (cons :max max)))
	(rest-alist (zsy/plist->alist rest-args)))
    ctrl))

(maxsys/make-make-maps-macro ctrls 'maxsys/make-ctrl)


;;; controller modules part

(defun maxsys/make-ctrl-module (name ctrls &rest rest-args)
  (setq *maxsys/ctrl-module-id-counter*
	(let ((module-id (plist-get rest-args :id)))
	  (if module-id
	      module-id
	    (+ *maxsys/ctrl-module-id-counter* 1))))
  (let ((p (list (cons :id *maxsys/ctrl-module-id-counter*)
		 (cons :name name)
		 (cons :type :ctrl-module)
		 (cons :ctrls ctrls)))
	(rest-alist (zsy/plist->alist rest-args)))
    ;; filter :id key
    (setq rest-alist (assq-delete-all :id rest-alist))
    (append p (reverse rest-alist))))

(maxsys/make-make-maps-macro ctrls 'maxsys/make-ctrl)
