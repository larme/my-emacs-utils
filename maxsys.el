;;; -*- lexical-binding: t -*-
;;; define parameters of a max/msp system and export them to json files

(require 'json)
(require 's)
(require 'seq)
(require 'zsy)

(defvar *maxsys/parameter-id-counter* -1)
(defvar *maxsys/parameter-page-size* 8)
(defvar *maxsys/parameters* nil)
(defvar *maxsys/module-id-counter* -1)
(defvar *maxsys/modules* nil)

(defvar *maxsys/ctrl-module-id-counter* -1)
(defvar *maxsys/ctrls* nil)
(defvar *maxsys/ctrl-modules* nil)

(defvar *maxsys/parameter-ctrl-mappings* nil)
(defvar *maxsys/parameter-ctrl-mapping-modules* nil)

;;; general accessors of map
(defun maxsys/-default-get-scriptname-func (m)
  (or (alist-get :scriptname m)
      (alist-get :name m)))

(defun maxsys/get-scriptname (m)
  (let ((scriptname-func (or (alist-get :scriptname-func m)
			     #'maxsys/-default-get-scriptname-func)))
    (funcall scriptname-func m)))

;;; parameter part

(defun maxsys/make-parameter (name min max default &rest rest-args)
  (setq *maxsys/parameter-id-counter*
	(let ((param-id (plist-get rest-args :id))
	      (page-id (plist-get rest-args :start-page)))
	  (if param-id
	      param-id
	    (if page-id
		(* *maxsys/parameter-page-size* page-id)
	      (+ *maxsys/parameter-id-counter* 1)))))
  
  (let ((p `((:id . ,*maxsys/parameter-id-counter*)
	     (:name . ,name)
	     (:type . :parameter)
	     (:min . ,min)
	     (:max . ,max)
	     (:default . ,default)))
	(rest-alist (zsy/plist->alist rest-args)))
    ;; filter :id and :start-page key
    (setq rest-alist (assq-delete-all :id rest-alist))
    (setq rest-alist (assq-delete-all :start-page rest-alist))
    (setq *maxsys/parameters*
	  (cons (append p (reverse rest-alist))
		*maxsys/parameters*))))

(defun maxsys/make-parameters (form &optional make-param-func)
  "handling multiple parameters list"
  (let ((make-param-func (or make-param-func
			     'maxsys/make-parameter))
	(tag (car form)))
    (if (consp tag)
	(dolist (subform form)
	  (apply make-param-func subform))
      (apply make-param-func form))))

(defun maxsys/mk-params-helper (entries &optional make-param-func)
  (let ((make-param-func (or make-param-func
			     'maxsys/make-parameter))
	(params-form nil))
    (dolist (entry entries params-form)
      (let (to-cons-form)
	;; entry can be in `(:name ...)' raw parameter form
	;; or `variable-term' form
	;; or `(func ...)' form
	(if (consp entry)
	    (let ((param-name (car entry))
		  (rest (cdr entry)))	      
	      (setq to-cons-form
		    (if (or (keywordp param-name)
			    (integerp param-name))
			`(list ,@entry)
		      entry)))
	  (setq to-cons-form entry))
	(setq params-form
	      (cons `(maxsys/make-parameters ,to-cons-form (quote ,make-param-func))
		    params-form))))
    (reverse params-form)))

(defmacro maxsys/mk-params (&rest entries)
  `(let ((*maxsys/parameter-id-counter* -1)
	 (*maxsys/parameters* nil))
     ,@(maxsys/mk-params-helper entries)
     (reverse *maxsys/parameters*)))

(defun maxsys/mk-module (id name params &rest rest)
  (let ((head (list (cons :name name)
		    (cons :id id)
		    (cons :params params)))
	(tail (zsy/plist->alist rest)))
    (append head tail)))


;;; module part - similar to parameter part
(defun maxsys/make-module (name params &rest rest-args)
  (setq *maxsys/module-id-counter*
	(let ((module-id (plist-get rest-args :id)))
	  (if module-id
	      module-id
	    (+ *maxsys/module-id-counter* 1))))
  (let ((p `((:id . ,*maxsys/module-id-counter*)
	     (:name . ,name)
	     (:type . :module)
	     (:params . ,params)))
	(rest-alist (zsy/plist->alist rest-args)))
    ;; filter :id key
    (setq rest-alist (assq-delete-all :id rest-alist))
    (setq *maxsys/modules*
	  (cons (append p (reverse rest-alist))
		*maxsys/modules*))))

(defun maxsys/make-modules (form &optional make-module-func)
  "handling multiple module list"
  (let ((make-module-func (or make-module-func
			      'maxsys/make-module))
	(tag (car form)))
    (if (consp tag)
	(dolist (subform form)
	  (apply make-module-func subform))
      (apply make-module-func form))))

(defun maxsys/mk-modules-helper (entries &optional make-module-func)
  (let ((make-module-func (or make-module-func
			      'maxsys/make-module))
	(modules-form nil))
    (dolist (entry entries modules-form)
      (let (to-cons-form)
	;; entry can be in `(:name ...)' raw parameter form
	;; or `variable-term' form
	;; or `(func ...)' form
	(if (consp entry)
	    (let ((name (car entry))
		  (rest (cdr entry)))	      
	      (setq to-cons-form
		    (if (keywordp name)
			`(list ,@entry)
		      entry)))
	  (setq to-cons-form entry))
	(setq modules-form
	      (cons `(maxsys/make-modules ,to-cons-form (quote ,make-module-func))
		    modules-form))))
    (reverse modules-form)))

(defmacro maxsys/mk-modules (&rest entries)
  `(let ((*maxsys/module-id-counter* -1)
	 (*maxsys/modules* nil))
     ,@(maxsys/mk-modules-helper entries)
     (reverse *maxsys/modules*)))

;;; controller modules
(defun maxsys/make-ctrl (id min max &rest rest-args)
  (let ((ctrl (list
	       (cons :id id)
	       (cons :min min)
	       (cons :max max)))
	(rest-alist (zsy/plist->alist rest-args)))
    (setq *maxsys/ctrls*
	  (cons ctrl *maxsys/ctrls*))))

(defmacro maxsys/mk-ctrls (&rest entries)
  `(let ((*maxsys/ctrls* nil))
     ,@(maxsys/mk-params-helper entries 'maxsys/make-ctrl)
     (reverse *maxsys/ctrls*)))

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
    (setq *maxsys/ctrl-modules*
	  (cons (append p (reverse rest-alist))
		*maxsys/ctrl-modules*))))

(defmacro maxsys/mk-ctrl-modules (&rest entries)
  `(let ((*maxsys/ctrl-module-id-counter* -1)
	 (*maxsys/ctrl-modules* nil))
     ,@(maxsys/mk-modules-helper entries 'maxsys/make-ctrl-module)
     (reverse *maxsys/ctrl-modules*)))

;;; controller <-> parameter part
(defun maxsys/make-parameter-ctrl-mapping (param-name
					   ctrl-module-name
					   ctrl-id
					   mapping-mode
					   &rest rest-args)
  (let ((mapping (list
		  (cons :param-name param-name)
		  (cons :ctrl-module-name ctrl-module-name)
		  (cons :ctrl-id ctrl-id)
		  (cons :mapping-mode mapping-mode)))
	(rest-alist (zsy/plist->alist rest-args)))
    (setq *maxsys/parameter-ctrl-mappings*
	  (cons mapping *maxsys/parameter-ctrl-mappings*))))

(defmacro maxsys/mk-parameter-ctrl-mappings (&rest entries)
  `(let ((*maxsys/parameter-ctrl-mappings* nil))
     ,@(maxsys/mk-params-helper entries 'maxsys/make-parameter-ctrl-mapping)
     (reverse *maxsys/parameter-ctrl-mappings*)))

(defun maxsys/make-parameter-ctrl-mapping-module (name mappings &rest rest-args)
  (let ((p (list (cons :name name)
		 (cons :type :parameter-ctrl-mapping-module)
		 (cons :mappings mappings)))
	(rest-alist (zsy/plist->alist rest-args)))
    (setq *maxsys/parameter-ctrl-mapping-modules*
	  (cons (append p (reverse rest-alist))
		*maxsys/parameter-ctrl-mapping-modules*))))

(defmacro maxsys/mk-parameter-ctrl-mapping-modules (&rest entries)
  `(let ((*maxsys/parameter-ctrl-mapping-modules* nil))
     ,@(maxsys/mk-modules-helper entries 'maxsys/make-parameter-ctrl-mapping-module)
     (reverse *maxsys/parameter-ctrl-mapping-modules*)))

;;; transformer

(defun maxsys/-module->pattrmap (module)
  "convert a module's parameters to a map for max's pattr system"
  (let ((params (alist-get :params module)))
    (seq-map
     (lambda (param)
       (cons (maxsys/get-scriptname param)
	     param))
     params)))

(defun maxsys/modules->pattrmap (modules)
  "convert modules to a map for max's pattr system

Parameters and modules are identified by their scriptname, which
by default are their names but this behavior can be customized by
supplying individual functions"
  (seq-map
   (lambda (module)
     (let* ((module-scriptname (maxsys/get-scriptname module))
	    (params-map (maxsys/-module->pattrmap module)))
       (cons module-scriptname params-map)))
   modules))

;;; file writers

(defun maxsys/-parameter->variable-line (param &optional prefix)
  (let* ((param-line-tmpl "k_%s%s_idx = %d;")
	 (param-prefix (if prefix
			   (concat prefix "_")
			 ""))
	 (param-id (alist-get :id param))
	 (param-str (zsy/keyword-name (alist-get :name param)))
	 (param-variable-name (s-replace "-" "_" param-str)))
    (format param-line-tmpl
	    param-prefix
	    param-variable-name
	    param-id)))

(defun maxsys/-module->variable-block (module)
  (let* ((module-id (alist-get :id module))
	 (module-name (zsy/keyword-name (alist-get :name module)))
	 (params (alist-get :params module))
	 (param-prefix (alist-get :param-prefix module))
	 (params-lines (seq-map
			(lambda (param)
			  (maxsys/-parameter->variable-line param
							    param-prefix))
			params))
	 (params-text (s-join "\n" params-lines))
	 (module-header (format "// module %d: %s\n" module-id module-name)))
    (s-concat module-header params-text "\n\n")))

(defun maxsys/construct-variable-text (modules)
  (let* ((modules-texts (seq-map 'maxsys/-module->variable-block
				 modules)))
    (s-join "\n" modules-texts)))

(defun maxsys/mk-files (modules)
  (let ((dir-path default-directory)
	(module-in-json (let ((json-encoding-pretty-print t))
			  (json-encode modules))))
    (write-region (maxsys/construct-variable-text modules) nil "vars.txt")
    (write-region module-in-json nil "../patchers/modules.json")
    nil))

(provide 'maxsys)
