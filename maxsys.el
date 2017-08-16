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

(defun maxsys/make-parameters (form)
  "handling multiple parameters list"
  (let ((tag (car form)))
    (if (consp tag)
	(dolist (subform form)
	  (apply 'maxsys/make-parameter subform))
      (apply 'maxsys/make-parameter form))))

(defun maxsys/mk-params-helper (entries)
  (let (params-form)
    (dolist (entry entries params-form)
      (let (to-cons-form)
	;; entry can be in `(:name ...)' raw parameter form
	;; or `variable-term' form
	;; or `(func ...)' form
	(if (consp entry)
	    (let ((param-name (car entry))
		  (rest (cdr entry)))	      
	      (setq to-cons-form
		    (if (keywordp param-name)
			`(list ,@entry)
		      entry)))
	  (setq to-cons-form entry))
	(setq params-form
	      (cons `(maxsys/make-parameters ,to-cons-form)
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

(defun maxsys/make-modules (form)
  "handling multiple module list"
  (let ((tag (car form)))
    (if (consp tag)
	(dolist (subform form)
	  (apply 'maxsys/make-module subform))
      (apply 'maxsys/make-module form))))

(defun maxsys/mk-modules-helper (entries)
  (let (modules-form)
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
	      (cons `(maxsys/make-modules ,to-cons-form)
		    modules-form))))
    (reverse modules-form)))

(defmacro maxsys/mk-modules (&rest entries)
  `(let ((*maxsys/module-id-counter* -1)
	 (*maxsys/modules* nil))
     ,@(maxsys/mk-modules-helper entries)
     (reverse *maxsys/modules*)))

;;; transformer

(defun maxsys/-module->pattrmap (module)
  "convert a module's parameters to a map for max's pattr system"
  (setq default-param-scriptname-func
	(or (alist-get :default-param-scriptname-func module)
	    (lambda (param) (alist-get :name param))))
  (let ((params (alist-get :params module)))
    (seq-map
     (lambda (param)
       (let ((param-scriptname-func
	      (or (alist-get :param-scriptname-func param)
		  default-param-scriptname-func)))
	 (cons (funcall param-scriptname-func param)
	       param)))
     params)))

(defun maxsys/modules->pattrmap (modules)
  "convert modules to a map for max's pattr system

Parameters and modules are identified by their scriptname, which
by default are their names but this behavior can be customized by
supplying individual functions"
  (seq-map
   (lambda (module)
     (let* ((module-scriptname-func
	     (or (alist-get :module-scriptname-func module)
		 (lambda (module)
		   (alist-get :name module))))
	    (module-scriptname (funcall module-scriptname-func module))
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
