;;; -*- lexical-binding: t -*-
;;; define parameters of a max/msp system and export them to json files

(require 'json)
(require 's)
(require 'seq)
(require 'zsy)
(require 'eieio)

(defconst maxpatcher/+literal-maxclass+
  (list :bpatcher :button :comment :dial :flonum :function
	:message :multislider :number
	:slider :toggle)
  "some max objects have their own classes instead of `newclass'")

(defvar maxpatcher/*patch-state* nil)

(defvar maxpatcher/*fileversion* 1)
(defvar maxpatcher/*appversion* '((:major . 7)
				  (:minor . 3)
				  (:revision . 4)
				  (:architecture . "x86")
				  (:modernui . 1)))

(defun maxpatcher/id->obj-str (id)
  (format "obj-%d" id))

(defgeneric maxpatcher/to-alist (obj)
  "convert a box or a patch into alist map")

(defclass maxpatcher/<patch-state> ()
  ((box-id-counter :initarg :box-id-counter
		   :initform 0
		   :documentation "box id counter, auto inc")
   (boxes :initarg :boxes
	  :initform '()
	  :documentation "boxes of the patch")
   (lines :initarg :lines
	  :initform '()
	  :documentation "lines of the patch"))
  "record representing the state of a raw max patch")

(defgeneric maxpatcher/inc-box-id-counter (state &optional n)
  "increase box-id-counter by n")

(defmethod maxpatcher/inc-box-id-counter ((state maxpatcher/<patch-state>) &optional n)
  (let* ((current-id-counter (oref state :box-id-counter))
	 (new-id-counter (+ 1 current-id-counter)))
    (oset state :box-id-counter new-id-counter)
    new-id-counter))

(defgeneric maxpatcher/clear-state (state)
  "clear patch state")

(defmethod maxpatcher/clear-state ((state maxpatcher/<patch-state>))
  state)

(defgeneric maxpatcher/add-box (state box)
  "add a box to patcher state")

(defmethod maxpatcher/add-box ((state maxpatcher/<patch-state>) box)
  (let ((box-id (oref box :id))
	(state-id (oref state :box-id-counter)))
    (when (> box-id state-id)
      (oset state :box-id-counter box-id))
    (object-add-to-list state :boxes box)))

(defgeneric maxpatcher/add-line (state line)
  "add a line to patcher state")

(defmethod maxpatcher/add-line ((state maxpatcher/<patch-state>) line)
  (object-add-to-list state :lines line))

(defmethod maxpatcher/to-alist ((state maxpatcher/<patch-state>))
  (let ((main-alist (list (cons :fileversion maxpatcher/*fileversion*)
			  (cons :appversion maxpatcher/*appversion*)))
	(boxes-alist (seq-map 'maxpatcher/to-alist
			      (oref state :boxes)))
	(lines-alist (seq-map 'maxpatcher/to-alist
			      (oref state :lines))))
    (when boxes-alist
      (setq main-alist (cons (cons :boxes boxes-alist)
			     main-alist)))
    (when lines-alist
      (setq main-alist (cons (cons :lines lines-alist)
			     main-alist)))
    (list (cons :patcher main-alist))))

(defclass maxpatcher/<box> ()
  ((id :initarg :id
       :initform -1
       :type integer
       :documentation "object id, if ignored then use the auto inc box-counter")
   (maxclass :initarg :maxclass
	     :initform nil
	     :documentation "object maxclass, flonum, zl etc.")
   (text :initarg :text
	 :initform ""
	 :type string
	 :documentation)))

(defmethod initialize-instance :after ((box maxpatcher/<box>) &rest slots)
  (when (= -1 (oref box :id))
    (let ((new-id (maxpatcher/inc-box-id-counter maxpatcher/*patch-state*)))
      (oset box :id new-id))))

(defgeneric maxpatcher/get-text (box)
  "get box text")

(defun maxpatcher/literal-maxclass-p (class)
  (memq class maxpatcher/+literal-maxclass+))

(defmethod maxpatcher/get-text ((box maxpatcher/<box>))
  (let ((class (oref box :maxclass))
	(text (oref box :text)))
    (if (maxpatcher/literal-maxclass-p class)
	text
      (concat (zsy/keyword-name class)
	      " "
	      text))))

(defmethod maxpatcher/to-alist ((box maxpatcher/<box>))
  (let* ((id (oref box :id))
	 (maxclass (oref box :maxclass))
	 (maxclass-text (if (maxpatcher/literal-maxclass-p maxclass)
			    (zsy/keyword-name maxclass)
			  "newobj"))
	 (text (maxpatcher/get-text box)))
    (list (cons :box
		(list (cons :id (maxpatcher/id->obj-str id))
		      (cons :maxclass maxclass-text)
		      (cons :text text))))))

(defclass maxpatcher/<line> ()
  ((src-obj-id :initarg :src-obj-id
	       :type integer
	       :documentation "source object id")
   (src-obj-outlet :initarg :src-obj-outlet
		   :type integer
		   :documentation "source object outlet")
   (dest-obj-id :initarg :dest-obj-id
		:type integer
		:documentation "destination object id")
   (dest-obj-inlet :initarg :dest-obj-inlet
		   :type integer
		   :documentation "destination object inlet")))

(defmethod maxpatcher/to-alist ((line maxpatcher/<line>))
  (with-slots (src-obj-id
	       src-obj-outlet
	       dest-obj-id
	       dest-obj-inlet)
      line
    (list (cons :patchline
		(list (cons :source
			    (list (maxpatcher/id->obj-str src-obj-id)
				  src-obj-outlet))
		      (cons :destination
			    (list (maxpatcher/id->obj-str dest-obj-id)
				  dest-obj-inlet)))))))

(defun maxpatcher/connect (src-obj-id src-obj-outlet dest-obj-id dest-obj-inlet)
  (let ((line (make-instance 'maxpatcher/<line>
			     :src-obj-id src-obj-id
			     :src-obj-outlet src-obj-outlet
			     :dest-obj-id dest-obj-id
			     :dest-obj-inlet dest-obj-inlet)))
    (maxpatcher/add-line maxpatcher/*patch-state* line)))

(defmacro maxpatcher/with-canvas (&rest body)
  `(let ((maxpatcher/*patch-state* (make-instance 'maxpatcher/<patch-state>)))
     (unwind-protect
	 (progn
	   ,@body)
       (maxpatcher/clear-state maxpatcher/*patch-state*))))

;;; for testing now
(maxpatcher/with-canvas
 (make-instance 'maxpatcher/<box>)
 (make-instance 'maxpatcher/<box>)
 (setq myo
       (make-instance 'maxpatcher/<box> :maxclass :flonum))
 (maxpatcher/add-box maxpatcher/*patch-state* myo)
 (setq myo2
       (make-instance 'maxpatcher/<box> :maxclass :number))
 (maxpatcher/add-box maxpatcher/*patch-state* myo2)
 (maxpatcher/connect 3 0 4 0)
 (setq al (maxpatcher/to-alist maxpatcher/*patch-state*))
 (setq my-patch maxpatcher/*patch-state*))

(let ((json-encoding-pretty-print t))
  (zsy/write-string-to-file 
   (json-encode (maxpatcher/to-alist my-patch))
   "~/test.maxpat"))

(provide 'maxpatcher)
