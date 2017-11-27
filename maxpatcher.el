;;; -*- lexical-binding: t -*-
;;; define parameters of a max/msp system and export them to json files

(require 'json)
(require 's)
(require 'seq)
(require 'zsy)
(require 'eieio)

(defvar maxpatcher/*patch-state*)

(defvar maxpatcher/*fileversion* 1)
(defvar maxpatcher/*appversion* '((:major . 7)
				  (:minor . 3)
				  (:revision . 4)
				  (:architecture . "x86")
				  (:modernui . 1)))

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

(defclass maxpatcher/<box> ()
  ((id :initarg :id
       :initform nil
       :documentation "")))


(defmethod initialize-instance :after ((box maxpatcher/<box>) &rest slots)
  (unless (oref box :id)
    (let ((new-id (maxpatcher/inc-box-id-counter maxpatcher/*patch-state*)))
      (oset box :id new-id))))

(setq o (make-instance maxpatcher/<box>))
(oref o :id )

(defmacro maxpatcher/with-patch (&rest body)
  `(let ((maxpatcher/*patch-state* (make-instance 'maxpatcher/<patch-state>)))
     (unwind-protect
	 (progn
	   ,@body)
       (maxpatcher/clear-state maxpatcher/*patch-state*))))

(maxpatcher/with-patch
 (make-instance 'maxpatcher/<box>)
 (make-instance 'maxpatcher/<box>)
 (make-instance 'maxpatcher/<box>))
(provide 'maxpatcher)
