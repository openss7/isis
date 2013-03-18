;;;  $RCSfile: isis.cl,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:22 $  
;;; -*- Mode:Lisp; Package:ISIS; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-
;;; Coded by Robert Cooper
;;; 
;;;
;;;
;;;      ISIS release V2.0, May 1990
;;;      Export restrictions apply
;;;
;;;      The contents of this file are subject to a joint, non-exclusive
;;;      copyright by members of the ISIS Project.  Permission is granted for
;;;      use of this material in unmodified form in commercial or research
;;;      settings.  Creation of derivative forms of this software may be
;;;      subject to restriction; obtain written permission from the ISIS Project
;;;      in the event of questions or for special situations.
;;;      -- Copyright (c) 1990, The ISIS PROJECT
;;;
;;;
;;; allegro-CL ISIS interface.
;;;

(in-package "ISIS")
(provide 'isis)

;;; EXPORTS
(export '(isis-init
          object-to-id id-to-object delete-id
         ))

;;; USE/IMPORTS
(require 'foreign)
(require 'trace)
(use-package :foreign-functions)

;;; Load ISIS C libraries and do defforeigns of C functions we call.
(defparameter *isis-dir* "/usr/u/isis")
   ;; Sometime we'll switch to a proper load-path, auto-compile scheme

(require 'isis-c-load "isis-c-load")

;;; Load other files in this package.
(require 'isis-task "isis-task")
(require 'isis-msg "isis-msg")
(require 'isis-tools "isis-tools")

;;;
;;; Generation of object ids.
;;;
(let ((last-id 0))
  (defun new-id ()
    (if (typep (setq last-id (+ last-id 1)) 'fixnum)
        last-id
        (progn
          (warn "Warning: object-to-id is now reusing ids starting at 1")
          (setq last-id 1)))))

;;; Mapping from lisp objects to ISIS ids. 
;;; Multiple calls to object-to-id for the same (eql) object will return
;;; the same id value.
;;; We could be smart here and avoid storing atomic values
;;; since they won't change.

(defvar *isis-lisp-objects*)

(defun object-to-id (obj)
  (let ((pair (rassoc obj *isis-lisp-objects*)))
    (if pair
        (car pair)                         ; Previously registered id.
        (let ((id (new-id)))
          (setq *isis-lisp-objects* (acons id obj *isis-lisp-objects*))
          id))))

(defun delete-id (id)
  (let ((pair (assoc id *isis-lisp-objects*)))
    (if pair
        (setf (cdr pair) 'deleted-object)
        (error "isis:id-to-object given bad id ~s" id))))

(defun id-to-object (id)
  (let* ((pair (assoc id *isis-lisp-objects*))
         (obj (cdr pair)))
    (if (and pair (not (eq obj 'deleted-object)))
        obj
        (error "isis:id-to-object given bad id ~s" id))))

;;;
;;; Mapping from c-callable lisp function objects to c-callable addresses.
;;;

(defvar *isis-lisp-funs*) ;; Association list of function objects
                          ;; and their C-callable addresses.
(defun register-isis-fun (f)
  "registers f and returns the c-callable function address"
  (declare (function f))
  (let ((pair (assoc f *isis-lisp-funs*)))
    (if pair
        (cdr pair)                         ; Previously registered address.
        (let ((address (values (register-function f))))
          (setq *isis-lisp-funs* (acons f address *isis-lisp-funs*))
          address))))
          

;;;
;;; Error handling.
;;;

(defun-c-callable cl-panic ((char-ptr :unsigned-long))
  (let ((mess (c-string-to-lisp-string char-ptr)))
    (error "~a" mess)))
  
;;;
;;; Tell the C world about the C-callable lisp functions.
;;;

(defvar *isis-inited* nil)
(defconstant *isis-max-procs* 64) ; Same as constant MAX_PROCS in isis.h
(defconstant *isis-max-sites* 127)  ;; Same as MAX_SITES in isis.h

(defcstruct (lisp-fun :malloc)
  (mutex-lock :long)
  (mutex-unlock :long)
  (thread-self :long)
  (thread-wait :long)
  (thread-signal :long)
  (thread-fork :long)
  (thread-free :long)
  (thread-cleanup :long)
  (thread-wait-fun :long)
  (thread-yield :long)
  (panic :long))
  
(defun isis-init (port-nr)
  "Register this client with the ISIS system"
  (if *isis-inited*
      (t-final))

  (isis-task-init)
  (setq *isis-lisp-funs* nil)
  (setq *isis-lisp-objects* nil)
  ;;  Should be able to do this with a macro or a map or something!
  (let ((funs (make-lisp-fun)))
    (setf (lisp-fun-mutex-lock funs) 	(register-isis-fun 'cl-mutex-lock))
    (setf (lisp-fun-mutex-unlock funs) 	(register-isis-fun 'cl-mutex-unlock))
    (setf (lisp-fun-thread-self funs) 	(register-isis-fun 'cl-thread-self))
    (setf (lisp-fun-thread-wait funs) 	(register-isis-fun 'cl-thread-wait))
    (setf (lisp-fun-thread-signal funs) (register-isis-fun 'cl-thread-signal))
    (setf (lisp-fun-thread-fork funs) 	(register-isis-fun 'cl-thread-fork))
    (setf (lisp-fun-thread-free funs) 	(register-isis-fun 'cl-thread-free))
    (setf (lisp-fun-thread-cleanup funs)(register-isis-fun 'cl-thread-cleanup))
    (setf (lisp-fun-thread-wait-fun funs)
                                       (register-isis-fun 'cl-thread-wait-fun))
    (setf (lisp-fun-thread-yield funs)  (register-isis-fun 'cl-thread-yield))
    (setf (lisp-fun-panic funs) 	(register-isis-fun 'cl-panic))

    (isis-init-lisp-funs funs))
  (if (eql port-nr -1)  
      (t-init)               ; Test task system without connecting to isis.
      (isis-init-c port-nr)) ; Full init.
  (setq *isis-inited* t))
  
;;; isis-init should recover from "already connected" error and give user
;;; the choice of doing a logical kill/restart process. (Via continuable error
;;; facility)

;;; :trace cl-mutex-lock cl-mutex-unlock cl-thread-free cl-thread-wait cl-thread-signal cl-thread-self cl-thread-fork cl-panic cl-thread-wait-fun
