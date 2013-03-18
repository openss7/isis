;;;  $RCSfile: isis.lisp,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:27 $  
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
;;; Lucid-CL ISIS interface.
;;;

(in-package "ISIS")
(provide 'isis)

;;; EXPORTS
(export '(isis-init
          object-to-id id-to-object delete-id
         ))

;;; Load other lisp files in this package.
(defparameter *isis-dir* "/usr/u/isis")
   ;; Sometime we'll switch to a proper load-path, auto-compile scheme
(require 'isis-task "isis-task")
(require 'isis-msg "isis-msg")
(require 'isis-tools "isis-tools")

;;; Generation of object ids.
;;; These ids must be ODD, since we use them for function objects, and
;;; ISIS wants to differenticate function object-ids from regular
;;; C function pointers (which are even on all the architectures we
;;; currently support). See cl_task.h.

(let ((last-id -1))
  (defun new-id ()
    (if (typep (setq last-id (+ last-id 2)) 'fixnum)
        last-id
        (progn
          (warn "Warning: object-to-id is now reusing ids starting at 1")
          (setq last-id 1)))))

;;; Mapping from lisp objects to ISIS ids. 
;;; Multiple calls to object-to-id for the same (eql) object will return
;;; the same id value.
;;; We could be smart here and avoid storing stationary values
;;; since they won't move.

(defvar *isis-lisp-objects*)

(defun register-isis-fun (f)
  (object-to-id f))

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
;;; User callbacks.
;;;

(def-foreign-callable (call-lucid
                       (:return-type :arbitrary))
    ((rtn-id :fixnum) (n-args :fixnum)
     (arg1 :unsigned-32bit) (arg2 :unsigned-32bit)
     (arg3 :unsigned-32bit) (arg4 :unsigned-32bit))
  (let ((rtn (id-to-object rtn-id)))
    (ecase n-args
      (1 (funcall rtn arg1))
      (2 (funcall rtn arg1 arg2))
      (3 (funcall rtn arg1 arg2 arg3))
      (4 (funcall rtn arg1 arg2 arg3 arg4)))))

;;;
;;; Error handling.
;;;

(def-foreign-callable cl-panic ((char-ptr (:pointer :character)))
  (let ((mess (c-string-to-lisp-string char-ptr)))
    (error "~a" mess)))
  
;;;
;;; isis-init
;;;

(defvar *isis-inited* nil)
(defconstant *isis-max-procs* 64) ; Same as constant MAX_PROCS in isis.h
(defconstant *isis-max-sites* 127)  ;; Same as MAX_SITES in isis.h

(defun isis-init (port-nr)
  "Register this client with the ISIS system"
  ;;; If we are already inited we should really finalize and then
  ;;; re-initialize. That's requires lots of work on the ISIS clib
  ;;; so for now we just avoid reiniting.
  (if (not *isis-inited*)
      (progn
        (isis-task-init)
        (setq *isis-lisp-objects* nil)
        (if (eql port-nr -1)  
            (t-init)            ; Test task system without connecting to isis.
            (isis-init-c port-nr)) ; Full init.
        (setq *isis-inited* t))))
  
;;; isis-init should recover from "already connected" error and give user
;;; the choice of doing a logical kill/restart process. (Via continuable error
;;; facility)

;;; Load ISIS C libraries and do defforeigns of C functions we call.
;;; Do this last to avoid intermediate undefined symbol messages.
(require 'isis-c-load "isis-c-load")

(defmacro isis-trace (fun)
  `(defadvice (,fun isis-tracing) (&rest args)
    (format t "~s thread ~d calling ~s~%"
     *current-process* *isis-thread-id*
     (cons (quote ,fun) args))
    (let ((result (apply-advice-continue args)))
      (format t "~s thread ~d returning from ~s with ~s~%"
              *current-process* *isis-thread-id*
              (quote ,fun) result)
      result)))

;(defun my-process-lock (l)
;  (format t "lock contains ~s~%" l)
;  (if (eql l *current-process*)
;      (error "ALREADY LOCKED BY ME ~S" l)
;      (let ((result (process-lock l)))
;        (format t "lock now contains ~s~%" l)
;        result)))

;(defun my-process-unlock (l)
;  (format t "lock contains ~s~%" l)
;  (if (not (eql l *current-process*))
;      (error "NOT LOCKED BY ME ~S" l)
;      (let ((result (process-unlock l)))
;        (format t "lock now contains ~s~%" l)
;        result)))

(defun do-traces ()
  (isis-trace isis::cl-mutex-lock )
  (isis-trace isis::cl-mutex-unlock )
  (isis-trace isis::cl-thread-free )
  (isis-trace isis::cl-thread-wait )
  (isis-trace isis::cl-thread-signal)
  (isis-trace isis::cl-thread-self)
  (isis-trace isis::cl-thread-fork)
  (isis-trace isis::cl-thread-cleanup)
  (trace isis::cl-panic)
  (isis-trace isis::cl-thread-wait-fun)
  (isis-trace isis::register-isis-fun)
  (isis-trace isis::object-to-id)
  (isis-trace isis::process-suspend)
  (isis-trace isis::process-run)
  (isis-trace isis::invoke)
  (trace make-process)
  (trace isis::call-lucid)
;  (isis-trace my-process-lock)
;  (isis-trace my-process-unlock)
  )


;  (defadvice (process-lock isis-tracing) (l)
;    (format t "~s calling process-lock~%" *current-process*)
;    (if (eql l *current-process*)
;    (let ((result (advice-continue l)))
;      (format t "~s returning from process-lock with ~s~%"
;              *current-process* result)
;      result))

;  (defadvice (process-unlock isis-tracing) (l)
;    (format t "~s calling process-unlock~%" *current-process*)
;    (if (not (eql l *current-process*))
;      (error "NOT LOCKED BY ME ~s" l))
;    (let ((result (advice-continue l)))
;      (format t "~s returning from process-unlock with ~s~%"
;              *current-process* result)
;      result))
                                                                                                                                                                                                                                                               
