;;;  $RCSfile: isis-task.lisp,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:17 $  
;;; -*- Mode:Lisp; Package:ISIS; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-
;;; Coded by Robert Cooper
;;;
;;; Lucid-CL ISIS interface: task/thread support
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

(in-package "ISIS")         ;; Only part of the isis package actually.
(provide 'isis-task)

;;; We want to go: (shadow 'sleep "LISP") because we redefine sleep below.
;;; however we can't shadow symbols in the lisp package. Instead:
;;(unintern 'sleep "LISP")

;;; EXPORTS
(export '(isis-task isis-mainloop
          isis-entry defun-isis-entry defun-callback
          isis-input isis-output isis-signal isis-chwait
          isis-input-sig isis-output-sig isis-signal-sig isis-chwait-sig
          isis-signal
          sleep isis-timeout
          t-fork t-fork-urgent t-fork-msg t-fork-urgent-msg
          t-sig t-sig-all t-sig-urgent t-wait
          ))

;;;
;;; Individual task suspension. Should be called within "without-scheduling".
;;;

(defun process-suspend (p)
  (if (eq p *current-process*)
      (catch 'isis-thread-wakeup
        (system:process-wait-allowing-scheduling "isis-thread-suspended"
                                                 #'(lambda () nil)))
      (error "isis::process-suspend: argument must be the current process")))

;;; Trace around all process switches.
(defun advise-suspend ()
  (defadvice (process-suspend trace-switches) (p)
    (if (eq p *current-process*)
        (prog2
            (format t "<?~d?>" *isis-thread-id*)
            (advice-continue p)
          (format t "<!~d!>" *isis-thread-id*))
        (advice-continue p))))
;;;(advise-suspend)

(defun process-run (p)
  (interrupt-process p
      #'(lambda () (throw 'isis-thread-wakeup nil))))

;;; without-process-lock macro

(defmacro without-process-lock ((lock-form) &body body)
  `(unwind-protect
    (progn
      (if (eq ,lock-form *current-process*)
          (progn
;            (format t "unlocking ~s~%" ,lock-form)
            (process-unlock ,lock-form))
          (format t
                  "without-process-lock: Warning: ~s not locked by me~%"
                  ,lock-form))
      (progn ,@body))
    (if (not (eq ,lock-form *current-process*))
        (progn
;          (format t "relocking ~s~%" ,lock-form)
          (process-lock ,lock-form)))))

;;;
;;; ISIS task management functions, callable from C.
;;;
;; Actually for ISIS purposes there will only ever be one lock
;; so mutex_alloc is not needed and there need be no arg to 
;; mutex_lock/unlock.
;; (Further, all C code is executed without signals hence no lisp process switches
;; will occur. So we could avoid grabbing isis-mutex until a process was likely
;; to leave the C world via an upcall into the lisp world. This could save time
;; for simple ISIS calls. See isis_thread_enter and isis_thread_exit in cl_task.c)

(defvar *isis-mutex*)
(defmacro lock-val (l) `(car ,l))  ; Store the lock as the car of a list.

;;; Not needed in Lucid.
(defun isis-ticker ()
  "Kluge function needed to prod lisp scheduler"
  (loop
   (process-wait-with-timeout "tick" 1 #'(lambda () nil))))

(defun isis-task-init ()
  (setq *isis-mutex* (list nil)))
;;  (make-process :name "ISIS-ticker" :function #'isis-ticker))

(def-foreign-callable (cl-mutex-lock (:return-type :boolean)) ()
  (if (eq (lock-val *isis-mutex*) *current-process*)
      (error "cl-mutex-lock: already locked by me ~s" (lock-val *isis-mutex*))
      (process-lock (lock-val *isis-mutex*))))


(def-foreign-callable (cl-mutex-unlock (:return-type :boolean)) ()
  (if (eq (lock-val *isis-mutex*) *current-process*)
      (process-unlock (lock-val *isis-mutex*))
      (error "cl-mutex-unlock: not locked by me ~s" (lock-val *isis-mutex*))))

;;; Threads.
;;; The special variable *isis-thread-id* is locally bound in each 
;;; process to that process's thread id.

(defvar *isis-thread-id* 'unknown-thread)

(def-foreign-callable (cl-thread-self (:return-type :signed-32bit)) ()
  (if (eq *isis-thread-id* 'unknown-thread)
      (setq *isis-thread-id* (object-to-id *current-process*))
      *isis-thread-id*))

;;; Thread wait and signal just use the process run reasons mechanism. 

(def-foreign-callable (cl-thread-wait (:return-type :fixnum))
    ((thread-id :signed-32bit))
  (with-scheduling-inhibited
      (without-process-lock ((lock-val *isis-mutex*))
              (process-suspend (id-to-object thread-id))))
  0)

(def-foreign-callable (cl-thread-signal (:return-type :fixnum))
    ((thread-id :signed-32bit))
  (process-run (id-to-object thread-id))
  0)

;;; Thread initial function.

(defun initial-function (arg)
  (declare (special *isis-thread-id*))
  (let ((*isis-thread-id* (object-to-id *current-process*)))
    (setf (process-name *current-process*)
                        (format nil "isis-thread-~d" *isis-thread-id*))
    (unwind-protect
       (invoke arg)
      (if (eq (lock-val *isis-mutex*) *current-process*)
          (process-unlock (lock-val *isis-mutex*))))))

(def-foreign-callable (cl-thread-fork (:return-type :fixnum))
    ((arg :unsigned-32bit))
  (make-process :name "isis-thread-??"
                :function #'initial-function :args (list arg))
;  (process-allow-schedule)
;  (lisp:sleep 2)
  0)

(def-foreign-callable (cl-thread-free (:return-type :fixnum))
    ((thread-id :signed-32bit))
  (delete-id thread-id)
  0)

(def-foreign-callable (cl-thread-cleanup (:return-type :fixnum)) ()
  (let ((locker (lock-val *isis-mutex*)))
    (if (not (null locker))
        (process-unlock (lock-val *isis-mutex*) locker)))
  0)


;;; Select calls from ISIS.

(defun isis-select ()
  (if (zerop (isis-select-from-lisp))
      nil
      t))

(def-foreign-callable (cl-thread-wait-fun (:return-type :boolean)) ()
  (process-wait "ISIS doing a select" 'isis-select))

(def-foreign-callable (cl-thread-yield (:return-type :boolean)) ()
  (process-allow-schedule))

;;; User callable task functions
;;; In all cases where a routine and an argument would be passed in C
;;; its probably more convenient and efficient to use lisp closures.

(defun isis-task (handler name)
  (isis-task (register-isis-fun handler) name))

(defun isis-entry (entry handler &optional (name ""))
  (isis-entry-c entry
                (register-isis-fun handler)
                name))

(defmacro defun-callback (func args &body body)
  `(defun ,func ,args ,@body))

;;; Example:
;;(defun-isis-entry foo 3 (m)
;;  (print "foo called with message m"))

(defmacro defun-isis-entry (func entry (msg-arg) &body body)
  `(progn
    (defun ,func (,msg-arg)
      ,@body)
    (isis-entry ,entry ',func (symbol-name ',func))
    ',func))

(defun isis-input (file-des handler &optional (arg 0))
  (isis-input-c file-des (register-isis-fun handler) arg))
(defun isis-output (file-des handler &optional (arg 0))
  (isis-output-c file-des (register-isis-fun handler) arg))
(defun isis-signal (signo handler &optional (arg 0))
  (isis-signal-c signo (register-isis-fun handler) arg))
(defun isis-chwait (handler &optional (arg 0))
  (isis-chwait-c (register-isis-fun handler) arg))

(defun isis-input-sig (file-des condition &optional (arg 0))
  (isis-input-sig-c file-des condition (object-to-id arg)))
(defun isis-output-sig (file-des condition &optional (arg 0))
  (isis-output-sig-c file-des condition (object-to-id arg)))
(defun isis-signal-sig (signo condition &optional (arg 0))
  (isis-signal-sig-c signo condition (object-to-id arg)))
(defun isis-chwait-sig (condition &optional (arg 0))
  (isis-chwait-sig-c condition (object-to-id arg)))

(defun isis-mainloop (routine &optional (arg 0))
  (isis-mainloop-c (register-isis-fun routine) arg))

(defun sleep (secs) ; Redefine standard sleep function to call into ISIS.
  (isis-sleep secs))

(defun isis-timeout (milliseconds routine &optional (arg 0))
  (isis-timeout-c milliseconds (register-isis-fun routine) arg 0))

(defun t-fork (routine &optional (arg 0))
  (isis-fork (register-isis-fun routine) arg 0))
(defun t-fork-urgent (routine &optional (arg 0))
  (isis-fork-urgent (register-isis-fun routine) arg 0))
(defun t-fork-msg (routine msg)
  (isis-fork (register-isis-fun routine) 0 msg))
(defun t-fork-urgent-msg (routine msg)
  (isis-fork-urgent (register-isis-fun routine) 0 msg))

(defun t-sig (condition value)
  (t-sig-c condition (object-to-id value)))
(defun t-sig-all (condition value)
  (t-sig-all-c condition (object-to-id value)))
(defun t-sig-urgent (condition value)
  (t-sig-urgent-c condition (object-to-id value)))

(defun t-wait (condition &optional (why ""))
  (id-to-object (t-wait-l condition why)))
  ;; Unfortunately we can't call free-slot on the id here since
  ;; we may have been t-sig-all'ed.
