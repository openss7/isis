;;;  $RCSfile: isis-tools.lisp,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:20 $  
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
;;; Lucid-CL ISIS interface: toolkit
;;;

(in-package "ISIS")         ;; Only part of the isis package actually.
(provide 'isis-tools)

;;; EXPORTS
(export '(isis-rexec
          pg-monitor
          pg-join pg-subgroup
          allow-xfers-xd
          coord-cohort
          sv-monitor sv-watch
          pg-watch proc-watch
          x-term
          x-item-id x-item-outcome x-item-info
          x-list-len
          ))

(defun isis-rexec (n-wanted gaddr sites prog args env user passwd)
  "Returns list of addresses where programs were started up"

  (declare (special *isis-max-sites*)) ; Defined in isis.cl
  (let ((avec (make-c-address-vector *isis-max-sites*)))
    (prog1
        (if (plusp
             (isis-rexec-c n-wanted gaddr
                           sites  ;; Obtained from siteview-slist
                           prog args env user passwd
                           avec))
            (c-address-vector-to-address-list avec)
            nil))
    (free avec)))

(defun pg-monitor (gaddr routine &optional (arg 0))
  (pg-monitor-c gaddr (register-isis-fun routine) arg))


;;; Constants for pg_join calls (from isis.h and lmgr.h).
(defconstant pg-dontcreate 7)
(defconstant pg-logged 6)
(defconstant l-auto 1)
(defconstant l-manual 0) 
(defconstant pg-init 1)
(defconstant pg-monitor 4)
(defconstant pg-xfer 2)
(defconstant pg-bigxfer 8)
(defconstant pg-credentials 5)
(defconstant pg-join-authen 3)
(defconstant pg-client-authen 10)
(defconstant xd-user 4)

(defun pg-join (gname
                &key dontcreate logged init monitor xfer bigxfer
                     credentials join-authen client-authen)
  (let ((args '(0)))
    (if dontcreate
        (setq args (cons pg-dontcreate
                         args)))
    (if logged
        (let ((fname (the string (first logged)))
              (replay-entry (the fixnum (second logged)))
              (flush-type (ecase (third logged)
                            (auto l-auto)
                            (manual l-manual)))
              (end-replay
               (let ((f (fourth logged)))
                 (if (typep f 'function)
                     (register-isis-fun f)
                     0))))
          (setq args (list* pg-logged fname replay-entry flush-type end-replay
                            args))))
    (if init
        (setq args
              (list* pg-init
                     (register-isis-fun
                      (the function (if (listp init)
                                        (first init)
                                        init)))
                     args)))
    (if monitor
        (let ((f (register-isis-fun
                      (the function (if (listp monitor)
                                        (first monitor)
                                        monitor))))
              (arg (if (listp monitor)
                       (second monitor)
                       0)))
          (setq args
                (list* pg-monitor f arg args))))
    (if xfer
        (let ((domain (the fixnum (first xfer)))
              (send-routine (register-isis-fun (the function (second xfer))))
              (recv-routine (register-isis-fun (the function (third xfer)))))
          (setq args (list* pg-xfer domain send-routine recv-routine
                            args))))
    (if bigxfer
        (setq args (cons pg-bigxfer args)))        
    (if credentials
        (setq args (list* pg-credentials (the string credentials)
                          args)))
    (if join-authen
        (setq args (list* pg-join-authen
                          (register-isis-fun (the function join-authen))
                          args)))
    (if client-authen
        (setq args (list* pg-client-authen
                          (register-isis-fun (the function client-authen))
                          args)))
    (apply #'pg-join-c gname args)
    ))

(defun pg-subgroup (pgaddr sgname incarn members clients)
  (pg-subgroup-c pgaddr sgname incarn
                 (address-list-to-c-address-vector members t)
                 (address-list-to-c-address-vector clients t)))

(defun allow-xfers-xd (gname xd send-routine)
  (allow-xfers-xd-c gname (+ xd-user xd)
                    (register-isis-fun (the function send-routine))
                    0))

(defun coord-cohort (msg gaddr action got-reply &optional choose)
  (declare (function action got-reply choose))
  (let ((action-f (register-isis-fun action))
        (got-reply-f (register-isis-fun got-reply)))
  (if choose
      (coord-cohort-l msg gaddr action-f got-reply-f 0
                      (register-isis-fun choose))
      (coord-cohort-c msg gaddr action-f got-reply-f 0))))

;;; Constants for watch calls (from isis.h)
(defconstant w-fail 0)
(defconstant w-recover 1)
(defconstant w-leave 2)
(defconstant w-join 3)
(defconstant w-monitor -1)

(defun sv-monitor (routine &optional (arg 0))
  (sv-monitor-c (register-isis-fun routine) arg))
(defun sv-watch (site-id event routine &optional (arg 0))
  (sv-watch-c site-id
              (ecase event
                (fail w-fail)
                (recover w-recover))
              (register-isis-fun routine)
              arg))

(defun pg-watch (gaddr who event routine &optional (arg 0))
  (declare (function routine))
  (pg-watch-c gaddr who
              (ecase event
                (leave w-leave)
                (join w-join))
              (register-isis-fun routine)
              arg))

(defun proc-watch (who routine &optional (arg 0))
  (declare (function routine))
  (proc-watch-c who (register-isis-fun routine) arg))

(defun x-term (participant-name on-prepare on-commit on-abort msg)
  (x-term-msg participant-name
              (register-isis-fun on-prepare)
              (register-isis-fun on-commit)
              (register-isis-fun on-abort)
              msg))

;;; Definition of transaction recovery records returned by x-outcomes.
(def-foreign-struct x-item
  (id      :type address)
  (outcome :type :signed-32bit)
  (info    :type (:pointer message))) 
(def-foreign-struct x-list
  (len     :type :signed-32bit) ;; Size of the following vector.
  (items   :type (:array x-item (1))))
