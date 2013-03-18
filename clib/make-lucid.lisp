;;;  $RCSfile: make-lucid.lisp,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:37 $  
;;; Compile Lucid CL ISIS interface.
;;; Robert Cooper
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
;;; This is pointed to by a symbolic link in SUN3/allegro_clib/make-lucid.cl
;;; It is easiest to startup lucid from SUN3/allegro_clib and then 
;;; (load "make-lucid")

    (in-package "USER")
    (compile-file "isis.lisp"
                  :output-file "isis.sbin")
    (compile-file "isis-task.lisp"
                  :output-file "isis-task.sbin")
    (compile-file "isis-msg.lisp"
                  :output-file "isis-msg.sbin")
    (compile-file "isis-tools.lisp"
                  :output-file "isis-tools.sbin")
    (compile-file "isis-c-load.lisp"
                  :output-file "isis-c-load.sbin")
