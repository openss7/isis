;;;  $RCSfile: make-allegro.cl,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:35 $  
;;; Compile Allegro CL ISIS interface.
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
;;; This is pointed to by a symbolic link in SUN3/allegro_clib/make-allegro.cl
;;; It is easiest to startup allegro from SUN3/allegro_clib and then 
;;; :ld make-allegro

    (compile-file "isis.cl"        :output-file "isis.fasl")
    (compile-file "isis-task.cl"   :output-file "isis-task.fasl")
    (compile-file "isis-msg.cl"    :output-file "isis-msg.fasl")
    (compile-file "isis-tools.cl"  :output-file "isis-tools.fasl")
    (compile-file "isis-c-load.cl" :output-file "isis-c-load.fasl")
