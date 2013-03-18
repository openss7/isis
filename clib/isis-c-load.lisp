;;;  $RCSfile: isis-c-load.lisp,v $ $Revision: 2.92 $ $Date: 90/08/27 13:28:46 $  
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
;;; Lucid-CL ISIS interface: foreign function definition.
;;;

(in-package "ISIS")
(provide 'isis-c-load)

;;; EXPORTS: ISIS C routines which can be called directly from lisp.
(export '(address save-address my-host site-name 
          my-address condition
          my-host my-process-id my-site-id my-site-incarn my-site-no
          nulladdress
          groupview-viewid groupview-incarn groupview-flag groupview-gaddr
          groupview-joined groupview-departed
          siteview-viewid siteview-slist siteview-incarn siteview-failed
          siteview-recovered
          x-list-items-id
          addr-cmp addr-isequal addr-isnull addr-ismine
          alist-len
          isis-accept-events isis-decon-wait isis-disconnect isis-input-drain
          isis-logentry
          isis-perror
          isis-sleep
          isis-start-done
          pg-getlocalview pg-getview pg-local-lookup
          pg-rank pg-rank-all
          cl-dump
          run-tasks
          pg-addclient pg-delclient pg-delete pg-leave
          pg-lookup
          pg-monitor-cancel pg-signal pg-unmonitor
          abortreply
          simple-bcast simple-cbcast simple-fbcast simple-abcast simple-gbcast
          flush
          forward nullreply
          simple-reply
          paddr peid pmsg
          site-getview
          sv-monitor-cancel sv-monitor-cancel
          pg-join-inhibit pg-client
          xfer-out
          pg-watch-cancel proc-watch-cancel
          isis-print
          log-recovered log-checkpoint log-flush log-write
          rmgr-register rmgr-unregister rmgr-update
          x-abort x-begin x-commit x-getid x-outcomes x-outcomes-flush
          t-holder t-pass t-request
          msg-newmsg msg-getlen msg-getreplyto msg-getsender msg-gettruesender
          msg-gettype msg-getscantype msg-increfcount msg-isforwarded
          msg-msgcheck
          msg-copy msg-delete
          msg-fread msg-fwrite msg-read msg-write msg-rewind
          
          free  ;; Of course its the user's problem to decide when to
                   call this!
          ))
          
;;; Tell lisp about the ISIS C functions.
;;; Most of the following functions are directly callable by application
;;; level lisp code.
;;; Those that have lisp wrapping functions to do register_value etc
;;; have the comment "; wrapped" after them.

(def-foreign-function  ; How to make a C programmer happy in lisp:
    bcopy (from) (to) (nbytes :fixnum)) ; Expert use only!

(defmacro def-foreign-list (funs)
  (list* 'progn
         (mapcar #'(lambda (fun)
                   (list* 'def-foreign-function fun))
                 funs)))

(def-foreign-list 
    (
      ;;  lisp_lib.o
      ((condition    (:name "_make_condition")
                     (:return-type :pointer)))
      ((address      (:name "_make_address")
                     (:return-type (:pointer address)))
                     (site :fixnum)
                     (incarn :fixnum)
                     (pro :fixnum)
                     (entry :fixnum))
      ((save-address (:return-type (:pointer address)))
                     (addr))
      ((my-host      (:name "_get_my_host")
                     (:return-type :simple-string)))
      ((site-name    (:name "_get_site_name")
                     (:return-type :simple-string))
                     (site-no :fixnum))
      ;; Access functions for groupviews. We could use def-foreign-struct
      ;; but since these C routines already exist we might as well use
      ;; them. It also avoids the need to update lisp code whenever the
      ;; struct changes.
      ((groupview-viewid (:name "_get_gv_viewid"))
                         (gview))
      ((groupview-incarn (:name "_get_gv_incarn"))
                         (gview))
      ((groupview-flag   (:name "_get_gv_flag"))
                         (gview))
      ((groupview-gaddr  (:name "_get_gv_gaddr"))
                         (gview))
      ((groupview-name   (:name "_get_gv_name")
                         (:return-type :simple-string))
                         (gview))
      ((get-gv-nclient   (:return-type :fixnum))
                         (gview))
      ((get-gv-clients   (:return-type :unsigned-32bit))
                         (gview))
      ((get-gv-nmemb     (:return-type :fixnum))
                         (gview))
      ((get-gv-members   (:return-type :unsigned-32bit))
                         (gview))
      ((groupview-joined (:name "_get_gv_joined")
                         (:return-type (:pointer address)))
                         (gview))
      ((groupview-departed (:name "_get_gv_departed")
                         (:return-type (:pointer address)))
                         (gview))
      ((siteview-viewid  (:name "_get_sv_viewid")
                         (:return-type :fixnum))
                         (sview))
      ((siteview-slist   (:name "_get_sv_slist")
                         (:return-type (:pointer :signed-16bit)))
                         ;; Access using c-short-vector-ref
                         (sview))
      ((siteview-incarn  (:name "_get_sv_incarn")
                         (:return-type (:pointer :signed-8bit)))
                         ;; Access using c-byte-vector-ref
                         (sview))
      ((siteview-failed  (:name "_get_sv_failed")
                         (:return-type :pointer))
                         ;; Need access functions for bitvecs.
                         (sview))
      ((siteview-recovered (:name "_get_sv_recovered")
                         (:return-type :pointer))
                         ;; Need access functions for bitvecs.
                         (sview))
      ((x-list-items-id  (:name "_get_x_list_items_id")
                         (:return-type (:pointer address)))
                         (xlist) (i :fixnum))
      ;;  flib.o
      ((my-address       (:name "_myaddress_")
                         (:return-type (:pointer address))))
      ((my-process-id    (:name "_mypid_")
                         (:return-type :fixnum)))
      ((my-site-id       (:name "_mysid_")
                         (:return-type :fixnum)))
      ((my-site-incarn   (:name "_mysincarn_")
                         (:return-type :fixnum)))
      ((my-site-no       (:name "_mysno_")
                         (:return-type :fixnum)))
      ((nulladdress      (:name "_nulladdress_")
                         (:return-type (:pointer address))))
      ))

(def-foreign-list
    (
      ;; tk_rexec.o 
      ((isis-rexec-c     (:name "_isis_rexec")             ; wrapped
                         (:return-type :fixnum))
                         (nwanted :fixnum) (gid)
                         (sites) (prog :simple-string)
                         (args (:array :simple-string))
                         (env (:array :simple-string))
                         (user :simple-string) (passwd :simple-string)
                         (addrs (:array address)))
      ;; tk_authen.o
      ;; cl_isis.o 
      ((isis-select-from-lisp (:return-type :fixnum))) ; used internally
      ((addr-cmp         (:name "_addr_cmp_fun")
                         (:return-type :fixnum))
                         (a1) (a2))
      ((addr-isequal     (:return-type :boolean))
                         (a1) (a2))
      ((addr-ismine      (:return-type :boolean))
                         (a))
      ((addr-isnull      (:name "_addr_isnull_fun")
                         (:return-type :boolean))
                         (a))
      ((alist-len        (:return-type :fixnum))
                         (alist))
      ((isis-accept-events (:name "_isis_accept_events"))
                         (flag :fixnum))
      (isis-decon-wait   (nwant :fixnum))
      (isis-disconnect)
      ((isis-entry-c     (:name "_isis_entry"))                  ; wrapped
                         (entry :fixnum) (rtn :fixnum) (rname :simple-string))
      ((isis-init-c      (:name "_isis_init")
                         (:return-type :fixnum))
                         (port-no :fixnum))                          ; wrapped
      ((isis-input-c     (:name "_isis_input_fun"))                  ; wrapped
                         (file-des :fixnum) (handler :fixnum) (arg :fixnum))
      ((isis-output-c    (:name "_isis_output_fun"))                 ; wrapped
                         (file-des :fixnum) (handler :fixnum) (arg :fixnum))
      ((isis-signal-c    (:name "_isis_signal_fun"))                 ; wrapped
                         (signo :fixnum) (handler :fixnum) (arg :fixnum))
      ((isis-chwait-c    (:name "_isis_chwait_fun"))                 ; wrapped
                         (handler :fixnum) (arg :fixnum))
      ((isis-input-sig-c (:name "_isis_input_sig_fun"))              ; wrapped
                         (file-des :fixnum) (cond (:pointer condition))
                         (arg :fixnum))
      ((isis-output-sig-c (:name "_isis_output_sig_fun"))            ; wrapped
                         (file-des :fixnum) (cond (:pointer condition))
                         (arg :fixnum))
      ((isis-signal-sig-c (:name "_isis_signal_sig_fun"))            ; wrapped
                         (signo :fixnum) (cond (:pointer condition))
                         (arg :fixnum))
      ((isis-chwait-sig-c (:name "_isis_chwait_sig_fun"))            ; wrapped
                         (cond (:pointer condition)) (arg :fixnum))
      (isis-input-drain)
      (isis-logentry     (gaddr) (entry-no :fixnum))
      ((isis-mainloop-c  (:name "_isis_mainloop"))
                         (routine :fixnum) (arg :fixnum))
      (isis-perror       (str :simple-string))
      ((isis-read        (:return-type :fixnum)))
      ((isis-sleep       (:name "_isis_sleep"))
                         (seconds :fixnum))

      ((isis-sleep-ms    (:return-type :fixnum))
                         (ms :fixnum))
      (isis-start-done)
      ((isis-task-c      (:name "_isis_task"))
                         (routine :fixnum) (rname :simple-string))
      ((isis-timeout-c   (:name "_isis_timeout")
                         (:return-type :fixnum))
                         (time :fixnum) (routine :fixnum)
                         (arg0 :fixnum) (arg1 :fixnum))
      (panic             &rest args)        ; Should just use error instead.
      (pentry            (entryno :fixnum))
      ((pg-getlocalview  (:return-type :pointer))
                         (gaddr))
      ((pg-getview        (:return-type :pointer))
                         (gaddr))
      ((pg-local-lookup   (:return-type :pointer))
                         (gname :simple-string))
      ((pg-rank          (:name "__pg_rank")
                         (:return-type :fixnum))
                         (gaddr) (who))
      ((pg-rank-all      (:name "__pg_rank_all")
                         (:return-type :fixnum))
                         (gaddr) (who))
      (pr-dump           (how :fixnum))
      (run-tasks)
))
      ;; cl_inter.o 
      ;; cl_bypass.o 
      ;; cl_pgroup.o 

(def-foreign-list
    (
     ((pg-addclient     (:return-type :fixnum))
      (gaddr)
      (paddr))
     ((pg-delclient     (:return-type :fixnum))
                         (gaddr)
                         (paddr))
      ((pg-delete        (:return-type :fixnum))
                         (gaddr))
      ((pg-leave         (:return-type :fixnum))
                         (gaddr))
      ((pg-lookup        (:return-type (:pointer address)))
                         (name :simple-string))
      ((pg-monitor-c     (:name "_pg_monitor")               ; wrapped
                         (:return-type :fixnum))
                         (gaddr)
                         (routine :fixnum) (arg :fixnum))
      ((pg-monitor-cancel (:return-type :fixnum))
                         (pwid :fixnum))
      ((pg-signal        (:return-type :fixnum))
                         (gaddr) (signo :fixnum))
      (pg-unmonitor      (gaddr))
      ;; cl_bcast.o 
      ((simple-abcast    (:name "_abcast")
                         (:return-type :unsigned-32bit))
                         &rest args)
      ((abcast-l         (:return-type :unsigned-32bit))         ; wrapped
                         &rest args)
      ((abortreply       (:return-type :unsigned-32bit))
                         (msg))
      ((simple-bcast     (:name "_bcast")
                         (:return-type :unsigned-32bit))
                         &rest args)
      ((bcast-l          (:return-type :unsigned-32bit))         ; wrapped
                         &rest args)
      ((simple-cbcast    (:name "_cbcast")
                         (:return-type :unsigned-32bit))
                         &rest args)
      ((cbcast-l         (:return-type :unsigned-32bit))         ; wrapped
                         &rest args)
      ((simple-fbcast    (:name "_fbcast")
                         (:return-type :unsigned-32bit))
                         &rest args)
      ((fbcast-l         (:return-type :unsigned-32bit))         ; wrapped
                         &rest args)
      (flush)
      (forward           (fmsg) (to)
                         (entry-no :fixnum) (cmsg))
      ((simple-gbcast    (:name "_gbcast")
                         (:return-type :unsigned-32bit))
                         &rest args)
      ((gbcast-l         (:return-type :unsigned-32bit))         ; wrapped
                         &rest args)
      ((nullreply        (:return-type :fixnum))
                         (msg))
      ((simple-reply     (:name "_reply"))
                         &rest args)
      (reply-l           &rest args)                             ; wrapped

      ;; cl_task.o 
      (invoke            (arg :unsigned-32bit))              ; used internally
      (isis-fork         (routine :fixnum) (arg :fixnum)
                         (msg))                    ; wrapped
      (isis-fork-urgent  (routine :fixnum) (arg :fixnum)
                         (msg))                    ; wrapped
      (t-final)          ; used internally
      (t-init)           ; used internally
      ((t-sig-c          (:name "_t_sig"))
                         (cond (:pointer condition))
                         (value :fixnum))                  ; wrapped
      ((t-sig-all-c      (:name "_t_sig_all"))
                         (cond (:pointer condition))
                         (value :fixnum))                  ; wrapped
      ((t-sig-urgent-c   (:name "_t_sig_urgent"))
                         (cond (:pointer condition))
                         (value :fixnum))                  ; wrapped
      ((t-wait-l         (:return-type :fixnum))
                         (cond (:pointer condition))
                         (why :simple-string))
))
      ;; cl_queues.o 
      ;; cl_dump.o 

(def-foreign-list
    (
      ((cl-dump          (:max-rest-args 4))
                         (level :fixnum) (why :simple-string)
                         &rest args)
      (paddr             (addr))
      (paddrs            (addrs)) ; Null terminated!!
      (pmsg              (msg))
      ;; cl_alloc.o 
      ;; cl_sview.o 
      ((site-getview     (:return-type :pointer)))
      ((sv-monitor-c     (:name "_sv_monitor")
                         (:return-type :fixnum))
                         (routine :fixnum) (arg :fixnum))         ; wrapped
      ((sv-monitor-cancel (:return-type :fixnum))
                         (svid :fixnum))
      ((sv-watch-c       (:name "_sv_watch")
                         (:return-type :fixnum))
                         (siteid :fixnum) (event :fixnum) (routine :fixnum)
                         (arg :fixnum))                   ; wrapped
      ((sv-watch-cancel  (:return-type :fixnum))
                         (svid :fixnum))
      ;; cl_news.o 
      (news-apost        (slist (:array :signed-16bit))
                                        ; Null terminated site-id list.
                         (subject :simple-string)
                         (mp)
                         (back :fixnum))
      ((news-cancel      (:return-type :fixnum))
                         (subject :simple-string))
      (news-clear        (slist (:array :signed-16bit))
                                        ; Null terminated site-id list.
                         (subject :simple-string))
      (news-clear-all    (slist (:array :signed-16bit))
                                        ; Null terminated site-id list.
                         (subject :simple-string))
      (news-post         (slist (:array :signed-16bit))
                                        ; Null terminated site-id list.
                         (subject :simple-string)
                         (mp)
                         (back :fixnum))
      ((news-subscribe   (:return-type :fixnum))
                         (subject :simple-string)
                         (entry-no :fixnum)
                         (back :fixnum))
      ;; cl_groups.o 
      ;; cl_join.o 
      ((allow-xfers-xd-c (:name "_allow_xfers_xd"))
                         (gname :simple-string)                ; wrapped 
                         (xd :fixnum) (send-rtn :fixnum) (recv_rtn :fixnum))
      ((pg-client        (:return-type :fixnum))
                         (gaddr)
                         (credentials :simple-string))
      ((pg-join-c        (:name "_pg_join")
                         (:return-type (:pointer address)))
                         &rest args)                           ; wrapped
      ((pg-join-inhibit-c (:name "_pg_join_inhibit"))
                         (flag :fixnum))                       ; wrapped
      ((pg-subgroup-c    (:name "_pg_subgroup")
                         (:return-type (:pointer address)))    ; wrapped
                         (gaddr) (sgname :simple-string)
                         (incarn :fixnum)
                         (mlist) (clist))
      ((xfer-flush       (:return-type :fixnum)))
      ((xfer-out         (:name "_do_xfer_out"))
                         (locator :fixnum) (mp))
      ;; cl_coord.o 
      ((cc-terminate     (:name "_cc_terminate_msg"))
                         (msg))
      ((coord-cohort-c   (:name "_coord_cohort") (:return-type :fixnum))
                         (msg) (gaddr)                         ; wrapped
                         (action :fixnum) (got-result :fixnum) (arg :fixnum))
      ((coord-cohort-l   (:return-type :fixnum))               ; wrapped
                         (msg) (gaddr)
                         (action :fixnum) (got-result :fixnum) (arg :fixnum)
                         (choose :fixnum))
      ))

(def-foreign-list
    (
      ;; cl_watch.o 
      ((pg-watch-c       (:name "_pg_watch")                   ; wrapped
                         (:return-type :fixnum))
                         (gaddr) (who)
                         (event :fixnum) (routine :fixnum) (arg :fixnum))
      ((pg-watch-cancel  (:return-type :fixnum))
                         (wid :fixnum))
      (proc-monitor-dump)
      ((proc-watch-c     (:name "_proc_watch")                 ; wrapped
                         (:return-type :fixnum))
                         (paddr) (routine :fixnum)
                         (arg :fixnum))
      ((proc-watch-cancel (:return-type :fixnum))
                         (wid :fixnum))
      ;; cl_print.o 

      ;; g_parse.o -- forget guards for now. 
      ;; g_eval.o 
      ;; cl_lmgr.o 
      ((log-checkpoint   (:return-type :fixnum))
                         (gname :simple-string))
      ((log-flush        (:return-type :fixnum))
                         (gaddr))
      ((log-recovered-c  (:name "_log_checkpoint")        ; wrapped
                         (:return-type :fixnum))
                         (gaddr))
      ((log-write        (:return-type :fixnum))
                         (gaddr)
                         (msg))
      ;; cl_setjmp.o 
      ;; tk_rmgr.o 
      ((rmgr-register    (:return-type :fixnum))
                         (key :simple-string))
      ((rmgr-unregister    (:return-type :fixnum)))
      ((rmgr-update      (:return-type :fixnum))
                         (key :simple-string) (program :simple-string)
                         (argv (:pointer :simple-string))
                         (env (:pointer :simple-string)))

      ;; cl_xaction.o 
      ((x-abort          (:return-type :fixnum)))
      ((x-begin          (:return-type :fixnum)))
      ((x-commit         (:return-type :fixnum))
                         (phases :fixnum))
      ((x-getid          (:return-type (:pointer address))))
      ((x-outcomes       (:return-type :pointer))
                         (part-name :simple-string))
      (x-outcomes-flush  (part-name :simple-string) (outcomes))
      ((x-term-msg       (:return-type :fixnum))
                         (part-name :simple-string) (on-prepare :fixnum)
                         (on-commit :fixnum) (on-abort :fixnum)
                         (msg))
      ;; cl_token.o 
      ((t-holder         (:return-type (:pointer address)))
                         (gaddr) (name :simple-string))
      ((t-pass           (:return-type :fixnum))
                         (gaddr) (name :simple-string))
      ((t-request        (:return-type :fixnum))
                         (gaddr) (name :simple-string)
                         (pass-on-fail :boolean))
      ))

      ;; history.o 
      ;; cl_failed.o 
      ;; cl_sundummy.o 

(def-foreign-list
    (
      ((msg-copy         (:return-type :pointer))
                         (msg))
      ((msg-delete       (:name "_MSG_DELETE"))
                         (msg))
      (msg-genmsg        &rest args)    ; Expert use only!
      ((msg-newmsg       (:return-type :pointer)))
      ;; msg_types.o  -- can't define new message field types froms lisp yet
      ;; msg_utils.o 
      ((msg-getdests-c   (:name "_msg_getdests_fun")            ; wrapped
                         (:return-type (:pointer address)))
                         (msg))
      ((msg-getlen       (:name "_msg_getlen_fun")
                         (:return-type :fixnum))
                         (msg))
      ((msg-getreplyto   (:name "_msg_getreplyto_fun")
                         (:return-type (:pointer address)))
                         (msg))
      ((msg-getsender    (:name "_msg_getsender_fun")
                         (:return-type (:pointer address)))
                         (msg))
      ((msg-gettruesender (:name "_msg_gettruesender_fun")
                         (:return-type (:pointer address)))
                         (msg))
      ((msg-gettype      (:return-type :fixnum))
                         (msg)
                         (field :signed-32bit)  ; Actually signed-8bit !
                         (inst :fixnum))
      ((msg-getscantype  (:return-type :fixnum))
                         (msg))
      ((msg-increfcount   (:name "_msg_increfcount_fun"))
                         (msg))
      ((msg-isforwarded  (:name "_msg_isforwarded_fun")
                         (:return-type :boolean))
                         (msg))
      (msg-msgcheck      (msg))
      ;; msg_fmt.o 
      ((msg-gen          (:return-type :pointer))
                         &rest args)    ; Expert use only!
      (msg-putfld        &rest args)    ; Expert use only!
      (msg-getfld        &rest args)    ; Expert use only!
      ((msg-get          (:return-type :fixnum))              ; wrapped
                         &rest args)
      ((msg-put          (:return-type :fixnum))              ; wrapped
                         &rest args)
      (msg-rewind        (msg))
      ;; msg_fio.o 
      ((msg-fread        (:return-type :pointer))
                         (file))
      ((msg-fwrite       (:return-type :fixnum))
                         (file) (msg))
      ((msg-read         (:return-type :pointer))
                         (file-desc :fixnum))
      ((msg-write        (:return-type :fixnum))
                         (file-desc :fixnum) (msg))
      ))
      

;; Must do the foreign load after the def-foreign-function calls.
(load-foreign-files "isis_c_refs.o" '("libisis1.a" "libisis2.a" "libisism.a" "-lc"))
  ;; This is actually non-standard. It should say "-lisis1 -lisis2 -lisism"
  ;; and redefine the library load path to find them.

(defun free (ptr)
  (free-foreign-pointer ptr))

;;; To Do: paddr, pmsg, and isis_print should be bracketed by lisp stream
;;; flush operations. Currently the lisp and C output gets scrambled.
;;; (Needs to be done in allegro as well.)
