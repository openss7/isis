;;;  $RCSfile: isis-c-load.cl,v $ $Revision: 2.92 $ $Date: 90/08/27 13:28:55 $  
;;; -*- Mode:Lisp; Package:ISIS; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-
;;; Coded by Robert Cooper
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
          
;;; USE/IMPORTS
(require 'foreign)
(use-package :foreign-functions)

(load "isis_c_refs.o" :foreign-files
      '("libisis1.a"
        "libisis2.a"
        "libisism.a" ))

;;; Tell lisp about the ISIS C functions.
;;; All C functions have _ in their names mapped to -.
;;; Many C functions are not directly callable from lisp, but need
;;; a wrapping function to do register_value etc. These C functions
;;; have the comment "; wrapped" after them in what follows.
;;; Where a name clash could occur between the C function and the 
;;; corresponding lisp function, the C function has "-c" appended to 
;;; its name;

(defforeign-list  ; How to make a C programmer happy in lisp:
    '((strlen :arguments (integer)) ; Expert use only!
      (strncpy :arguments t) ; Expert use only!
      (malloc :arguments (fixnum)) ; Expert use only!
      (free :arguments (integer))
      (bcopy :arguments t) ; Expert use only!
     ))

(defforeign-list 
    '(
      ;;  lisp_lib.o
      (condition        :entry-point "_make_condition" :arguments nil)
      (address          :entry-point "_make_address" 
                        :arguments (fixnum fixnum fixnum fixnum))
      (save-address     :entry-point "_save_address" 
                        :arguments (integer))
      (get-my-host      :entry-point "_get_my_host"
                        :arguments nil) ;wrapped
      (get-site-name    :entry-point "_get_site_name"
                        :arguments (fixnum)) ; wrapped
      (groupview-viewid :entry-point "_get_gv_viewid"
                        :arguments (integer))
      (groupview-incarn :entry-point "_get_gv_incarn"
                        :arguments (integer))
      (groupview-flag   :entry-point "_get_gv_flag"
                        :arguments (integer))
      (groupview-gaddr  :entry-point "_get_gv_gaddr"
                        :arguments (integer))
      (get-gv-name      :entry-point "_get_gv_name"
                        :arguments (integer)) ; wrapped
      (get-gv-nclient   :entry-point "_get_gv_nclient"
                        :arguments (integer)) ; wrapped
      (get-gv-clients   :entry-point "_get_gv_clients"
                        :arguments (integer)) ; wrapped
      (get-gv-nmemb     :entry-point "_get_gv_nmemb"
                        :arguments (integer)) ; wrapped
      (get-gv-members   :entry-point "_get_gv_members"
                        :arguments (integer)) ; wrapped
      (groupview-joined :entry-point "_get_gv_joined"  
                        :arguments (integer))
      (groupview-departed :entry-point "_get_gv_departed"  
                          :arguments (integer))
      (siteview-viewid  :entry-point "_get_sv_viewid"
                        :arguments (integer))
      (siteview-slist   :entry-point "_get_sv_slist"
                        :arguments (integer))
          ;; Access using c-short-vector-ref
      (siteview-incarn  :entry-point "_get_sv_incarn"
                        :arguments (integer))
          ;; Access using c-byte-vector-ref
      (siteview-failed  :entry-point "_get_sv_failed"
                        :arguments (integer))
          ;; Need access functions for bitvecs.
      (siteview-recovered :entry-point "_get_sv_recovered"
                          :arguments (integer))
      (x-list-items-id  :entry-point "_get_x_list_items_id"
                        :arguments (integer fixnum))
      ;;  flib.o
      (my-address       :entry-point "_myaddress_" :arguments nil)
      (my-process-id    :entry-point "_mypid_"     :arguments nil)
      (my-site-id       :entry-point "_mysid_"     :arguments nil)
      (my-site-incarn   :entry-point "_mysincarn_" :arguments nil)
      (my-site-no       :entry-point "_mysno_"     :arguments nil)
      (nulladdress      :entry-point "_nulladdress_" :arguments nil)

      ;; tk_rexec.o 
      (isis-rexec-c     :entry-point "_isis_rexec"
                        :arguments (fixnum integer integer string
                                    (vector simple-string)
                                    (vector simple-string)
                                    string string integer)) ; wrapped
      ;; tk_authen.o
      ;; cl_isis.o 
      (isis-select-from-lisp :entry-point "_isis_select_from_lisp"
                             :arguments nil)  ; used internally
      (addr-cmp         :entry-point "_addr_cmp_fun"
                        :arguments (integer integer))
      (addr-isequal-c   :entry-point "_addr_isequal"
                        :arguments (integer integer)) ; wrapped
      (addr-ismine-c    :entry-point "_addr_ismine"
                        :arguments (integer)) ; wrapped
      (addr-isnull-c    :entry-point "_addr_isnull_fun"
                        :arguments (integer)) ; wrapped
      (alist-len        :entry-point "_alist_len"
                        :arguments (integer))
      (isis-accept-events :entry-point "_isis_accept_events"
                          :arguments (fixnum))
      (isis-decon-wait  :entry-point "_isis_decon_wait"
                        :arguments (fixnum))
      (isis-disconnect  :entry-point "_isis_disconnect")
      (isis-entry-c     :entry-point "_isis_entry"
                        :arguments (fixnum fixnum string)) ; wrapped
      (isis-init-c      :entry-point "_isis_init"
                        :arguments (fixnum)) ; wrapped
      (isis-input-c     :entry-point "_isis_input_fun"
                        :arguments (fixnum integer fixnum)) ; wrapped
      (isis-output-c    :entry-point "_isis_output_fun"
                        :arguments (fixnum integer fixnum)) ; wrapped
      (isis-signal-c    :entry-point "_isis_signal_fun"
                        :arguments (fixnum integer fixnum)) ; wrapped
      (isis-chwait-c    :entry-point "_isis_chwait_fun"
                        :arguments (integer fixnum)) ; wrapped
      (isis-input-sig-c :entry-point "_isis_input_sig_fun"
                        :arguments (fixnum integer integer)) ;wrapped
      (isis-output-sig-c :entry-point "_isis_output_sig_fun"
                        :arguments (fixnum integer integer)) ;wrapped
      (isis-signal-sig-c :entry-point "_isis_signal_sig_fun"
                        :arguments (fixnum integer integer)) ;wrapped
      (isis-chwait-sig-c :entry-point "_isis_chwait_sig_fun"
                        :arguments (integer integer)) ;wrapped
      (isis-input-drain :entry-point "_isis_input_drain" :arguments nil)
      (isis-logentry    :entry-point "_isis_logentry"
                        :arguments (integer fixnum))
      (isis-mainloop-c  :entry-point "_isis_mainloop"
                        :arguments (integer fixnum)) ; wrapped
      (isis-perror      :entry-point "_isis_perror"
                        :arguments (string))
      (isis-read        :entry-point "_isis_read"
                        :arguments nil)
      (isis-sleep       :entry-point "_isis_sleep"
                        :arguments (fixnum))
      (isis-sleep-ms    :entry-point "_isis_sleep_ms"
                        :arguments (fixnum))
      (isis-start-done  :entry-point "_isis_start_done"
                        :arguments nil)
      (isis-task        :entry-point "_isis_task"
                        :arguments (fixnum string)) ; wrapped
      (isis-timeout-c   :entry-point "_isis_timeout"
                        :arguments (fixnum integer fixnum fixnum)) ; wrapped
      (panic)           ; Should just use error instead.
      (pentry           :arguments (integer fixnum))
      (pg-getlocalview  :entry-point "_pg_getlocalview"
                        :arguments (integer))
      (pg-getview       :entry-point "_pg_getview"
                        :arguments (integer))
      (pg-local-lookup  :entry-point "_pg_local_lookup"
                        :arguments (string))
      (pg-rank          :entry-point "__pg_rank"
                        :arguments (integer integer))
      (pg-rank-all      :entry-point "__pg_rank_all"  
                        :arguments (integer integer))
      (cl-dump          :entry-point "_cl_dump"
                        :arguments (fixnum))
      (pr-dump          :entry-point "_pr_dump"
                        :arguments (fixnum))
      (run-tasks        :entry-point "_run_tasks" :arguments nil)
      ;; cl_inter.o 
      ;; cl_bypass.o 
      ;; cl_pgroup.o 
      (pg-addclient     :entry-point "_pg_addclient"
                        :arguments (integer integer))
      (pg-delclient     :entry-point "_pg_delclient"
                        :arguments (integer integer))
      (pg-delete        :entry-point "_pg_delete"
                        :arguments (integer))
      (pg-leave         :entry-point "_pg_leave"
                        :arguments (integer))
      (pg-lookup        :entry-point "_pg_lookup"
                        :arguments (string))
      (pg-monitor-c     :entry-point "_pg_monitor"
                        :arguments (integer integer fixnum)) ; wrapped
      (pg-monitor-cancel :entry-point "_pg_monitor_cancel"
                         :arguments (integer))
      (pg-signal        :entry-point "_pg_signal"
                        :arguments (integer fixnum))
      (pg-unmonitor     :entry-point "_pg_unmonitor"
                        :arguments (integer))
      ;; cl_bcast.o 
      (simple-abcast    :entry-point "_abcast"
                        :arguments t) ; Not very useful.
      (abcast-l         :entry-point "_abcast_l"
                        :arguments t) ; wrapped
      (abortreply       :arguments (integer))
      (simple-bcast     :entry-point "_bcast" :arguments t) 
      (bcast-l          :entry-point "_bcast_l"
                        :arguments t) ; wrapped
      (simple-cbcast    :entry-point "_cbcast" :arguments t)
      (cbcast-l         :entry-point "_cbcast_l"
                        :arguments t) ; wrapped
      (simple-fbcast    :entry-point "_fbcast" :arguments t)
      (fbcast-l         :entry-point "_fbcast_l"
                        :arguments t) ; wrapped
      (flush            :arguments nil)
      (forward          :arguments (integer integer fixnum integer))
      (simple-gbcast    :entry-point "_gbcast" :arguments t)
      (gbcast-l         :entry-point "_gbcast_l"
                        :arguments t) ;wrapped
      (nullreply        :arguments integer)
      (simple-reply     :entry-point "_reply" :arguments t)
      (reply-l          :entry-point "_reply_l"
                        :arguments t) ; wrapped
      ;; cl_task.o 
      (invoke           :arguments (integer)
                        :return-type :void) ; used internally
      (isis-fork        :entry-point "_isis_fork"
                        :arguments (integer fixnum integer)) ; wrapped
      (isis-fork-urgent :entry-point "_isis_fork_urgent"
                        :arguments (integer fixnum integer)) ;wrapped
      (isis-init-lisp-funs :entry-point "_isis_init_lisp_funs"
                           :arguments (integer) ; used internally
                           :return-type :void)
      (t-final :entry-point "_t_final"
                        :arguments nil) ; used internally
      (t-init :entry-point "_t_init"
                        :arguments nil) ; used internally
      (t-sig-c          :entry-point "_t_sig"
                        :arguments (integer fixnum)) ; wrapped
      (t-sig-all-c      :entry-point "_t_sig"
                        :arguments (integer fixnum)) ; wrapped
      (t-sig-urgent-c   :entry-point "_t_sig_urgent"
                        :arguments (integer fixnum)) ; wrapped
      (t-wait-l :entry-point "_t_wait_l"
                        :arguments (integer string)) ; wrapped
      ;; cl_queues.o 
      ;; cl_dump.o 
      (paddr            :arguments (integer))
      (paddrs           :arguments (integer)) ; Null terminated address list!
      (pmsg             :arguments (integer))
      ;; cl_alloc.o 
      ;; cl_sview.o 
      (site-getview     :entry-point "_site_getview" :arguments nil)
      (sv-monitor-c     :entry-point "_sv_monitor"
                        :arguments (integer fixnum)) ; wrapped
      (sv-monitor-cancel :entry-point "_sv_monitor_cancel"
                        :arguments (fixnum))
      (sv-watch-c       :entry-point "_sv_watch"
                        :arguments (fixnum fixnum integer fixnum)) ; wrapped
      (sv-watch-cancel  :entry-point "_sv_watch_cancel"
                        :arguments (fixnum))
      ;; cl_news.o 
      (news-apost       :entry-point "_news_apost"
                        :arguments (short-vector string integer fixnum))
                              ; Null terminated site-id list.
      (news-cancel      :entry-point "_news_cancel"
                        :arguments (string))
      (news-clear       :entry-point "_news_clear"
                        :arguments (short-vector string))
                              ; Null terminated site-id list.
      (news-clear-all   :entry-point "_news_clear_all"
                        :arguments (short-vector string))
                              ; Null terminated site-id list.
      (news-post        :entry-point "_news_post"
                        :arguments (short-vector string integer fixnum))
                              ; Null terminated site-id list.
      (news-subscribe   :entry-point "_news_subscribe"
                        :arguments (string fixnum fixnum))
      ;; cl_groups.o 
      ;; cl_join.o 
      (allow-xfers-xd-c :entry-point "_allow_xfers_xd"
                        :arguments (string fixnum integer fixnum)) ; wrapped
      (pg-client        :entry-point "_pg_client"
                        :arguments (integer string))
      (pg-join-c        :entry-point "_pg_join"
                        :arguments t) ; wrapped
      (pg-join-inhibit-c  :entry-point "_pg_join_inhibit"
                          :arguments (fixnum)) ; wrapped
      (pg-subgroup      :entry-point "_pg_subgroup"                ;wrapped
                        :arguments (integer string fixnum integer integer)) 
      (xfer-flush       :entry-point "_xfer_flush" :arguments nil)
      (xfer-out         :entry-point "_do_xfer_out"
                        :arguments (fixnum integer))
      ;; cl_coord.o 
      (cc-terminate     :entry-point "_cc_terminate_msg"
                        :arguments (fixnum))
      (coord-cohort-c   :entry-point "_coord_cohort" ;wrapped
                        :arguments (integer integer integer integer fixnum)) 
      (coord-cohort-l   :entry-point "_coord_cohort_l"
                        :arguments (integer integer integer integer fixnum
                                    integer))  ;wrapped
      ;; cl_watch.o 
      (pg-watch-c       :entry-point "_pg_watch"                     ; wrapped
                        :arguments (integer integer fixnum integer fixnum)) 
      (pg-watch-cancel  :entry-point "_pg_watch_cancel"
                        :arguments (fixnum))
      (proc-monitor-dump :entry-point "_proc_monitor_dump" :arguments nil)
      (proc-watch-c     :entry-point "_proc_watch"
                        :arguments (integer integer fixnum)) ; wrapped
      (proc-watch-cancel :entry-point "_proc_watch_cancel"
                        :arguments (fixnum))
      ;; cl_print.o 
      (isis-print       :entry-point "_isis_print")
      ;; g_parse.o -- forget guards for now. 
      ;; g_eval.o 
      ;; cl_lmgr.o 
      (log-checkpoint   :entry-point "_log_checkpoint"
                        :arguments (string))
      (log-flush        :entry-point "_log_flush"
                        :arguments (integer))
      (log-recovered-c  :entry-point "_log_recovered"
                        :arguments (integer)) ; wrapped
      (log-write        :entry-point "_log_write"
                        :arguments (integer integer))
      ;; cl_setjmp.o 
      ;; tk_rmgr.o 
      (rmgr-register    :entry-point "_rmgr_register"
                        :arguments (string))
      (rmgr-unregister  :entry-point "_rmgr_unregister"
                        :arguments nil)
      (rmgr-update      :entry-point "_rmgr_update"
                        :arguments (string string (vector simple-string)
                                    (vector simple-string)))
      ;; cl_xaction.o 
      (x-abort          :entry-point "_x_abort" :arguments nil)
      (x-begin          :entry-point "_x_begin" :arguments nil)
      (x-commit         :entry-point "_x_commit"
                        :arguments (fixnum))
      (x-getid          :entry-point "_x_getid" :arguments nil)
      (x-outcomes       :entry-point "_x_outcomes"
                        :arguments (string))
      (x-outcomes-flush :entry-point "_x_outcomes_flush"
                        :arguments (string integer))
      (x-term-msg       :entry-point "_x_term_msg"                 ;wrapped
                        :arguments (string integer integer integer integer)) 
      ;; cl_token.o 
      (t-holder         :entry-point "_t_holder"
                        :arguments (integer string))
      (t-pass           :entry-point "_t_pass"
                        :arguments (integer string))
      (t-request-c      :entry-point "_t_request"
                        :arguments (integer string fixnum)) ;wrapped
      ;; history.o 
      ;; cl_failed.o 
      ;; cl_sundummy.o 
      ;; msg_new.o 
      (msg-copy         :entry-point "_msg_copy"
                        :arguments (integer))
      (msg-delete       :entry-point "_MSG_DELETE"
                        :arguments (integer))
      (msg-genmsg       :entry-point "_msg_genmsg")      ; Expert use only!
      (msg-newmsg       :entry-point "_msg_newmsg")
      ;; msg_types.o  -- can't define new message field types froms lisp yet
      ;; msg_utils.o 
      (msg-getdests     :entry-point "_msg_getdests_fun"

                        :arguments (integer)) ; wrapped
      (msg-getlen       :entry-point "_msg_getlen_fun"
                        :arguments (integer))
      (msg-getreplyto   :entry-point "_msg_getreplyto_fun"
                        :arguments (integer))
      (msg-getsender    :entry-point "_msg_getsender_fun"
                        :arguments (integer))
      (msg-gettruesender :entry-point "_msg_gettruesender_fun"
                         :arguments (integer))
      (msg-gettype      :entry-point "_msg_gettype"
                        :arguments (integer fixnum fixnum))
                        ;; 2nd arg is actually an unsigned-byte!!
      (msg-getscantype  :entry-point "_msg_getscantype"
                        :arguments (integer))
      (msg-increfcount  :entry-point "_msg_increfcount_fun"
                        :arguments (integer))
      (msg-isforwarded-c :entry-point "_msg_isforwarded_fun"
                         :arguments (integer)) ;wrapped
      (msg-msgcheck-c   :entry-point "_msg_msgcheck"
                        :arguments (integer)) ; wrapped
      ;; msg_fmt.o 
      (msg-gen          :entry-point "_msg_gen")    ; Expert use only!
      (msg-putfld       :entry-point "_msg_putfld") ; Expert use only!
      (msg-getfld       :entry-point "_msg_getfld") ; Expert use only!
      (msg-get          :entry-point "_msg_get" :arguments t) ;wrapped
      (msg-put          :entry-point "_msg_put" :arguments t) ;wrapped
      (msg-rewind       :entry-point "_msg_rewind"
                        :arguments (integer))
      ;; msg_fio.o 
      (msg-fread        :entry-point "_msg_fread"
                        :arguments (integer))
      (msg-fwrite       :entry-point "_msg_fwrite"
                        :arguments (integer integer))
      (msg-read         :entry-point "_msg_read"
                        :arguments (fixnum))
      (msg-write        :entry-point "_msg_write"
                        :arguments (fixnum integer))
      ))

;;; Simple wrappers for most functions. 
;;; (More complex wrappers are in other files.)

(defun my-host ()
  (c-string-to-lisp-string (get-my-host)))
(defun site-name (i)
  (c-string-to-lisp-string (get-site-name i)))

(defun wrap-pred (bool)
  (if (zerop bool)
      nil
      t))

(defun addr-isequal (a1 a2) (wrap-pred (addr-isequal-c a1 a2)))
(defun addr-isnull (a1) (wrap-pred (addr-isnull-c a1)))
(defun addr-ismine (a1) (wrap-pred (addr-ismine-c a1)))
(defun log-recovered (gaddr) (wrap-pred (log-recovered-c gaddr)))
(defun msg-isforwarded (msg) (wrap-pred (msg-isforwarded-c msg)))
(defun msg-msgcheck (msg) (wrap-pred (msg-msgcheck-c msg)))

(defun pg-join-inhibit (flag)
  (pg-join-inhibit-c (if flag 1 0)))
(defun t-request (gaddr name pass-on-fail)
  (t-request-c gaddr name (if pass-on-fail 1 0)))
