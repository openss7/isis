/*  $RCSfile: isis_c_refs.c,v $ $Revision: 2.92 $ $Date: 90/08/27 13:28:58 $  */
/* This function exists to reference all ISIS C routines
 *  used from lisp. Needed by the Allegro CL foreign functions inteface.
 *  (Actually far too many C routines are in this list. Must trim the
 *  ones that nobody has any business calling.)
 *  Coded by Robert Cooper
 *
 *      ISIS release V2.0, May 1990
 *      Export restrictions apply
 *
 *      The contents of this file are subject to a joint, non-exclusive
 *      copyright by members of the ISIS Project.  Permission is granted for
 *      use of this material in unmodified form in commercial or research
 *      settings.  Creation of derivative forms of this software may be
 *      subject to restriction; obtain written permission from the ISIS Project
 *      in the event of questions or for special situations.
 *      -- Copyright (c) 1990, The ISIS PROJECT
 */
isis_c_refs()
{
    /*  lisp_lib.o */
    get_gv_clients();
    get_gv_departed();
    get_gv_flag();
    get_gv_gaddr();
    get_gv_incarn();
    get_gv_joined();
    get_gv_members();
    get_gv_name();
    get_gv_nclient();
    get_gv_nmemb();
    get_gv_viewid();
    get_my_host();
    get_site_name();
    get_sv_failed();
    get_sv_incarn();
    get_sv_recovered();
    get_sv_slist();
    get_sv_viewid();
    get_x_list_items_id();
    make_address();
    save_address();

    /*  flib.o */
    myaddress_();
    mypid_();
    mysid_();
    mysincarn_();
    mysno_();
    nulladdress_();

    /*  tk_rexec.o */
    isis_rexec();
    /*  tk_authen.o */
    au_filter();
    au_permit();
    au_request_verify();
    au_revoke_perm();
    /*  cl_isis.o */
    isis_select_from_lisp();
    act_begin();
    act_block();
    act_end();
    act_ev();
    act_restart();
    act_scan();
    act_start();
    addr_isequal();
    addr_ismine();
    alist_len();
    cc_gotres();
    check_cl_watch_queue();
    cl_del_pgroup();
    cl_do_del_pgroup();
    cl_getrname();
    cl_local_delivery();
    cl_new_view();
    cl_rcv_reply();
    cl_register();
    cl_rname();
    cl_setscope();
    cl_watch_cancel();
    cl_watch_for();
    clw_free();
    collect_reply();
    do_cl_dump();
    dump_act();
    dump_cl_watch_queue();
    find_act();
    fork_sighandlers();
    gop_lookup();
    isis();
    isis_accept_events();
    isis_decon_wait();
    isis_disconnect();
    isis_entry();
    isis_gotmsg();
    isis_has_crashed();
    isis_init();
    isis_input_drain();
    isis_invoke();
    isis_logentry();
    isis_mainloop();
    isis_overflow();
    isis_perror();
    isis_pg_copy();
    isis_read();
    isis_send();
    isis_setfilter();
    isis_sleep();
    isis_sleep_ms();
    isis_start_done();
    isis_task();
    isis_timeout();
    isis_timeout_reschedule();
    lisp_exit();
    panic();
    pentry();
    pg_getlocalview();
    pg_getview();
    pg_local_lookup();
    _pg_rank();
    _pg_rank_all();
    pgroups_dump();
    pr_dump();
    pr_makedump();
    pr_rescan();
    pr_shutdown();
    protos_despool();
    run_isis();
    run_tasks();
    set_entry();
    /*  cl_inter.o */
    dump_interclient();
    intercl_deliver();
    intercl_do_input();
    intercl_drain_any_input();
    intercl_gotmsg();
    intercl_init();
    intercl_newview();
    intercl_quiet();
    net_send();
    intercl_sweep();
    intercl_unblock();
    intercl_wantflush();
    io_free();
    lazy_flush();
    tank_status();
    /*  cl_bypass.o */
    by_dump();
    bypass_checkview();
    bypass_flush();
    bypass_inactive();
    bypass_init();
    bypass_precheck();
    bypass_recv();
    bypass_send();
    bypass_unblock();
    pbuf_dirty();
    /*  cl_pgroup.o */
    pg_addclient();
    pg_delclient();
    pg_delete();
    pg_leave();
    pg_list();
    pg_lookup();
    pg_monitor();
    pg_monitor_act();
    pg_monitor_cancel();
    pg_monitor_dump();
    pg_new_view();
    pg_pwatch_invoke();
    pg_signal();
    pg_unmonitor();
    pw_free();
    /*  cl_bcast.o */
    BCAST();
    abcast();
    abcast_l();
    abortreply();
    bc_cancel();
    bc_getevent();
    bc_poll();
    bc_wait();
    bcast();
    bcast_l();
    cbcast();
    cbcast_l();
    do_bcast();
    do_reply();
    do_reply_l();
    eid_sender();
    fbcast();
    fbcast_l();
    flush();
    forward();
    gbcast();
    gbcast_grow();
    gbcast_l();
    nullreply();
    reply();
    reply_l();
    /*  cl_task.o */
/*     call_lisp(); */
    do_task_dequeue();
    invoke();
    isis_entry_stacksize();
    isis_fork();
    isis_fork_urgent();
#ifdef ALLEGRO_CL
    isis_init_lisp_funs();
#endif ALLEGRO_CL
    spanic();
    t_final();
    t_init();
    t_on_sys_stack();
    t_scheck();
    t_set_stacksize();
    t_sig();
    t_sig_all();
    t_sig_urgent();
    t_yield();
    t_wait();
    t_wait_l();
    task_swtch();
    thread_isis_cleanup();
    thread_isis_enter();
    thread_isis_exit();
    /*  cl_queues.o */
    pg_add();
    pg_alloc();
    pg_find();
    qu_add();
    qu_add_cb();
    qu_add_pg();
    qu_add_sid();
    qu_alloc();
    qu_alloc_pg();
    qu_find();
    qu_freeall();
    qu_resort();
    /*  cl_dump.o */
    atoaddr();
    callername();
    cl_dump();
    dump_cond();
    dump_sid();
    dump_sview();
    dump_task();
    isis_logging();
    paddr();
    paddrs();
    peid();
    pmsg();
    psid();
    /*  cl_alloc.o */
    malloc_dump();
    mallocate();
    mdeallocate();
    /*  cl_sview.o */
    site_getview();
    site_monitor_dump();
    sv_doecall();
    sv_dovcall();
    sv_free();
    sv_init();
    sv_monitor();
    sv_monitor_cancel();
    sv_new_sview();
    sv_watch();
    sv_watch_cancel();
    /*  cl_news.o */
    news_apost();
    news_cancel();
    news_clear();
    news_clear_all();
    news_post();
    news_subscribe();
    /*  cl_groups.o */
    add_gname();
    add_group();
    gi_free();
    group_unmap();
    isis_gv_alloc();
    isis_gv_free();
    map_gaddr();
    map_gname();
    /*  cl_join.o */
    allow_xfers_xd();
    do_xfer_out();
    join_block();
    join_init();
    pg_client();
    pg_client_verifier();
    pg_dojoin();
    pg_join();
    pg_join_inhibit();
    pg_join_verifier();
    pg_subgroup();
    xfer_flush();
    xfer_out();
    xfer_rcv_small();
    xfer_rcv_unpack();
    xfer_to_checkpoint();
    /*  cl_coord.o */
    cc_panic();
    cc_result();
    cc_terminate();
    cc_watching();
    cohort();
    coord_cohort();
    coord_cohort_l();
    coord_init();
    coordinator();
    do_ccterminate();
    /*  cl_watch.o */
    cl_pmonitor();
    do_cl_proc_failed();
    pg_watch();
    pg_watch_cancel();
    proc_monitor_dump();
    proc_watch();
    proc_watch_cancel();
    w_free();
    w_init();
    w_new_view();
    /*  cl_print.o */
    isis_print();
    /*  cl_lmgr.o */
    log_action();
    log_checkpoint();
    log_end_log_msg();
    log_flush();
    log_has_ckpt();
    log_init();
    log_recovered();
    log_remove();
    log_replay_ckpt();
    log_replay_msgs();
    log_start_recovery();
    log_write();
    log_write_msg();
    logging_out();
    /*  cl_setjmp.o */
    isis_longjmp();
    isis_setjmp();
    /*  tk_rmgr.o */
    rmgr_create();
    rmgr_getinfo();
    rmgr_join();
    rmgr_lasttofail();
    rmgr_mh_rmnews();
    rmgr_mh_rmup();
    rmgr_register();
    rmgr_restart();
    rmgr_start_log();
    rmgr_stop_log();
    rmgr_unregister();
    rmgr_update();
    /*  cl_xaction.o */
    dump_trans();
    x_abort();
    x_begin();
    x_commit();
    x_getid();
    x_init();
    x_outcomes();
    x_outcomes_flush();
    x_term();
    xid_to_groupname();
    /*  cl_token.o */
    dump_tokens();
    map_token();
    t_holder();
    t_pass();
    t_request();
    tk_init();
    /*  history.o */
    /*  cl_failed.o */
    isis_failed();
    /*  cl_sundummy.o */
    /*  msg_fields.o */
    msg_deletefield();
    msg_insertfield();
    msg_replacefield();
    /*  msg_memory.o */
    /*  msg_msgs.o */
    msg_getmsg();
    msg_insertmsg();
    /*  msg_new.o */
    msg_copy();
    msg_genmsg();
    msg_newmsg();
    msg_reconstruct();
    /*  msg_types.o */
    msg_convertaddress();
    msg_convertbitv();
    msg_convertevent();
    msg_convertfield();
    msg_convertgldesc();
    msg_convertgroupview();
    msg_convertinterclient();
    msg_convertintersite();
    msg_convertlong();
    msg_convertpgroup();
    msg_convertshort();
    msg_convertverify();
    msg_definetype();
    msg_makekey();
    msg_typename();
    /*  msg_utils.o */
    msg_callertrace();
    msg_dumpmsgs();
    msg_gettype();
    msg_msgcheck();
    msg_printaccess();
    msg_printheader();
    msg_setdests();
    /*  msg_fmt.o */
    msg_dogetf();
    msg_doputf();
    msg_gen();
    msg_get();
    msg_getfld();
    msg_put();
    msg_putfld();
    msg_rewind();
    /*  msg_fio.o */
    msg_fread();
    msg_fwrite();
    msg_read();
    msg_write();
}
