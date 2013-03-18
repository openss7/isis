#  $RCSfile: makefile,v $ $Revision: 2.104 $ $Date: 90/09/12 13:25:58 $  
SHELL = /bin/sh
CFLAGS = -I../../include -D$(MCHTYPE) $(MCHDEPCFLAGS) $(OPTIM)
CFLAGSNOOPT = -I../../include -D$(MCHTYPE) $(MCHDEPCFLAGS)
CPPFLAGS = -I../../include -D$(MCHTYPE) $(MCHDEPCFLAGS) 
DBGCFLAGS = -I../../include -D$(MCHTYPE) $(MCHDEPCFLAGS) -g
S = ../../clib

CLOFILES = cl_isis.o cl_inter.o cl_bypass.o cl_pgroup.o cl_bcast.o cl_task.o \
	cl_queues.o cl_dump.o cl_sview.o cl_groups.o cl_join.o \
	cl_coord.o cl_watch.o cl_print.o cl_spool1.o \
	cl_setjmp.o cl_token.o cl_sunlwp_fix.o history.o

TKOFILES = lisp_lib.o flib.o flib1.o flib2.o tk_rexec.o tk_authen.o tk_connect.o \
	tk_remote.o tk_lmgr.o tk_rmgr.o tk_news.o tk_spool2.o tk_long_haul.o tk_xaction.o \
	cl_plist.o

DUOFILES = cl_failed.o ap_dummy.o tk_cdummy.o rmgr_dummy.o lmgr_dummy.o \
	x_dummy.o spool_dummy.o cl_sundummy.o

all:		libisis1.a libisis2.a 
allegro_clib:	libisis1.a libisis2.a isis_c_refs.o libisism.a
lucid_clib:	libisis1.a libisis2.a isis_c_refs.o libisism.a

libisis1.a: $(TKOFILES)
	rm -f libisis1.a
	ar lruv libisis1.a $(TKOFILES)
	-if test $(MCHTYPE) != HPUX -a $(MCHTYPE) != AIX -a $(MCHTYPE) != AUX ; then ranlib libisis1.a; fi

libisis2.a: $(CLOFILES) $(DUOFILES)
	rm -f libisis2.a
	ar lruv libisis2.a $(CLOFILES) $(DUOFILES)
	-if test $(MCHTYPE) != HPUX -a $(MCHTYPE) != AIX -a $(MCHTYPE) != AUX ; then ranlib libisis2.a; fi

isis_c_refs.o:	$(S)/isis_c_refs.c; $(CC) -c $(CFLAGS) $(S)/isis_c_refs.c
libisism.a:	../mlib/libisism.a
	/bin/cp ../mlib/libisism.a libisism.a; 
	ar ld libisism.a msg_sa.o
	-if test $(MCHTYPE) != HPUX -a $(MCHTYPE) != AIX -a $(MCHTYPE) != AUX ; then ranlib libisism.a; fi

history.o:	$(S)/history.c; $(CC) -c $(CFLAGS) $(S)/history.c
lisp_lib.o:	$(S)/lisp_lib.c; $(CC) -c $(CFLAGS) $(S)/lisp_lib.c
flib.o:        $(S)/flib.c; $(CC) -c $(CFLAGS) $(S)/flib.c
flib1.o:        $(S)/flib1.c; $(CC) -c $(CFLAGS) $(S)/flib1.c
flib2.o:        $(S)/flib2.c; $(CC) -c $(CFLAGS) $(S)/flib2.c
cl_isis.o:	$(S)/cl_isis.c
		-if test $(MCHTYPE) != HPUX; \
			then $(CC) -c $(CFLAGS) $(S)/cl_isis.c; fi 
		-if test $(MCHTYPE) = HPUX; \
			then $(CC) -c $(CFLAGSNOOPT) $(S)/cl_isis.c; fi 
cl_inter.o:	$(S)/cl_inter.c; $(CC) -c $(CFLAGS) $(S)/cl_inter.c
cl_bypass.o:	$(S)/cl_bypass.c; $(CC) -c $(CFLAGS) $(S)/cl_bypass.c
cl_task.o:      $(S)/cl_task.c
		-if test $(MCHTYPE) != HPUX -a $(MCHTYPE) != SUN3 -a $(MCHTYPE) != SUN4; then $(CC) -c $(CFLAGS) $(S)/cl_task.c; fi
		-if test $(MCHTYPE) = HPUX; then $(CC) -c $(CFLAGSNOOPT) $(S)/cl_task.c; fi
		-if test $(MCHTYPE) = SUN3 -o $(MCHTYPE) = SUN4; then cc -c $(CFLAGS) $(S)/cl_task.c; fi
cl_pgroup.o:	$(S)/cl_pgroup.c; $(CC) -c $(CFLAGS) $(S)/cl_pgroup.c
cl_bcast.o:	$(S)/cl_bcast.c; $(CC) -c $(CFLAGS) $(S)/cl_bcast.c
cl_queues.o:	$(S)/cl_queues.c; $(CC) -c $(CFLAGS) $(S)/cl_queues.c
cl_dump.o:	$(S)/cl_dump.c; $(CC) -c $(CFLAGS) $(S)/cl_dump.c
cl_sview.o:	$(S)/cl_sview.c; $(CC) -c $(CFLAGS) $(S)/cl_sview.c
cl_groups.o:	$(S)/cl_groups.c; $(CC) -c $(CFLAGS) $(S)/cl_groups.c
cl_join.o:	$(S)/cl_join.c; $(CC) -c $(CFLAGS) $(S)/cl_join.c
cl_coord.o:	$(S)/cl_coord.c; $(CC) -c $(CFLAGS) $(S)/cl_coord.c
cl_watch.o:	$(S)/cl_watch.c; $(CC) -c $(CFLAGS) $(S)/cl_watch.c
cl_print.o:	$(S)/cl_print.c; $(CC) -c $(CFLAGS) $(S)/cl_print.c
cl_failed.o:	$(S)/cl_failed.c; $(CC) -c $(CFLAGS) $(S)/cl_failed.c
ap_dummy.o:	$(S)/ap_dummy.c; $(CC) -c $(CFLAGS) $(S)/ap_dummy.c
cl_sundummy.o:	$(S)/cl_sundummy.c; $(CC) -c $(CFLAGS) $(S)/cl_sundummy.c
cl_setjmp.o:	cl_setjmp.s $(S)/cl_setjmp.c
		-if test $(MCHTYPE) != GOULD -a $(MCHTYPE) != RT43 -a $(MCHTYPE) != MIPS  -a $(MCHTYPE) != AIX -a $(MCHTYPE) != MACH -a $(MCHTYPE) != APOLLO -a $(MCHTYPE) != SGI -a $(MCHTYPE) != CONVEX ; then cc -c $(CPPFLAGS) cl_setjmp.s; fi
		-if test $(MCHTYPE) = GOULD -o $(MCHTYPE) = RT43 -o $(MCHTYPE) = MIPS  -o $(MCHTYPE) = AIX -o $(MCHTYPE) = MACH -o $(MCHTYPE) = APOLLO -o $(MCHTYPE) = SGI -o $(MCHTYPE) = CONVEX; then $(CC) -c $(CFLAGS) $(S)/cl_setjmp.c; fi
cl_setjmp.s:	$(S)/cl_setjmp.s; /lib/cpp $(CPPFLAGS) $(S)/cl_setjmp.s > x; mv x cl_setjmp.s
tk_xaction.o:	$(S)/tk_xaction.c; $(CC) -c $(CFLAGS) $(S)/tk_xaction.c
cl_token.o:	$(S)/cl_token.c; $(CC) -c $(CFLAGS) $(S)/cl_token.c
cl_sunlwp_fix.o:	$(S)/cl_sunlwp_fix.c; cc -c $(CPPFLAGS) $(S)/cl_sunlwp_fix.c
cl_spool1.o:	$(S)/cl_spool1.c; $(CC) -c $(CFLAGS) $(S)/cl_spool1.c
tk_rmgr.o:	$(S)/tk_rmgr.c; $(CC) -c $(CFLAGS) $(S)/tk_rmgr.c
tk_rexec.o:	$(S)/tk_rexec.c; $(CC) -c $(CFLAGS) $(S)/tk_rexec.c
tk_authen.o:	$(S)/tk_authen.c; $(CC) -c $(CFLAGS) $(S)/tk_authen.c
tk_connect.o:	$(S)/tk_connect.c; $(CC) -c $(CFLAGS) $(S)/tk_connect.c
tk_remote.o:	$(S)/tk_remote.c; $(CC) -c $(CFLAGS) $(S)/tk_remote.c
cl_plist.o:	$(S)/cl_plist.c; $(CC) -c $(CFLAGS) $(S)/cl_plist.c
tk_lmgr.o:	$(S)/tk_lmgr.c; $(CC) -c $(CFLAGS) $(S)/tk_lmgr.c
lmgr_dummy.o:	$(S)/lmgr_dummy.c; $(CC) -c $(CFLAGS) $(S)/lmgr_dummy.c
rmgr_dummy.o:	$(S)/rmgr_dummy.c; $(CC) -c $(CFLAGS) $(S)/rmgr_dummy.c
tk_spool2.o:	$(S)/tk_spool2.c; $(CC) -c $(CFLAGS) $(S)/tk_spool2.c
tk_long_haul.o:	$(S)/tk_long_haul.c; $(CC) -c $(CFLAGS) $(S)/tk_long_haul.c
tk_news.o:	$(S)/tk_news.c; $(CC) -c $(CFLAGS) $(S)/tk_news.c
tk_cdummy.o:	$(S)/tk_cdummy.c; $(CC) -c $(CFLAGS) $(S)/tk_cdummy.c
x_dummy.o:	$(S)/x_dummy.c; $(CC) -c $(CFLAGS) $(S)/x_dummy.c
spool_dummy.o:	$(S)/spool_dummy.c; $(CC) -c $(CFLAGS) $(S)/spool_dummy.c
