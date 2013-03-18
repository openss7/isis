#  $RCSfile: makefile,v $ $Revision: 2.25 $ $Date: 90/09/18 08:38:10 $  
SHELL=/bin/sh
S = ../../protos
DFLAGS = 
CFLAGS =  -I../../include $(OPTIM) -D$(MCHTYPE) $(MCHDEPCFLAGS)
CFLAGSNOOPT =  -I../../include -D$(MCHTYPE) $(MCHDEPCFLAGS)
CPPFLAGS = -I../../include -D$(MCHTYPE)
LIBFILES = ../mlib/libisism.a $(OPLIBS)
LFLAGS = $(MCHDEPCFLAGS)

OFILES = pr_abcast.o pr_cbcast.o pr_gbcast.o pr_init.o pr_dump.o \
	pr_addr.o pr_astore.o pr_gmgr.o \
	pr_mcast.o pr_queues.o pr_task.o pr_local.o \
	pr_main.o pr_inter.o pr_client.o pr_dlist.o pr_clentry.o \
	pr_fdect.o pr_watch.o pr_viewlocks.o pr_glocks.o pr_setjmp.o pr_msgtank.o \
        history.o

CFILES = $(S)/pr_abcast.c $(S)/pr_cbcast.c $(S)/pr_gbcast.c $(S)/pr_init.c $(S)/pr_dump.c \
	$(S)/pr_addr.c $(S)/pr_astore.c $(S)/pr_gmgr.c \
	$(S)/pr_mcast.c $(S)/pr_queues.c $(S)/pr_task.c $(S)/pr_local.c \
	$(S)/pr_main.c $(S)/pr_inter.c $(S)/pr_client.c $(S)/pr_dlist.c $(S)/pr_clentry.c \
	$(S)/pr_fdect.c $(S)/pr_watch.c $(S)/pr_viewlocks.c $(S)/pr_glocks.c \
        $(S)/pr_msgtank.c $(S)/history.c

protos: $(OFILES) ../mlib/libisism.a
	/bin/rm -f protos
	$(CC) $(LFLAGS)  -o protos $(OFILES) $(LIBFILES)

protos.gprof: $(OFILES) ../mlib/libisism.a
	$(CC) $(LFLAGS) -pg -o protos.gprof $(OFILES) $(LIBFILES)

lint:
	lint -I../../include -D$(MCHTYPE) $(CFILES) ../../mlib/mlib-l.c

history.o:	$(S)/history.c; $(CC) -c $(CFLAGS) $(S)/history.c
pr_abcast.o:	$(S)/pr_abcast.c; $(CC) -c $(CFLAGS) $(S)/pr_abcast.c
pr_cbcast.o:	$(S)/pr_cbcast.c; $(CC) -c $(CFLAGS) $(S)/pr_cbcast.c
pr_gbcast.o:	$(S)/pr_gbcast.c; $(CC) -c $(CFLAGS) $(S)/pr_gbcast.c
pr_init.o:	$(S)/pr_init.c; $(CC) -c $(CFLAGS) $(S)/pr_init.c
pr_dump.o:	$(S)/pr_dump.c; $(CC) -c $(CFLAGS) $(S)/pr_dump.c
pr_addr.o:	$(S)/pr_addr.c; $(CC) -c $(CFLAGS) $(S)/pr_addr.c
pr_astore.o:	$(S)/pr_astore.c; $(CC) -c $(CFLAGS) $(S)/pr_astore.c
pr_gmgr.o:	$(S)/pr_gmgr.c; $(CC) -c $(CFLAGS) $(S)/pr_gmgr.c
pr_mcast.o:	$(S)/pr_mcast.c; $(CC) -c $(CFLAGS) $(S)/pr_mcast.c
pr_queues.o:	$(S)/pr_queues.c; $(CC) -c $(CFLAGS) $(S)/pr_queues.c
pr_task.o:	$(S)/pr_task.c
		-if test $(MCHTYPE) != SUN3 -a $(MCHTYPE) != SUN4 -a $(MCHTYPE) != HPUX; then $(CC) -c $(CFLAGS) $(S)/pr_task.c; fi
		-if test $(MCHTYPE) = HPUX; then $(CC) -c $(CFLAGSNOOPT) $(S)/pr_task.c; fi
		-if test $(MCHTYPE) = SUN3 -o $(MCHTYPE) = SUN4; then cc $(CFLAGS) -c $(S)/pr_task.c; fi
pr_local.o:	$(S)/pr_local.c; $(CC) -c $(CFLAGS) $(S)/pr_local.c
pr_main.o:	$(S)/pr_main.c; $(CC) -c $(CFLAGS) $(S)/pr_main.c
pr_inter.o:	$(S)/pr_inter.c; $(CC) -c $(CFLAGS) $(S)/pr_inter.c
pr_client.o:	$(S)/pr_client.c; $(CC) -c $(CFLAGS) $(S)/pr_client.c
pr_dlist.o:	$(S)/pr_dlist.c; $(CC) -c $(CFLAGS) $(S)/pr_dlist.c
pr_clentry.o:	$(S)/pr_clentry.c; $(CC) -c $(CFLAGS) $(S)/pr_clentry.c
pr_fdect.o:	$(S)/pr_fdect.c; $(CC) -c $(CFLAGS) $(S)/pr_fdect.c
pr_watch.o:	$(S)/pr_watch.c; $(CC) -c $(CFLAGS) $(S)/pr_watch.c
pr_viewlocks.o:	$(S)/pr_viewlocks.c; $(CC) -c $(CFLAGS) $(S)/pr_viewlocks.c
pr_glocks.o:	$(S)/pr_glocks.c; $(CC) -c $(CFLAGS) $(S)/pr_glocks.c
pr_setjmp.o:    pr_setjmp.s $(S)/pr_setjmp.c
		-if test $(MCHTYPE) != GOULD -a $(MCHTYPE) != RT43 -a $(MCHTYPE) != MIPS -a $(MCHTYPE) != AIX -a $(MCHTYPE) != MACH -a $(MCHTYPE) != APOLLO -a $(MCHTYPE) != SGI -a $(MCHTYPE) != CONVEX; then cc -c $(CFLAGS) pr_setjmp.s; fi
		-if test $(MCHTYPE) = GOULD -o $(MCHTYPE) = RT43 -o $(MCHTYPE) = MIPS -o $(MCHTYPE) = AIX -o $(MCHTYPE) = MACH -o $(MCHTYPE) = APOLLO -o $(MCHTYPE) = SGI -o $(MCHTYPE) = CONVEX; then $(CC) -c $(CFLAGS) $(S)/pr_setjmp.c; fi
pr_setjmp.s:    $(S)/pr_setjmp.s; /lib/cpp $(CPPFLAGS) $(S)/pr_setjmp.s > x; mv x pr_setjmp.s
pr_msgtank.o: 	$(S)/pr_msgtank.c; $(CC) -c $(CFLAGS) $(S)/pr_msgtank.c
