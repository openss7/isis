#  $RCSfile: makefile,v $ $Revision: 2.14 $ $Date: 90/08/06 13:47:32 $  
SHELL=/bin/sh
S = ../../mlib
CFLAGS = -I../../include  $(OPTIM) -D$(MCHTYPE) $(MCHDEPCFLAGS) $(ALLEGRO_CL)
DBCCFLAGS = -I../../include -D$(MCHTYPE) $(MCHDEPCFLAGS) $(ALLEGRO_CL)

OFILES = msg.o msg_types.o msg_fmt.o msg_fio.o msg_alloc.o msg_sa.o history.o

all:  libisism.a

libisism.a: $(OFILES)
	rm -f libisism.a
	-$(S)/fix_mlib_for_gcc $(CC)
	ar lr libisism.a $(OFILES)
	if test $(MCHTYPE) != HPUX -a $(MCHTYPE) != AIX -a $(MCHTYPE) != AUX; then ranlib libisism.a; fi; exit 0

history.o: $(S)/history.c
	$(CC) -c $(CFLAGS) $(S)/history.c
msg.o: $(S)/msg.c
	$(CC) -c $(CFLAGS) $(S)/msg.c
msg_types.o: $(S)/msg_types.c
	$(CC) -c $(CFLAGS) $(S)/msg_types.c
msg_fmt.o: $(S)/msg_fmt.c
	$(CC) -c $(CFLAGS) $(S)/msg_fmt.c
msg_fio.o: $(S)/msg_fio.c
	$(CC) -c $(CFLAGS) $(S)/msg_fio.c
msg_sa.o: $(S)/msg_sa.c
	$(CC) -c $(CFLAGS) $(S)/msg_sa.c
msg_alloc.o: $(S)/msg_alloc.c
	$(CC) -c $(CFLAGS) $(S)/msg_alloc.c
