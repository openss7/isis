#  $RCSfile: makefile,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:56 $  
SHELL=/bin/sh
S = ../../util
DBGCFLAGS = $(MCHDEPCFLAGS) -D$(MCHTYPE) -I../../include -c -g
CFLAGS = $(MCHDEPCFLAGS) -D$(MCHTYPE) -I../../include -c $(OPTIM)
LDFLAGS =  $(MCHDEPCFLAGS) -g
LIBS = ../clib/libisis1.a ../clib/libisis2.a ../mlib/libisism.a

all:	isis rexec spooler news rmgr \
	rmgr_cmd lmgr cmd tabex prstat \
	xmgr

some:  isis rexec

isis:	isis.o $(LIBS); $(CC) -o isis $(LDFLAGS) isis.o $(LIBS) $(OPLIBS)
tabex:	tabex.o; $(CC) -o tabex tabex.o
xmgr:	xmgr.o $(LIBS); $(CC) -o xmgr $(LDFLAGS) xmgr.o $(LIBS) $(OPLIBS)
rexec:	rexec.o $(LIBS); $(CC) -o rexec $(LDFLAGS) rexec.o $(LIBS) $(OPLIBS)
spooler:	spooler.o long_haul.o $(LIBS)
		$(CC) -o spooler $(LDFLAGS) spooler.o long_haul.o $(LIBS) $(OPLIBS)
news:	news.o $(LIBS); $(CC) -o news $(LDFLAGS) news.o $(LIBS) $(OPLIBS)
rmgr_cmd: rmgr_cmd.o $(LIBS)
		$(CC) -o rmgr_cmd $(LDFLAGS) rmgr_cmd.o $(LIBS) $(OPLIBS)
rmgr:	rmgr.o $(LIBS); $(CC) -o rmgr $(LDFLAGS) rmgr.o $(LIBS) $(OPLIBS)
lmgr:	lmgr.o $(LIBS); $(CC) -o lmgr $(LDFLAGS) lmgr.o $(LIBS) $(OPLIBS)
cmd:	cmd.o cmdy.o cmdl.o $(LIBS)
		$(CC) -o cmd $(LDFLAGS) cmd.o cmdy.o cmdl.o $(LIBS) $(OPLIBS)
prstat:  prstat.o $(LIBS); $(CC) -o prstat $(LDFLAGS) prstat.o $(LIBS) $(OPLIBS)

isis.o:		$(S)/isis.c; $(CC) $(CFLAGS) $(S)/isis.c
tabex.o:	$(S)/tabex.c; $(CC) $(CFLAGS) $(S)/tabex.c
rexec.o:	$(S)/rexec.c; $(CC) $(CFLAGS) $(S)/rexec.c
spooler.o:	$(S)/spooler.c; $(CC) $(CFLAGS) $(S)/spooler.c
long_haul.o:    $(S)/long_haul.c; $(CC) $(CFLAGS) $(S)/long_haul.c
xmgr.o:		$(S)/xmgr.c; $(CC) $(CFLAGS) $(S)/xmgr.c
news.o:		$(S)/news.c; $(CC) $(CFLAGS) $(S)/news.c
rmgr.o:		$(S)/rmgr.c; $(CC) $(CFLAGS) $(S)/rmgr.c
rmgr_cmd.o:	$(S)/rmgr_cmd.c; $(CC) $(CFLAGS) $(S)/rmgr_cmd.c
lmgr.o:		$(S)/lmgr.c; $(CC) $(CFLAGS) $(S)/lmgr.c

cmd.o:		$(S)/cmd.c; $(CC) $(CFLAGS) $(S)/cmd.c
cmdy.o:		cmdy.c; $(CC) $(CFLAGS) cmdy.c
cmdl.o:		cmdl.c; $(CC) $(CFLAGS) cmdl.c
cmdy.c:		$(S)/cmdy.y; yacc -d $(S)/cmdy.y; mv y.tab.h cmdy.h; mv y.tab.c cmdy.c
cmdl.c:		$(S)/cmdl.l; lex $(S)/cmdl.l; mv lex.yy.c cmdl.c
prstat.o:	$(S)/prstat.c; $(CC) $(CFLAGS) $(S)/prstat.c
