#  $RCSfile: makefile,v $ $Revision: 1.5 $ $Date: 90/04/25 13:15:25 $ 
SHELL = /bin/sh
MCH = MCHTYPE=SUN3

MCHDEP = $(MCH) MCHDEPCFLAGS="-DNOSUNLWP" OPLIBS=
# Use the following line if you want to use Sun lightweight tasks.
#MCHDEP = $(MCH) MCHDEPCFLAGS="-DSUNLWP" OPLIBS=-llwp
# Use the following line if you want to enable bypass communication mode.
#MCHDEP = $(MCH) MCHDEPCFLAGS="-DNOSUNLWP -DBYPASS" OPLIBS=

# The following lines define useful sets of options for using either the 
# gcc compiler or the standard C compiler on your system.
OPTIMSTD = CC=cc OPTIM="-O3"
DEBUGSTD = CC=cc OPTIM="-g"
OPTIMGCC =  CC=gcc OPTIM="-g -O"
DEBUGGCC = CC=gcc OPTIM="-g"
WARNGCC =  CC=gcc OPTIM="-g -O -W"
WALLGCC =  CC=gcc OPTIM="-g -Wall -O"
NOWARNGCC =  CC=gcc OPTIM="-g -w -O"

ALLEGRO_DIR = /usr/u/isis/allegro/run/
ALLEGRO_DEP = $(MCH) MCHDEPCFLAGS="-DALLEGRO_CL -DNOALARMS -I$(ALLEGRO_DIR)lib/misc" OPTIM=-g

LUCID_DEP = $(MCH) MCHDEPCFLAGS="-DLUCID_CL" OPTIM=-g

all: readme mlib clib protos util demos

readme:
	@cat README

allegro: allegro_action
lucid:   lucid_action
protos:	pr_action
clib:	cl_action
mlib:	ml_action
util:	ut_action
demos: de_action

allegro_action:
	cd allegro_clib; make allegro_clib $(ALLEGRO_DEP) ; cd ..
lucid_action:
	cd lucid_clib; make lucid_clib $(LUCID_DEP) ; cd ..
pr_action:
	cd protos; make $(MCHDEP) $(OPTIMSTD) ; cd ..
cl_action: 
	cd clib; make $(MCHDEP) $(OPTIMSTD) ; cd ..
ml_action: 
	cd mlib; make $(MCHDEP) $(OPTIMSTD) ; cd ..
ut_action: 
	cd util; make $(MCHDEP) $(OPTIMSTD) ; cd ..
de_action: 
	cd demos; make $(MCHDEP) $(OPTIMSTD) ; cd ..

UTILPROGS = cmd isis lmgr news prstat rexec \
            rmgr rmgr_cmd spooler tabex xmgr 
DEMOPROGS = bank fortran_test grid lh_sp_test lisp_test \
            qa selftest silentjoins sun_grid teller testjoins \
            testtasks twenty wanClient wanServer spread

install:
	(cd bin; /bin/rm -f protos $(UTILPROGS) $(DEMOPROGS) )
	- (cd demos/pmk; make install )
	ln protos/protos bin
	(cd util; ln $(UTILPROGS) ../bin )
	- (cd demos; ln $(DEMOPROGS) ../bin )
	/bin/rm -f lib/libisis1.a lib/libisis2.a lib/libisism.a
	ln clib/libisis1.a clib/libisis2.a mlib/libisism.a lib

clean:
	/bin/rm -f protos/*.[os] clib/*.[os] mlib/*.o util/*.o demos/*.o
	/bin/rm -f util/cmd[ly].[ch]
	- (cd demos/pmk; make clean )
	- /bin/rm -f allegro_clib/*.[os] lucid_clib/*.[os]

realclean: clean
	- (cd demos/pmk; make realclean )
	/bin/rm -f protos/protos
	(cd util; /bin/rm -f $(UTILPROGS) )
	(cd demos; /bin/rm -f $(DEMOPROGS) )
	/bin/rm -f bin/*
	/bin/rm -f clib/libisis1.a clib/libisis2.a mlib/libisism.a
	/bin/rm -f lib/*.a
	- (cd allegro_clib; /bin/rm -f *.fasl libisis1.a libisis2.a libisism.a *.a )
	- (cd lucid_clib; /bin/rm -f *.sbin libisis1.a libisis2.a libisism.a *.a )
	(cd run_demos; /bin/rm -f *.log core )
	(cd run_isis; /bin/rm -f *.log core )

