#   $RCSfile: makefile,v $ $Revision: 2.1 $ $Date: 90/07/31 16:17:22 $
#
# pmake
# Makefile
# modified by Mark J. Steiglitz, 10/26/89
#

# Make file for pmake - ISIS parallel make application
#
S = ../../../demos/pmk
MACHINE = $(MCHTYPE)
ABSDIR = `cd ../../bin; pwd`
CFLAGS = -D$(MACHINE) $(MCHDEPCFLAGS) -DABSDIR="\"$(ABSDIR)\"" -I../../../include -I$(S) -c -g
LIBS = ../../clib/libisis1.a ../../clib/libisis2.a ../../mlib/libisism.a
PUBOBJS = check.o input.o macro.o main.o make.o reader.o rules.o 
PMKOBJS = pmklst.o pmkgph.o pmkio.o pmklib.o pmksched6.o pmkexec.o
INCLUDES = $(S)/make.h $(S)/pmkdat.h ../../../include/isis.h

all: warn pmake pmkexec

warn:
	@echo Warning: This version of pmk looks for binaries in $(ABSDIR)

realclean:	clean
	/bin/rm -f pmake pmkexec
clean:
	/bin/rm -f $(PUBOBJS) 2>> /dev/null
	/bin/rm -f $(PMKOBJS) 2>> /dev/null

install:
	/bin/rm -f ../../bin/pmake ../../bin/pmkexec
	ln pmake pmkexec ../../bin

pmake: $(PUBOBJS) pmklst.o pmklib.o $(LIBS)
	cc -o pmake $(PUBOBJS) pmklst.o pmklib.o $(LIBS)

pmkexec: $(PMKOBJS) $(LIBS)
	cc -o pmkexec $(PMKOBJS) $(LIBS) -lrpcsvc
# files ported from public domain make utility

check.o : $(S)/check.c $(INCLUDES)
	cc $(CFLAGS) $(S)/check.c

input.o : $(S)/input.c $(INCLUDES)
	cc $(CFLAGS) $(S)/input.c

macro.o : $(S)/macro.c $(INCLUDES)
	cc $(CFLAGS) $(S)/macro.c

main.o : $(S)/main.c $(INCLUDES)
	cc $(CFLAGS)  $(S)/main.c

make.o : $(S)/make.c $(INCLUDES)
	cc $(CFLAGS) $(S)/make.c

reader.o : $(S)/reader.c $(INCLUDES)
	cc $(CFLAGS) $(S)/reader.c

rules.o : $(S)/rules.c $(INCLUDES)
	cc $(CFLAGS) $(S)/rules.c

# files unique to parallel make

pmklst.o : $(S)/pmklst.c $(INCLUDES)
	cc $(CFLAGS) $(S)/pmklst.c 

pmkgph.o : $(S)/pmkgph.c $(INCLUDES)
	cc $(CFLAGS) $(S)/pmkgph.c

pmkio.o : $(S)/pmkio.c $(INCLUDES)
	cc $(CFLAGS) $(S)/pmkio.c

pmklib.o : $(S)/pmklib.c $(INCLUDES)
	cc $(CFLAGS) $(S)/pmklib.c

pmksched6.o : $(S)/pmksched6.c $(INCLUDES)
	cc $(CFLAGS) $(S)/pmksched6.c

pmkexec.o : $(S)/pmkexec.c $(INLCUDES)
	cc $(CFLAGS) $(S)/pmkexec.c

