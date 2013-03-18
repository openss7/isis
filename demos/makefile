#  $RCSfile: makefile,v $ $Revision: 2.17 $ $Date: 90/08/14 09:24:14 $  
SHELL=/bin/sh
S = ../../demos
CFLAGS = $(MCHDEPCFLAGS) -D$(MCHTYPE) -I../../include -I$(S) -c  $(OPTIM)
DBGCFLAGS = $(MCHDEPCFLAGS) -D$(MCHTYPE) -I../../include -I$(S) -c -g
LDFLAGS = $(MCHDEPCFLAGS)
LIBS = ../clib/libisis1.a ../clib/libisis2.a ../mlib/libisism.a
GRIDFLAGS = MCHDEPCFLAGS="$(MCHDEPCFLAGS)" OPLIBS="$(OPLIBS)" OPTIM="$(OPTIM)"
PMAKEFLAGS = MCHDEPCFLAGS="$(MCHDEPCFLAGS)" OPLIBS="$(OPLIBS)"

all:	testtasks twenty qa testjoins silentjoins \
	bank teller selftest \
	lh_sp_test wanClient wanServer \
	lisp_test grid sun_grid fortran_test pmake spread
	

twenty:	twenty.o $(LIBS)
	$(CC) $(LDFLAGS) -o twenty twenty.o $(LIBS) $(OPLIBS)
qa:	qa.o $(LIBS)
	$(CC) $(LDFLAGS) -o qa qa.o $(LIBS) $(OPLIBS)
testjoins:	testjoins.o $(LIBS)
	$(CC) $(LDFLAGS) -o testjoins testjoins.o $(LIBS) $(OPLIBS)
silentjoins:	silentjoins.o $(LIBS)
	$(CC) $(LDFLAGS) -o silentjoins silentjoins.o $(LIBS) $(OPLIBS)
bank: bank.o $(LIBS)
	$(CC) -o bank $(LDFLAGS) bank.o $(LIBS) $(OPLIBS)
teller: teller.o $(LIBS)
	$(CC) -o teller $(LDFLAGS) teller.o $(LIBS) $(OPLIBS)
fortran_test:  ftest.o $(LIBS)
	-if test $(MCHTYPE) = SUN3 -o $(MCHTYPE) = SUN4; then f77 $(LDFLAGS) -o fortran_test ftest.o $(LIBS) $(OPLIBS); fi
testtasks:	testtasks.o $(LIBS)
	$(CC) -o testtasks $(LDFLAGS) testtasks.o $(LIBS) $(OPLIBS)
selftest:	selftest.o $(LIBS)
	$(CC) -o selftest $(LDFLAGS) selftest.o $(LIBS) $(OPLIBS)
lisp_test:	lisp_test.o $(LIBS)
	$(CC) -o lisp_test $(LDFLAGS) lisp_test.o $(LIBS) $(OPLIBS)
lh_sp_test: 	lh_sp_test.o $(LIBS)
	$(CC) -o lh_sp_test $(LDFLAGS) lh_sp_test.o $(LIBS) $(OPLIBS)
wanClient: 	wanClient.o $(LIBS)
	$(CC) -o wanClient $(LDFLAGS) wanClient.o $(LIBS) $(OPLIBS)
wanServer: 	wanServer.o $(LIBS)
	$(CC) -o wanServer $(LDFLAGS) wanServer.o $(LIBS) $(OPLIBS)


twenty.o:	$(S)/twenty.c; $(CC) $(CFLAGS) $(S)/twenty.c
qa.o:		$(S)/qa.c; $(CC) $(CFLAGS) $(S)/qa.c
testjoins.o:	$(S)/testjoins.c; $(CC) $(CFLAGS) $(S)/testjoins.c
silentjoins.o:	$(S)/silentjoins.c; $(CC) $(CFLAGS) $(S)/silentjoins.c
bank.o: 	$(S)/bank.c; $(CC) $(CFLAGS) $(S)/bank.c
teller.o: 	$(S)/teller.c; $(CC) $(CFLAGS) $(S)/teller.c
ftest.o:        $(S)/ftest.F
	-if test $(MCHTYPE) = SUN3 -o $(MCHTYPE) = SUN4; \
	then $(COMPILE.F) $(CFLAGS) -D$(MCHTYPE) -I../../include -I$(S) -c $(S)/ftest.F; fi
testtasks.o:    $(S)/testtasks.c; $(CC) $(CFLAGS) $(S)/testtasks.c
selftest.o:    $(S)/selftest.c; $(CC) $(CFLAGS) $(S)/selftest.c
lisp_test.o:    $(S)/lisp_test.c; $(CC) $(CFLAGS) $(S)/lisp_test.c
lh_sp_test.o:	$(S)/lh_sp_test.c; $(CC) $(CFLAGS) $(S)/lh_sp_test.c
wanClient.o:	$(S)/wanClient.c; $(CC) $(CFLAGS) $(S)/wanClient.c
wanServer.o:	$(S)/wanServer.c; $(CC) $(CFLAGS) $(S)/wanServer.c

# Only build pmake if we can find the Sun rpc services library.
pmake:
	@-if test -d pmk; then                                       \
	  if test -r /lib/librpcsvc.a      -o                       \
		  -r /usr/lib/librpcsvc.a  -o                       \
		  -r /usr/local/lib/librpcsvc.a ;                   \
	  then echo Found rpcsvc library--building pmake demo;      \
	       echo cd pmk \; make $(PMAKEFLAGS) \; cd .. ;      \
	       cd pmk; make $(PMAKEFLAGS) ; cd .. ;                 \
	  else echo "Cannot find rpcsvc library--not building pmake"; \
	  fi   \
	fi

# Conditionally build grid and sun_grid depending what windowing 
# libraries we find.
grid:	do_grid
do_grid:	
	@-if test -r /lib/libX11.a       -o   \
                 -r /usr/lib/libX11.a    -o   \
                 -r /usr/local/lib/libX11.a ; \
        then echo Found X11 library--building grid demo for X Version 11;   \
	     echo make -f make_grid $(GRIDFLAGS) grid;                      \
	     make -f make_grid $(GRIDFLAGS) grid;                           \
	else echo Cannot find X11 library--not building grid demo for X Version 11;\
        fi

spread:	do_spread
do_spread:	
	@-if test -r /lib/libX11.a       -o   \
                 -r /usr/lib/libX11.a    -o   \
                 -r /usr/local/lib/libX11.a ; \
        then echo Found X11 library--building spreadsheet demo for X Version 11;   \
	     echo make -f make_grid $(GRIDFLAGS) spread;                      \
	     make -f make_grid $(GRIDFLAGS) spread;                           \
	else echo Cannot find X11 library--not building spreadsheet demo for X Version 11;\
        fi

sun_grid:	do_sun_grid
do_sun_grid:
	@-if test -r /lib/libsuntool.a       -o   \
                 -r /usr/lib/libsuntool.a    -o   \
                 -r /usr/local/lib/libsuntool.a ; \
        then echo Found suntools library--building grid demo for suntools;\
	     echo make -f make_grid $(GRIDFLAGS) sun_grid;                \
	     make -f make_grid $(GRIDFLAGS) sun_grid;                     \
        fi
