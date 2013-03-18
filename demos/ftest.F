c  $RCSfile: ftest.F,v $ $Revision: 2.2 $ $Date: 90/06/11 10:27:39 $  
c	This file gives an example of using ISIS from
c	inside a fortran (actually, F77 under UNIX) program.
c 	The program doesn't do much of anything.  It creates/joins
c	a process group and ships some data around.
c
c	Note in particular the way that address pointers are
c	handled.  This differs from the norm for ISIS, and
c	requires some non-standard looking format strings as well.
c
#include "fisis.h"

        external maintask, update, mongr
	call isistask(maintask, " maintask")
	call isistask(mongr, " monitor group")
	call isisentry(1, update, " update")
        print *, "ftest connecting to ISIS using port 1753"
        call isisinit(1753)
	call isismainloop(maintask, 0)
	end

	subroutine maintask
        implicit integer *4 (a-z)
        common gaddr
        external send, rcv, mongr
	print *, " this is the main task running"
        gaddr = pgjoin("fgroup", PGXFER, 0, send, rcv, PGMONITOR, mongr, 0, 0)
        print *, " main task joined fgroup, gaddr "
        call paddr(gaddr)
        call gaddr = pglookup("fgroup")
        print *, " main task lookup of fgroup, gaddr "
        call paddr(gaddr)
        igv = pggetview(gaddr)
	ivid = gvviewid(igv)
	ivh = ivid/65536
	ivl = ivid-ivh*65536
        print *, " lookup of group view, viewid ", ivh, ivl
        print *, " nmemb", gvnmemb(igv)
        print *, " nclient", gvnclient(igv)
        call pmembers(igv)
        call pclients(igv)
        isv = sitegetview()
        print *, " lookup of site view, viewid ", svviewid(isv)
        isl = svslist(isv, 0)
        isn = isl/256
        isc = isl-isn*256
        print *, " site view 0 = ", isn, icn
        isl = svslist(isv, 1)
        isn = isl/256
        isc = isl-isn*256
        print *, " site view 1 = ", isn, icn
        print *, " ... main task done"
	end

	subroutine update(imp)
        implicit integer (a-z)
	character *1 str(20)
        integer addr
	print *, " this is update, imp = ", imp
        ng =  msgget(imp, " %d,%s,%-a", n, str, addr)
        print *, "  got nitems", ng, ":  n=",n, "  str=", str, "  addr=", addr
        print *, " paddr(addr) ="
        call paddr(addr)
        call reply(imp, " %d", 55)
	end

        subroutine mongr(igv)
        implicit integer (a-z)
        common gaddr
        common /farble/ n,i(10)
        integer myaddr
        ivid = gvviewid(igv)
        ivh = ivid/65536
        ivl = ivid-ivh*65536
        print *, " got a new group, viewid ", ivh, ivl
        print *, " nmemb", gvnmemb(igv)
        print *, " nclient", gvnclient(igv)
        call pmembers(igv)
        call pclients(igv)
        n = n+1
	myaddr = myaddress()
	nsent=bcast(gvgaddr(igv),1,"%d,%s,%A[1]",n,"hello",myaddr,1000,"%d",i)
        print *, "  did a broadcast: nsent",nsent,"reply",i
        print *, " done in mongr"
        end

	subroutine send(iloc)
        implicit integer *4 (a-z)
        common gaddr
	print *," send called,iloc ",iloc," gaddr ",gaddr
        call paddr(gaddr)
	call xferout(1, " %d,%s", 37, " nixon")
        call xferout(2, " %A[1]", gaddr)
	end

	subroutine rcv(iloc, imp)
        implicit integer *4 (a-z)
        character *1  where(40)
        print *, " rcv called, iloc ", iloc, "  message ", imp
        if(iloc .eq. 1) goto 100
        mg = msgget(imp, " %-a", addr)
        print *, "msg_get returnss", mg, " case 2: addr "
	call paddr(addr)
	print *, " done in rcv"
        goto 200
100	mg = msgget(imp, " %d,%s", n, where)
        print *, " case 1: msg_get=", mg, " n ", n, " , str ", where
200    	end
