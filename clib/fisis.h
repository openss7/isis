/*  $RCSfile: fisis.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:47 $  */
#define PGINIT                  1
#define PGXFER                  2
#define PGJOIN_AUTHEN           3
#define PGMONITOR               4
#define PGCREDENTIALS           5
#define PGLOGGED                6
#define PGDONTCREATE            7
#define PGBIGXFER               8
#define PGINCARN                9
#define PGCLIENT_AUTHEN         10
#define PGWAITDELAY             11

#define ORIGINAL                0
#define TAKEOVER                1

#define WFAIL                   0
#define WRECOVER                1
#define WJOIN                   2
#define WLEAVE                  3
#define WMONITOR                -1

#define sleep(n)                isis_sleep(n)
#define t_wait(cond)            t_wait_l(cond, 0)

#define IEUNKNOWN               -1
#define IERESTRICTED            -2
#define IETOOLONG               -3
#define IENOTLOCAL              -4
#define IEBADARG                -5
#define IETOTFAIL               -6
#define IENOTIMP                -7
#define IEAGAIN                 -8
#define IENOTALLOWED            -9
#define IECONNECT               -10
#define IEBROKEN                -11
#define IEBADFITEM              -12
#define IEMISSMATCH             -13
#define IENOTMEMB               -14
#define IEGUARD                 -15
#define IEGPARSE                -16
#define IEMUSTJOIN              -17
#define IENOTLOGGED             -18
#define IERELOG                 -19
#define IENOCKPT                -20
#define IELOGIO                 -21
#define IEABORT                 -22
#define IENESTEDTRANS           -23
#define IENOTRANS               -24
#define IEPARTICIPANT           -25
#define IENOTRANSRECOV          -26

#define ISISBLOCK               512

#define ASIZE                   6
#define EIDSIZE                 10
#define BVSIZE                  4
