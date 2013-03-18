/*
 *	Originally by Ken Birman
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
char prstat_rcsid[] = "$Revision: 2.15 $$Date: 90/08/06 13:48:58 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/util/RCS/prstat.c,v $";
#include "isis.h"

#ifndef _TIME_
#include <time.h>
#endif  _TIME_

#include "pr_stats.h"

int     rate = 10;
int     isis_portno;

#define pgtok(a)        ((a)/(1024/NBPG))

main(argc, argv)
  char **argv;
  {
        void main_loop();
        int isis_portno = 0;
        while(--argc)
        {
            register n = atoi(*++argv);
            if(n > 0 && n < 300)
                rate = n;
            else if(n > 0 && n < 4096)
                isis_portno = n;
            else
                panic("Usage: prstat [port-no] rate");
        }
        isis_init(isis_portno);
        isis_mainloop(main_loop, NULLARG);
  }

struct isis_stats old_sbuf, new_sbuf, dt;

void
main_loop()
  {
        register count = 0;
        isis_start_done();
        printf("NT = tasks running now.  FORK = forks this cycle.  CTXT = context switches\n");
        printf("C+ = cbcasts started.  C- = cbcasts finished.  Same for A+, A-, G+, G\n");
        printf("GA = gbcast aborts.  FAN = average broadcast fanout\n");
        printf("CP = cbcast packets generated.  CM = messages they contained.\n");
        printf("CR = local cbcast deliveries.  DP = cbcast dups discarded.\n");
        printf("AS = astore rounds run.  SYS = messages client->system\n");
        printf("REP = messages system->clients.  FA = cached view faults.  LK = namespace lookups\n");
        printf("VC = View changes.  IS = Intersite packets sent.  IG = Intersite received.\n");
        printf("IM = Valid non-dup non-ack messages. NL, ND = Local/total deletable astore entries\n");
        print("CG = Number of times protos congested. CGF tells how.\n");
        printf("NB = Kbytes in use; MB = Kbytes in messages, IB = Kbytes in intersite layer\n");
        pr_stats(&old_sbuf);
        old_sbuf.is_time -= rate;
        forever
        {
            if(count++ % 30 == 0)
                phdr();
            pr_stats(&new_sbuf);
            pstats();
            old_sbuf = new_sbuf;
            if(rate)
                sleep(rate);
        }
  }

phdr()
  {
        printf("\n  TIME   NT FORK CTXT  C+  C- A+ A- G+ G- GA FAN  CP  CM  CR  DP AS SYS REP FA LK VC  IS  IG  IM NL ND CG  CGF   NB  MB  IB\n");
  }

pstats()
  {
        register n, fanout, fanouth, fanoutl;
        int hr, mn, sec;
        register struct tm *tp;
        for(n = 0; n < S_NSTATS; n++)
            dt.is_stats[n] = new_sbuf.is_stats[n]-old_sbuf.is_stats[n];
        if((dt.is_time = new_sbuf.is_time-old_sbuf.is_time) < rate)
	    dt.is_time = rate;
        if(fanout = dt.is_stats[S_CBSTART]+dt.is_stats[S_ABSTART]+dt.is_stats[S_GBSTART])
            fanout = dt.is_stats[S_FANOUT]*10/fanout;
        fanouth = fanout/10;
        fanoutl = fanout%10;
#if   	(SUN || AUX)
        tp = localtime(&new_sbuf.is_time);
        printf("%.2d:%.2d.%.2d%3d%5d%5d%4d%4d%3d%3d%3d%3d%3d%2d.%1d",
            tp->tm_hour, tp->tm_min, tp->tm_sec,
#else
        printf("%.2d:%.2d.%.2d%3d%5d%5d%4d%4d%3d%3d%3d%3d%3d%2d.%1d",
            0, 0, 0,
#endif	(SUN || AUX)
            new_sbuf.is_ntasks,
            dt.is_stats[S_NFORK],
            dt.is_stats[S_NSWTCH],
            dt.is_stats[S_CBSTART],
            dt.is_stats[S_CBDONE],
            dt.is_stats[S_ABSTART],
            dt.is_stats[S_ABDONE],
            dt.is_stats[S_GBSTART],
            dt.is_stats[S_GBDONE] - dt.is_stats[S_GBABORTS],
            dt.is_stats[S_GBABORTS],
            fanouth, fanoutl);
        printf("%4d%4d%4d%4d%3d%4d%4d%3d%3d%3d%4d%4d%4d%3d%3d%3d  ",
            dt.is_stats[S_CBSENT],
            dt.is_stats[S_CBCOUNT],
            dt.is_stats[S_CBDELIV],
            dt.is_stats[S_CBDUP],
            dt.is_stats[S_ASROUNDS],
            dt.is_stats[S_SYSCALLS],
            dt.is_stats[S_CLSENT],
            dt.is_stats[S_CFAULT],
            dt.is_stats[S_LOOKUP],
            dt.is_stats[S_VCHANGE],
            dt.is_stats[S_ISENT],
            dt.is_stats[S_IGOT],
            dt.is_stats[S_IMSGS], 
            new_sbuf.is_nlocdelete,
            new_sbuf.is_ndelete,
            dt.is_stats[S_CONGEST]);
        if(new_sbuf.is_congest&IS_CONGEST)
	{
            if(new_sbuf.is_congest&IS_INTER)
		putchar('i');
	    else
		putchar('-');
            if(new_sbuf.is_congest&IS_MEM)
		putchar('m');
	    else
		putchar('-');
            if(new_sbuf.is_congest&IS_MSG)
		putchar('M');
	    else
		putchar('-');
            if(new_sbuf.is_congest&IS_TASK)
		putchar('t');
	    else
		putchar('-');
	}
	else
	    printf("    ");
        printf("%4d%4d%4d\n",
            new_sbuf.is_memuse/1000,
            new_sbuf.is_msgmem/1000,
            new_sbuf.is_inter/1000);
        fflush(stdout);
  }

pr_stats(buf)
  struct isis_stats *buf;
  {
        register message *mp = msg_newmsg();
        isis(PR_STATS, mp, buf, sizeof(isis_stats));
        msg_delete(mp);
  }
