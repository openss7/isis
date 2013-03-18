/*  $RCSfile: tk_rmgr.c,v $ $Revision: 2.21 $ $Date: 90/07/25 13:48:55 $  */
/*
 *      tk_rmgr.c  --  recovery manager: client code
 *      Originally coded by Frank Schmuck
 *
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
 *
 */

/*  changes for new ISIS interface:
 *  12-14-87 Frank: changed all bcast's, and msg_xxx to new version.
 *  (there are no reply's in tk_rmgr.c)
 *  changes for new Ken Kane's log manager:
 *  04-26-88 Frank: Add 'key' to the view logging facility.
 *      rmgr_start(stop)_log
 *      rmgr_getinfo
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#ifndef GOULD
#include <fcntl.h>
#endif 
#include <sys/file.h>
#include "isis.h"
#include "rmgr.h"

extern sview *site_getview();

int  rmgr_optv = FALSE;

/*********************************************************************
*
*  rmgr_update
*
*********************************************************************/

static  void  write_entry(rcfile, pgname, program, argv, envp)
    FILE *rcfile;
    char *pgname;
    char *program;
    char *argv[], *envp[];
{
    if (program == NULL) return;
    fprintf(rcfile, "\"%s\"\t%s\n  {", pgname, program);
    if (argv != NULL  &&  *argv != NULL) for (;;) {
        fputs(*argv++, rcfile);
        if (*argv == NULL) break;
        fputs(", ", rcfile);
    }
    fputs("}\n  {", rcfile);
    if (envp != NULL  &&  *envp != NULL) for (;;) {
        fputs(*envp++, rcfile);
        if (*envp == NULL) break;
        fputs(", ", rcfile);
    }
    fputs("}\n", rcfile);
}


#define BUFSIZE 1024

int  rmgr_update(key, program, argv, envp)
    char *key;
    char *program;
    char *argv[], *envp[];
{
    int    lock;                /* file descriptor for rmgr.lock file */
    FILE   *rcfile;             /* rmgr.rc file */
    FILE   *rc2file;            /* rmgr.rc2 file */
    int    entry_no;            /* number of old rmgr.rc entry */
    int    entry_count;
    char   *buffer;

    /*** lock rmgr.rc file ***/
    lock = open(RMGRLOCK, O_WRONLY | O_CREAT | O_EXCL, 0744);
    if (lock < 0) return RM_ELOCKED;
    close(lock);
    /*** try ot find previous entry in rmgr.rc file ***/
    if ((rcfile = fopen(RMGRRC, "r+")) == NULL) {
        fputs("rmgr_update: can't open rmgr.rc file\n", stderr);
        return -1;
    }
    buffer = malloc(BUFSIZE);
    entry_no = entry_count = -1;
    while (fgets(buffer, BUFSIZE, rcfile) != NULL) {
        if (buffer[0] == '"') {
            entry_count++;
            /*** compare name in buffer with key ***/
            {   register char *p1 = buffer;
                register char *p2 = key-1;
            
                while (*++p1 == *++p2) ;
                if (*p1 == '"'  &&  *p2 == '\0') {
                    entry_no = entry_count;
                    break;
                }
            }
        }
        /*** skip to end of line ***/
        while (buffer[strlen(buffer)-1] != '\n') {
            if (fgets(buffer, BUFSIZE, rcfile) == NULL) break;
        }
    }
    if (entry_no < 0) {
        /*** append new entry to rmgr.rc file ***/
        if (program == NULL) {
            free(buffer);
            unlink(RMGRLOCK);
            return RM_ENOTFOUND;
        }
        fseek(rcfile, 0L, 2);
        write_entry(rcfile, key, program, argv, envp);
        fclose(rcfile);
    } else {
        /*** replace old entry by new entry in rmgr.rc file ***/
        fseek(rcfile, 0L, 0);
        if ((rc2file = fopen(RMGRRC2, "w+")) == NULL) {
            fputs("rmgr_update: can't open rmgr.rc2 file\n", stderr);
            free(buffer);
            return -1;
        }
        entry_count = -1;
        while ((fgets(buffer, BUFSIZE, rcfile)) != NULL) {
            if (buffer[0] == '"') {
                entry_count++;
                if (entry_count == entry_no) {
                    write_entry(rc2file, key, program, argv, envp);
                }
            }
            if (entry_count != entry_no) fputs(buffer, rc2file);
            while (buffer[strlen(buffer)-1] != '\n') {
                if (fgets(buffer, BUFSIZE, rcfile) == NULL) break;
                if (entry_count != entry_no) fputs(buffer, rc2file);
            }
        }
        fclose(rcfile);
        fclose(rc2file);
        rename(RMGRRC2, RMGRRC);
    }
    free(buffer);
    unlink(RMGRLOCK);
    return 0;
}


/*********************************************************************
*
*  rmgr_register, rmgr_unregister
*
*********************************************************************/

int  rmgr_register(key)
    char  *key;
{
    int         n;
    long        answer;
    address     addr;

    /*** send registration message to local rmgr ***/
    addr = ADDRESS(my_site_no, my_site_incarn, RMGR, 0);
    n = cbcast(&addr, MSG_REGISTER, "%d %s", getpid(), key, 1, "%d", &answer);
    return (n == 1)? (int)answer: -1;
}

int  rmgr_unregister()
{
    int         n;
    long        answer;
    address     addr;

    /*** send unregistration message to local rmgr ***/
    addr = ADDRESS(my_site_no, my_site_incarn, RMGR, 0);
    n = cbcast(&addr, MSG_UNREGISTER, "%d", getpid(), 1, "%d", &answer);
    return (n == 1)? (int)answer: -1;
}


/*********************************************************************
*
*  rmgr_getinfo
*
*********************************************************************/

static  rmgr_info  info = { 0, {-1} };
static  condition  rmup_cond;
static  bool       slist[MAX_SITES];
static  int        news_count;

void  rmgr_mh_rmup(mp)
    message  *mp;
{
    long  site;

    msg_get(mp, "%d", &site);
    if (rmgr_optv) printf("rmgr_mh_rmup: news from site %d\n", site);
    if (slist[site]) {
        news_count++;
        t_sig(&rmup_cond, 0);
    }
}


rmgr_info  *rmgr_getinfo(pgname, noblock)
    char *pgname;
    int  noblock;
{
static  char     filename[256];
        int      i, cc, fd;
        int      ns, nr, news_last;
        sview    *sv;
static  address   alist[PG_ALEN];
static  groupview vlist[PG_ALEN];
static  address   wlist[PG_ALEN];

#define  iv  info.rm_view
#define  vv  vlist[i]

    /*** get last pg view from view file ***/
    sprintf(filename, "%s/%s", RMGRDIR, pgname);
    fd = open(filename, O_RDWR, 0);
    if (fd < 0) {
        cc = 0;
    } else {
        cc = read(fd, (char *)&info.rm_key[0], sizeof(rmgr_viewlog));
        close(fd);
    }
    if (cc < sizeof(rmgr_viewlog)) {
        /*** no previous view recorded ***/
        info.rm_mode = 0x00;
        info.rm_key[0] = '\0';
        info.rm_view.gv_viewid = 0;
        info.rm_view.gv_incarn = 0;
        info.rm_view.gv_gaddr = NULLADDRESS;
        strncpy(info.rm_view.gv_name, pgname, PG_GLEN);
        return &info;
    } 
    /*** slist:= list of sites in lastview ***/
    for (i = 0; i < MAX_SITES; i++) slist[i] = 0;
    for (i = 0; i < info.rm_view.gv_nmemb; i++) {
            slist[info.rm_view.gv_members[i].addr_site] = TRUE;
    }
    slist[my_site_no] = FALSE;
    /*** subscribe to news subject RMGR:UP ***/
    news_count = news_last = 0;
    news_subscribe("RMGR:UP", GENERIC_RM_UP, 0);
    for (;;) {
        /*** build alist from slist ***/
        sv = site_getview();
        ns = 0;
        for (i = 0; i < MAX_SITES; i++) {
            if (slist[i]) {       
                alist[ns++] = ADDRESS(i, (int)sv->sv_incarn[i], RMGR, MSG_LASTVIEW);
            }
        }
        alist[ns] = NULLADDRESS;
        /*** bcast msg to rmgr's ***/
        if (ns > 0) {
            nr = cbcast_l("l", alist, "%s", pgname, ns, "%p %a", vlist, wlist);
        } else {
            nr = 0;
        }
        /*** check if any received views is more recent than rm_view ***/ 
        for (i = 0; i < nr; i++) {
            slist[wlist[i].addr_site] = FALSE;
            if (vv.gv_incarn > iv.gv_incarn
            ||  vv.gv_incarn == iv.gv_incarn  &&  vv.gv_viewid > iv.gv_viewid) {
                /*** rm_view is not most recent ***/
                info.rm_mode = RM_LOG | RM_SURE;
                news_cancel("RMGR:UP");
                return &info;
            }
        }
        if (nr == ns) {
            /*** no other site has a more recent view ***/
            info.rm_mode = RM_LOG | RM_RECENT | RM_SURE;
            news_cancel("RMGR:UP");
            return &info;
        }
        if (noblock) {
            /*** don't have enough answeres yet, but client does'nt want to wait ***/
            info.rm_mode = RM_LOG | RM_RECENT;
            news_cancel("RMGR:UP");
            return &info;
        }
        /*** wait for news on RMGR:UP subject, then try again ***/
        if (news_count == news_last) {
            if (rmgr_optv)
                printf("rmgr_getview: waiting for news (%d)\n", news_count);
            news_last = news_count;
            t_wait_l(&rmup_cond, "isis system: wants news on RMGR:UP");
        }
    }
}


/*********************************************************************
*
*  rmgr_start_log,  rmgr_stop_log
*
*********************************************************************/

#define MAXLOG 20

static  struct log {
        address  gad;
        int      fd;
        char     key[RMLEN];
        int      mid;
    } logtable[MAXLOG];

static  int lognext = 0;


static  void  rmgr_monitor(pg, lt)
    groupview    *pg;
    struct log   *lt;
{
    rmgr_viewlog  rlog;
    int  cc;

    /*** read old view from view file ***/
    if (lseek(lt->fd, (off_t)0, L_SET) < (off_t)0) {
        panic("rmgr_monitor: lseek 1 failed");
    }
    cc = read(lt->fd, (char *)&rlog, sizeof(rmgr_viewlog));
    if (cc = sizeof(rmgr_viewlog)) {
        if (rlog.rl_view.gv_incarn == pg->gv_incarn
        &&  rlog.rl_view.gv_viewid >= pg->gv_viewid) {
            /* view has already been written by another member
             * of the group at this site */
            return;
        }
    } else if (cc != 0) {
        panic("rmgr_monitor: read failed");
    }
    /*** write new view into pg view file ***/
    if (lseek(lt->fd, (off_t)0, L_SET) < (off_t)0) {
        panic("rmgr_monitor: lseek 2 failed");
    }
    strcpy(rlog.rl_key, lt->key);
    rlog.rl_view = *pg;
    cc = write(lt->fd, (char *) &rlog, sizeof(rmgr_viewlog));
    if (cc < sizeof(rmgr_viewlog)) {
        panic("rmgr_monitor: write failed");
    }
    if (fsync(lt->fd) != 0) {
        panic("rmgr_monitor: fsync failed");
    }
}


int  rmgr_start_log(gad, key)
    address     *gad;
    char        *key;
{
    char        filename[256];          /* view file path name */
    int         fd;                     /* view file descriptor */
    int         mid;                    /* monitor id */
    groupview   *pgv;                   /* current group view */


    if (strlen(key) >= RMLEN)
        panic("rmgr_start_log: key too long");

    /*** need new entry in logtable ***/
    if (lognext >= MAXLOG) return -1;

    /*** get current pg view ***/
    if ((pgv = pg_getview(gad)) == NULL) return -3;
    
    /*** open/create view file ***/
    sprintf(filename, "%s/%s", RMGRDIR, pgv->gv_name);
    fd = open(filename, O_RDWR | O_CREAT, 0644);
    if (fd < 0) return -4;
    
    /*** start pg monitor and make logtable entry ***/
    logtable[lognext].gad = *gad;
    logtable[lognext].fd = fd;
    strcpy(logtable[lognext].key, key);
    if ((mid = pg_monitor(gad, rmgr_monitor, &logtable[lognext])) < 0)
        return -5;
    logtable[lognext].mid = mid;
    /*** write current pg view into view file ***/
    rmgr_monitor(pgv, &logtable[lognext]);
    
    lognext++;
    return 0;
}


int  rmgr_stop_log(gad, key)
    address     *gad;
    char        *key;
{
    int         i;

    /*** find entry in logtable ***/
    for (i = 0; i < lognext; i++) {
        if (logtable[i].gad.addr_groupid == gad->addr_groupid
        &&  strcmp(logtable[i].key, key) == 0)
            break;
    }
    if (i >= lognext) return -1;

    /*** cancel pg monitor ***/
    if (pg_monitor_cancel(logtable[i].mid) != 0) return -1;
    
    /*** close view file and remove logtable entry ***/
    close(logtable[i].fd);
    lognext--;
    if (lognext > i) logtable[i] = logtable[lognext];
    
    return 0;
}

int  rmgr_lasttofail(gname, key, old_incarn_p, noblock)
    char  *gname, *key;
    int   *old_incarn_p;
    int   noblock;
{
    rmgr_info  *ri;

    ri = rmgr_getinfo(gname, noblock);
    switch (ri->rm_mode) {

    case RM_SURE | RM_LOG | RM_RECENT:
        /*** this site was among the last to fail ***/
        if (ri->rm_view.gv_members[0].addr_site == my_site_no
        &&  strcmp(ri->rm_key, key) == 0) {
            if (old_incarn_p)
                *old_incarn_p = ri->rm_view.gv_incarn;
            return 1;
        } else {
            return 0;
        }

    case RM_SURE | RM_LOG:
        /*** this site was not the last to fail ***/
        return 0;

    case RM_LOG | RM_RECENT:
    case RM_LOG:
        /*** can't tell because other sites are still down ***/
        return -1;

    default:
        /*** no view is logged ***/
        return 2;
    }
}

/*********************************************************************
*
*  rmgr_create,  rmgr_join,  rmgr_restart
*
*********************************************************************/

address *rmgr_create(rmi)
    rmgr_info  *rmi;
{
    long     incarn;
    address  *gaddr;
    message  *mp;
    char     subj[SUBJLEN];

    /*** calculate new group incarnation number ***/
    if ((rmi->rm_mode & RM_LOG) == 0) {
        incarn = (long)1;
    } else if ((rmi->rm_mode & (RM_RECENT|RM_SURE)) == (RM_RECENT|RM_SURE)) {
        incarn = (long)rmi->rm_view.gv_incarn+1;
    } else {
        /*** can't create new group if previous incarn not known ***/
        return &NULLADDRESS;
    }
    
    /*** create new group ***/
    if (rmgr_optv)
         printf("cl_rmgr: creating(%s, %d)\n", rmi->rm_view.gv_name, incarn);
    gaddr = pg_join(rmi->rm_view.gv_name, PG_INCARN, (int)incarn, 0);
    if (gaddr->addr_site == 0) return &NULLADDRESS;
    
    /*** announce group creation on the news ***/
    mp = msg_gen("%d %A", incarn, gaddr, 1);
    sprintf(subj, "RM_NEWS:%s", rmi->rm_view.gv_name);
    news_post((site_id*)NULL, subj, mp, 0);
    msg_delete(mp);
    return gaddr;
}


static  condition  rmj_cond;
static  address    rmj_gaddr;
static  char      *rmj_pgname;
static  int        rmj_incarn;

address *rmgr_join(rmi)
    rmgr_info  *rmi;
{
    char     subj[SUBJLEN];
    address  *gaddr;

    /*** subscribe to group creation news ***/
    rmj_pgname = rmi->rm_view.gv_name;
    rmj_incarn = (rmi->rm_mode & RM_LOG)? rmi->rm_view.gv_incarn: 0;
    sprintf(subj, "RM_NEWS:%s", rmi->rm_view.gv_name);
    if (news_subscribe(subj, GENERIC_RM_NEWS, 1) < 0) return &NULLADDRESS;

    /*** if group doesn't exist: wait for news announcement ***/
    gaddr = pg_lookup(rmi->rm_view.gv_name);
    if (gaddr->addr_site == 0) {
        if (rmgr_optv)
            printf("cl_rmgr: join: waiting for news ...\n");
        t_wait_l(&rmj_cond, "isis system: rmgr_join wants news");
        gaddr = &rmj_gaddr;
    }
    /*** cancel news subscription; join the group ***/
    news_cancel(subj);
    if (rmgr_optv)
    {
         print("cl_rmgr: joining(%s)... ", rmi->rm_view.gv_name);
         paddr(gaddr);
         print("\n");
    }
    return pg_join(rmi->rm_view.gv_name, 0);
}

void  rmgr_mh_rmnews(mp)
    message  *mp;
{
    long     incarn;
    address  gaddr;

    msg_get(mp, "%d %a", &incarn, &gaddr);
    if (incarn > rmj_incarn) {
        rmj_gaddr = gaddr;
        t_sig(&rmj_cond, 0);
    }
}


address  *rmgr_restart(pgname)
    char  *pgname;
{
    rmgr_info  *rmi;
    int         create_flag;
    address     *gaddr;

#define  rv  rmi->rm_view

    /*** find out whether to create or join the group ***/
    rmi = rmgr_getinfo(pgname, 0);
    if (rmgr_optv) {
        printf("cl_rmgr: rm_mode = %d\n", rmi->rm_mode);
        printf("         rm_view = {%d.%d, %d, [gaddr], \"%s\", %d, %d, [alist]}\n",
            VMM(rv.gv_viewid), rv.gv_incarn, rv.gv_name, rv.gv_nmemb, rv.gv_nclient);
    }
    if (! (rmi->rm_mode & RM_LOG)) {
        gaddr = pg_lookup(pgname);
        create_flag = (gaddr->addr_site == 0);
    } else if (rmi->rm_mode & RM_RECENT) {
        create_flag = (rmi->rm_view.gv_members[0].addr_site == my_site_no);
    } else {
        create_flag = 0;
    }
    
    /*** create or join group ***/
    if (create_flag) {
        return rmgr_create(rmi);
    } else {
        return rmgr_join(rmi);
    }
}

char *
rmgrbuf_init(buf, fmt, arg)
  register char *buf, *fmt, *arg;
  {
        if(*buf == 0)
            sprintf(buf, fmt, arg);
        return(buf);
  }
