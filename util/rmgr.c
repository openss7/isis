/*
 *	Originally by Frank Schmuck
 *  rmgr.c  --  recovery manager
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

/*  changes for new ISIS interface:
 *  12-14-87 Frank: changed all replies and msg_xxx to new version,
 *      except for one msg_getfield in mh_cl_failed(). (There are no
 *      bcasts in rmgr.c)
 */

char rmgr_rcsid[] = "$Revision: 2.6 $$Date: 90/06/20 14:36:28 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/util/RCS/rmgr.c,v $";

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/signal.h>
#if(HPUX)
#include <time.h>
#else
#include <sys/time.h>
#endif
#include <sys/resource.h>
#include "isis.h"
#include "rmgr.h"

/*
 *  If a process fails more than five times within 30 seconds
 *  the recovery manager will give up on it:
 */
#define RESTART_TIME 30
#define RESTART_COUNT 5

extern  int  isis_socket;

static  bool  optv = FALSE;


/*********************************************************************
*
*  registration table 
*
*********************************************************************/

#define MAXREG  128

int  stat_reg;                  /* statistic: number of processes registered */

static  struct rt {
            char  key[RMLEN];
            int   pid;
            int   wid;
            int   restart_count;
            long  restart_time;
            bool  used;
        } reg_table[MAXREG] = { {"", 0, FALSE} };


static  struct rt  *reg_new()
/*
 *  alloacte new entry in registration table
 */
{
    register struct rt   *regp;
    
    for (regp = reg_table; regp < reg_table+MAXREG; regp++) {
        if (! regp->used) {
            stat_reg++;
            bzero(regp, sizeof (struct rt));
            regp->used = TRUE;
            return regp;
        }
    }
    return NULL;
}


void  reg_del(regp)
    register struct rt   *regp;
/*
 *  delete entry in registration table
 */
{
    stat_reg--;
    regp->used = FALSE;
}

 
static  struct rt  *reg_find(pid)
    register int pid;
/*
 *  find entry in registration table
 */
{
    register struct rt   *regp;
    
    for (regp = reg_table; regp < reg_table+MAXREG; regp++) {
        if (regp->used  &&  regp->pid == pid) return regp;
    }
    return NULL;
}

/*********************************************************************
*
*  read_entry  --  reads the next entry from the rmgr.rc file 
*  fork_exec   --  fork and exec a program
*
*********************************************************************/

#define MAXARG   64             /* max number of arg's + env's */
#define ARGLEN 2048             /* max leng of all arg + env strings */

static  char    *key, *program;
static  char    *argv[MAXARG];
static  char    **envp;
static  char    argbuf[ARGLEN];


static  int  read_entry(rcfile)
    register  FILE  *rcfile;
/*
 *  return 0:    key, program, argv, envp contain values read from rcfile.
 *  return EOF:  eof on rcfile was reached before complete entry was read.
 *  return >0:   read aborted because argv or argbuf overflowed.
 */
#define reteof  if (feof(rcfile)) return EOF
{
    register  char  c, *s;
    register  int   i;
    bool      done;

    /*** read key ***/
    key = NULL;
    while ((c = fgetc(rcfile)) != '"') {
        while (c != '\n') {
            if ((c = fgetc(rcfile)) == EOF) return EOF;
        }
    }
    s = argbuf;
    while ((*s++ = fgetc(rcfile)) != '"') reteof;
    *(s-1) = '\0';
    key = argbuf;

    /*** read program ***/
    while ((c = fgetc(rcfile))==' '  ||  c=='\t'  ||  c=='\n') reteof;
    program = s;
    *s++ = c;
    while ((c = fgetc(rcfile))!=' '  &&  c!='\t'  &&  c!='\n') {
        *s++ = c;
        reteof;
    }
    *s++ = '\0';
    
    /*** read argv ***/
    i = 0;
    done = FALSE;
    while (fgetc(rcfile) != '{') reteof;
    while (! done) {
        argv[i] = s;
        for (;;) {
            if ((*s = fgetc(rcfile)) == EOF) return EOF;
            if (*s == '}') {
                done = TRUE;
                break;
            }
            if (*s == ',') {
                if ((c = fgetc(rcfile)) != ' ') ungetc(c, rcfile);
                break;
            }
            if (s++ >= argbuf+ARGLEN) return 1;
        }
        *s++ = '\0';
        if (i++ >= MAXARG) return 2;
    }
    if (i == 1  &&  *argv[0] == '\0') i--;
    argv[i++] = NULL;

    /*** read envp ***/
    envp = &argv[i];
    done = FALSE;
    while (fgetc(rcfile) != '{') reteof;
    while (! done) {
        argv[i] = s;
        for (;;) {
            if ((*s = fgetc(rcfile)) == EOF) return EOF;
            if (*s == '}') {
                done = TRUE;
                break;
            }
            if (*s == ',') {
                if ((c = fgetc(rcfile)) != ' ') ungetc(c, rcfile);
                break;
            }
            if (s++ >= argbuf+ARGLEN) return 1;
        }
        *s++ = '\0';
        if (i++ >= MAXARG) return 2;
    }
    if (envp == &argv[i-1]  &&  *envp == '\0') i--;
    argv[i++] = NULL;

    /*** skip to end of line ***/
    while ((c = fgetc(rcfile)) != '\n'  &&  c != EOF) ;
    return 0;
}


static  int  fork_exec(rcfile)
    FILE  *rcfile;
/*
 *  forks a child and execs a program (program, argv, envp)
 */
{
    return isis_fork_execve(program, argv, envp, fileno(rcfile), -1);
}


/*********************************************************************
*
*  main_task 
*
*********************************************************************/

void main_task()
/*
 *  this routine is forked off by main()
 */
{
    struct stat statbuf;
    FILE        *rcfile;
    int         lock;
    struct rt   *regp;
    int         pid;
    int         rc;
    message     *mp;

    isis_start_done();
    if (optv) printf("rmgr %d: initial restart sequence started\n", my_site_no);
    /*** find or created rmgr directory ***/
    if (stat(RMGRDIR, &statbuf)) {
        /*** create rmgr directory ***/
        if (errno != ENOENT  ||  mkdir(RMGRDIR, 0777)) {
            perror(RMGRDIR);
            panic("can't create rmgr directory <%s>: rmgr exiting", RMGRDIR);
        }
    } else {
        if ((statbuf.st_mode & S_IFMT) != S_IFDIR)
            panic("can't create rmgr directory (file already exists)");
    }
    /*** lock and open rmgr.rc file ***/
    lock = open(RMGRLOCK, O_WRONLY | O_CREAT, 0744);
    close(lock);
    if ((rcfile = fopen(RMGRRC, "a+")) == NULL) {
        perror(RMGRRC);
        panic("can't open rmgr.rc file");
    }

    /* rewind(RMGRRC); */
    rewind(rcfile); 

    /*** read rmgr.rc file, restarting programs ***/
    while ((rc = read_entry(rcfile)) != EOF) {
        if (rc != 0) {
            fprintf(stderr, "rmgr %d: buffer overflow; can't restart \"%s\".\n",
              my_site_no, key);
            continue;
        }
        if ((pid = fork_exec(rcfile)) < 0) {
            continue;
        }
        if ((regp = reg_new()) == NULL) {
            fprintf(stderr, 
              "rmgr %d: registration table overflow; can't register \"%s\".\n",
              my_site_no, key);
            continue;
        }
        strncpy(regp->key, key, RMLEN);
        regp->pid = pid;
        sleep(3);
    }
    if (key != NULL) {
        fprintf(stderr,
          "rmgr %d: incomplete rmgr.rc entry; can't restart \"%s\".\n",
          my_site_no, key);
    }
    fclose(rcfile);
    unlink(RMGRLOCK);
    mp = msg_gen("%d", my_site_no);
    news_post(NULL, "RMGR:UP", mp, 0);
    msg_delete(mp);
    if (optv)
        printf("rmgr %d: initial restart sequence completed\n", my_site_no);
}


void
reaper()
{
    register pid;
    if((pid = wait(0)) > 0)
        restart_child(pid);
}


/*********************************************************************
*
*  mh_command
*
*********************************************************************/

static  void  mh_command(mp)
    message *mp;
/*
 *  handler for command messages
 */
{
    int      argc;
    char     *argv[32];
    int      i;
    char     *answer = "ok.\n";
    char     buffer[256];
    struct rt   *regp;


    getargs(mp, argc, argv, 32);

    if (strcmp(argv[0], "echo") == 0) {
        printf("rmgr %d:", my_site_no);
        for (i = 0; i < argc; i++) {
            printf(" %s", argv[i]);
        }
        printf("\n");

    } else if (strcmp(argv[0], "status") == 0) {
        sprintf(answer = buffer, "%d processes registered; %s\n",
          stat_reg, optv? "+v": "");

    } else if (strcmp(argv[0], "dump") == 0) {
        printf("rmgr %d:  <<registration table dump>>\n", my_site_no);
        for (regp = reg_table; regp < reg_table+MAXREG; regp++) {
            if (regp->used) {
                printf("    \"%s\"\tpid = %d, wid = %d, restart(%d %d)\n",
                  regp->key, regp->pid, regp->wid,
                  regp->restart_count, regp->restart_time);
            }
    }


    } else if (strcmp(argv[0], "+v") == 0) {
        optv = TRUE;

    } else if (strcmp(argv[0], "-v") == 0) {
        optv = FALSE;

    } else if (strcmp(argv[0], "help") == 0) {
        sprintf(answer = buffer,
          "Commands: echo, status, dump, +v, -v, help.\n");

    } else {
        sprintf(answer = buffer, "*** %s: unknown command\n", argv[0]);
    }
    reply(mp, "%s", answer);
}

/*********************************************************************
*
*  wh_proc        --  handler for proc_watch
*  restart_child  --  restart child after SIGCHLD
*
*********************************************************************/

static  void  restart(regp)
    struct rt   *regp;
/*
 *  restarts a process 
 */
{
    int         rc;
    int         lock;
    FILE        *rcfile;
    int         i;
    long        t;

    /*** give up if the process has failed too often ***/
    t = time(0);
    if (t - regp->restart_time > RESTART_TIME) {
        regp->restart_count = 0;
        regp->restart_time = t;
    } else {
        if (regp->restart_count++ > RESTART_COUNT) {
            fprintf(stderr, 
              "rmgr %d: restart count exceeded; giving up on \"%s\".\n",
              my_site_no, regp->key);
            reg_del(regp);
            return;
        }
    }
    /*** lock and open rmgr.rc file ***/
    for (i = 0; i < 10; i++) {
        lock = open(RMGRLOCK, O_WRONLY | O_CREAT | O_EXCL, 0744);
        if (lock >= 0) {
            close (lock);
            break;
        }
        sleep(2);
    }
    if (lock < 0) {
        fprintf(stderr,
          "rmgr %d: rmgr.rc file seems to be deadlocked\n", my_site_no);
    }
    if ((rcfile = fopen(RMGRRC, "r")) == NULL) {
        fprintf(stderr, 
          "rmgr %d: open (rmgr.rc) failed; can't restart \"%s\".\n",
          my_site_no, regp->key);
        unlink(RMGRLOCK);
        reg_del(regp);
        return;
    }
    /*** find rmgr.rc entry for restarting program ***/
    for (;;) {
        if ((rc = read_entry(rcfile)) == EOF) {
            fprintf(stderr, 
              "rmgr %d: rmgr.rc entry not found; can't restart \"%s\".\n", 
              my_site_no, regp->key);
            reg_del(regp);
            break;
        }
        if (strcmp(key, regp->key) == 0) {
            if (rc != 0) {
                fprintf(stderr, 
                    "rmgr %d: buffer overflow; can't restart \"%s\".\n",
                    my_site_no, key);
                reg_del(regp);
            } else if ((regp->pid = fork_exec(rcfile)) < 0) {
                reg_del(regp);
            }
            break;
        }
    }
    fclose(rcfile);
    unlink(RMGRLOCK);
}



static  void  wh_proc(paddr, what, arg)
    address  *paddr;
    int      what, arg;
/*
 *  this message is sent by isis whenever a local client fails 
 */
{
    int         pid;
    struct rt   *regp;

    /*** find entry in registration table ***/
    pid = paddr->addr_process;
    if ((regp = reg_find(pid)) == NULL) return;
    if (optv) printf("rmgr %d: wh_proc(%d)\n", my_site_no, pid);
    /*** restart the program ***/
    regp->wid = 0;
    restart(regp);
}

restart_child(pid)
    int  pid;
/*
 *  does not run as a task!
 */
{
    struct rt   *regp;

    /*** find entry in registration table ***/
    if ((regp = reg_find(pid)) == NULL) return;
    if (optv) printf("rmgr %d: restart_child(%d)\n", my_site_no, pid);
    /*** restart the program ***/
    restart(regp);
}


/*********************************************************************
*
*  mh_register    --  message handler for MSG_REGISTER
*  mh_unregister  --  message handler for MSG_UNREGISTER
*
*********************************************************************/

static  void  mh_register(mp)
    message *mp;
/*
 *  this message is sent by a local client that wants to register itself
 *  as the local representative for a pgroup 
 */
{
    long         pid;
    char        *key;
    struct rt   *regp;
    long        answer;
    address     *paddr;

    msg_get(mp, "%d %-s", &pid, &key);
    paddr = msg_getsender(mp);
    if (optv) printf("rmgr %d: register(\"%s\", %d)\n", my_site_no, key, pid);
    if ((regp = reg_find(pid)) == NULL) {
        regp = reg_new();
    }
    if (regp == NULL) {
        fprintf(stderr, 
           "rmgr %d: registration table overflow; can't register \"%s\".\n",
           my_site_no, key);
        answer = -1;
    } else {
        strncpy(regp->key, key, RMLEN);
        if (regp->pid == 0) {
            regp->pid = pid;
            regp->wid = proc_watch(paddr, wh_proc, 0);
        }
        answer = 0;
    }
    reply(mp, "%d", answer);
    return;
}


static  void  mh_unregister(mp)
    message *mp;
/*
 *  this message is sent by a local client that wants to register itself
 *  as the local representative for a pgroup 
 */
{
    long         pid;
    struct rt   *regp;
    long        answer;

    msg_get(mp, "%d", &pid);
    if (optv) printf("rmgr %d: unregister(%d)\n", my_site_no, pid);
    if ((regp = reg_find(pid)) == NULL) {
        answer = -1;
    } else {
        if (regp->wid != 0)
            proc_watch_cancel(regp->wid);
        reg_del(regp);
        answer = 0;
    }
    reply(mp, "%d", answer);
    return;
}


/*********************************************************************
*
*  message handling routines:  mh_lastview
*
*********************************************************************/

static  void  mh_lastview(mp)
    message *mp;
/*
 *  this message is sent by a restarting task at another site, if
 *  it wants to find out what was the last view logged at this
 *  site for a given pgroup
 */
{
    char    *pgname;
    char    filename[256];
    int     fd, cc;
    groupview  answer;

    msg_get(mp, "%-s", &pgname);
    if (optv) printf("rmgr %d: lastview(\"%s\")\n", my_site_no, pgname);
    /*** prepare answer ***/
    sprintf(filename, "%s/%s", RMGRDIR, pgname);
    fd = open(filename, O_RDWR, 0);
    if (fd < 0) {
        cc = 0;
    } else {
        cc = read(fd, (char *)&answer, sizeof(groupview));
        close(fd);
    }
    if (cc < sizeof(groupview)) {
        answer.gv_incarn = 0;
        answer.gv_viewid = 0;
    }
    /*** reply ***/
    reply(mp, "%P %A", &answer, 1, &my_address, 1);
    return;
}


/*********************************************************************
*
*  main 
*
*********************************************************************/

main(argc, argv)
    int  argc;
    char *argv[];
{
    int  client_port;   /* port number for talking to isis */
    int  i;
    int  pid;
    int  readmask;

    /*** read command line arguments ***/
    client_port = 0;
    i = 0;
    for (i = 1; i < argc; i++) {
        if (*argv[i] >= '0'  &&  *argv[i] <= '9') {
            client_port = atoi(argv[i]);
        } else if (strcmp(argv[i], "+v") == 0) {
            optv = TRUE;
        } else {
            fprintf(stderr, "usage: %s [+v] port\n", argv[0]);
            exit(-1);
        }
    }
    if (client_port == 0) {
        fprintf(stderr, "usage: %s [+v] port\n", argv[0]);
        exit(-1);
    }

    /*** set up isis stuff ***/
    freopen("/dev/null", "r", stdin);
    my_process_id = RMGR;
    isis_init(client_port);
#ifndef HPUX
    isis_signal(SIGCHLD, reaper, NULLARG);
#else
    isis_signal(SIGCLD, reaper, NULLARG);
#endif

    /*isis_entry(GENERIC_CL_FAILED, mh_cl_failed,  "mh_cl_failed");*/
    isis_entry(MSG_COMMAND,       mh_command,    "mh_command");
    isis_entry(MSG_LASTVIEW,      mh_lastview,   "mh_lastview");
    isis_entry(MSG_REGISTER,      mh_register,   "mh_register");
    isis_entry(MSG_UNREGISTER,    mh_unregister, "mh_unregister");

    /*** main loop ***/

    isis_mainloop(main_task, NULLARG);

    
}
