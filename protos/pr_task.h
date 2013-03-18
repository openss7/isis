/*  $RCSfile: pr_task.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:17 $  */
/*
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
 *
 */
/* task.h:  multitasking datastructures */

#include <setjmp.h>

#ifdef  CTHREADS
#define THREADS                 1

#include <cthreads.h>

#define MUTEX_T                 mutex_t
#define MUTEX_ALLOC(m)          m = mutex_alloc()
#define CONDITION_T             condition_t
#define CONDITION_ALLOC(c)      c = condition_alloc()
#define CONDITION_FREE(c)       condition_free(c)
#define THREAD_T                cthread_t
#define MUTEX_LOCK(m)           mutex_lock(m);
#define MUTEX_UNLOCK(m)         mutex_unlock(m);
#define CONDITION_WAIT(c)       condition_wait(c,isis_mutex);
#define CONDITION_SIGNAL(c)     condition_signal(c);
#define THREAD_SELF(t)          t = cthread_self()
#define THREAD_FORK(f,a,s)      cthread_detach(cthread_fork(f,a))
#endif				/* CTHREADS */

#ifdef  SUNLWP
#define THREADS                 1

#include <lwp/lwp.h>
#include <lwp/stackdep.h>

#ifdef  __GNUC__		/* GCC and CC use different struct-by-value passing convention */
#undef      mon_create
#define     mon_create(m)       panic("Called C-Library LWP routine from GCC compiled pr_task.c!");
#endif				/* __GNUC__ */

#define MUTEX_T                 mon_t
#define MUTEX_ALLOC(m)          mon_create(&m)
#define CONDITION_T             cv_t
#define CONDITION_ALLOC(c)      cv_create(&c,isis_mutex)
#define CONDITION_FREE(c)       cv_destroy(c)
#define THREAD_T                caddr_t
#define MUTEX_LOCK(m)           mon_enter(m)
#define MUTEX_UNLOCK(m)         mon_exit(m)
#define CONDITION_WAIT(c)       cv_wait(c)
#define CONDITION_SIGNAL(c)     cv_notify(c)
#define THREAD_SELF(s)          { lwp_self(&lwp_tid); s = lwp_tid.thread_id; }
#define THREAD_FORK(f,a,s)      lwp_create(&lwp_tid,f,MINPRIO,0,s,1,a)

thread_t lwp_tid;
#endif				/* SUNLWP */

#ifdef  APOLLO
#define THREADS                 1

#include <apollo/base.h>
#include <apollo/task.h>
#include <apollo/mutex.h>

#define DEF_STACKLEN            64*1024

#define MUTEX_T                 mutex_$lock_rec_t
#define THREAD_T                task_$handle_t
#define MUTEX_ALLOC(m)          mutex_$init(&m)
#define MUTEX_LOCK(m)           { mutex_$lock(&m, mutex_$wait_forever); pfm_$enable_faults(); }
#define MUTEX_UNLOCK(m)         mutex_$unlock(&m)
#define CONDITION_T             mutex_$lock_rec_t
#define CONDITION_ALLOC(c)      { MUTEX_ALLOC(c); MUTEX_LOCK(c); }
#define CONDITION_FREE(c)       MUTEX_UNLOCK(c)
#define CONDITION_WAIT(c)       { MUTEX_UNLOCK(isis_mutex); MUTEX_LOCK(c); MUTEX_LOCK(isis_mutex); }
#define CONDITION_SIGNAL(c)     { MUTEX_UNLOCK(c); }
#define THREAD_SELF(s)          s = task_$get_handle()
#define THREAD_FORK(f,a,s)      task_$create((task_$routine_p)f,(void*)a,0,DEF_STACKLEN,1,task_$forever,0,&acr_status)

status_$t acr_status;

#endif				/* APOLLO */

/* Task-related constants */
# define        words           *4	/* Bytes per word */
# define        kw              *4*1024	/* Bytes per k-word */

#ifndef THREADS
# define        TASKLEN         (4 kw)	/* size of task struct */
# define        STACKLEN        (TASKLEN-sizeof(task)-sizeof(int))
#else
#ifndef SUNLWP
# define        TASKLEN         (sizeof(task))	/* size of task struct */
# define        STACKLEN        0
#else
# define        TASKLEN         (4 kw)	/* size of task struct */
# define        STACKLEN        (TASKLEN-sizeof(task)-sizeof(int))
#endif				/* SUNLWP */
#endif				/* THREADS */

/* Signal disciplines */
# define        SD              0	/* Signal delayed */
# define        SU              1	/* Signal urgent */

struct task {
#ifndef THREADS
	jmp_buf task_env;		/* saved registers */
	int env_pad[16];		/* setjmp buf may lack room */
#endif					/* THREADS */
	int task_flag;			/* Flag for this task */
	int task_msgid;			/* RPC msg id number */
	int task_rval;			/* To return from t_wait */
	int (*task_routine) ();		/* Routine called */
	char *task_arg0;		/* Arg passed */
	address task_addr;		/* Address for this task */
	message *task_msg_arg;		/* Message arg to routine, if any */
	condition *task_queue;		/* Waiting on this qnode */
	bitvec task_watching;		/* Sites this task is watching */
	condition task_cond;		/* For waiting */
	condition task_iwait;		/* For waiting in iterated case */
	condition task_mwant;		/* Wants message */
	message *task_msg;		/* Special for indirect (iterated) addressing */
	qnode *task_msgs;		/* Message we got */
	qnode *task_active;		/* Node on active task qnode */
	char task_nwant;		/* Number wanted */
	char task_nreplies;		/* Number so far */
	char task_nullreps;		/* Null reps, failures observes so far */
	char task_nsent;		/* Number of messages sent */
	char task_nresp;		/* ... answers received */
	char *task_waitingfor;		/* Why I am waiting */
	char *task_done;		/* Used for pr_dump */
	address *task_dests;		/* Used for pr_dump */
#ifdef THREADS
	CONDITION_T task_runme;		/* Signal to run me... */
	THREAD_T task_threadid;		/* Thread's unique id */
	char *task_name;		/* Copy of cthread->name */
#endif					/* THREADS */
	char *task_stack;		/* stack area, normal case */
};

#define TASK_INACTIVE	0x0001	/* Not yet forked off */

/* Variables for managing the task table */
extern task *ctp;			/* Current task */
condition runqueue;			/* Runqueue */
condition tasks;			/* Task qnode */

#ifdef  THREADS
MUTEX_T isis_mutex;			/* Held when in ISIS */
#endif				/* THREADS */
