/*  $RCSfile: cl_task.h,v $ $Revision: 2.27 $ $Date: 90/07/26 17:27:55 $  */
/*
 *	Originally coded by Ken Birman
 *      Extended by Robert Cooper to support LISP
 *      task.h:  multitasking datastructures 
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

/*** Interface routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
void	isis_entry_stacksize(int entry, int size);
void	t_scheck	();
void	t_set_stacksize	(int size);
void	t_sig		(condition *cp, VOID *rval);
void	t_sig_all	(condition *cp, VOID *rval);
void	t_sig_urgent	(condition *cp, VOID *rval);
void	*t_yield	();
void 	*t_wait		(condition *cp);
void	*t_wait_l	(condition *cp, char *why);
#if ( __cplusplus || c_plusplus )
int	t_on_sys_stack	(i_routine_vs, VOID *arg);
#else
int	t_on_sys_stack	(int (*routine)(VOID *argp), VOID *arg);
#endif
#ifdef __cplusplus
}
#endif
#else

void	*t_yield();
void	*t_wait_();
void	*t_wait_l();
void	t_sig();
void    t_sig_all();

#endif

/*** Internal routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
task	*do_task_dequeue(condition *cp);
void	isis_accept_events_loop	(int flag);
void	spanic		();
void	t_init		();
void	task_swtch	(task *tp, void (*routine)(VOID *arg), VOID *arg0);
int	thread_isis_cleanup();
void	thread_isis_enter(condition *cp, char *why);
void	thread_isis_exit();

#if ( __cplusplus || c_plusplus ) 
task	*isis_fork	(v_routine_vs, VOID *arg0, message *mp);
void	isis_fork_urgent(v_routine_vs, VOID *arg0, message *mp);
#else
task	*isis_fork	(void (*routine)(VOID *arg), VOID *arg0, message *mp);
void	isis_fork_urgent(void (*routine)(VOID *arg), VOID *arg0, message *mp);
#endif
#ifdef __cplusplus
}
#endif
#else

task                   *do_task_dequeue();
void     isis_accept_events_loop(), task_swtch();
void	 spanic(), t_set_stacksize();
void	thread_isis_enter();
void	thread_isis_exit();
#endif

#ifdef  DB_TASKS
# define        db_print(str, val)        print(str, val)
#else
# define        db_print(str, val)
#endif

/* Task-related constants */
# define        words           *4              /* Bytes per word */
# define        kw              *4*1024         /* Bytes per k-word */
#ifndef SUN4
# define        DEF_TASKLEN         (4 kw)          /* size of task struct */
#else
# define        DEF_TASKLEN         (6 kw)          /* size of task struct */
#endif

/* Signal disciplines */
# define        SD              0               /* Signal delayed */
# define        SU              1               /* Signal urgent */

#include <setjmp.h>

/* Thread-dependent tasking macros.
*  There is only one isis mutual exclusion lock "isis_mutex" held by the 
*  single thread which is currently executing inside ISIS. 
* MUTEX_T          Type of a mutual exclusion lock.
* MUTEX_ALLOC(m)   Create a lock in variable "m" of type MUTEX_T
* MUTEX_LOCK(m)    Lock the mutex "m"
* MUTEX_UNLOCK(m)  Unlock the mutex "m"
*                  There is only one ISIS mutual exclusion lock, called
*                  "isis_mutex", held by the single thread which is
*                  currently executing inside ISIS. This fact is used by 
*                  some of the task implementations.
*
* THREAD_YIELD(m) Releases isis_mutex and must guarantee other threads a fair
*                 chance to run. Usually just doing an unlock and lock is OK,
*                 but some thread implementations are unfair.
* SELECT(b,i,o,e,t) Same arguments and meaning as the Unix select(2) call.
*                 Some task systems need to redefine this to work properly.
*
* THREAD_T        Unique task identifier.
* THREAD_SELF(t)  Assigns the THREAD_T value for the currently executing
*                 thread to variable t.
* Each ISIS task object has a field called task_threadid of type 
* THREAD_T for storing a thread identifier. The value returned 
* from THREAD_SELF will be assigned to this field. 
* Subsequent calls to THREAD_SELF by this thread/task must 
* compare == to this field.
* THREAD_SELF could be called before or after THREAD_ALLOC for a given
* thread (i.e. the THREAD_SELF value must be created before the THREAD_ALLOC
* call.
*
* THREAD_FORK(f,a,s) THREAD_FORK creates an new thread with initial function
*                    f, with argument a, and stack s (only needed for some
*                    thread implementations). The new thread should be
*                    immediately runnable.
*
* Each thread is individually made runnable and suspended by the ISIS
* scheduler with THREAD_SIGNAL and THREAD_WAIT. The task_runme field
* of an ISIS task object is of type THREAD_COND_T. This is typically
* a private per-thread condition variable upon which a task waits when
* suspended. 
* THREAD_COND_T      Type of task_runme field of an ISIS task structure.
* THREAD_ALLOC(tp)   Creates a private condition variable for the ISIS task
*                    structure pointed to by tp. Typically this condition
*                    variable is of type THREAD_COND_T and is assigned to the 
*                    task_runme field, but other implementations are possible.
* THREAD_WAIT(tp)    Cause the task structre pointed by tp to suspend itself.
* THREAD_SIGNAL(tp)  Make the task pointed to by tp runnable.
* THREAD_FREE(tp)    Deallocate thread-specific resources, typically including
*                    the task_runme field.
* ISISCALL1(rtn, arg)    Calls user callback routine "rtn", with 1, 2 or 3 args.
* ISISCALL2(rtn, arg1, arg2)
* ISISCALL3(rtn, arg1, arg2, arg3)
*/

/* Default select implementation. */
#define SELECT(b,i,o,e,t)       select(b,i,o,e,t)

#define ISIS_BGCALL		if(1) BEGINFROMC
#define ISIS_ECALL			ENDFROMC else

/* Default CALL implementation */
#define ISISCALL0(rtn)                         ISIS_BGCALL ((*rtn)()); ISIS_ECALL
#define ISISCALL1(rtn, arg)                    ISIS_BGCALL ((*rtn)(arg)); ISIS_ECALL
#define ISISCALL2(rtn, arg1, arg2)             ISIS_BGCALL ((*rtn)(arg1, arg2)); ISIS_ECALL
#define ISISCALL3(rtn, arg1, arg2, arg3)       ISIS_BGCALL ((*rtn)(arg1, arg2, arg3)); ISIS_ECALL
#define ISISCALL4(rtn, arg1, arg2, arg3, arg4) ISIS_BGCALL ((*rtn)(arg1, arg2, arg3, arg4)); ISIS_ECALL

#if    FUN_TYPES
int    _ISISCALL1( ifunc *rtn, VOID *arg);
#endif FUN_TYPES



/*** MACH C Threads implementation. ***/
#ifdef	CTHREADS
#define THREADS			1

#include <cthreads.h>

#define	MUTEX_T		        mutex_t
#define MUTEX_ALLOC(m)	        m = mutex_alloc()
#define MUTEX_LOCK(m)           mutex_lock(m)
#define MUTEX_UNLOCK(m)	        mutex_unlock(m)
#define THREAD_YIELD(m)         { MUTEX_UNLOCK(m); cthread_yield(); MUTEX_LOCK(m); }
#define THREAD_T                cthread_t
#define THREAD_SELF(t)          t = cthread_self()
#define THREAD_FORK(f,a,s)      cthread_detach(cthread_fork(f,a))
#define THREAD_COND_T		condition_t
#define THREAD_ALLOC(tp)	{ (tp)-> task_runme = condition_alloc(); db_print("THREAD_ALLOC for tp %x\n", tp); }
#define THREAD_WAIT(tp)  	{ db_print("THREAD_WAIT will release MUTEX (task %x)\n", tp); condition_wait((tp)-> task_runme,isis_mutex); db_print("THREAD_WAIT has re-acquired MUTEX (task %x)\n", isis_ctp); }
#define THREAD_SIGNAL(tp)     	{ db_print("CONDITION_SIGNAL thread %x\n", tp); condition_signal((tp)-> task_runme); }
#define THREAD_FREE(tp)		condition_free((tp)-> task_runme)
#endif CTHREADS

/*** Sun Lightweight Processes implementation. ***/
#ifdef  SUNLWP 
#define THREADS                 1 
 
#include <lwp/lwp.h>
#include <lwp/stackdep.h>

#if __GNUC__ && SUN4
/* On a SUN4 8-byte structures can't be passed by value between gcc and Sun C.
   So we use some wrapper functions. */
int _mon_enter(mon_t *mid);
int _mon_exit(mon_t *mid);
int _cv_wait(cv_t *cv);
int _cv_notify(cv_t *cv);
int _cv_destroy(cv_t *cv);
#define mon_enter(mid) _mon_enter(&(mid)) 
#define mon_exit(mid)  _mon_exit(&(mid)) 
#define cv_wait(cv) _cv_wait(&(cv))
#define cv_notify(cv) _cv_notify(&(cv))
#define cv_destroy(cv) _cv_destroy(&(cv))
/* Those are the only wrappers we need, but the following functions would
   need wrappers if we used them: mon_break, mon_cond_enter, mon_waiters,
   mon_destroy, lwp_destroy, lwp_yield, lwp_setpri, lwp_suspend, lwp_resume,
   lwp_join, lwp_ping, lwp_getregs, lwp_setregs, lwp_getstate, cv_send,
   cv_broadcast, cv_waiters
*/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
int lwp_self(thread_t *tid);
int lwp_create();
#ifdef __cplusplus
}
#endif
#else
int lwp_self();
int lwp_create();
#  endif
#endif

#define MUTEX_T                 mon_t
#define MUTEX_ALLOC(m)          mon_create(&m)
#define MUTEX_LOCK(m)           mon_enter(m)
#define MUTEX_UNLOCK(m)         mon_exit(m)
#define THREAD_YIELD(m)         { MUTEX_UNLOCK(m); MUTEX_LOCK(m); }

#define THREAD_T                caddr_t
thread_t  lwp_tid;              /* lwp IDs are big so need this variable. */
#define THREAD_SELF(s)          { lwp_self(&lwp_tid); s = lwp_tid.thread_id; }
#define THREAD_FORK(f,a,s)      lwp_create(&lwp_tid,f,MINPRIO,0,s,1,a)

#define THREAD_COND_T           cv_t
#define THREAD_ALLOC(tp)        cv_create(&((tp)-> task_runme), isis_mutex)
#define THREAD_WAIT(tp)         cv_wait((tp)-> task_runme)
#define THREAD_SIGNAL(tp)       cv_notify((tp)-> task_runme)
#define THREAD_FREE(tp)         cv_destroy((tp)-> task_runme)

#endif SUNLWP   

/*** Apollo task implementation. ***/
#ifdef  APOLLO 
#define THREADS                 1 
 
#include <apollo/base.h>
#include <apollo/task.h>
#include <apollo/mutex.h>

#define DEF_STACKLEN            64*1024

#define MUTEX_T                 mutex_$lock_rec_t
#define MUTEX_ALLOC(m)          mutex_$init(&(m))
#define MUTEX_LOCK(m)           { mutex_$lock(&(m), mutex_$wait_forever);  \
                                  pfm_$enable_faults();  }
#define MUTEX_UNLOCK(m)         mutex_$unlock(&(m))
#define THREAD_YIELD(m)         { MUTEX_UNLOCK(m); MUTEX_LOCK(m); }

#define THREAD_T                task_$handle_t
#define THREAD_SELF(s)          s = task_$get_handle()
#define THREAD_FORK(f,a,s)      task_$create((task_$routine_p)f,(void*)a,0,DEF_STACKLEN,1,task_$forever,0,&acr_status)
 
#define THREAD_COND_T           mutex_$lock_rec_t
#define THREAD_ALLOC(tp)        { MUTEX_ALLOC((tp)->task_runme);     \
                                  MUTEX_LOCK((tp)-> task_runme); }
#define THREAD_FREE(tp)         MUTEX_UNLOCK((tp)-> task_runme)
#define THREAD_WAIT(tp)         { MUTEX_UNLOCK(isis_mutex);         \
                                  MUTEX_LOCK((tp)->task_runme);     \
                                  MUTEX_LOCK(isis_mutex); }
#define THREAD_SIGNAL(tp)    { MUTEX_UNLOCK(tp-> task_runme); }

status_$t acr_status;

#endif APOLLO   

/*** Allegro Common Lisp task defines. ***/
#ifdef ALLEGRO_CL
#include <lisp.h>
#define THREADS                 1 

/* Struct of pointers to C-callable lisp functions. */
typedef struct {
    void (*mutex_lock)();
    void (*mutex_unlock)();
    int  (*thread_self)();
    void (*thread_wait)();
    void (*thread_signal)();
    void (*thread_fork)();
    void (*thread_free)();
    void (*thread_cleanup)();
    void (*thread_wait_fun)();
    void (*thread_yield)();
    void (*panic)();
} clisp_funs;

clisp_funs *cl;

/* Since there's only one mutex lock we'll avoid passing it to/from lisp. */
#define MUTEX_T           long  /* Never used */
#define MUTEX_ALLOC(m)  
#define MUTEX_LOCK(m) 	  call_allegro(cl-> mutex_lock, 0)
#define MUTEX_UNLOCK(m)   call_allegro(cl-> mutex_unlock, 0)

#define THREAD_COND_T	  long
#define THREAD_ALLOC(tp)  (tp)-> task_runme = -1; /* Dummy value. */
#define THREAD_FREE(tp)   call_allegro(cl-> thread_free, (tp)-> task_threadid)
#define THREAD_WAIT(tp)   call_allegro(cl-> thread_wait, (tp)-> task_threadid)
#define THREAD_SIGNAL(tp) call_allegro(cl-> thread_signal, (tp)-> task_threadid)
#define THREAD_YIELD(m)   { MUTEX_UNLOCK(m);                 \
                            call_allegro(cl-> thread_yield, 0); \
                            MUTEX_LOCK(m);                   \
                          }
#define THREAD_T          long
#define THREAD_SELF(t)    t = FixnumToInt(call_allegro(cl-> thread_self, 0))
/* "invoke" is always the initial routine of a task, so we can avoid
   worrying about passing unknown routine pointers to lisp (which is a hassle).
*/
#define THREAD_FORK(f,a,s) call_allegro(cl-> thread_fork, a)
/* #define THREAD_FORK(f,a,s)                          \
        if ((f) == invoke) {                           \
          call_allegro(cl-> thread_fork, a);               \
        } else panic("THREAD_FORK not forking invoke") 
*/

# undef SELECT
#define SELECT(b,i,o,e,t)  lisp_select(b,i,o,e,t)
       
/* Possible future optimizations:
   Actually we need only claim the mutex lock when leaving the C 
   when upcalling back into lisp. All C-code is executing without timeslicing
   in Allegro CL. Thus thread_isis_enter and _exit need not grab the mutex
   since such TEMP_TASKs never do upcalls back into lisp (these are always
   forked). Only tasks created in the ISIS/C world need to grab the mutex.
   However upon any process switch a test must be made for TASK_TEMP and
   the mutex not unlocked in such cases.
*/
#endif ALLEGRO_CL

/*** Lucid CL task defines ***/
#ifdef LUCID_CL
#define THREADS                 1 

/* C-callable functions in Lucid. */
void cl_mutex_lock();
void cl_mutex_unlock();
int  cl_thread_self();
void cl_thread_wait();
void cl_thread_signal();
void cl_thread_fork();
void cl_thread_free();
void cl_thread_cleanup();
void cl_thread_wait_fun();
void cl_thread_yield();
void cl_panic();
int  call_lucid();

/* Since there's only one mutex lock we'll avoid passing it to/from lisp. */
#define MUTEX_T              long  /* Never used */
#define MUTEX_ALLOC(m)  	
#define MUTEX_LOCK(m) 	     cl_mutex_lock();
#define MUTEX_UNLOCK(m)      cl_mutex_unlock();

#define THREAD_COND_T	     long
#define THREAD_ALLOC(tp)     (tp)-> task_runme = -1; /* Dummy value. */
#define THREAD_FREE(tp)      cl_thread_free((tp)-> task_threadid)
#define THREAD_WAIT(tp)      cl_thread_wait((tp)-> task_threadid)
#define THREAD_SIGNAL(tp)    cl_thread_signal((tp)-> task_threadid)
#define THREAD_YIELD(m)      { MUTEX_UNLOCK(m);                 \
                               cl_thread_yield();               \
                               MUTEX_LOCK(m);                   \
                             }
#define THREAD_T             long
#define THREAD_SELF(t)       t = cl_thread_self()
/* "invoke" is always the initial routine of a task, so we can avoid
   worrying about passing unknown routine pointers to lisp (which is a hassle).
*/
#define THREAD_FORK(f,a,s)   cl_thread_fork(a)
/* #define THREAD_FORK(f,a,s)                          \
        if ((f) == invoke) {                           \
          cl_thread_fork(a);                           \
        } else panic("THREAD_FORK not forking invoke") 
*/

# undef SELECT
#define SELECT(b,i,o,e,t)  lisp_select(b,i,o,e,t)

/* Lucid callbacks are represented as odd numbers that Lucid maps
   to actual lisp functions. We currently support Lucid on 680x0s and
   Sparc neither of which allows odd routine start addresses.
   An alternative way of doing this is to add a bit similar to XBY_REF
   to the ISIS_STATE variable, and to all callback structures (like wnode)
   to record. Then pass this to CALL and test it. This would avoid any
   dependency on routine pointer formats.
*/
#undef ISISCALL0
#undef ISISCALL1
#undef ISISCALL2
#undef ISISCALL3
#undef ISISCALL4
#define ISISCALL0(rtn)                  \
    (((int)rtn&1 == 1) ? call_lucid(rtn, 0, 0, 0, 0, 0)  : (*(ifunc *)rtn)())
#define ISISCALL1(rtn, a1)             \
    (((int)rtn&1 == 1) ? call_lucid(rtn, 1, a1,0, 0, 0)  : (*(ifunc *)rtn)(a1))
#define ISISCALL2(rtn, a1, a2)         \
    (((int)rtn&1 == 1) ? call_lucid(rtn, 2, a1,a2,0, 0)  : (*(ifunc *)rtn)(a1,a2))
#define ISISCALL3(rtn, a1, a2, a3)     \
    (((int)rtn&1 == 1) ? call_lucid(rtn, 3, a1,a2,a3,0)  : (*(ifunc *)rtn)(a1,a2,a3))
#define ISISCALL4(rtn, a1, a2, a3, a4) \
    (((int)rtn&1 == 1) ? call_lucid(rtn, 4, a1,a2,a3,a4) : (*(ifunc *)rtn)(a1,a2,a3,a4))
#endif LUCID_CL

struct  event_id
{
        int            e_msgid;                        /* Event corresponds to this message  */
        int            e_op;                           /* Operation name */
        address        e_pname;                        /* Process */
};

struct  task
{
        jmp_buf         task_env;                       /* Saved registers */
        int             env_pad[16];                    /* Some jmp_bufs are too small */
        event_id        task_eid;                       /* Current event id */
        int             task_msgid;                     /* RPC msgid number */
        int             task_ccmsgid;                   /* msgid number for cc_terminate */
        VOID            *task_rval;                     /* To return from t_wait */
        vfunc           *task_routine;                  /* Routine called */
        int             task_sleep;                     /* How long? */
        int             task_entry;                     /* Entry number, if any */
	int		task_entries;			/* Counts calls to ISIS_ENTER */
        short           task_act;                       /* Activity id for this task, if any */
        short           task_flag;                      /* flags */
        char            *task_arg0;                     /* Arg passed */
        address         task_addr;                      /* Address of this task */
        message         *task_msg_arg;                  /* Message arg to routine, if any */
        condition       *task_queue;                    /* Waiting on this qnode */
        condition       task_cond;                      /* For waiting */
        char            *task_waitingfor;               /* Why waiting */
        condition       task_mwant;                     /* Wants message */
        message         *task_msg;                      /* Special for indirect (iterated) addressing */
        qnode           *task_msgs;                     /* Message we got */
        qnode           *task_active;                   /* Node on active task qnode */
        address         *task_cohorts;                  /* Cohort list, if in cc alg. */
        address         task_truesender;                /* Also for cc alg */
        address         task_sentto;                    /* Who I sent the last message to */
        char            task_nwant;                     /* Number wanted */
        char            task_nreplies;                  /* Number so far */
        char            task_nullreps;                  /* Null reps, failures observed so far */
        char            task_nsent;                     /* Number of messages sent */
        char            task_nresp;                     /* ... answers received */
        char            *task_done;                     /* Used for cl_dump */
        address         *task_dests;                    /* Used for cl_dump */
#ifdef THREADS
        THREAD_COND_T   task_runme;                     /* Signal to run me... */
        THREAD_T        task_threadid;                  /* Thread's unique id */
	char            *task_name;                     /* Copy of cthread->name */
#endif THREADS
        char            *task_end;                      /* stack area, normal case */
        char            *task_stack;                    /* stack area, normal case */
};

/* Field names for qnode nodes */
#define TA_ISTASK               1

/* Variables for managing the task table */
extern task            *isis_ctp;                      /* Current task */
extern condition       isis_runqueue;                  /* Runqueue */
extern condition       isis_tasks;                     /* Task qnode */
extern condition       task_decongested;
extern int             task_congest;
extern int             isis_ntasks;                    /* Counts active tasks */
#define                my_eid                          isis_ctp->task_eid

/* Macros for fork routines */
# define t_fork(routine, arg)           isis_fork(routine, arg, NULLMP)
# define t_fork_urgent(routine, arg)    isis_fork_urgent(routine, arg, NULLMP)
# define t_fork_msg(routine, mp)        isis_fork(routine, mp, mp)
# define t_fork_urgent_msg(routine, mp) isis_fork_urgent(routine, mp, mp)

# define t_waiting(condp)               ((*(condp)) != 0)

#define TASK_START      0x01                            /* Set for the startup task */
#define TASK_LOGGED     0x02                            /* Set if logged */
#define TASK_INHIBIT    0x04                            /* Set if inhibits joins */
#define TASK_CONGESTED  0x08                            /* Congested and waiting */
#define TASK_XBYREF     0x10                            /* Current caller used pass-by-ref */
#define TASK_ABYREF     0x20                            /* Called used address passing by ref */
#define TASK_TEMP       0x40                            /* Just visiting */
#define TASK_PRNAME     0x80                            /* Printable routine name */
#define TASK_ZOMBIE     0x100				/* A member of the walking dead */

#define task_dequeue(cp)  ((*cp == 0)? (task*)-1: do_task_dequeue(cp))

#define ISIS_MONITOR_ENTER(count,cond)  if(1) { if(count) (void)t_wait_l(&cond, "ISIS_MONITOR_ENTER"); ++count; } else
#define ISIS_MONITOR_EXIT(count,cond)   if(1) { if(--count == 0) t_sig(&cond, 0); } else

#ifdef  THREADS
MUTEX_T isis_mutex;   /* Define the single ISIS mutex variable. */

# define        ISIS_ENTER()	ISIS_ENTER_L(&isis_runqueue,0)
# define        ISIS_ENTER_L(cp,why) if(1) {                            \
                                    THREAD_T self;                      \
                                    THREAD_SELF(self);                  \
                                    if(isis_ctp->task_threadid != self) \
                                        thread_isis_enter(cp, why);     \
                                    else ++isis_ctp->task_entries;      \
                                } else 
# define        ISIS_EXIT()	if(1) {                                 \
                                    if(isis_ctp->task_flag&TASK_TEMP)   \
                                        thread_isis_exit();             \
                                } else
# define        ISIS_RETURN(x)	if(1) {                                 \
                                    if(isis_ctp->task_flag&TASK_TEMP)   \
                                            thread_isis_exit();         \
                                    return(x);                          \
                                } else

# define        THREAD_LEAVE_ISIS()   thread_isis_exit()
# define        THREAD_ENTER_ISIS()   ISIS_ENTER()

#else    THREADS

# define	ISIS_ENTER()
# define	ISIS_ENTER_L(a,b)
# define 	ISIS_EXIT()
# define 	ISIS_RETURN(x)    return(x)

#endif   THREADS
