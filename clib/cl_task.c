/*  $RCSfile: cl_task.c,v $ $Revision: 2.107 $ $Date: 90/09/13 22:40:02 $  */
/*
 *      Task.c -- code related to task management
 *
 *	Originally coded by Ken Birman
 *      Modified by Robert Cooper to support Allegro tasks
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
 */

# include <string.h>

# include "isis.h"

#define		ISIS_TASKHI	32		  /* A lot of tasks */
#define		ISIS_TASKLO	16		  /* A more comfortable level */

int                    isis_ntasks;               /* Counts active tasks */
int                    task_congest;              /* Flags task congestion */
condition              task_decongested;
bitvec                 inhibit_entries;           /* Entries for which joinsd should be inhibited */
condition              isis_runqueue;             /* Runqueue */
condition              isis_tasks;                /* Task qnode */

static task isis_main_task;                       /* Task descriptor for main task */      

#ifdef  THREADS
#  define  DUMMYTASK     &isis_main_task          /* used as a dummy in THREADS case */
#else
#  define  DUMMYTASK     (task*)0                 /* No special interp otherwise */
#endif

task    *isis_scheduler;
task    *isis_ctp = &isis_main_task;              /* Scheduler is running initially */

/* On some machine setjmp and longjmp work fine for ISIS */
#ifndef THREADS
#    if     (AIXRS|GOULD|hp9000s800|NEXT|MIPS|RT43|SGI|CONVEX)
#        define    isis_setjmp      _setjmp
#        define    isis_longjmp     _longjmp
#    endif  (AIXRS|GOULD|hp9000s800|NEXT|MIPS|RT43|SGI|CONVEX)
#    if     (AIX)
#        define    isis_setjmp      setjmp
#        define    isis_longjmp     longjmp
#    endif
#    if     (SUN4)
#        include   "jmp_pragmas.h"
#    endif
#else  THREADS
#    ifndef ALLEGRO_CL
#     define   isis_setjmp(x)    0
#     define   isis_longjmp(x)   panic("isis_longjmp shouldn't have been called")
#    endif ALLEGRO_CL
#endif THREADS


static  message *task_msg_arg;
int     TASKLEN;
address NULLADDRESS;
static  entry_stacksize[128];
#define	esize(n)	(n >= 0 && n <= 127 && entry_stacksize[n] > 0)
void invoke();
         
static adesc   st_ad ={ 0, 0, 4};

# define        st_alloc()      ((task*)mallocate(&st_ad))
# define        st_free(x)      mdeallocate((char*)x, &st_ad)




#if ALLEGRO_CL || LUCID_CL

t_final()
  /* Called to deinitialize the ISIS task world when unwinding in lisp. */
  {
#if ALLEGRO_CL 
        call_allegro(cl-> thread_cleanup, 0);
#endif
#if LUCID_CL
        cl_thread_cleanup();
#endif
        isis_state &= ~ISIS_TINIT;
        /* Probably lots of other things to do...
           including killing(?) all ISIS tasks, deleting messages etc. */
  }
#endif ALLEGRO_CL || LUCID_CL


#ifdef ALLEGRO_CL

int cl_threads_inited = FALSE;

/* Called by lisp when it initializes ISIS */
void 
isis_init_lisp_funs (funs)
  clisp_funs *funs;
  {
        cl = funs;
        cl_threads_inited = TRUE;    
  }

call_allegro(routine, arg)
  ifunc *routine;
  int arg;
  /* Hack to get around Allegro register trashing bug. Its slow but
  *  it should be fixed soon.
  */
  {
        struct {
            jmp_buf         task_env;    /* Saved registers */
            int             env_pad[16]; /* Some jmp_bufs are too small */
        } regs;
        int rv;

        if(isis_setjmp(regs.task_env) == 0)
        {
            rv = (*routine)(arg);
            isis_longjmp(regs.task_env, 1);
        }
        return(rv);
  }


#endif /* ALLEGRO_CL */

int
_ISISCALL1(func, arg)
  ifunc *func;
  VOID *arg;
  {
	register rv;
	BEGINFROMC;
	    rv = (*func)(arg);
	ENDFROMC;
	return rv;
  }

void
t_init()
  {
	if(isis_state&ISIS_TINIT)
	    return;
	isis_state |= ISIS_TINIT;
	if(isis_tasks == NULLQP)
	    isis_tasks = qu_null();
#ifdef THREADS
        THREAD_SELF(isis_main_task.task_threadid);
	/* Make sure it differs from thread of mainstack */
        --isis_main_task.task_threadid;
#ifdef ALLEGRO_CL
        if (!cl_threads_inited) 
        {
            panic("Allegro Common Lisp did not initialize ISIS properly");
        }
#endif ALLEGRO_CL       
	db_print("initial allocation of isis_mutex in t_init\n", 0);
	MUTEX_ALLOC(isis_mutex);
#else
	isis_main_task.task_active = qu_add_tp(isis_tasks, TA_ISTASK, &isis_main_task);
#endif THREADS
        if(TASKLEN == 0)
            t_set_stacksize(DEF_TASKLEN); 
  }
 
#define task_enqueue(cp, tp, discipline)                        \
  {                                                             \
        if(tp)                                                  \
        {                                                       \
            register qnode *qp;                                 \
            if((qp = *cp) == 0)                                 \
            {                                                   \
                qu_tnull(qp);                                   \
                *cp = qp;                                       \
            }                                                   \
            if(discipline == SU)                                \
                qp = qp->qu_next;                               \
            qu_add_tp(qp, TA_ISTASK, tp);                       \
            tp->task_queue = cp;                                \
        }                                                       \
  }

task *
do_task_dequeue(cp)
  register condition *cp;
  {
        register qnode *qp, *np;
        register task *tp;
        qp = *cp;
        np = qu_head(qp);
        tp = np->qu_task;
        tp->task_queue = 0;
        qu_free(np);
        if(qp->qu_next == qp)
        {
            qu_free(qp);
            *cp = 0;
        }
        return(tp);
  }

#if(THREADS)
static	short use_lwpstk;
#endif

void
t_set_stacksize(size) 
  register size;
  { 
#       if(THREADS)
#           if(SUNLWP)
                if(size == -1)
		{
		    ++use_lwpstk;
		    return;
		}
                size += sizeof(task);
#           else
                size = sizeof(task);
#           endif
#       else
            size += sizeof(task);
#       endif
        if(TASKLEN > size)
            return;
        st_ad.a_isize = TASKLEN = size; 
  }

void
isis_entry_stacksize(entry, size)
  int entry, size;  
  {
#ifndef	THREADS
        entry_stacksize[entry] = size+sizeof(task);
#else
#ifdef  SUNLWP
        if(size != -1)
	    size += sizeof(task);
        entry_stacksize[entry] = size;
#endif
#endif	THREADS
  }

void
t_scheck()
  {
#ifndef THREADS
        char stackp;
        if(&stackp <= isis_ctp->task_stack && isis_ctp != isis_scheduler)
            spanic();
#endif THREADS
  }

void
spanic()
  {
        print("ISIS stack overflow for task ");
        cl_rname(isis_ctp->task_routine);
        print("(%x) qu_freelist %x\n", isis_ctp->task_arg0, qu_freelist);
        panic("ISIS stack overflow");
  }

#ifndef THREADS
#define T_scheck()                                                      \
  {                                                                     \
        char stackp;                                                    \
        if(&stackp <= isis_ctp->task_stack && isis_ctp != isis_scheduler)\
            spanic();                                                   \
  }
#else
#define T_scheck()
#endif THREADS

void
isis_fork_urgent(routine, arg0, mp)
  vfunc *routine;
  VOID *arg0;
  message *mp;
  {
        ISIS_ENTER();
        if(task_msg_arg = mp)
            msg_increfcount(mp);
        task_enqueue(&isis_runqueue, isis_ctp, SU);
        task_swtch(NULLTASK, routine, arg0);
        ISIS_EXIT();
  }

static  task *delayed_task, *new_task;

task *
isis_fork(routine, arg0, mp)
  vfunc *routine;
  VOID *arg0;
  message *mp;
  {
	ISIS_ENTER();
        if(task_msg_arg = mp)
            msg_increfcount(mp);
        delayed_task = isis_ctp;
        task_swtch(NULLTASK, routine, arg0);
        ISIS_RETURN(new_task);
  }

static  char *stackp;
static  ifunc *call_on_sys_stack;
static  char *call_arg;
static  condition call_wait;

int
t_on_sys_stack(routine, arg)
  ifunc *routine;
  VOID *arg;
  {
	extern isis_wakeup_sync;
        register res;
	ISIS_ENTER();
        if(isis_ctp == isis_scheduler)
            ISIS_RETURN( _ISISCALL1(routine, arg) );
        call_on_sys_stack = routine;
        call_arg = arg;
        res = (int) t_wait_l(&call_wait, "call_on_sys_stack");
	isis_wakeup_sync = 1;
        ISIS_RETURN(res);
  }

static task *free_tp = 0, *old_tp;

void
isis_accept_events_loop(flag)
  int flag;
  { 
	if(isis_scheduler != isis_ctp)
	    abort();
        while(isis_state&ISIS_INIT)
            run_isis(flag);
  } 

/* HP 9000/300 doesn't like optimizing asm statements. */
#ifdef          hp9000s300
#    include    "OPT_LEVEL_1.h"
#endif

void
task_swtch(tp, routine, arg0)
  register task *tp;
  register vfunc *routine;
  register VOID *arg0;
  {
        T_scheck();
#ifdef DB_TASKS
	print("task_switch task_swtch %x [ %x(0x%x) ]\n", tp, routine, arg0);
#endif
        if(tp == (task*)-1 && (tp = isis_scheduler) == (task*)0)
        {
            routine = (vfunc*)isis_accept_events_loop;
	    arg0 = (char*)ISIS_BLOCK;
	}
        if(tp == isis_ctp)
            return;
        if(isis_state&ISIS_XBYREF)
            isis_ctp->task_flag |= TASK_XBYREF;
        else
            isis_ctp->task_flag &= ~TASK_XBYREF;
        old_tp = isis_ctp;

#ifdef  THREADS
        if(tp)
        {
	   isis_ctp = tp;
	   THREAD_SIGNAL(tp);
        }
#else  	THREADS
        if(isis_setjmp(isis_ctp->task_env))
            goto out;
#endif  THREADS

        if(tp == 0)
        {
	    register STACKLEN;
            if((isis_state&ISIS_RECEIVING) && esize(isis_enum))
                tp = (task*)malloc(STACKLEN = entry_stacksize[isis_enum]);
            else
	    {
                tp = st_alloc();
                STACKLEN = TASKLEN;
	    }
	    db_print("task_switch using new tp %x\n", tp);
	    if((vfunc*)routine == (vfunc*)isis_accept_events_loop)
		isis_scheduler = tp;
	    STACKLEN -= sizeof(task) + (1 words); 
            ++isis_created;
            if(++isis_ntasks == ISIS_TASKHI && !task_congest)
	  	++task_congest;
            tp->task_queue = 0;
            tp->task_msgid = 0;
            tp->task_cond = 0;
            tp->task_waitingfor = 0;
            tp->task_mwant = 0;
            tp->task_msg = 0;
	    tp->task_dests = 0;
            tp->task_sleep = 0;
            tp->task_flag = isis_ctp->task_flag&TASK_XBYREF;
	    tp->task_entries = 0;
            tp->task_act = isis_ctp->task_act;
            tp->task_active = qu_add_tp(isis_tasks, TA_ISTASK, tp);
            tp->task_routine = routine;
            tp->task_arg0 = arg0;
            tp->task_addr = my_address;
	    tp->task_entry = isis_enum;
	    tp->task_addr.addr_entry = isis_enum;
	    isis_enum = 0;
            tp->task_msg_arg = task_msg_arg;
            task_msg_arg = 0;
            tp->task_cohorts = (address*)0;
            tp->task_stack = ((char*)(&tp->task_stack))+sizeof(char*);
	    tp->task_end = (char*)&tp->task_stack[STACKLEN];
#ifdef      SUNLWP
            {
		register n = tp->task_entry;
                if(use_lwpstk || (n >= 0 && n <= 127 && entry_stacksize[n] == -1))
                    tp->task_end = tp->task_stack = (char*)lwp_newstk();
	    }
#endif      SUNLWP
	    isis_ctp = tp;
#ifdef  THREADS
	    THREAD_ALLOC(tp);
	    THREAD_FORK(invoke, tp, tp->task_end);
#else   THREADS
            /* Change stack pointers.  Alignment is important for many machines */ 
#           if(VAX)
            {
                stackp = &tp->task_stack[STACKLEN-(2 words)];
                stackp = (char*)((int)stackp & ~0x3);
                asm("movl  _stackp,sp");
            }   
#           endif
#           if(GOULD)
            {
                stackp = &tp->task_stack[STACKLEN-(24 words)];
                stackp = (char*)((int)stackp & ~0x7);
                asm("movw  _stackp,b2");
            }   
#           endif
#           if(SUN3)
            {
                stackp = &tp->task_stack[STACKLEN-(2 words)];
                stackp = (char*)((int)stackp & ~0x3);
                asm("movl  _stackp,sp");
            }   
#           endif
#           if(AUX)
            {
                stackp = &tp->task_stack[STACKLEN-(2 words)];
                stackp = (char*)((int)stackp & ~0x3);
                asm("mov.l  stackp,%sp");
            }
#           endif
#           if(NEXT)
            {
                stackp = &tp->task_stack[STACKLEN-(16 words)];
                stackp = (char*)((int)stackp & ~0x3);
                asm("movel  _stackp,sp");
            }   
#           endif
#           if(SUN4)
            {
#               include <sun4/asm_linkage.h>
                stackp = &tp->task_stack[STACKLEN-(32 words)];
                stackp = (char*)((int)stackp & ~(STACK_ALIGN-1));
                asm("t       0x3");   /* T_FLUSH_WINDOW */
                asm("sethi   %hi(_stackp),%o0");
                asm("ld      [%o0+%lo(_stackp)],%sp");
                asm("clr     %fp");
            }   
#           endif
#           if(HPUX)
            {
#ifdef          hp9000s300
                    stackp = &tp->task_stack[STACKLEN-(2 words)];
                    stackp = (char*)((int)stackp & ~0x3);
                    asm("mov.l  _stackp,%sp");
#endif          hp9000s300
#ifdef          hp9000s800
                    stackp = &tp->task_stack[0];
                    stackp = (char*)((((int)stackp+7) & ~0x7)+ 48);
                    set_sp(stackp);
#endif          hp9000s800
            }
#           endif
#           if(AIX|MIPS|RT43|SGI)
            {
                jmp_buf env;
                int *envp;

                if (isis_setjmp(env) == 0) {
                    stackp = &tp->task_stack[STACKLEN-(2 words)];
                    stackp = (char*)((int)stackp & ~0x7);
                    envp = (int *)env;
                    envp[JB_SP] = (int)stackp;
                    isis_longjmp(env, 1);
                }
            }    
#           endif
#           if(AIXRS)
            {
                jmp_buf env;
                int *envp;
                if (isis_setjmp(env) == 0) {
                    stackp = &tp->task_stack[STACKLEN-(14 words)];
                    stackp = (char*)((int)stackp & ~0x7);
                    envp = (int *)env;
                    bcopy(envp[JB_SP], stackp, 14 words);
                    envp[JB_SP] = (int)stackp;
                    isis_longjmp(env, 1);
                }
            }
#           endif
            invoke();
	    goto out;
#endif  THREADS
        }
        ++isis_switched;

#ifdef	THREADS
            THREAD_WAIT(old_tp);
#else 	THREADS
	    isis_ctp = tp;
            isis_longjmp(tp->task_env, 1);
#endif	THREADS

out:
        if(isis_ctp == 0)
	    panic("isis_ctp null in task_swtch");
        if(isis_ctp->task_flag&TASK_XBYREF)
	    isis_state |= ISIS_XBYREF;
        else
	    isis_state &= ~ISIS_XBYREF;
        if(free_tp)
        {
	    if(free_tp->task_entry != -1 && esize(free_tp->task_entry))
	        free(free_tp);
	    else
	        st_free(free_tp);
	    free_tp = 0;
        }
        if(isis_ctp == &isis_main_task && call_on_sys_stack)
        {
	    register res;
	    res = _ISISCALL1(call_on_sys_stack, call_arg);
	    t_sig(&call_wait, (void *) res);
	    call_on_sys_stack = 0;
        }
  }

void
invoke()
  {
        register task *tp;
        /* Initial invocation of a routine */
	tp = isis_ctp;
	db_print("invoke: tp %x wants MUTEX\n", tp);
#ifdef	THREADS
        MUTEX_LOCK(isis_mutex);
        THREAD_SELF(tp->task_threadid);
#endif
	db_print("invoke: tp %x got MUTEX\n", tp);
        if(isis_state&ISIS_RECEIVING)
        {
	    isis_state &= ~ISIS_RECEIVING;
	    if(isis_gip)
	        tp->task_flag |= TASK_LOGGED;
	    if(bit(&inhibit_entries, isis_enum))
	    {
	        tp->task_flag |= TASK_INHIBIT;
	        pg_join_inhibit(1);
	    }
	    if(isis_eid)
	        tp->task_eid = *isis_eid;
        }
        else
	    tp->task_entry = -1;
#ifdef DB_TASKS
	print("invoke: tp %x obtained MUTEX call %s %x(0x%x)\n",
               tp, delayed_task? "delayed": "urgent", tp->task_routine,
               tp->task_arg0);
#endif
	new_task = isis_ctp;
        if(delayed_task)
	{
            register task *tp = delayed_task;
            delayed_task = 0;
	    /* Wait for delayed task to finish and for runqueue to cycle */
            task_enqueue(&isis_runqueue, isis_ctp, SD);
	    /* Resume delayed task */
            task_swtch(tp, NULLROUTINE, NULLARG);
        }
	begin
	{
	    register state = isis_state&ISIS_XBYREF;
	    isis_state &= ~ISIS_XBYREF;
            if(state&ISIS_XBYREF)
	        ISISCALL1(tp->task_routine, &tp->task_arg0);
	    else
	        ISISCALL1(tp->task_routine, tp->task_arg0);
	}
        /* Done, deallocate the stack */
        if((isis_state&ISIS_STARTUP) && (isis_ctp->task_flag&TASK_START))
            isis_start_done();
        qu_free(isis_ctp->task_active);
#ifdef  THREADS
        THREAD_FREE(isis_ctp);
#endif  THREADS
        if(isis_ctp->task_flag&TASK_LOGGED)
            log_end_log_msg();
        if(isis_ctp->task_flag&TASK_INHIBIT)
            pg_join_inhibit(0);
        if(isis_ctp->task_msg_arg)
            msg_delete(isis_ctp->task_msg_arg);
        tp = task_dequeue(&isis_runqueue);
	if(tp == (task*)-1)
	    tp = isis_scheduler;
        free_tp = isis_ctp;
	if(--isis_ntasks == ISIS_TASKLO && task_congest)
	{
	    task_congest = 0;
	    t_sig_all(&task_decongested, 0);
	}
        isis_ctp = tp;
#ifdef  DB_THREADS
	print("invoke doing longjmp...\n");
#endif  DB_THREADS
#ifdef  THREADS
	if(isis_ctp)
	    THREAD_SIGNAL(isis_ctp);
	db_print("invoke: tp %x released MUTEX\n", tp);
        MUTEX_UNLOCK(isis_mutex);
#else   THREADS
	if(isis_ctp)
            isis_longjmp(isis_ctp->task_env, 1);
#endif  THREADS
#ifdef  DB_THREADS
	print("invoke returns...\n");
#endif  DB_THREADS
  }

/****************************************************************/
/*                                                              */
/*                 Basic monitor support                        */
/*                                                              */
/****************************************************************/
void
t_sig(cp, rval)
  register condition *cp;
  VOID *rval;
  {
        register qnode *qp;
#ifdef  THREADS
        THREAD_T self;
        THREAD_SELF(self);
        if(isis_ctp->task_threadid != self)
             MUTEX_LOCK(isis_mutex);
#endif THREADS
        if(qp = qu_head(*cp))
        {
            register task *np;
	    np = task_dequeue(cp);
            np->task_rval = rval;
            task_enqueue(&isis_runqueue, np, SD);
        }
#ifdef THREADS
        if(isis_ctp->task_threadid != self)
	{
	     db_print("t_sig: extern thread exits and releases MUTEX\n", 0);
             MUTEX_UNLOCK(isis_mutex);
	}
#endif THREADS
  }

void
t_sig_all(cp, rval)
  register condition *cp;
  VOID *rval;
  {
        while(*cp)
            t_sig(cp, rval);
  }

void
t_sig_urgent(cp, rval)
  register condition *cp;
  VOID *rval;
  {
#ifdef  THREADS
        THREAD_T self;
        THREAD_SELF(self);
        if(isis_ctp->task_threadid != self)
	     panic("t_sig_urgent: illegal from a non-isis task");
#endif
        if(qu_head(*cp))
        {
            register task *np;
            np = task_dequeue(cp);
            np->task_rval = rval;
            task_enqueue(&isis_runqueue, isis_ctp, SU);
            task_swtch(np, NULLROUTINE, NULLARG);
        }
  }

#undef  t_wait
void *
t_wait(cp)
  register condition *cp;
  {
        return(t_wait_l(cp, NULLARG));
  }


void *
t_wait_l(cp, why)
  register condition *cp;
  char *why;
  {
        task *tp;
#ifdef  THREADS
        THREAD_T self;
        THREAD_SELF(self);
        if(isis_ctp->task_threadid != self)
	{
	    thread_isis_enter(cp, why);
            ISIS_RETURN((void*)isis_ctp->task_rval);
	}
        ++isis_ctp->task_entries;
#endif  THREADS
	if(isis_ctp == isis_scheduler)
	    isis_scheduler = 0;
        isis_ctp->task_waitingfor = why;
        task_enqueue(cp, isis_ctp, SD);
        tp = task_dequeue(&isis_runqueue);
        task_swtch(tp, NULLROUTINE, NULLARG);
        ISIS_RETURN((void*)isis_ctp->task_rval);
  }

void *
t_yield()
  {
        register task *tp;
        ISIS_ENTER();
        if(isis_ctp != isis_scheduler)
            ISIS_RETURN(t_wait_l(&isis_runqueue, "isis system: wants to let other tasks run first"));
        tp = task_dequeue(&isis_runqueue);
        if(tp != isis_ctp)
            task_swtch(tp, NULLROUTINE, NULLARG);
        ISIS_RETURN((void*)isis_ctp->task_rval);
  }

int    thread_wants_entry;

#ifdef THREADS

#define	MAX_ZOMBIES	10

static	nzombie;

void
thread_isis_enter(cp, why)
  condition *cp;
  char *why;
  {
	register task *tp;
	register qnode *qp;
	int istate = isis_state;
	THREAD_T self;
	if(!isis_state&ISIS_TINIT)
	    panic("ISIS task package not initialized: must call isis_init() or t_init()\n");
	THREAD_SELF(self);
	++thread_wants_entry;
	db_print("thread_isis_enter: thread requests MUTEX\n", 0);
	MUTEX_LOCK(isis_mutex);
	db_print("thread_isis_enter: thread acquired MUTEX\n", 0);
	--thread_wants_entry;
	for(qp = isis_tasks->qu_next; qp != isis_tasks; qp = qp->qu_next)
	{
	    tp = qp->qu_task;
	    if(tp->task_threadid == self)
	    {
                if(tp->task_flag&TASK_ZOMBIE)
		{
                    --nzombie;
		    tp->task_flag &= ~TASK_ZOMBIE;
		}
		db_print("thread_isis_enter is reusing an entry for task %x\n", tp);
		if(istate&ISIS_XBYREF)
		    tp->task_flag |= TASK_XBYREF;
		else
		    tp->task_flag &= ~TASK_XBYREF;
	        tp->task_entries = 1;
		tp->task_act = act_blocked;
	        if(cp && (cp != &isis_runqueue || isis_scheduler || isis_ctp != DUMMYTASK))
		{
		    tp->task_waitingfor = why;
		    task_enqueue(cp, tp, SD);
		    if(isis_scheduler == 0)
			db_print("there isn't any scheduler so thread_isis_enter is starting one\n", 0);
		    else
			db_print("there is already a scheduler so thread_isis_enter is just going to wait\n", 0);
                    if(isis_scheduler == 0 && isis_ctp == DUMMYTASK)
                    {
                        isis_ctp = tp;
			tp->task_msgid = isis_main_task.task_msgid;
			isis_main_task.task_msgid = 0;
                        task_swtch(NULLTASK, (vfunc *) isis_accept_events_loop,
                                   (void *) ISIS_BLOCK);
		    }
                    else
		        THREAD_WAIT(tp);
		}
		else if(cp == 0 || cp == &isis_runqueue)
		{
		    db_print("thread_isis_enter is simply setting isis_ctp to %x\n", tp);
		    isis_ctp = tp;
		}
		isis_state |= istate&ISIS_XBYREF;
	        return;
	    }
	}
        tp = st_alloc();
        ++isis_created;
	if(++isis_ntasks == ISIS_TASKHI && !task_congest)
	    ++task_congest;
        tp->task_threadid = self;
	tp->task_act = act_blocked;
        tp->task_queue = 0;
        tp->task_msgid = 0;
        tp->task_cond = 0;
	tp->task_entries = 1;
        tp->task_mwant = 0;
        tp->task_msg = 0;
        tp->task_flag = TASK_TEMP;
	tp->task_entries = 1;
	if(istate&ISIS_XBYREF)
	    tp->task_flag |= TASK_XBYREF;
        tp->task_active = qu_add_tp(isis_tasks, TA_ISTASK, tp);
#ifdef  CTHREADS
        tp->task_routine = (vfunc*)self->func;
        tp->task_arg0 = self->arg;
	tp->task_name = self->name;
#else   CTHREADS
        tp->task_routine = NULLROUTINE;
        tp->task_arg0 = 0;
	tp->task_name = 0;
#endif  CTHREADS
        tp->task_addr = my_address;
        tp->task_msg_arg = 0;
        tp->task_cohorts = (address*)0;
        tp->task_waitingfor = 0;
	tp->task_sleep = 0;
	db_print("thread_isis_enter is making a new entry for task %x\n", tp);
	THREAD_ALLOC(tp);
	if(cp && (cp != &isis_runqueue || isis_scheduler || isis_ctp != DUMMYTASK))
	{
            tp->task_waitingfor = why;
	    task_enqueue(cp, tp, SD);
	    if(isis_scheduler == 0)
		db_print("there isn't any scheduler so thread_isis_enter is starting one\n", 0);
	    else
		db_print("there is already a scheduler so thread_isis_enter is just going to wait\n", 0);
	    if(isis_scheduler == 0 && isis_ctp == DUMMYTASK)
	    {
		isis_ctp = tp;
                tp->task_msgid = isis_main_task.task_msgid;
                isis_main_task.task_msgid = 0;
		task_swtch(NULLTASK, (vfunc *) isis_accept_events_loop,
                           (void *) ISIS_BLOCK);
	    }
            else
	        THREAD_WAIT(tp);
	}
        else if(cp == 0 || cp == &isis_runqueue)
	{
	    db_print("thread_isis_enter is simply setting isis_ctp to %x\n", tp);
	    isis_ctp = tp;
	}
	isis_state |= istate&ISIS_XBYREF;
  }

void
thread_isis_exit()
  {
	if(isis_ctp->task_flag&TASK_TEMP)
	{
	    db_print("thread_isis_exit: thread entry count %d\n", isis_ctp->task_entries);
            if(--isis_ctp->task_entries > 0)
	        return;
            if(isis_ctp->task_entries < 0)
	    {
	        print("thread_isis_exit: entry count %d\n", isis_ctp->task_entries);
	        abort();
	    }
	    isis_ctp->task_flag |= TASK_ZOMBIE;
	    if(++nzombie == MAX_ZOMBIES)
	        thread_isis_cleanup();
	    db_print("thread_isis_exit: thread %x releases MUTEX\n", isis_ctp);
	}
        isis_ctp = task_dequeue(&isis_runqueue);
	db_print("thread_isis_exit: next thread to run will be %x\n", isis_ctp);
        if(isis_ctp != (task*)-1 || (isis_ctp = isis_scheduler))
            THREAD_SIGNAL(isis_ctp);
        MUTEX_UNLOCK(isis_mutex);
  }

thread_isis_cleanup()
  {
        register task *tp;
	register qnode *qp, *nqp;
	for(qp = isis_tasks->qu_next; qp != isis_tasks; qp = nqp)
	{
	    nqp = qp->qu_next;
	    tp = qp->qu_task;
	    if((tp->task_flag&TASK_ZOMBIE) == 0)
		continue;
            qu_free(tp->task_active);
	    THREAD_FREE(tp);
	    st_free(tp);
	}
  }

#else  THREADS

void
thread_isis_enter(cp, why)
  condition *cp;
  char *why;
{}

void
thread_isis_exit()         {}

#endif THREADS
