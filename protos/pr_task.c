/*  $RCSfile: pr_task.c,v $ $Revision: 2.24 $ $Date: 90/09/12 13:27:58 $  */
/*
 *	Originally coded by Ken Birman
 *      Task.c -- code related to task management
 *
 * Although urgent and immediate mode are supported here, we recommend that
 * they not be used.
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
 *
 */

# include <stdio.h>
# include <string.h>

# include "pr.h"

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
#     define   isis_setjmp(x)    0
#     define   isis_longjmp(x)   panic("isis_longjmp shouldn't have been called")

#endif THREADS

adesc           st_ad ={ TASKLEN, 0, 8 };

# define        st_alloc()      ((task*)mallocate(&st_ad))
# define        st_free(x)      mdeallocate((char*)x, &st_ad)

task    scheduler;              /* Task descriptor for system task */
task    *ctp = &scheduler;      /* Scheduler is running initially */

static  swtch();

# define        NULLROUTINE     (int(*)())      0
# define        NULLARG         (char*)         0
# define        NULLTASK        (task*)         0
address         NULLADDRESS;

static  message *task_msg_arg;

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
task_dequeue(cp)
  register condition *cp;
  {
        register qnode *qp, *np;
        register task *tp;
	if(*cp == 0)
	    return(&scheduler);
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

t_init()
  {
	tasks = qu_null();
#ifdef THREADS
        MUTEX_ALLOC(isis_mutex);
        MUTEX_LOCK(isis_mutex);
        CONDITION_ALLOC(ctp->task_runme);
        THREAD_SELF(ctp->task_threadid);
#endif THREADS
  }

run_tasks()
  {
        register task *tp;

        scheduler.task_routine = run_tasks;
        while((tp = task_dequeue(&runqueue)) != &scheduler)
            swtch(tp, NULLROUTINE, NULLARG);
  }

int     inhibit_swtch;

t_fork_urgent(routine, arg0, mp)
  int (*routine)();
  char *arg0;
  message *mp;
  {
        if(inhibit_swtch)
            panic("t_fork_urgent with inhibit_swtch set");
        if(task_msg_arg = mp)
            msg_increfcount(mp);
        task_enqueue(&runqueue, ctp, SU);
        swtch(NULLTASK, routine, arg0);
  }

static  task *delayed_task;
static  fork_mode;

t_fork_immed(routine, arg0, mp)
  int (*routine)();
  char *arg0;
  message *mp;
  {
        if(task_msg_arg = mp)
            msg_increfcount(mp);
        delayed_task = ctp;
        fork_mode = SU;
        swtch(NULLTASK, routine, arg0);
  }

t_fork(routine, arg0, mp)
  int (*routine)();
  char *arg0;
  message *mp;
  {
        if(task_msg_arg = mp)
            msg_increfcount(mp);
        delayed_task = ctp;
        fork_mode = SD;
        swtch(NULLTASK, routine, arg0);
  }

static delayedcall(routine, arg0)
  int (*routine)();
  char *arg0;
  {
        register task *tp = delayed_task;
        delayed_task = 0;
        task_enqueue(&runqueue, ctp, fork_mode);
        swtch(tp, nullroutine, (char*)0);
        (*routine)(arg0);
  }

t_scheck()
  {
#ifndef THREADS
        char stackp;
        if(ctp != &scheduler && &stackp <= ctp->task_stack)
            panic("Stack violation");
#endif
  }

#ifdef	THREADS
#define	t_scheck()
#endif

static  char *stackp;
static task *free_tp = 0;
static task *old_tp;
static invoke();

/* HP 9000/300 doesn't like optimizing asm statements. */
#ifdef          hp9000s300
#include        "OPT_LEVEL_1.h"
#endif

static swtch(tp, routine, arg0)
  register task *tp;
  register int (*routine)();
  register char *arg0;
  {
        t_scheck();
        /* Save entry information */
#ifndef THREADS
        if(isis_setjmp(ctp->task_env))
            goto task_resumes;
#endif THREADS

        /* Allocate a stack for the new task if needed */
        old_tp = ctp;
        if(tp == 0)
        {
            ctp = tp = st_alloc();
            ++ntasks;
            EVENT(S_NFORK);
            tp->task_queue = 0;
            tp->task_msgid = 0;
            tp->task_cond = 0;
            tp->task_iwait = 0;
            tp->task_mwant = 0;
            tp->task_msg = 0;
            bclr(&tp->task_watching);
            tp->task_msgs = qu_null();
            tp->task_active = qu_add_tp(tasks, TA_ISTASK, tp);
            tp->task_routine = routine;
            tp->task_arg0 = arg0;
            tp->task_addr = my_address;
            tp->task_addr.addr_entry = my_entry;
            tp->task_msg_arg = task_msg_arg;
            task_msg_arg = 0;
            tp->task_stack = ((char*)(&tp->task_stack))+sizeof(char*);
	    ctp = tp;
#ifdef  THREADS
            CONDITION_ALLOC(tp->task_runme);
            THREAD_FORK(invoke, tp, (char*)&tp->task_stack[STACKLEN]);
#else   THREADS
            /* Change stack pointers.  Alignment is important for many machines */
#           if(VAX)
            {
                stackp = &ctp->task_stack[STACKLEN-(2 words)];
                stackp = (char*)((int)stackp & ~0x3);
                asm("movl  _stackp,sp");
            }
#           endif
#           if(GOULD)
            {
                stackp = &ctp->task_stack[STACKLEN-(24 words)];
                stackp = (char*)((int)stackp & ~0x7);
                asm("movw  _stackp,b2");
            }
#           endif
#           if(AUX)
            {
                stackp = &ctp->task_stack[STACKLEN-(2 words)];
                stackp = (char*)((int)stackp & ~0x3);
                asm("mov.l  stackp,%sp");
            }
#           endif
#           if(SUN3)
            {
                stackp = &ctp->task_stack[STACKLEN-(2 words)];
                stackp = (char*)((int)stackp & ~0x3);
                asm("movl  _stackp,sp");
            }
#           endif
#           if(NEXT)
            {
                stackp = &ctp->task_stack[STACKLEN-(16 words)];
                stackp = (char*)((int)stackp & ~0x3);
                asm("movel _stackp,sp");
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
                    stackp = &ctp->task_stack[STACKLEN-(2 words)];
                    stackp = (char*)((int)stackp & ~0x3);
                    asm("mov.l  _stackp,%sp");
#endif          hp9000s300
#ifdef          hp9000s800
                    stackp = &ctp->task_stack[0];
                    stackp = (char*)((((int)stackp+7) & ~0x7)+ 48);
                    set_sp(stackp);
#endif          hp9000s800
            }
#           endif
#           if(MIPS|AIX|RT43|SGI)
            {
                jmp_buf env;
                int *envp;

                if (isis_setjmp(env) == 0) {
                    stackp = &ctp->task_stack[STACKLEN-(2 words)];
                    stackp = (char*)((int)stackp & ~0x7);
                    envp = (int *)env;
                    envp[JB_SP] = (int)stackp;
                    isis_longjmp(env, 1);
		    panic("unexpected return from longjmp");
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
                    panic("** unexpected return from longjmp");
                }
            }
#           endif
            invoke();  /* No return */
#endif  THREADS
        }
#ifdef  THREADS
        else
            CONDITION_SIGNAL(tp->task_runme);
#endif  THREADS

        EVENT(S_NSWTCH);
        ctp = tp;
#ifdef  THREADS
        CONDITION_WAIT(old_tp->task_runme);
#else   THREADS
        isis_longjmp(tp->task_env, 1);
	panic("unexpected return from longjmp");

task_resumes:
#endif  THREADS
        if(free_tp)
        {
	    st_free(free_tp);
	    free_tp = 0;    
            --ntasks;
	}
  }

static invoke()
  {
        register task *tp;
        /* Initial invocation of a routine */
	tp = ctp;
#ifdef  THREADS
        MUTEX_LOCK(isis_mutex);
        THREAD_SELF(tp->task_threadid);
#endif
        if(delayed_task)
            delayedcall(tp->task_routine, tp->task_arg0);
        else
            (*tp->task_routine)(tp->task_arg0);
        /* Done, deallocate the stack */
        qu_free(ctp->task_active);
        qu_freeall(ctp->task_msgs);
#ifdef  THREADS
        CONDITION_FREE(ctp->task_runme);
#endif  THREADS
        if(ctp->task_msg_arg)
            msg_delete(ctp->task_msg_arg);
        free_tp = ctp;
#ifdef  THREADS
        ctp = task_dequeue(&runqueue);
        CONDITION_SIGNAL(ctp->task_runme);
        MUTEX_UNLOCK(isis_mutex);
#else
	tp = task_dequeue(&runqueue);
        EVENT(S_NSWTCH);
        ctp = tp;
        isis_longjmp(tp->task_env, 1);
	/* Never returns */
	panic("unexpected return from longjmp");
#endif  THREADS
  }

t_sdump()
  {
        register *sp;
        register n = 0;
        t_scheck();
        sp = (int*)stackp;
        while(sp <= (int*)&ctp->task_stack[STACKLEN])
        {
            print("%8.8x  ", *sp++);
            if((++n&3) == 0)
                print("\n");
        }
        if(n&3)
            print("\n");
  }

/****************************************************************/
/*                                                              */
/*                 Basic monitor support                        */
/*                                                              */
/****************************************************************/
t_sig_all(cp, rval)
  register condition *cp;
  {
        while(*cp)
            t_sig(cp, rval);
  }

t_sig(cp, rval)
  register condition *cp;
  {
        register qnode *qp = *cp;
        if(qu_head(qp))
        {
            register task *tp = task_dequeue(cp);
            task_enqueue(&runqueue, tp, SD);
            tp->task_rval = rval;
        }
  }

t_sig_urgent(cp, rval)
  register condition *cp;
  {
        register qnode *qp;

        if(qp = qu_head(*cp))
        {
            qp->qu_task->task_rval = rval;
            task_enqueue(&runqueue, ctp, SU);
            swtch(task_dequeue(cp), NULLROUTINE, NULLARG);
        }
  }

t_sig_immed(cp, rval)
  register condition *cp;
  {
        register qnode *qp = *cp;

        if(qu_head(qp))
        {
            register task *tp = task_dequeue(cp);
            task_enqueue(&runqueue, tp, SU);
            tp->task_rval = rval;
        }
  }

t_wait(cp, why)
  condition *cp;
  char *why;
  {
        register task *tp;
        if(inhibit_swtch)
            panic("t_wait(%x,%s) with inhibit_swtch set", cp, why);
        ctp->task_waitingfor = why;
        task_enqueue(cp, ctp, SD);
        tp = task_dequeue(&runqueue);
        swtch(tp, NULLROUTINE, NULLARG);
        return(ctp->task_rval);
  }

t_suspend(why)
  char *why;
  {
        register task *tp;
        if(inhibit_swtch)
            panic("t_suspend(%s) with inhibit_swtch set", why);
        tp = task_dequeue(&runqueue);
        if(tp != ctp)
	{
            ctp->task_waitingfor = why;
            swtch(tp, NULLROUTINE, NULLARG);
	}
        return(ctp->task_rval);
  }
