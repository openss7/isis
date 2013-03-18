/*  $RCSfile: pr_queues.h,v $ $Revision: 2.1 $ $Date: 90/06/11 10:15:58 $  */
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
 *      Queue management utilities
 */

typedef struct {
	qnode *qu_isqueue;
	int qu_isint;
} qun;

struct qnode {
	qnode *qu_next;
	qnode *qu_last;
	int (*qu_freeroutine) ();
	int qu_flag;
	union {
		int qu_isint;
		char *qu_ischar;
		address qu_isaddress;
		int (*qu_isproc) ();
	} qu_nun;
	union {
		char *qu_ischar;
		char *qu_arechars[2];
		qnode *qu_arequeues[2];
		qun qu_qun;
		int qu_isint;
		int (*qu_isproc) ();
		bitvec qu_isbitvec;
		struct task *qu_istask;
		condition qu_iscond;
		address qu_isdaddress;
		message *qu_ismsg;
		mdesc *qu_ismd;
		site_id qu_issid;
		ioq *qu_isioq;
		sys_groupview *qu_issys_groupview;
		wait_item qu_iswitem;
		char *qu_aretimeargs[3];
	} qu_dun;
};

/* Node names */
#define         qu_name         qu_nun.qu_isint
#define         qu_time         qu_nun.qu_isint
#define         qu_pname        qu_nun.qu_isaddress
#define         qu_sname        qu_nun.qu_ischar
#define         qu_string       qu_nun.qu_ischar
#define         qu_callback     qu_nun.qu_isproc

/* Data fields */
#define         qu_data         qu_dun.qu_ischar
#define         qu_args         qu_dun.qu_arechars
#define         qu_value        qu_dun.qu_isint
#define         qu_queue        qu_dun.qu_qun.qu_isqueue
#define         qu_msgid        qu_dun.qu_qun.qu_isint
#define         qu_queues       qu_dun.qu_arequeues
#define         qu_cond         qu_dun.qu_iscond
#define         qu_address      qu_dun.qu_isdaddress
#define         qu_msg          qu_dun.qu_ismsg
#define         qu_md           qu_dun.qu_ismd
#define         qu_bitvec       qu_dun.qu_isbitvec
#define         qu_sid          qu_dun.qu_issid
#define         qu_ioq          qu_dun.qu_isioq
#define         qu_proc         qu_dun.qu_isproc
#define         qu_task         qu_dun.qu_istask
#define         qu_pg           qu_dun.qu_issys_groupview
#define         qu_witem        qu_dun.qu_iswitem
#define         qu_timeargs     qu_dun.qu_aretimeargs

/* Aliased fields */
#define         qu_site         qu_address.site
#define         qu_incarn       qu_address.incarn
#define         qu_process      qu_address.process

/* Aliased routines */
extern qnode *qu_freelist, *qu_newQP;
extern adesc qu_adesc;

#define qalloc()        ((qnode*)mallocate(&qu_adesc))

#define qu_remove(QP)                                                   \
  {                                                                     \
        QP->qu_next->qu_last = QP->qu_last;                             \
        QP->qu_last->qu_next = QP->qu_next;                             \
        QP->qu_next = QP->qu_last = QP;                                 \
  }

#define qu_head(QP)  (((QP) && (QP) != (QP)->qu_next)? (QP)->qu_next: (qnode*)0)

#define qu_alloc1(QP, NAME, D0, ROUTINE)                                \
  {                                                                     \
        if((QP = qu_head(qu_freelist)) == (qnode*)0)                    \
            QP = qalloc();                                              \
        else                                                            \
        {                                                               \
             QP->qu_next->qu_last = QP->qu_last;                        \
             QP->qu_last->qu_next = QP->qu_next;                        \
             memalloc += sizeof(qnode);                                  \
        }                                                               \
        QP->qu_next = QP->qu_last = QP;                                 \
        QP->qu_string = (char*)NAME;                                    \
        QP->qu_args[0] = (char*)D0;                                     \
        QP->qu_freeroutine = ROUTINE;                                   \
  }

#define qu_alloc3(QP, NAME, D0, D1, D2, ROUTINE)                        \
  {                                                                     \
        if((QP = qu_head(qu_freelist)) == (qnode*)0)                    \
            QP = qalloc();                                              \
        else                                                            \
        {                                                               \
             QP->qu_next->qu_last = QP->qu_last;                        \
             QP->qu_last->qu_next = QP->qu_next;                        \
             memalloc += sizeof(qnode);                                  \
        }                                                               \
        QP->qu_next = QP->qu_last = QP;                                 \
        QP->qu_string = (char*)NAME;                                    \
        QP->qu_args[0] = (char*)D0;                                     \
        QP->qu_args[1] = (char*)D1;                                     \
        QP->qu_args[2] = (char*)D2;                                     \
        QP->qu_freeroutine = ROUTINE;                                   \
  }

#define qu_allocpg(QP, PNAME, D0, ROUTINE)                              \
  {                                                                     \
        if((QP = qu_head(qu_freelist)) == (qnode*)0)                    \
            QP = qalloc();                                              \
        else                                                            \
        {                                                               \
             QP->qu_next->qu_last = QP->qu_last;                        \
             QP->qu_last->qu_next = QP->qu_next;                        \
             memalloc += sizeof(qnode);                                  \
        }                                                               \
        QP->qu_next = QP->qu_last = QP;                                 \
        QP->qu_pname = PNAME;                                           \
        QP->qu_pname.addr_entry = 0;                                    \
        QP->qu_args[0] = (char*)D0;                                     \
        QP->qu_freeroutine = ROUTINE;                                   \
  }

#define qu_tnull(QP)                                                    \
  {                                                                     \
        if((QP = qu_head(qu_freelist)) == (qnode*)0)                    \
            QP = qalloc();                                              \
        else                                                            \
        {                                                               \
             QP->qu_next->qu_last = QP->qu_last;                        \
             QP->qu_last->qu_next = QP->qu_next;                        \
             memalloc += sizeof(qnode);                                  \
        }                                                               \
        QP->qu_next = QP->qu_last = QP;                                 \
        QP->qu_freeroutine = nullroutine;                               \
  }

#define qu_append(QP, NP)                                               \
  {                                                                     \
        register qnode *OP = QP->qu_last;                               \
        OP->qu_next = NP;                                               \
        NP->qu_last = OP;                                               \
        QP->qu_last = NP;                                               \
        NP->qu_next = QP;                                               \
  }

#ifdef SUN			/* Everyone else has a compiler bug */
#define qu_free(QP)                                                     \
  {                                                                     \
        if(QP)                                                          \
        {                                                               \
            (QP->qu_next->qu_last = QP->qu_last)->qu_next = QP->qu_next;\
            if(QP->qu_freeroutine)                                      \
                (*QP->qu_freeroutine)(QP->qu_data);                     \
            begin                                                       \
            {                                                           \
                register qnode *QF = qu_freelist;                       \
                if(QF == 0)                                             \
                    qu_freelist = QF = qu_null();                       \
                QP->qu_next = QF;                                       \
                (QP->qu_last = QF->qu_last)->qu_next = QP;              \
                QF->qu_last = QP;                                       \
                memfree += sizeof(qnode);                               \
            }                                                           \
        }                                                               \
  }
#else
#define qu_free(QP)                                                     \
  {                                                                     \
        if(QP)                                                          \
        {                                                               \
            QP->qu_last->qu_next = QP->qu_next;                         \
            QP->qu_next->qu_last = QP->qu_last;                         \
            if(QP->qu_freeroutine)                                      \
                (*QP->qu_freeroutine)(QP->qu_data);                     \
            begin                                                       \
            {                                                           \
                register qnode *QF = qu_freelist;                       \
                if(QF == 0)                                             \
                    qu_freelist = QF = qu_null();                       \
                QP->qu_next = QF;                                       \
                QF->qu_last->qu_next = QP;                              \
                QP->qu_last = QF->qu_last;                              \
                QF->qu_last = QP;                                       \
                memfree += sizeof(qnode);                               \
            }                                                           \
        }                                                               \
  }
#endif

/* Simple aliased routines */
#define         qu_null()               qu_alloc(0,(char*)0,nullroutine)
#define         qu_alloc_qu(n,v,r)      qu_alloc(n,(char*)v,r)
#define         qu_add_qu(q,n,v)        qu_add(q,n,(char*)v,qu_freeall)
#define         qu_add_cond(q,n)        qu_add(q,n,(char*)0,nullroutine)
#define         qu_add_mp(q,n,v,r)      qu_add(q,n,(char*)v,r)
#define         qu_add_md(q,n,v,r)      qu_add(q,n,(char*)v,r)
#define         qu_add_proc(q,n,v,r)    qu_add(q,n,(char*)v,r)
#define         qu_add_tp(q,n,v)        qu_add(q,n,(char*)v,nullroutine)
#define         qu_add_ioq(q,n,v)       qu_add(q,n,(char*)v,nullroutine)
#define         qu_alloc_int(n,v)       qu_alloc(n,(char*)v,nullroutine)
#define         qu_alloc_tp(n,v)        qu_alloc(n,(char*)v,nullroutine)
#define         pg_add_sys_groupview(q,n,v,r)  pg_add(q,n,(char*)v,r)
#define         pg_add_qu(q,n,v)        pg_add(q,n,(char*)v,qu_freeall)

int qu_freeall();
qnode *qu_alloc();
qnode *qu_find(), *pg_find(), *qu_add_bits();
qnode *qu_add(), *pg_add(), *qu_add_cb(), *qu_add_sid();
