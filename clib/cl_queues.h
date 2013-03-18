/*  $RCSfile: cl_queues.h,v $ $Revision: 2.5 $ $Date: 90/06/11 10:33:32 $  */
/*
 *	Originally coded by Ken Birman
 *      Queue management utilities
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

struct qnode {
	qnode *qu_next;
	qnode *qu_last;
	vfunc *qu_freeroutine;
	int qu_flag;
	union {
		int qu_isint;
		char *qu_ischar;
		address qu_isaddress;
		int (*qu_isproc) ();
		site_id qu_issid;
		x_id qu_isxid;
	} qu_nun;
	union {
		char *qu_ischar;
		qnode *qu_isqueue;
		int qu_isint;
		int qu_areints[3];
		char *qu_arechars[3];
		int (*qu_isproc) ();
		bitvec qu_isbitvec;
		task *qu_istask;
		condition qu_iscond;
		address qu_isdaddress;
		site_id qu_issid;
		pwatch *qu_ispw;
		wnode *qu_iswnode;
		groupview *qu_isgroupv;
		etree *qu_iset;
		gnode *qu_isnd;
		event_id *qu_iseid;
		svmon *qu_issvm;
		bc_args *qu_isbc;
		cl_watch *qu_isclw;
		ioq *qu_isioq;
		mdesc *qu_ismd;
		ginfo *qu_isgip;
		x_info *qu_isxinfo;
		x_part *qu_isxpart;
		bc_node *qu_isbcn;
		struct {
			qnode *qu_isaqueue;
			int qu_isanint;
		} qu_ismlist;
		struct {
			message *qu_ismsg;
			int qu_isanint;
		} qu_ismslist;
	} qu_dun;
};

/* Node names */
#define         qu_name         qu_nun.qu_isint
#define         qu_time         qu_nun.qu_isint
#define         qu_act          qu_nun.qu_isint
#define         qu_count        qu_nun.qu_isint
#define         qu_string       qu_nun.qu_ischar
#define         qu_pname        qu_nun.qu_isaddress
#define         qu_callback     qu_nun.qu_isproc
#define         qu_wsid         qu_nun.qu_issid
#define         qu_xid          qu_nun.qu_isxid
#define         qu_mseqn        qu_nun.qu_isint

/* Data fields */
#define         qu_data         qu_dun.qu_ischar
#define         qu_type         qu_dun.qu_arechars[0]
#define         qu_value        qu_dun.qu_arechars[1]
#define         qu_len          qu_dun.qu_arechars[2]
#define         qu_arg          qu_dun.qu_ischar
#define         qu_args         qu_dun.qu_arechars
#define         qu_left         qu_dun.qu_areints[0]
#define         qu_right        qu_dun.qu_areints[1]
#define         qu_queue        qu_dun.qu_isqueue
#define         qu_cond         qu_dun.qu_iscond
#define         qu_address      qu_dun.qu_isdaddress
#define         qu_msg          qu_dun.qu_ismslist.qu_ismsg
#define         qu_viewid       qu_dun.qu_ismslist.qu_isanint
#define         qu_gseqn        qu_dun.qu_areints[2]
#define         qu_pn           qu_dun.qu_areints[2]
#define         qu_timeargs     qu_dun.qu_arechars
#define         qu_bitvec       qu_dun.qu_isbitvec
#define         qu_sid          qu_dun.qu_issid
#define         qu_proc         qu_dun.qu_isproc
#define         qu_task         qu_dun.qu_istask
#define         qu_pwatch       qu_dun.qu_ispw
#define         qu_ioq          qu_dun.qu_isioq
#define         qu_md           qu_dun.qu_ismd
#define         qu_wnode        qu_dun.qu_iswnode
#define         qu_gv           qu_dun.qu_isgroupv
#define         qu_nd           qu_dun.qu_isnd
#define         qu_et           qu_dun.qu_iset
#define         qu_eid          qu_dun.qu_iseid
#define         qu_gip          qu_dun.qu_isgip
#define         qu_svm          qu_dun.qu_issvm
#define         qu_bc           qu_dun.qu_isbc
#define         qu_clw          qu_dun.qu_isclw
#define         qu_xinfo        qu_dun.qu_isxinfo
#define         qu_xpart        qu_dun.qu_isxpart
#define         qu_bcn          qu_dun.qu_isbcn

/* Aliased fields */
#define         qu_site         qu_address.site
#define         qu_incarn       qu_address.incarn
#define         qu_process      qu_address.process

/* Aliased routines */
extern qnode *qu_freelist;
extern qnode *qu_newqp;
extern adesc qu_adesc;

#define qalloc()        ((qnode*)mallocate(&qu_adesc))

#define qu_remove(qp)                                                   \
  {                                                                     \
        qp->qu_next->qu_last = qp->qu_last;                             \
        qp->qu_last->qu_next = qp->qu_next;                             \
        qp->qu_next = qp->qu_last = qp;                                 \
  }

#define qu_head(qp)  (((qp) && (qp) != (qp)->qu_next)? (qp)->qu_next: NULLQP)

#define qu_alloc1(qp, name, d0, routine)                                \
  {                                                                     \
        if((qp = qu_head(qu_freelist)) == NULLQP)                       \
            qp = qalloc();                                              \
        else                                                            \
             (qp->qu_next->qu_last = qp->qu_last)->qu_next = qp->qu_next;\
        qp->qu_next = qp->qu_last = qp;                                 \
        qp->qu_string = (char*)name;                                    \
        qp->qu_args[0] = (char*)d0;                                     \
        qp->qu_freeroutine = routine;                                   \
  }

#define qu_alloc3(qp, name, d0, d1, d2, routine)                        \
  {                                                                     \
        if((qp = qu_head(qu_freelist)) == NULLQP)                       \
            qp = qalloc();                                              \
        else                                                            \
             (qp->qu_next->qu_last = qp->qu_last)->qu_next = qp->qu_next;\
        qp->qu_next = qp->qu_last = qp;                                 \
        qp->qu_string = (char*)name;                                    \
        qp->qu_args[0] = (char*)d0;                                     \
        qp->qu_args[1] = (char*)d1;                                     \
        qp->qu_args[2] = (char*)d2;                                     \
        qp->qu_freeroutine = routine;                                   \
  }

#define qu_tnull(qp)                                                    \
  {                                                                     \
        if((qp = qu_head(qu_freelist)) == NULLQP)                       \
            qp = qalloc();                                              \
        else                                                            \
        {                                                               \
             qp->qu_next->qu_last = qp->qu_last;                        \
             qp->qu_last->qu_next = qp->qu_next;                        \
        }                                                               \
        qp->qu_next = qp->qu_last = qp;                                 \
        qp->qu_freeroutine = NULLROUTINE;                               \
  }

#define qu_append(qp, np)                                               \
  {                                                                     \
        (np->qu_last = qp->qu_last)->qu_next = np;                      \
        qp->qu_last = np;                                               \
        np->qu_next = qp;                                               \
  }

#ifdef SUN			/* Everyone else has a compiler bug */
#define qu_free(qp)                                                     \
  {                                                                     \
        if(qp)                                                          \
        {                                                               \
            (qp->qu_next->qu_last = qp->qu_last)->qu_next = qp->qu_next;\
            if(qp->qu_freeroutine)                                      \
                ISISCALL1(qp->qu_freeroutine, qp->qu_data);                 \
            begin                                                       \
            {                                                           \
                register qnode *qf = qu_freelist;                       \
                if(qf == 0)                                             \
                    qu_freelist = qf = qu_null();                       \
                qp->qu_next = qf;                                       \
                (qp->qu_last = qf->qu_last)->qu_next = qp;              \
                qf->qu_last = qp;                                       \
            }                                                           \
        }                                                               \
  }
#else
#define qu_free(qp)                                                     \
  {                                                                     \
        if(qp)                                                          \
        {                                                               \
            qp->qu_last->qu_next = qp->qu_next;                         \
            qp->qu_next->qu_last = qp->qu_last;                         \
            if(qp->qu_freeroutine)                                      \
                ISISCALL1(qp->qu_freeroutine, qp->qu_data);                 \
            begin                                                       \
            {                                                           \
                register qnode *qf = qu_freelist;                       \
                if(qf == 0)                                             \
                    qu_freelist = qf = qu_null();                       \
                qp->qu_next = qf;                                       \
                (qp->qu_last = qf->qu_last)->qu_next = qp;              \
                qf->qu_last = qp;                                       \
            }                                                           \
        }                                                               \
  }
#endif

#define         qu_null()               qu_alloc(0,NULLARG,NULLROUTINE)
#define         qu_alloc_qu(n,v,r)      qu_alloc(n,(VOID*)v,(vfunc*)r)
#define         qu_add_qu(q,n,v)        qu_add(q,n,(VOID*)v,(vfunc *) qu_freeall)
#define         qu_add_cond(q,n)        qu_add(q,n,NULLARG,NULLROUTINE)
#define         qu_add_mp(q,n,v,r)      qu_add(q,n,(VOID*)v,(vfunc*)r)
#define         qu_add_md(q,n,v,r)      qu_add(q,n,(VOID*)v,(vfunc*)r)
#define         qu_add_proc(q,n,v,r)    qu_add(q,n,(VOID*)v,(vfunc*)r)
#define         qu_add_tp(q,n,v)        qu_add(q,n,(VOID*)v,NULLROUTINE)
#define         qu_add_gp(q,n,v)        qu_add(q,n,(VOID*)v,NULLROUTINE)
#define         qu_add_gw(q,n,v,r)      qu_add(q,n,(VOID*)v,(vfunc*)r)
#define         qu_add_nd(q,n,v)        qu_add(q,n,(VOID*)v,NULLROUTINE)
#define         qu_add_et(q,n,v,r)      qu_add(q,n,(VOID*)v,(vfunc*)r)
#define         qu_add_eid(q,n,v,r)     qu_add(q,n,(VOID*)v,(vfunc*)r)
#define         qu_add_gip(q,n,g)       qu_add(q,n,(VOID*)g,NULLROUTINE)
#define         qu_add_svm(q,n,v,r)     qu_add(q,n,(VOID*)v,(vfunc*)r)
#define         qu_add_bc(q,n,v,r)      qu_add(q,n,(VOID*)v,(vfunc*)r)
#define         qu_add_ioq(q,n,v)       qu_add(q,n,(VOID*)v,NULLROUTINE)
#define         qu_add_clw(q,n,v,r)     qu_add(q,n,(VOID*)v,(vfunc*)r)
#define         qu_alloc_int(n,v)       qu_alloc(n,(VOID*)v,NULLROUTINE)
#define         qu_alloc_tp(n,v)        qu_alloc(n,(VOID*)v,NULLROUTINE)
#define         pg_add_pgroup(q,n,v,r)  pg_add(q,n,(VOID*)v,(vfunc*)r)
#define         pg_add_qu(q,n,v)        pg_add(q,n,(VOID*)v,(vfunc *) qu_freeall)
#define         pg_add_wp(q,n,v,r)      pg_add(q,n,(VOID*)v,(vfunc*)r)
#define         pg_add_mp(q,n,v,r)      pg_add(q,n,(VOID*)v,(vfunc*)r)
#define         pg_add_pw(q,n,v,r)      pg_add(q,n,(VOID*)v,(vfunc*)r)
#define         pg_add_ioq(q,n,v,r)     pg_add(q,n,(VOID*)v,(vfunc*)r)
#define         wsid_add_wp(q,s,v,r)    qu_add(q,(int)s,v,(vfunc*)r)

/*** Routines ***/

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	qnode *pg_add(qnode * qp, address * pname, VOID * data, void (*routine) (VOID * arg));
	qnode *pg_alloc(address * pname, VOID * data, void (*routine) (VOID * arg));
	qnode *pg_find(qnode * qp, address * pname);
	qnode *qu_add(qnode * qp, int name, VOID * data, void (*routine) (VOID * arg));
	qnode *qu_add_cb(qnode * qp, void (*proc) (VOID * arg), VOID * arg0, VOID * arg1);
	qnode *qu_add_pg(qnode * qp, int name, address * paddr);
	qnode *qu_add_sid(qnode * qp, int name, int sid);
	qnode *qu_alloc(int name, VOID * data, void (*routine) (VOID * arg));
	qnode *qu_alloc_pg(int name, address * paddr);
	qnode *qu_find(qnode * qp, int item);
	void qu_freeall(qnode * qp);
	void qu_resort(qnode * qp, qnode * np);
#ifdef __cplusplus
}
#endif
#else

void qu_freeall();
qnode *qu_alloc(), *qu_find(), *pg_find(), *qu_add();
qnode *pg_add(), *qu_add_pg(), *qu_add_sid(), *qu_add_cb();

#endif
