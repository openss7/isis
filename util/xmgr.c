/* Simple Transaction Recovery Manager. 

   By Robert Cooper

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

   Reliably records a list of committed transactions. A list of aborted 
   transactions is not needed since we use the presumed abort technique.

   Currently if a transaction completes (commits or aborts) with
   none of the participants failing this service is not used at all.
   When a participant fails (during transaction termination) 
   this service is used to record the outcome for that participant to
   find what happened when it recovers.

   It is likely that in situations where a participant dies the whole transaction
   is aborted anyway (especially in the common case of only one participant), so
   we use the presumed abort strategy to reduce the storage and communication
   requirements of transactions. Thus only commits are recorded. If this service
   doesn't know about a completed transaction it must have aborted.

   Things to do:
   Currently a copy of this service is started up on every site. It should be
   started up on say five sites, and generate extra copies of itself (using
   rexec) if some copies die. 

   Currently all sites reply to a get_outcome message. We could have only
   one site reply, and the caller retry on failure.

   I'm not sure whether we should do manual flushing of the lmgr log, because
   there seems to be a long time between automatic flushes. However xmgr is
   very reliable and its unlikely to experience total failure.
*/

char xmgr_rcsid[] = "$Revision: 2.25 $$Date: 90/08/14 11:24:34 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/util/RCS/xmgr.c,v $";
#include "isis.h"

typedef struct {
    x_id *ids; 
    int max;     /* Elements 0 to max-1 of ids exist. */
    int n;       /* Elements 0 to n-1 of ids are defined, n <= max. */
} idlist;
#define increment 20 /* Amount by which to increase ids when it gets full. */

/* Replicated state of recovery manager. */
idlist committed; /* Ids of committed transactions. */

void
add_to_list(list, id)
  idlist *list;
  x_id *id;
  /* Add id to list. id should not already exist in list. */
{
    if (list-> n >= list-> max) {
        /* Increase the size of the array by increment. */
        list-> max += increment;
        list-> ids = (x_id *) realloc(list-> ids, sizeof(x_id) * (list-> max));
    }
    list-> ids[(list-> n)++] = *id;
}

bool
search_list(list, id)
  idlist *list;
  x_id *id;
  /* Test whether id is a member of list. */
{
    register int i;
    register int n = list-> n;
    register x_id *ids = list-> ids;
    
    for (i = 0; i < n; i++) {
        if (xid_cmp(id, &(ids[i])) == 0) {
            return(TRUE);
        }
    }
    return(FALSE);
}  

void
send_state(locator, gaddr)
  int locator;
  address *gaddr;
{
    xfer_out(1, "%d", committed.n);
    xfer_out(2, "%A", committed.ids, committed.n);
}

void
receive_state(locator, msg)
  int locator;
  message *msg;
{
    if (locator == 1) {
#       ifdef trans_debug
        print("Transaction Recovery Manager: restart");
#       endif
        msg_get(msg, "%d", &(committed.n));
        committed.max = committed.n + increment;
        committed.ids = (x_id *) malloc(sizeof(x_id) * committed.max);

    } else if (locator == 2) {
        msg_get(msg, "%A", committed.ids, (int *) 0);

    } else {
        panic("XMgr, receive_state: got bad locator");
    }
}

void
init_lists(gaddr)
  address *gaddr; /* Ignored. */
{
#   ifdef trans_debug
    print("Transaction Recovery Manager: total startup\n");    
#   endif
    committed.n = 0;
    committed.max = increment;
    committed.ids = (x_id *) malloc(sizeof(x_id) * increment);
}

condition wake_me_up;

void
lmgr_check_done()
{
	address lmgr;
	lmgr = my_address; lmgr.addr_process = LMGR;
	cbcast(&lmgr, LR_CHECK_DONE, "", 1, "");
	t_sig(&wake_me_up, 0);
}

void
lmgr_check_timeout()
{
        sleep(30);
	t_sig(&wake_me_up, 0);
}

void
transaction_service() {
    address *gaddr;
    int join_retries = 10;

    sleep(15);
    forever {
        print("Transaction Manager: checking that lmgr is initialized...\n");
	t_fork((void*)lmgr_check_done, 0);
	t_fork((void*)lmgr_check_timeout, 0);
	t_wait(&wake_me_up);
        print("Transaction Manager: lmgr initialized, resuming xmgr startup seq...\n");
        gaddr = pg_join(xmgr_service,
                        PG_INIT, init_lists,
                        PG_LOGGED, xmgr_service, 0, L_AUTO, NULL,
                        PG_XFER, 1, send_state, receive_state,
                        0);
        if (addr_isnull(gaddr)) {
            if (isis_errno == IE_MUSTJOIN &&
                join_retries-- > 0)
            {
		static printed;
		if(printed++ == 0)
                    print("XMgr: waiting for a more recently failed XMgr to recover\n");
                sleep(30);
                continue;
            } else {
                isis_perror("XMgr, pg_join");
                panic("");
            }
        } else {
            break;
        }
    }
    isis_logentry(gaddr, XR_SAVE_OUTCOME);
    isis_start_done();
    
    print("Transaction Manager (xmgr): initialization complete.\n");

#   ifndef trans_debug
    begin
    {
        char logfile[128];
        sprintf(logfile, "%s/xmgr.log", isis_dir);
        freopen(logfile, "w", stdout); /* Save error messages in a log. */
        chmod(logfile, 666);
    }
#   endif
}

void
handle_save_outcome(msg)
  message *msg;
  /* Input: "%A %d" id, outcome (must be X_COMMIT)
     Output: ""

     Record the outcome of the transaction with this id. Since we presume abort
     we only allow X_COMMIT outcomes to be recorded.
  */
{
    int i;
    x_id id;
    int dummy;
    int outcome;
    msg_get(msg, "%A %d", &id, &dummy, &outcome);

    if (outcome == X_ABORT) {
        print("XMgr: save_outcome got obsolete X_ABORT outcome for transaction ");
        paddr(&id); print("\n");
        reply(msg, "");
    } 

    /* Check it doesn't already exist. */
    if (search_list(&committed, &id)) {
#       ifdef trans_debug
        print("save_outcome: "); paddr(&id); print(" duplicate commit\n");
#       endif            
    } else {
#       ifdef trans_debug
        print("save_outcome: "); paddr(&id); print(" commit\n");
#       endif            
        add_to_list(&committed, &id);
    }
    reply(msg, "");
}

void
handle_get_outcome(msg)
  message *msg;
/* Input: "%A" id
   Output: "%d" outcome (one of X_COMMIT, X_ABORT).

   Return the saved outcome of the transaction with this id. We presume
   a transaction aborted if we don't know anything about it.
*/
{
    int i;
    x_id id;
    int dummy;
    msg_get(msg, "%A", &id, &dummy);

    if (search_list(&committed, &id)) {
#       ifdef trans_debug
        print("get_outcome: "); paddr(&id); print(" commit\n");
#       endif            
        reply(msg, "%d", X_COMMIT);
        return;
    } else {
#       ifdef trans_debug
        print("get_outcome: "); paddr(&id); print(" abort\n");
#       endif            
        reply(msg, "%d", X_ABORT);
        return;
    }
}

void
main (argc, argv)
  int argc;
  char *argv[];
{
    int isis_port = 0;

    if (argc != 2) {
        fprintf(stderr, "usage: %s port\n", argv[0]);
        exit(-1);
    }
    isis_port = atoi(argv[1]);
    if (isis_port == 0) {
        fprintf(stderr, "usage: %s port\n", argv[0]);
        exit(-1);
    }
    my_process_id = XMGR; /* Special negative process id to ease identification. */
        /* Need to modify cmd to recognize this process. */

    isis_init(isis_port);
 
    isis_task((vfunc*)transaction_service, "transaction_service");
    isis_task((vfunc*)lmgr_check_done, "lmgr_check_done");
    isis_task((vfunc*)lmgr_check_timeout, "lmgr_check_timeout");
    isis_entry(XR_SAVE_OUTCOME, (vfunc*)handle_save_outcome, "handle_save_outcome");
    isis_entry(XR_GET_OUTCOME, (vfunc*)handle_get_outcome, "handle_get_outcome");

    isis_mainloop((vfunc*)transaction_service, NULLARG);
}
