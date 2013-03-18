/*  $RCSfile: bank.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:58 $  */
/*******************************************************************************
 Toy bank transaction demo program: bank server.

 This is a very simple demo program for the Isis transaction mechanism.

 Currently we allow only one bank server, because there is no location
 function to find which of many servers has a particular account.
*******************************************************************************/

#include    "isis.h"
#include "bank.h"
#include <string.h>

#ifndef	MACH
char *malloc();
#endif

typedef struct {
	char *name;
	int balance;			/* Balance after application of all committed transactions,
					   balance >= 0 */
	int cleared;			/* Guaranteed usable balance. balance will equal this if
					   all in-progress transactions which increase the balance
					   fail while all in-progress transactions which decrease
					   it succeed. Precisely: cleared = balance + sum
					   (trans.change for all trans in transactions such that
					   trans.name = account.name and trans.change < 0) */
} account;
qnode *accounts;			/* Keyed by account name. */
static adesc account_ad = { sizeof(account), sizeof(account), 8 };

typedef struct {
	x_id id;
	char *name;
	bool ok;			/* How we will vote in the prepare phase. */
	int change;			/* Effect of this transaction on this account will be:
					   account.balance += change */
} trans;
qnode *transactions;			/* Keyed by (transaction id) x (account name). */
static adesc trans_ad = { sizeof(trans), sizeof(trans), 8 };

/* For consistency we require that all account balances are non-negative.
   Consistency is maintained by ensuring for each account:
       account.cleared >= 0,
   If this cannot be maintained the offending transaction(s) is aborted.
   This combined with the mutual exclusion of each bank operation provides
   the necessary concurrency control.

   Accounts are created with zero balance when first referenced in an operation.
*/

#define trim(name) { \
    if (strlen(name) > max_account_name) { \
        name[max_account_name] = '\000';        \
    }                                           \
}

bool test = FALSE;
int port_nr = 0;			/* ISIS port number */
char branch_name[sizeof(BANK_SERVICE) + sizeof("-branch-99999")];
char log_file_name[sizeof(branch_name) + sizeof(".log")];
FILE *log_file;

static void log_prepare(), log_outcome();
int update();

/*******************************************************************************
 Account list management routines.
*******************************************************************************/

static qnode *
qu_find_string(qp, name)
	register qnode *qp;
	char *name;
{
	register qnode *np;

	for (np = qp->qu_next; np != qp; np = np->qu_next) {
		if (strcmp(np->qu_string, name) == 0) {
			return (np);
		}
	}
	return (NULLQP);
}

#define qu_find_account(qp, name) qu_find_string(qp, name)
#define qu_add_account(qp, name, acc) qu_add(qp, (int) name, (char *) acc, NULLROUTINE)

static account *
add_account(name)
	char *name;
{
	account *acc = (account *) mallocate(&account_ad);

	acc->name = name;
	acc->balance = 0;
	acc->cleared = 0;
	qu_add_account(accounts, name, acc);
	return (acc);
}

static account *
find_account(name)
	char *name;
{
	qnode *qp;

	if (qp = qu_find_account(accounts, name)) {
		return ((account *) (qp->qu_data));
	} else {
		return (add_account(name));
	}
}

static void				/* Currently not used. */
free_account(name)
	char *name;

  /* There must be no trans records for this account. */
{
	register qnode *qp;

	if (qp = qu_find_account(accounts, name)) {
		account *acc = (account *) (qp->qu_data);

		free(acc->name);
		mdeallocate(acc, &account_ad);
		qu_free(qp);
	}
}

/*******************************************************************************
 Transaction list management routines.
*******************************************************************************/

static qnode *
qu_find_trans(qp, id)
	x_id *id;
	register qnode *qp;
{
	register qnode *np;

	for (np = qp->qu_next; np != transactions; np = np->qu_next) {
		if (xid_cmp(&(np->qu_xid), id) == 0) {
			return (np);
		}
	}
	return (NULLQP);
}

static qnode *
qu_add_trans(qp, id, info)
	register qnode *qp;
	x_id *id;
	trans *info;

  /* add a node to qp, returns np */
{
	register qnode *np;

	qu_alloc1(np, 0, 0, NULLROUTINE);
	np->qu_xid = *id;
	np->qu_data = (char *) info;
	qu_append(qp, np);
	return (np);
}

static qnode *
first_trans(id)
	x_id *id;
{
	return (qu_find_trans(transactions, id));
}

trans *
this_trans(qp)
	register qnode *qp;
{
	return ((trans *) (qp ? qp->qu_data : 0));
}

static qnode *
next_trans(id, prev)
	x_id *id;
	qnode *prev;
{
	return (qu_find_trans(prev, id));
}

static trans *
add_trans(id, name)
	x_id *id;
	char *name;
{
	register trans *info;

	info = (trans *) mallocate(&trans_ad);
	info->id = *id;
	info->name = name;
	info->change = 0;
	info->ok = FALSE;	/* Until proven otherwise. */
	qu_add_trans(transactions, id, info);
	return (info);
}

static trans *
find_trans_for_account(id, name)
	x_id *id;
	char *name;
{
	qnode *qp;
	trans *info;

	for (qp = first_trans(id); info = this_trans(qp); qp = next_trans(id, qp)) {
		if (strcmp(info->name, name) == 0) {
			return (info);
		}
	}
	return (add_trans(id, name));
}

static void
qu_free_trans(qp)
	qnode *qp;
{
	/* Don't deallocate trans.name, its pointed to by the account record (and other trans
	   records maybe). */
	mdeallocate(qp->qu_data, &trans_ad);
	qu_free(qp);
}

static void
free_trans(id)
	x_id *id;

  /* If a trans struct for this id exists in xactions, remove it and reclaim its storage. */
{
	register qnode *qp;

	if (qp = qu_find_trans(transactions, id)) {
		qu_free_trans(qp);
	}
}

/*******************************************************************************
 Transaction prepare/commit routines.
*******************************************************************************/

bool
bank_prepare(id)
	x_id *id;
{
	qnode *qp;
	trans *info;

	for (qp = first_trans(id); info = this_trans(qp); qp = next_trans(id, qp)) {
		if (info->ok) {
			log_prepare(id, info->name, info->change);
		} else {
			return (FALSE);	/* Intentions we have written so far will get cleaned up by 
					   do_abort. */
		}
	}
	return (TRUE);
}

bool
complete_updates(id)
	x_id *id;
{
	bool at_least_one = FALSE;
	trans *info;
	qnode *qp = first_trans(id);

	while (info = this_trans(qp)) {
		qnode *lp = qp;
		account *acc = find_account(info->name);

		acc->balance += info->change;
		if (info->change >= 0) {
			acc->cleared += info->change;
		}
		qp = next_trans(id, qp);
		qu_free_trans(lp);
		at_least_one = TRUE;
	}
	return (at_least_one);
}

void
bank_commit(id)
	x_id *id;
{
	if (complete_updates(id)) {
		/* Only write a commit record if there was at least one prepare record. (There
		   won't be any for a read-only transaction.) */
		log_outcome(id, X_COMMIT);
	}
}

bool
forget_updates(id)
	x_id *id;
{
	bool at_least_one = FALSE;
	trans *info;
	qnode *qp = first_trans(id);

	while (info = this_trans(qp)) {
		qnode *lp = qp;
		account *acc = find_account(info->name);

		if (info->change < 0) {
			acc->cleared -= info->change;
		}
		qp = next_trans(id, qp);
		qu_free_trans(lp);
		at_least_one = TRUE;
	}
	return (at_least_one);
}

void
bank_abort(id)
	x_id *id;
{
	if (forget_updates(id)) {
		log_outcome(id, X_ABORT);
	}
}

void
join_transaction()
{
	if (x_term(branch_name, bank_prepare, bank_commit, bank_abort, "") < 0) {
		if (isis_errno = IE_PARTICIPANT) {
			/* Already a participant: must be a multi-part transaction. */
		} else {
			/* Some other error. */
			isis_perror("bank:x_term");
		}
	}

	/* Note that we don't supply any extra user data (arguments 5 etc.) to the x_term call. The 
	   transaction id is enough to identify the intention records in the bank_log upon
	   recovery.

	   As an alternative, we could have called x_term for every account that this transaction
	   touches, and supplied the intention record as user data here. In this case we could have 
	   dispensed with our own log entirely. However the participant name would have had to be
	   branch_name concatenated with account name, and a transaction would have to be
	   restricted to modifying a given account only once per transaction (not really a major
	   restriction). */
}

/*******************************************************************************
 Bank operations and message handlers.
*******************************************************************************/

void
handle_deposit(msg)
	message *msg;
{
	int success;
	char *name;
	int amount;

	msg_get(msg, "%+s %d", &name, &amount);
	trim(name);
	if (amount < 0) {
		success = bank_bad_argument;
	} else {
		join_transaction();
		success = update(x_getid(), name, amount);
	}
	reply(msg, "%d", success);
}

void
handle_withdraw(msg)
	message *msg;
{
	int success;
	char *name;
	int amount;

	msg_get(msg, "%+s %d", &name, &amount);
	trim(name);
	if (amount < 0) {
		success = bank_bad_argument;
	} else {
		join_transaction();
		success = update(x_getid(), name, -amount);
	}
	reply(msg, "%d", success);
}

int
update(id, name, change)
	x_id *id;
	char *name;
	int change;
{
	account *acc = find_account(name);
	trans *info = find_trans_for_account(id, name);

	int oldchange = info->change;

	if (test) {
		print("Update ");
		paddr(id);
		print(" %s %d\n", name, change);
	}
	/* Check legality. */
	if (change < 0) {
		if ((acc->balance + change) < 0) {
			return (bank_account_overdrawn);
		} else if ((acc->cleared + change) < 0) {
			return (bank_account_conflict);
		}
	}

	/* Undo effects of previous operations by this transaction on this account. */
	if (oldchange < 0) {
		acc->cleared -= oldchange;
	}
	/* Apply composition of previous operations and this operation. */
	change += oldchange;
	info->change = change;
	if (change < 0) {
		acc->cleared += change;
	}
	info->ok = TRUE;
	return (0);
}

void
handle_inquiry(msg)
	message *msg;
{
	char *name;
	account *acc;

	msg_get(msg, "%+s %d", &name);
	trim(name);
	join_transaction();
	if (test) {
		print("Inquiry %s\n", name);
	}
	acc = find_account(name);
	reply(msg, "%d %d", acc->balance, acc->cleared);
}

/*******************************************************************************
 Log file management.

 The log file contains three kinds of records:
 X_PREPARE transaction-id change account-name
 X_COMMIT  transaction-id
 X_ABORT   transaction-id

For a completed transaction there will be a sequence of prepare records, one for 
each account modified by the transaction, followed by either a commit or abort
record, depending upon the outcome of the transaction.

For an incomplete transaction no commit or abort record will exist in the log.
Upon recovery the eventual outcome of such transactions must be obtained
by calling the x_outcomes routine, and the commit or abort record appended to
the log. 

Once recovery is complete the correct state of the bank can be recreated
in primary memory by reading the log and applying the updates of all 
prepare records for committed transactions. This would be expensive if
the bank had been operating for a long time. In a real setting the bank state
would be checkpointed to disk, and prefix of the log deleted.

*******************************************************************************/

typedef struct {
	int entry_type;
	x_id id;
} outcome_rec;

typedef struct {
	int entry_type;
	x_id id;
	char name[max_account_name + 1];	/* Null terminated and <= max_account_name long */
	int change;
} prepare_rec;

static void
log_prepare(id, name, change)
	x_id *id;
	char *name;
	int change;
{
	prepare_rec *rec = (prepare_rec *) malloc(sizeof(prepare_rec));

	if (test) {
		print("Writing Prepare record {");
		paddr(id);
		print(", %s, %d}\n", name, change);
	}
	rec->entry_type = X_PREPARE;
	rec->id = *id;
	strcpy(rec->name, name);
	rec->change = change;
	fwrite(rec, sizeof(prepare_rec), 1, log_file);
	fflush(log_file);
}

static void
log_outcome(id, outcome)
	int outcome;
	x_id *id;
{
	outcome_rec rec;

	if (test) {
		if (outcome == X_COMMIT) {
			print("Writing Commit  ");
		} else if (outcome = X_ABORT) {
			print("Writing Abort   ");
		} else {
			print("Writing ??%d ", outcome);
		}
		print("record {");
		paddr(id);
		print("}\n");
	}

	rec.entry_type = outcome;
	rec.id = *id;
	fwrite(&rec, sizeof(rec), 1, log_file);
	fflush(log_file);
	/* Could be smarter about when to flush and not to flush the log file. */
}

static int
find_outcome(outcomes, id)
	x_list *outcomes;
	x_id *id;

  /* Search the list returned by x_outcomes for the outcome of a transaction. (Maybe this routine
     should be put into clib.) */
{
	int i;
	x_item *item;

	for (i = 0; i < outcomes->len; i++) {
		item = &(outcomes->items[i]);
		if (xid_cmp(&(item->id), id) == 0) {
			return (item->outcome);
		}
	}
	return (X_FINISHED);	/* Must have completely terminated. */
}

static void
do_recovery()
{
#   define absolute_seek 0
	x_list *outcomes;
	int n_read = 0;
	int n_recovered = 0;
	outcome_rec rec;
	qnode *qp;
	long last_rec;			/* Position of last good record in log file. */

	print("Reading from %s\n", log_file_name);
	outcomes = x_outcomes(branch_name);

	if (!outcomes) {
		isis_perror("bank.c:do_recovery");
		return;
	}

	/* Process the log, redoing all the operations it indicates. */
	last_rec = 0;
	rewind(log_file);
	while (fread(&rec, sizeof(rec), 1, log_file)) {
		struct {
			char name[max_account_name + 1];
			int change;
		} rest;
		int n;
		char *name;
		int entry_type = rec.entry_type;
		x_id *id;

		id = &rec.id;
		++n_read;

		switch (entry_type) {
		case X_PREPARE:
			if (!fread(&rest, sizeof(rest), 1, log_file)) {
				print("Truncated log file, ignoring last record\n");
				break;
			}
			if (test) {
				print("Prepare record {");
				paddr(id);
				print(", %s, %d}\n", rest.name, rest.change);
			}
			n = strlen(rest.name);
			name = (char *) malloc(n + 1);
			strcpy(name, rest.name);
			trim(name);
			if (update(id, name, rest.change) != 0) {
				panic("Error: update from log failed\n");
			}
			break;
		case X_COMMIT:
			if (test) {
				print("Commit  record {");
				paddr(id);
				print("}\n");
			}
			complete_updates(id);
			break;
		case X_ABORT:
			if (test) {
				print("Abort   record {");
				paddr(id);
				print("}\n");
			}
			forget_updates(id);
			break;
		}
		last_rec = ftell(log_file);
	}

	fseek(log_file, last_rec, absolute_seek);
	if (n_read != 0) {
		print("%d log records read\n", n_read);
	}

	/* The bank is now uptodate with the time of the crash, complete any incomplete
	   transactions. */
	while (qp = qu_head(transactions)) {
		/* Successively look at first element of transactions list. Each iteraction will
		   process and remove that element. */
		x_id *id;

		id = &(this_trans(qp)->id);
		switch (find_outcome(outcomes, id)) {
		case X_COMMIT:
			print("Committing ");
			paddr(id);
			print("\n");
			bank_commit(id);
			break;
		case X_ABORT:
			print("Aborting ");
			paddr(id);
			print("\n");
			bank_abort(id);
			break;
		case X_FINISHED:
			panic("Transaction should have finished!! ");
			paddr(id);
			print("\n");
			break;
		}
		++n_recovered;
	}

	if (n_recovered == 0) {
		print("Nothing to recover\n");
	} else {
		print("Recovery completed\n");
	}

	x_outcomes_flush(branch_name, outcomes);
}

/*******************************************************************************
 Server main line. 
*******************************************************************************/

void
bank_control()
{
	address *server;
	groupview *gview;

	isis_start_done();

	server = pg_join(BANK_SERVICE, 0);
	if (addr_isnull(server)) {
		isis_perror("bank:pg_join failed");
		exit(-1);
	}
	gview = pg_getview(server);
	if (!gview) {
		panic("Unable to get groupview");
	} else if (gview->gv_nmemb > 1) {
		panic("More than one bank_server running: committing suicide\n");
	}

	if (!(log_file = fopen(log_file_name, "a+"))) {
		perror("can't open log file");
		exit(1);
	}
	do_recovery(branch_name);
}

void
main(argc, argv)
	int argc;
	char **argv;
{
	print("Bank Service");
	/* Read in command line arguments: <port-nr> Isis port number (optional). -t Enable debug
	   output. */

	while (argc-- > 1) {
		++argv;
		switch (**argv) {
		case '-':
			switch ((*argv)[1]) {
			case 't':
				test = TRUE;
				continue;
			default:
				panic("Bad argument: <%s>\n", *argv);
			}
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			port_nr = atoi(*argv);
			continue;
		      badarg:
			panic("Bad argument: <%s>\n", *argv);
		}
	}

/* Initialize and run ISIS */
	isis_init(port_nr);
	sprintf(branch_name, "%s-branch-%d", BANK_SERVICE, my_site_no);
	sprintf(log_file_name, "%s.log", branch_name);
	print(" at branch %d\n", my_site_no);

	accounts = qu_null();
	transactions = qu_null();

	isis_entry(BANK_DEPOSIT, handle_deposit, "handle_deposit");
	isis_entry(BANK_WITHDRAW, handle_withdraw, "handle_withdraw");
	isis_entry(BANK_INQUIRY, handle_inquiry, "handle_inquiry");
	isis_mainloop(bank_control, NULLARG);
}
