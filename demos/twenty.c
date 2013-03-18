/*  $RCSfile: twenty.c,v $ $Revision: 2.1 $ $Date: 90/05/16 15:05:05 $  */
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
 */
/*
 *      A fancier twenty questions program
 */

#include "isis.h"
#include "twenty.h"

void main_task(), query(), hello(), monitor_group(), work_partition();
void start_one();
int compare();
void _exit();
address *gid;

int rexec_in_progress;
int my_number;
int CLIENT_PORT;

char db[NLINES][NFIELDS][STRLEN];
char cnames[NCAT][STRLEN];
int nfields, nlines, ncat;
int client_trace;

void
ignore()
{
}

void
main(argc, argv)
	int argc;
	char **argv;
{
	while (argc-- > 1) {
		switch (**++argv) {
		default:
		      badarg:
			panic("Bad argument: <%s>\n", *argv);

		case '-':
			switch ((*argv)[1]) {
			case 't':
				++client_trace;
				continue;
			default:
				goto badarg;
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
			CLIENT_PORT = atoi(*argv);
			continue;
		}
	}

	/* Connect to ISIS, then fork off appropriate procedure */
	isis_task(main_task, "main_task");
	isis_task(monitor_group, "monitor_group");

	isis_entry(TWENTY_QUERY, query, "query");
	isis_entry(TWENTY_HELLO, hello, "hello");
	isis_entry(TWENTY_IGNORE, ignore, "ignore");

	isis_init(CLIENT_PORT);
	/* This is a test to confirm that isis_accept_events works: */
	t_fork(main_task, 0);
	forever isis_accept_events(ISIS_BLOCK);
}

groupview cur_view;

/*
 * Monitor changes to view 
 * Everyone sees the same view, so the coordinator can be selected
 * as the first (==oldest) listed member.  The coordinator
 * does restarts as needed
 */
void
monitor_group(gv)
	groupview *gv;
{
	gid = &gv->gv_gaddr;
	cur_view = *gv;
	work_partition(gv);
	/* Coordinator is the oldest member of the group */
	if (addr_ismine(gv->gv_members)) {
		print("NEW VIEW: %d.%d = ", VMM(gv->gv_viewid));
		paddrs(gv->gv_members);
		if (gv->gv_nclient) {
			print("[");
			paddrs(gv->gv_clients);
			print("]");
		}
		if (!addr_isnull(&gv->gv_joined)) {
			print(", joined: ");
			paddr(&gv->gv_joined);
		}
		if (!addr_isnull(&gv->gv_departed)) {
			print(", departed: ");
			paddr(&gv->gv_departed);
		}
		print("\n");
	}
	if (rexec_in_progress && gv->gv_nmemb > rexec_in_progress)
		rexec_in_progress = 0;
	else if (rexec_in_progress)
		rexec_in_progress = gv->gv_nmemb;
	if (addr_ismine(gv->gv_members)) {
		if (rexec_in_progress == 0 && gv->gv_nmemb < NMEMBER + NSTANDBY) {
			rexec_in_progress = gv->gv_nmemb;
			start_one();
		}
	}
}

#define TWENTY  "twenty"

char *jargs[]
    = {
	"twenty", 0, 0
};

void
start_one()
{
	register count = 0;
	static sno = 1;
	static site_id sid[2];
	address pname[2];
	register site_id *sp;
	register nsites;
	sview *v, *site_getview();
	char client[30];

      again:
	v = site_getview();
	for (sp = v->sv_slist; *sp; sp++)
		continue;
	nsites = sp - v->sv_slist;
	if (sno >= nsites)
		sno = 0;
	*sid = v->sv_slist[sno];
	sprintf(client, "%d", CLIENT_PORT);
	jargs[1] = client;
	++sno;
	if (isis_rexec(1, gid, sid, TWENTY, jargs, (char **) 0,
		       "", "", pname) != 1 || pname->addr_site == 0) {
		print("Rexec 'twenty' failed at site %d/%d\n", SITE_NO(*sid), SITE_INCARN(*sid));
		if (++count < 2)
			goto again;
		panic("Twenty giving up the ghost");
	}
}

void
init_proc()
{
	register FILE *file;
	register c, n;

	if ((file = fopen("questions.dat", "r")) == 0) {
		perror("questions.dat");
		panic("can't read the questions database");
	}
	do {
		register char *fp = db[0][nfields++];

		while ((c = fgetc(file)) > 0 && c != '\n' && c != '\t')
			*fp++ = c;
		*fp = 0;
	}
	while (c != '\n' && c > 0);
	nlines = 1;
	do {
		for (n = 0; n < nfields; n++) {
			register char *sp = db[nlines][n];

			while ((c = fgetc(file)) != '\n' && c != '\t' && c > 0)
				*sp++ = c;
			*sp = 0;
		}
		if (*db[nlines][0])
			++nlines;
	}
	while (c > 0);
}

void
send_db(locator)
	int locator;
{
	xfer_out(0, "%C", db, NFIELDS * STRLEN * nlines);
}

void
recv_db(locator, msg)
	int locator;
	register message *msg;
{
	msg_get(msg, "%C", db, (int *) 0);
}

void
hello(mp)
	register message *mp;
{
	if (my_number)
		nullreply(mp);
	reply(mp, "%C", db, NFIELDS * STRLEN);
}

int
client_check(credentials)
	char *credentials;
{
	if (strcmp(credentials, "qa") == 0)
		return (0);
	print("TWENTY QUESTIONS CREDENTIALS CHECK FAILED FOR <%s>\n", credentials);
	return (-1);
}

/* Startup of a sub-program */
void
main_task()
{
	register c, n;
	int gotline();

	pg_join("twenty_qa", 0);
	gid = pg_join("twenty",
		      PG_INIT, init_proc,
		      PG_MONITOR, monitor_group, 0,
		      PG_XFER, 0, send_db, recv_db, PG_CLIENT_AUTHEN, client_check, 0);
	if (addr_isnull(gid)) {
		print("twenty: pg_join failed.\n");
		exit(0);
	}
	my_number = -1;
	for (nfields = 0; db[0][nfields][0]; nfields++)
		continue;
	for (nlines = 1; db[nlines][0][0]; nlines++)
		continue;
	ncat = 0;
	c = 0;
	for (n = 1; n < nlines; n++)
		if (strcmp(db[n][0], db[c][0])) {
			strcpy(cnames[ncat++], db[n][0]);
			c = n;
		}
	isis_start_done();
}

/*
 * Each time the group view changes, divide up the work.
 */
void
work_partition(gv)
	register groupview *gv;
{
	register address *ap;

	for (ap = gv->gv_members; !addr_isnull(ap); ap++)
		if (addr_ismine(ap)) {
			my_number = ap - gv->gv_members;
			if (my_number >= NMEMBER)
				/* Standby's get negative numbers */
				my_number = NMEMBER - my_number - 1;
			return;
		}
	sleep(300);
	panic("work_partition -- I'm not in the member list (never happens)");
}

/*
 *      
 */
void
query(mp)
	register message *mp;
{
	int cat, class, f, n, comp;
	char *query, *heading;

	msg_get(mp, "%d,%d,%-s", &cat, &class, &query);
	if (strcmp(query, "bye") == 0)
		exit(0);
	if (strcmp(query, "exit") == 0)
		exit(0);
	if (strcmp(query, "quit") == 0)
		exit(0);
	if (strcmp(query, "shutdown") == 0)
		exit(0);
	cat %= ncat;
	heading = query;
	while (*query != '=' && *query != '>' && *query != '<' && *query)
		++query;
	comp = *query;
	*query++ = 0;
	for (f = 0; f < nfields; f++)
		if (strcmp(db[0][f], heading) == 0)
			break;
	/* In H mode, everyone answers.  In V mode, only one answers */
	switch (class) {
		char *answ;
		int count;

	case 'H':
		if (my_number < 0) {
			nullreply(mp);
			break;
		}
		answ = 0;
		count = 0;
		if (f == nfields)
			answ = "F";
		else
			for (n = 1; n < nlines; n++) {
				if (strcmp(db[n][0], cnames[cat]))
					continue;
				else if (count++ % NMEMBER == my_number) {
					if (compare(comp, db[n][f], query) == 0) {
						if (answ && *answ != 'Y')
							answ = "?";
						else
							answ = "Y";
					} else {
						if (answ && *answ != 'N')
							answ = "?";
						else
							answ = "N";
					}
				}
			}
		if (answ == 0)
			answ = "*";
		reply(mp, "%d,%d", my_number, *answ);
		break;

	case 'V':
		if ((my_number < 0) || (f % NMEMBER != my_number)) {
			nullreply(mp);
			break;
		}
		answ = 0;
		if (f == nfields)
			answ = "F";
		else
			for (n = 1; n < nlines; n++)
				if (strcmp(db[n][0], cnames[cat]))
					continue;
				else if (compare(comp, db[n][f], query) == 0) {
					if (answ && *answ != 'Y')
						answ = "?";
					else
						answ = "Y";
				} else {
					if (answ && *answ != 'N')
						answ = "?";
					else
						answ = "N";
				}
		if (answ == 0)
			answ = "*";
		reply(mp, "%d,%d", my_number, *answ);
		break;

	default:
		print("Unknown query class: '%c'\n", class);
		reply(mp, "%d,%d", my_number, '*');
		break;
	}
}

int
compare(op, s1, s2)
	int op;
	char *s1, *s2;
{
	register n1, n2;

	if (op != '<' && op != '>')
		return (strcmp(s1, s2));
	n1 = atoi(s1);
	n2 = atoi(s2);
	if (op == '<')
		return (n1 >= n2);
	return (n1 <= n2);
}
