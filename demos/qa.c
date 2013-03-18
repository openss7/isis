/*  $RCSfile: qa.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:37 $  */
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
 *      Front end program for playing twenty questions
 */

#include "isis.h"
#include "twenty.h"

int verbose, use_bypass, use_pgclient;
int CLIENT_PORT;

void
main(argc, argv)
	int argc;
	char **argv;
{
	void ask_questions();

	while (argc-- > 1)
		switch (**++argv) {
		case '-':
			switch (*++*argv) {
			case 'v':
				++verbose;
				continue;
			default:
				printf("-%c: unknown option\n", **argv);
				continue;
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

	/* Connect to ISIS */
	isis_entry(TWENTY_QUERY, (vfunc *) nullreply, "query");
	isis_entry(TWENTY_HELLO, (vfunc *) nullreply, "hello");
	isis_entry(TWENTY_IGNORE, (vfunc *) nullreply, "ignore");

	isis_init(CLIENT_PORT);

	isis_mainloop(ask_questions, NULLARG);
}

int delay[]
    = {
	2, 2, 2, 2, 2, -1
};

void
ask_questions()
{
	int cat, class;
	char string[120];
	register char *sp;
	register c;
	address *gid;

	isis_start_done();
	gid = pg_lookup("twenty");
	if (addr_isnull(gid))
		panic("twenty-questions asker -- can't connect to database program");
	printf("Welcome to... twenty questions!\nQuery categories are... ");
	begin {
		register n;
		static nretries;
		char cnames[NFIELDS][STRLEN];

		bzero(cnames, sizeof(cnames));
		while (cbcast(gid, TWENTY_HELLO, "", 1, "%C", cnames, (int *) 0) != 1)
			if (++nretries > 2)
				panic("Twenty questions service has been shut down!");
		for (n = 0; n < NFIELDS; n++)
			if (*cnames[n])
				print("%s ", cnames[n]);
	}
	print("\n\nEnter a random number [0]: ");
	sp = string;
	while ((c = getchar()) != '\n')
		*sp++ = c;
	*sp = 0;
	cat = atoi(string);
	print("Use bypass communication? [n] ");
	sp = string;
	while ((c = getchar()) != '\n')
		*sp++ = c;
	*sp = 0;
	if (string[0] == 'y')
		++use_bypass;
	else {
		print("Use pg_client communication accelerator? [y] ");
		sp = string;
		while ((c = getchar()) != '\n')
			*sp++ = c;
		*sp = 0;
		if (string[0] != 'n')
			++use_pgclient;
	}
	if (use_bypass)
		gid = pg_join("twenty_qa", 0);
	else if (use_pgclient)
		pg_client(gid, "qa");
	printf("Enter `* query' or `query'...\n");
	forever {
		int nrep = 1;

		print("? ");
		c = getchar();
		if (c <= 0) {
			print("... exiting (but leaving twenty running)\n");
			exit(0);
		}
		if (c == '\n')
			continue;
		if (c == '*') {
			class = 'H';
			c = getchar();
		} else if (c == '@') {
			class = 'H';
			c = getchar();
			nrep = 50;
		} else
			class = 'V';
		sp = string;
		do {
			if (c != ' ' && c != '\t')
				*sp++ = c;
		}
		while ((c = getchar()) != '\n');
		*sp = 0;
		if (sp == string)
			continue;
		while (nrep--) {
			register nwant, nrep, n;
			static int who[NMEMBER + 1];
			static int rep[NMEMBER + 1];

			nwant = (class == 'H') ? NMEMBER : 1;
			n = 0;
			while ((nrep = cbcast(gid, TWENTY_QUERY, "%d,%d,%s", cat, class, string,
					      nwant, "%d,%d", who, rep)) != nwant) {
				if (nrep == 0)
					panic("twenty questions service has been shut down");
				if (n == 0)
					print
					    ("twenty questions service is restarting... please be patient\n");
				else if (delay[n] < 0)
					break;
				sleep(delay[n++]);
				print("... retrying\n");
			}
			if (nrep != nwant)
				panic("twenty questions service has failed!");
			for (n = 0; n < nrep; n++) {
				register i = 0;

				if (class == 'H')
					while (who[i] != n && i < nrep)
						i++;
				switch (rep[i]) {
				case 'F':
					print("<field unknown> ");
					continue;
				case 'Y':
					print("yes ");
					continue;
				case 'N':
					print("no ");
					continue;
				case '?':
					print("sometimes ");
					continue;
				default:
					print("<something went wrong>");
					continue;
				}
			}
			print("\n");
		}
	}
}
