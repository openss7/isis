/*
 *	Originally written by Frank Schmuck
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
 */
/*********************************************************************
*
*  cmd -- send commands to isis pgroups 
*
*********************************************************************/

/*  changes for new ISIS interface:
 *  12-14-87 Frank: changed all bcast's and msg_xxx to new version,
 *      except for a msg_addfield in CmdSend.
 *      (there are no reply's in cmd.c)
 */

char cmd_rcsid[] =
    "$Revision: 2.28 $$Date: 90/08/14 16:08:53 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/util/RCS/cmd.c,v $";

#include <stdio.h>
#include <sys/types.h>
#if(HPUX)
#include <time.h>
#else
#include <sys/time.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <sys/wait.h>

#include "isis.h"

#if(HPUX)
extern char *strchr();

#define index(s, c)     strchr(s, c)
#else
extern char *index();
#endif

extern int isis_socket;
extern sview *site_getview();
extern gl_desc *pg_list();
extern char *re_comp();
extern isis_dontlogdumps;

#define fd_stdin  0
#define MAXANSW  128

static address *gid;			/* process group address */
static groupview *gv;			/* process group view */
bool optv = FALSE;			/* verbose option */
static int stat_cmd = 0;		/* status: no of active cmd_tasks */
static address adrl[MAX_PROCS + 1];	/* address list for send command */
static message *rmsg[MAX_PROCS];	/* pointers to reply messages */

/*********************************************************************
*
*  misc routines 
*
*********************************************************************/

MakeAlist(s, g, p)
	char *s, *g, *p;
{
	int site, proc;

	if (g && strcmp(g, "*") == 0)
		g = NULL;
    /*** convert site number and process id ***/
	site = (s == NULL) ? -1 : (*s == '.') ? my_site_no : atoi(s);
	if (p != NULL) {
		proc = atoi(p);
	} else if (g != NULL) {
		if (strcmp(g, "protocols") == 0) {
			proc = PROTOCOLS;
			g = NULL;
		} else if (strcmp(g, "protos") == 0) {
			proc = PROTOCOLS;
			g = NULL;
		} else if (strcmp(g, "rexec") == 0) {
			proc = REXEC;
			g = NULL;
		} else if (strcmp(g, "rmgr") == 0) {
			proc = RMGR;
			g = NULL;
		} else if (strcmp(g, "isis") == 0) {
			proc = ISIS;
		} else if (strcmp(g, "news") == 0) {
			proc = NEWS;
			g = NULL;
		} else if (strcmp(g, "xmgr") == 0) {
			proc = XMGR;
			g = NULL;
		} else {
			proc = 0;
		}
	} else {
		proc = 0;
	}
	if (proc < 0) {
		if (g != NULL) {
			printf(" ***  bad address\n");
			return 0;
		}
		return make_system_alist(site, proc);
	} else {
		return make_group_alist(site, g, proc);
	}
}

make_system_alist(site, proc)
	int site, proc;
{
	site_id *slist;
	int i;

	slist = site_getview()->sv_slist;
	if (site >= 0) {
	/*** send only to one site ***/
		adrl[0] = NULLADDRESS;
		for (i = 0; slist[i] != 0; i++) {
			if (SITE_NO(slist[i]) == site) {
				adrl[0].addr_site = SITE_NO(slist[i]);
				adrl[0].addr_incarn = SITE_INCARN(slist[i]);
				adrl[0].addr_process = proc;
				adrl[0].addr_entry = MSG_COMMAND;
				adrl[0].addr_portno = 0;
				adrl[1] = NULLADDRESS;
				break;
			}
		}
	} else {
	/*** send to all sites ***/
		for (i = 0; slist[i] != 0; i++) {
			adrl[i].addr_site = SITE_NO(slist[i]);
			adrl[i].addr_incarn = SITE_INCARN(slist[i]);
			adrl[i].addr_process = proc;
			adrl[i].addr_portno = 0;
			adrl[i].addr_entry = MSG_COMMAND;
		}
		adrl[i] = NULLADDRESS;
	}
	return (int) adrl;
}

make_group_alist(site, g, proc)
	int site, proc;
	char *g;
{
	address *ap1, *ap2;
	int i;

    /*** get group ***/
	if (g != NULL) {
		if (optv)
			printf("  calling pg_lookup(\"%s\");\n", g);
		gid = pg_lookup(g);
		if (gid->addr_site == 0) {
			printf(" ***  group not known: %s\n", g);
			return 0;
		}
		if (optv)
			printf("  calling pg_getview(gid);\n");
		if ((gv = pg_getview(gid)) == NULL) {
			printf(" ***  pg_getview failed: %s\n", g);
			return 0;
		}
	} else if (gid->addr_site != 0) {
		if ((gv = pg_getview(gid)) == NULL) {
			printf(" ***  pg_getview failed\n");
			return 0;
		}
	} else {
		printf(" ***  bad address\n");
		return 0;
	}
    /*** compile address list ***/
	if (site >= 0 || proc > 0) {
		ap1 = adrl;
		for (ap2 = gv->gv_members; !aptr_isnull(ap2); ap2++) {
			if ((site < 0) || (ap2->addr_site == site)
			    && (proc <= 0) || (ap2->addr_process == proc)) {
				*ap1 = *ap2;
				(ap1++)->addr_entry = MSG_COMMAND;
			}
		}
		*ap1 = NULLADDRESS;
	} else {
		adrl[0] = *gid;
		adrl[0].addr_entry = MSG_COMMAND;
		adrl[1] = NULLADDRESS;
	}
	return (int) adrl;
}

char *help_help[] = {
	"  ------- command ------ ---------------- description ---------------",
	"  SItes                  Print the current site view.",
	"  List  <scope>          List all known process groups within a scope.",
	"  Group <group-name>     Print a process group view.",
	"  Send  <address> <arg0> <arg1> ... <argn>",
	"                         Send a command message to a set of processes.",
	"  Dump                   Dump the state of the protos process.",
	"  Pr_Dump                Dump the protos process to its log file",
	"  SNapshot               Dump the state of ALL processes in the system",
	"  Rescan                 Cause all ISIS systems to rescan their sites files",
	"  ShutDown               Cause the protocols process to shutdown.",
	"  Help                   Print a this command list.",
	"  Help <command>         Print more information about a command.",
	"  +v, -v                 Set (reset) verbose option.",
	"  STatus                 Print status of cmd program.",
	"  Quit                   Quit this program.",
	NULL
};

char *help_send[] = {
	"SYNTAX:  send  <address> <arg0> <arg1> ... <argn>",
	"",
	"DESCRIPTION:",
	"  Send puts the given arguments into a messages and broadcasts the message",
	"  to the the set of proceses denoted by <address> (entry MSG_COMMAND, see",
	"  \"cl_cmd.h\").  It waits for replies and prints them.",
	"  Type 'help address' for information on how to specify addresses.",
	NULL
};

char *help_addr[] = {
	"  Adresses may be specified in one of the following forms:",
	"",
	"  <site>:<group>:<process>",
	"      denotes the member of <group> at <site> with pid <process>.",
	"  <site>:<group>",
	"      denotes all members of <group> at <site>.",
	"  <group>",
	"      denotes all members of <group>.",
	"",
	"  In all of the above forms <group> may be specified as '*' (or left out) in",
	"  which case the most recently referenced group (in a 'group' or 'send' command)",
	"  is used.",
	"      Isis system processes may be addressed by leaving out <group> and",
	"  specifying <process> as -1, -2, ...  (the symbolic names 'protos', 'rexec'",
	"  'rmgr', 'isis', 'news', and 'xmgr' are also recognized).",
	NULL
};

char *help_list[] = {
	"SYNTAX:  list",
	"         list  @<scope>",
	"         list  @*",
	"",
	"         list  <group-name>",
	"         list  @<scope>:<group-name>",
	"         list  @*:<group-name>",
	"",
	"DESCRIPTION:",
	"  List prints all process groups known within scopes to which this site belongs",
	"  (list), known within a specified scope (list @<scope>), or known throughout ",
	"  the system (list @*).",
	"",
	"  If a group name is specified only groups with that name are printed.  The",
	"  name may contain the wild card characters '*', '?', and '[...]' which are",
	"  interpreted in the same way as file name substitution in csh.",
	NULL
};

char *help_sites[] = {
	"SYNTAX:   sites",
	"",
	"DESCRIPTION:",
	"  Sites calls the routine sv_getview and prints the returned view",
	NULL
};

char *help_group[] = {
	"SYNTAX:    group <group-name>",
	"",
	"DESCRIPTION:",
	"  Group calls the routine pg_getview and prints the returned view",
	NULL
};

char *help_dump[] = {
	"SYNTAX:    dump",
	"",
	"DESCRIPTION:",
	"  Dump causes protos at the site where the command is executed to",
	"  produce a formatted dump of its internal state in the file",
	"  /tmp/protos.dump, and then prints the contents of this file.",
	NULL
};

char *help_prdump[] = {
	"SYNTAX:    pr_dump",
	"",
	"DESCRIPTION:",
	"  Pr_dump causes protos at the site where the command is executed to",
	"  produce a formatted dump of its internal state, appending this to",
	"  the log file for this site (<siteno>.log in <isis_dir>.)  Nothing is",
	"  printed on the console.",
	NULL
};

char *help_snap[] = {
	"SYNTAX:    snapshot",
	"",
	"DESCRIPTION:",
	"  Snapshot causes all the isis sites in the system and all the client",
	"  programs on those sites to simultaneously create log files showing their",
	"  internal states.  This should only be used by a system adminstrator",
	NULL
};

char *help_shutdown[] = {
	"SYNTAX:    shutdown",
	"",
	"DESCRIPTION:",
	"  Shutdown causes ISIS at this site to shut itself down.  The command",
	"  only be used by the system administrator.",
	NULL
};

char *help_rescan[] = {
	"SYNTAX:    rescan",
	"",
	"DESCRIPTION:",
	"  Rescan causes all ISIS processes to reread the sites file after it",
	"  has been changed to add a new site, delete an old one, etc.",
	"  All sites file should be changed before this command is issued.",
	NULL
};

phelp(text)
	char **text;
{
	while (*text != NULL) {
		printf("%s\n", *text++);
	}
}

int
pattern_match(e, s)
	register char *e, *s;
{
	char c;

	while (*s) {
		switch (c = *e++) {

		case '?':
			break;

		case '*':
			while (!pattern_match(e, s))
				if (*s++ == '\0')
					return 0;
			return 1;

		case '[':
			while (c = *e++) {
				if (c == ']')
					return 0;
				if (c == '\\')
					if ((c = *e++) == 0)
						return 0;
				if (c == *s)
					break;
				if (*e == '-') {
					if (*++e == '\\')
						e++;
					if (*e == 0)
						return 0;
					if (*s <= *e++ && c <= *s)
						break;
				}
			}
			do {
				if (c == '-')
					c = *e++;
				if (c == '\\')
					c = *e++;
				if (c == 0)
					return 0;
			} while ((c = *e++) != ']');
			break;

		case '\\':
			c = *e++;
		default:
			if (c != *s)
				return 0;
		}
		s++;
	}
	while (*e == '*')
		e++;
	return (*e == 0);
}

/*********************************************************************
*
*  command handling task 
*
*********************************************************************/

void
cmd_task(argv)
	char *argv[];
{
	char c;

	if (argv == NULL) {
	/*** check for ^D ***/
		if ((c = getchar()) == EOF) {
			putchar('\n');
			exit(0);
		}
		ungetc(c, stdin);
	}
	stat_cmd++;
	cmdl_reset(argv);
	cmdy_reset();
	yyparse();
	stat_cmd--;
	if (argv == NULL) {
		/* Prompt for next command. */
		fputs("cmd> ", stdout);
		fflush(stdout);
	} else {
		exit(0);
	}
}

CmdSites()
{
	sview *sv;
	site_id *sp;

	sv = site_getview();
	printf(" ***  viewid = %d/%x\n", sv->sv_viewid & 0xFF, sv->sv_viewid >> 8);
	for (sp = sv->sv_slist; *sp; sp++) {
		printf("      %-30s  [site_no %d  site_incarn %d]\n",
		       site_names[SITE_NO(*sp)], SITE_NO(*sp), SITE_INCARN(*sp));
	}
}

CmdList(scope, group)
	char *scope, *group;
{
	char buf[64], *arg, *emsg;
	gl_desc *glp, *tlp;
	char *pattern = NULL;
	int n, m, s;
	address *gid;

	if (group && strpbrk(group, "?*[")) {
		pattern = group;
		group = NULL;
	}
	if (scope) {
		if (group)
			sprintf(arg = buf, "@%s:%s", scope, group);
		else
			sprintf(arg = buf, "@%s", scope);
	} else {
		arg = group ? group : "";
	}
	if (optv)
		printf("  calling pg_list(\"%s\");\n", arg);
	glp = tlp = pg_list(arg);
	for (n = 0; glp->gl_name[0]; glp++) {
		if (pattern && pattern_match(pattern, glp->gl_name) != 1)
			continue;
		if (n++ == 0)
			printf("  %-15s %-13s residing at sites ...\n", "name", "address");
		gid = &glp->gl_addr;
		sprintf(buf, "%d/%d/%d", gid->addr_site, gid->addr_incarn, gid->addr_groupid);
		printf("  %-15s %-13s", glp->gl_name, buf);
		for (s = m = 0; s < MAX_SITES; s++) {
			if (bit(&glp->gl_sites, s))
				printf("%c%d", m++ ? ',' : ' ', s);
		}
		printf("\n");
	}
	if (n == 0)
		printf(" ***  no match\n");
	free((char *) tlp);
}

CmdGroup(group)
	char *group;
{
	address *ap1, *ap2;

	if (strcmp(group, "*") != 0) {
		if (optv)
			printf("  calling pg_lookup(\"%s\");\n", group);
		gid = pg_lookup(group);
	}
	if (gid->addr_site == 0) {
		printf(" ***  group not known\n");
	} else {
		printf(" ***  gid =     [site %d / incarn %d : gid %d]\n",
		       gid->addr_site, gid->addr_incarn, gid->addr_groupid);
		if (optv)
			printf("  calling pg_getview(gid);\n");
		if ((gv = pg_getview(gid)) != NULL) {
			printf("      view =    [\"%s\"  ", gv->gv_name);
			printf("incarn %d  viewid %d.%d  nmemb %d  nclient %d]\n",
			       gv->gv_incarn, VMM(gv->gv_viewid), gv->gv_nmemb, gv->gv_nclient);
			printf("      members = ");
			paddrs(gv->gv_members);
			printf("\n");
		} else {
			printf("      view =  (no view available)\n");
		}
	}
}

CmdSend(alist, argv)
	address alist[];
	char *argv[];
{
	message *mp;
	int i, n;
	address *ap1, *ap2;
	char *answer;
	address *from;

	if (alist == NULL || argv == NULL)
		return;

    /*** fill out a command message ***/
	mp = msg_newmsg();
	for (i = 0; argv[i] != NULL; i++) {
		msg_addfield(mp, FLD_ARGV, argv[i], FTYPE_CHAR, strlen(argv[i]) + 1);
	}
    /*** broadcast the message to the group ***/
	if (optv) {
		printf("  cbcast to [ ");
		for (ap1 = alist; !aptr_isnull(ap1); ap1++) {
			if (ap1 > adrl)
				printf(", ");
			printf("%d/%d:%d", ap1->addr_site, ap1->addr_incarn,
			       addr_ispid(ap1) ? ap1->addr_process : ap1->addr_groupid);
		}
		printf(" ]\n");
	}
	n = cbcast_l("lm", alist, mp, ALL, rmsg);
	msg_delete(mp);
	if (optv)
		printf("  got %d answer(s)\n", n);
	for (i = 0; i < n; i++) {
		msg_get(rmsg[i], "%-s", &answer);
		from = msg_getsender(rmsg[i]);
		printf(" %d/%d:%d  %s",
		       from->addr_site, from->addr_incarn, from->addr_process, answer);
		msg_delete(rmsg[i]);
	}
}

char *MORE[] = {
	"/usr/ucb/more", "/bin/more", "/usr/bin/more", "/bin/cat", "/usr/bin/cat", 0
};

char *ARGP[] = {
	"more", "/tmp/protos.dump", 0
};

char **more;

CmdDump()
{
	print("<< ISIS SYSTEM DUMP >>\n");
	unlink("/tmp/protos.dump");
	pr_makedump("/tmp/protos.dump");
	if (more == 0) {
		more = MORE;
		while (notrunable(*more) && *more)
			++more;
		if (*more == 0)
			print("Can't find `more' -- dump in /tmp/protos.dump\n");
	}
	if (*more)
		isis_fork_execve(*more, ARGP, 0, -1);
#   ifndef FORK_BROKEN
	wait(0);
#   endif
}

notrunable(prog)
	char *prog;
{
	struct stat s;
	register mode;

	if (prog == 0)
		return (EFAULT);
	if (stat(prog, &s) == -1)
		return (errno);
	mode = s.st_mode;
	if ((mode & S_IFMT) != S_IFREG)
		return (EACCES);
	if ((mode & S_ISUID) && (mode & S_IEXEC) == 0)
		return (EACCES);
	if ((mode & S_ISGID) && (mode & (S_IEXEC >> 3)) == 0)
		return (EACCES);
	if ((mode & (S_IEXEC >> 6)) == 0)
		return (EACCES);
	return (0);
}

CmdPrdump()
{
	pr_dump(0);
}

CmdPrsnap()
{
	pr_dump(-1);
}

CmdRescan()
{
	pr_rescan();
}

CmdShutdown()
{
	pr_shutdown();
}

CmdHelp(h)
	char *h;
{
	if (h == NULL) {
		phelp(help_help);
	} else if (strcmp(h, "send") == 0) {
		phelp(help_send);
	} else if (strcmp(h, "address") == 0) {
		phelp(help_addr);
	} else if (strcmp(h, "list") == 0) {
		phelp(help_list);
	} else if (strcmp(h, "sites") == 0) {
		phelp(help_sites);
	} else if (strcmp(h, "group") == 0) {
		phelp(help_group);
	} else if (strcmp(h, "dump") == 0) {
		phelp(help_dump);
	} else if (strcmp(h, "pr_dump") == 0) {
		phelp(help_prdump);
	} else if (strcmp(h, "snapshot") == 0) {
		phelp(help_snap);
	} else if (strcmp(h, "shutdown") == 0) {
		phelp(help_shutdown);
	} else if (strcmp(h, "rescan") == 0) {
		phelp(help_rescan);
	} else {
		printf(" ***  Can't find <%s>.  Help info. available for:\n", h);
		printf
		    ("          send, address, list, sites, group, dump, pr_dump, snapshot, shutdown\n",
		     h);
	}
}

CmdStatus()
{
	printf(" cmd:  my_site_no: %d,  active cmd_task's: %d;  %s\n",
	       my_site_no, stat_cmd, optv ? "+v" : "");
}

CmdQuit()
{
	exit(0);
}

/*********************************************************************
*
*  main 
*
*********************************************************************/

static int
getport()
{
	FILE *sfile;
	char host[64];
	char str[64], *s, c;
	int cport;

	gethostname(host, 64);
	if ((s = index(host, '.')) != NULL)
		*s = '\0';
	if ((sfile = fopen("sites", "r")) == NULL)
		return 0;
	while (fscanf(sfile, " %*c %*d:%*d,%d,%*d %s", &cport, str) == 2) {
		if ((s = index(str, '.')) != NULL)
			*s = '\0';
		if (strcmp(host, str) == 0) {
			fclose(sfile);
			return cport;
		}
		do
			c = getc(sfile);
		while (c != '\n' && c != EOF);
	}
	fclose(sfile);
	return 0;
}

main(argc, argv)
	int argc;
	char *argv[];
{
	int client_port;		/* port number for talking to isis */
	bool single_cmd = FALSE;
	bool no_isis = FALSE;
	int mask;
	int rmask;
	int nfd;
	int i;

	client_port = 0;
	isis_dontlogdumps = 1;
    /*** read command line arguments ***/
	i = 0;
	while (argv[++i]) {
		if (*argv[i] >= '0' && *argv[i] <= '9') {
			client_port = atoi(argv[i]);
		} else if (strcmp(argv[i], "+v") == 0) {
			optv = TRUE;
			break;
		} else if (strcmp(argv[i], "-i") == 0) {
			no_isis = TRUE;
		} else if (strcmp(argv[i], "-c") == 0) {
			single_cmd = TRUE;
			argv = &argv[i + 1];
			break;
		} else {
			single_cmd = TRUE;
			argv = &argv[i];
			break;
		}
	}

    /*** set up isis stuff ***/
	if (no_isis) {
		printf("cmd: running unconnected to isis\n");
	} else {
		isis_init(client_port);
		ISIS_ENTER();
		isis_start_done();
	}

	gid = &NULLADDRESS;
	if (single_cmd) {
		t_fork((vfunc *) cmd_task, argv);
		for (;;) {
			isis_accept_events(ISIS_BLOCK);
		}

	} else {
		mask = 1 << fd_stdin | 1 << isis_socket;
		fputs("cmd> ", stdout);
		fflush(stdout);
		isis_input(fd_stdin, (vfunc *) cmd_task, NULL);
		for (;;) {
			isis_accept_events(ISIS_BLOCK);
		}
	}
}
