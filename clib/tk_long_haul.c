/*  $RCSfile: tk_long_haul.c,v $ $Revision: 2.88 $ $Date: 90/08/17 14:03:24 $  */

/*
 * LONG HAUL DIRECT INTERFACE
 *
 * Coded by Mesaac Makpangou
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

#define ISIS_SYS
#include "isis.h"
#include "spooler.h"
#define IE_PARTICIPANTS_LIST -1111
#define IE_SETNAME -2222

char *
dirfilename(userpath, dir)
	char *userpath, *dir;
{
	static char buf[500];
	int i;
	char *p;

	i = strlen(dir);
	strcpy(buf, dir);
	if (buf[i - 1] != '/') {
		strcat(buf, "/");
	}
	p = (char *) strrchr(userpath, '/');
	if (p == 0)
		p = userpath - 1;
	strcat(buf, p + 1);
	return buf;
}

int
wset_join(wanApplName)
	char *wanApplName;
{
	message *lh_msg;
	struct timeval time;

	ISIS_ENTER();
	lh_msg = msg_gen("%s", wanApplName);
	gettimeofday(&time, (struct timezone *) 0);
	msg_putfld(lh_msg, SYSFLD_SPSCAN, "%d,%d,%d%d%A[1]",
		   SP_TIME, time.tv_sec, time.tv_sec, SP_SENDER, msg_getsender(lh_msg));
	sp_send(LH_JOIN, "wanManager", lh_msg);
	msg_delete(lh_msg);
	ISIS_RETURN(0);
}

/* participantsTab is a null string terminating list of clusters names. */
bitvec *
giveConvID(setName, participantsTab)
	char *setName;
	char **participantsTab;
{
	message *msg;
	int rval, i;
	struct timeval time;
	bitvec *convIdPtr = (bitvec *) 0;
	address *myspooler;

	ISIS_ENTER();

	{
		myspooler = pg_lookup("@*:spooler");
		if (addr_isnull(myspooler))
			panic("<isis-spooler>: not operational");
	}
	if ((participantsTab == (char **) 0) || (participantsTab[0] == (char *) 0)) {
		isis_errno = IE_PARTICIPANTS_LIST;
		return (convIdPtr);
	}
	if (setName == (char *) 0) {
		isis_errno = IE_SETNAME;
		return (convIdPtr);
	}
	convIdPtr = (bitvec *) calloc(1, sizeof(bitvec));
	msg = msg_newmsg();
	msg_put(msg, "%s", setName);
	for (i = 0; participantsTab[i] != (char *) 0; i++)
		msg_put(msg, "%s", participantsTab[i]);
	gettimeofday(&time, (struct timezone *) 0);
	msg_putfld(msg, SYSFLD_SPSCAN, "%d,%d,%d%d%A[1]",
		   SP_TIME, time.tv_sec, time.tv_sec, SP_SENDER, msg_getsender(msg));
	if ((rval = bcast(myspooler, LH_CONVID_REQUEST, "%m", msg, 0, 1, "%B", &convIdPtr)) == -1) {
		isis_perror("<isis-spooler>: bcast error");
		panic("<isis-spooler>: too many bcast errors");
	}
	ISIS_RETURN(convIdPtr);
}

int
do_pc_bcast(wanApplName, entry, destName, msg, lh_entry, convID)
	char *wanApplName, *destName;
	message *msg;
	bitvec *convID;
	int lh_entry, entry;
{
	char *net;
	struct timeval time;

	ISIS_ENTER();
	if (!strcmp(destName, "all_clusters") || !strcmp(destName, "allClusters"))
		net = "all";
	else if (!strcmp(destName, "local_cluster") || !strcmp(destName, "localCluster"))
		net = "local";
	else
		net = destName;
	msg_putfld(msg, SYSFLD_NETWORK, "%s", net);
	if (convID != (bitvec *) 0)
		msg_putfld(msg, SYSFLD_NETWORK, "%B", convID);
	gettimeofday(&time, (struct timezone *) 0);
	msg_putfld(msg, SYSFLD_SPSCAN, "%s%d", wanApplName, entry);
	msg_putfld(msg, SYSFLD_SPSCAN, "%d,%d,%d%d%A[1]",
		   SP_TIME, time.tv_sec, time.tv_sec, SP_SENDER, msg_getsender(msg));
	sp_send(lh_entry, wanApplName, msg);
	ISIS_RETURN(0);
}

int
lh_cbcast_m(wanApplName, entry, destName, msg)
	char *wanApplName, *destName;
	int entry;
	message *msg;
{
	return (do_pc_bcast(wanApplName, entry, destName, msg, LH_CBCAST, (bitvec *) 0));
}

int
pc_cbcast_m(wanApplName, entry, destName, msg, convID)
	char *wanApplName, *destName;
	int entry;
	message *msg;
	bitvec *convID;
{
	return (do_pc_bcast(wanApplName, entry, destName, msg, LH_CBCAST, convID));
}

int
lh_abcast_m(wanApplName, entry, destName, msg)
	char *wanApplName, *destName;
	int entry;
	message *msg;
{
	return (do_pc_bcast(wanApplName, entry, destName, msg, LH_ABCAST, (bitvec *) 0));
}

int
pc_abcast_m(wanApplName, entry, destName, msg, convID)
	char *wanApplName, *destName;
	int entry;
	message *msg;
	bitvec *convID;
{
	return (do_pc_bcast(wanApplName, entry, destName, msg, LH_ABCAST, convID));
}

int
do_pc_file_xfer(wanApplName, entry, destName, msg, path, convID, srcdir, destdir, op)
	char *wanApplName, *destName, *path, *srcdir, *destdir;
	int entry, op;
	message *msg;
	bitvec *convID;
{
	struct timeval time;
	char *net, *p;
	char *buf[500];
	FILE *f;
	int i;

	ISIS_ENTER();
	if ((path[0] == '/') || (srcdir == 0) || (*srcdir == '\000')) {
		if ((f = fopen(path, "r")) == NULL)
			return (-1);
	} else {
		if ((f = fopen(dirfilename(path, srcdir), "r")) == NULL)
			return (-1);
	}
	fclose(f);
	if (!strcmp(destName, "all_clusters") || !strcmp(destName, "allClusters")) {
		if (op == LH_COPY) {
			net = "all";
		} else {
			return (-1);
		}
	} else if (!strcmp(destName, "local_cluster") || !strcmp(destName, "localCluster"))
		net = "local";
	else
		net = destName;

	/* construct source and destination names */
	if ((path[0] == '/') || (srcdir == 0) || (*srcdir == '\000'))
		msg_putfld(msg, SYSFLD_FILE, "%s", path);
	else
		msg_putfld(msg, SYSFLD_FILE, "%s", dirfilename(path, srcdir));

	if ((destdir) && strcmp(destdir, ""))
		msg_putfld(msg, SYSFLD_FILE, "%s", dirfilename(path, destdir));
	else {
		p = (char *) strrchr(path, '/');
		if (p)
			msg_putfld(msg, SYSFLD_FILE, "%s", p + 1);
		else
			msg_putfld(msg, SYSFLD_FILE, "%s", path);
	}
	msg_putfld(msg, SYSFLD_FILE, "%d", op);
	msg_putfld(msg, SYSFLD_NETWORK, "%s", net);
	if (convID != (bitvec *) 0)
		msg_putfld(msg, SYSFLD_NETWORK, "%B", convID);
	msg_putfld(msg, SYSFLD_SPSCAN, "%s%d", wanApplName, entry);
	gettimeofday(&time, (struct timezone *) 0);
	msg_putfld(msg, SYSFLD_SPSCAN, "%d,%d,%d%d%A[1]",
		   SP_TIME, time.tv_sec, time.tv_sec, SP_SENDER, msg_getsender(msg));
	sp_send(LH_XFER, wanApplName, msg);
	ISIS_RETURN(0);
}

int
lh_file_xfer_l(wanApplName, entry, destName, msg, filename, srcdir, destdir)
	char *wanApplName, *destName, *srcdir, *destdir, *filename;
	int entry;
	message *msg;
{
	return (do_pc_file_xfer(wanApplName, entry, destName, msg, filename,
				(bitvec *) 0, srcdir, destdir));
}

static char default_lh_src_dir[500];
static char default_lh_dst_dir[500];

void
lh_file_xfer_srcdir(dir)
	char *dir;
{
	strcpy(default_lh_src_dir, dir);
}

void
lh_file_xfer_dstdir(dir)
	char *dir;
{
	strcpy(default_lh_dst_dir, dir);
}

void
lh_file_srcerr_handler(gname, entry)
	char *gname;
	int entry;
{
	message *mp = msg_gen("%s%d", gname, entry);

	sp_control_send(SP_LH_SRCERR, mp);
	msg_delete(mp);
}

void
lh_file_dsterr_handler(gname, entry)
	char *gname;
	int entry;
{
	message *mp = msg_gen("%s%d", gname, entry);

	sp_control_send(SP_LH_DSTERR, mp);
	msg_delete(mp);

}

int
lh_file_copy(wanApplName, entry, destName, msg, filename)
	char *wanApplName, *destName, *filename;
	int entry;
	message *msg;
{
	return (do_pc_file_xfer(wanApplName, entry, destName, msg, filename,
				(bitvec *) 0, default_lh_src_dir, default_lh_dst_dir), LH_COPY);
}

int
lh_file_move(wanApplName, entry, destName, msg, filename)
	char *wanApplName, *destName, *filename;
	int entry;
	message *msg;
{
	return (do_pc_file_xfer(wanApplName, entry, destName, msg, filename,
				(bitvec *) 0, default_lh_src_dir, default_lh_dst_dir, LH_MOVE));
}
