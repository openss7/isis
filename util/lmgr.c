/*
 *	Originally by Ken Kane
 *      Log Manager: Initialization code for log manager at
 *                   time of ISIS startup.
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

char lmgr_rcsid[] =
    "$Revision: 2.31 $$Date: 90/08/25 19:42:49 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/util/RCS/lmgr.c,v $";
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <sys/errno.h>
#include <dirent.h>
#include "isis.h"

/* By not replying, leave /bin/xmgr hung until /bin/lmgr exits */
void
lmgr_check_done(mp)
	register message *mp;
{
}

main(argc, argv)
	int argc;
	char *argv[];
{
	int client_port, path_stat, mk_stat;
	int old_mask;
	char *isis_path, *logs_path, *rm_cmd;
	struct stat buf;
	address xmgr;

	client_port = 0;

	if (argc != 2) {
		fprintf(stderr, "usage: %s port\n", argv[0]);
		exit(-1);
	}

	client_port = atoi(argv[1]);
	if (client_port == 0) {
		fprintf(stderr, "usage: %s port\n", argv[0]);
		exit(-1);
	}

	isis_entry(LR_CHECK_DONE, lmgr_check_done, "lmgr_check_done");
	my_process_id = LMGR;
	isis_init(client_port);

	/* Set the ISIS directory path name and logs directory path name */

	isis_path = malloc(strlen(isis_dir) + 1);
	strcpy(isis_path, isis_dir);

	logs_path = malloc(strlen(isis_path) + sizeof(LOG_SUBDIR) + 2);
	strcpy(logs_path, isis_path);
	strcat(logs_path, "/");
	strcat(logs_path, LOG_SUBDIR);

	/* Check to make sure that the ISIS directory exists */

	path_stat = stat(isis_path, &buf);
	if (path_stat != 0) {
		perror("Log Manager: ISIS directory cannot be accessed");
		panic("Log Manager: Fatal Error");
	}

	/* Check to make sure that the ISIS logs directory exists. If it does not, then create it. */

	path_stat = stat(logs_path, &buf);
	if ((path_stat != 0) && (errno != ENOENT)) {
		perror("Log Manager: ISIS logs directory cannot be accessed");
		panic("Log Manager: Fatal Error");
	}
	if ((path_stat != 0) && (errno == ENOENT)) {
		old_mask = umask(0);
		mk_stat = mkdir(logs_path, 0777);
		umask(old_mask);
		if (mk_stat != 0) {
			perror("Log Manager: Can't create ISIS logs directory");
			panic("Log Manager: Fatal Error");
		}
		printf("Log Manager: Created ISIS logs directory\n");
	}

	/* Remove temporary log files */
	rm_cmd = malloc(strlen(logs_path) + sizeof(LOG_TSPEC) + 14);
	strcpy(rm_cmd, "/bin/rm -r -f ");
	strcat(rm_cmd, logs_path);
	strcat(rm_cmd, "/");
	strcat(rm_cmd, LOG_TSPEC);
	system(rm_cmd);

	lmgr_scandir(logs_path);

	/* Handle check_done requests, if any */
	isis_start_done();
	isis_accept_events(0);

	printf("Log Manager: Startup Completed (exiting normally)\n");
	exit(0);
}

lmgr_scandir(ldir)
	char *ldir;
{
	register DIR *dirp;
	struct dirent *dp;

	chdir(ldir);
	dirp = opendir(".");
	while ((dp = readdir(dirp)) != NULL) {
		register char *str;
		char new_name[MAXNAMLEN + 1];

		if (*dp->d_name == '.')
			continue;
		str = dp->d_name;
		while (*str)
			++str;
		while (str != dp->d_name && *str != '.')
			--str;
		if (strcmp(str, ".OLDFORMAT") == 0)
			continue;
		if (l_check_log(ldir, dp->d_name) == 0)
			continue;
		strcpy(new_name, dp->d_name);
		strcat(new_name, ".OLDFORMAT");
		print("Renaming <%s/%s> as <%s/%s>\n", ldir, dp->d_name, ldir, new_name);
		unlink(new_name);
		if (rename(dp->d_name, new_name) == -1)
			perror("isis-lmgr rename failed");
	}
}

l_check_log(l_dir, l_name)
	char *l_dir, *l_name;
{
	register int cc;
	FILE *l_fp;
	register status = 0;

	/* Try opening, but not creating, log file */
	l_fp = fopen(l_name, "r+");
	if (l_fp == NULL)
		print("Warning: isis-lmgr can't access <%s/%s>\n", l_dir, l_name);
	else {
		log_hdr l_hdr;

		fseek(l_fp, 0L, 0);
		cc = fread(&l_hdr, 1, sizeof(log_hdr), l_fp);
		if (cc < sizeof(log_hdr)) {
			print("Warning: <%s/%s> is not a log file\n", l_dir, l_name);
			status = -1;
		} else if (strcmp(l_hdr.version, L_VERSION) != 0) {
			print("Warning: <%s/%s> is not in the current log-file format\n", l_dir,
			      l_name);
			status = -1;
		}
	}
	fclose(l_fp);
	return (status);
}
