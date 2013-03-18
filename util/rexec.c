/*
 *	Originally by Ken Birman
 *      setuid added by Mark Steiglitz, 1/9/90, V2.0
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

char rexec_rcsid[] = "$Revision: 2.16 $$Date: 90/08/07 16:05:42 $$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/util/RCS/rexec.c,v $";
#include "isis.h"
#include "rexec.h"
#include "string.h"
#include <errno.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <sys/wait.h>

char          *crypt();
int           rexec_req();
struct passwd *authenticate();
char          *getenv();

int           CLIENT_PORT;
FILE          *rfile;

int           numpaths;
char          **pathlist;

char*
make_dup(s)
  register char *s;
  {
	register char *s2;

	if(s == NULL)
	    return(s);
	s2 = (char*)malloc(strlen(s)+1);
	strcpy(s2, s);
	return(s2);
  }
  

main(argc, argv)
  char **argv;
  {
        int reaper();
	char fname[60];
        while(argc-- > 1)
            switch(**++argv)
            {
              default:
              badarg:
                panic("Bad argument: <%s>\n", *argv);

              case '0': case '1': case '2': case '3': case '4':
              case '5': case '6': case '7': case '8': case '9':
                CLIENT_PORT = atoi(*argv);
                continue;
            }

        my_process_id = REXEC;
        isis_init(CLIENT_PORT);
        isis_task((vfunc *) reaper, "reaper");
        isis_chwait((vfunc *) reaper, 0);
        isis_entry(REXEC_REQ, (vfunc *) rexec_req, "rexec_req");
	if(*isis_dir)
	    sprintf(fname, "%s/rexec.log", isis_dir);
	else
	    sprintf(fname, "rexec.log");
        unlink(fname);
        if((rfile = fopen(fname, "w")) == (FILE*)0)
	{
	    print("isis-rexec (site %d): unable to open %s\n", my_site_no, fname);
            rfile = fopen("/dev/null", "w");
	}
	makepathlist();

#ifdef HPUX
        setresuid(geteuid(),-1,-1);
#else
	setreuid(geteuid(),-1);
#endif
  
#ifdef FORK_BROKEN
	/* Can't have rexec running as root if can't setuid() */
	if (!getuid())
	  {
	    fprintf(rfile, "rexec running as root and FORK_BROKEN, so can't setuid() ... aborting\n");
	    fprintf(stderr, "<isis-rexec> aborting: running as root and can't setuid()\n");
	    exit(-1);
	  }
#endif

        /* Now enter ISIS main loop */
        isis_mainloop(NULLROUTINE, NULLARG);
  }

reaper()
  {
	register pid = wait3(0, WNOHANG, 0);
	if(pid != -1)
            fprintf(rfile, "rexec %d/%d: termination of pid %d\n", my_site_no,
		    my_site_incarn, pid);
  }

makepathlist()
  {
        int i;
	char *path;

        path = make_dup(getenv("PATH"));
	if (strtok(path, ":") == NULL)
	{
	    numpaths=0;
	    return;
	}
	else
	    numpaths=1;
	while (strtok(NULL, ":") != NULL)
	    numpaths++;

	free(path);
	pathlist = (char**)malloc(numpaths*sizeof(char*));
	path = make_dup(getenv("PATH"));
	pathlist[0] = strtok(path, ":");
	for (i = 1; i < numpaths; i++)
	  pathlist[i] = strtok(NULL, ":");
  }

static char *args[MAX_ARGS], *env[MAX_ENV];

rexec_req(mp)
  register message *mp;
  {
        extern errno;
        address pname;
        char *prog = msg_getfield(mp, RE_PROG, 1, (int*)0);
        char *user = msg_getfield(mp, RE_USER, 1, (int*)0);
        char *passwd = msg_getfield(mp, RE_PASSWD, 1, (int*)0);
        register n, na, ne;
	char *hostname = site_names[msg_getsender(mp)->addr_site];
	struct passwd *pwd;
	char *path;

        pname = ADDRESS(my_site_no, my_site_incarn, 0, 0);
        na = msg_getfields(mp, RE_ARGS, args, (int*)0, MAX_ARGS-1);
        args[na] = 0;
        ne = msg_getfields(mp, RE_ENV, env, (int*)0, MAX_ENV-1);
        env[ne] = 0;
        fprintf(rfile, "rexec %d/%d: execve <%s> args < ", my_site_no,
		my_site_incarn, prog);
        for(n = 0; n < na; n++)
            fprintf(rfile, "%s ", args[n]);
        fprintf(rfile, "> env < ");
        for(n = 0; n < ne; n++)
            fprintf(rfile, "%s ", env[n]);
        fprintf(rfile, ">\n");
        fflush(rfile);
	pwd = authenticate(user, hostname, passwd);
	if((pname.addr_entry = inpath_runable(prog, &path, pwd->pw_dir)) == 0)

#ifdef FORK_BROKEN /*sorry, setuid not supported for Apollo yet*/

#ifdef REXEC_REQUIRE_SETUID
	{
	    if (getuid() != pwd->pw_uid)
	    {
	        fprintf(rfile, "... rexec failed: REXEC_REQUIRE_SETUID, but can't setuid() (FORK_BROKEN)\n");
		fprintf(stderr, "<isis-rexec> unable to execute %s\n", path);
		pname = NULLADDRESS;
	    }
	    else
	        pname.addr_process =
		  isis_fork_execve(path, args, env, fileno(rfile), -1);
	}
#else /* not REXEC_REQUIRE_SETUID */
	pname.addr_process =
	  isis_fork_execve(path, args, env, fileno(rfile), -1);
#endif /* not REXEC_REQUIRE_SETUID */

#else /* not FORK_BROKEN */
	{
	    if (!getuid())
	        pname.addr_process =
		  isis_fork_su_execve(pwd->pw_name, pwd->pw_uid, pwd->pw_gid,
				      path, args, env, fileno(rfile), -1);
	    else

#ifdef REXEC_REQUIRE_SETUID
	    {
		if (getuid() != pwd->pw_uid)
		{
		    fprintf(rfile, "... rexec failed: REXEC_REQUIRE_SETUID, but can't setuid()\n");
		    fprintf(stderr,
			    "<isis-rexec> unable to execute %s\n", path);
		    pname = NULLADDRESS;
		}
		else
		    pname.addr_process =
		      isis_fork_execve(path, args, env, fileno(rfile), -1);
	    }
#else /* not REXEC_REQUIRE_SETUID */
  	        pname.addr_process =
		  isis_fork_execve(path, args, env, fileno(rfile), -1);
#endif /* not REXEC_REQUIRE_SETUID */

	}
#endif /* not FORK_BROKEN */

	else if(pname.addr_entry != 0)
	{
	    errno = pname.addr_entry;
	    fprintf(rfile, "... rexec failed errno %d\n", errno);
	    fprintf(stderr, "<isis-rexec> unable to execute ");
	    perror(path);
	}
	if(pname.addr_process == -1)
	{
	    perror("fork");
	    pname = NULLADDRESS;
	}
	free(path);
        fprintf(rfile, "... address is %d/%d:%d.%d\n", pname.addr_site,
		pname.addr_incarn, pname.addr_process, pname.addr_entry);
        fflush(rfile);
        reply(mp, "%A[1]", &pname);
  }

inpath_runable(prog, pathp, homedir)
  char *prog;
  char **pathp;
  char *homedir;
  {
        char path[MAXPATHLEN];
	char *dir;
	int result;
	int i;

        if (*prog == '/')
	{
	    *pathp = make_dup(prog);
            return(runable(prog));
	}
	if (homedir)
	{
	    strcpy(path, homedir);
	    strcat(path, "/");
	    strcat(path, prog);
	    if ((result = runable(path)) == 0)
	    {
	        *pathp = make_dup(path);
		return(result);
	    }
	}
	for (i = 0; i < numpaths; i++)
	{
	    strcpy(path, pathlist[i]);
	    strcat(path, "/");
	    strcat(path, prog);
	    if ((result = runable(path)) == 0)
	    {
	        *pathp = make_dup(path);
		return(result);
	    }
	}
	*pathp = make_dup(prog);
	return(runable(prog));
  }

runable(prog)
  char *prog;
  {
        struct stat s;
        register mode;
        if(prog == 0)
            return(EFAULT);
        if(stat(prog, &s) == -1)
            return(errno);
        mode = s.st_mode;
        if((mode&S_IFMT) != S_IFREG)
            return(EACCES);
        if((mode&S_ISUID) && (mode&S_IEXEC) == 0)
            return(EACCES);
        if((mode&S_ISGID) && (mode&(S_IEXEC>>3)) == 0)
            return(EACCES);
        if((mode&(S_IEXEC>>6)) == 0)
            return(EACCES);
        return(0);
  }

struct passwd *authenticate(user, hostname, passwd)
  char *user, *hostname, *passwd;
  {
        struct passwd *pwd;
        char *namep;

	if (!*user)
	    return(getpwnam("nobody"));

        pwd = getpwnam(user);
        if (pwd == NULL)
        {
            fprintf(stderr, "<isis-rexec> setuid failed: Login incorrect.\n");
	    return(getpwnam("nobody"));
        }
	if (!pwd->pw_uid) /*don't allow root*/
	{
	    fprintf(stderr, "<isis-rexec> setuid failed: root not allowed\n");
	    return(getpwnam("nobody"));
	}

#ifdef REXEC_ALLOW_RSH
	/*try rshd-style authentication*/
	if (!ruserok(hostname, !pwd->pw_uid, user, user))
	    return(pwd);
#endif

	/*try rexecd-style authentication*/
	if (pwd->pw_passwd)
	{
	    namep = crypt(passwd, pwd->pw_passwd);
	    if (!strcmp(namep, pwd->pw_passwd))
	        return(pwd);
	}
	
	/*neither worked*/
	return(getpwnam("nobody"));
  }

/* VARARGS */
isis_fork_su_execve(name, uid, gid, program, argp, envp, fd0, fd1, fd2)
  char *name, *program, **argp, **envp;
  {
        register pid;

	if(pid = isis_dofork())
	    return(pid);
	if(fd0 != -1)
	{
	    if(fd0)
		close(fd0);
	    if(fd1 != -1)
	    {
		if(fd1)
		    close(fd1);
		if(fd2 > 0)
		    close(fd2);
	    }
	}
	if ((gid != getgid()) || (gid != getegid()))
	{
	    if (setgid(gid) == -1)
            {
	        fprintf(stderr, "ISIS: unable to exec <%s>: setgid(%d)",
			program,gid);
	        perror("");
	        exit(0);
	    }
	    initgroups(name, gid);
	}
	if ((uid != getuid()) || (gid != getgid()))
            if (setuid(uid) == -1)
            {
                fprintf(stderr, "ISIS: unable to exec <%s>: setuid(%d)",
			program,uid);
		perror("");
                exit(0);
            }
	if(envp != (char**)0)
	    execve(program, argp, envp);
	else
	    execvp(program, argp);
	fprintf(stderr, "ISIS: unable to exec <%s>\n", program);
	exit(0);
  }
