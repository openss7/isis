/*   $RCSfile: main.c,v $ $Revision: 2.3 $ $Date: 90/08/08 10:01:11 $ */
/*
 *	make [-f makefile] ... [-ins ... ] [target(s) ...]
 *
 *	(Better than EON mk but not quite as good as UNIX make)
 *
 *	-f makefile name
 *	-i ignore exit status
 *	-n Pretend to make
 *	-p Print all macros & targets
 *	-q Question up-to-dateness of target.  Return exit status 1 if not
 *	-r Don't not use inbuilt rules
 *	-s Make silently
 *	-t Touch files instead of making them
 *	-m Change memory requirements (EON only)
 *      -G output graph file name
 *      -P isis port number
 *      -S number of servers
 *      -K keep graph and report files
 *      -A accumulate parallel steps instead of executing them
 */

#ifndef ABSDIR
#define ABSDIR "/usr/u/isis/SUN/bin"
#endif

#include <stdio.h>
#include <make.h>
#include <sys/file.h>

#ifdef unix
#include <sys/errno.h>
#endif
#ifdef eon
#include <sys/err.h>
#endif
#ifdef os9
#include <errno.h>
#endif

#if	defined(HPUX)
char *strrchr(), *strchr();
#define	rindex(a,b)	strrchr(a,b)
#define	index(a,b)	strchr(a,b)
#endif


#ifdef eon
#define MEMSPACE	(16384)
#endif


char *			myname;
char *			makefile;	/*  The make file  */
#ifdef eon
unsigned		memspace = MEMSPACE;
#endif

FILE *			ifd;		/*  Input file desciptor  */
bool			domake = TRUE;	/*  Go through the motions option  */
bool			ignore = FALSE;	/*  Ignore exit status option  */
bool			silent = FALSE;	/*  Silent option  */
bool			dprint = FALSE;	/*  Print debuging information  */
bool			rules = TRUE;	/*  Use inbuilt rules  */
bool			dotouch = FALSE;/*  Touch files instead of making  */
bool			quest = FALSE;	/*  Question up-to-dateness of file  */

FILE *pmake_stream;           /* parallel make graph file */
int isis_port = 0;          /* ISIS port number - default from /etc/services */
int num_procs = 6;            /* number of servers to use */
bool execute_psteps = TRUE;   /* execute steps or just accumulate in graph
				 file */
bool keep_files = FALSE;      /* remove output */

void
main(argc, argv)
int			argc;
char **			argv;
{
	register char *		p;		/*  For argument processing  */
	int			estat = 0;	/*  For question  */
	register struct name *	np;
	char graph_file[64];

	printf("SERIAL PHASE\n");
	strcpy(graph_file,"pmake.gph");     /* default output file */

	myname = (argc-- < 1) ? "make" : *argv++;

	while ((argc > 0) && (**argv == '-'))
	{
		argc--;		/*  One less to process  */
		p = *argv++;	/*  Now processing this one  */

		while (*++p != '\0')
		{
			switch(*p)
			{
			case 'f':	/*  Alternate file name  */
				if (*++p == '\0')
				{
					if (argc-- <= 0)
						usage();
					p = *argv++;
				}
				makefile = p;
				goto end_of_args;

		        case 'G':	/*  Alternate graph name  */
				if (*++p == '\0')
				{
					if (argc-- <= 0)
						usage();
					p = *argv++;
				}
				strcpy(graph_file,p);
				goto end_of_args;

			case 'P':	/*  Alternate ISIS port  */
				if (*++p == '\0')
				{
					if (argc-- <= 0)
						usage();
					p = *argv++;
				}
				sscanf(p,"%d",&isis_port);
				goto end_of_args;

			case 'S':	/*  Alternate number of servers  */
				if (*++p == '\0')
				{
					if (argc-- <= 0)
						usage();
					p = *argv++;
				}
				sscanf(p,"%d",&num_procs);
				goto end_of_args;

#ifdef eon
			case 'm':	/*  Change space requirements  */
				if (*++p == '\0')
				{
					if (argc-- <= 0)
						usage();
					p = *argv++;
				}
				memspace = atoi(p);
				goto end_of_args;
#endif
			case 'n':	/*  Pretend mode  */
				domake = FALSE;
				break;
			case 'i':	/*  Ignore fault mode  */
				ignore = TRUE;
				break;
			case 's':	/*  Silent about commands  */
				silent = TRUE;
				break;
			case 'p':
				dprint = TRUE;
				break;
			case 'r':
				rules = FALSE;
				break;
			case 't':
				dotouch = TRUE;
				break;
			case 'q':
				quest = TRUE;
				break;
			case 'A':
				execute_psteps = FALSE;
				break;
			case 'K':
				keep_files = TRUE;
				break;
				
			default:	/*  Wrong option  */
				usage();
			}
		}
	end_of_args:;
	}

#ifdef eon
	if (initalloc(memspace) == 0xffff)  /*  Must get memory for alloc  */
		fatal("Cannot initalloc memory");
#endif

	if (makefile &&
	    (strcmp(makefile, "-") == 0))   /*  Can use stdin as makefile  */
		ifd = stdin;
	else
		if (!makefile)		/*  If no file, then use default */
		{
			if ((ifd = fopen(DEFN1, "r")) == (FILE *)0)
#ifdef eon
				if (errno != ER_NOTF)
					fatal("Can't open %s; error %02x", DEFN1, errno);
#endif
#ifdef unix
				if (errno != ENOENT)
					fatal("Can't open %s; error %02x", DEFN1, errno);
#endif
#ifdef  DEFN2
#  ifndef os9
			if ((ifd == (FILE *)0)
				  && ((ifd = fopen(DEFN2, "r")) == (FILE *)0))
				fatal("Can't open %s", DEFN2);
#  else   os9
				fatal("Can't open %s", DEFN1);
#  endif  os9
#else   DEFN2
			fatal("Can't open %s", DEFN1);
#endif  DEFN2
		}
		else
			if ((ifd = fopen(makefile, "r")) == (FILE *)0)
				fatal("Can't open %s", makefile);

	makerules();

	setmacro("$", "$");

	while (argc && (p = index(*argv, '=')))
	{
		char		c;

		c = *p;
		*p = '\0';
		setmacro(*argv, p+1);
		*p = c;

		argv++;
		argc--;
	}

	input(ifd);	/*  Input all the gunga  */
	fclose(ifd);	/*  Finished with makefile  */
	lineno = 0;	/*  Any calls to error now print no line number */

	if (dprint)
		prt();	/*  Print out structures  */

	np = newname(".SILENT");
	if (np->n_flag & N_TARG)
		silent = TRUE;

	np = newname(".IGNORE");
	if (np->n_flag & N_TARG)
		ignore = TRUE;

	precious();

	if (!firstname)
		fatal("No targets defined");

	circh();	/*  Check circles in target definitions  */

	init_lib();

	pmake_stream = fopen(graph_file,"a");
	
	if (pmake_stream)	
	  {	  
	    if (!argc)
	      estat = make(firstname, 0);
	    else while (argc--)
	      {
		if (!dprint && !silent && strcmp(*argv, "love") == 0)
		  printf("Not war!\n");
		
		estat |= make(newname(*argv++), 0);
	      }
	    fclose(pmake_stream);

	    if (execute_psteps && (get_file_sz(graph_file) > 0))
	      {
		char job[120];
		printf("PARALLEL PHASE\n");
		printf("%s\n",isis_dir);
		strcpy(job,ABSDIR);
		strcat(job,"/pmkexec");
		
		if(access(job,X_OK)==0)
		  {
		    sprintf(job,"%s/pmkexec %s pmk %d %d",ABSDIR,
			    graph_file,isis_port,num_procs);
		    printf("RUNNING %s\n",job);
		    system(job);
		    
		    /* print the results */
		    printf("RESULTS:\n");
		    sprintf(job,"cat %s.stdo*",graph_file);
		    system(job);
		  }
		else
		  {
		    printf("Can't find %s.  Be sure ", job);
		    printf("pmake and pmkexec are in the same directory.\n");
		  }
		/* clean up */
		if (!keep_files)
		  {
		    sprintf(job,"rm %s*",graph_file);
		    system(job);
		  }
	      }
	  } else perror("graph file open error ");

	if (quest)
		exit(estat);
	else
		exit(0);
}


usage()
{
	fprintf(stderr, "Usage: %s [-f makefile] [-G graphfile] [-P isis_port] [-S num_servers]\n                 [-inpqrstAK] [macro=val ...] [target(s) ...]\n", myname);
	exit(1);
}


void
fatal(msg, a1, a2, a3, a4, a5, a6)
char	*msg;
{
	fprintf(stderr, "%s: ", myname);
	fprintf(stderr, msg, a1, a2, a3, a4, a5, a6);
	fputc('\n', stderr);
	exit(1);
}

