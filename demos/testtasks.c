/*  $RCSfile: testtasks.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:52 $  */
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
 * We use this to test "ports" of ISIS to new system types
 * It exercises the task mechanisms in a mild but adequate way
 * and reveals problems saving/restoring registers.  Can be
 * linked to clib and will run even if ISIS is not up
 */

#include <isis.h>

#define		MAXTASKS	1001
#define		MAXSWITCH	500000

condition cond[MAXTASKS];
int siged[MAXTASKS];

int switched = 0;
int next_task = 0;
int n_tasks = 3;
int inc = 1;
int urgent_flag = 0;
int quiet;
int depth;

#define		test_print  if(!quiet) print

short crud[] = { 1, 2, 3, 4, 5, 6, 7, 7, 8, 9, 1, 2, 43, 4, 6, 7, 12 };

do_t_wait(depth, cp)
	register depth;
	register condition *cp;
{
	char stuff[4 * 9];

	bcopy(crud, stuff, sizeof(stuff));
	if (depth)
		return (do_t_wait(--depth, cp));
	return ((int) t_wait(cp));
}

void
do_t_sig(depth, cp, v)
	register depth;
	VOID *v;
	register condition *cp;
{
	char stuff[4 * 19];

	bcopy(crud, stuff, sizeof(stuff));
	if (depth)
		do_t_sig(--depth, cp, v);
	else
		t_sig(cp, v);
	return;
}

void
do_t_sig_urg(depth, cp, v)
	register depth;
	VOID *v;
	register condition *cp;
{
	char stuff[4 * 29];

	bcopy(crud, stuff, sizeof(stuff));
	if (depth)
		do_t_sig_urg(--depth, cp, v);
	else
		t_sig_urgent(cp, v);
	return;
}

int
TWAIT(n)
	register n;
{
	register r;

	if (r = siged[n]) {
		test_print("%2d: Waiting (** pre-signaled **).\n", n);
		siged[n] = 0;
		return (r - 1);
	}
	test_print("%2d: Waiting.\n", n);
	return (do_t_wait(++depth & 0x17, &cond[n]));
}

void
TSIG(how, next, n)
	int how;
	register next, n;
{
	if (t_waiting(&cond[next]) == 0) {
		test_print("%2d: %s signal %2d (** wasn't waiting **)\n", n,
			   (how && n) ? "urgently" : "delay", next);
		siged[next] = n + 1;
		return;
	}
	test_print("%2d: %s signal %2d <%x>\n", n, (how && n) ? "urgently" : "delay", next,
		   &cond[next]);
	if (how && n) {
		do_t_sig_urg(++depth & 0x17, &cond[next], (VOID *) n);
	} else
		do_t_sig(++depth & 0x17, &cond[next], (VOID *) n);
}

#ifdef THREADS

#define	THREAD_ENTER()       if(n&1) ISIS_ENTER();
#define THREAD_EXIT()        if(n&1) ISIS_EXIT();

#else				/* THREADS */

#define	THREAD_ENTER()
#define THREAD_EXIT()

#endif				/* THREADS */

void
test_task(n)
	int n;
{
	register i0, i1, i2, i3, i4, i5, i6, i7, *p0, *p1, *p2, *p3, *p4, *p5, *p6, *p7;
	int who = -1;

	i0 = n & 0x7;
	i1 = i0 + 1;
	i2 = i0 + 2;
	i3 = i0 + 3;
	i4 = i0 + 4;
	i5 = i0 + 5;
	i6 = i0 + 6;
	i7 = i0 + 7;
	p0 = (int *) (i0 * 0x100 + 0x00);
	p1 = (int *) (i0 * 0x100 + 0x10);
	p2 = (int *) (i0 * 0x100 + 0x20);
	p3 = (int *) (i0 * 0x100 + 0x30);
	p4 = (int *) (i0 * 0x100 + 0x40);
	p5 = (int *) (i0 * 0x100 + 0x50);
	p6 = (int *) (i0 * 0x100 + 0x60);
	p7 = (int *) (i0 * 0x100 + 0x70);
	if (!quiet) {
		printf("%2d[%x]: Starting.  [%d/%d/%d/%d/%d/%d/%d/%d;%x/%x/%x/%x/%x/%x/%x/%x].\n",
		       n, isis_ctp, i0, i1, i2, i3, i4, i5, i6, i7, p0, p1, p2, p3, p4, p5, p6, p7);
		fflush(stdout);
	}
	if (n != 0)
		who = TWAIT(n);
	forever {
		THREAD_ENTER();
		if (!quiet) {
			printf
			    ("%2d[%x]: Running.  [%d/%d/%d/%d/%d/%d/%d/%d;%x/%x/%x/%x/%x/%x/%x/%x].  %2d woke me up\n",
			     n, isis_ctp, i0, i1, i2, i3, i4, i5, i6, i7, p0, p1, p2, p3, p4, p5,
			     p6, p7, who);
			fflush(stdout);
		}
		if ((next_task += inc) >= n_tasks)
			next_task -= n_tasks;
		TSIG(urgent_flag, next_task, n);
		if (++switched > MAXSWITCH)
			break;
		who = TWAIT(n);
		if (n == 0 && (++inc >= n_tasks))
			inc = 1;
		THREAD_EXIT();
	}
	test_print("%2d: Done.\n", n);
}

void
main(argc, argv)
	int argc;
	char **argv;
{
	register n;

	while (argc-- > 1)
		switch (**++argv) {
		default:
			n_tasks = atoi(*argv);
			if (n_tasks <= 0)
				panic("Usage");
			if (n_tasks > MAXTASKS)
				n_tasks = MAXTASKS;
			for (n = 2; n < n_tasks; n++)
				if (n_tasks % n == 0)
					panic("Usage");
			break;
		case '-':
			switch (*++*argv) {
			case 'u':
				++urgent_flag;
				break;
			case 'q':
				++quiet;
				break;
			}
		}
	if (argc > 1)
		++urgent_flag;
	test_print("Task tester: %d tasks, urgent %d\n", n_tasks, urgent_flag);
	t_init();
	ISIS_ENTER();
	isis_task(test_task, "task_tester: test task");
	for (n = n_tasks - 1; n != 0; n--) {
		test_print("Forking task %d\n", n);
#	if(THREADS)
#           if(SUNLWP)
		if (n & 1)
			THREAD_FORK(test_task, n, malloc(8096) + 8092);
		else
#           else
		if (n & 1)
			THREAD_FORK(test_task, n, 0);
		else
#           endif
#	endif
			t_fork(test_task, (void *) n);
	}
	test_print("Forking main task\n");
	isis_mainloop(test_task, NULLARG);
	test_print("Task tester: finished\n");
}
