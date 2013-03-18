/*  $RCSfile: tk_connect.c,v $ $Revision: 2.104 $ $Date: 90/09/12 13:25:59 $  */
/*
 *  Simple tool to make a connection between two ISIS processes 
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

#define ISIS_SYS
#include "isis.h"

static qnode *connected;
static u_short isis_conn_port;
static vfunc *conn_handler = NULL;
static condition want_connect;

/* Exchange addresses on a stream connection file descriptor.  The */
/* address of the other guy is returned. */
static int
isis_handshake(fdes, addr)
	int fdes;
	address *addr;
{
	address *taddr;
	message *msg;
	bitvec imask;

	/* Send out my address */
	msg = msg_newmsg();
	msg_put(msg, "%A[1]", &my_address);
	if (msg_write(fdes, msg)) {
		msg_delete(msg);
		return (-1);
	}
	msg_delete(msg);

	/* Wait for reply */
	bclr(&imask);
	bis(&imask, fdes);
	if (isis_select(MAXBITS, (int *) &imask, (int *) NULL, (int *) NULL,
			(struct timeval *) NULL) < 0) {
		fprintf(stderr, "isis_handshake: failed isis_wait\n");
		return (-1);
	}
	/* Read other guys address */
	if ((msg = msg_read(fdes)) == NULL)
		return (-1);
	if (msg_get(msg, "%-A[1]", &taddr) < 1) {
		msg_delete(msg);
		return (-1);
	}
	*addr = *taddr;
	msg_delete(msg);

	return (0);
}

/* Declare a function to accept incoming connection attempts */
#ifdef CONVEX
void *
#else
vfunc *
#endif					/* CONVEX */
isis_connect_register(fun)
	vfunc *fun;
{
	vfunc *oldfun;

	oldfun = conn_handler;
	conn_handler = fun;
	isis_task(conn_handler, "registered connection handler");
	return (oldfun);
}

static void
isis_conn_accept(sock)
	int sock;
{
	saddr from;
	int fromlen, fdes;
	address addr;
	bitvec imask;

	/* Build bit vector describing which descriptor to read from. */
	bclr(&imask);
	bis(&imask, sock);
	/* Wait for input */
	while (isis_select(MAXBITS, (int *) &imask, (int *) NULL, (int *) NULL,
			   (struct timeval *) NULL) >= 0) {
		/* Accept the TCP connection */
		fromlen = sizeof(saddr);
		fdes = accept(sock, (struct sockaddr *) &from, &fromlen);
		if (fdes == -1) {
			perror("isis_conn_accect: Accept");
			continue;
		}
		/* Get the address of the other guy */
		if (isis_handshake(fdes, &addr)) {
			fprintf(stderr, "isis_conn_accept: failed to handshake\n");
			close(fdes);
			continue;
		}
		/* Add him to the list of connected sites */
		pg_add(connected, &addr, (char *) fdes, NULLROUTINE);
		t_sig_all(&want_connect, 0);
		/* Call the user specified handler routine */
		if (conn_handler)
			(*conn_handler) (&addr);
		bclr(&imask);
		bis(&imask, sock);
	}
	fprintf(stderr, "isis_conn_accept: isis_wait failed\n");
}

static void
isis_conn_inquiry(mp)
	register message *mp;
{
	reply(mp, "%h", isis_conn_port);
}

/* Initialize connection interface */
void
isis_connect_init()
{
	int slen;
	int sock;
	saddr addr;
	static port_no = 1950;

	/* Initialize queues */
	connected = qu_null();

	/* Initialize entry points */
	isis_entry(GENERIC_ISIS_CONNECT, isis_conn_inquiry, "isis_conn_inquiry");

	/* Initialize connection listening port */
	if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1)
		panic("isis_conn_init: socket");

	/* Bind socket to any port */
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = INADDR_ANY;
	addr.sin_port = htons(port_no);
	while (bind(sock, (struct sockaddr *) &addr, sizeof(addr)) == -1) {
		if (errno != EADDRINUSE)
			panic("isis_conn_init: bind");
		++port_no;
		addr.sin_port = htons(port_no);
	}
	isis_conn_port = port_no;
	/* Set handler for input on transfer port */
	if (listen(sock, 5) == -1)
		panic("isis_conn_init: listen");
	(void) signal(SIGPIPE, SIG_IGN);
	isis_task(isis_conn_accept, "isis_conn_accept");
	t_fork(isis_conn_accept, (void *) sock);
}

static
iaddr_cmp(a1, a2)
	register address *a1, *a2;
{
	register n;

	if (n = (a1->addr_site - a2->addr_site))
		return (n);
	return (a1->addr_process - a2->addr_process);
}

/* Called on both sides to initiate the connection */
int
isis_connect(who)
	address *who;
{
	int fdes = -1;
	register qnode *cp;
	address whocopy;
	u_short targport;
	saddr targsock;

	ISIS_ENTER();
	whocopy = *who;
	whocopy.addr_entry = 0;
	if (addr_ismine(&whocopy))
		ISIS_RETURN(fdes);

	while (iaddr_cmp(who, &my_address) < 0) {
		if (cp = pg_find(connected, &whocopy)) {
			fdes = (int) cp->qu_data;
			qu_free(cp);
			ISIS_RETURN(fdes);
		}
		(void) t_wait_l(&want_connect, "waiting for remote connect");
	}
	/* Get the address of the port the other guy is listening at */
	if (cbcast(&whocopy, GENERIC_ISIS_CONNECT, "", 1, "%h", &targport) < 1) {
		errno = ECONNABORTED;
		ISIS_RETURN(-1);
	}
	targsock = *get_addr_by_address(&whocopy, (int) targport);
	/* Build a socket to connect to him */
	if ((fdes = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
		perror("isis_connect: socket");
		ISIS_RETURN(-1);
	}
	/* Connect to his listening port */
	if (connect(fdes, (struct sockaddr *) &targsock, sizeof(targsock)) == -1) {
		perror("isis_connect: connect");
		close(fdes);
		ISIS_RETURN(-1);
	}
	/* Handshake with the other guy to give him my address */
	if (isis_handshake(fdes, &whocopy) == -1) {
		close(fdes);
		ISIS_RETURN(-1);
	}

	ISIS_RETURN(fdes);
}
