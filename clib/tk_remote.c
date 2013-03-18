/*  $RCSfile: tk_remote.c,v $ $Revision: 2.79 $ $Date: 90/08/14 10:46:46 $  */
/*
 *	Originally coded by Ken Birman
 *
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
 *
 *      Remote client->isis interface
 */

#define   ISIS_SYS
# include "isis.h"

extern saddr mother_addr;
extern address my_mother;

isis_remote(mother_machine, REMOTE_PORT)
	register char *mother_machine;
{
	register message *mp;
	message *rmp;
	register struct hostent *hep;
	struct hostent *gethostbyname();
	static saddr sin;
	static port_no = 2000;
	int site_no;
	register try;

	my_process_id = getpid();
	begin {
		extern char *getenv();
		register char *envp = getenv("ISISREMOTE");

		if (envp)
			REMOTE_PORT = atoi(envp);
	}
	if (REMOTE_PORT == 0) {
		register struct servent *sp;
		struct servent *getservbyname();

		sp = getservbyname("isis", "bcast");
		if (sp == (struct servent *) 0)
			panic("isis.*: service not listed in /etc/services on this host");
		REMOTE_PORT = ntohs(sp->s_port);
	}
	intercl_socket = socket(AF_INET, SOCK_DGRAM, 0);
	sin.sin_family = AF_INET;
	sin.sin_port = htons(port_no);
	while (bind(intercl_socket, (struct sockaddr *) &sin, sizeof(sin)) == -1) {
		if (errno != EADDRINUSE)
			panic("Can't allocate UDP port!");
		++port_no;
		sin.sin_port = htons(port_no);
	}
	set_isis_sockopts(intercl_socket);
	if ((hep = gethostbyname(mother_machine)) == 0)
		panic("gethostbyname(3N): site %s unknown", mother_machine);
	mother_addr.sin_family = AF_INET;
	bcopy(hep->h_addr, &mother_addr.sin_addr, hep->h_length);
	mother_addr.sin_port = htons(REMOTE_PORT);
	gethostname(my_host, 64);
	mp = msg_gen("%d,%s,%d", ISIS_M_REMOTE, my_host, my_process_id);
	rmp = udp_rpc(intercl_socket, mother_machine, &mother_addr, 0, 0, mp);
	msg_delete(mp);
	if ((mp = rmp) == (message *) 0)
		panic("isis_remote unable to connect to isis at remote system");
	begin {
		int type;
		extern message *mother_rep_msg;

		msg_get(mp, "%d,%A[1],%A[1]", &type, &my_mother, &my_address);
		my_site_no = my_address.addr_site;
		my_site_incarn = my_address.addr_incarn;
		my_process_id = my_address.addr_process;
		my_port_no = my_address.addr_portno;
		mother_addr.sin_port = htons(my_mother.addr_portno);
		if (type != ISIS_M_ACK)
			panic("isis_remote unable to connect to isis at remote system");
		/* Mother reply message is picked up in cl_isis.c */
		mother_rep_msg = mp;
	}
	isis_init_l(REMOTE_PORT, ISIS_REMOTE | ISIS_PANIC);
}
