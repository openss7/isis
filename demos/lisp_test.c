#include "isis.h"
#include <ctype.h>
#include <string.h>
int atoi();

void
main_service()
{
	address addrs[3], *gaddr;
	char chars[10];
	message *m, *ms[1];
	int i, n;
	short h;
	char *s;

	isis_start_done();
	gaddr = pg_join("mtest", 0);

	print("Bcast m1\n");
	if (bcast(gaddr, 1, "%d", 123, ALL, "%d", &i) > 0) {
		print("Reply m1 %d\n", i);
	} else {
		print("No replies m1\n");
	}

	print("Bcast m1.1\n");
	m = msg_gen("%d", 212);
	if (bcast_l("m", gaddr, 1, m, ALL, ms) > 0) {
		msg_get(ms[0], "%d", &i);
		print("Reply m1 %d\n", i);
	} else {
		print("No replies m1.1\n");
	}

	print("Bcast m2\n");
	if (bcast(gaddr, 2, "%h", 456, ALL, "%h", &h) > 0) {
		print("Reply m2 %d\n", h);
	} else {
		print("No replies m2\n");
	}

	print("Bcast m3\n");
	if (bcast(gaddr, 3, "%s", "abc", ALL, "%-s", &s) > 0) {
		print("Reply m3 %s\n", s);
	} else {
		print("No replies m3\n");
	}

	print("Bcast m4\n");
	if (bcast(gaddr, 4, "%A[1]", &my_address, ALL, "%A", addrs, &n)
	    > 0) {
		print("Reply m4 ");
		for (i = 0; i < n; i++) {
			paddr(&(addrs[i]));
			print(" ");
		}
		print("\n");
	} else {
		print("No replies m4\n");
	}

	print("Bcast m4.1\n");
	addrs[0] = my_address;
	addrs[0].addr_entry = 1;
	addrs[1] = my_address;
	addrs[1].addr_entry = 2;
	addrs[2] = my_address;
	addrs[2].addr_entry = 3;
	if (bcast(gaddr, 4, "%A", addrs, 3, ALL, "%A", addrs, &n) > 0) {
		print("Reply m4.1 ");
		for (i = 0; i < n; i++) {
			paddr(&(addrs[i]));
			print(" ");
		}
		print("\n");
	} else {
		print("No replies m4.1\n");
	}

	print("Bcast m5\n");
	strncpy(chars, "zyxwvutsrq", 10);
	m = msg_gen("%C", chars, 10);
	if (bcast(gaddr, 5, "%m", m, ALL, "%m", &m) > 0) {
		print("Reply m5 ");
		msg_get(m, "%-C", &s, &n);
		for (i = 0; i < n; i++) {
			print("%d ", s[i]);
		}
		print("\n");
	} else {
		print("No replies m5\n");
	}
}

void
handle_m1(msg)
	message *msg;
{
	int i;

	msg_get(msg, "%d", &i);
	print("Got m1: %d\n", i);
	reply(msg, "%d", i * 2);
}

void
handle_m2(msg)
	message *msg;
{
	short i;

	msg_get(msg, "%h", &i);
	print("Got m2: %d\n", i);
	reply(msg, "%h", i * 2);
}

void
handle_m3(msg)
	message *msg;
{
	char *s, *c;

	msg_get(msg, "%-s", &s);
	print("Got m3: %s\n", s);
	for (c = s; *c != 0; ++c) {
		if (isupper(*c)) {
			*c = tolower(*c);
		} else if (islower(*c)) {
			*c = toupper(*c);
		}
	}
	reply(msg, "%s", s);
}

void
handle_m4(msg)
	message *msg;
{
	address *a;
	int n, i;

	msg_get(msg, "%-A", &a, &n);
	print("Got m4: %d [ ", n);
	for (i = 0; i < n; i++) {
		paddr(&a[i]);
		print(" ");
	}
	print("]\n");
	for (i = 0; i < n; i++) {
		++a[i].addr_entry;
	}
	reply(msg, "%A", a, n);
}

void
handle_m5(msg)
	message *msg;
{
	message *m;
	char *c;
	int n, i;

	msg_get(msg, "%m", &m);
	print("Got m5: ");
	pmsg(m);
	print("\n");

	print("contents: ");
	msg_get(m, "%-C", &c, &n);
	print(" %d [ ", n);
	for (i = 0; i < n; i++) {
		print("%d ", c[i]);
	}
	print(" ] \n");
	for (i = 0; i < n; i++) {
		if (isupper(c[i])) {
			c[i] = tolower(c[i]);
		} else if (islower(c[i])) {
			c[i] = toupper(c[i]);
		}
	}
	reply(msg, "%m", msg_gen("%C", c, n));
}

void
main(argc, argv)
	int argc;
	char *argv[];
{
	int isis_port = 0;

	if (argc >= 2) {
		isis_port = atoi(argv[1]);
	}
	isis_init(isis_port);

	isis_task(main_service, "main_service");
	isis_entry(1, handle_m1, "handle_m1");
	isis_entry(2, handle_m2, "handle_m2");
	isis_entry(3, handle_m3, "handle_m3");
	isis_entry(4, handle_m4, "handle_m4");
	isis_entry(5, handle_m5, "handle_m5");

	isis_mainloop(main_service, NULLARG);
};
