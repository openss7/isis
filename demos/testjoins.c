/*  $RCSfile: testjoins.c,v $ $Revision: 2.22 $ $Date: 90/09/14 13:24:46 $  */
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

/* Used to test ISIS, not much of a demo */
#include    "isis.h"
#include    "grid.h"

#define     RECEIVE     1

#define     ADD         0
#define     DIV         1

address     *gaddr;
int         max_updates = 1000;
int         my_index;
int         fdes, silent, ngot;
void         join(), receive(), change(), display_input();
void        do_xfer(), get_xfer(), pick(), tjprint();
int	    ndests();
char	    *exclude = "";

void
main (argc, argv)
  int   argc;
  char  **argv;
{
    int     port_no = 0;
go = 1;
mode = AB_MODE;

    /* Read in command line arguments */
    while (argc-- > 1)
        if (**++argv >= '0' && **argv <= '9')
            port_no = atoi (*argv);
        else if (**argv == '-')
            switch ((*argv)[1])
            {
              case 'x':
                exclude = "x";
                break;

              case 'u':
                max_updates = atoi (&((*argv)[2]));
                break;

              case 's':
		silent++;
                break;

              case 'm':
                mode = ((*argv)[2] == 'c' ? CB_MODE :
                                ((*argv)[2] == 'g' ? GB_MODE :
                                     ((*argv)[2] == 'r' ? NO_MODE : AB_MODE)));
                break;

              default:
                panic ("testjoins: invalid option %c\n", (*argv)[1]);
            }

    if(mode != CB_MODE)
        exclude = "";

    /* Initialize */
/*    fdes = init_display();*/
    SRANDOM (getpid());

    /* Initialize and run ISIS */
    isis_init (port_no);
    isis_task (join, "join");
    isis_entry (RECEIVE, receive, "receive");
    isis_mainloop (join, NULLARG);
}




/* Join (or create) group "grid"; transfer state if joining */
void
join()
{
print ("join: calling pg_join\n");
    gaddr = pg_join ("grid",
                     PG_MONITOR, change, 0,
                     PG_XFER, 0, do_xfer, get_xfer,
                     0);
    if (addr_isnull (gaddr))
        panic ("testjoins: join failed");
print ("join: done\n");
    isis_start_done();
}



/* Pick a random update; transmit it to the group */
void
update()
{
    int                 x, y, op, arg, type;
    static int          n_updates;

    if (n_updates++ == max_updates)
        printf ("Completed %d updates!\n", max_updates);
    if (!go || n_updates > max_updates)
    {
        printf("gp %d n_updates %d max_updates %d\n", go, n_updates, max_updates);
        return;
    }

    pick (&x, &y, &op, &arg);
    tjprint ("update: %d %d %d %d\n", x, y, op, arg);
    if (mode == NO_MODE)
    {
        type = RANDOM() & 0xff;
        if (type < 20)
            type = GB_MODE;
        else if (type < 120)
            type = AB_MODE;
        else if (type < 200)
        type = CB_MODE;
    }
    else
        type = mode;
    switch (type)
    {
      case AB_MODE:
        abcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, 0);
        break;

      case CB_MODE:
        cbcast_l (exclude, gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, 0);
        break;

      case GB_MODE:
        gbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, 0);
        break;

      default:
        fbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, 0);
        break;
    }
    if(*exclude)
    {
        message *mp = msg_gen("%l%l%l%l", x, y, op, arg);
        void receive();
        while(task_congest)
            (void)t_wait(&task_decongested);
        t_fork_msg(receive, mp);
        msg_delete(mp);
    }
}



/* Receive an update message; perform update; do a new update if */
/* a remote update has been received                             */
void
receive (mp)
  register message  *mp;
{
    int         x, y, op, arg;
int who = 0;
    static int  n_recd;

    msg_get (mp, "%l%l%l%l", &x, &y, &op, &arg);
msg_get(mp, "%d", &who);
    tjprint ("receive:             %d %d %d %d\n", x, y, op, arg);
    if(ndests(mp) < n_memb) {
        print("%d got a message with sid=%d, %d dests but group has %d members\n",
	    my_process_id, msg_getid(mp), ndests(mp), n_memb);
        pmsg(mp);
    }

    switch (op)
    {
      case ADD:
        value[x][y] += arg;
        break;

      case DIV:
        value[x][y] = value[x][y] / 2;
        break;

      default:
        print ("testjoins: received invalid operation %d:\n", op);
	pmsg(mp);
        msg_printaccess(mp);
	panic("testjoins: bad opcode");
    }
    color[x][y] = (color[x][y] ? 0 : 1);
/*    display (x, y);*/

    if (++n_recd >= n_memb)
    {
        n_recd = 0;
        update();
    }
}

int
ndests(mp)
  message *mp;
  {
	register address *dp = msg_getdests(mp);
	register n = 0;
	while(!aptr_isnull(dp))
	    ++dp, ++n;
	return(n);
  }




/* If group membership changes, change the value of n_memb and my_index */
void
change (pg, arg)
  register groupview    *pg;
  int                   arg;
{
    gaddr = &pg->gv_gaddr;
    n_memb = pg->gv_nmemb;
    my_index = pg_rank (gaddr, &my_address);
print ("change: n_memb = %d, my_index = %d, counted %d receives\n", n_memb, my_index, ngot);
    ngot = 0;
/*    display_title();*/
    if(n_memb > 1)
        update();
}



void
print_value()
  {
        register i;
        for(i = 0; i < 8; i++)
        {
            register *v = value[i];
            register char *c = color[i];
            print("%.2d.%d %.2d.%d %.2d.%d %.2d.%d ", v[0], c[0],  v[1], c[1], v[2], c[2], v[3], c[3]);
            print("%.2d.%d %.2d.%d %.2d.%d %.2d.%d\n", v[4], c[4], v[5], c[5], v[6], c[6], v[7], c[7]);
        }
  }


/* Do state transfer; send current values in table */
void
do_xfer (loc)
  int   loc;
{
print ("do_xfer:\n");
print_value();
    if (loc == -1)
        xfer_out (loc, "%L%C%l", value[0], sizeof (value) / sizeof (int),
                                          color[0], sizeof (color), mode);
print ("do_xfer: done\n");
}


/* Receive transferred state; store in table */
void
get_xfer (loc, mp)
  int       loc;
  message   *mp;
{
    int     rmode;

print ("get_xfer:\n");
   msg_get (mp, "%L%C%l", value[0], (int *) 0, color[0], (int *) 0, &rmode);
print_value();
/*   mode = rmode;*/
/*   display_all();*/
print ("get_xfer: done\n");
}   



/* Randomly pick a point in the grid, an operation, and an argument */
void
pick (x, y, op, arg)
  int   *x, *y, *op, *arg;
{
    if (mode == CB_MODE)
    {
        register int    n_for_me, xy;

        n_for_me = (63 - my_index) / n_memb;
        xy = ((RANDOM() & 0xffff) % n_for_me) * n_memb + my_index;
        *x = (xy & 070) >> 3;
        *y = xy & 07;
    }
    else
    {
        *x = RANDOM() & 0x7;
        *y = RANDOM() & 0x7;
    }
    *op = RANDOM() & 0x1;
    *arg = RANDOM() & 0xf;
}

void
tjprint(str, a,b,c,d,e,f,g)
  int a, b, c, d, e, f, g;
  char *str;
  {
	if(!silent)
	    print(str, a,b,c,d,e,f,g);
  }
