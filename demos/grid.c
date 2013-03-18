/*  $RCSfile: grid.c,v $ $Revision: 2.28 $ $Date: 90/09/15 20:35:47 $  */
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
#include    "isis.h"
#include    "grid.h"

#define     RECEIVE     1

#define     ADD         0
#define     DIV         1

address     *gaddr;
int         my_index;
int         n_recd;
int         experimental;
int         port_no;
extern      stream, use_rpc;
void        join(), receive(), change();
void	    display_input(), display_all(), display(), display_title();
void        display_mainloop();
void        do_xfer(), get_xfer(), pick(), error();
void        XParseGeometry();

void
main (argc, argv)
  int   argc;
  char  **argv;
{
    int     x, y, w, h;
    char  *displayname;
    char  *fontname;

    port_no = 0;
    x = y = 0;
    nstopped = 0;
    displayname = "";
    fontname = "8x13";  /* Boring font which most people should have!! */

    /* Read in command line arguments */
    while (argc-- > 1)
        if (**++argv >= '0' && **argv <= '9')
            port_no = atoi (*argv);
        else if (**argv == '-')
            switch ((*argv)[1])
            {
              case 'x':
		++experimental;
		break;

              case 'g':
                XParseGeometry(&((*argv)[2]), &x, &y, &w, &h);
                /* Actually we ignore w and h. */
                break;

              case 'd':
                displayname = &((*argv)[2]);
                break;
                
              case 'f':
                fontname = &((*argv)[2]);
                break;

              default:
                printf ("grid: invalid option %c\n", (*argv)[1]);
                fflush (stdout);
                break;
            }

    /* Initialize */
    SRANDOM (getpid());
    display_mainloop(displayname, fontname, x, y);
}

/* Join (or create) group "grid"; transfer state if joining */
void
join()
{
    /* Initialize and run ISIS */
    isis_init (port_no); 
    isis_task (join, "join"); 
    isis_entry (RECEIVE, receive, "receive"); 

    gaddr = pg_join ("grid",
                     PG_MONITOR, change, 0,
                     PG_XFER, 0, do_xfer, get_xfer,
                     0);
    if (addr_isnull (gaddr))
        error ("grid: join failed");
    isis_start_done();
}



/* Pick a random update; transmit it to the group */
void
update()
{
    int                 x, y, op, arg, type;
    extern              bypass_in_use;

loop:
    begin
    {
        if (!go)
        {
            ++nstopped;
            return;
        }
        pick (&x, &y, &op, &arg);
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
          int rval[20];

          case AB_MODE:
            abcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, use_rpc? ALL: 0, "%d", rval);
            break;

          case CB_MODE:
            cbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, use_rpc? ALL: 0, "%d", rval);
            break;

          case FB_MODE:
            fbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, use_rpc? ALL: 0, "%d", rval);
            break;

          case MB_MODE:
            mbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, use_rpc? ALL: 0, "%d", rval);
            break;

          case GB_MODE:
            gbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, use_rpc? ALL: 0, "%d", rval);
            break;
    
          default:
            fbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, use_rpc? ALL: 0, "%d", rval);
            break;
        }
	if(!use_rpc)
	    return;
        if (stream && my_index == 0)
            goto loop;
        else if (!stream && n_recd >= n_memb)
        {
            n_recd = 0;
            goto loop;
        }
    }
}



/* Receive an update message; perform update; do a new update if */
/* a remote update has been received                             */
void
receive (mp)
  register message  *mp;
{
    int         x, y, op, arg;

    ++n_recd;
    msg_get (mp, "%l%l%l%l", &x, &y, &op, &arg);

    switch (op)
    {
      case ADD:
        value[x][y] += arg;
        break;

      case DIV:
        value[x][y] = value[x][y] / 2;
        break;

      default:
        error ("grid: received invalid operation %d", op);
    }
    color[x][y] = (color[x][y] ? 0 : 1);

    display (x, y);

    if(use_rpc == 0)
    {
        if (stream && my_index == 0)
            update();
        else if (!stream && n_recd >= n_memb)
        {
            n_recd = 0;
            update();
        }
    }
    else if(msg_getid(mp))
	reply(mp, "%d", my_process_id);

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
    display_title();
    t_fork(update, 0);
}



/* Do state transfer; send current values in table */
void
do_xfer (loc)
  int   loc;
{
    if (loc == -1)
        xfer_out (loc, "%L%C%l%l%l", value[0], sizeof (value) / sizeof (int),
	  color[0], sizeof (color), mode, stream, use_rpc);
}


/* Receive transferred state; store in table */
void
get_xfer (loc, mp)
  int       loc;
  message   *mp;
{
   msg_get (mp, "%L%C%l%l%l", value[0], (int *) 0, color[0], (int *) 0,
		&mode, &stream, &use_rpc);
   display_all();
}   



/* Randomly pick a point in the grid, an operation, and an argument */
void
pick (x, y, op, arg)
  int   *x, *y, *op, *arg;
{
    if (mode == CB_MODE || mode == FB_MODE)
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

    

/* Print out error message and exit */
void
error (fmt, a0, a1, a2, a3)
  int a0, a1, a2, a3;
  char *fmt;
{
    printf (fmt, a0, a1, a2, a3);
    printf ("\n");
    fflush (stdout);
    exit(0);
}
