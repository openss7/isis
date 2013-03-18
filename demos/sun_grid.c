/*  $RCSfile: sun_grid.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:45 $  */
/*
 * This illustrates a program that might form the basis for a distributed
 * spreadsheet computation, broadcast based...
 */

#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include <sys/file.h>

#include "isis.h"

#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include <suntool/menu.h>

#define	begin
#define	forever		for(;;)


int		PIX_BLACK, PIX_WHITE, PIX_INVERT, PIX_REVERSE, PIX_OR, PIX_COPY;
int		suntools_loaded;

#define 	scrheight	(8*40+100)
#define 	height		(8*40)
#define		width 		(8*60)

Pixrect	*basepr;
Pixwin *gridpw, *panelpw;
Canvas 			*grid_canvas;
Panel	 		*grid_panel;
Pixfont			*screen_r_11;
Frame			*base_frame;
int			port_no, my_index, disp_off, nstopped = 1, verbose;

void control_panel_layout(), CLIP_ON(), draw_grid();

void blat(), pick();
int srandom(), random();
int  atoi(), getpid(), printf(), fflush(), _exit();
void isis_task(), isis_entry(), isis_init(), panic();


#define	RECEIVE		1

void
main(argc, argv)
  int argc;
  char **argv;
  {
	static join();
        void receive();
	char BANNER[132];

	sprintf(BANNER, "ISIS sun_grid demo (pid %d)", getpid());

	screen_r_11 = pf_open("/usr/lib/fonts/fixedwidthfonts/screen.r.11");
	base_frame = (Frame*)window_create(NULL, FRAME,
	    FRAME_LABEL,	BANNER,
	    WIN_FONT,		screen_r_11,
	    WIN_WIDTH,		width+10,
	    WIN_HEIGHT,		scrheight,
	    WIN_X,		2,
	    WIN_Y,		2,
            WIN_ERROR_MSG,      "Cannot create frame",
	    0);
	if(base_frame == (Frame*)0)
		panic("Couldn't create grid frame\n");
	basepr = ((Pixwin*)base_frame)->pw_prretained;

	grid_panel = (Panel*)window_create(base_frame, PANEL,
	    WIN_FONT,		screen_r_11,
	    WIN_WIDTH, 		width,
	    WIN_HEIGHT, 	height/4,
	    WIN_X,		0,
	    WIN_Y,		height+4,
	    PANEL_BLINK_CARET,	FALSE,
	    0);
	if(grid_panel == (Panel*)0)
		panic("Couldn't create grid control panel\n");

	panelpw = (Pixwin*)window_get(grid_panel, WIN_PIXWIN);

	grid_canvas = (Canvas*)window_create(base_frame, CANVAS,
	    WIN_FONT,			screen_r_11,
	    WIN_BELOW,		        grid_panel,
	    WIN_WIDTH, 			width,
	    WIN_HEIGHT,		 	height,
	    WIN_X,			0,
	    WIN_Y,			0,
	    0);
	if(grid_canvas == (Canvas*)0)
		panic("Couldn't create grid display canvas\n");

	gridpw = canvas_pixwin(grid_canvas);
	control_panel_layout();
	CLIP_ON();
	draw_grid();
	while(--argc) switch(**++argv)
	{
	  case '-':
		++verbose;
		continue;
	  default:
		port_no = atoi(*argv);
	}
        srandom (getpid());
        /* Initialize and run ISIS */
        isis_task (join, "join");
        isis_entry (RECEIVE, receive, "receive");
        isis_init_suntools(port_no);
	run_tasks();
	window_main_loop(base_frame);
	exit(0);
  }

void
sgprint(fmt, a, c, d, e, f, g, h, i, j, k)
  char *fmt;
  int a, c, d, e, f, g, h, i, j, k; 
  {
	if(verbose)
	    printf(fmt, a, c, d, e, f, g, h, i, j, k);
  }

void
draw_grid()
  {
	register i, j;
	/* Tries to draw lines */
	for(i = 1; i < 8; i++)
	    pw_rop(gridpw, width/8*i, 0, 1, height, PIX_BLACK, (Pixrect*)0, 0, 0);
	for(i = 1; i < 8; i++)
	    pw_rop(gridpw, 0, height/8*i, width, 1, PIX_BLACK, (Pixrect*)0, 0, 0);
	for(i = 0; i < 8; i++)
	    for(j = 0; j < 8; j++)
		blat(i, j);
  }

void
CLIP_ON()
  {
        PIX_BLACK = PIX_SRC|PIX_COLOR(1);
        PIX_WHITE = PIX_SRC|PIX_COLOR(0);
        PIX_INVERT = (PIX_SRC^PIX_DST);
        PIX_REVERSE = PIX_NOT(PIX_DST);
        PIX_OR   = PIX_SRC|PIX_DST;
        PIX_COPY = PIX_SRC;
  }

void
CLIP_OFF()
  {
        PIX_BLACK = PIX_SRC|PIX_COLOR(1)|PIX_DONTCLIP;
        PIX_WHITE = PIX_SRC|PIX_COLOR(0)|PIX_DONTCLIP;
        PIX_INVERT = (PIX_SRC^PIX_DST)|PIX_DONTCLIP;
        PIX_REVERSE = PIX_NOT(PIX_DST)|PIX_DONTCLIP;
        PIX_OR   = PIX_SRC|PIX_DST|PIX_DONTCLIP;
        PIX_COPY = PIX_SRC;
  }

typedef struct button_info button_info;

struct  button_info
{
        char            *bu_text;
        int             (*bu_proc)();
        int             bu_x, bu_y;
};

void bcast_proc(), go_proc(), disp_proc();
static  quit_proc(), join();

button_info     buttons[]
={
        { "join", join, PANEL_CU(0), PANEL_CU(0) },
        { "quit", quit_proc, PANEL_CU(30), PANEL_CU(0) },
	0
};

Panel_item Members, Speed, StopGo, Bcast;

void
control_panel_layout()
  {
        register button_info *bi = buttons;
 
        while(bi->bu_text)
        {
            (void)panel_create_item(grid_panel, PANEL_BUTTON,
                PANEL_LABEL_IMAGE, panel_button_image(grid_panel, bi->bu_text, 8, screen_r_11),
                PANEL_NOTIFY_PROC, bi->bu_proc,
                PANEL_LABEL_X, bi->bu_x,
                PANEL_LABEL_Y, bi->bu_y, 0);
            ++bi;
        }
	Bcast = panel_create_item(grid_panel, PANEL_CHOICE,
	    PANEL_LABEL_STRING, "Broadcast type: ",
	    PANEL_CHOICE_STRINGS, "mcast", "cbcast", "abcast", "gbcast", 0,
	    PANEL_NOTIFY_PROC, bcast_proc,
	    PANEL_ITEM_X, PANEL_CU(0),
	    PANEL_ITEM_Y, PANEL_CU(1),
	0);
	StopGo = panel_create_item(grid_panel, PANEL_CHOICE,
	    PANEL_CHOICE_STRINGS, "stop", "start", 0,
            PANEL_CHOICE_IMAGES,
                 panel_button_image(grid_panel, "stop", 8, screen_r_11),
                 panel_button_image(grid_panel, "start", 8, screen_r_11), 0,
	    PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
	    PANEL_NOTIFY_PROC, go_proc,
	    PANEL_ITEM_X, PANEL_CU(10),
	    PANEL_ITEM_Y, PANEL_CU(0),
	0);
	(void)panel_create_item(grid_panel, PANEL_CHOICE,
            PANEL_CHOICE_IMAGES,
                 panel_button_image(grid_panel, "disp", 8, screen_r_11),
                 panel_button_image(grid_panel, "quiet", 8, screen_r_11), 0,
	    PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
	    PANEL_NOTIFY_PROC, disp_proc,
	    PANEL_ITEM_X, PANEL_CU(20),
	    PANEL_ITEM_Y, PANEL_CU(0),
	0);
	Members = panel_create_item(grid_panel, PANEL_MESSAGE,
	    PANEL_LABEL_STRING, "Ready to join...",
            PANEL_ITEM_X, PANEL_CU(0),
            PANEL_ITEM_Y, PANEL_CU(2)+2,
            0);
	Speed = panel_create_item(grid_panel, PANEL_SLIDER,
	    PANEL_LABEL_STRING, "Updates/sec: ",
	    PANEL_SLIDER_WIDTH, width/2,
	    PANEL_VALUE_FONT, screen_r_11,
	    PANEL_SHOW_VALUE, TRUE,
	    PANEL_SHOW_RANGE, TRUE,
	    PANEL_MIN_VALUE, 0,
	    PANEL_MAX_VALUE, 250,
            PANEL_ITEM_X, PANEL_CU(0),
            PANEL_ITEM_Y, PANEL_CU(3),
            0);
  }

int	my_rank;

void
display_title(nmemb, rank)
  int nmemb, rank;
  {
	char string[120];
	sprintf(string, "%d members, my rank is %d\n", nmemb, rank);
        panel_set(Members, PANEL_LABEL_STRING, string, 0);
  }

#define     ADD         0
#define     DIV         1

#define     FB_MODE     0
#define     CB_MODE     1
#define     AB_MODE     2
#define     GB_MODE     3
#define     DI_MODE     4

int     value[8][8];
char    color[8][8], oldcolor[8][8];
int     mode;
int     n_memb;
int     go;
address	*gaddr, member_list[32];

void
print_value()
  {
	register i;
	for(i = 0; i < 8; i++)
	{
	    register *v = value[i];
	    register char *c = color[i];
	    sgprint("%.2d.%d %.2d.%d %.2d.%d %.2d.%d ", v[0], c[0],  v[1], c[1], v[2], c[2], v[3], c[3]);
	    sgprint("%.2d.%d %.2d.%d %.2d.%d %.2d.%d\n", v[4], c[4], v[5], c[5], v[6], c[6], v[7], c[7]);
	}
  }

void
bcast_proc(item, value, event)
  Panel_item item;
  int value;
  Event *event;
  {
	mode = value;
  }

void
disp_proc(item, value, event)
  Panel_item item;
  int value;
  Event *event;
  {
	if(disp_off && (disp_off = value) == 0)
	    draw_grid();
	else
	    disp_off = value;
  }

void
go_proc(item, value, event)
  Panel_item item;
  int value;
  Event *event;
  {
	if(!go && (go = value) && !addr_isnull(gaddr))
	{
	    void update();
	    /* Fork off one update per update this version stopped */
	    while(nstopped-- > 0)
                if(nstopped)
		    isis_timeout(500*nstopped, update, 0);
                else
	            t_fork(update, 0);
	    nstopped = 0;
	    run_tasks();
	}
	else
	    go = value;
  }

static quit_proc()
  {
	exit(0);
  }

/* Called every 10 updates */
void
show_speed(speed)
  int speed;
  {
	static struct timeval old_time;
	static first;
	static oldspeed;
	if(first++ < 2)
	    gettimeofday(&old_time, (struct tzp*)0);
	else
	{
            struct timeval cur_time;
            register time;
            gettimeofday(&cur_time, (struct tzp*)0);
            time = (cur_time.tv_sec-old_time.tv_sec)*1000 + cur_time.tv_usec/1000;
	    speed = speed*1000/time;
	    if(oldspeed == 0)
	        oldspeed = speed;
	    speed = (oldspeed*3 + speed)/4;
	    panel_set_value(Speed, speed);
	    old_time = cur_time;
	    oldspeed = speed;
	}
  }


/* Join (or create) group "grid"; transfer state if joining */
static join()
  {
        void do_xfer(), get_xfer(), change();
	static joined;
	if(joined++)
	    return;
        isis_task(change, "sun_grid:change");
	sgprint("join: calling pg_join\n");
        gaddr = pg_join ("grid", PG_MONITOR, change, 0, PG_XFER, 0, do_xfer, get_xfer, 0);
        if (addr_isnull (gaddr))
            panic ("grid: join failed");
	show_speed();
	sgprint("join: done\n");
        isis_start_done();
  }



/* Pick a random update; transmit it to the group */
void
update()
  {
        int x, y, op, arg;

        if (!go)
	{
	    ++nstopped;
            return;
	}

        pick (&x, &y, &op, &arg);
	sgprint ("update: %d %d %d %d\n", x, y, op, arg);
        switch (mode)
        {
          case FB_MODE:
            fbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, 0);
            break;

          case AB_MODE:
            abcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, 0);
            break;

          case CB_MODE:
            cbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, 0);
            break;

          case GB_MODE:
            gbcast (gaddr, RECEIVE, "%l%l%l%l", x, y, op, arg, 0);
            break;
            break;
        }
}



/* Receive an update message; perform update; do a new update if */
/* a remote update has been received                             */
void
receive (mp)
  register message  *mp;
  {
        int         x, y, op, arg;
        static int  n_recd, count;

        msg_get (mp, "%l%l%l%l", &x, &y, &op, &arg);
        sgprint ("receive:             %d %d %d %d\n", x, y, op, arg);

        switch (op)
        {
          case ADD:
            value[x][y] += arg;
            break;

          case DIV:
            value[x][y] = value[x][y] / 2;
            break;

          default:
            panic ("grid: received invalid operation %d", op);
        }
        color[x][y] = (color[x][y] ? 0 : 1);
        blat (x, y);
        if (++n_recd >= n_memb)
        {
            n_recd = 0;
            update();
        }
	if(++count == 50)
	{
	    show_speed(count);
	    count = 0;
	}
  }

void
blat(i, j)
  int i, j;
  {
	char string[20];
	if(disp_off)
	    return;
	sprintf(string, "%2d", value[i][j]);
	if(oldcolor[i][j])
	    pw_writebackground(gridpw, 2+width/8*i, 2+height/8*j, width/8-3, height/8-3, PIX_WHITE);
	pw_text(gridpw, width/8*i+width/16, height/8*j+height/16, PIX_BLACK, screen_r_11, string);
	if(color[i][j])
	    pw_writebackground(gridpw, 2+width/8*i, 2+height/8*j, width/8-3, height/8-3, PIX_REVERSE);
	oldcolor[i][j] = color[i][j];
  }


/* If group membership changes, change the value of n_memb and my_index */
void
change (pg, arg)
  register groupview    *pg;
  int                   arg;
  {
	register address *ap, *bp;
	gaddr = &pg->gv_gaddr;
        if((n_memb = pg->gv_nmemb) == 0)
	    panic("join failed");
        my_index = pg_rank (gaddr, &my_address);
	sgprint ("change: n_memb = %d, my_index = %d\n", n_memb, my_index);
	print_value();
	bp = member_list;
	*bp = my_address;
	bp++->addr_entry = RECEIVE;
	for(ap = pg->gv_members; !addr_isnull(ap); ap++)
	    if(!addr_ismine(ap))
	    {
		*bp = *ap;
	        bp++->addr_entry = RECEIVE;
	    }
	*bp = NULLADDRESS;
        display_title(n_memb, my_index);
        update();
  }



/* Do state transfer; send current values in table */
void
do_xfer (loc)
  int   loc;
  {
	sgprint("do_xfer:\n");
	print_value();
        if (loc == -1)
            xfer_out (loc, "%L%C%d%d", value[0], sizeof (value) / sizeof (int), color[0], sizeof (color), mode, go);
	sgprint("do_xfer: done\n");
  }


/* Receive transferred state; store in table */
void
get_xfer (loc, mp)
  int       loc;
  message   *mp;
  {
         msg_get (mp, "%L%C%d%d", value[0], (int *) 0, color[0], (int *) 0, &mode, &go);
	 sgprint("get_xfer:\n");
	 print_value();
	 panel_set_value(StopGo, go);
	 panel_set_value(Bcast, mode);
	 draw_grid();
	 sgprint("get_xfer: done\n");
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
            xy = ((random() & 0xffff) % n_for_me) * n_memb + my_index;
            *x = (xy & 070) >> 3;
            *y = xy & 07;
        }
        else
        {
            *x = random() & 0x7;
            *y = random() & 0x7;
        }
        *op = random() & 0x1;
        *arg = random() & 0xf;
  }
