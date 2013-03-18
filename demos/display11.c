/*  $RCSfile: display11.c,v $ $Revision: 2.28 $ $Date: 90/09/15 20:36:01 $  */
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

#include    "grid.h"
#include    "isis.h"  /* Needed to get define for exit to avoid SUN lwp bug. */
#include    <stdio.h>
#include    <signal.h>
#include    <sys/time.h>
#include    <X11/Xlib.h>

char        name[19];
int         namelen;
Display     *dpy;
GC          gc;
int         screen;
XFontStruct *font;
int         black, white;
short       charwidth, charheight;
int         windwidth, windheight, titleheight;
int         bdrwidth;
int         joined = 0;         
int         experimental;
int         use_rpc = 0;
int         my_index = 0;
int	    stream = 0;
Window      win;

#include <string.h>
int gethostname();
int XSetFont(), XStoreName(), XMapRaised(), XMoveWindow(), XSelectInput();
int XSetForeGround(), XFillRectangle(), XSetBackground(), XDrawImageString();
int XSync();
int XFlush(), XNextEvent(), XSetForeground();
void start(), display_all(), display_title(), display_input(), join();

int main_loop_count;
pcount()
{
	signal(SIGUSR1, pcount);
	print("count %d\n", main_loop_count);
}
void
display_mainloop(displayname, fontname, x, y)
  char *displayname, *fontname;
  int x, y;
{
    void XTimeOut();
    struct timeval tenseconds;

    gethostname (name, 19);
    for (namelen = 0; name[namelen] && name[namelen] != '.' && namelen < 18;
                                                                    namelen++)
        continue;
    name[namelen] = '\0';
        
    dpy = XOpenDisplay (displayname);
    screen = DefaultScreen(dpy);
    gc = DefaultGC(dpy, screen);
    black = BlackPixel(dpy, screen);
    white = WhitePixel(dpy, screen);
    if ((font = XLoadQueryFont (dpy, fontname)) == 0) {
        panic("can't find font");
    }
    XSetFont(dpy, gc, font-> fid);
    charwidth = font->max_bounds.rbearing - font->min_bounds.lbearing;
    charheight = font->ascent + font->descent;
    windwidth = 48 * charwidth + 6;
    titleheight = 3 * charheight + 1;
    windheight = titleheight + 24 * charheight + 6;
    bdrwidth = 2;

    win = XCreateSimpleWindow (dpy, RootWindow(dpy, screen),
                               x, y, windwidth, windheight, bdrwidth,
                               black, white);
       /* The x and y positions here don't seem to do anything: the
          window manager grabs control of placement regardless. */
    XStoreName(dpy, win, name);
    XMapRaised (dpy, win);
    if (x != 0 && y != 0) { /* Hack to move window where I want it. */
        XMoveWindow (dpy, win, x, y);
    }
    XSelectInput (dpy, win, ExposureMask | ButtonPressMask);
    display_all();
    start ();
    join();
    forever {
	static struct timeval timeout ={ 1, 0};
	void XInputFlush();
	XInputFlush();
        isis_accept_events(ISIS_TIMEOUT, &timeout);
    }
}

void
XInputFlush()
  {
        while (XPending(dpy))
            display_input();
  }

void
display (x, y)
  int   x, y;
{
    char            data[7];
    int             fore, back;
    int             dlen;
    register int    bx, by;

    if (color[x][y])
    {
        fore = white;
        back = black;
    }
    else
    {
        fore = black;
        back = white;
    }
    sprintf (data, "%d", value[x][y]);
    dlen = strlen (data);
    bx = 6 * x * charwidth + x;
    by = titleheight + 3 * y * charheight + y;


    XSetForeground(dpy, gc, back);
    XFillRectangle(dpy, win, gc, bx, by, 6*charwidth, 3*charheight);
    XSetForeground(dpy, gc, fore);
    XSetBackground(dpy, gc, back);
    XDrawImageString(dpy, win, gc,
                     bx + (6 - dlen) * charwidth / 2,
                     by + charheight + font->ascent,
                     data, dlen);
    XSetForeground(dpy, gc, black);
    XSetBackground(dpy, gc, white);
    XFlush(dpy);
}


void
display_all()
{
    register int    i, j;

    XFillRectangle(dpy, win, gc, 0, titleheight - 1, windwidth, 1);
    for (i = 1; i < 8; i++)
        XFillRectangle(dpy, win, gc, 0,
                       titleheight+3*i*charheight+i-1, windwidth, 1);
    for (i = 1; i < 8; i++)
        XFillRectangle(dpy, win, gc,
                       6*i*charwidth+i-1, 3*charheight, 1, windheight);
    display_title();
    for (i = 0; i < 8; i++)
        for (j = 0; j < 8; j++)
            display (i, j);
}

void
display_title()
{
    register int    i;
    register char   *str; 
    char            string[64];

    str = (mode == FB_MODE ? "fbcast":
           (mode == AB_MODE ? "abcast" :
            (mode == CB_MODE ? "cbcast" : 
             (mode == MB_MODE ? "mbcast" :
              (mode == GB_MODE ? "gbcast" : "random")))));
    XDrawImageString(dpy, win, gc, 0*charwidth+5, charheight + font->ascent,
                     str, 6);
                                                                 
    if (joined && n_memb > 0)
    {
        sprintf (string, "(member %d/%d)", my_index, n_memb);
        for (i = strlen (string); i < 12; i++)
            strcat (string, " ");
        XDrawImageString(dpy, win, gc, 7*charwidth+3,
                         charheight + font->ascent,
                         string, 12);
    }

    if(experimental)
    {
        str = (stream ? "str+" : "str-");
        XDrawImageString(dpy, win, gc, 25*charwidth+6, charheight + font->ascent,
                         str, 4);

        str = (use_rpc ? "rpc+" : "rpc-");
        XDrawImageString(dpy, win, gc, 31*charwidth+6, charheight + font->ascent,
                         str, 4);
    }

    str = (my_index && stream)? "    ": (go ? "stop" : "run");
    XDrawImageString(dpy, win, gc, 37*charwidth+7, charheight + font->ascent,
                     str, 4);

    str = (joined ? "exit" : "join");
    XDrawImageString(dpy, win, gc, 43*charwidth+6, charheight + font->ascent,
                     str, 4);

    XFlush(dpy);
}

void
forkupdate()
{
	void update();
	t_fork(update, 0);
}


void
display_input()
{
    XEvent      event;

    XNextEvent (dpy, &event);
    if (((XButtonEvent *) &event)-> window == win)
        if (event.type == Expose)
        {
            display_all();
        }
        else if (event.type == ButtonPress)
        {
            if ((((XButtonEvent *) &event)->y < titleheight))
                if (((XButtonEvent *) &event)->x >= 43 * charwidth + 7)
                    exit(0);
                else if (((XButtonEvent *) &event)->x > 37 * charwidth + 7)
                {
                    go = (go ? 0 : 1);
                    display_title();
                    if (go)
                    {
                        if (nstopped > 0)
                        {
                            while(nstopped > 1)
                            {
                                isis_timeout(500*nstopped, forkupdate, 0, NULLARG);
                                nstopped--;
                            }
                            nstopped = 0;
                            forkupdate();
                        }
                    }
                }
        }
}


void
start()
{
    XEvent      event;
    register x;

    while (!joined)
    {
        XNextEvent  (dpy, &event);
        if (((XButtonEvent *) &event)-> window == win)
            if (event.type == Expose)
                display_all();
            else if (event.type == ButtonPress)
                if (((XButtonEvent *) &event)->y < titleheight)
		{
		    x = ((XButtonEvent *) &event)->x;
		    x = (x-5)/charwidth;
                    if (x < 48)
                    {
			/* [stream: 25-30 use_rpc 31-35] stop: 37-42 exit: 43-48 */
                        if (x < 10)
                            mode = (mode == AB_MODE ? CB_MODE :
                                     (mode == CB_MODE ? FB_MODE :
                                       (mode == FB_MODE ? MB_MODE :
                                         (mode == MB_MODE ? GB_MODE :
                                           (mode == GB_MODE ? NO_MODE : AB_MODE)))));
                        else if(x >= 25)
			{
			    if(experimental)
			    {
                                if (x < 31)
                                    stream ^= 1;
                                else if(x < 37)
				    use_rpc ^= 1;
                                else
                                    joined = go = 1;
			    }
			    else
			        joined = go = 1;
			}
                        display_title();
                    }
		}

    }
}
