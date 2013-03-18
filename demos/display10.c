/*  $RCSfile: display10.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:24:02 $  */
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
#include    "isis.h"		/* Needed to get define for exit to avoid SUN lwp bug. */
#include    <stdio.h>
#include    <signal.h>
#include    <X/Xlib.h>

char name[19];
int namelen;
FontInfo *font;
short width, height;
int twidth, theight, titleheight;
int joined = 0;
Window win;

#include <string.h>
int gethostname();
int XMapWindow(), XMoveWindow(), XSelectInput();
int XPixSet(), XText();
int XFlush(), XNextEvent();
void start(), display_all(), display_title();

int
init_display(displayname, fontname, x, y)
	char *displayname, *fontname;
	int x, y;

  /* Arguments are just for compatibility with display11.c, we ignore them. */
{
	char def[20];
	OpaqueFrame frame;

	print("display: %s, font: %s, x: %d, y: %d\n", displayname, fontname, x, y);
	gethostname(name, 19);
	for (namelen = 0; name[namelen] && name[namelen] != '.' && namelen < 18; namelen++)
		continue;
	name[namelen] = '\0';

	XOpenDisplay(displayname);
	font = XOpenFont(fontname);
	width = font->width;
	height = font->height;
	twidth = 48 * width + 6;
	titleheight = 3 * height + 1;
	theight = titleheight + 24 * height + 6;
	sprintf(def, "=%dx%d", twidth, theight);
	frame.x = x;		/* Doesn't seem to do anything 'cos WM gets overrides us. */
	frame.y = y;
	frame.bdrwidth = 2;
	frame.border = BlackPixmap;
	frame.background = WhitePixmap;
	win = XCreate(name, "grid", "", def, &frame, twidth, theight);
	XMapWindow(win);
	if (x != 0 && y != 0) {	/* Hack to move window where I want it. */
		XMoveWindow(win, x, y);
	}
	XSelectInput(win, ExposeRegion | ButtonPressed);
	display_all();
	start();
	return (dpyno());
}

void
display(x, y)
	int x, y;
{
	char data[7];
	int fore, back;
	int dlen;
	register int bx, by;

	if (color[x][y]) {
		fore = WhitePixel;
		back = BlackPixel;
	} else {
		fore = BlackPixel;
		back = WhitePixel;
	}
	sprintf(data, "%d", value[x][y]);
	dlen = strlen(data);
	bx = 6 * x * width + x;
	by = titleheight + 3 * y * height + y;
	XPixSet(win, bx, by, 6 * width, 3 * height, back);
	XText(win, bx + (6 - dlen) * width / 2, by + height, data, dlen, font->id, fore, back);
	XFlush();
}

void
display_all()
{
	register int i, j;

	XPixSet(win, 0, titleheight - 1, twidth, 1, BlackPixel);
	for (i = 1; i < 8; i++)
		XPixSet(win, 0, titleheight + 3 * i * height + i - 1, twidth, 1, BlackPixel);
	for (i = 1; i < 8; i++)
		XPixSet(win, 6 * i * width + i - 1, 3 * height, 1, theight, BlackPixel);
	display_title();
	for (i = 0; i < 8; i++)
		for (j = 0; j < 8; j++)
			display(i, j);
}

void
display_title()
{
	register int i;
	register char *str;
	char string[13];

	XText(win, width, height, name, namelen, font->id, BlackPixel, WhitePixel);

	if (joined && n_memb > 0) {
		sprintf(string, "(%d member%s)", n_memb, (n_memb == 1 ? "" : "s"));
		for (i = strlen(string); i < 12; i++)
			strcat(string, " ");
		XText(win, 18 * width + 3, height, string, 12, font->id, BlackPixel, WhitePixel);
	}

	str = (mode == AB_MODE ? "abcast" : (mode == CB_MODE ? "cbcast" :
					     (mode == GB_MODE ? "gbcast" : "random")));
	XText(win, 30 * width + 5, height, str, 6, font->id, BlackPixel, WhitePixel);

	str = (joined ? "exit" : "join");
	XText(win, 37 * width + 6, height, str, 4, font->id, BlackPixel, WhitePixel);

	str = (go ? "stop" : " go ");
	XText(win, 43 * width + 7, height, str, 4, font->id, BlackPixel, WhitePixel);
	XFlush();
}

void
display_input()
{
	XEvent event;

	do {
		XNextEvent(&event);
		if (event.window == win)
			if (event.type == ExposeRegion || event.type == ExposeWindow)
				display_all();
			else if (event.type == ButtonPressed)
				if ((((XButtonEvent *) & event)->y < titleheight) &&
				    (((XButtonEvent *) & event)->x > 36 * width + 6))
					if (((XButtonEvent *) & event)->x > 42 * width + 7) {
						go = (go ? 0 : 1);
						display_title();
						if (go) {
							while (nstopped--)
								if (nstopped)
									isis_timeout(500 * nstopped,
										     update, 0,
										     NULLARG);
								else
									t_fork(update, 0);
						}
					} else
						exit(0);
	}
	while (QLength() != 0);
}

void
start()
{
	XEvent event;

	while (!joined) {
		XNextEvent(&event);
		if (event.window == win)
			if (event.type == ExposeRegion || event.type == ExposeWindow)
				display_all();
			else if (event.type == ButtonPressed)
				if (((XButtonEvent *) & event)->y < titleheight)
					if ((((XButtonEvent *) & event)->x > 30 * width + 5) &&
					    (((XButtonEvent *) & event)->x < 42 * width + 6)) {
						if (((XButtonEvent *) & event)->x < 36 * width + 5)
							mode = (mode == AB_MODE ? CB_MODE :
								(mode == CB_MODE ? GB_MODE :
								 (mode ==
								  GB_MODE ? NO_MODE : AB_MODE)));
						else
							joined = go = 1;
						display_title();
					}

	}
}
