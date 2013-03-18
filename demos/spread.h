/*  $RCSfile: spread.h,v $ $Revision: 2.17 $ $Date: 90/09/13 14:31:01 $  */
/*
 *	Originally coded by Robert Wisniewski
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#ifndef HPUX
/* X11/Xos.h is buggy for HP systems. */
#include <X11/Xos.h>
#endif
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/StringDefs.h>

#define MAX_SPDSHTS      100
#define MAX_SPDS_GRP     10
#define MAX_MENUS        10
#define NEWSPREAD        1
#define ADDTOGROUP       2
#define AB_MODE     0
#define CB_MODE     1
#define GB_MODE     2
#define NO_MODE     3
#define RECEIVE          1
#define GLOBALRECEIVE    2
#define MERGERECEIVE     3
#define SWITCHRECEIVE    4
#define SMALLFONT  "6x10"
#define NORMALFONT "8X13"
#define BIGFONT    "9x15"
#define BOLDFONT   "hack-n"

#if (HPUX)
#define RANDOM rand
#define SRANDOM srand
int srand(), rand();
#else
#define RANDOM random
#define SRANDOM srandom
int srandom(), random();
#endif

#define max(A,B)  ((A) > (B) ? (A) : (B))
#define min(A,B)  ((A) < (B) ? (A) : (B))

int value[8][8];
char color[8][8];
int mode;
int n_memb;
char go;
int my_index;
int fdes;

int parscol, parsrow, parsdep;		/* entry we wish to parse */
int parsrun, parsnum;			/* used for calculating my defined funct i.e., avg, min,
					   etc. */
char *parse_str;			/* string we are going to parse with */
double parse_val;			/* value returned from parsing */
int valid_parse;			/* indicates if everything was normal when we parsed */
int strloc;				/* the location so far in our parse string */

char display[10][10][20][32];		/* [depth][column][row][max numb chars per string] what is
					   displayed */
char entry[10][10][20][32];		/* actual entry into table i.e., formulas and titles */

char rname[32];
int row, column, depth, oldr, oldc, oldd, rrow, rcol, rdep;	/* normal old and received depth
								   column and row */
int dispdepth;				/* the depcth at which the particular spreadsheet displays */
int validentry[10][10][20];		/* matches entryval tell if the entry is valid 1 if valid 0 
					   if not valid */
							   /* int entryval[10][10][20]; *//* the value of the expression returned
							      by yacc */
double entryval[10][10][20];		/* the value of the expression returned by yacc */

struct element {			/* a doubly linked list */
	int depth, column, row;		/* depth, column and row this element is dependent on */
	struct element *prev;		/* pointer to previous element in list */
	struct element *next;		/* poinmter to next element in list */
};
struct element *frwd_dep[10][10][20];	/* dependency array indicating what cell d,i,j is
					   depenedent on */
struct element *dependency[10][10][20];	/* dependency array indicating which cells are dependent on 
					   d,i,j */
struct element *temp;			/* a temporary element */
struct element *del_dep;		/* possibly deleted dependencies */

struct listelem {			/* record to indicate a spreadsheet name, and wht group it
					   belongs to */
	char spreadname[32];		/* name of the spreadsheet */
	char groupname[32];		/* name of group that spreadname belongs to */
};
struct listelem group_list[MAX_SPDSHTS];

char mergename[32];			/* name of spreadsheet we are going to merge with */
char spreadgroup[32];			/* name of the spreagroup to which we belong */
char letter[300];			/* look up table for letters */
char dspdshtname[32];			/* what we will display i.e., spreadsheet name : spdshtname 
					 */
char spdshtname[32];			/* name of group user want to origionally join and display */
char spdsht_list[21][32];		/* list of spreadsheet which process depends on */
char dspdsht_list[21][32];		/* array to store the order the spreadsheets are displayed
					   in */
int numb_in_list;			/* number of spreadsheets in list but not yet read in */
int numb_spdshts;			/* number of spreadsheets in a particular group */
int total_numb_spdshts;			/* number of all the spreadsheets in all the groups */

#define ctl(c) ('c'&037)

char name[19];
int namelen;
Display *dpy;
GC gc;
int screen;
XFontStruct *font;
unsigned long black, white;
short width, height;
int twidth, theight, titleheight;
int bdrwidth;
Window win;
char buffer[20];
int bufsize;
int joined;

int merging;				/* boolean 1 if we are in the process of merging two spread 
					   grups */
int look;				/* easily used value to check if element exists in
					   dependency list */
int depend;				/* boolean 1 if we are assigning depnedencies */
int BoxHilight;				/* boolean 1 if highlighted */
int shiftkey, controlkey, escapekey;	/* boolean 1 if that key is still being pressed down */
int key;				/* keycode of the key just pressed */

#define makeNaN(X)	(((union { double d; struct { unsigned :1, e:11; } s; } \
			*)&X)->s.e = 0x7ff)

XEvent event;

int numb_menus;
struct menu_struct {
	int left;
	int width;
	char label[32];
	char sub_menu[MAX_MENUS][32];
	int numb_sub_menus;
} menu[5];
int menu_bot;
int menu_space;
int menu_margin;

/* some graphic global variables -could be constants but for flexibility
   we'll leave as variables */

int botwidth, rtwidth;			/* width of bottom and right side of spreadsheet where
					   additions to group are displayed */
int colwidth, rowwidth;			/* width of columns and rows */
int leftx, topy;			/* x,y coords of upper left point on screen */
int numbcols, numbrows;			/* number of columns and rows */
int botmargin, rtmargin;		/* the bottom and right margins on the display window */

int protected;				/* number of write protected spreadsheets in our group */

/* variables used for the demo mode
   make tile a gray scale */
int demo_mode;				/* if 0 is off, if 1 is on */
int run_demo;				/* if 0 is off, if 1 is on */
Pixmap tile, tile1;

#define gray_width 16
#define gray_height 16
#define gray_x_hot 8
#define gray_y_hot 8

/* a slightly darker gray */
#define gray1_width 16
#define gray1_height 16
#define gray1_x_hot 8
#define gray1_y_hot 8

struct demo_element {			/* singly linked list of celss changed by the demo_mode
					   being on */
	int depth, column, row;		/* depth, column and row of highlighted cell */
	struct demo_element *next;	/* poinmter to next demo_element in list */
};

struct demo_element *demo_list;

long delay_time;			/* the amount of time between auto updates in our demo
					   program */

/* functions from the grammer part of the program */
int yyparse();

/* functions from the isis or control part of the program */
void display_input(), join(), change();
void receive(), global_receive(), merge_receive(), switch_receive();
void send_data(), receive_data(), create_group();
void create_global(), send_global(), receive_global();
void add_to_global(), get_join_group(), join_spd();
void inform_group_change(), inform(), error(), compute_dependency();

/* functions from the graphical part of the program */
int init_display(), gethostname();
int find(), dsearch(), get_spdsht_depth(), access_new_spd();
int check_in_global(), get_menu();
void display_all(), ChangeFont(), display_title();
void start(), display_input(), change_display_level();
void proc_mouse_down(), disp_inverse_text(), proc_key_down();
void display_error();
void update_display(), recompute(), check_forward_dependencies();
void get_all_spreads(), read_new_spd(), init_var(), init_menus();
void save_file(), check_for_resize(), open_menu();
void display_demo_list(), clear_demo_list();
void run_demof(), draw_inverse_text();

#ifdef HPUX
long time();
#else
#   ifndef MACH
int time();
#   endif MACH
#endif				/* HPUX */

#ifdef SUN
int atoi(), printf(), fflush(), getpid(), fclose(), sprintf();
int fscanf(), fputs(), fputc(), fprintf();
#endif
