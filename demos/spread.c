/*  $RCSfile: spread.c,v $ $Revision: 2.7 $ $Date: 90/09/13 14:30:30 $  */
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

char grid_rcsid[] = "$Source: /usr/fsys/isisfsys/b/isis/isisv2.1/demos/RCS/spread.c,v $$Revision: 2.7 $$Date: 90/09/13 14:30:30 $";

#include    "isis.h"
#include    "spread.h"
#include    <signal.h>
#include    <ctype.h>
#include    <string.h>
#include    <stdio.h>

/* debug (synchronous) X mode switch, non-zero = on */
/* #define _Xdebug 1 */

address     *globalgaddr;
address     *gaddr; 


void
main (argc, argv)
  int   argc;
  char  **argv;
{
    int     port_no;
    int     x, y;
    char    *displayname;
    char    *fontname;
    char    delay_str[3];

    port_no = 0;
    spdshtname[0] = '\0';
    x = y = 0;
    run_demo = 0;
    demo_mode = 0;
    bufsize = 20;
    joined = go =0;
    displayname = "";
    fontname = "8x13";  /* a font most people should have */
    strcpy(spdshtname, "globalspd");
    strcpy(dspdshtname,"spreadsheet name : ");
    strcat(dspdshtname, spdshtname);
    /* Read in command line arguments */
    while (argc-- > 1)
        if (**++argv >= '0' && **argv <= '9')
            port_no = atoi (*argv);
        else if (**argv == '-')
            switch ((*argv)[1])
            {
              case 'g': /* user wants to join global group */
                strcpy(spdshtname, "globalspd");
                strcpy(dspdshtname,"spreadsheet name : ");
                strcat(dspdshtname, spdshtname);
                break;

              case 'n': /* user wants a specific spreadsheet name */
                strcpy (spdshtname, &((*argv)[2]));
                if (strlen (spdshtname) > 10) {
                    printf("Spreadsheets names are limited to ten characters, yours was shortened\n");
                    spdshtname[11] = '\0';
                }
                strcpy(dspdshtname,"spreadsheet name : ");
                strcat(dspdshtname, spdshtname);
                break;
                
              case 'd':
                strcpy (delay_str,  &((*argv)[2]));
                delay_time = atoi(delay_str);
                run_demo = 1;
                demo_mode = 1;
                break;

              case 'f':
                fontname = &((*argv)[2]);
                break;

              default:
                printf ("spreadsheet: invalid option %c\n", (*argv)[1]);
                fflush (stdout);    
                break;
            }
     /* Initialize */
    fdes = init_display(displayname, fontname, x, y);
    SRANDOM (getpid());

    /* Initialize and run ISIS */
    isis_init (port_no); 
    isis_task (join, "join"); 
    isis_entry (RECEIVE, receive, "receive"); 
    isis_entry (GLOBALRECEIVE, global_receive, "global_receive");
    isis_entry (MERGERECEIVE, merge_receive, "merge_receive");
    isis_entry (SWITCHRECEIVE, switch_receive, "switch_receive");
    /* isis_input is called later in join_spd, this is to insure the
       user does not try to get a mouse click in before we have had a
       chance to read in all the spreadsheets. */
    /*isis_input (fdes, display_input, NULLARG); */
    isis_logging (1); 
    isis_mainloop (join, NULLARG); 
}  /* end function main */


/* join (or create) the global group that contains all the spreadsheets */
void
join()
{
    globalgaddr = pg_join ("spreadsheet",
                           PG_INIT, create_global,
                           PG_XFER, 0, send_global, receive_global,
                           0);
    get_join_group();
    add_to_global(spdshtname, spreadgroup);
    join_spd();
    if (run_demo == 1) {
        strcpy (menu[2].sub_menu[0], "turn off high-lighting");
        demo_mode = 1;
        t_fork(run_demof,NULL);
    }
}  /* end functino join */


/* add this particular spreadsheet to the global list */
void
add_to_global (sname, sgroup)
  char sname[32];
  char sgroup[32];
{
    abcast (globalgaddr, GLOBALRECEIVE, "%s%s", sname, sgroup, 0);
}  /* end function add to global */


/* get join group tells a process which particular group it should join to get 
   access to the data for its spreadsheet */
void
get_join_group()
{
    int   i,found;
    char  tempstr[32];
    char  join_group[32];
    FILE  *fp;

    found = 0;
    if (spdshtname[0] == '\0') 
        error ("spreadsheet name with 0 characters");
        /* sprintf(spreadgroup,"%s%d","default",RANDOM() & 0xff);  pick a random group */
    else {
        for (i = 0; i < total_numb_spdshts; i++) {
            if (strcmp(spdshtname, group_list[i].spreadname) == 0) {
                strcpy(spreadgroup,group_list[i].groupname);
                found = 1;
            }
        }
        if (found == 0 && spdshtname[0] != '\0') { /* we need to make a new group name */
            /* find out what group to join */
            fp = fopen(spdshtname,"r");
            if (fp == NULL) {
                strcpy(join_group,spdshtname);
            }
            else {
                fgets (join_group, 32 , fp);
                join_group[strlen(join_group) - 1] = '\0';
                fclose(fp);
            }
            strcpy(group_list[i].spreadname, spdshtname);
            strcpy(tempstr,"spread");
            strcpy(group_list[i].groupname, strcat(tempstr,join_group));
            strcpy(spreadgroup,group_list[i].groupname);
        }
    }
}  /* end function get join group */


/* create a global group that will tell individual processes which spreadgroup they need to join */
void
create_global()
{
    total_numb_spdshts = 0;
}  /* end function create global */


/* transfer the part of state dealing with group list and spreadlists - not spread cells */
void
send_global(loc, gaddr)
  int loc;
  address     *gaddr; 
{
    int i;
    
    for (i = 0; i < total_numb_spdshts; i++) {
        xfer_out(i, "%s%s", group_list[i].spreadname, group_list[i].groupname, 1);
    }
}  /* end function send global */


/* receive information on the global group lists and spread lists */
void
receive_global(loc, mp)
  int       loc;
  message   *mp;
{
    msg_get(mp, "%s%s", group_list[loc].spreadname, group_list[loc].groupname, (int *)0);
    total_numb_spdshts = loc + 1;
}  /* end function receive global */


/* Join (or create) group "spreadsheet name"; transfer state (spreadsheet cells) if joining 
   assumes that we have already assigned meaningful values to spreadgroup, spreadname */
void
join_spd()
{
    /*char tempname[32];*/
    
    gaddr = pg_join (spreadgroup,
                     PG_INIT, create_group,
                     PG_MONITOR, change, 0,
                     PG_XFER, 0, send_data, receive_data,
                     0);
    isis_start_done();   
    if (numb_spdshts == 0) /* if we started from scratch read in all spreadsheets */
        get_all_spreads(spdshtname);
    compute_dependency();
    if (spdshtname[0] != '\0') {
        dispdepth = get_spdsht_depth(spdshtname);
        if (dispdepth == -1)
            error("we should have found a spreadsheet");
    }
    change_display_level(dispdepth * botwidth + 600);
    
    /* printf("%s  my display depth is %d\n",spdshtname, dispdepth); */
    display_all();

    /* wait to here to call isis_input so as to prevent user from
       getting a mouse click in to early  see  main()  */
    isis_input (fdes, display_input, NULLARG);
   
    if (addr_isnull (gaddr))
        error ("spreadsheet: join failed");
}  /* end function join spread */
 

/* We are going to be joinging a new group, first tell the new group members we
   are going to be joining the tell all members in our group that we are going to 
   be joinging a new group, Each member will then respectively join the new group
   we don't have to explicitly send messages to everyone in our group to tell them
   since they will know aas we knew when we received a message that a certain cell
   needed a new spreadsheet membership in a group is going to change, either because
   a completely new spreadsheet has joined, or because there is going to be a merger
   of two groups that already contain spreadsheet(s) spdshtname wants to join the 
   group that add_name is in */
void
inform_group_change(add_name)
  char add_name[32];
{
    int  i;
    char answer[32];
    
    save_file(0);
    abcast (globalgaddr, MERGERECEIVE, "%s%s%d%d%d", spdshtname, 
            add_name, dispdepth, rcol, rrow, ALL, "%s", answer);
    /* find out name of group we need to switch too */
    for (i = 0; i < total_numb_spdshts; i++) {
        if (strcmp(group_list[i].spreadname, add_name) == 0)
            strcpy(spreadgroup, group_list[i].groupname);
    }
    join_spd();
}  /* end function inform group change */


/* if here we were informed by a member in our group that we needed to switch
   groups.  Therefore everyone in group must switch to the new group address */
void
switch_receive (mp)
  register message *mp;
{
    reply(mp,"%s","message received");
}  /* end swicth receive */


/* if a process receives this command and the sender wants to join the processor's
   group, then the processor needs to read in all the spread sheets associated with
   with the spreadsheet that is joining */
void
merge_receive (mp)
  register message  *mp;
{
    char  joining_spread[32],join_to_spread[32],join_group[32],answer[32];
    int   i;
    int   mdep, mcol, mrow, newdep, tempdepth;

    msg_get (mp, "%s%s%d%d%d", joining_spread, join_to_spread, &mdep, &mcol, &mrow);
    for (i = 0; i < total_numb_spdshts; i++) {
        if (strcmp(group_list[i].spreadname, join_to_spread) == 0)
            strcpy(join_group, group_list[i].groupname);
    }
    if (strcmp(spreadgroup, join_group) == 0) {
        tempdepth = numb_spdshts;
        access_new_spd(joining_spread);
        newdep = tempdepth + mdep;
        abcast (gaddr, RECEIVE, "%s%d%d%s", spdsht_list[newdep], 
                mcol, mrow, entry[newdep][mcol][mrow], ALL, "%s", answer);
    }
    display_all();
    reply(mp,"%s","message received");
}  /* end function merge receive */


/* if a new spreadsheet wants to join an existing group we have to update everybody's
   lists to indicate the new spreadsheet's group is different */
void
global_receive(mp)
  register message  *mp;
{
    char add_spread[32], add_group[32];
         
    msg_get (mp, "%s%s", add_spread, add_group);
    strcpy(group_list[total_numb_spdshts].spreadname, add_spread);
    strcpy(group_list[total_numb_spdshts].groupname, add_group);
    total_numb_spdshts = total_numb_spdshts + 1;
}  /* end function global receive */


/* inform group of a change made to the spreadsheet enrty[idep][icol][irow] */
void
inform(idep, icol, irow, ientry)
    int    icol, irow, idep;
    char   ientry[32];
{
    char answer[32];

    abcast (gaddr, RECEIVE, "%s%d%d%s", spdsht_list[idep], icol, irow, 
            entry[idep][icol][irow], ALL, "%s", answer);
} /* end functino inform */


/* Receive an Update Message about cell(dispdepth][icol][irow]from another spreadsheet 
   process that message by updating necessary cells and by displaying new contents 
   a note about receiveing depth: by receiving the name of the spreadsheet that was
   sending the message rather than the actual number of the depth, we can allow for
   different views of the same spreadsheet package, that is when this particular 
   spreadsheet j receives a message from spreadsheet spreadname it will determine what
   level the sending spreadsheet is in j's view and calculate off that*/
void
receive (mp)
  register message  *mp;
{
    int         size, val;
    struct demo_element *temp;
    char        rentry[32];  /* entry we are to receive */
        
    msg_get (mp, "%s%d%d%s", rname, &rcol, &rrow, rentry);
    rdep = get_spdsht_depth(rname);
    if (rdep == -1)
        error ("couldn't get the depth of an existing spreadsheet");
    
    strcpy(entry[rdep][rcol][rrow], rentry);
    strcpy(display[rdep][rcol][rrow], rentry);
    update_display(rdep,rcol,rrow);

    if (dispdepth == rdep) {
        XClearArea(dpy, win, 101+rcol*75, 101+rrow*25, 74, 24, False);
        if ((BoxHilight == 1)  &&  (oldr == rrow)  &&  (oldc == rcol)) {
            XClearArea(dpy, win, 101+oldc*75, 101+oldr*25, 74, 24, False);
            XClearArea(dpy, win, 200, 30, 800, 40, False);
            BoxHilight = 0;                   
        }
    }  /* end if dispdepth == rdep */

    size = strlen(display[rdep][rcol][rrow]);

    /* assign dependencies */
    depend = 1;
    parse_str = entry[rdep][rcol][rrow];
    /* since yyparse uses parsrow, parscol, parsdep, set them */
    strloc = 0;
    del_dep = NULL;
    /* set entry we wish to parse */
    parsdep = rdep;
    parscol = rcol;
    parsrow = rrow;
    parsnum = 0;
    val = yyparse();
    if (del_dep == NULL)
        /* there is no entry in tht cell anymore make entry val = NULL */
        validentry[rdep][rcol][rrow] = NULL;
    check_forward_dependencies(rdep,rcol,rrow);
    depend = 0;
    clear_demo_list();
    update_display(rdep,rcol,rrow);
     if (dependency[rdep][rcol][rrow] != NULL) 
        recompute(rdep,rcol,rrow);

    if (dispdepth == rdep) {
        XClearArea(dpy, win, 101+rcol*75, 101+rrow*25, 74, 24, False);
        if (demo_mode == 1 && (strlen(display[rdep][rcol][rrow]) > 0)) {
            /* insert a new element into the demo_list */
            temp = ((struct demo_element *) malloc(sizeof(struct demo_element))); 
            temp->next = demo_list;
            temp->depth = rdep;
            temp->column = rcol;
            temp->row = rrow;
            demo_list = temp;
            /* display in darker gray the inverse test */
            XSetTile(dpy, gc, tile1); 
            XSetFillStyle(dpy, gc, FillTiled);
            XFillRectangle(dpy, win, gc, 101+rcol*75, 101+rrow*25, 74, 24);
            XSetTile(dpy, gc, tile); 
            XSetFillStyle(dpy, gc, FillSolid);
            XSetForeground(dpy,gc, white);
            XSetBackground(dpy,gc, black);
            size = strlen(display[rdep][rcol][rrow]);
            XDrawImageString(dpy, win, gc, 110 + 75*rcol, 117 + 25*rrow, 
                             display[rdep][rcol][rrow], min(size,7));
            XSetForeground(dpy,gc, black);
            XSetBackground(dpy,gc, white);
        }
        else
            XDrawImageString(dpy, win, gc, 110 + 75*rcol, 117 + 25*rrow, 
                             display[rdep][rcol][rrow], min(size,7));
    }  /* end if dispdepth == rdep */
    
    XFlush(dpy);
    nullreply(mp);
    /* should include reply to message only if a consistent external
       picture is desired,  abcast guarantees a consistent internal picture */
    /*    reply(mp,"%s","message received");*/
}  /* end function receive */


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
} /* end function change */


/* Do state transfer of all the spreadsheet cells; send current values in table */
void
send_data (loc, gaddr)
  int   loc;
  address *gaddr;
{
    int i,j,k;
    
    for (i = 0; i < numb_spdshts; i++) {
        xfer_out(i, "%s", spdsht_list[i], 1);
    }
    for (i = 0; i < numb_spdshts; i++){
        for (j = 0; j < 10; j++) {
            for (k = 0; k < 20; k++) {
                xfer_out( (i*200) + (j*20) + k + MAX_SPDS_GRP, "%s%s%d%d", 
                         entry[i][j][k], display[i][j][k], 
                         entryval[i][j][k], validentry[i][j][k], 1);
            }
        }
    }
}  /* end function send data */


/* Receive transferred state(of spreadsheet cells); store in arrays */
void
receive_data (loc, mp)
  int       loc;
  message   *mp;
{
    int adjustloc;

    if (loc < MAX_SPDS_GRP) {
        msg_get(mp, "%s", spdsht_list[loc], (int *) 0);
        strcpy(dspdsht_list[loc],spdsht_list[loc]);
        numb_spdshts = loc +1;
        strcpy(spdsht_list[numb_spdshts], "####");
    }
    else {
        adjustloc = loc - MAX_SPDS_GRP;
        msg_get(mp, "%s%s%d%d", entry[adjustloc/200][(adjustloc%200)/20][adjustloc%20], 
                display[adjustloc/200][(adjustloc%200)/20][adjustloc%20], 
                &(entryval[adjustloc/200][(adjustloc%200)/20][adjustloc%20]), 
                &(validentry[adjustloc/200][(adjustloc%200)/20][adjustloc%20]), (int *)0);
        numb_spdshts = (adjustloc/200) + 1;
    }
}  /* end function receive data */


/* Create group allows a process to create a new group and reads from the appropriate files */
void
create_group (gaddr)
  address *gaddr;
{
    FILE *fp;
   
    if (merging == 1) {   
        get_all_spreads(mergename);
    }
    else {
        numb_spdshts = 1;
        if (spdshtname[0] == '\0') {
            error ("spreadsheet name with 0 characters");
            /* strcpy(dspdshtname," ");
            strcpy(spdsht_list[0],"default");
             strcpy(dspdsht_list[0],spdsht_list[0]);
            numb_spdshts = 1;*/
        }
        if (spdshtname[0] != '\0') {
            strcpy(dspdshtname,"spreadsheet name : ");
            strcat(dspdshtname, spdshtname);
            fp = fopen(spdshtname,"r");
            if (fp == NULL) {
                strcpy(spdsht_list[0],spdshtname);
                strcpy(dspdsht_list[0],spdsht_list[0]);
                strcpy(spdsht_list[1],"####");/* write termination into string */
                fclose(fp);
                numb_spdshts = 1; /* must be 1 since this is the only one */
                /* total_numb_spdshts = total_numb_spdshts + 1;*/
            }
            else {
                fclose(fp);
                numb_spdshts = 0;
            }
        }
    }
}  /* end function create group */


/* Print out error message and exit */
void
error (fmt, a0, a1, a2, a3)
  char *fmt;
  int a0, a1, a2, a3;
{
    display_error(strcat("**error**",fmt), 2);

    printf ("an error has occurred\n");
    printf (fmt, a0, a1, a2, a3);
    printf ("\n");
    fflush (stdout);
    exit(0);
}  /* end function error */


/* compute all the dependency relations for the process that just joined */
void
compute_dependency()
{
    int i,j,k,c,val,do_copy;
    char tempstr[33];

    for (i = 0; i < numb_spdshts; i++) {
        for (j = 0; j < 10; j++) {
            for (k = 0; k < 20; k++) {
                if (isdigit(entry[i][j][k][0]) || entry[i][j][k][0] == '-')
                {
                    do_copy = 1;
                    tempstr[0] = '=';
                    tempstr[1] = entry[i][j][k][0];
                    for (c = 1; c < strlen(entry[i][j][k]); c++)
                    {
                        if (!isdigit(entry[i][j][k][c]) && entry[i][j][k][c] != '.')
                        {
                            do_copy = 0;
                            break;
                        }
                        else
                            tempstr[c+1] = entry[i][j][k][c];
                    }
                    if (do_copy)
                    {
                        tempstr[c+1] = 0x0;
                        strcpy(entry[i][j][k], tempstr); 
                    }
                }

                if (entry[i][j][k][0] == '=') {
                      /* assign dependencies */
                    depend = 1;
                    parse_str = entry[i][j][k];
                    strloc = 0;
                    del_dep = NULL;
                    /* set entry we wish to parse */
                    parsdep = i;
                    parscol = j;
                    parsrow = k;
                    parsnum = 0;
                    val = yyparse();
                    if (del_dep == NULL)
                        /* there is no entry in tht cell anymore make entry val = NULL */
                        validentry[i][j][k] = NULL;
                    check_forward_dependencies(i,j,k);
                    depend = 0;
                    update_display(i,j,k);
                    if (dependency[i][j][k] != NULL)
                        recompute(i,j,k);
                }
            }
        }
    }
}  /* end function computer dependency */


/*******************************************************************
*  essentially this ends  group communicating procedures, now  the *
*  graphics processing procedures                                  *
*******************************************************************/

/* start up the spreadsheet going by getting the original window and by calling
   all other initializing routines */
int
init_display(displayname, fontname, x, y)
  char displayname[32], fontname[32];
  int x, y;
{

    gethostname (name, 19);
    for (namelen = 0; name[namelen] && name[namelen] != '.' && namelen < 18; namelen++)
        continue;
    name[namelen] = '\0';

    dpy = XOpenDisplay (displayname);
    if (dpy == NULL) {
        printf("Unable to establish xwindow connection, set display variable appropriately\n");
        exit(0);
    }

    screen = DefaultScreen(dpy);
    gc = DefaultGC(dpy, screen);
    black = BlackPixel(dpy, screen);
    white = WhitePixel(dpy, screen);

    init_var();
    ChangeFont(NORMALFONT);

    bdrwidth = 2;
    theight = topy + numbrows*rowwidth + botwidth + botmargin;
    twidth = leftx + numbcols*colwidth + rtwidth + rtmargin;
    win = XCreateSimpleWindow (dpy, RootWindow(dpy, screen),
                               x, y, twidth, theight, bdrwidth, black, white);
    XStoreName(dpy, win, name);
    XMapRaised (dpy, win);
    if (x != 0 && y != 0) { 
        XMoveWindow (dpy, win, x, y);
    }

    XSelectInput (dpy, win, ExposureMask | ButtonPressMask | KeyPressMask 
                          | KeyReleaseMask| ButtonMotionMask | ButtonReleaseMask);
    joined = go = 0;
    XClearArea(dpy, win, 200, 30, 800, 40, False);
    display_all();
    start();
    return (dpy-> fd);
}  /* end function init_display */


/* display all the content of the spreadsheet including title cells lines etc.*/
void
display_all()
{
    register int    i, j,size;
    char            n[3],l;

    n[0] = '0';
    l = 'A';

    for (i = 0; i <= numbrows; i++)
        XDrawLine(dpy, win, gc, leftx, topy+rowwidth*i, 
                  leftx+numbcols*colwidth, topy+rowwidth*i);
    for (i = 0; i < numbrows; i++) {
        sprintf(n,"%d",i);
	size = strlen(n);
	XDrawImageString(dpy, win, gc, 75, 115+25*i, n, size);
      }

    for (i = 0; i <= numbcols; i++)
        XDrawLine(dpy, win, gc, leftx+colwidth*i, topy, leftx+colwidth*i, 600);
    for (i=0; i < 10; i++) {
	XDrawImageString(dpy, win, gc, 130+75*i, 85, &(l), 1);
	l++;
    }

    for (i = 0; i < 10; i++) {
      for (j = 0; j < 20; j++) {
	size = strlen(display[dispdepth][i][j]);
	XDrawImageString(dpy, win, gc, 110 + 75*i, 117 + 25*j, 
                         display[dispdepth][i][j], min(size,7));
      }
    }
    /*XDrawRectangle(dpy,win,gc, 675,25,125,40);
    XDrawRectangle(dpy,win,gc, 676,26,123,38);*/

    ChangeFont(SMALLFONT);
    XSetFont(dpy, gc, font-> fid);
    for (i = 0; i < numb_spdshts; i++) {
        XDrawLine(dpy, win, gc, leftx + botwidth*i, 
                  topy + numbrows*rowwidth + botwidth*i, 
                  leftx + botwidth + botwidth*i, 
                  topy + numbrows*rowwidth + botwidth + botwidth*i); /* bot left diag */
        XDrawLine(dpy, win, gc, leftx + botwidth + botwidth*i, 
                  topy + numbrows*rowwidth + botwidth + botwidth*i, 
                  leftx + numbcols*colwidth + rtwidth + rtwidth*i, 
                  topy +numbrows*rowwidth + botwidth + botwidth*i); /* bot line */
        XDrawLine(dpy, win, gc, leftx + numbcols*colwidth + rtwidth*i, topy + rtwidth*i, 
                  leftx + numbcols*colwidth + rtwidth + rtwidth*i, 
                  topy + rtwidth + rtwidth*i); /* top right diag */
        XDrawLine(dpy, win, gc, leftx + numbcols*colwidth + rtwidth + rtwidth*i, 
                  topy + rtwidth + rtwidth*i, leftx + numbcols*colwidth + rtwidth + rtwidth*i,  
                  topy + numbrows*rowwidth + botwidth + botwidth*i); /* right line */
        XDrawLine(dpy, win, gc, leftx + numbcols*colwidth  + rtwidth*i, 
                  topy + numbrows*rowwidth + botwidth*i, 
                  leftx + numbcols*colwidth + rtwidth + rtwidth*i, 
                  topy + numbrows*rowwidth + botwidth + botwidth*i); /* bot right diag */
        size = strlen(dspdsht_list[i]);
        XDrawImageString(dpy, win, gc, leftx + botwidth + 2 + botwidth*i, 
                         topy + numbrows*rowwidth + 10 + botwidth*i, dspdsht_list[i], size);
    }
    ChangeFont(NORMALFONT);
    if (demo_mode == 1)
        display_demo_list(); 
    display_title();
}  /* end functino display_all */


void
ChangeFont(fname)
  char fname[32];
{
  if ((font = XLoadQueryFont (dpy, fname)) == 0) {
        error ("can't find font");
    }
    XSetFont(dpy, gc, font-> fid);
}  /* end function Change Font */


/* display the title part of the spreadsheet */
void
display_title()
{
    register int    i;
    char            string[13];

    XDrawImageString(dpy, win, gc, 1, 10, "spreadsheet group:", 18);
    XDrawImageString(dpy, win, gc, 151, 10, spreadgroup, strlen(spreadgroup));
    XDrawImageString(dpy, win, gc, 1, 24, dspdshtname, strlen(dspdshtname)); 
    XDrawImageString(dpy, win, gc, 1, 38, "members in group :", 18);
   
    if (joined == 0) {
        ChangeFont(BIGFONT);
        XDrawImageString(dpy, win, gc, 200, 50, "use start menu to begin", 23);
        ChangeFont(NORMALFONT);
    }

    XClearArea(dpy, win, 500, 10, 400, 20, False);
    if (run_demo == 1) {
        if (demo_mode == 1) {
            XDrawImageString(dpy, win, gc, 500, 20, "demo mode: Simulating and high-lighting", 39);
        }
        else {
            XDrawImageString(dpy, win, gc, 500, 20, "demo mode: Simulating", 21); 
        }
    }  /* end if run demo == 1 */
    else {
        if (demo_mode == 1) {
            XDrawImageString(dpy, win, gc, 500, 20, "demo mode: high-lighting", 24);
        }
        else {
            ;
        }
    } /* end else run demo == 0 */

    XDrawLine(dpy, win, gc, menu[0].left - menu_margin, 0 , menu[0].left - menu_margin, menu_bot);
    for (i = 0; i < numb_menus; i++) {
        XDrawImageString(dpy, win, gc, menu[i].left, 10, menu[i].label, strlen(menu[i].label));
        XDrawLine(dpy, win, gc, menu[i+1].left - menu_margin, 0 , 
                  menu[i+1].left - menu_margin, menu_bot);
        XDrawLine(dpy, win, gc, menu[i].left - menu_margin, menu_bot, 
                  menu[i+1].left - menu_margin, menu_bot);
    }
    if (joined && n_memb > 0) {
        sprintf (string, "%d", n_memb);
        XDrawImageString(dpy, win, gc, 151, 38, string, strlen(string));
    }                    

    if (joined == 1)
        XClearArea(dpy, win, 200, 30, 800, 40, False);

    XFlush(dpy);
}  /* end function display title */


void
start()
{
    XEvent      event;

    BoxHilight = 0;

    while (!joined) {
        XNextEvent  (dpy, &event);
        if (((XButtonEvent *) &event)-> window == win)
            if (event.type == Expose)
                display_all();
            else if (event.type == ButtonPress) {
                if ((((XButtonEvent *) &event)->y < (menu_bot)) &&
                    (((XButtonEvent *) &event)->x > (menu[0].left - menu_margin)) &&
                    (((XButtonEvent *) &event)->x < (menu[numb_menus].left - menu_margin))) {
                        open_menu(((XButtonEvent *) &event)->x);
                }
            }
    }
}  /* end function start */


/* display and process input - receive a new X Event and call appropriate function */
void
display_input()
{
    int       x,y,z;
    int       size;
    KeySym key;
    XComposeStatus compose;
    int charcount;

    do {
     
        XNextEvent (dpy, &event);
        z = event.type;
        buffer[1] = '\0';
	
	if (((XButtonEvent *) &event)-> window == win)
            if (event.type == Expose) {
                while (XCheckTypedEvent(dpy, Expose, &event))
                    ;
                display_all();
            }
	    else if (event.type == KeyPress) {
                charcount = XLookupString(&event.xkey, buffer, bufsize, &key, &compose);
                proc_key_down(key);
            }
                    
            else if (event.type == ButtonPress) {
                XClearArea(dpy, win, 200, 30, 800, 40, False);
                y = ((XButtonEvent *) &event)->y;
                x = ((XButtonEvent *) &event)->x;
	        if (BoxHilight == 1) {
                    inform(dispdepth, oldc, oldr, entry[dispdepth][oldc][oldr]); 
                }
                if ((x >= 100) && (y >= 100) && (x < 850) && (y < 600)) {
		    row = (y-100)/25;
		    column = (x-100)/75;
		    proc_mouse_down();
                }

                /* check if mouse was clicked in spreadsheet list at bottom */
                else if ((x > (leftx + (y - 600))) && 
                         (x < leftx + numbcols*colwidth + (y - 600)) &&
                         (y > topy + numbrows*rowwidth) &&
                         (y < topy + numbrows*rowwidth + botwidth*numb_spdshts))
                    change_display_level(y);

                /* check if mouse was click in menu bar */
                else if ((x > (menu[0].left - menu_margin)) &&
                         (x < (menu[numb_menus].left - menu_margin)) &&
                         (y < (menu_bot)))
                    open_menu(x);

		else {
                    BoxHilight = 0;
                    if (demo_mode == 0) {
                        XClearArea(dpy, win, 101+oldc*75, 101+oldr*25, 74, 24, False);
                        size = strlen(display[dispdepth][column][row]);
                        XDrawImageString(dpy, win, gc, 110 + 75*oldc, 117 + 25*oldr, 
                                         display[dispdepth][oldc][oldr], min(size,7));
                    }
                }
            }

        XFlush(dpy);/*if (z == 2){printf(" ");}*/
    }
    while (QLength(dpy) != 0);
}  /* end function display_input */


/* the mouse was clicked just beneath the main spreadsheet in the list of spreadsheets, 
   thus we need to change the display depth to be this new spreadsheet */
void
change_display_level(ycoord)
  int ycoord;
{
    int level;
    char tempname[32];

    clear_demo_list();
    level = (ycoord - 600) / botwidth;
    /* dispdepth = level;*/
       
    dispdepth = get_spdsht_depth(dspdsht_list[level]);
    strcpy(tempname, dspdsht_list[level]);
    strcpy(dspdsht_list[level],dspdsht_list[0]);
    strcpy(dspdsht_list[0],tempname);

    XClearArea(dpy, win, leftx, topy, leftx*numbcols, topy*numbrows, False);
    strcpy(dspdshtname,"spreadsheet name : ");
    strcat(dspdshtname, dspdsht_list[0]);

    XClearArea(dpy, win, leftx + 1 + botwidth*level, 
               topy + numbrows*rowwidth + 1 + botwidth*level, 82, 10, False);
    XClearArea(dpy, win, leftx + 1, 
               topy + numbrows*rowwidth + 1, 82, 10, False);

    /* make sure the area we are to print in is clear */
    XClearArea(dpy, win, 150, 0, 200, 40, False); 
    display_all();
} /* end function change_display_level */


/* if a mouse button press was detected we arrive here.  We have the x,y coords so take
   appropriate action */
void
proc_mouse_down()
{
    int size;
   
    if (BoxHilight) {
        XClearArea(dpy, win, 101+oldc*75, 101+oldr*25, 74, 24, False);
	size = strlen(display[dispdepth][oldc][oldr]);
	XDrawImageString(dpy, win, gc, 110 + 75*oldc, 117 + 25*oldr, 
                         display[dispdepth][oldc][oldr], min(size,7));
    }
       
    XFillRectangle(dpy, win, gc, 101+column*75, 101+row*25, 74, 24);
    disp_inverse_text(column,row);
    XClearArea(dpy, win, 200, 30, 800, 40, False);
    size = strlen(entry[dispdepth][column][row]);
    XDrawImageString(dpy, win, gc, 200, 50, entry[dispdepth][column][row], min(size,32)); 
    XFlush(dpy);
    BoxHilight = 1;
    oldr = row;
    oldc = column;
}  /* end function process mouse down */


/* simply display inverse text in the particular box we are appending.  This deal only
   with the current cell we are adding text to */
void
disp_inverse_text(c,r)
   int c,r;  /* the column, row we wish to display the inverse text entry in */
{
    int size;
    char dispentry[32];

    XSetForeground(dpy,gc, white);
    XSetBackground(dpy,gc, black);
    size = strlen(entry[dispdepth][c][r]);
    strcpy(dispentry, entry[dispdepth][column][row]);
    if (size > 7) {
        dispentry[6] = '\\';
        dispentry[7] = '\0';
    }  
    XDrawImageString(dpy, win, gc, 110 + 75*column, 117 + 25*row, 
                     dispentry, min(size,7)); 
    XSetForeground(dpy,gc,black);
    XSetBackground(dpy,gc,white);
}  /* end function display inverse text */


/* if a key down event was detected we arrive here, we know what key was pressed
   so determine what needs to occur */
void
proc_key_down(keysym)
  KeySym keysym;
{
    int size;
  
    size = strlen(entry[dispdepth][column][row]);
    
    if (keysym == XK_Return){
        if (BoxHilight == 1) 
            /* tell group about new entry */
            inform(dispdepth, oldc, oldr, entry[dispdepth][oldc][oldr]); 
    }
    if (keysym == XK_Delete){
        if (BoxHilight == 1) { /* if we are actually making an entry */
            entry[dispdepth][column][row][size-1] = '\0';
            XClearArea(dpy, win, 200, 30, 800, 40, False);
            size = strlen(entry[dispdepth][column][row]);
            XDrawImageString(dpy, win, gc, 200, 50, entry[dispdepth][column][row], min(size,32)); 
            XFillRectangle(dpy, win, gc, 101+column*75, 101+row*25, 74, 24);
            disp_inverse_text(column,row);
        }
    }
    if (((keysym >= XK_KP_Space) && (keysym <= XK_KP_9)) || 
           ((keysym >= XK_space) && (keysym <= XK_asciitilde))) {
        if (BoxHilight == 1) { /* if we are actually making an entry */
            entry[dispdepth][column][row][size] = buffer[0];
            entry[dispdepth][column][row][size+1] = '\0';
            size = strlen(entry[dispdepth][column][row]);
            XClearArea(dpy, win, 200, 30, 800, 40, False);
            XDrawImageString(dpy, win, gc, 200, 50, entry[dispdepth][column][row], min(size,32)); 
            disp_inverse_text(column,row);
        }
    }
}  /* end function process key down */


/* display an error message on the screen */
void
display_error (str, duration)
  char str[64];
  int duration;
{
    int       size;
    time_t    *testtime;
    long      temp_time;

    XClearArea(dpy, win, 200, 30, 800, 40, False);
    size = strlen(str);
    ChangeFont(BIGFONT);
    XDrawImageString(dpy, win, gc, 200, 58, str, min(size,64)); 
    ChangeFont(NORMALFONT);

    time(&testtime);
    temp_time = (long)testtime;
    while (time(&testtime) < (duration + temp_time))
        ;
}  /* end function display error */


/* update the elements in the display array by parsing the particular
   entry element that we just changed */
void
update_display(d,c,r)
  int   d,c,r; /* depth, column and row to update */
{
    int val;
    register i, do_copy;
    char tempstr[33];
    

    if (entry[d][c][r][0] == '\0')
        validentry[d][c][r] = NULL;

    if (isdigit(entry[d][c][r][0]) || entry[d][c][r][0] == '-')
        {
        do_copy = 1;
        tempstr[0] = '=';
        tempstr[1] = entry[d][c][r][0];
        for (i = 1; i < strlen(entry[d][c][r]); i++)
            {
            if (!isdigit(entry[d][c][r][i]) && entry[d][c][r][i] != '.')
               {
               do_copy = 0;
               break;
               }
            else
               tempstr[i+1] = entry[d][c][r][i];
            }
        if (do_copy)
           {
           tempstr[i+1] = 0x0;
           strcpy(entry[d][c][r], tempstr); 
           }
        }

    if (entry[d][c][r][0] == '=') {
        parse_str = entry[d][c][r];
	strloc = 0;
        valid_parse = 1;
        /* set entry we wish to parse */
        parsdep = d;
        parscol = c;
        parsrow = r;
        parsnum = 0;
        val = yyparse();
 
	if (val == 0 && valid_parse == 1) {
	    sprintf(display[d][c][r],"%.2f",parse_val);
	    entryval[d][c][r] = parse_val;
            validentry[d][c][r] = 1;  /* a vlid entry no exists */
	}
	else { /* display element = "error" */
            validentry[d][c][r] = NULL;
	    display[d][c][r][0] = 'e';
	    display[d][c][r][1] = 'r';
	    display[d][c][r][2] = 'r';
	    display[d][c][r][3] = 'o';
	    display[d][c][r][4] = 'r';
	    display[d][c][r][5] = '\0';
	}
    }
    else {
        strcpy(display[d][c][r],entry[d][c][r]);
    }
    if (strlen(display[d][c][r]) > 7) {
        display[d][c][r][6] = '\\';
        display[d][c][r][7] = '\0';
    }
} /* end function update_display */


/* since an entry cell was just chnaged and it had dependencies, recopmute all the cells
   that depended on the newly changed cell */
void
recompute(d,c,r)
int d,c,r; /* depth, column and row of the netry that was just changed */
{
    struct demo_element  *temp;
    struct element       *search;
    int                  cdep,ccol,crow,size;  /* curernt depth, column and row */

    search = dependency[d][c][r];
    while (search != NULL ) {
        cdep = search->depth;
        ccol = search->column;
        crow = search->row;
        update_display(cdep,ccol,crow);
        size = strlen(display[cdep][ccol][crow]);
        XClearArea(dpy, win, 101+ccol*75, 101+crow*25, 74, 24, False);
        if (demo_mode == 1 && dispdepth == cdep) {  /* display those values changed as gray */
            temp = ((struct demo_element *) malloc(sizeof(struct demo_element))); 
            temp->next = demo_list;
            temp->depth = cdep;
            temp->column = ccol;
            temp->row = crow;
            demo_list = temp;
            XSetFillStyle(dpy, gc, FillTiled);
            XFillRectangle(dpy, win, gc, 101+ccol*75, 101+crow*25, 74, 24);
            XSetFillStyle(dpy, gc, FillSolid);
            XSetForeground(dpy,gc,white);
            XSetBackground(dpy,gc,black);
            XDrawImageString(dpy, win, gc, 110 + 75*ccol, 117 + 25*crow, 
                             display[cdep][ccol][crow], min(size,7));
            XSetForeground(dpy,gc,black);
            XSetBackground(dpy,gc,white);
        }
        else {
            XDrawImageString(dpy, win, gc, 110 + 75*ccol, 117 + 25*crow,
                             display[dispdepth][ccol][crow], min(size,7));
        }
       	if (dependency[search->depth][search->column][search->row] != NULL)
	    recompute(search->depth,search->column,search->row);
	search = search->next;
    }
}  /* end function recompute */


/* display demo list draws the elements in demo list in inverse text 
   doesn`t affect the demo list */
void
display_demo_list()
{
    int size;
    struct demo_element *temp;

    temp = ((struct demo_element *) malloc(sizeof(struct demo_element))); 
    temp = demo_list;

    if (temp != NULL) {
        XSetTile(dpy, gc, tile1); 
        XSetFillStyle(dpy, gc, FillTiled);
        XFillRectangle(dpy, win, gc, 101+temp->column*75, 101+temp->row*25, 74, 24);
        XSetTile(dpy, gc, tile); 
        XSetFillStyle(dpy, gc, FillSolid);
        XSetForeground(dpy,gc,white);
        XSetBackground(dpy,gc,black);
        size = strlen(display[temp->depth][temp->column][temp->row]);
        XDrawImageString(dpy, win, gc, 110 + 75*temp->column, 117 + 25*temp->row, 
                         display[temp->depth][temp->column][temp->row], min(size,7));
        XSetForeground(dpy,gc,black);
        XSetBackground(dpy,gc,white);
        
        temp = temp->next;
        
        XSetFillStyle(dpy, gc, FillTiled);  /* set to gray */
        while (temp != NULL) {
            XClearArea(dpy, win, 101+temp->column*75, 101+temp->row*25, 74, 24, False);
            XFillRectangle(dpy, win, gc, 101+temp->column*75, 101+temp->row*25, 74, 24);
            size = strlen(display[temp->depth][temp->column][temp->row]);
            XSetForeground(dpy,gc,white);
            XSetBackground(dpy,gc,black);
            XDrawImageString(dpy, win, gc, 110 + 75*temp->column, 117 + 25*temp->row, 
                         display[temp->depth][temp->column][temp->row], min(size,7));
            XSetForeground(dpy,gc,black);
            XSetBackground(dpy,gc,white);
            temp = temp->next;
        }  /* end while */
        XSetFillStyle(dpy, gc, FillSolid);
    }
}  /* end function display demo list */


/* clear demo list redisplays the elements in the demo list
   without their special highlighting and leaves demo_list = NULL*/
void
clear_demo_list()
{
    int size;

    while (demo_list != NULL) {
        XClearArea(dpy, win, 101+demo_list->column*75, 101+demo_list->row*25, 74, 24, False);
        size = strlen(display[demo_list->depth][demo_list->column][demo_list->row]);
        XDrawImageString(dpy, win, gc, 110 + 75*demo_list->column, 117 + 25*demo_list->row, 
                         display[demo_list->depth][demo_list->column][demo_list->row], min(size,7));
        demo_list = demo_list->next;
    }  /* end while */
}  /* end function clear demo list */


void
run_demof()
{   
    time_t    *testtime;
    long      temp_time;
    int       demo_depth, col, row;
    long      tempint;
    char      tempstr[16];
    int       dig;
    int       found;

    found = 0;
    
    demo_depth = get_spdsht_depth(dspdsht_list[0]);
    time(&testtime);
    temp_time = (long)testtime;
    while (run_demo) {
        found = 0;
        isis_sleep(delay_time);  /* allow for process to be in background until
                                    the end of the delay time */
        demo_depth = get_spdsht_depth(dspdsht_list[0]);
        col = RANDOM() % 9;
        row = RANDOM() % 19;
        while (found == 0 && (time(&testtime) < (1 + delay_time + temp_time))) {
            col = RANDOM() % 9;
            row = RANDOM() % 19;

            if (entry[demo_depth][col][row][0] == '=' ||
                entry[demo_depth][col][row][0] == '-') {
                dig = 1;
                if (entry[demo_depth][col][row][1] == '-') { /* i.e. a negative number */
                    dig = 2;
                    tempstr[0] = '-';
                }
                while (isdigit(entry[demo_depth][col][row][dig])) {
                    tempstr[dig - 1] = entry[demo_depth][col][row][dig];
                    dig = dig + 1;
                }
                if (entry[demo_depth][col][row][dig] == '\0') {
                /* we have a number only so increment it by a random amount */  
                    tempstr[dig - 1] = '\0';
                    tempint = atoi(tempstr);
                    /* generate a random number  -10 < x < 10 */
                    dig = RANDOM() % 20; 
                    dig = dig - 10;
                    tempint = tempint + dig;
                    sprintf(entry[demo_depth][col][row], "%s%d", "=", tempint);
                    found = 1;
                }
            }  /* end if entry != '=' */
        }
        /* check to see if we have any events that need attention */
        isis_accept_events(0);
        if (XPending(dpy) > 0)
            display_input();
        if (found == 1)
            inform(demo_depth, col, row, entry[demo_depth][col][row]); 
        temp_time = (long)testtime;
    }
}  /* end function demof */



/* look for a dependency entry depeendecy[dep1,col1,row1], 
   if one exists return a 1, else return a 0  */
int
find(dep1,col1,row1,dep2,col2,row2)
  int     dep1,col1,row1,dep2,col2,row2;
{
    temp = dependency[dep1][col1][row1];
    while (temp != NULL) {
        if ( (temp->column == col2) && (temp->row == row2) && (temp->depth == dep2))
            return(1);
        temp = temp->next;
    }
    return(0);  /* if we get here we didn't find element */
}  /* end function find */
   

/* compare the frwd dependency list to the del_dep list and update appropriately
   forward dependencies is the list that tell what cells you depend on 
   this is a fairly lengthy procedure, but mainly it just looks through the
   lists and determines if a cell needs to be added or deleted from the
   forward dependency lists */
void
check_forward_dependencies(ldep,lcol,lrow)
  int ldep,lcol,lrow; /* the depth, column and row we want to assign dependencies to */
{
    struct element  *search;
    struct element  *ltemp;
    int             test;
 
    search = ((struct element *) malloc(sizeof(struct element)));
    ltemp = ((struct element *) malloc(sizeof(struct element))); 
    search = frwd_dep[ldep][lcol][lrow];

    while (search != NULL) {
        test = dsearch(del_dep,search);
        if (test == 0) {
            ltemp = dependency[search->depth][search->column][search->row];
            while ((ltemp->depth != ldep) || (ltemp->column != lcol) || (ltemp->row != lrow)) {
                ltemp = ltemp->next;
                if (ltemp == NULL)
                    error ("fell off the end of dependency list");
            }
            if (ltemp->prev == NULL)
                ltemp = ltemp->next;
            else {
                ltemp->prev->next = ltemp->next;
                if (ltemp->next != NULL)
                    ltemp->next->prev = ltemp->prev;
            }
        }
        search = search->next;
    }
    /* set up list of forward dependencies */
    frwd_dep[ldep][lcol][lrow] = del_dep;
}  /* end function check forward depenedencies */


/* look for an element on list  if it exists return 1 else return 0 */
int
dsearch(list,elment)
  struct element *list,*elment;
{
    struct element *ltemp;
    ltemp = list;
    while (ltemp != NULL) {
        if ( (ltemp->depth == elment->depth) && (ltemp->column == elment->column)
                                             && (ltemp->row == elment->row))
            return(1);
        ltemp = ltemp->next;
    }
    return(0);    /* we didn't find element in list */
}  /* end function dsearch */


/* return the depth (index) of the spreadsheet asked for in spreadloc 
   return -1 if the spreadsheet wasn't found */
int
get_spdsht_depth(spreadloc)
  char spreadloc[32];
{
    int i;

    for (i = 0; i < 10; i++)
        if ( strcmp(spdsht_list[i], spreadloc) == 0)
            return (i);
    return(-1);
    
}  /* end function get spreadsheet depth */


/* if  spreadsheet spreadloc exists, read in its data and return 
   the depth of the new spreadsheet, if not return -1 */
int
access_new_spd(spreadloc)
  char spreadloc[32];
{
    FILE *fp;
    int retval;

    fp = fopen(spreadloc, "r");
    if (fp == NULL)
        return(-1); /* since there was no spreadsheet, we can't get a value */
    fclose(fp);
    retval = numb_spdshts;
    get_all_spreads(spreadloc);
    return(retval);
} /* end function access new spreadsheet */


/* get all the spreadsheets associated with the new spreadsheet file fp */
void
get_all_spreads(locname)
  
  char locname[32];
{
    int   temp_depth;
    FILE  *fp;
    
    fp = fopen(locname,"a");
    if (fp == NULL)
        protected = protected + 1;
    fclose(fp);

    temp_depth = 0;
    fp = fopen(locname,"r");
    if (fp == NULL) 
        error ("you tried to access a spreadsheet which no longer exists");
    fgets(spdsht_list[numb_spdshts], 32, fp);
    spdsht_list[numb_spdshts][strlen(spdsht_list[numb_spdshts])-1] = '\0';
    strcpy(dspdsht_list[numb_spdshts], spdsht_list[numb_spdshts]);

    while (spdsht_list[numb_spdshts][0] != '#' && 
           spdsht_list[numb_spdshts][1] != '#') {
        if (numb_spdshts >= 10)
            error("You tried to access more than 10 spreadsheets");
        if (strcmp(spdsht_list[numb_spdshts], locname) == 0) { 
            temp_depth = numb_spdshts;}
        else{
            read_new_spd(numb_spdshts,spdsht_list[numb_spdshts]);}
        numb_spdshts = numb_spdshts + 1;
        fgets(spdsht_list[numb_spdshts], 32, fp);
        spdsht_list[numb_spdshts][strlen(spdsht_list[numb_spdshts])-1] = '\0';
        strcpy(dspdsht_list[numb_spdshts], spdsht_list[numb_spdshts]);
    }   
    fclose(fp);
    read_new_spd(temp_depth,spdsht_list[temp_depth]);
    check_for_resize();
}  /* end function get all spreads */


/* check in global determines if the spreadsheet in question is in the global picture 
   that is if it is open and in use.*/
int
check_in_global(spdname)
  char spdname[32];
{
    int i;
    
    for (i = 0; i < total_numb_spdshts; i++) {
        if (strcmp(spdname, group_list[i].spreadname) == 0) {
            return(1);
        }
    }
    return(-1);
} /* end function check in global */


/* we have just accesssed a new spreadsheet which does exist, read it in */
void
read_new_spd(locdepth,spreadloc)
  int locdepth;
  char spreadloc[32];
{
    FILE *fp;
    int i, j, check;
    char str1[32]; /* filler string used to read non-useful data */
    char tempname[32];
    char str[64];

    fp = fopen(spreadloc, "a");    
    if (fp == NULL) {
        protected = protected + 1;
        sprintf(str, "%s %s %s", "file", spreadloc, "is read only");
        display_error(str, 2);
    }
    fclose(fp);

    fp = fopen(spreadloc, "r");    
    if (fp == NULL) 
        error ("you tried to access a spreadsheet which no longer exists");
    /* check if our global picture includes this spreadsheet we are about to add */
    check = check_in_global(spreadloc);
    if (check == -1){
        strcpy (tempname, spreadloc);
        /*strcpy (tempstr, "spread");
        strcpy (tempgroup, strcat(tempstr, spreadloc));*/
        add_to_global(tempname, spreadgroup);
    }
    /* total_numb_spdshts = total_numb_spdshts + 1; */
    str1[0] = '\0';
    while (str1[0] != '#' && str1[1] != '#') { 
        /* get to actual data i.e., skip to line after #### */
        fgets(str1, 32, fp);
    }
    for (i = 0; i < 10; i++) {
        for (j = 0; j < 20; j++) {
            fgets(entry[locdepth][i][j], 32, fp);
            entry[locdepth][i][j][strlen(entry[locdepth][i][j])-1] = '\0';
            fgets(display[locdepth][i][j], 32, fp);
            display[locdepth][i][j][strlen(display[locdepth][i][j])-1] = '\0';
            fscanf(fp, "%d %d", &entryval[locdepth][i][j], &validentry[locdepth][i][j]);
            fgets(str1, 32, fp);
        }
    }
    fclose(fp);
}  /* end function read new spread */


/* initialize all the necessary variables */
void
init_var()
{
    int   i,j,scr_depth;
    unsigned long fground,bground;

    static char gray_bits[] = {
        0x5555, 0xaaaa, 0x5555, 0xaaaa, 
        0x5555, 0xaaaa, 0x5555, 0xaaaa, 
        0x5555, 0xaaaa, 0x5555, 0xaaaa,
        0x5555, 0xaaaa, 0x5555, 0xaaaa};

    static char gray1_bits[] = {
        0xaa, 0xaa, 0xff, 0xff, 0xaa, 0xaa, 0xff, 0xff, 0xaa, 0xaa, 0xff, 0xff,
        0xaa, 0xaa, 0xff, 0xff, 0xaa, 0xaa, 0xff, 0xff, 0xaa, 0xaa, 0xff, 0xff,
        0xaa, 0xaa, 0xff, 0xff, 0xaa, 0xaa, 0xff, 0xff};

    for (i = 0; i < 10; i++) {
        for (j = 0; j < 20; j++) {
            spdsht_list[i][0] = '\0';
            display[0][i][j][0] = '\0';
            entry[0][i][j][0] = '\0';
            entryval[0][i][j] = 0;
            validentry[0][i][j] = 0;
            dependency[0][i][j] = NULL;
            frwd_dep[0][i][j] = NULL;
        }
    }
    protected = 0;  /* start off with 0 protected spreadsheets */
    merging = 0;
    dispdepth = 0;
    oldd = 0;
    total_numb_spdshts = 0;
    numb_spdshts = 0;
    numb_in_list = 0;
    shiftkey = 0;
    escapekey = 0;
    controlkey = 0;
    depend = 0;
    BoxHilight = 0;
    /* here are the graphics globals */
    botwidth = 12;
    rtwidth = 10;
    colwidth = 75;
    rowwidth = 25;
    leftx = 100;
    topy = 100;
    numbcols = 10;
    numbrows = 20;
    botmargin = 25;
    rtmargin = 25;
    /* set up the stipple pixmap */
    fground = BlackPixel(dpy, screen);
    bground = WhitePixel(dpy, screen);
    scr_depth = DefaultDepth(dpy, screen);

    tile = XCreatePixmapFromBitmapData(dpy, RootWindow(dpy, screen), gray_bits, gray_width, 
                                       gray_height, fground, bground, scr_depth);
    tile1 = XCreatePixmapFromBitmapData(dpy, RootWindow(dpy, screen), gray1_bits, gray1_width,
                                        gray1_height, fground, bground, scr_depth);
    XSetTile(dpy, gc, tile); 
    demo_list = ((struct demo_element *) malloc(sizeof(struct demo_element))); 
    demo_list = NULL;
    init_menus();
}  /* end function initialize variables */


void
init_menus()
{
    XFontStruct   *font_info;
    int           direction, ascent, descent;
    int           i, j, large ;
    XCharStruct   overall;
    char          *fontname;
 
  /* initialize menus */
    numb_menus = 3;
    menu_bot = 14;
    menu_margin = 8;
    menu_space = 20;

    fontname = NORMALFONT; 

    font_info = XLoadQueryFont(dpy, fontname);

    /* set up start menu */
    strcpy (menu[0].label, "start");
    strcpy (menu[0].sub_menu[0], "join isis");
    menu[0].numb_sub_menus = 1;
    menu[0].left = 300;
    XTextExtents(font_info, menu[0].label, strlen(menu[0].label), 
                     &direction, &ascent, &descent, &overall);
    menu[1].left = menu[0].left + overall.width + 16;
    /*strcpy (menu[0].sub_menu[1], "start without isis");*/

    /* set up file menu */
    strcpy (menu[1].label, "file");
    strcpy (menu[1].sub_menu[0], "save");
    strcpy (menu[1].sub_menu[1], "save all");
    strcpy (menu[1].sub_menu[2], "quit");
    menu[1].numb_sub_menus = 3;

    /* set up options menu */
    strcpy (menu[2].label, "demo");
    strcpy (menu[2].sub_menu[0], "turn off high-lighting");
    if (run_demo == 0)
        strcpy (menu[2].sub_menu[1], "simulate input");
    else
        strcpy (menu[2].sub_menu[1], "stop simulation");
    menu[2].numb_sub_menus = 2;

    XTextExtents(font_info, menu[1].label, strlen(menu[1].label), 
                     &direction, &ascent, &descent, &overall);
    menu[2].left = menu[1].left + overall.width + 16;
    XTextExtents(font_info, menu[2].label, strlen(menu[2].label), 
                     &direction, &ascent, &descent, &overall);
    menu[3].left = menu[2].left + overall.width + 16;
    menu[4].left = menu[3].left;
    menu[5].left = menu[3].left;

 
    for (i = 0; i < numb_menus; i++) {
        large = 0;
        for (j = 0; j < menu[i].numb_sub_menus; j++) {
            if (strlen(menu[i].sub_menu[j]) > strlen(menu[i].sub_menu[large]))
                large = j;
        }
        XTextExtents(font_info, menu[i].sub_menu[large], strlen(menu[i].sub_menu[large]), 
                     &direction, &ascent, &descent, &overall);
        menu[i].width = overall.width + 4;
    }    
    /* hack to insure we have enough room to write 'high-light changes' */
    strcpy (menu[2].sub_menu[0], "high-light changes");
}  /* end function initialize menus */


/* as a process leaves save all the files that are associated with its spreadsheet
   it would probably be possible to save only the file associated with this specific
   spreadsheet, but by saving all the files at a more frequent interval, we increase
   our chances of survival in a crash and also decrease the number of updates with
   have been entered into the spreadsheet and lost because of a crash.  If number is
   0 we save all files that are currently open.  If the number is 1 then we save only
   the spreadsheet that is at display level 0, i.e., the one that is currently being
   edited */
void
save_file(number)
  int number;
{
    FILE   *fp;
    int    i,j,k;
    char   str[64];

      if (spdshtname[0] != '\0') {
          k = 0;
          while (spdsht_list[k][0] != '#' && spdsht_list[k][1] != '#') {
              if (strcmp(dspdsht_list[0],spdsht_list[k]) == 0 || number == 0) {
                  fp = fopen(spdsht_list[k],"w");
                  if (fp != NULL) {
                      for (i = 0; i < numb_spdshts; i++) {
                          fputs(spdsht_list[i], fp);
                          fputc('\n', fp);
                      }
                      fputs("####", fp);
                      fputc('\n', fp);
                      for (i = 0; i < 10; i++) {
                          for (j = 0; j < 20; j++) {
                              fputs(entry[k][i][j], fp);
                              fputc('\n', fp);
                              fputs(display[k][i][j], fp);
                              fputc('\n', fp);
                              fprintf(fp, "%d %d\n", entryval[k][i][j], validentry[k][i][j]);
                          }
                      }
                      fclose(fp);
                  }
                  else {
                      sprintf(str, "%s %s", "unable to write to file", spdsht_list[k]);
                      display_error(str, 2);
                      /*printf("sorry couldn't write to file %s\n",spdsht_list[k]);*/
                  }
              }  /* end if strcmp == 0 */
              k = k + 1; 
          } /* end while */
      }  /* end spread sheet name [0] != '\0`  */
}  /* end function save file */


/* resize window because we have possibly added new spreadsheets */
void
check_for_resize()
{
    XResizeWindow(dpy, win, leftx + numbcols*colwidth + numb_spdshts*rtwidth + rtmargin, 
                  topy + numbrows*rowwidth + numb_spdshts*botwidth + botmargin);
    display_all();
}  /* end functino check for resize */


/**********************************************************************
*  This is a menu driver it determines which option of which          *
*  menu was chosen and takes appropriate action.                      *
**********************************************************************/

/* open menu is the function that initiates appropriate action for a given
   menu select.  It caalls get_menu to determine which menu option was chosen
   then either takes immediate action or calls a function to process the
   chosen menu choice */
void
open_menu(x_coord)
  int x_coord;
{
    int menu_choice, sub_menu_choice;  /* menu choice is the menu the user has picked and
                                          sub menu choice is the sub menu he has picked */
    KeySym          keysym;
    XComposeStatus  compose;
    int             charcount,number;

    sub_menu_choice = get_menu(x_coord);
    menu_choice = 0;
    while (x_coord > menu[menu_choice].left - 8) {
        menu_choice = menu_choice + 1;
    }
    menu_choice = menu_choice - 1;
    
    if (go == 0) {
        switch (menu_choice) {  /* determine which menu option chosen from */
          case 0:  /* user has clicked on start menu */
            switch (sub_menu_choice) {
              case 0:  /* join isis */
                joined = go = 1;
                XClearArea(dpy, win, 200, 30, 800, 40, False);
                display_title();
                break;
              default:
                break;
            }
          case 1:  /* user has click on file menu */
            /* do nothing, we have not yet started the spreadsheet */
            break;
          default:
            break;
        }
    } /* end if g==0 */
    else {
        switch (menu_choice) {  /* determine which menu option chosen from */
          case 0:  /* user has clicked on start menu, so do */
            break;  /* nothing, spreadsheet already started */
          case 1:  /* user has clicked on file menu */
            switch (sub_menu_choice) {
              case 0: /* save */
                save_file(1);
                break;
              case 1: /* save_all */
                save_file(0);
                break;
              case 2: /* quit */
/*                save_file(0); */
                exit(0); /* leave isis */
                break;
              default:
                break;
            }
            break;
          case 2:  /* user has clicked on options menu */
            switch (sub_menu_choice) {
              case 0: /* turn demo mode on/off */
                if (demo_mode == 0) {  /* was off turn it on */
                    strcpy (menu[2].sub_menu[0], "turn off high-lighting");
                    demo_mode = 1;
                }
                else {  /* was on turn it off */
                    strcpy (menu[2].sub_menu[0], "high-light changes"); 
                    clear_demo_list();
                    demo_mode = 0;
                }
                display_all();
                break;

              case 1:
                if (run_demo == 0) {  /* was off turn it on */
                    strcpy (menu[2].sub_menu[1], "stop simulation");
                    strcpy (menu[2].sub_menu[0], "turn off high-lighting");
                    demo_mode = 1;

                    display_error("Enter a single digit for the seconds delay you would like.",1);

                    while ( !(XCheckTypedEvent(dpy, KeyPress, &event)))
                        ;
                    XClearArea(dpy, win, 200, 30, 800, 40, False);
                    charcount = XLookupString(&event.xkey, buffer, bufsize, &keysym, &compose);
                                         
                    number = buffer[0];
                    if (number > 47 && number < 58)
                        delay_time = number - 48;
                    else
                        delay_time = 1;

                    run_demo = 1;
                    display_all();
                    t_fork(run_demof,NULL);
                }
                else {  /* was on turn it off */
                    strcpy (menu[2].sub_menu[1], "simulate input");
                    run_demo = 0;
                }
                display_all();
                break;

              default:
                break;
            }
            break;
          default:
            break;
        }
    } /* end else go <> 0 */
}  /* end function open_menu */


int
get_menu(x_coord)
  int x_coord;
{
    XEvent     menu_event;
    int        i;
    int        choosing;
    int        menu_choice;
    int        sub_pane, old_pane;
    Window     root, child;
    int        root_x, root_y;
    int        pos_x, pos_y;
    unsigned   int key_but;

    choosing = 1;
    menu_choice = 0;
    old_pane = -1;
    while (x_coord > menu[menu_choice].left - 8) {
        menu_choice = menu_choice + 1;
    }
    menu_choice = menu_choice - 1;
    
    /* clear are we are about to display the menu in */
    XClearArea(dpy, win, menu[menu_choice].left - menu_margin, menu_bot, 
               menu[menu_choice].width + 2*menu_margin + 2, 
               menu_space * menu[menu_choice].numb_sub_menus + 2, False);
    
    /* highlight the menu that was chosen */
    XFillRectangle(dpy, win, gc, menu[menu_choice].left + 1 - menu_margin, 0,                   
                   (menu[menu_choice+1].left - menu[menu_choice].left) - 1, menu_bot);
    draw_inverse_text(menu[menu_choice].label, menu[menu_choice].left, 10);

    XDrawLine(dpy, win, gc, menu[menu_choice].left - menu_margin, menu_bot , 
              menu[menu_choice].left + menu[menu_choice].width + menu_margin, menu_bot);
    XDrawLine(dpy, win, gc, menu[menu_choice].left - menu_margin, menu_bot + 1 , 
              menu[menu_choice].left + menu[menu_choice].width + menu_margin, menu_bot + 1);

    for (i = 0; i < menu[menu_choice].numb_sub_menus; i++) {
        /* draw horizontal lines */
        XDrawLine(dpy, win, gc, menu[menu_choice].left - menu_margin, 
                  menu_bot + menu_space + menu_space * i, 
                  menu[menu_choice].left + menu[menu_choice].width + menu_margin, 
                  menu_bot + menu_space + menu_space * i);
        XDrawLine(dpy, win, gc, menu[menu_choice].left - menu_margin, 
                  1 + menu_bot + menu_space + menu_space * i, 
                  menu[menu_choice].left + menu[menu_choice].width + menu_margin, 
                  1 + menu_bot + menu_space + menu_space * i);
        XDrawImageString(dpy, win, gc, menu[menu_choice].left, 
                         14 + menu_bot + menu_space * i, menu[menu_choice].sub_menu[i], 
                         strlen(menu[menu_choice].sub_menu[i]));

        XDrawLine(dpy, win, gc, menu[menu_choice].left - menu_margin + 1, 
                  menu_bot + menu_space * i, menu[menu_choice].left - menu_margin + 1, 
                  menu_bot + menu_space + menu_space * i);
        XDrawLine(dpy, win, gc, menu[menu_choice].left - menu_margin, 
                  menu_bot + menu_space * i, menu[menu_choice].left - menu_margin, 
                  menu_bot + menu_space + menu_space * i);

        /* draw vertical lines */
        XDrawLine(dpy, win, gc, 
                  menu[menu_choice].left + menu[menu_choice].width + menu_margin, 
                  menu_bot + menu_space * i,
                  menu[menu_choice].left + menu[menu_choice].width +  menu_margin, 
                  menu_bot +menu_space + menu_space * i);
        XDrawLine(dpy, win, gc, 
                  menu[menu_choice].left + menu[menu_choice].width + menu_margin + 1, 
                  menu_bot + menu_space * i,
                  menu[menu_choice].left + menu[menu_choice].width +  menu_margin + 1, 
                  menu_bot +menu_space + menu_space * i);
    }
    while (choosing) {
        XNextEvent (dpy, &menu_event);

        switch (menu_event.type) {
          case Expose :
              break;

          case ButtonRelease :
              /* highlight the menu that was chosen */
              XClearArea(dpy, win, menu[menu_choice].left + 1 - menu_margin, 0,                   
                         menu[menu_choice+1].left - menu[menu_choice].left - 1, 
                         menu_bot, False);
              XDrawImageString(dpy, win, gc, menu[menu_choice].left, 10, 
                               menu[menu_choice].label, strlen(menu[menu_choice].label));
              choosing = 0;
              XClearArea(dpy, win, menu[menu_choice].left - menu_margin, menu_bot, 
                         menu[menu_choice].width + 2*menu_margin + 2, 
                         menu_space * menu[menu_choice].numb_sub_menus + 2, False);
              break;
          
          case MotionNotify :
              XQueryPointer(dpy, menu_event.xmotion.window, &root, &child, &root_x, &root_y,
                            &pos_x, &pos_y, &key_but);
              if ((pos_x > (menu[menu_choice].left - menu_margin)) && 
                  (pos_x <( menu[menu_choice].left + menu[menu_choice].width + menu_margin)) && 
                  (pos_y > menu_bot) && 
                  (pos_y < (menu_bot + menu_space * menu[menu_choice].numb_sub_menus))) {
                  sub_pane = ((pos_y - menu_bot) / 20);
                  if (old_pane != sub_pane) {
                      /* invert newly selected sub_menu */
                      XFillRectangle(dpy, win, gc, menu[menu_choice].left - menu_margin + 2, 
                                     menu_bot + menu_space * sub_pane + 2, 
                                     menu[menu_choice].width + 2*menu_margin - 2, 
                                     menu_space - 2);
                      draw_inverse_text(menu[menu_choice].sub_menu[sub_pane], menu[menu_choice].left, 
                                        14 + menu_bot + menu_space * sub_pane);
                      if (old_pane != -1) { /* uninvert old sub menu assuming there was one */
                          XClearArea(dpy, win, menu[menu_choice].left - menu_margin + 2, 
                                     menu_bot + menu_space * old_pane + 2,
                                     menu[menu_choice].width + 2*menu_margin - 2,
                                     menu_space - 2, 
                                     False);
                          XDrawImageString(dpy, win, gc, menu[menu_choice].left, 
                                           14 + menu_bot + menu_space * old_pane, 
                                           menu[menu_choice].sub_menu[old_pane], 
                                           strlen(menu[menu_choice].sub_menu[old_pane]));
                      }
                      old_pane = sub_pane;
                  }
              }
              else { /* uninvert old sub menu assuming there was one */
                  if (old_pane != -1) {
                      XClearArea(dpy, win, menu[menu_choice].left - menu_margin + 2, 
                                 menu_bot + menu_space * old_pane + 2,
                                 menu[menu_choice].width + 2*menu_margin - 2,
                                 menu_space - 2, 
                                 False);
                      XDrawImageString(dpy, win, gc, menu[menu_choice].left, 
                                       14 + menu_bot + menu_space * old_pane, 
                                       menu[menu_choice].sub_menu[old_pane], 
                                       strlen(menu[menu_choice].sub_menu[old_pane]));
                      old_pane = -1;
                  }
              }
              break;
            default:
              ;
          }
    }
    display_all();
    XFlush(dpy);
    return(old_pane);
}  /* end function get menu */


void
draw_inverse_text(str, x, y)
  char   *str;
  int    x;
  int    y;
{
    XSetForeground(dpy,gc,white);
    XSetBackground(dpy,gc,black);
    XDrawImageString(dpy, win, gc, x, y, str, strlen(str));
    XSetForeground(dpy,gc,black);
    XSetBackground(dpy,gc,white);
}  /* end function draw inverse text */
