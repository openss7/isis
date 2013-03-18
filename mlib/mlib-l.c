/*  $RCSfile: mlib-l.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:12:31 $  */
/*
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
 */
#include    "msg.h"
char *
msg_addfield (msg, field, data, type, len)
  register message  *msg;
  register u_char   field;
  register char     *data;
  register u_char   type;
  register int      len;
{}
char *
msg_insertfield (msg, field, data, type, len)
  register message  *msg;
  u_char            field;
  char              *data;
  u_char            type;
  int               len;
{}
msg_unpkfields (msg, field, first, n, dp, lenp)
  register message  *msg;
  register u_char   field;
  register          first;
  char              **dp;
  int               *lenp;
{}
msg_deletefield (msg, field, inst)
  message           *msg;
  register u_char   field;
  register int      inst;
{}
char *
msg_replacefield (msg, field, data, type, len)
  register message  *msg;
  u_char            field;
  char              *data;
  short             type;
  int               len;
{}
static writev(sock, iovp, iovl)
  register sock;
  register iovec *iovp;
  {}
extern  errno;
msg_fwrite(file, mp)
  FILE *file;
  register message *mp;
  {}
message *
msg_fread(file)
  FILE *file;
  {}
msg_write(sock, mp)
  register message *mp;
  {}
message *
msg_read(sock)
  {}
message *
msg_gen(va_alist)
  va_dcl
  {}
msg_put(va_alist)
  va_dcl
  {}
msg_putfld(va_alist)
  va_dcl
  {}
msg_doputf(mp, fname, ap)
  message *mp;
  va_list *ap;
  {}
msg_get(va_alist)
  va_dcl
  {}
msg_getfld(va_alist)
  va_dcl
  {}
msg_dogetf(mlist, nmsgs, fname, fpt, argp)
  message **mlist;
  va_list *argp;
  int *fpt;
  {}
msg_rewind(mp)
  register message *mp;
  {}
isis_define_type(typeno, formatletter, size, converter)
  char formatletter;
  ifunc *converter;
  {}
block_desc *
msg_accessalloc ()
  {}
block_desc *
msg_blockalloc (req_len)
  register int  req_len;
  {}
msg_blockfree (blk_desc)
  register block_desc   *blk_desc;
  {}
msg_blockcheck (blk_desc)
  register block_desc   *blk_desc;
  {}
msg_blockdouble (descp)
  register block_desc   **descp;
  {}
msg_printblkdesc (blk_desc)
  register block_desc   *blk_desc;
  {}
msg_recordalloc (blk_desc)
  register block_desc   *blk_desc;
  {}
msg_recordfree (blk_desc)
  register block_desc   *blk_desc;
  {}
/* msg_addmsg  : Insert a msg2 into msg1 as a field           */
msg_addmsg (msg1, field, msg2)
  register message  *msg1;
  message           *msg2;
  u_char            field;
 {}
msg_insertmsg (msg1, field, msg2)
  register message  *msg1;
  message           *msg2;
  u_char            field;
 {}
message *
msg_getmsg (msg, field, inst)
  register message  *msg;
  register u_char   field;
  register int      inst;
{}
msg_unpkmsgs (msg, field, first, n, msgs)
  register message  *msg;
  u_char            field;
  int               first, n;
  message           *msgs[];
{}
message *
msg_newmsg ()
{}
message *
msg_genmsg (va_alist)
   va_dcl
{}
message *
msg_reconstruct (data, blk_desc)
  char          *data;
  block_desc    *blk_desc;
 {}
msg_delete (msg)
  register message  *msg;
{}
msg_split (msg)
  register message  *msg;
{}
message *
msg_copy (msg)
  message   *msg;
{}
msg_convertfield (start, f_desc)
  register char         *start;
  register field_desc   *f_desc;
{}
msg_makekey (shortcode, longcode)
  u_char    shortcode, longcode;
{}
msg_convertshort (shortp)
  register short    *shortp;
{}
msg_convertlong (longp)
  register long     *longp;
{}
msg_convertaddress (addrp)
  register address  *addrp;
{}
msg_convertbitv (bp)
  register bitvec  *bp;
{}
msg_convertpgroup (pg)
  register sys_groupview    *pg;
{}
msg_convertverify (vi)
  register verify    *vi;
{}
msg_convertgroupview (gv)
  register struct groupview    *gv;
{}
msg_convertgldesc (gl)
  register gl_desc *gl;
{}
msg_convertevent (eid)
  register event_id *eid;
{}
typedef struct intersite intersite;
#include    "pr_intersite.h"
msg_convertintersite (is)
  register intersite    *is;
{}
msg_convertinterclient (ic)
  register interclient  *ic;
{}
msg_convertheader (hdr)
  register header   *hdr;
{}
msg_definetype (type, size, converter, string)
  u_char    type;
  int       size;
  int       (*converter)();
  char      *string;
{}
char *
msg_typename (type)
  register u_char   type;
{}
address*
msg_setdest (msg, dest)
  register message  *msg;
  address*           dest;
{}
address*
msg_setdests (msg, alist)
  register message  *msg;
  register address  *alist;
{}
address *
msg_getdests (msg)
  register message  *msg;
{}
address*
msg_getreplyto (msg)
  register message  *msg;
{}
address*
msg_getsender (msg)
  register message *msg;
{}
address*
msg_gettruesender (msg)
  register message *msg;
{}
/* msg_increfcount : Increment the reference count of a given message */
msg_increfcount (msg)
  register message  *msg;
{}
msg_ismsg (msg)
  message   *msg;
{}
msg_makelazy (msg, howlazy)
  register message  *msg;
  int               howlazy;
{}
msg_gettype (msg, field, inst)
  register message  *msg;
  u_char            field;
  int               inst;
{}
msg_printaccess (msg)
 register message   *msg;
{}
msg_printheader (msg)
  register message  *msg;
{}
msg_printfield (msg, field, inst)
  register message  *msg;
  u_char            field;
  int               inst;
{}
msg_recordmsgalloc (msg)
  register message  *msg;
{}
msg_recordmsgfree (msg)
  register message  *msg;
{}
msg_dumpmsgs (level)
  int   level;
{}
msg_dumpblocks()
{}
msg_msgcheck (msg)
  message   *msg;
{}
msg_callertrace (routine, msg, caller)
  char      *routine;
  message   *msg;
  char      *caller;
{}
