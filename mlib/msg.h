/*  $RCSfile: msg.h,v $ $Revision: 2.22 $ $Date: 90/08/23 21:36:53 $  */
#ifndef     MSG
#define     MSG

/*
 *    Header for new message library
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

/* Constants, can be changed */
#define MSG_MAXIOV	128
#define MSG_BLKLEN      1024	/* Good size for allocations */

#ifndef AIX
#  define MAXSIZE         7500	/* Piggybacking packet size limit */
#  define MSG_MUSTFRAG    7750	/* Fragment anything larger than this */
#  define MAXMSGLEN       8192	/* Largest ever sent/received */
#  define FRAGSIZE        7500	/* Fragment size */
#  define STAGEBUFSIZE   10240	/* Size of staging area */
#else
#  define MAXSIZE         1350	/* Piggybacking packet size limit */
#  define MSG_MUSTFRAG    1350	/* Fragment anything larger than this */
#  define MAXMSGLEN       2048	/* Largest ever sent/received */
#  define FRAGSIZE        1350	/* Fragment size */
#  define STAGEBUFSIZE    2048
#endif				/* AIX */

#ifndef FUN_TYPES		/* FUN_TYPES may have already been set up by isis.h */
#  ifdef __STDC__
#    ifndef APOLLO
#      define FUN_TYPES  1
#    endif
#  endif
#  if ( __cplusplus || c_plusplus )
#    define FUN_TYPES    1
#  endif
#endif

#include    <sys/types.h>
#include    <netinet/in.h>
#ifndef va_start
#include <varargs.h>
#endif

#ifndef     FILE
#include    <stdio.h>
#endif				/* FILE */

#ifndef     UIO
#include    <sys/uio.h>
#define     UIO
#endif

#if             (HPUX)
#include    <memory.h>
#define     bcopy(a,b,c)        memcpy(b,a,c)
#define     bzero(a,b)          memset(a,0,b)
#endif

#include    "cl_typedefs.h"
#include    "sysfields.h"

#ifndef     TRUE
#define     TRUE                1
#endif				/* TRUE */

#ifndef     FALSE
#define     FALSE               0
#endif				/* FALSE */

#define     NOT_LAZY            0
#define     LAZY_ALWAYS         1
#define     LAZY_IFLOCALDEST    2
#define     ULTRA_LAZY          3

#define     FWI_SENDER          0
#define     FWI_NEWDEST         1

#define     PADDED_LEN(len)     (((len) + 7) & ~0x7)

#if ! ( __cplusplus || c_plusplus )
typedef union address address;
#endif

union address {
	struct ADDRESS {
		union {
			short u_process;
			short u_groupid;
		} ad_un;
		u_char u_addr_site;
		u_char u_addr_incarn;
		short u_addr_portno;	/* See below for special values */
		short u_addr_entry;
	} a_a;
	long a_l[2];
};

#define     addr_process        a_a.ad_un.u_process
#define     addr_groupid        a_a.ad_un.u_groupid
#define     addr_site           a_a.u_addr_site
#define     addr_incarn         a_a.u_addr_incarn
#define     addr_portno         a_a.u_addr_portno
#define     addr_entry          a_a.u_addr_entry

/* Special values for 'port' field */
#define     ISACT               -1
#define     ISAGID              -2
#define     ISPLIST             -3
#define     ISALGID             -4

#define     _docmp(a,la,b,lb)   ((la[0]!=lb[0])?(la[0]-lb[0]): (a->addr_portno>=0&&b->addr_portno>=0)? 0: (a->addr_portno-b->addr_portno))
#define     addr_cmp(a,b)       _docmp((a),(a)->a_l,(b),(b)->a_l)
#define     paddr_cmp(a,b)      _dopcmp((a),(a)->a_l,(b),(b)->a_l)
#define     aptr_isnull(ad)     ((ad)->addr_site == 0)
#define     addr_isnull(ad)     ((ad) == 0 || (ad)->addr_site == 0)
#define     addr_isequal(a1,a2) (addr_cmp(a1, a2) == 0)
#define     addr_ismine(addr)   (addr_cmp(addr, &my_address) == 0)
#define     addr_islocal(addr)  (addr->site == my_site_no)
#define     addr_isrclient(addr)((addr)->addr_portno >= 0 && ((addr->addr_process) < -10))
#define     addr_ispid(addr)    ((addr)->addr_portno >= 0)
#define     addr_isgid(addr)    ((addr)->addr_portno == ISAGID)
#define     addr_isplist(addr)  ((addr)->addr_portno == ISPLIST)
#define     addr_islgid(addr)   ((addr)->addr_portno == ISALGID)
#define     addr_isgroup(gaddr) (addr_isgid(gaddr) || addr_islgid(gaddr))
#define     addr_isact(addr)    ((addr)->addr_portno == ISACT)
#define     act_isequal(a1,a2)  (addr_isequal(a1,a2) && ((a1)->addr_entry == (a2)->addr_entry))

#if ! ( __cplusplus || c_plusplus )
typedef struct msg_fld msg_fld;
#endif

struct msg_fld {
	u_char fld_name;		/* Field name */
	u_char fld_type;		/* Data type */
	u_char fld_padding[6];		/* MIPS wants everything double-word aligned... */
	int fld_len;			/* length, doesn't include padding */
	msg_fld *fld_next;		/* Linkage pointer */
	/* Data goes here */
};

#if ! ( __cplusplus || c_plusplus )
typedef struct block_desc block_desc;
#endif

struct block_desc {
	int blk_avail;			/* Amount in use */
	int blk_len;			/* Total length */
	int blk_nlinks;			/* Normally 0 */
	block_desc *blk_next;		/* Next block */
	char *blk_ptr;			/* Next free byte */
	char blk_data[4];		/* Data starts here */
};

#define     MSG_BLKHDR         (sizeof(block_desc)-4)
#define     MSG_BLKSIZE        (MSG_BLKLEN-MSG_BLKHDR)

#if ! ( __cplusplus || c_plusplus )
typedef struct msg_hdr msg_hdr;
#endif

struct msg_hdr {			/* For transmission */
	int hdr_len;			/* In `network' format */
	int hdr_nlinks;			/* Only when in core... */
	char hdr_version[4];		/* Version info */
	u_char hdr_shortcode;		/* For FTYPE=short */
	u_char hdr_longcode;		/* For FTYPE=long,float */
	u_char hdr_dblcode;		/* For FTYPE=double */
	u_char hdr_flags;		/* Flags */
	address hdr_sender;		/* Most recent sender */
	address hdr_replyto;		/* reply to */
	address hdr_dest;		/* If has one group dest... for lists, uses SYSFLD_ALIST */
	int hdr_msgid;			/* message-id, odd if no reply expected */
	int hdr_pad;			/* Pad to double-word boundary */
};

#define     MSG_PBDIRTY         0x01	/* Pbuffer dirty */
#define     MSG_XFIELDS         0x02	/* Has special fields */

#define     DBL_HILO            0
#define     DBL_LOHI            1

#if ! ( __cplusplus || c_plusplus )
typedef struct message message;
#endif

struct message {			/* When in use */
	message *msg_next;		/* Used to keep list of active msgs */
	int msg_len;			/* In `host' format */
	int msg_refs;			/* Counts refs to this message */
	msg_hdr *msg_hdr;		/* First field is hdr */
	char *msg_base;			/* For reconstruct only */
	msg_fld *msg_fields;		/* To first field */
	msg_fld *msg_last;		/* To last field */
	block_desc *msg_extents;	/* Blocks of data for this message */
	int msg_nextents;		/* Number of blocks of data */
	struct iovec *msg_iov;		/* IO vector, if initialized */
	int msg_iovlen;			/* Len of IO vector, if initialized */
	int msg_lname;			/* Last name in getfield/getfp */
	int msg_linst;			/* Last inst in getfield/getfp */
	msg_fld *msg_lfield;		/* Last field found */
	int msg_fpointer;		/* Used in msg_get */
	address *msg_dests;		/* Dests field, if known */
	address *msg_act;		/* SYSFLD_ACT, if any */
	address *msg_forwarder;		/* SYSFLD_FORWARD, if any */
	char *msg_byi;			/* Bypass info, if any */
	int msg_transport;		/* Message transported by this protocol */
};

#if ! ( __cplusplus || c_plusplus )
typedef struct field_ref field_ref;
#endif

struct field_ref {
	int fr_len;			/* Length */
	u_char fr_type;			/* True type */
	u_char fr_xref;			/* xbyref bit */
	u_char fr_pad[2];		/* Back to a 4-byte boundary */
	char *fr_ptr;			/* To data */
	vfunc *fr_routine;		/* Free routine */
};

struct msg_ref {
	message *mr_msg;		/* To message */
};

#define	is_field_ref(fp)        ((field_ref*)&fp[1])
#define	is_msg_ref(fp)          ((struct msg_ref*)&fp[1])

/*** Interface Macros ***/

#define msg_convertchar(c)
#define msg_getid(mp)           (mp)->msg_hdr->hdr_msgid
#define msg_setid(mp,id)        if(1) (mp)->msg_hdr->hdr_msgid = id; else
#define msg_isforwarded(mp)     (!addr_isnull((mp)->msg_forwarder))
#define msg_getlen(mp)          (mp)->msg_len
#define msg_addfield(mp,field,data,type,len)   msg_insertfield(mp,field,data,type,len)
#define msg_addmsg(mp,field,msg)               msg_insertmsg(mp,field,msg)
#define msg_increfcount(mp)                    if(1) ++(mp)->msg_refs; else
#define msg_setdest(mp,dest)    if(1) {                                             \
				      address dests[2];                             \
				      dests[0] = *dest;                             \
				      dests[1] = NULLADDRESS;                       \
				      msg_setdests(mp,dests);                       \
				} else ;
#define msg_setreplyto(mp,dest) if(1) (mp)->msg_hdr->hdr_replyto = *dest; else
#define msg_setsender(mp,send)  if(1) (mp)->msg_hdr->hdr_sender = *send; else
#define msg_getreplyto(mp)      (&(mp)->msg_hdr->hdr_replyto)
#define msg_getpbflag(mp)       ((mp)->msg_hdr->hdr_flags&MSG_PBDIRTY)
#define msg_setpbflag(mp)       if(1) (mp)->msg_hdr->hdr_flags |= MSG_PBDIRTY; else
#define msg_getsender(mp)       (&(mp)->msg_hdr->hdr_sender)
#define msg_gettruesender(mp)   (addr_isnull((mp)->msg_forwarder) ?        \
                                 &(mp)->msg_hdr->hdr_sender :             \
                                 &(mp)->msg_forwarder)

#define msg_getforwarder(mp)    ((mp)->msg_forwarder)
#define msg_setforwarder(mp,f)  if(1) (mp)->msg_forwarder = (address*)msg_replacefield(mp, SYSFLD_FORWARD, (char*)f, FTYPE_ADDRESS, sizeof(address)*2); else
#define isis_define_type(t,s,r) isis_do_define_type(t,s,r)
#define msg_body(bp)            bp->blk_ptr

/* Manipulate fields... */
#define msg_getdests(mp)        ((mp)->msg_dests)
#define msg_getact(mp)          ((mp)->msg_act)
#define msg_getbyi(mp)          ((by_info*)(mp)->msg_byi)
#define msg_setact(mp,act)      if(1) (mp)->msg_act = (address*)msg_replacefield(mp, SYSFLD_ACT, (char*)act, FTYPE_ADDRESS, sizeof(address)); else
#define msg_setbyi(mp,byi)      if(1) (mp)->msg_byi = msg_insertfield(mp, SYSFLD_BYPASS, (char*)byi, FTYPE_BYPASS, sizeof(by_info)); else
#define msg_getalist(mp)        ((address*)msg_getfield(mp, SYSFLD_ALIST, 1, (int*)0))
#define msg_makelazy(mp,hl)     msg_replacefield(mp, SYSFLD_LAZY, (char*)&lazy_levels[hl], FTYPE_LONG, sizeof(int))
#define msg_delete(mp)          if(--(mp)->msg_refs == 0) msg_deallocate(mp); else

extern int lazy_levels[];

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	int isis_do_define_type(int formatletter, int size, vfunc * converter);
	void msg_convertaddress(address * addrp);
	void msg_convertbitv(struct bitvec *bp);
	void msg_convertevent(struct event_id *eid);
	void msg_convertfield(msg_fld * f_desc, int flag);
	void msg_convertgroupview(struct groupview *gv);
	void msg_convertlong(long *longp);
	void msg_convertpgroup(struct sys_groupview *pg);
	void msg_convertshort(short *shortp);
	message *msg_copy(message * msg);
	void MSG_DELETE(message * msg);	/* for use in callbacks */
	void msg_deallocate(message * msg);
	void msg_deleteall(message * msg, int name);
	void msg_deletefield(message * msg, int name, int inst);
	void msg_dumpmsgs();
	message *msg_fread(FILE *file);
	int msg_fwrite(FILE *file, message * mp);
	message *msg_genmsg(int name, char *data, int type, int len, int null_term);
#if ( __cplusplus || c_plusplus )
	message *msg_gen(char *format, ...);
	int msg_get(message * msg, char *format ...);
	int msg_getfld(message * msg, int name, int *pos_p, char *format ...);
#else
	message *msg_gen( /* char *format, ... */ );
	int msg_get( /* message *msg, char *format, ... */ );
	int msg_getfld( /* message *msg, int name, int* pos_p, char *format, ... */ );
#endif
	caddr_t msg_getfield(message * msg, int name, int inst, int *lenp);
	int msg_getfields(message * msg, int name, caddr_t *datap, int *lenp, int n);
	message *msg_getmsg(message * mp, int name, int inst);
	int msg_getmsgs(message * mp, int name, message ** msgs, int n);
	int msg_getscantype(message * msg);
	message *msg_getmsg(message * msg, int name, int inst);
	int msg_gettype(message * msg, int name, int inst);
	message *msg_newmsg();
#if ( __cplusplus || c_plusplus )
	int msg_put(message * msg, char *format, ...);
	int msg_putfld(message * msg, int name, char *format ...);
#else
	int msg_put( /* message *msg, char *format, ... */ );
	int msg_putfld( /* message *msg, int name, char *format, ... */ );
#endif
	message *msg_read(int sock);
	int msg_rewind(message * mp);
	address *msg_setdests(message * mp, address * dests);
	int msg_write(int sock, message * mp);

#ifdef __cplusplus
}
#endif
#else				/* FUN_TYPES */

int isis_do_define_type();
void msg_convertshort();
void msg_convertshort();
void msg_convertlong();
void msg_convertsiteid();
void msg_convertpgroup();
message *msg_copy();
void msg_deletefield();
void MSG_DELETE();
void msg_deallocate();
void msg_dumpmsgs();
caddr_t msg_getfield();
int msg_getfields();
message *msg_getmsg();
int msg_getmsgs();
message *msg_fread();
int msg_fwrite();
message *msg_gen(), *msg_genmsg();
message *msg_getmsg();
int msg_gettype();
message *msg_newmsg();
message *msg_read();
address *msg_setdests();
int msg_write();

#endif				/* FUN_TYPES */

/*** Internal routines ***/
#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	block_desc *msg_malloc(int len);
	struct iovec *msg_getiovec(message * msg);
	int msg_getiovlen(message * msg);
	void msg_callertrace(char *routine, message * msg, char *caller);
	void msg_msgcheck(message * msg);
	void msg_convertgldesc(struct gl_desc *gl);
	void msg_converthdr(msg_hdr * hdr);
	void msg_convertinterclient(struct interclient *ic);
	void msg_convertintersite(struct intersite *is);
	void msg_convertverify(struct verify *vi);
	int msg_definetype(int type, int size, void (*converter) (), char *string);
	int msg_dogetf(message ** mlist, int nmsgs, int fname, int *fpt, va_list * argp);
	int msg_doputf(message * mp, int fname, va_list * ap);
	char *msg_insertfield(message * msg, int name, char *data, int type, int len);
	void msg_insertmsg(message * msg1, int name, message * msg2);
	void msg_insertref(message * mp, int name, char *data, int type, int len, vfunc * routine);
	void msg_makekey(int shortcode, int longcode, int dblcode);
	void msg_makereal(message * mp);
	void msg_printaccess(message * msg);
	void msg_printheader(message * msg);
	message *msg_reconstruct(block_desc * bp);
	char *msg_replacefield(message * msg, int name, char *data, int type, int len);
	char *msg_typename(int type);
#ifdef __cplusplus
}
#endif
#else				/* FUN_TYPES */

block_desc *msg_malloc();
struct iovec *msg_getiovec();
int msg_getiovlen();
void msg_callertrace();
void msg_msgcheck();
void msg_convertgldesc();
void msg_converthdr();
void msg_convertinterclient();
void msg_convertintersite();
void msg_convertverify();
int msg_definetype();
int msg_dogetf();
int msg_doputf();
char *msg_insertfield();
void msg_insertmsg();
void msg_insertref();
void msg_makekey();
void msg_makereal();
void msg_printaccess();
void msg_printheader();
message *msg_reconstruct();
char *msg_replacefield();
char *msg_typename();

#endif

/* Field types */
#define     FTYPE_ADDRESS       'a'
#define     FTYPE_BITVEC        'b'
#define     FTYPE_CHAR          'c'
#define     FTYPE_SHORT         'h'
#define     FTYPE_LONG          'd'
#define     FTYPE_EVENT         'e'
#define     FTYPE_FLOAT         'f'
#define     FTYPE_DOUBLE        'g'
#define     FTYPE_PGROUP        'p'
#define     FTYPE_MESSAGE       'm'
#define     FTYPE_SITEID        0x80
#define     FTYPE_INTERSITE     0x81
#define     FTYPE_GROUPVIEW     0x82
#define     FTYPE_GLDESC        0x83
#define     FTYPE_VERIFY        0x84
#define     FTYPE_INTERCLIENT   0x85
#define     FTYPE_FIELDREF      0x86
#define     FTYPE_BYPASS        0x87
#define     FTYPE_SVIEW         0x88

#define     FTYPE_DELETED       0xFF

/* Special values that can be passed for <nwanted> and <alen> */
#define     ALL                 10000
#define     MAJORITY            10001
#define     QUORUM              10001	/* Alias for MAJORITY */
#define     AMESSAGE            -1
#define     ASTRING             -2
#define     AMALLOC             -3	/* Supported only in protos */

/* Memory management info */
extern int msg_usagestats, msg_tracemsgs, msg_tracecaller;
extern long msg_namsgs, msg_nfmsgs;
extern long msg_nalloc, msg_nfree, msg_memused, msg_memfree;

#if ! ( __cplusplus || c_plusplus )
char *callername();
#endif

/* Byte swapping info */
extern u_short msg_shortmask;
extern u_long msg_longmask;

#define         MSG_SHORTCODE   ((((u_char *) (&msg_shortmask))[0] << 2 | \
                                 ((u_char *) (&msg_shortmask))[1]) << 4)
#define         MSG_LONGCODE    (((((u_char *) (&msg_longmask))[0] << 2 | \
                                 ((u_char *) (&msg_longmask))[1]) << 2 | \
                                 ((u_char *) (&msg_longmask))[2]) << 2 | \
                                 ((u_char *) (&msg_longmask))[3])

/* Site info */
extern address my_address;
extern int my_site_no;
extern int my_site_incarn;
extern int my_process_id;

#ifndef	print
#define	print	isis_print
#endif

#endif				/* MSG */
