/*  $RCSfile: msg.c,v $ $Revision: 2.21 $ $Date: 90/08/13 20:41:19 $  */
/*
 * msg.c: message utilities, new version 
 * 	Originally coded by Tommy Joseph
 *	Rewritten for speed by Ken Birman, 1989
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
 *
 *
 * Basic data structure is pretty simple, only tricky aspect
 * is that there are 3 separate sorts of ``reference counters'':
 *	msg->msg_refs mscounts msg_increfcount/msg_delete
 *	msg->hdr_nlinks counts number of distinct message* structs sharing
 *         this list of fields
 *      bp->blk_nlinks is the number of messages sharing this block - 1
 * Two messages share a block due to calls to msg_reconstruct with
 * message containing a nested message.  In this case the nested message has
 * an offset into the block; start address is given by msg->msg_base.  
 *
 * If such a message is modified a new block will normally be needed.  In this
 * case the "pseudo" block will be the last one on the list of extents, hence 
 * is the block with bp->blk_next == (blk_desc*)0.
 */

#define  ISIS_SYS
#include "cl_typedefs.h"
#include "msg.h"
#include "isis_alloc.h"

#define  begin

#ifndef  ISISCALL1
#    define ISISCALL1(proc, arg)        (*(proc))(arg)
#endif   ISISCALL1


int	lazy_levels[] ={ 0, 1, 2, 3};

static adesc   msg_ad ={ sizeof(message), 0, 32 };
static adesc   iov_ad ={ sizeof(struct iovec)*MSG_MAXIOV, 0, 32 };

static adesc   blk_ad  ={ MSG_BLKLEN, 0, 32 };
static adesc   mblk_ad  ={ MSG_BLKHDR+MAXMSGLEN, 0, 4 };

#define	BLK_KEEP	32		/* Size of local freelist */
#define	MBLK_KEEP	8		/* Size of local freelist */
#define	MSG_KEEP	16		/* Size of local freelist */
#define IOV_KEEP        16              /* Size of IOV freelist */

#define MSG_SIZELIMIT	4194304	        /* 4 Mbytes: (arbitrary) upper limit on message size */

static blk_count, mblk_count, msg_count, iov_count;
static block_desc *blk_freelist, *mblk_freelist;
static message    *msg_freelist;
static char       **iov_freelist;
static union      { char STRING[4]; long LONG; } isis_version;

static  double dbl = 1.0;

#define MSG_DBLCODE     (((int*)&dbl)[0]==0)

int               panic();
#ifdef FUN_TYPES
static void       pfld(msg_fld *fp);
#else
static void       pfld();
#endif

#define msg_init(mp)                                       \
  {                                                        \
	mp->msg_refs = 1;                                  \
	mp->msg_len = 0;                                   \
	mp->msg_base = 0;                                  \
	mp->msg_fields = 0;                                \
	mp->msg_last = 0;                                  \
	mp->msg_extents = 0;                               \
	mp->msg_nextents = 0;                              \
	mp->msg_iov = 0;                                   \
	mp->msg_lname = 0;                                 \
	mp->msg_fpointer = 0;                              \
	mp->msg_dests = (address*)0;			   \
	mp->msg_act = (address*)0;			   \
	mp->msg_byi = (char*)0;	         		   \
	mp->msg_forwarder = (address*)0;		   \
	mp->msg_iov = (struct iovec*)0;                    \
	mp->msg_transport = 0;                             \
  }

#define msg_free(mp)                                      \
        if(msg_count < MSG_KEEP)                          \
        {                                                 \
            ++msg_count;                                  \
	    memfree += sizeof(message);                   \
            mp->msg_next = msg_freelist;                  \
            msg_freelist = mp;                            \
        }                                                 \
        else                                              \
	    mdeallocate((char*)mp, &msg_ad)


#define iov_free(io)                                      \
        if(iov_count < IOV_KEEP)                          \
        {                                                 \
            ++iov_count;                                  \
            *((char***)io) = iov_freelist;                \
            iov_freelist = (char**)io;                    \
        }                                                 \
        else                                              \
	    mdeallocate((char*)io, &iov_ad)


#ifdef DB_MESSAGE
static	message   *msg_list;

message *
msg_active(mp)
  register message *mp;
  {
	if(!msg_tracemsgs)
	    return(mp);
	if(msg_list == (message*)0)
	    msg_list = mp;
	else
	{
	    register message *msg, *lmsg = (message*)0;
	    for(msg = msg_list; msg && msg != mp; msg = msg->msg_next)
		lmsg = msg;
	    if(msg == mp)
		return(mp);
	    lmsg->msg_next = mp;
	}
	mp->msg_next = 0;
	return(mp);
  }

void
msg_dumpmsgs()
  {
	register message *msg;
	print("ALLOCATED MSGS:\n");
	for(msg = msg_list; msg; msg = msg->msg_next)
	    msg_printaccess(msg);
  }
#else
void
msg_dumpmsgs()
  {
  }
#endif

/* Create a pseudo-copy */
message *
msg_copy(mp)
  register message *mp;
  {
	register message *nmp;
#ifdef  DB_MESSAGE
        register message *next;
#endif         
	if(msg_count)
	{
	    --msg_count;
	    nmp = msg_freelist;
	    msg_freelist = nmp->msg_next;
	    memalloc += sizeof(message);
	}
	else
	    nmp = (message*)mallocate(&msg_ad);
#ifdef  DB_MESSAGE
	msg_active(nmp);
	next = nmp->msg_next;
	*nmp = *mp;
	nmp->msg_next = next;
#else
	*nmp = *mp;
#endif
	nmp->msg_refs = 1;
	++nmp->msg_hdr->hdr_nlinks;
	nmp->msg_iov = 0;
	++msg_namsgs;
	return(nmp);
  }

void blk_free(bp)
  register block_desc *bp;
  {
	if(--bp->blk_nlinks == -1)
	{
	    if(bp->blk_len > MAXMSGLEN)
		free((caddr_t)bp);
	    else if(bp->blk_len == MSG_BLKSIZE)
	    {
		++blk_count;
		bp->blk_next = blk_freelist;
		blk_freelist = bp;
                memfree += MSG_BLKLEN;
	    }
	    else
	    {
		++mblk_count;
		bp->blk_next = mblk_freelist;
		mblk_freelist = bp;
                memfree += MSG_BLKHDR+MAXMSGLEN;
	    }
	}
	while(blk_count > BLK_KEEP)
	{
	    --blk_count;
	    bp = blk_freelist;
	    blk_freelist = bp->blk_next;
            memalloc += MSG_BLKLEN;
	    mdeallocate((char*)bp, &blk_ad);
	}
	while(mblk_count > MBLK_KEEP)
	{
	    --mblk_count;
	    bp = mblk_freelist;
	    mblk_freelist = bp->blk_next;
            memalloc += MSG_BLKHDR+MAXMSGLEN;
	    mdeallocate((char*)bp, &mblk_ad);
	}
  }

void
MSG_DELETE(mp)
  register message *mp;
  {
	msg_delete(mp);
  }

/* NEVER CALL DIRECTLY */
void
msg_deallocate(mp)
  register message *mp;
  {
	register msg_hdr *hdr = mp->msg_hdr;
	++msg_nfmsgs;
	if(--hdr->hdr_nlinks == 0)
	{
	    register block_desc *bp, *nbp;
	    register msg_fld *fp;
	    if(hdr->hdr_flags&MSG_XFIELDS)
                for(fp = mp->msg_fields; fp; fp = fp->fld_next)
	            if(fp->fld_type == FTYPE_MESSAGE)
		        msg_delete(is_msg_ref(fp)->mr_msg);
	            else if(fp->fld_type == FTYPE_FIELDREF && is_field_ref(fp)->fr_routine)
		        if(is_field_ref(fp)->fr_xref)
		            ISISCALL1(is_field_ref(fp)->fr_routine, &is_field_ref(fp)->fr_ptr);
		        else
		            ISISCALL1(*is_field_ref(fp)->fr_routine, is_field_ref(fp)->fr_ptr);
            for(bp = mp->msg_extents; bp; bp = nbp)
            {
                nbp = bp->blk_next;
		if(--bp->blk_nlinks == -1)
		{
		    if(bp->blk_len > MAXMSGLEN)
			free((caddr_t)bp);
	            else if(bp->blk_len == MSG_BLKSIZE)
		    {
			++blk_count;
			bp->blk_next = blk_freelist;
			blk_freelist = bp;
		        memfree += MSG_BLKLEN;
		    }
		    else
		    {
			++mblk_count;
			bp->blk_next = mblk_freelist;
			mblk_freelist = bp;
		        memfree += MSG_BLKHDR+MAXMSGLEN;
		    }
		}
            }
	    while(blk_count > BLK_KEEP)
	    {
	        --blk_count;
	        bp = blk_freelist;
	        blk_freelist = bp->blk_next;
		memalloc += MSG_BLKLEN;
	        mdeallocate((char*)bp, &blk_ad);
	    }
	    while(mblk_count > MBLK_KEEP)
	    {
	        --mblk_count;
	        bp = mblk_freelist;
	        mblk_freelist = bp->blk_next;
		memalloc += MSG_BLKHDR+MAXMSGLEN;
	        mdeallocate((char*)bp, &mblk_ad);
	    }
	}
        if(mp->msg_iov) {
            iov_free(mp->msg_iov);
	    mp->msg_iov = 0;
	  }
#ifdef  DB_MESSAGE
	if(msg_tracemsgs)
	{
	    register message *msg, *lmsg = (message*)0;
	    for(msg = msg_list; msg && msg != mp; msg = msg->msg_next)
		lmsg = msg;
	    if(msg != mp)
		panic("msg_tracemsgs: %x not on active list!", mp);
	    else if(lmsg)
	        lmsg->msg_next = msg->msg_next;
	    else
	        msg_list = msg->msg_next;
	}
#endif  DB_MESSAGE
	msg_free(mp);
  }

void
msg_deleteall(mp, name)
  register message *mp;
  register name;
  {
	register msg_fld *fp;
	if(mp->msg_hdr->hdr_nlinks > 1)
	    msg_makereal(mp);
	for(fp = mp->msg_fields; fp; fp = fp->fld_next)
	    if(fp->fld_name == (u_char)name)
		fp->fld_name = FTYPE_DELETED;
  }

void
msg_deletefield(mp, name, inst)
  register message *mp;
  register name, inst;
  {
        register msg_fld *fp;
	if(mp->msg_hdr->hdr_nlinks > 1)
	    msg_makereal(mp);
        if(inst > 0)
            for(fp = mp->msg_fields; fp; fp = fp->fld_next)
                if(fp->fld_name == (u_char)name)
		    if(--inst == 0)
		    {
                        fp->fld_name = FTYPE_DELETED; 
			return;
		    }
  }

caddr_t
msg_getfield(mp, name, inst, lenp)
  register message *mp;
  register name, inst, *lenp;
  {
        register msg_fld *fp;
	register i0 = inst;
	if(mp->msg_lname == name && inst >= mp->msg_linst)
	{
	    fp = mp->msg_lfield;
	    inst -= mp->msg_linst-1;
	    goto go;
	}
        if(inst > 0)
	    for(fp = mp->msg_fields; fp; fp = fp->fld_next)
                if(fp->fld_name == (u_char)name)
		go: if(--inst == 0)
		    {
			mp->msg_lname = name;
			mp->msg_linst = i0;
			mp->msg_lfield = fp;
                        if(fp->fld_type != FTYPE_FIELDREF)
			{
			    if(lenp)
			        *lenp = fp->fld_len;
                            return((caddr_t)&fp[1]);
			}
			else
			{
			    if(lenp)
			        *lenp = is_field_ref(fp)->fr_len;
                            return((caddr_t)is_field_ref(fp)->fr_ptr);
			}
		    }
	return((caddr_t)0);
  }

int
msg_getfields(mp, name, datap, lenp, n)
  register message *mp;
  register caddr_t *datap;
  register name, *lenp, n;
  {
        register msg_fld *fp;
	register n0 = n;
        if(n > 0)
            for(fp = mp->msg_fields; fp; fp = fp->fld_next)
                if(fp->fld_name == (u_char)name)
		{
                    if(fp->fld_type != FTYPE_FIELDREF)
		    {
                        *datap++ = (caddr_t)&fp[1];
		        if(lenp)
			    *lenp++ = fp->fld_len;
		    }
		    else
		    {
                        *datap++ = (caddr_t)is_field_ref(fp)->fr_ptr;
		        if(lenp)
			    *lenp++ = is_field_ref(fp)->fr_len;
		    }
		    if(--n == 0)
			return(n0);
		}
	return(n0-n);
  }

int
msg_getiovlen(mp)
  register message *mp;
  {
	if(mp->msg_iov == 0)
	    (void)msg_getiovec(mp);
	return(mp->msg_iovlen);
  }

struct iovec *
msg_getiovec(mp)
  register message *mp;
  {
	register len;
	register block_desc *bp;
	register struct iovec *io;
	if(mp->msg_iov)
	    return(mp->msg_iov);
        if(iov_count)
        {
            --iov_count;
	    io = (struct iovec*)iov_freelist;
            iov_freelist = *(char***)io;
        }
        else
	    io = (struct iovec*)mallocate(&iov_ad);
	mp->msg_iov = io;
	mp->msg_iovlen = 0;
        bp = mp->msg_extents;
	io += mp->msg_nextents;
	len = 0;
	/*
	 * Takes advantage of structure, which is one or more real blocks followed 
         * by one pseudo-block.  msg_len also includes length of indirectly referenced
         * blocks and messages... because the length of real blocks is known
         * accurately, the length of the pseudo-block is always deduced from
         * <msg_len>-<real block lens>-<indirect ref lens>-<imbedded msg lens>
         */
	while(bp)
        {
	    --io;
	    if(bp->blk_next)
	    {
	        io->iov_base = bp->blk_data;
	        io->iov_len = bp->blk_len-bp->blk_avail;
		len += io->iov_len;
	    }
	    else  /* For last block, size is computed */
	    {
                /* Careful... might be a pseudo-block */
	        if((io->iov_base = mp->msg_base) == (char*)0)
		    io->iov_base = bp->blk_data;
		len = mp->msg_len-len;
		io->iov_len = PADDED_LEN(len);
	    }
	    mp->msg_iovlen++;
	    bp = bp->blk_next;
        }
	io += mp->msg_nextents;
	begin
	{
	    register msg_fld *fp;
	    if(mp->msg_hdr->hdr_flags&MSG_XFIELDS)
	    {
	        for(fp = mp->msg_fields; fp; fp = fp->fld_next)
	            if(fp->fld_type == FTYPE_FIELDREF)
		    {
                        register padded_len = PADDED_LEN(is_field_ref(fp)->fr_len);
			io->iov_base = is_field_ref(fp)->fr_ptr;
			io->iov_len = padded_len;
			++io;
			++mp->msg_iovlen;
			mp->msg_iov->iov_len -= padded_len;
		    }
	        for(fp = mp->msg_fields; fp; fp = fp->fld_next)
	            if(fp->fld_type == FTYPE_MESSAGE)
	            {
		        register message *msg;
		        register struct iovec *miov;
                        register miol;
		        msg = is_msg_ref(fp)->mr_msg;
		        miov = msg_getiovec(msg);
                        miol = msg->msg_iovlen;
                        if(miol)
                        {
		            bcopy((char*)miov, (char*)io, miol*sizeof(*io));
		            io += miol;
	                    mp->msg_iovlen += miol;
                        }
			mp->msg_iov->iov_len -= msg->msg_len;
	            }
	    }
	}
	if(mp->msg_iovlen > MSG_MAXIOV)
	    panic("iovec overflow, increase MSG_MAXIOV");
	if(mp->msg_iovlen > 4)
	{
	    register miol;
	    register iovec *iop;
	    io = mp->msg_iov;
	    iop = io+1;
	    for(miol = 1; miol < mp->msg_iovlen; iop++, miol++)
	        if(iop->iov_base == io->iov_base+io->iov_len)
		    io->iov_len += iop->iov_len;
	        else if(++io != iop)
		    *io = *iop;
	    mp->msg_iovlen = (io-mp->msg_iov)+1;
	}
	begin
	{
	    register msg_hdr *hdr = mp->msg_hdr;
	    hdr->hdr_len = htonl(mp->msg_len);
	    hdr->hdr_shortcode = MSG_SHORTCODE;
	    hdr->hdr_longcode = MSG_LONGCODE;
            hdr->hdr_dblcode = MSG_DBLCODE;
	    if((*((long*)hdr->hdr_version) = isis_version.LONG) == 0)
	    {
		bcopy(ISIS_VERSION, isis_version.STRING, sizeof(long));
	        *((long*)hdr->hdr_version) = isis_version.LONG;
	    }
	}
	if(mp->msg_len > MSG_SIZELIMIT)
	    panic("message too long to transmit, increase MSG_SIZELIMIT");
	return(mp->msg_iov);
  }

message *
msg_getmsg(mp, name, inst)
  register message *mp;
  register name, inst;
  {
        register msg_fld *fp;
        if(inst > 0)
	    for(fp = mp->msg_fields; fp; fp = fp->fld_next)
                if(fp->fld_name == (u_char)name)
		    if(--inst == 0)
		    {
			register message *msg;
                        if(fp->fld_type != FTYPE_MESSAGE)
			    return((message*)0);
			msg = is_msg_ref(fp)->mr_msg;
			msg_increfcount(msg);
			return(msg);
		    }
	return((message*)0);
  }

int
msg_getmsgs(mp, name, msgs, n)
  register message *mp, **msgs;
  register name, n;
  {
        register msg_fld *fp;
	register n0 = n;
        if(n > 0)
	    for(fp = mp->msg_fields; fp; fp = fp->fld_next)
                if(fp->fld_name == (u_char)name)
		{
		    register message *msg;
		    if(fp->fld_type != FTYPE_MESSAGE)
			panic("msg_getmsgs: ftype %d not a message!", fp->fld_type);
		    msg = is_msg_ref(fp)->mr_msg;
		    msg_increfcount(msg);
		    *msgs++ = msg;
		    if(--n == 0)
			return(n0);
		}
	return(n0-n);
  }

/* msg_getscantype : Return the type of the current SYSFLD_SCAN field. */
msg_getscantype (msg)
  message *msg;
{
    if (msg-> msg_fpointer == 0)
    {
        msg-> msg_fpointer = 1;
    }
    return (msg_gettype(msg, SYSFLD_SCAN, msg-> msg_fpointer));
}

int
msg_gettype(mp, name, inst)
  register message *mp;
  register int name, inst;
  {
        register msg_fld *fp;
	register i0 = inst;
	if(mp->msg_lname == name && inst >= mp->msg_linst)
	{
	    fp = mp->msg_lfield;
	    inst -= mp->msg_linst-1;
	    goto go;
	}
        if(inst > 0)
	    for(fp = mp->msg_fields; fp; fp = fp->fld_next)
                if(fp->fld_name == (u_char)name)
		go: if(--inst == 0)
		    {
                        mp->msg_lname = name;
			mp->msg_linst = i0;
			mp->msg_lfield = fp;
			if(fp->fld_type != FTYPE_FIELDREF)
                            return(fp->fld_type);
                        return(is_field_ref(fp)->fr_type);
		    }
	return(0);
  }

void
msg_callertrace(routine, mp, caller)
  char *routine, *caller;
  message *mp;
  {
  }

void
msg_msgcheck(mp)
  message *mp;
  {
  }

message   *
msg_genmsg(name, data, type, len, null_term)
  int name, type, len, null_term;
  char *data;
  {
	register message *mp;
	if(null_term)
	    panic("old-style msg_genmsg call!");
	mp = msg_newmsg();
	(void)msg_insertfield(mp, name, data, type, len);
	return(mp);
  }

msg_hdr *
msg_inserthdr(mp, old_hdr)
  register message *mp;
  register msg_hdr *old_hdr;
  {
	register msg_hdr *hdr;
	register padded_len;
	register block_desc *bp;
	padded_len = PADDED_LEN(sizeof(msg_hdr));
	if(blk_count)
	{
	    bp = blk_freelist;
	    blk_freelist = bp->blk_next;
	    --blk_count;
	    memalloc += MSG_BLKLEN;
	}
	else
	    bp = ((block_desc*)mallocate(&blk_ad));
	bp->blk_len = MSG_BLKSIZE;
	bp->blk_ptr = bp->blk_data+padded_len;
	bp->blk_next = (block_desc*)0;
	bp->blk_avail = bp->blk_len-padded_len;
	bp->blk_nlinks = 0;
	mp->msg_extents = bp;
	mp->msg_nextents = 1;
	mp->msg_len = padded_len;
	hdr = (msg_hdr*)bp->blk_data;
	mp->msg_hdr = hdr;
	if(old_hdr)
	    *hdr = *old_hdr;
	else
	{
	    hdr->hdr_sender = my_address;
	    hdr->hdr_replyto = my_address;
            hdr->hdr_dest.addr_site = 0;
            hdr->hdr_msgid = 0;
	    hdr->hdr_flags = 0;
	}
	hdr->hdr_nlinks = 1;
	return(hdr);
  }

char    *
msg_insertfield(mp, name, data, type, len)
  register message *mp;
  int name, type, len;
  char *data;
  {
	register msg_fld *fp;
	if(mp->msg_hdr->hdr_nlinks > 1)
	    msg_makereal(mp);
	begin
	{
	    register padded_len;
	    register block_desc *ext;
	    padded_len = PADDED_LEN(len+sizeof(msg_fld));
	    ext = mp->msg_extents;
	    if(ext == (block_desc*)0 || padded_len > ext->blk_avail || ext->blk_nlinks)
	    {
	        ext = msg_malloc(padded_len);
		ext->blk_next = mp->msg_extents;
		mp->msg_extents = ext;
		++mp->msg_nextents;
		ext->blk_avail = ext->blk_len;
	    }
	    fp = (msg_fld*)ext->blk_ptr;
            mp->msg_len += padded_len;
	    ext->blk_ptr += padded_len;
	    ext->blk_avail -= padded_len;
	}
        fp->fld_name = (u_char)name;
	fp->fld_type = (u_char)type;
	fp->fld_len = len;
	fp->fld_next = (msg_fld*)0;
	begin
	{
	    register msg_fld *lfp;
	    if(lfp = mp->msg_last)
	    {
		lfp->fld_next = fp;
                mp->msg_last = fp;
	    }
	    else
		mp->msg_fields = mp->msg_last = fp;
	}
	if(mp->msg_iov)
	{
	    iov_free(mp->msg_iov);
	    mp->msg_iov = (struct iovec*)0;
	}
	++fp;
        if(len)
	    bcopy(data, (char*)fp, len);
	return((char*)fp);
  }

void
msg_insertref(mp, name, data, type, len, routine)
  register message *mp;
  int name, type, len;
  char *data;
  vfunc *routine;
  {
	extern isis_state;
	register msg_fld *fp;
	field_ref fieldref;
	if(len <= 0)
	    panic("msg_insertref: bad field length (%d)", len);
	fieldref.fr_ptr = data;
	fieldref.fr_routine = routine;
	fp = &((msg_fld*)msg_insertfield(mp, name, (char*)&fieldref, FTYPE_FIELDREF, sizeof(fieldref)))[-1];
	is_field_ref(fp)->fr_len = len;
	is_field_ref(fp)->fr_type = type;
	is_field_ref(fp)->fr_xref = (isis_state& /* ISIS_XBYREF */ 0x400)? 1: 0;
	mp->msg_len += PADDED_LEN(len);
	mp->msg_hdr->hdr_flags |= MSG_XFIELDS;
  }

/* Actually inserts a ``pseudo copy'' of the message */
void
msg_insertmsg(mp, name, msg)
  register message *mp;
  message *msg;
  int name;
  {
	register message *rmsg = msg;
	msg_increfcount(rmsg);
	msg_insertfield(mp, name, (char*)&msg, FTYPE_MESSAGE, sizeof(message**));
	mp->msg_len += rmsg->msg_len;
	mp->msg_hdr->hdr_flags |= MSG_XFIELDS;
  }

void
msg_printheader(mp)
  register message *mp;
  {
	register msg_hdr *hdr = mp->msg_hdr;
	register address *ap;
        if(hdr->hdr_nlinks > 1)
	    print("PSEUDO_");
	print("MSG %x: len %d refs %d flag 0x%.2x, nlinks %d", mp, mp->msg_len, mp->msg_refs, hdr->hdr_flags, hdr->hdr_nlinks);
	if(mp->msg_base)
	    print(", base %x", mp->msg_base);
	print("\nSender "); paddr(&hdr->hdr_sender);
	print(", msgid %d", hdr->hdr_msgid);
	if(hdr->hdr_msgid&1)
	    print(", replyto "); paddr(&hdr->hdr_replyto);
	if(ap = msg_getdests(mp))
	{
	    print(", dests ");
	    paddrs(ap);
	}
	print("\n");
	if(mp->msg_lname)
	    print("... last found: field 0x%.2x inst %d ptr %x\n", mp->msg_lname, mp->msg_linst, mp->msg_lfield);
  }

void
msg_printaccess(mp)
  register message *mp;
  {
	register msg_fld *fp;
	register struct iovec *io;
	static void pblks();

	msg_printheader(mp);
	for(fp = mp->msg_fields; fp; fp = fp->fld_next)
            pfld(fp);
        pblks(mp->msg_extents, 0, mp->msg_len, mp->msg_base);
	if(io = mp->msg_iov)
	{
	    register niov = mp->msg_iovlen;
	    while(niov--)
	    {
	        print("  iovec starts %x len %d\n", io->iov_base, io->iov_len);
		++io;
	    }
	}
	for(fp = mp->msg_fields; fp; fp = fp->fld_next)
	    if(fp->fld_type == FTYPE_MESSAGE)
	        msg_printaccess(is_msg_ref(fp)->mr_msg);
	
  }

static void pfld(fp)
  register msg_fld *fp;
  {
	switch(fp->fld_type)
        {
          default:
	    print("  Field 0x%.2x: <%s> len %d at %x\n", fp->fld_name, msg_typename(fp->fld_type), fp->fld_len, &fp[1]);
	    break;
          case FTYPE_DELETED:
	    break;
          case FTYPE_MESSAGE:
	    print("  Field 0x%.2x: fld_len %d REF <message> %x\n", fp->fld_name, fp->fld_len, is_msg_ref(fp)->mr_msg);
	    break;
          case FTYPE_FIELDREF:
	    begin
	    {
	        register field_ref *fr = is_field_ref(fp);
	        print("  Field 0x%.2x: fld_len %d REF <%s> len %d routine %x data %x\n", fp->fld_name, fp->fld_len, msg_typename(fr->fr_type), fr->fr_len, fr->fr_routine, fr->fr_ptr);
	    }
        }
  }

static void pblks(bp, len, mlen, base)
  register block_desc *bp;
  register mlen, len;
  register char *base;
  {
	if(bp == 0)
	    return;
	pblks(bp->blk_next, len+bp->blk_len-bp->blk_avail, mlen, base);
        if(bp->blk_next)
	    print("  Block %x len %d unused %d nlinks %d\n", bp, bp->blk_len, bp->blk_avail, bp->blk_nlinks);
	else
	    print("  Pseudo-block base %x in block %x len %d unused %d nlinks %d\n", base, bp, mlen-len, bp->blk_avail, bp->blk_nlinks);
  }

/*
 * FTYPE_FIELDREF with multiple refs to it managed by these routines 
 *   fref_register:  first time, list with 2 refs, later inc ref count
 *   fref_countdown: count refs, free when down to 0
 * This mechanism should essentially never be used.  You
 * would need to generate multiple refs to a FIELDREF field
 * and then change one of the messages by modifying its contents...
 */

typedef struct	reflist reflist;

struct	reflist
{
	char	*rl_ptr;
	vfunc	*rl_routine;
	int 	rl_count;
};

/* Can be short -- seems really unlikely this could ever get long */
#define FREFLEN        8

reflist frefs[FREFLEN];

void
fref_countdown(ptr)
  register char *ptr;
  {
        register reflist *rp;
	for(rp = frefs; rp != &frefs[FREFLEN]; rp++)
	    if(rp->rl_ptr == ptr || rp->rl_ptr == (char*)0)
		break;
	if(rp->rl_ptr != ptr)
	    panic("fref_countdown");
	if(--rp->rl_count)
	    return;
        (*rp->rl_routine)(rp->rl_ptr);
        rp->rl_routine = (vfunc*)0;
  }

void
fref_register(fr)
  register field_ref *fr;
  {
        register reflist *rp;
	if(fr->fr_routine == (vfunc *) fref_countdown)
	{
	    for(rp = frefs; rp != &frefs[FREFLEN]; rp++)
	        if(rp->rl_ptr == fr->fr_ptr || rp->rl_ptr == (char*)0)
		    break;
	    if(rp->rl_ptr != fr->fr_ptr)
	        panic("fref_register");
	}
	else
	{
	    for(rp = frefs; rp != &frefs[FREFLEN]; rp++)
	        if(rp->rl_routine == (vfunc*)0)
		    break;
	    if(rp == &frefs[FREFLEN])
		panic("fref_register: list overflow");
	    rp->rl_routine = fr->fr_routine;
	    rp->rl_ptr = fr->fr_ptr;
	    rp->rl_count = 1;
	}
	++rp->rl_count;
  }

/* Given a message that was a copy, make it into a real one */
void
msg_makereal(mp)
  register message *mp;
  {
	register msg_fld *fp = mp->msg_fields;
	register msg_hdr *hdr = mp->msg_hdr;
	--hdr->hdr_nlinks;
	begin
	{
	    register message *next = mp->msg_next;
            msg_init(mp);
	    mp->msg_next = next;
	}
	hdr = msg_inserthdr(mp, hdr);
	if(fp) do switch(fp->fld_type)
	{
	  case FTYPE_DELETED:
	      continue;
	  case FTYPE_MESSAGE:
	      msg_insertmsg(mp, fp->fld_name, is_msg_ref(fp)->mr_msg);
	      continue;
	  case FTYPE_FIELDREF:
	      begin
	      {
	          register msg_fld *nfp;
		  register field_ref *fr = is_field_ref(fp);
	          nfp = &((msg_fld*)msg_insertfield(mp, fp->fld_name, (char*)&fp[1], fp->fld_type, fp->fld_len))[-1];
	          fref_register(fr);
	          fr->fr_routine = (vfunc *) fref_countdown;
	          is_field_ref(nfp)->fr_routine = (vfunc *) fref_countdown;
	          is_field_ref(nfp)->fr_xref = 0;
	      }
	      continue;
	  default:
	      msg_insertfield(mp, fp->fld_name, (char*)&fp[1], fp->fld_type, fp->fld_len);
	      continue;
	}
	while(fp = fp->fld_next);
  }

message *
msg_newmsg()
  {
	register message *mp;
	if(msg_count)
	{
	    --msg_count;
	    mp = msg_freelist;
	    msg_freelist = mp->msg_next;
	    memalloc += sizeof(message);
	}
	else
	    mp = (message*) mallocate(&msg_ad);
#ifdef  DB_MESSAGE
	msg_active(mp);
#endif
	msg_init(mp);
	++msg_namsgs;
	(void)msg_inserthdr(mp, (msg_hdr*)0);
	return(mp);
  }

/* Allocate space to read and reconstruct <len> byte message */
block_desc *
msg_malloc(len)
  register len;
  {
	register block_desc *bp;
	if(len < 0 || (len&3) || len > MSG_SIZELIMIT)
	{
	    
	    print("WARNING: msg library: invalid message len (%d)\n", len);
	    return((block_desc*)0);
	}
	if(len <= MSG_BLKSIZE)
	{
            if(blk_count)
	    {
		bp = blk_freelist;
		blk_freelist = bp->blk_next;
	        memalloc += MSG_BLKLEN;
	        --blk_count;
	    }
	    else
	    {
	        bp = ((block_desc*)mallocate(&blk_ad));
	        bp->blk_len = MSG_BLKSIZE;
	    }
	}
	else if(len <= MAXMSGLEN)
	{
            if(mblk_count)
	    {
		bp = mblk_freelist;
		mblk_freelist = bp->blk_next;
	        memalloc += MSG_BLKHDR+MAXMSGLEN;
	        --mblk_count;
	    }
	    else
	    {
	        bp = ((block_desc*)mallocate(&mblk_ad));
	        bp->blk_len = MAXMSGLEN;
	    }
	}
	else
	{
	    if((bp = (block_desc*)malloc(len+MSG_BLKHDR)) == 0)
		panic("msg_malloc: no more space");
	    bp->blk_len = len;
	}
	bp->blk_ptr = bp->blk_data;
	bp->blk_next = (block_desc*)0;
	bp->blk_avail = bp->blk_len;
	bp->blk_nlinks = 0;
	return(bp);
  }

message *
msg_doreconstruct(bp)
  register block_desc *bp;
  {
	register msg_hdr *hdr;
	register message *mp;
	register char *data;
	register len;
	char *bp_end;
	msg_fld *ffp = (msg_fld*)0, *fmp = (msg_fld*)0;
	int convert;
#define dfp      ((msg_fld*)data)

	hdr = (msg_hdr*)bp->blk_ptr;
	hdr->hdr_nlinks = 1;
	if(msg_count)
	{
	    --msg_count;
	    mp = msg_freelist;
	    msg_freelist = mp->msg_next;
	    memalloc += sizeof(message);
	}
	else
	    mp = (message*) mallocate(&msg_ad);
#ifdef  DB_MESSAGE
	msg_active(mp);
#endif
	msg_init(mp);
	++msg_namsgs;
	mp->msg_extents = bp;
	mp->msg_nextents = 1;
	while(*(int*)hdr->hdr_version != isis_version.LONG)
	{
	    if(!isis_version.LONG)
	    {
		bcopy(ISIS_VERSION, isis_version.STRING, 4);
		continue;
	    }
	    print("WARNING "); paddr(&my_address);
	    print(": msg_reconstruct -- invalid message or from incompatible version of ISIS (%s)\n", hdr->hdr_version);
	    msg_delete(mp);
	    return((message*)0);
	}
	if(convert = (hdr->hdr_shortcode != MSG_SHORTCODE || hdr->hdr_longcode != MSG_LONGCODE || hdr->hdr_dblcode != MSG_DBLCODE))
	{
	    msg_makekey(hdr->hdr_shortcode, hdr->hdr_longcode, hdr->hdr_dblcode);
            msg_converthdr(hdr);
	}
	bp_end = &bp->blk_data[bp->blk_len-bp->blk_avail];
	len = ntohl(hdr->hdr_len);
	if(len < 0 || &bp->blk_data[len] > bp_end)
	{
	    print("WARNING: msg_doreconstruct: imbedded message has impossible length %d\n", len);
	    msg_delete(mp);
	    return((message*)0);
	}
	mp->msg_len = len;
	mp->msg_hdr = hdr;
	len -= sizeof(msg_hdr);
	data = (char*)&hdr[1];
	while(len)
	{
	    register fld_len;
	    register msg_fld *next;
	    if(mp->msg_fields == 0)
		mp->msg_fields = mp->msg_last = dfp;
	    else
	    {
		mp->msg_last->fld_next = dfp;
		mp->msg_last = dfp;
	    }
            next = dfp->fld_next;
	    dfp->fld_next = 0;
	    if(convert)
		msg_convertfield(dfp, FALSE);
	    fld_len = PADDED_LEN(sizeof(msg_fld)+dfp->fld_len);
	    if(fld_len < 0 || fld_len > len)
	    {
	        print("WARNING: msg_doreconstruct: message field has impossible length %d\n", fld_len);
	        msg_delete(mp);
	        return((message*)0);
	    }
            if(dfp->fld_type == FTYPE_FIELDREF && ffp == (msg_fld*)0)
		ffp = dfp;
            else if(dfp->fld_type == FTYPE_MESSAGE && fmp == (msg_fld*)0)
		fmp = dfp; 
	    else switch(dfp->fld_name)
	    {
	    case SYSFLD_DESTS:
		mp->msg_dests = (address*)(&dfp[1]);
		break;
	    case SYSFLD_BYPASS:
		mp->msg_byi = (char*)(&dfp[1]);
		break;
	    case SYSFLD_ACT:
		mp->msg_act = (address*)(&dfp[1]);
		break;
	    case SYSFLD_FORWARD:
		mp->msg_forwarder = (address*)(&dfp[1]);
		break;
	    }
	    len -= fld_len;
	    data += fld_len;
	    if(next == 0)
	        break;
	    if(data >= bp_end)
	        panic("msg_doreconstruct: message field has type %d impossible length %d\n", dfp->fld_type, fld_len);
	}
	if(ffp || fmp)
	{
	    int bad = 0;
	    register msg_fld *fp;
	    hdr->hdr_flags |= MSG_XFIELDS;
	    for(fp = ffp; fp; fp = fp->fld_next)
		if(fp->fld_type == FTYPE_FIELDREF)
		{
		    register fld_len;
		    register field_ref *fr = is_field_ref(fp);
		    fr->fr_ptr = data;
		    fr->fr_routine = (vfunc*)0;
                    fld_len = fr->fr_len;
                    if(convert)
	                msg_convertfield(fp, TRUE);
                    fld_len = PADDED_LEN(fld_len);
                    data += fld_len;
		    len -= fld_len;
		}
	    for(fp = fmp; fp; fp = fp->fld_next)
	        if(fp->fld_type == FTYPE_MESSAGE)
	        {
		    register message *msg;
		    bp->blk_ptr = data;
	 	    if((msg = msg_doreconstruct(bp)) == (message*)0)
		    {
		        fp->fld_name = FTYPE_DELETED;
			++bad;
		    }
		    else
		    {
		        msg->msg_base = data;
		        bp->blk_nlinks++;
	 	        is_msg_ref(fp)->mr_msg = msg;
		        len -= msg->msg_len;
		        data += msg->msg_len;
		    }
	        }
	    if(bad)
	    {
	        msg_delete(mp);
		mp = (message*)0;
	    }
	}
	if(len)
	    print("WARNING: msg_reconstruct -- (residual len %d)\n", len);
	bp->blk_ptr = data;
	return(mp);
  }

message *
msg_reconstruct(bp)
  block_desc *bp;
  {
  	register message *mp;
	if((mp = msg_doreconstruct(bp)) == (message*)0)
	    return(mp);
	if(bp->blk_len-bp->blk_avail != mp->msg_len)
	{
	    print("WARNING: msg_reconstruct -- message len %d expected %d\n", bp->blk_len-bp->blk_avail, mp->msg_len);
	    msg_delete(mp);
	    return((message*)0);
	}
	return(mp);
  }

char    *
msg_replacefield(mp, name, data, type, len)
  register message *mp;
  int name;
  char *data;
  register type, len;
  {
	register msg_fld *fp;
	if(mp->msg_hdr->hdr_nlinks > 1)
	    msg_makereal(mp);
	fp = mp->msg_fields;
        while(fp && fp->fld_name != (u_char)name)
	    fp = fp->fld_next;
	if(fp)
	{
	    if(fp->fld_type == FTYPE_MESSAGE || fp->fld_type == FTYPE_FIELDREF)
		print("WARNING: msg_replacefield: illegal prior field type (%s)\n", msg_typename(fp->fld_type));
	    if(fp->fld_len == len)
	    {
		fp->fld_type = (u_char)type;
	        ++fp;
		if(len)
		    bcopy(data, (char*)fp, len);
		return((char*)fp);
	     }
	     else
		fp->fld_name = FTYPE_DELETED;
	}
        if(type == FTYPE_MESSAGE)
	    print("WARNING: msg_replacefield: illegal new field type (%s)\n", msg_typename(type));
	return(msg_insertfield(mp, name, data, type, len));
  }

address *
msg_setdests(mp, dests)
  register message *mp;
  register address *dests;
  {
        register len = sizeof(address);
	register address *alist = dests;
	register msg_fld *fp;
	register address *base;
        while(!aptr_isnull(alist))
	{
            ++alist;
	    len += sizeof(address);
	}
	if(base = mp->msg_dests)
	{
	    fp = (msg_fld*)base;
	    if((--fp)->fld_len >= len)
	    {
		base = (address*)&fp[1];
		bcopy(dests, (char*)base, len);
		return(base);
	    }
	    fp->fld_name = FTYPE_DELETED;
	}
	base = (address*)msg_insertfield(mp, SYSFLD_DESTS, (char*)dests, FTYPE_ADDRESS, len);
	mp->msg_dests = base;
	return(base);
  }	

int
alist_len(alist)
  register address *alist;
  {
        register n = 0;
        while(!aptr_isnull(alist))
            ++alist, ++n;
        return(n);
  }
