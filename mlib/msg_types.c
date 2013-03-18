/*  $RCSfile: msg_types.c,v $ $Revision: 2.1 $ $Date: 90/07/30 14:20:36 $  */
/*
 *      Originally coded by Tommy Joseph, modified by Ken Birman
 *
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
 *
 */

#define     ISIS_SYS
#include    "isis.h"
#include    "pr_intersite.h"

/* Except on Sparc, putting this in static memory gives a better addressing mode */
static	short	s_temp;
static	long	l_temp;
static  int     msg_tinited;

#define     msg_doconvertshort(sp)          {                      \
        s_temp = *(short *)(sp);                                   \
   ((char *)(sp))[msg_shortkey[0]] =((char *)&s_temp)[0];          \
   ((char *)(sp))[msg_shortkey[1]] =((char *)&s_temp)[1];          \
}

#define     msg_doconvertlong(lp)           {                      \
        l_temp = *(long *)(lp);                                    \
   ((char *)(lp))[msg_longkey[0]] =((char *)&l_temp)[0];           \
   ((char *)(lp))[msg_longkey[1]] =((char *)&l_temp)[1];           \
   ((char *)(lp))[msg_longkey[2]] =((char *)&l_temp)[2];           \
   ((char *)(lp))[msg_longkey[3]] =((char *)&l_temp)[3];           \
 }


#define     msg_doconvertaddress(ap)        {                      \
        msg_doconvertshort(&(ap)->addr_process)                    \
        msg_doconvertshort(&(ap)->addr_portno);                    \
        msg_doconvertshort(&(ap)->addr_entry);                     \
}

u_short     msg_shortmask = 0x1;
u_long      msg_longmask  = 0x10203;
char        msg_shortkey[2];
char        msg_longkey[4];
int         msg_dblkey;
#define     MSG_KEY(i,x)       (((u_char)((x) &(0x3 <<(6 - 2 *(i))))) >> \
                                                                (6 - 2 *(i)))

typedef     struct msg_tdesc    msg_tdesc;

struct msg_tdesc
{
        u_char  type;
        u_short size;
        void    (*converter)();
        char    *name;
};

static      msg_tdesc*  msg_typeptr[0x100];

void        msg_tinit();
void        msg_convertdouble();

static  double dbl = 1.0;

#define MSG_DBLCODE	(((int*)&dbl)[0]==0)

void
msg_converthdr(hdr)
  register msg_hdr *hdr;
  {
        hdr->hdr_shortcode = MSG_SHORTCODE;
        hdr->hdr_longcode = MSG_LONGCODE;
	hdr->hdr_dblcode = MSG_DBLCODE;
	msg_doconvertaddress(&hdr->hdr_sender);
	msg_doconvertaddress(&hdr->hdr_replyto);
	msg_doconvertaddress(&hdr->hdr_dest);
	msg_doconvertlong(&hdr->hdr_msgid);
  }

void
msg_convertfield(afp, fref_flag)
  msg_fld  *afp;
  int      fref_flag;
  {
        register msg_fld    *fp;
        register msg_tdesc  *t_desc;
        register char       *start, *end;
        register            fld_len, type;
	fp = afp;
        type = fp->fld_type;
        if(type == FTYPE_FIELDREF)
        {
            register field_ref *fr = is_field_ref(fp);
            if(fref_flag == 0)
            {
                msg_doconvertlong(&fp->fld_len);
                msg_doconvertlong(&fr->fr_len);
                return;
            }
            start = fr->fr_ptr;
	    fld_len = fr->fr_len;
            type = fr->fr_type;
        }
        else
        {
            msg_doconvertlong(&fp->fld_len);
            start =(char*)&fp[1];
            fld_len = fp->fld_len;
        }
        if(type == 0 || type == FTYPE_CHAR || type == FTYPE_MESSAGE)
            return;
        if((t_desc = msg_typeptr[type]) == 0)
        {
            print("msg_convertfield: Warning! Unknown type %d\n", type);
            return;
        }
        if(t_desc->converter == 0)
            return;
        if(t_desc->size == 0) /* For FTYPE_PGROUP, which has variable size */
        {
            ISISCALL2(t_desc->converter,(char*)&fp[1], fp->fld_len);
            return;
        }

        /* Modified code to take advantage of in-line performance - tclark 7/20/90 */
        end = start + fld_len - t_desc->size;
        switch(type)
	  {
          case FTYPE_LONG:
             for( ; start <= end; start += sizeof(long) )
                msg_doconvertlong(start);
             break;
       
          case FTYPE_SHORT:
             for( ; start <= end; start += sizeof(short) )
                msg_doconvertshort(start);
             break;
          
          case FTYPE_DOUBLE:
          case FTYPE_FLOAT: 
          default:
             for( ; start <= end; start += t_desc->size )
                ISISCALL2(t_desc->converter, start, fp->fld_len);
             break;
	   }
}
        
void
msg_makekey(shortcode, longcode, dblcode)
  int       shortcode, longcode, dblcode;
  {
        register int    i, j;
        if(msg_tinited == 0)
	    msg_tinit();
        if(MSG_KEY(0, shortcode) ==((char *)(&msg_shortmask))[0])
        {
            msg_shortkey[0] = 0;
            msg_shortkey[1] = 1;
        }
        else
        {
            msg_shortkey[0] = 1;
            msg_shortkey[1] = 0;
        }
	if(dblcode == MSG_DBLCODE)
	    msg_dblkey = 0;
	else
	    msg_dblkey = 1;
        for(i = 0; i < 4; i++)
            for(j = 0; j < 4; j++)
                if(MSG_KEY(i, longcode) ==((char *) &msg_longmask)[j])
                    msg_longkey[i] = j;
  }

void
msg_convertshort(shortp)
  register short *shortp;
  {
        msg_doconvertshort(shortp);
  }

void
msg_convertlong(longp)
  register long *longp;
  {
        msg_doconvertlong(longp);
  }

void
msg_convertfloat(floatp)
  register float *floatp;
  {
        msg_doconvertlong(((long*)floatp));
  }

void
msg_convertdouble(dblp)
  register double *dblp;
  {
	register long *dlp;
	dlp = (long*)dblp;
        msg_doconvertlong(dlp);
        msg_doconvertlong(&dlp[1]);
	if(msg_dblkey)
	{
	    register long temp;
	    temp = dlp[1];
	    dlp[1] = dlp[0];
	    dlp[0] = temp;
	}
  }

void
msg_convertaddress(addrp)
  register address  *addrp;
  {
        msg_doconvertshort(&addrp->addr_process);
        msg_doconvertshort(&addrp->addr_portno);
        msg_doconvertshort(&addrp->addr_entry);
  }

void
msg_convertbitv(bp)
  register bitvec  *bp;
  {
#       if(BVL != 4)
        {
            register int    i;
            for(i = 0; i < ISIS_BVL; i++)
                msg_doconvertlong(&bp->bv_data[i]);
        }
#       else
        {
            msg_doconvertlong(&bp->bv_data[0]);
            msg_doconvertlong(&bp->bv_data[1]);
            msg_doconvertlong(&bp->bv_data[2]);
            msg_doconvertlong(&bp->bv_data[3]);
        }
#       endif
  }

void
msg_convertpgroup(pg)
  register sys_groupview    *pg;
  {
        register address *ap;
        register int n_addr, n_addrs;

        msg_doconvertlong(&pg->pg_viewid);
        msg_doconvertlong(&pg->pg_incarn);
        msg_doconvertaddress(&pg->pg_gid);
        msg_doconvertshort(&pg->pg_nmemb);
        msg_doconvertshort(&pg->pg_nclient);

	n_addrs = pg->pg_nmemb+pg->pg_nclient+3;
	ap = pg->pg_alist;
	for(n_addr = 0; n_addr < n_addrs; n_addr++, ap++)
            msg_doconvertaddress(ap);
  }

void
msg_convertverify(vi)
  register verify    *vi;
  {
        msg_doconvertaddress(&vi->vi_gid);
        msg_doconvertlong(&vi->vi_viewid);
        msg_convertbitv(&vi->vi_sites);
  }

void
msg_convertgroupview(gv)
  register struct groupview    *gv;
  {
        register address *ap;
        register int n_addr;

        msg_doconvertlong(&gv->gv_viewid);
        msg_doconvertlong(&gv->gv_incarn);
        msg_doconvertlong(&gv->gv_flag);
        msg_doconvertaddress(&gv->gv_gaddr);
        msg_doconvertshort(&gv->gv_nmemb);
        msg_doconvertshort(&gv->gv_nclient);
	n_addr = 0;
        for(ap = gv->gv_members; n_addr <= gv->gv_nmemb; ap++, n_addr++)
            msg_doconvertaddress(ap);
	n_addr = 0;
        for(ap = gv->gv_clients; n_addr <= gv->gv_nclient; ap++, n_addr++)
            msg_doconvertaddress(ap);
        msg_doconvertaddress(&gv->gv_joined);
        msg_doconvertaddress(&gv->gv_departed);
  }


void
msg_convertsview(sv)
  register struct sview    *sv;
  {
        register site_id *sid;

        msg_doconvertlong(&sv->sv_viewid);
        for(sid = sv->sv_slist; *sid; sid++)
           msg_doconvertshort((short *)sid);           
        msg_convertbitv(&sv->sv_failed);
        msg_convertbitv(&sv->sv_recovered);
        
  }


void
msg_convertgldesc(gl)
  register gl_desc *gl;
  {
        msg_doconvertlong(&gl->gl_viewid);
        msg_doconvertlong(&gl->gl_nmembers);
        msg_doconvertlong(&gl->gl_nclients);
        msg_convertbitv(&gl->gl_sites);
        msg_doconvertaddress(&gl->gl_addr);
  }

void
msg_convertevent(eid)
  register event_id *eid;
  {
        msg_doconvertlong(&eid->e_msgid);
        msg_doconvertlong(&eid->e_op);
        msg_doconvertaddress(&eid->e_pname);
  }

void
msg_convertintersite(is)
  register intersite    *is;
  {
        msg_doconvertshort(&is->is_from);
        msg_doconvertshort(&is->is_dest);
        msg_doconvertshort(&is->is_seqn);
        msg_doconvertshort(&is->is_aseqn);
        msg_doconvertshort(&is->is_viewid);
        msg_doconvertlong(&is->is_abits);
        msg_doconvertlong(&is->is_abprio);
  }

void
msg_convertinterclient(ic)
  register interclient  *ic;
  {
        msg_doconvertaddress(&ic->ic_from);
        msg_doconvertaddress(&ic->ic_dest);
        msg_doconvertshort(&ic->ic_seqn);
        msg_doconvertshort(&ic->ic_aseqn);
        msg_doconvertlong(&ic->ic_abits);
  }

void
msg_convertbypass(bv, len)
  register by_info *bv;
  register len;
  {
        register *bp;

        msg_doconvertaddress(&bv->by_group);
        msg_doconvertaddress(&bv->by_dest);
        msg_doconvertlong(&bv->by_viewid);
        msg_doconvertlong(&bv->by_myseqn);
        len =(len-byi_size(0)) >> 2;
        bp = bv->by_bseq;
        while(len)
        {
            msg_doconvertlong(bp);
            ++bp; --len;
        }
  }

adesc   ty_adesc ={ sizeof(msg_tdesc), 0, 2};
#define ty_alloc()  ((msg_tdesc*)mallocate(&ty_adesc))

int
msg_definetype(type, size, converter, string)
  int       type;
  int       size;
  void      (*converter)();
  char      *string;
  {
        register msg_tdesc  *t_desc;
        if(msg_tinited == 0)
	    msg_tinit();
        if(type == 0 || type > 0x80)
        {
            print("msg_definetype: invalid type number %d(%s)\n", type, string);
            return(-1);
        }
        if(t_desc = msg_typeptr[type])
        {
            print("msg_definetype: attempt to redefine type %d(%s)\n", type, t_desc->name);
            return(-1);
        }
        t_desc = msg_typeptr[type] = ty_alloc();
        t_desc->size = size;
        t_desc->converter = converter;
        if(string == 0)
        {
            string = malloc(8);
            sprintf(string, "type%d", type);
        }
        t_desc->name = string;
        (++t_desc)->type = 0;
        return(0);
  }

char *
msg_typename(type)
  register int      type;
  {
        register msg_tdesc  *t_desc;
        if(msg_tinited == 0)
	    msg_tinit();
        if(type == FTYPE_MESSAGE)
            return("message");
        if(type == FTYPE_FIELDREF)
            return("REF");
        if(t_desc = msg_typeptr[type])
            return(t_desc->name);
        return("unknown");
}

msg_tdesc   msg_typetable[] =
{       FTYPE_CHAR,     1,                  0,                      "char",
        FTYPE_SHORT,    2,                  msg_convertshort,       "short",
        FTYPE_LONG,     4,                  msg_convertlong,        "long",
        FTYPE_ADDRESS,  sizeof(address),   msg_convertaddress,     "address",
        FTYPE_SITEID,   2,                  msg_convertshort,       "siteid",
        FTYPE_PGROUP,   0,                  msg_convertpgroup,      "pgroup",
        FTYPE_VERIFY,   0,                  msg_convertverify,      "verify",
        FTYPE_INTERSITE, sizeof(intersite), msg_convertintersite,  "intersite",
        FTYPE_INTERCLIENT, sizeof(interclient), msg_convertinterclient,  "interclient",
        FTYPE_BITVEC,   4,                  msg_convertlong,        "bitvec",
        FTYPE_GROUPVIEW, sizeof(struct groupview),
                                        msg_convertgroupview,   "groupview",
        FTYPE_EVENT,    sizeof(event_id),  msg_convertevent,       "event_id",
        FTYPE_GLDESC,   sizeof(gl_desc),   msg_convertgldesc,      "gldesc",
        FTYPE_BYPASS,   sizeof(by_info),   msg_convertbypass,      "byinfo",
        FTYPE_FLOAT,    sizeof(float),     msg_convertfloat,       "float",
        FTYPE_DOUBLE,   sizeof(double),    msg_convertdouble,      "double",
        FTYPE_SVIEW,    sizeof(sview),     msg_convertsview,       "sview",
        0
};

void
msg_tinit()
  {
	register msg_tdesc *t_desc;
	++msg_tinited;
	for(t_desc = msg_typetable; t_desc->type; t_desc++)
	    msg_typeptr[t_desc->type] = t_desc;
  }
