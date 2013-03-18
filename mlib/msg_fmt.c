/*  $RCSfile: msg_fmt.c,v $ $Revision: 2.13 $ $Date: 90/08/01 21:04:50 $  */
/*
 *	Originally coded by Ken Birman
 *      Routines for formatted insertion/scanning of messages
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
 */

#include "isis.h"
#include "ctype.h"

typedef struct
{
        int     fi_size;
        int     fi_type;
} format_item;

#define FMESSAGE        -1
#define FSTRING         -2

format_item     format_table[26] 
={
        sizeof(address),        FTYPE_ADDRESS,          /* 'a' */
        sizeof(bitvec),         FTYPE_BITVEC,           /* 'b' */
        sizeof(char),           FTYPE_CHAR,             /* 'c' */
        sizeof(int),            FTYPE_LONG,             /* 'd' */
        sizeof(event_id),       FTYPE_EVENT,            /* 'e' */
        sizeof(float),          FTYPE_FLOAT,            /* 'f' */
        sizeof(double),         FTYPE_DOUBLE,           /* 'g' */
        sizeof(short),          FTYPE_SHORT,            /* 'h' */
        0,                      -1,                     /* 'i' */
        0,                      -1,                     /* 'j' */
        0,                      -1,                     /* 'k' */
        sizeof(int),            FTYPE_LONG,             /* 'l' */
        FMESSAGE,               FTYPE_MESSAGE,          /* 'm' */
        0,                      -1,                     /* 'n' */
        0,                      -1,                     /* 'o' */
        sizeof(groupview),      FTYPE_GROUPVIEW,        /* 'p' */
        0,                      -1,                     /* 'q' */
        0,                      -1,                     /* 'r' */
        FSTRING,                FTYPE_CHAR,             /* 's' */
        0,                      -1,                     /* 't' */
        0,                      -1,                     /* 'u' */
        0,                      -1,                     /* 'v' */
        0,                      -1,                     /* 'w' */
        0,                      -1,                     /* 'x' */
        0,                      -1,                     /* 'y' */
        0,                      -1,                     /* 'z' */
  };

#ifdef FUN_TYPES
int     _msg_doputf(message *mp, char *format, int fname, va_list *ap,
                    char **dp);
int     _msg_dogetf(message **mlist, int nmsgs, int fname, int *fpt,
                    va_list *argp, char *fmt, char **dp);
#else
int     _msg_doputf();
int     _msg_dogetf();
#endif

message *
msg_gen(va_alist)
  va_dcl
  {
        va_list ap;
        register message *mp = msg_newmsg();
        register rval;
        va_start(ap);
        rval = msg_doputf(mp, SYSFLD_SCAN, &ap);
        va_end(ap);
        if(rval == 0)
            return(mp);
        msg_delete(mp);
        return((message*)0);
  }

int
msg_put(va_alist)
  va_dcl
  {
        va_list ap;
        message *mp;
        register rval;
        va_start(ap);
	mp = VA_ARG(ap, message*);
        rval = msg_doputf(mp, SYSFLD_SCAN, &ap);
        va_end(ap);
        return(rval);
  }

int
msg_putfld(va_alist)
  va_dcl
  {
        va_list ap;
        message *mp;
        register rval, fname;
        va_start(ap);
	mp = VA_ARG(ap, message*);
	fname = VA_ARG(ap, int);
        rval = msg_doputf(mp, fname, &ap);
        va_end(ap);
        return(rval);
  }

int
msg_doputf(mp, fname, ap)
  message *mp;
  int     fname;
  va_list *ap;
  {
        char *format = VA_REF(*ap, char*);
	return(_msg_doputf(mp, format, fname, ap, (char**)0));
  }

int
_msg_doputf(mp, format, fname, ap, dp)
  register message *mp;
  register char *format;
  int     fname;
  va_list *ap;
  register char **dp;
  {
        register c;
        isis_errno = 0;
        while(c = *format++)
            if(c == '%')
            {
                int vector = 0, *where, len, by_ref = 0;
                register format_item *fi;
                c = *format++;
		if(c == '*')
	        {
		    ++by_ref;
		    c = *format++;
		}
                if(!isascii(c))
                {
                    isis_errno = IE_BADFITEM;
                    return(-1);
                }
                if(isupper(c))
                {
                    ++vector;
                    c = tolower(c);
		    if(dp)
		    {
			where = *((int**)*dp);
			*dp += sizeof(int);
		    }
                    else if(c != 'a')
                        where = VA_REF(*ap, int*);
                    else /* Fortran callers have ref ref pointers to addresses */
                        where = VA_ARG(*ap, int*);
                    if(*format != '[')
		    {
		        if(dp)
		        {
			    len = *((int*)*dp);
			    *dp += sizeof(int);
		        }
		        else
			    len = VA_ARG(*ap, int);
		    }
		    else
		    {
                        len = 0;
                        while(*++format != ']')
                            if(!isdigit(*format))
                            {
			        isis_errno = IE_BADFITEM;
			        return(-1);
                            }
			    else
                                len = len*10 + *format-'0';
                        ++format;
		    }
                }
                fi = &format_table[c-'a'];
                switch(fi->fi_size)
                {
                  case 0:
                    isis_errno = IE_BADFITEM;
                    return(-1);
                  default:
                    if(vector)
		    {
		        /* changed this to allow for null deallocators */
			vfunc *dealloc =
			  (by_ref ? VA_REF(*ap, vfunc*) : 0);
			len *= fi->fi_size;
			if(!by_ref || len < 64) 
			{
		            msg_insertfield(mp, fname, (char *) where, fi->fi_type, len);
			    /* Make sure that data deallocator is called */
			    if(dealloc)
				(*dealloc)((char *) where);
			}
		        else
		            msg_insertref(mp, fname, (char *) where, fi->fi_type, len, dealloc);
		    }
                    else switch(c)
                    {
                      case 'c':
                      case 'd':
                      case 'l':
                      case 'h': {
                        int is_int;
		        if(dp)
			{
			    is_int = *((int*)*dp);
			    *dp += sizeof(int);
			}
		        else
			    is_int = VA_ARG(*ap, int);
                        if(c == 'h')
                        {
                            short is_short = is_int;
                            msg_insertfield(mp, fname, (char *) &is_short, FTYPE_SHORT, sizeof(short));
                        }
                        else if (c == 'c')
                        {
                            char is_char = is_int;
                            msg_insertfield(mp, fname, &is_char, FTYPE_CHAR, sizeof(char));
                        }
                        else
                            msg_insertfield(mp, fname, (char *) &is_int, FTYPE_LONG, sizeof(long));
                        break;
                      }
                      default:
                        print("msg_put: %%%c x-by-value not supported (use %%%c[1] x-by-ref)\n", c, toupper(c));
                        isis_errno = IE_NOTIMP;
                        return(-1);
                    }
                    break;
                  case FMESSAGE:
                    if(vector)
                    {
                        isis_errno = IE_NOTIMP;
                        return(-1);
                    } 
		    if(dp)
		    {
		        msg_insertmsg(mp, fname, *((message**)*dp));
		        *dp += sizeof(message**);
		    }
		    else
		        msg_insertmsg(mp, fname, VA_ARG(*ap, message*));
                    break;
                  case FSTRING:
                    if(vector)
                    {
                        isis_errno = IE_NOTIMP;
                        return(-1);
                    } 
                    else
                    {
                        char *is_str;
		        if(dp)
		        {
			    is_str = *((char**)*dp);
			    *dp += sizeof(char **);
		        }
		        else
			    is_str = VA_REF(*ap, char*);
                        msg_insertfield(mp, fname, is_str, fi->fi_type, strlen(is_str)+1);
                    }
                    break;
                }
            }
	    else if(c == '{')
	    {
		char fcopy[128];
		char *datap;
		register char *fcp;
		register count = 1;
		fcp = fcopy;
		while(c = *format++)
		    if(c == '}' && --count == 0)
		        break;
		    else if((*fcp++ = c) == '{')
		        ++count;
		*fcp = 0;
		if(dp)
		{
		    datap = *((char**)*dp);
		    *dp += sizeof(char**);
		}
		else
		    datap = VA_REF(*ap, char*);
		if(*format == '[')
		{
		    count = 0;
		    while((c = *++format) && c != ']')
		    {
			count = count*10 + c-'0';
			++format;
		    }
		}
		else
		{
		    if(dp)
		    {
		        count = *((int*)*dp);
		        *dp += sizeof(int*);
		    }
		    else
		        count = VA_ARG(*ap, int);
		}
		while(count--)
		    _msg_doputf(mp, fcopy, fname, (va_list *)0, &datap);
	    }
        return(0);
  }

int
msg_get(va_alist)
  va_dcl
  {
        va_list ap;
        message *mp;
        int rval;
        va_start(ap);
	mp = VA_ARG(ap, message*);
        rval = _msg_dogetf(&mp, 1, SYSFLD_SCAN, &mp->msg_fpointer, &ap, (char*)0, (char**)0);
        va_end(ap);
        return(rval);
  }

int
msg_getfld(va_alist)
  va_dcl
  {
        va_list ap;
        message *mp;
        register fname, *msg_fpointer, rval;
        va_start(ap);
	mp = VA_ARG(ap, message*);
	fname = VA_ARG(ap, int);
	msg_fpointer = VA_REF(ap, int*);
        rval = _msg_dogetf(&mp, 1, fname, msg_fpointer, &ap, (char*)0, (char**)0);
        va_end(ap);
        return(rval);
  }

int
msg_dogetf(mlist, nmsgs, fname, fpt, argp)
  message **mlist;
  int nmsgs, fname;
  int *fpt;
  va_list *argp;
  {
	return _msg_dogetf(mlist, nmsgs, fname, fpt, argp, (char*)0, (char**)0);
  }

char    *isis_format;   /* Last format scanned */
int     isis_fitem;     /* Last format item scanned */

/*
 *      Ugly little routine to collect answers from a list of messages
 *      and put them into vectors of places for answers provided
 *      by the caller.  Called from the bcast routines too.
 */
int
_msg_dogetf(mlist, nmsgs, fname, fpt, argp, fmt, dp)
  message **mlist;
  int nmsgs, fname;
  int *fpt;
  va_list *argp;
  char *fmt, **dp;
  {
        register c;
        int getf_count = 0;
        va_list acopy;
	if(argp)
	    acopy = *argp;
        isis_errno = 0;
        while(nmsgs--)
        {
            register message *mp = *mlist++;
            register char *format;
            register *msg_fpointer = fpt;
            int dummy_ptr = 1;
            if(msg_fpointer == 0)
                msg_fpointer = &dummy_ptr;
	    else if(*msg_fpointer == 0)
		++*msg_fpointer;
            /* Will need to rescan the argument list for 0..nmsgs-1 */
            getf_count = 0;
            if(argp)
	    {
                *argp = acopy;
                isis_format = format = VA_REF(*argp, char*);
	    }
	    else
		isis_format = format = fmt;
            while(c = *format++) if(c == '%')
            {
                int vector = 0, do_malloc = 0, do_copy = 1, len, vlen;
                register format_item *fi;
                register char **ptr;
                c = *format++;
                if(c == '+')
                {
                    do_malloc = 1;
                    c = *format++;
                }
                else if(c == '-')
                {
                    do_copy = 0;
                    c = *format++;
                }
                if(!isascii(c))
                {
                    isis_errno = IE_BADFITEM;
                    return(-1);
                }
                if(isupper(c))
                {
                    ++vector;
                    c = tolower(c);
                    vlen = -1;
                    if(*format == '[')
                    {
                        vlen = 0;
                        while(*++format != ']')
                            if(!isdigit(*format))
                            {
			        isis_errno = IE_BADFITEM;
			        return(-1);
                            }
			    else
                                vlen = vlen*10 + *format-'0';
                        ++format;
                    }
                }
                fi = &format_table[c-'a'];
                if(msg_gettype(mp, fname, *msg_fpointer) != fi->fi_type)
                {
                    if(msg_getfield(mp, fname, *msg_fpointer, (int*)0) == 0)
                        break;
                    isis_errno = IE_MISSMATCH;
                    return(-1);
                }
                ++getf_count;
                if(argp)
                    ptr = &VA_REF(*argp, char*);
		else
		    ptr = dp;
                switch(fi->fi_size)
                {
                    register char *source;
                  case 0:
                    isis_errno = IE_BADFITEM;
                    return(-1);
                  default:
                  case FSTRING:
                    /* Take advantage of fact that we just did a msg_gettype */
		    begin
		    {
			register msg_fld *fp;
		        fp = mp->msg_lfield;
                        if(fp->fld_type != FTYPE_FIELDREF)
                        {
                            len = fp->fld_len;
                            source = (caddr_t)(&fp[1]);
                        }
                        else
                        {   
			    len = is_field_ref(fp)->fr_len;
                            source = (caddr_t)is_field_ref(fp)->fr_ptr;
                        }
		    }
                    ++*msg_fpointer;
                    if(do_malloc)
                    {
                        register char *dest = len? malloc(len): (char*)0;
			*(*(char***)ptr)++ = dest;
			if(len)
			    bcopy(source, dest, len);
                    }
                    else if(do_copy)
                    {
                        if((isis_state&ISIS_XBYREF) && (c=='a'||c=='b'||c=='p'||c=='e'))
			     panic("msg_get: %%%c not implemented from fortran (use %%-%c)", c, c);
			if(len)
			    bcopy(source, *ptr, len);
			(*ptr) += len;
                    }
                    else
			*(*(char***)ptr)++ = len? source: (char*)0;
		    len /= fi->fi_size;
		    if(vector && argp)
		    {
                        if(vlen == -1)
                        {
                            int **lptr;
		            lptr = &VA_REF(*argp, int*);
                            if(*lptr)
                                *(*lptr)++ = len;
                        }
                        else if(vlen != len)
		        {
                            isis_errno = IE_WRONGLEN;
                            return(-1);
		        }
		    }
		    else if(vector)
		    {
		        isis_errno = IE_BADARG;
		        return(-1);
		    }
                    break;
                  case FMESSAGE:
                    if(vector)
                    {
                        isis_errno = IE_NOTIMP;
                        return(-1);
                    } 
		    *(*(message***)ptr)++ = msg_getmsg(mp, fname, (*msg_fpointer)++);
                    break;
                }
		dp = ptr;
            }
	    else if(c == '{')
            {
                char fcopy[128];
                char *datap;
                register char *fcp;
                register count = 1;
                fcp = fcopy;
                while(c = *format++)
                    if(c == '}' && --count == 0)
                        break;
                    else if((*fcp++ = c) == '{')
                        ++count;
                *fcp = 0;
		if(dp)
		   datap = *((char**)*argp++);
		else
		   datap = VA_REF(*argp, char*);
                if(*format == '[')
                {
                    count = 0;
                    while((c = *++format) && c != ']')
                    {
                        count = count*10 + c-'0';
                        ++format;
                    }
                }
                else
		{
		    if(dp)
		       count = *((int*)*argp++);
		    else
		       count = VA_ARG(*argp, int);
		}
                while(count--)
		{
		    register rval;
		    message *msg = mp;
		    rval = _msg_dogetf(&msg, 1, fname, fpt, (va_list *)0, fcopy, &datap);
		    if(rval == -1)
			return(-1);
		    getf_count += rval;
		}
            }

        }
        return(getf_count);
  }

int
msg_rewind(mp)
  register message *mp;
  {
        mp->msg_fpointer = 1;
        return(0); /* Never fails! */
  }

int
isis_do_define_type(formatletter, size, converter)
  int formatletter;
  int size;
  vfunc *converter;
  {
        register char *string = (char *) malloc(32);
        register format_item *fi;
        fi = &format_table[formatletter-'a'];
        if(formatletter < 'a' || formatletter > 'z' || fi->fi_size)
            panic("isis_define_type %%%c: illegal or redefined formatletter", formatletter);
        sprintf(string, "user-defined type %%%c", formatletter);
        msg_definetype (formatletter, size, converter, string);
        fi->fi_size = size;
        fi->fi_type = formatletter;
        return(0);
  }
