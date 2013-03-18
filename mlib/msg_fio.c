/*  $RCSfile: msg_fio.c,v $ $Revision: 2.0 $ $Date: 90/05/04 15:12:40 $  */
/*
 *	Originally coded by Ken Birman
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
 */
#define   ISIS_SYS
# include "isis.h"

#ifndef  SIMWRITEV

#       define MAXIOVECLEN     16

#else   SIMWRITEV

#       define MAXIOVECLEN     1

#define min(a,b)               (a<b? a: b)

static writev(sock, iovp, iovl)
  register sock;
  register iovec *iovp;
  {
        if(iovl != 1)
            panic("writev iovl != 1!\n");
        return(write(sock, iovp->iov_base, min(iovp->iov_len, 4096)));
  }
#endif  SIMWRITEV

extern  errno;

/* Algorithm for writing a message to a FILE */
int
msg_fwrite(file, mp)
  FILE *file;
  register message *mp;
  {
        register nb, len = 0, iovlen;
        register struct iovec *iovp;
        register iov_len;
        register char *iov_base;
        t_scheck();
        iovlen = msg_getiovlen(mp);
        iovp = msg_getiovec(mp);
        iov_len = iovp->iov_len;
        iov_base = iovp->iov_base;
        while(iovlen)
        {
            while((nb = fwrite(iov_base, 1, iov_len, file)) == -1)
            {
                if(errno == EINTR)
                    continue;
                return(-1);
            }
            len += nb;
            while(nb)
            {
                if(iov_len <= nb)
                {
                    nb -= iov_len;
                    --iovlen;
                    ++iovp;
                    iov_len = iovp->iov_len;
                    iov_base = iovp->iov_base;
                }
                else
                {
                    iov_base += nb;
                    iov_len -= nb;
                    nb = 0;
                }
            }
        }
        if(len != msg_getlen(mp))
	{
	    msg_printaccess(mp);
            panic("algorithmic error in msg_fwrite (wrote %d expected %d)", len, msg_getlen(mp));
	}
        return(0);
  }

message *
msg_fread(file)
  FILE *file;
  {
        register message *mp;
        register long *space;
        register char *ptr;
        int length = 0;
	register block_desc *bp;
        register nb;

        while((nb = fread((char*)&length, 1, sizeof(long), file)) != sizeof(long))
        {
            if(nb == -1 && errno == EINTR)
                continue;
            return(0);
        }
        length = ntohl (length);
	if((bp = msg_malloc(length)) == (block_desc*)0)
	{
	    print("WARNING: closing file descriptor %d\n", fileno(file));
	    fclose(file);
	    return(0);
	}
        space = (long*)msg_body(bp);
        space[0] = htonl (length);
        ptr = (char*)&space[1];
        length -= sizeof(long);
	bp->blk_avail -= sizeof(long);
        while(length)
        {
            while((nb = fread(ptr, 1, length, file)) <= 0)
            {
                if(nb == -1 && errno == EINTR)
                    continue;
                return(0);
            }
	    bp->blk_avail -= nb;
            length -= nb;
            ptr += nb;
        }
        if(mp = msg_reconstruct(bp))
            return(mp);
        return(0);
  }

int
msg_write(sock, mp)
  int sock;
  register message *mp;
  {
        register nb, len = 0, iovlen;
        register struct iovec *iovp, *iovb;
        t_scheck();
        iovlen = msg_getiovlen(mp);
        iovb = iovp = (struct iovec*)malloc(nb = sizeof(struct iovec)*iovlen);
        bcopy(msg_getiovec(mp), iovp, nb);
        while(iovlen)
        {
            register iovl = iovlen;
            if(iovl > MAXIOVECLEN)
                iovl = MAXIOVECLEN;
            while((nb = writev(sock, iovp, iovl)) == -1)
            {
                if(errno == EINTR)
                    continue;
                if(sock == isis_socket)
                {
                    perror("isis_write");
                    isis_has_crashed(-1);
                }
                free(iovb);
                return(-1);
            }
            len += nb;
            while(nb)
            {
                if(iovp->iov_len <= nb)
                {
                    nb -= iovp->iov_len;
                    --iovlen;
                    ++iovp;
                }
                else
                {
                    iovp->iov_base += nb;
                    iovp->iov_len -= nb;
                    nb = 0;
                }
            }
        }
        if(len != msg_getlen(mp))
	{
	    msg_printaccess(mp);
            panic("algorithmic error in msg_write (wrote %d expected %d)", len, msg_getlen(mp));
	}
        free(iovb);
        return(0);
  }

message *
msg_read(sock)
  int sock;
  {
        register message *mp;
        register long *space;
        register char *ptr;
	register block_desc *bp;
        int length = 0;
        register nb;
        static ntries;

        while((nb = read(sock, (char*)&length, sizeof(long))) != sizeof(long))
        {
            if(nb == -1 && errno == EINTR)
                continue;
            if(sock == isis_socket && (nb < 0 || ntries++ == 10))
                isis_has_crashed(-1);
            return(0);
        }
        ntries = 0;
        length = ntohl (length);
	if((bp = msg_malloc(length)) == (block_desc*)0)
	{
	    print("... WARNING: closing file descriptor %d\n", sock);
	    return(0);
	}
        space = (long*)msg_body(bp);
        space[0] = htonl (length);
        ptr = (char*)&space[1];
        length -= sizeof(long);
	bp->blk_avail -= sizeof(long);
        while(length)
        {
            while((nb = read(sock, ptr, length)) <= 0)
            {
                if(nb == -1 && errno == EINTR)
                    continue;
                if(sock == isis_socket)
                {
                    if(nb == 0 && ntries++ < 10) 
                    {    
                        sleep(1);
                        continue; 
                    }
                    isis_has_crashed(-1);
                }
                return(0);
            }
	    bp->blk_avail -= nb;
            length -= nb;
            ptr += nb;
        }
        if(mp = msg_reconstruct(bp))
            return(mp);
        if(sock == isis_socket)
            isis_has_crashed(-1);
        return(0);
  }
