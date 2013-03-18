/*  $RCSfile: unix_lib.h,v $ $Revision: 2.52 $ $Date: 90/08/06 13:54:33 $  */
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
/* ANSI C prototypes for Unix library calls that Isis uses. */
/* Note that some of these declarations are not entirely standard
   but rather declare the types that Isis actually uses. For
   instance "select" is declared differently from the manual page
   synopsis.
*/

/* Only do this for systems that we know don't already have 
   function declarations in the system header files.
*/
#if (SUN3 || SUN4 || AUX)
/* Kind of a waste to include these system headers all the time. */
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/time.h>

#ifdef FUN_TYPES

extern int	atoi(char *str);
extern long	atol(char *str);
#ifdef __GNUC__
extern volatile void	abort(void);
#else
extern void	abort(void);
#endif
extern int	accept(int s, struct sockaddr *addr, int *addrlen);
extern void	alarm(unsigned int seconds);
extern int	bind(int s, struct sockaddr *name, int namelen);
extern void	bcopy(VOID *from, VOID *to, int n_bytes);
                /* Isis defines VOID as char on some compiler that give
                   compile errors on void * in assignments. */
extern void	bzero(VOID *bytes, int n_bytes);
extern VOID    *calloc(unsigned int nelem, unsigned elsize);
extern int	chmod(char *path, int mode);
extern int	close(int des);
extern int	connect(int s, struct sockaddr *name, int namelen);
extern int	fclose(FILE *stream);
extern int	fflush(FILE *stream);
extern int	fprintf(FILE *stream, char *format ...);
extern int	fputs(char *s, FILE *stream);
extern int	fread(VOID *ptr, int size, int nitems, FILE *stream);
extern int	fscanf(FILE *stream, char *format ...);
extern int	ftruncate(int fd, off_t length);
extern int	fwrite(VOID *ptr, int size, int nitems, FILE *stream);
extern int 	gethostname(char *name, int namelen);
extern int	getpid(void);
extern int	getppid(void);
extern void	free(VOID *ptr);
extern int	fseek(FILE *stream, long offset, int ptrname);
extern int	fsync(int fd);
extern long	ftell(FILE *stream);
extern int	execve(char *name, char *argv[], char **envp);
extern int	execvp(char *name, char *argv[]);
extern int	getsockname(int s, struct sockaddr *name, int *namelen);
extern int	gettimeofday(struct timeval *tp, struct timezone *tzp);
extern int	listen(int s, int backlog);
extern off_t	lseek(int des, off_t offset, int whence);
extern VOID    *malloc(unsigned size);
extern int 	mkdir(char *path, int mode);
extern int	mkstemp(char *name_template);
extern char    *mktemp(char *name_template);
extern int	open(char *path, int flags, int mode);
#if ( __cplusplus || c_plusplus )
extern void	perror(const char *);
#else
extern void	perror(char *s);
#endif
extern int	printf(char *format ...);
extern int	puts(char *s);
extern int	read(int d, VOID *buf, int nbytes);
extern VOID    *realloc(VOID *ptr, unsigned size);
extern int	recv(int s, VOID *buf, int len, int flags);
extern int	recvfrom(int s, VOID *buf, int len, int flags,
                         struct sockaddr *from, int *fromlen);
extern int	rename(char *from, char *to);
extern void	rewind(FILE *stream);
extern int	scanf(char *format ...);
extern int	select(int width, int *readfds, int *writefds, 
                       int *exceptfds, struct timeval *timeout);
extern int	send(int s, VOID *msg, int len, int flags);
extern int	sendto(int s, VOID *msg, int len, int flags,
                       struct sockaddr *to, int tolen);
extern int	sigsetmask(int mask);
extern long	strtol(char *str, char **ptr, int base);
extern int	socket(int domain, int type, int protocol);
#if ( __cplusplus || c_plusplus )
extern char    *sprintf(char*, const char* ...);
#else
extern char    *sprintf(char *s, char *format ...);
#endif
extern int	sscanf(char *s, char *format ...);
extern int	strlen(char *string);
extern char    *strcat(char *s1, char *s2);
extern char    *strcpy(char *to, char *from);
extern char    *strncpy(char *to, char *from, int n_bytes);
extern int	strcmp(char *s1, char *s2);
extern int	truncate(char *path, off_t length);
extern int 	umask(int new_mask); 
extern int	unlink(char *path);
extern int	write(int d, char *buf, int nbytes);

#else

extern int	atoi();
extern long	atol();
extern void	abort();
extern int	accept();
extern void	alarm();
extern int	bind();
extern void	bcopy();
extern void	bzero();
extern VOID    *calloc();
extern int	chmod();
extern int	close();
extern int	connect();
extern int	fclose();
extern int	fflush();
extern int	fprintf();
extern int	fputs();
extern int	fread();
extern int	fscanf();
extern int	ftruncate();
extern int	fwrite();
extern int 	gethostname();
extern int	getpid();
extern int	getppid();
extern void	free();
extern int	fseek();
extern int	fsync();
extern long	ftell();
extern int	execve();
extern int	execvp();
extern int	getsockname();
extern int	gettimeofday();
extern int	listen();
extern off_t	lseek();
extern int 	mkdir();
extern int	mkstemp();
extern char    *mktemp();
extern int	open();
extern void	perror();
extern int	printf();
extern int	puts();
extern int	read();
extern VOID    *realloc();
extern int	recv();
extern int	recvfrom();
extern int	rename();
extern void	rewind();
extern int	scanf();
extern int	select();
extern int	send();
extern int	sendto();
extern int	sigsetmask();
extern long	strtol();
extern int	socket();
extern char    *sprintf();
extern int	sscanf();
extern int	strlen();
extern char    *strcat();
extern char    *strcpy();
extern char    *strncpy();
extern int	strcmp();
extern int	truncate();
extern int 	umask();
extern int	unlink();
extern int	write();

#endif
#endif
