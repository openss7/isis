/*  $RCSfile: pr_setjmp.s,v $ $Revision: 2.14 $ $Date: 90/08/08 11:15:57 $  */
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
 *
 *      This is a modified version of _setjmp.s for use in ISIS
 *      It lacks most of the "botch" testing, which fails in ISIS
 *      due to non-standard stack frame allocation scheme.  The dummy 
 *      file pr_setjmp.c is used instead of this on machines where
 *      a native task package can be used, or where the native setjmp/longjmp
 *      works well enough for ISIS to use it directly.
 *
 *      Setjmp: store frame into jmp buffer (can overrun normal size buf. a little)
 *      Longjmp: restore frame and then return to the
 *      caller who called 'setjmp' in the first place
 *
 *      Unlike the standard versions, setjmp may assume that the
 *      caller will have saved all registers that might need to be saved.
 *      This is important because if these registers are not saved,
 *      the caller could see his register variables get trashed.
 */
#ifdef  VAX
        .text
        .globl  _isis_setjmp
        .align  2
_isis_setjmp:
        .word   0
        movl    4(ap),r0
        movl    r2,(r0)+
        movl    r3,(r0)+
        movl    r4,(r0)+
        movl    r5,(r0)+
        movl    r6,(r0)+
        movl    r7,(r0)+
        movl    r8,(r0)+
        movl    r9,(r0)+
        movl    r10,(r0)+
        movl    r11,(r0)+
        movl    12(fp),(r0)+            # save frame pointer of caller
        movl    16(fp),(r0)+            # save pc of caller
        clrl    r0
        ret

        .globl  _isis_longjmp
        .align  2
_isis_longjmp:
        .word   0
        movl    8(ap),r0                # return(v)
        movl    4(ap),r1                # fetch buffer
        movl    (r1)+,r2
        movl    (r1)+,r3
        movl    (r1)+,r4
        movl    (r1)+,r5
        movl    (r1)+,r6
        movl    (r1)+,r7
        movl    (r1)+,r8
        movl    (r1)+,r9
        movl    (r1)+,r10
        movl    (r1)+,r11
        movl    (r1)+,fp
        jmp     *(r1)
#endif  VAX

#ifdef  AUX
#define PARAM0          0x0(%sp)
#define PARAM           0x4(%sp)
#define PARAM2          0x8(%sp)
-
        global isis_setjmp
        global isis_longjmp
-
isis_setjmp:
        mov.l    PARAM,%a0         /* pointer to jmp_buf */
        mov.l    PARAM0,(%a0)      /* pc */
        clr.l    0x4(%a0)
        clr.l    0x8(%a0)
        movem.l  &0xFCFC,0xC(%a0) /* d2-d7, a2-a7 */
        clr.l    %d0              /* return 0 */
        rts
-
isis_longjmp:
        mov.l    PARAM,%a0        /* pointer to jmp_buf */
        mov.l    PARAM2,%d0       /* value returned */
        bne     isis%
        mov.l   &0x1,%d0
isis%:
        movem.l  0xC(%a0),&0xFCFC /* restore d2-d7, a2-a7 */
        mov.l    (%a0),(%sp)      /* restore pc of call to setjmp to stack */
        rts
#endif  AUX

#ifdef  SUN3
#define PARAMX( n )     sp@(4+n)
#define PARAM0          PARAMX(-4)
#define PARAM           PARAMX(0)
#define PARAM2          PARAMX(4)

.globl  _isis_setjmp; _isis_setjmp:
        movl    PARAM,a0        /* pointer to jmp_buf */
        movl    PARAM0,a0@      /* pc */
        clrl    a0@(4)
        clrl    a0@(8)
        moveml  #0xFCFC,a0@(12) /* d2-d7, a2-a7 */
        clrl    d0              /* return 0 */
        rts

.globl  _isis_longjmp; _isis_longjmp:
        movl    PARAM,a0        /* pointer to jmp_buf */
        movl    PARAM2,d0       /* value returned */
        bne     1$
        moveq   #1,d0
1$:
        moveml  a0@(12),#0xFCFC /* restore d2-d7, a2-a7 */
        movl    a0@,sp@         /* restore pc of call to setjmp to stack */
        rts
#endif

#ifdef  SUN4
        .text
        .seg    "text"

#include <sun4/asm_linkage.h>
#include <sun4/trap.h>

        PCVAL   =       0       ! offsets in buf structure
        SPVAL   =       4

        ENTRY(isis_setjmp)
        st      %o7, [%o0 + PCVAL]      ! note return pc
        st      %sp, [%o0 + SPVAL]      ! ... and callers sp
        retl
        clr     %o0                     ! return zero

        ENTRY(isis_longjmp)
        t       ST_FLUSH_WINDOWS        ! first flush all reg windows to the stack.
        ! now set up to "fault-in" old registers from prior context
        sub     %sp, WINDOWSIZE, %sp    ! establish new save area (paranoid)
        ld      [%o0 + PCVAL], %o7      ! return pc
        ld      [%o0 + SPVAL], %fp      ! recover new stack frame
	! now looks like we entered via "save", so can do restore...
        retl
        restore %o1, 0, %o0             ! return (val), pop old registers
#endif

#ifdef  HPUX
#ifdef hp9000s300
        global   _isis_setjmp,_isis_longjmp
        text    
                
_isis_setjmp:
        mov.l   4(%a7),%a0
        mov.l   %d0,0x34(%a0)
        mov.l   (%a7),(%a0)
        movm.l  %d2-%d7/%a0-%a7,4(%a0)
        clr.l   %d0
        rts     
                
_isis_longjmp:
        mov.l   4(%a7),%a0
        mov.l   8(%a7),%d0
        movm.l  4(%a0),%d2-%d7/%a0-%a7
        mov.l   (%a0),(%a7)
        tst.l   %d0
        bne     XX
        movq    &1,%d0
XX:     rts     
#endif hp9000s300
#ifdef hp9000s800

        .CODE
        .EXPORT get_sp
        .EXPORT set_sp

get_sp
        .PROC
        .CALLINFO 

        .ENTER
        BV      0(%rp)
        COPY    %sp,%ret0
        .LEAVE
        .PROCEND


set_sp
        .PROC
        .CALLINFO 

        .ENTER
        BV      0(%rp)
        COPY    %arg0,%sp
        .LEAVE
        .PROCEND

        .END

#endif hp9000s800
#endif

#ifdef  AIXRS
        .set stk,1                      
        .set stkmin, 56                 
        .set stkp9, stkmin              
        .set stkp8, 52
        .set stkp1, 24
        .set stktoc, 20                 
        .set stkret, 16                 
        .set stklink, 8                 
        .set stkcond, 4                 
        .set stkback, 0                 
        .set stkfpr31, -8               
        .set stkfpr30, -16              
        .set stkfpr29, -24              
        .set stkfpr28, -32              
        .set stkfpr27, -40              
        .set stkfpr26, -48              
        .set stkfpr25, -56              
        .set stkfpr24, -64              
        .set stkfpr23, -72              
        .set stkfpr22, -80              
        .set stkfpr21, -88              
        .set stkfpr20, -96              
        .set stkfpr19, -104             
        .set stkfpr18, -112             
        .set stkfpr17, -120             
        .set stkfpr16, -128             
        .set stkfpr15, -136             
        .set stkfpr14, -144             
        .set stkr31, -4                 
        .set stkr13, -76                
        .set stkr12, stkr13-4           
        .set stkr0, -128                
        .set stkr1, -124                
        .set stkr2, -120                
        .set stkr3, -116                
        .set stkpush, stkr13-4          
        .set stkpop, 0-stkpush          
        .set exstkr0,0                  
        .set extstkr1,4                 
        .set extstkr31,124              
        .set exstklr,128                
        .set exstkenv,132               
        .set exerrno,136                
	.set	jmpmask, 0	
	.set	jmpmask1, 4	
	.set	jmpret, 8	
	.set	jmpstk, 12	
	.set	jmptoc, 16	
	.set	jmpregs, 20	
	.set	jmpcr,96	
	.set	jmprsv1,100	
	.set	jmpfpr,104	
	.set	jmprsv2,248	
	.set	jmpbufsize,256	
	.extern .sigprocmask
	.csect .setjmp[PR]
	 .globl .setjmp[PR]
	mfcr	4 			
	st	4, jmpcr(3)
	mflr	4			
	st	1, jmpstk(3)		
	st	2, jmptoc(3)		
	st	4, jmpret(3)		
	stm	13, jmpregs(3)	
	bl	.jmpsavefpr[PR]		
	mr	13, 3			
	stu	1, -stkmin(1)		
	mr	5, 3			
	cal	3, 0(0)		
	cal	4, 0(0)		
	bl	.sigprocmask		
	cror	15, 15, 15
	cal	1, stkmin(1)		
	cal	3, 0(0)		
	l	6, jmpret(13)		
	mtlr	6			
	l	13, jmpregs(13)	
	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
	.toc
        .csect  setjmp[DS]
        .globl  setjmp[DS]
       .long   .setjmp[PR]
        .long   TOC[t0]
	.extern	.sigcleanup
	.extern	.longjmperror
	.extern	.abort
	.csect .longjmp[PR]
	 .globl .longjmp[PR]
	l	13, jmpstk(3)		
	cmp	0, 1, 13		
	ble	0, long_skip		
	stu	1, -stkmin(1)		
	bl	.longjmperror		
	cror	15, 15, 15
	bl	.abort			
	cror	15, 15, 15
	cal	1, stkmin(1)		
long_skip:
	mr	13, 3			
	mr	14, 4			
	stu	1, -stkmin(1)		
	bl	.sigcleanup		
	cror	15, 15, 15
	cal	1, stkmin(1)		
	mr	3, 13			
	mr	4, 14			
	l       5, jmpret(3)		
	l       1, jmpstk(3)		
	l       2, jmptoc(3)		
	bl	.jmprestfpr[PR]		
	cmpi	0, 4, 0		
	mtlr	5			
	lm      13, jmpregs(3)	
	l	5, jmpcr(3)		
	mtcrf	0x38, 5
	mr	3, 4			
	bne	0, jmp1		
	cal	3, 1(0)		
jmp1:
	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
	.toc
        .csect  longjmp[DS]
        .globl  longjmp[DS]
       .long   .longjmp[PR]
        .long   TOC[t0]
	.extern .sigprocmask
	.csect .sigsetjmp[PR]
	 .globl .sigsetjmp[PR]
	mfcr	5 			
	st	5, jmpcr(3)
	cmpi	0, 4, 0		
	mflr	5			
	st	1, jmpstk(3)		
	st	2, jmptoc(3)		
	st	5, jmpret(3)		
	stm	13, jmpregs(3)	
	bl	.jmpsavefpr[PR]		
	mr	13, 3			
	mr	5, 3			
	cal	3, -1(0)		
	st	3, jmpmask(5)		
	st	3, jmpmask1(5)
	cal	3, 0(0)		
	cal	4, 0(0)		
	beq	ss_skipit		
	stu	1, -stkmin(1)		
	bl	.sigprocmask		
	cror	15, 15, 15
	cal	1, stkmin(1)		
ss_skipit:
	cal	3, 0(0)		
	l	6, jmpret(13)		
	mtlr	6			
	l	13, jmpregs(13)	
	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
	.toc
        .csect  sigsetjmp[DS]
        .globl  sigsetjmp[DS]
       .long   .sigsetjmp[PR]
        .long   TOC[t0]
	.extern	.sigcleanup
	.extern	.longjmperror
	.extern	.abort
	.csect .siglongjmp[PR]
	 .globl .siglongjmp[PR]
	l	13, jmpstk(3)		
	cmp	0, 1, 13		
	ble	0, sl_long_skip	
	stu	1, -stkmin(1)		
	bl	.longjmperror		
	cror	15, 15, 15
	bl	.abort			
	cror	15, 15, 15
	cal	1, stkmin(1)		
sl_long_skip:
	mr	13, 3			
	mr	14, 4			
	l	5, jmpmask(3)		
	l	15, jmpmask1(3)	
	cmpi	0, 5, -1		
	cmpi	1, 15, -1		
	crand	0*4+0x02, 0*4+0x02, 1*4+0x02
	beq	0, sl_skipit	    	
	stu	1, -stkmin(1)		
	bl	.sigcleanup		
	cror	15, 15, 15
	cal	1, stkmin(1)		
	mr	3, 13			
	mr	4, 14			
sl_skipit:
	l       5, jmpret(3)		
	l       1, jmpstk(3)		
	l       2, jmptoc(3)		
	bl	.jmprestfpr[PR]		
	cmpi	0, 4, 0		
	mtlr	5			
	lm      13, jmpregs(3)	
	l	5, jmpcr(3)		
	mtcrf	0x38, 5
	mr	3, 4			
	bne	0, sigjmp1		
	cal	3, 1(0)		
sigjmp1:
	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
	.toc
        .csect  siglongjmp[DS]
        .globl  siglongjmp[DS]
       .long   .siglongjmp[PR]
        .long   TOC[t0]
	.csect ._setjmp[PR]
	 .globl ._setjmp[PR]
	mfcr	4 			
	st	4, jmpcr(3)
	mflr	4			
	st	1, jmpstk(3)		
	st	2, jmptoc(3)		
	st	4, jmpret(3)		
	stm	13, jmpregs(3)	
	bl	.jmpsavefpr[PR]		
	mtlr	4			
	cal	3, 0(0)		
	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
	.extern	.longjmperror
	.extern	.abort
	.csect ._longjmp[PR]
	 .globl ._longjmp[PR]
	l	13, jmpstk(3)		
	l       5, jmpret(3)		
	l       1, jmpstk(3)		
	l       2, jmptoc(3)		
	bl	.jmprestfpr[PR]		
	cmpi	0, 4, 0		
	mtlr	5			
	lm      13, jmpregs(3)	
	l	5, jmpcr(3)		
	mtcrf	0x38, 5
	mr	3, 4			
	bne	0, _jmp1		
	cal	3, 1(0)		
_jmp1:
	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
	.csect .jmpsavefpr[PR]
	 .globl .jmpsavefpr[PR]
	stfd	14, jmpfpr (3)
	stfd	15, jmpfpr +  1 * 8 (3)
	stfd	16, jmpfpr +  2 * 8 (3)
	stfd	17, jmpfpr +  3 * 8 (3)
	stfd	18, jmpfpr +  4 * 8 (3)
	stfd	19, jmpfpr +  5 * 8 (3)
	stfd	20, jmpfpr +  6 * 8 (3)
	stfd	21, jmpfpr +  7 * 8 (3)
	stfd	22, jmpfpr +  8 * 8 (3)
	stfd	23, jmpfpr +  9 * 8 (3)
	stfd	24, jmpfpr + 10 * 8 (3)
	stfd	25, jmpfpr + 11 * 8 (3)
	stfd	26, jmpfpr + 12 * 8 (3)
	stfd	27, jmpfpr + 13 * 8 (3)
	stfd	28, jmpfpr + 14 * 8 (3)
	stfd	29, jmpfpr + 15 * 8 (3)
	stfd	30, jmpfpr + 16 * 8 (3)
	stfd	31, jmpfpr + 17 * 8 (3)
	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
	.csect .jmprestfpr[PR]
	 .globl .jmprestfpr[PR]
	lfd	14, jmpfpr (3)
	lfd	15, jmpfpr +  1 * 8 (3)
	lfd	16, jmpfpr +  2 * 8 (3)
	lfd	17, jmpfpr +  3 * 8 (3)
	lfd	18, jmpfpr +  4 * 8 (3)
	lfd	19, jmpfpr +  5 * 8 (3)
	lfd	20, jmpfpr +  6 * 8 (3)
	lfd	21, jmpfpr +  7 * 8 (3)
	lfd	22, jmpfpr +  8 * 8 (3)
	lfd	23, jmpfpr +  9 * 8 (3)
	lfd	24, jmpfpr + 10 * 8 (3)
	lfd	25, jmpfpr + 11 * 8 (3)
	lfd	26, jmpfpr + 12 * 8 (3)
	lfd	27, jmpfpr + 13 * 8 (3)
	lfd	28, jmpfpr + 14 * 8 (3)
	lfd	29, jmpfpr + 15 * 8 (3)
	lfd	30, jmpfpr + 16 * 8 (3)
	lfd	31, jmpfpr + 17 * 8 (3)
	br
        .align 2
	.tbtag 0x0,0xc,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0
#endif  AIXRS
