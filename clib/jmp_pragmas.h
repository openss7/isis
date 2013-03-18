/*  $RCSfile: jmp_pragmas.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:22:30 $  */
/* Pragma's needed for setjmp/longjmp on SUN4 only */

extern	isis_setjmp(), isis_longjmp();

#pragma	unknown_control_flow(isis_setjmp, isis_longjmp)
#pragma makes_regs_inconsistent(isis_setjmp, isis_longjmp)

