/*  $RCSfile: tk_connect.h,v $ $Revision: 2.104 $ $Date: 90/09/12 13:26:01 $  */

#if FUN_TYPES
vfunc *isis_connect_register(vfunc *fun);
void  isis_connect_init();
int   isis_connect(address *who);
#else
#ifdef CONVEX
void * isis_connect_register();
#else
vfunc *isis_connect_register();
#endif
void  isis_connect_init();
int   isis_connect();
#endif
