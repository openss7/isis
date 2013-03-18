/*  $RCSfile: cl_token.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:42 $  */
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

/*  Token tool interface routines. */

#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	address *t_holder(address * gaddr, char *name);
	int t_pass(address * gaddr, char *name);
	int t_request(address * gaddr, char *name, int pass_on_fail);
#ifdef __cplusplus
}
#endif
#endif
/*** Internal routines ***/
#if FUN_TYPES
#ifdef __cplusplus
extern "C" {
#endif
	void dump_tokens();
	token *map_token(address * gaddr, char *name);
	void tk_init();
#ifdef __cplusplus
}
#endif
#endif
