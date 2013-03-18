/*  $RCSfile: bits.h,v $ $Revision: 2.0 $ $Date: 90/05/04 15:21:34 $  */
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
 *
 *
 */

#ifndef MAXBITS
#    define MAXBITS 128
#endif

#define ISIS_BS      32
#define ISIS_BO      31
#define ISIS_BSHIFT  5
#define ISIS_BVL     4

struct  bitvec
{
        long    bv_data[ISIS_BVL];
};

#  define bis(vec,b)      ((vec)->bv_data[(b)>>ISIS_BSHIFT] |= 1<<((b) & ISIS_BO))
#  define bic(vec,b)      ((vec)->bv_data[(b)>>ISIS_BSHIFT] &= ~(1<<((b) & ISIS_BO)))
#  define bit(vec,b)      ((vec)->bv_data[(b)>>ISIS_BSHIFT] & (1<<((b) & ISIS_BO)))
#  define bisv(vec,bv)    {                                             \
            (vec)->bv_data[0] |= (bv)->bv_data[0];                      \
            (vec)->bv_data[1] |= (bv)->bv_data[1];                      \
            (vec)->bv_data[2] |= (bv)->bv_data[2];                      \
            (vec)->bv_data[3] |= (bv)->bv_data[3];                      \
}
#  define bandv(vec,bv)   {                                             \
            (vec)->bv_data[0] &= (bv)->bv_data[0];                      \
            (vec)->bv_data[1] &= (bv)->bv_data[1];                      \
            (vec)->bv_data[2] &= (bv)->bv_data[2];                      \
            (vec)->bv_data[3] &= (bv)->bv_data[3];                      \
}
#  define bicv(vec,bv)    {                                             \
            (vec)->bv_data[0] &= ~(bv)->bv_data[0];                     \
            (vec)->bv_data[1] &= ~(bv)->bv_data[1];                     \
            (vec)->bv_data[2] &= ~(bv)->bv_data[2];                     \
            (vec)->bv_data[3] &= ~(bv)->bv_data[3];                     \
}

#  define btst(vec)                                                     \
	((vec)->bv_data[0] || (vec)->bv_data[1] || (vec)->bv_data[2] || (vec)->bv_data[3])

#  define bitv(vec,bv)    (                                             \
           ((vec)->bv_data[0] & (bv)->bv_data[0]) ||                    \
           ((vec)->bv_data[1] & (bv)->bv_data[1]) ||                    \
           ((vec)->bv_data[2] & (bv)->bv_data[2]) ||                    \
           ((vec)->bv_data[3] & (bv)->bv_data[3])                       \
)

#  define bclr(vec)       { bzero((char*)vec,sizeof(bitvec)); }
#  define bset(vec)       bisv((bitvec*)vec,(bitvec*)&ones)
