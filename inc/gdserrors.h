/*  gdserrors.h
                              COPYRIGHT (c) 1994
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands
#>gdserrors.dc3

Header:    gdserrors.h

Purpose:   Define GDS error codes (both server and client)

File:      gdserrors.h

Author:    J.P. Terlouw

Updates:   Mar  3, 1994: JPT, Document created.
#<
*/

#if !defined(_gdserrors_h_)
#define _gdserrors_h_

/* ========================================================================== */
/*                              Result codes                                  */
/* -------------------------------------------------------------------------- */
#define GDS_SUCCESS         0
#define GDS_FATAL          -1
#define GDS_BADWPOS        -2
#define GDS_NOTFOUND       -3
#define GDS_INCOMPLETE     -4
#define GDS_OPENFAIL       -5
#define GDS_NOTFOUND_2     -6
#define GDS_NOTFOUND_3     -7
#define GDS_CREFAIL        -8
#define GDS_DELFAIL        -9
#define GDS_ARGERR        -10
#define GDS_NOTUNIQ       -11
#define GDS_NOTUNIQ_2     -12
#define GDS_MORETHANONE   -13
#define GDS_AXNOTFOUND    -14
#define GDS_AXNOTFOUND_2  -15
#define GDS_SZAXNOTFOUND  -16
#define GDS_ORAXNOTFOUND  -17
#define GDS_NOTFITS       -18
#define GDS_COORDUNDEF    -19
#define GDS_NOUNDEFCOORD  -20
#define GDS_BADFORMAT     -21
#define GDS_BADTYPE       -22
#define GDS_NOTUNIQ_3     -23
#define GDS_NOTUNIQ_4     -24
#define GDS_BUFOVFL       -25
#define GDS_INVVARREC     -26
#define GDS_RECTOOBIG     -27
#define GDS_AXPRESENT     -28
#define GDS_FBUFLENERR    -29
#define GDS_BUFLENERR     -30
#define GDS_BADTRANSID    -31
#define GDS_ALLOCFAIL     -32
#define GDS_EXTENDFAIL    -33
#define GDS_COUTRANGE     -34
#define GDS_CTOOBIG       -35
#define GDS_IOPENFAIL     -36
#define GDS_TRUNCATED     -37
#define GDS_BADLINK       -38
#define GDS_NOTPRESENT    -39
#define GDS_ALRPRESENT    -40
#define GDS_INSUFFINF     -41
#define GDS_BADDIM        -42
#define GDS_NOMORESUBS    -43
#define GDS_TYPNOTFITS    -44
#define GDS_ISINT         -45
#define GDS_ISREAL        -46
#define GDS_ISDBLE        -47
#define GDS_BADVERSION    -48
#define GDS_BADDSCFILE    -49
#define GDS_BADBYTEORDER  -50

#define GDS_NOPRIV        -51
#define GDS_LOCKED        -52
#define GDS_NOTLOCKED     -53
#define GDS_BADFUNCTION   -54
#define GDS_NOTOPEN       -55
#define GDS_CONNFAIL      -56
#define GDS_BADHANDLE     -57
#define GDS_NOMEM         -58
#define GDS_RENAMEFAIL    -59
#define GDS_BADHEADER     -60
#define GDS_BADDIR        -61

#define GDS_TABNOTFOUND   -66
#define GDS_TABBADTYPE    -67
#define GDS_TABPASTEOI    -68
#define GDS_TABSKIPROW    -69
#define GDS_TABEOI        -70
#define GDS_TABTOOFEW     -71
#define GDS_AXTOOLONG     -72
#endif /* _gdserrors_h_ */
