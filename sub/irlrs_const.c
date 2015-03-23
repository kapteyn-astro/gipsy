/* irlrs_const.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irlrs_const.dc2

Function:     irlrs_const

Purpose:      Purpose

Category:     IRAS LRS

File:         filename.c

Author:       P.R. Roelfsema

Description:

      This file contains all LRS instrument specific routines. The
   following  routines are available:

   IRLRS_DETTYPE  - finds out what type a detector is.
   IRLRS_DN2MV    - converts raw data numbers to milli volts.
   IRLRS_PWM2PERMV- converts millivolts to pico Watt per square meter per micron.
   IRLRS_JYPERPWM2- converts pico Watt per square meter to Janskys.
   IRLRS_POS2WAVE - converts inscan position to wavelength.
   IRLRS_LGAIN    - applies in-scan gain correction.
   IRLRS_XGAIN    - applies cross-scan gain correction.
   IRLRS_ADROOP   - applies anti-droop correction.
   IRLRS_COHEN_GAIN - applais Cohen correction factors

Updates:      Sep 19, 1990: PRR, Creation date.
              Nov 27, 1990: PRR, Added many routines.
              Dec 17, 1990: PRR, Added irlrs_adroop.
              Jun 12, 1991: PRR, Change of units: Wm-2mu-2 -> Jy and v.v.
              Jul 22, 1991: PRR, Added Cohen gain factors
	                         Added max xgain correction.
#<
*/

#include "gipsyc.h"
#include "math.h"
#include "stdlib.h"
#include "spline1.h"
#include "setfblank.h"
#include "ircc_mask.h"
#include "userreal.h"

#define OK			  0		/* no-error */
#define	BAD_DETNO		 -1		/* bad detector number */
#define NOMEMORY		 -5		/* no memory for buffers */
#define	UNDEFINED		 -9		/* all data undefined	*/
#define SW_DETECTOR		  1		/* short wavelength det. */
#define LW_DETECTOR		  2		/* long wavelength det. */

#define NR_LRS_DETS		  5		/* number of LRS detectors */
#define	FIRST_SW_DET		 71		/* first legal sw detector */
#define	LAST_SW_DET		 73		/* last legal sw detector */
#define	FIRST_LW_DET		 74		/* first legal lw detector */
#define	LAST_LW_DET		 75		/* last legal lw detector */
#define SAMPPERSAT 	 	 32		/* number of samps/satcal */

	/* short wavelength position -> wavlength conversion parameters */
#define SW_LAMBDA_0		( (double) 1.05   )	/* lambda zero point */
#define SW_AMPL			( (double) 4.613  )	/* amplitude */
#define	SW_INSCAN_0		( (double) 4.462  )	/* position zero point */

	/* long wavelength position -> wavlength conversion parameters */
#define LW_LAMBDA_0		( (double) 0.05   )	/* lambda zero point */
#define LW_AMPL			( (double) 8.368  )	/* amplitude */
#define	LW_INSCAN_0		( (double) 4.470  )	/* position zero point */
						/* C in meters per second */
#define	C			( (float)2.998e8 )

#define	TAUDROOPS	{ 10.0 , 10.0 , 10.0 , 10.0 , 10.0 }
						/* time constant antidroop */
#define	DROOPLIMITS	{  0.1 ,  0.1 ,  0.1 ,  0.1 ,  0.1 }
						/* antidroop limits */

#define	MAXLRSINT		256		/* highest LRS datanumber */

static float lut[ MAXLRSINT + 1 ] = { 
  8.0671780E-02 ,  8.3697021E-02 ,  8.6834528E-02 ,  9.0088442E-02 ,
  9.3463108E-02 ,  9.6962973E-02 ,  0.1005927     ,  0.1043571     ,
  0.1082611     ,  0.1123100     ,  0.1165091     ,  0.1208639     ,
  0.1253802     ,  0.1300640     ,  0.1349216     ,  0.1399593     ,
  0.1451838     ,  0.1506020     ,  0.1562211     ,  0.1620485     ,
  0.1680920     ,  0.1743595     ,  0.1808594     ,  0.1876001     ,
  0.1945906     ,  0.2018402     ,  0.2093584     ,  0.2171551     ,
  0.2252407     ,  0.2336258     ,  0.2423215     ,  0.2513392     ,
  0.2606908     ,  0.2703886     ,  0.2804455     ,  0.2908747     ,
  0.3016898     ,  0.3129051     ,  0.3245354     ,  0.3365959     ,
  0.3491025     ,  0.3620716     ,  0.3755202     ,  0.3894659     ,
  0.4039271     ,  0.4189226     ,  0.4344723     ,  0.4505961     ,
  0.4673154     ,  0.4846520     ,  0.5026286     ,  0.5212684     ,
  0.5405959     ,  0.5606363     ,  0.5814155     ,  0.6029607     ,
  0.6252998     ,  0.6484618     ,  0.6724769     ,  0.6973760     ,
  0.7231914     ,  0.7499564     ,  0.7777058     ,  0.8064750     ,
  0.8363017     ,  0.8672237     ,  0.8992811     ,  0.9325151     ,
  0.9669681     ,  1.002684      ,  1.039709      ,  1.078090      ,
  1.117877      ,  1.159119      ,  1.201870      ,  1.246184      ,
  1.292116      ,  1.339725      ,  1.389070      ,  1.440217      ,
  1.493225      ,  1.548165      ,  1.605104      ,  1.664113      ,
  1.725268      ,  1.788641      ,  1.854316      ,  1.922371      ,
  1.992892      ,  2.065965      ,  2.141681      ,  2.220133      ,
  2.301417      ,  2.385632      ,  2.472882      ,  2.563273      ,
  2.656915      ,  2.753921      ,  2.854409      ,  2.958498      ,
  3.066315      ,  3.177989      ,  3.293652      ,  3.413443      ,
  3.537503      ,  3.665980      ,  3.799022      ,  3.936790      ,
  4.079442      ,  4.227145      ,  4.380067      ,  4.538393      ,
  4.702296      ,  4.871972      ,  5.047610      ,  5.229407      ,
  5.417577      ,  5.612328      ,  5.813873      ,  6.022448      ,
  6.238276      ,  6.461596      ,  6.692662      ,  6.931717      ,
  7.179028      ,  7.434855      ,  7.699485      ,  7.973192      ,
  1.9834086E-05 ,  4.9206312E-05 ,  7.9665158E-05 ,  1.1125131E-04 ,
  1.4400674E-04 ,  1.7797592E-04 ,  2.1320273E-04 ,  2.4973549E-04 ,
  2.8762227E-04 ,  3.2691349E-04 ,  3.6766258E-04 ,  4.0992274E-04 ,
  4.5375124E-04 ,  4.9920630E-04 ,  5.4634857E-04 ,  5.9524126E-04 ,
  6.4594886E-04 ,  6.9854077E-04 ,  7.5308554E-04 ,  8.0965838E-04 ,
  8.6833240E-04 ,  9.2918723E-04 ,  9.9230406E-04 ,  1.0577684E-03 ,
  1.1256657E-03 ,  1.1960886E-03 ,  1.2691299E-03 ,  1.3448884E-03 ,
  1.4234654E-03 ,  1.5049648E-03 ,  1.5894960E-03 ,  1.6771738E-03 ,
  1.7681144E-03 ,  1.8624376E-03 ,  1.9602722E-03 ,  2.0617477E-03 ,
  2.1669990E-03 ,  2.2761701E-03 ,  2.3894040E-03 ,  2.5068521E-03 ,
  2.6286724E-03 ,  2.7550298E-03 ,  2.8860895E-03 ,  3.0220279E-03 ,
  3.1630271E-03 ,  3.3092760E-03 ,  3.4609693E-03 ,  3.6183088E-03 ,
  3.7815087E-03 ,  3.9507835E-03 ,  4.1263588E-03 ,  4.3084719E-03 ,
  4.4973637E-03 ,  4.6932879E-03 ,  4.8965053E-03 ,  5.1072882E-03 ,
  5.3259148E-03 ,  5.5526835E-03 ,  5.7878913E-03 ,  6.0318531E-03 ,
  6.2848954E-03 ,  6.5473546E-03 ,  6.8195844E-03 ,  7.1019460E-03 ,
  7.3948116E-03 ,  7.6985764E-03 ,  8.0136489E-03 ,  8.3404416E-03 ,
  8.6793927E-03 ,  9.0309568E-03 ,  9.3956012E-03 ,  9.7738085E-03 ,
  1.0166090E-02 ,  1.0572960E-02 ,  1.0994965E-02 ,  1.1432670E-02 ,
  1.1886652E-02 ,  1.2357518E-02 ,  1.2845899E-02 ,  1.3352439E-02 ,
  1.3877813E-02 ,  1.4422727E-02 ,  1.4987900E-02 ,  1.5574087E-02 ,
  1.6182067E-02 ,  1.6812652E-02 ,  1.7466677E-02 ,  1.8145014E-02 ,
  1.8848566E-02 ,  1.9578267E-02 ,  2.0335091E-02 ,  2.1120042E-02 ,
  2.1934168E-02 ,  2.2778548E-02 ,  2.3654303E-02 ,  2.4562595E-02 ,
  2.5504641E-02 ,  2.6481694E-02 ,  2.7495041E-02 ,  2.8546039E-02 ,
  2.9636085E-02 ,  3.0766616E-02 ,  3.1939153E-02 ,  3.3155240E-02 ,
  3.4416497E-02 ,  3.5724595E-02 ,  3.7081283E-02 ,  3.8488358E-02 ,
  3.9947692E-02 ,  4.1461211E-02 ,  4.3030933E-02 ,  4.4658963E-02 ,
  4.6347428E-02 ,  4.8098594E-02 ,  4.9914774E-02 ,  5.1798392E-02 ,
  5.3751934E-02 ,  5.5778012E-02 ,  5.7879299E-02 ,  6.0058594E-02 ,
  6.2318780E-02 ,  6.4662874E-02 ,  6.7093976E-02 ,  6.9615304E-02 ,
  7.2230212E-02 ,  7.4942179E-02 ,  7.7754796E-02 ,  8.0671780E-02 ,
  0.0           } ;



/*   
   The following data were taken from table IX.B.1, Expl. Supp., page IX-5.

   The contain the LRS detector characteristics Noise Equivalent Flux Density
(nefd) in Jy, the detector noise level (rmsmv) in milliVolts, the 
dimensionless intensity fudge-factor (fudge) and the mean detector output when
stimulated by a flash (flash) in milliVolts.
   These quantities can be used to convert detector output in mV to real units
like Janskys.
*/

                   /* detector number   71      72      73      74      75  */
static float nefd[ NR_LRS_DETS ]  = {  1.4  ,  1.6  ,  1.3  ,  3.0  ,  2.5  } ;

static float rmsmv[ NR_LRS_DETS ] = {  0.10 ,  0.04 ,  0.06 ,  0.12 ,  0.20 } ;

static float fudge[ NR_LRS_DETS ] = {  0.75 ,  0.75 ,  0.75 ,  1.0  ,  1.0  } ;

static float lmcon[ NR_LRS_DETS ] = { 10 , 10 , 10 , 10 , 10 } ; 
/*static float lmcon[ NR_LRS_DETS ] = { 10.5  , 10.5  , 16.5  , 16.5  , 16.5  } ;*/

/* not used (yet)
static float flash[ NR_LRS_DETS ] = { 11.9  ,  4.4  ,  7.4  , 22.8  , 46.3  } ;
*/


#define	SW_LAST_LM		87		/* last shortwave lgain item */

static float sw_lm[ SW_LAST_LM + 1 ] = {
   1.734  ,   2.788  ,   3.411  ,   3.901  ,   4.318  ,
   4.688  ,   5.023  ,   5.333  ,   5.621  ,   5.892  ,
   6.149  ,   6.394  ,   6.627  ,   6.852  ,   7.068  ,
   7.276  ,   7.478  ,   7.674  ,   7.864  ,   8.048  ,
   8.229  ,   8.404  ,   8.576  ,   8.744  ,   8.908  ,
   9.069  ,   9.226  ,   9.381  ,   9.533  ,   9.682  ,
   9.829  ,   9.973  ,  10.115  ,  10.255  ,  10.392  ,
  10.528  ,  10.662  ,  10.794  ,  10.924  ,  11.052  ,
  11.179  ,  11.304  ,  11.428  ,  11.550  ,  11.671  ,
  11.791  ,  11.909  ,  12.026  ,  12.141  ,  12.256  ,
  12.369  ,  12.482  ,  12.593  ,  12.703  ,  12.812  ,
  12.920  ,  13.027  ,  13.133  ,  13.238  ,  13.343  ,
  13.446  ,  13.549  ,  13.650  ,  13.751  ,  13.851  ,
  13.951  ,  14.049  ,  14.147  ,  14.244  ,  14.341  ,
  14.436  ,  14.531  ,  14.626  ,  14.719  ,  14.812  ,
  14.905  ,  14.997  ,  15.088  ,  15.179  ,  15.269  ,
  15.358  ,  15.447  ,  15.536  ,  15.623  ,  15.711  ,
  15.798  ,  15.884  ,  15.970  } ;

static float sw_lgain[ SW_LAST_LM + 1 ] = {
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.39   ,
   1.34   ,   1.30   ,   1.28   ,   1.25   ,   1.25   ,
   1.23   ,   1.23   ,   1.26   ,   1.27   ,   1.23   ,
   1.20   ,   1.19   ,   1.17   ,   1.16   ,   1.10   ,
   1.04   ,   1.01   ,   1.00   ,   0.99   ,   0.99   ,
   0.97   ,   0.95   ,   0.98   ,   0.99   ,   1.00   ,
   0.99   ,   0.99   ,   0.99   ,   1.00   ,   1.00   ,
   1.01   ,   1.00   ,   1.01   ,   1.03   ,   1.03   ,
   1.03   ,   1.05   ,   1.06   ,   1.07   ,   1.09   ,
   1.10   ,   1.15   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   } ;

/*
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.20   ,   1.40   ,
   1.60   ,   1.80   ,   1.71   ,   1.46   ,   1.39   ,
   1.34   ,   1.30   ,   1.28   ,   1.25   ,   1.25   ,
   1.23   ,   1.23   ,   1.26   ,   1.27   ,   1.23   ,
   1.20   ,   1.19   ,   1.17   ,   1.16   ,   1.10   ,
   1.04   ,   1.01   ,   1.00   ,   0.99   ,   0.99   ,
   0.97   ,   0.95   ,   0.98   ,   0.99   ,   1.00   ,
   0.99   ,   0.99   ,   0.99   ,   1.00   ,   1.00   ,
   1.01   ,   1.00   ,   1.01   ,   1.03   ,   1.03   ,
   1.03   ,   1.05   ,   1.06   ,   1.07   ,   1.09   ,
   1.10   ,   1.15   ,   1.62   ,   1.46   ,   1.30   ,
   1.15   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   } ;
*/


#define	LW_LAST_LM		87		/* last longwave lgain item */

static float lw_lm[ LW_LAST_LM +  1 ]   = { 
   1.499  ,   3.291  ,   4.398  ,   5.276  ,   6.026  ,
   6.692  ,   7.297  ,   7.855  ,   8.376  ,   8.867  ,
   9.331  ,   9.773  ,  10.196  ,  10.602  ,  10.993  ,
  11.370  ,  11.736  ,  12.090  ,  12.434  ,  12.769  ,
  13.095  ,  13.413  ,  13.724  ,  14.028  ,  14.325  ,
  14.617  ,  14.902  ,  15.182  ,  15.458  ,  15.728  ,
  15.994  ,  16.255  ,  16.512  ,  16.766  ,  17.015  ,
  17.261  ,  17.503  ,  17.743  ,  17.978  ,  18.211  ,
  18.441  ,  18.668  ,  18.893  ,  19.114  ,  19.333  ,
  19.550  ,  19.764  ,  19.976  ,  20.186  ,  20.394  ,
  20.599  ,  20.803  ,  21.004  ,  21.204  ,  21.401  ,
  21.597  ,  21.791  ,  21.984  ,  22.175  ,  22.364  ,
  22.551  ,  22.737  ,  22.922  ,  23.105  ,  23.286  ,
  23.466  ,  23.645  ,  23.822  ,  23.998  ,  24.173  ,
  24.347  ,  24.519  ,  24.690  ,  24.860  ,  25.029  ,
  25.197  ,  25.363  ,  25.529  ,  25.693  ,  25.856  ,
  26.019  ,  26.180  ,  26.340  ,  26.500  ,  26.658  ,
  26.815  ,  26.972  ,  27.128   } ;

static float lw_lgain[ LW_LAST_LM + 1 ] = {
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.07   ,
   1.04   ,   1.03   ,   1.00   ,   0.98   ,   0.95   ,
   0.93   ,   0.91   ,   0.89   ,   0.87   ,   0.86   ,
   0.85   ,   0.86   ,   0.86   ,   0.87   ,   0.86   , 
   0.86   ,   0.85   ,   0.84   ,   0.83   ,   0.82   ,
   0.82   ,   0.82   ,   0.82   ,   0.81   ,   0.81   ,
   0.80   ,   0.81   ,   0.81   ,   0.81   ,   0.81   ,
   0.80   ,   0.81   ,   0.80   ,   0.79   ,   0.84   ,
   0.85   ,   0.84   ,   0.87   ,   0.87   ,   0.83   ,
   0.83   ,   0.86   ,   0.87   ,   0.87   ,   0.90   ,
   0.90   ,   0.93   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00    } ;

/*
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.10   ,
   1.20   ,   1.30   ,   1.40   ,   1.26   ,   1.07   ,
   1.04   ,   1.03   ,   1.00   ,   0.98   ,   0.95   ,
   0.93   ,   0.91   ,   0.89   ,   0.87   ,   0.86   ,
   0.85   ,   0.86   ,   0.86   ,   0.87   ,   0.86   , 
   0.86   ,   0.85   ,   0.84   ,   0.83   ,   0.82   ,
   0.82   ,   0.82   ,   0.82   ,   0.81   ,   0.81   ,
   0.80   ,   0.81   ,   0.81   ,   0.81   ,   0.81   ,
   0.80   ,   0.81   ,   0.80   ,   0.79   ,   0.84   ,
   0.85   ,   0.84   ,   0.87   ,   0.87   ,   0.83   ,
   0.83   ,   0.86   ,   0.87   ,   0.87   ,   0.90   ,
   0.90   ,   0.93   ,   1.28   ,   1.21   ,   1.14   ,
   1.07   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00   ,   1.00   ,   1.00   ,
   1.00   ,   1.00   ,   1.00    } ;
*/

#define	NR_XGAINS	16		/* nr of entries in xgain table */

static float xgain[ NR_XGAINS ][ NR_LRS_DETS ] = {
      5.00  ,    5.00  ,    5.00  ,    2.48  ,    1.79  , 
      2.08  ,    1.58  ,    1.26  ,    1.58  ,    1.17  , 
      1.54  ,    1.33  ,    1.11  ,    1.28  ,    1.03  , 
      1.33  ,    1.21  ,    1.05  ,    1.13  ,    0.97  , 
      1.21  ,    1.14  ,    1.01  ,    1.05  ,    0.94  , 
      1.11  ,    1.08  ,    0.98  ,    1.01  ,    0.93  , 
      1.04  ,    1.03  ,    0.97  ,    0.99  ,    0.93  , 
      0.98  ,    0.98  ,    0.96  ,    0.98  ,    0.95  , 
      0.94  ,    0.95  ,    0.97  ,    0.98  ,    0.98  , 
      0.92  ,    0.94  ,    0.99  ,    0.99  ,    1.02  , 
      0.91  ,    0.93  ,    1.03  ,    1.00  ,    1.09  , 
      0.91  ,    0.94  ,    1.08  ,    1.11  ,    1.17  , 
      0.93  ,    0.96  ,    1.15  ,    1.07  ,    1.31  , 
      1.01  ,    1.02  ,    1.30  ,    1.01  ,    1.46  , 
      1.26  ,    1.39  ,    2.25  ,    1.13  ,    1.71  , 
      5.00  ,    5.00  ,    5.00  ,    1.72  ,    2.46   } ;


#define	NR_COHEN_GAINS	104		/* nr. of gains in Cohen table	*/

static float lm_cohen[ NR_COHEN_GAINS ] = {
 0.0           , 0.5           , 1.0           , 2.0           ,           
 3.0           , 4.0           , 5.0           , 5.75          ,
 6.5           , 6.75          , 7.0           , 7.3           ,
 7.6736002E+00 , 7.8635998E+00 , 8.0485001E+00 , 8.2285995E+00 ,
 8.4042997E+00 , 8.5757999E+00 , 8.7435999E+00 , 8.9077997E+00 ,
 9.0685997E+00 , 9.2263002E+00 , 9.3809004E+00 , 9.5327997E+00 ,
 9.6820002E+00 , 9.8285999E+00 , 9.9728003E+00 , 1.0115000E+01 ,
 1.0255000E+01 , 1.0392000E+01 , 1.0528000E+01 , 1.0662000E+01 ,
 1.0794000E+01 , 1.0924000E+01 , 1.1052000E+01 , 1.1179000E+01 ,
 1.1304000E+01 , 1.1428000E+01 , 1.1550000E+01 , 1.1671000E+01 ,
 1.1791000E+01 , 1.1909000E+01 , 1.2026000E+01 , 1.2141000E+01 ,
 1.2256000E+01 , 1.2369000E+01 , 1.2482000E+01 , 1.2593000E+01 ,
 1.2703000E+01 , 1.2790500E+01 , 1.3095000E+01 , 1.3413000E+01 ,
 1.3724000E+01 , 1.4028000E+01 , 1.4325000E+01 , 1.4617000E+01 ,
 1.4902000E+01 , 1.5182000E+01 , 1.5458000E+01 , 1.5728000E+01 ,
 1.5994000E+01 , 1.6254999E+01 , 1.6511999E+01 , 1.6766001E+01 ,
 1.7014999E+01 , 1.7261000E+01 , 1.7503000E+01 , 1.7743000E+01 ,
 1.7978001E+01 , 1.8211000E+01 , 1.8441000E+01 , 1.8667999E+01 ,
 1.8893000E+01 , 1.9114000E+01 , 1.9333000E+01 , 1.9549999E+01 ,
 1.9764000E+01 , 1.9976000E+01 , 2.0186001E+01 , 2.0393999E+01 ,
 2.0599001E+01 , 2.0802999E+01 , 2.1004000E+01 , 2.1204000E+01 ,
 2.1400999E+01 , 2.1597000E+01 , 2.1791000E+01 , 2.1983999E+01 ,
 2.2174999E+01 , 2.2364000E+01 , 2.2551001E+01 , 2.2737000E+01 ,
 23.0          , 23.25         , 23.5          , 23.75         ,
 24.0          , 25.0          , 26.0          , 27.0          ,
 28.0          , 30.0          , 35.0          , 40.0          } ;

static float gn_cohen[ NR_COHEN_GAINS ] = {
 1.0           , 1.0           , 1.0           , 1.0           ,
 1.0           , 1.0           , 1.0           , 1.0           ,
 1.0           , 1.0           , 1.0           , 1.0           ,
 9.7919875E-01 , 1.0416722E+00 , 1.0793177E+00 , 1.0986108E+00 ,
 1.1073092E+00 , 1.1075778E+00 , 1.1036209E+00 , 1.0965613E+00 ,
 1.0878743E+00 , 1.0786721E+00 , 1.0690680E+00 , 1.0602771E+00 ,
 1.0522034E+00 , 1.0439097E+00 , 1.0369358E+00 , 1.0300421E+00 ,
 1.0234720E+00 , 1.0186695E+00 , 1.0135212E+00 , 1.0095682E+00 ,
 1.0063103E+00 , 1.0040534E+00 , 1.0016778E+00 , 1.0003479E+00 ,
 1.0002164E+00 , 9.9883491E-01 , 9.9874854E-01 , 9.9920249E-01 ,
 9.9964333E-01 , 1.0000678E+00 , 1.0004798E+00 , 1.0008762E+00 ,
 1.0012642E+00 , 1.0016371E+00 , 1.0020019E+00 , 1.0023520E+00 ,
 1.0026914E+00 , 1.0029557E+00 , 1.0038372E+00 , 1.0046943E+00 ,
 1.0054697E+00 , 1.0061674E+00 , 1.0067918E+00 , 1.0073503E+00 ,
 1.0078427E+00 , 1.0082756E+00 , 1.0086530E+00 , 1.0089749E+00 ,
 1.0092461E+00 , 1.0094681E+00 , 1.0096439E+00 , 1.0097759E+00 ,
 1.0098650E+00 , 1.0099140E+00 , 1.0099243E+00 , 1.0098974E+00 ,
 1.0098350E+00 , 1.0097382E+00 , 1.0096085E+00 , 1.0094470E+00 ,
 1.0092543E+00 , 1.0090333E+00 , 1.0087836E+00 , 1.0085055E+00 ,
 1.0082017E+00 , 1.0078717E+00 , 1.0075165E+00 , 1.0071365E+00 ,
 1.0067350E+00 , 1.0063084E+00 , 1.0058621E+00 , 1.0053923E+00 ,
 1.0049043E+00 , 1.0043939E+00 , 1.0038646E+00 , 1.0033140E+00 ,
 1.0027456E+00 , 1.0021601E+00 , 1.0015579E+00 , 1.0009369E+00 ,
 1.0           , 1.0           , 1.0           , 1.0           ,
 1.0           , 1.0           , 1.0           , 1.0           ,
 1.0           , 1.0           , 1.0           , 1.0           } ;

static float err_cohen[ NR_COHEN_GAINS ] = {
 0.0           , 0.0           , 0.0           , 0.0           , 
 0.0           , 0.0           , 0.0           , 0.0           , 
 0.0           , 0.0           , 0.0           , 0.0           , 
 6.7341635E-03 , 7.1507990E-03 , 7.3919590E-03 , 7.5241933E-03 ,
 7.5541716E-03 , 7.6669729E-03 , 7.7781468E-03 , 7.8280568E-03 ,
 7.9290466E-03 , 8.0204364E-03 , 7.9051051E-03 , 7.9726633E-03 ,
 8.0678686E-03 , 8.1399111E-03 , 8.0788340E-03 , 8.0741290E-03 ,
 8.1037739E-03 , 8.1425151E-03 , 8.0551282E-03 , 8.1118625E-03 ,
 8.1075020E-03 , 8.1415270E-03 , 8.0990791E-03 , 8.0968067E-03 ,
 8.1517324E-03 , 8.1781968E-03 , 8.1779808E-03 , 8.1893699E-03 ,
 8.3165551E-03 , 8.5164430E-03 , 8.9019407E-03 , 8.9900689E-03 ,
 9.0720458E-03 , 9.5634190E-03 , 1.0009247E-02 , 9.9763265E-03 ,
 1.0270852E-02 , 9.2833675E-03 , 1.2806821E-02 , 1.3869423E-02 ,
 1.4660554E-02 , 1.5354089E-02 , 1.5791047E-02 , 1.6590437E-02 ,
 1.7094322E-02 , 1.7559374E-02 , 1.8058445E-02 , 1.8094463E-02 ,
 1.8254450E-02 , 1.8396633E-02 , 6.8887849E-03 , 6.6989213E-03 ,
 6.5737269E-03 , 6.4947181E-03 , 6.4259698E-03 , 6.4904806E-03 ,
 6.5716836E-03 , 6.5827440E-03 , 6.6181133E-03 , 6.6457284E-03 ,
 6.7407242E-03 , 6.8503045E-03 , 6.9648628E-03 , 7.0933904E-03 ,
 7.0841387E-03 , 7.1384613E-03 , 7.0181768E-03 , 6.8233651E-03 ,
 6.6476665E-03 , 6.6696252E-03 , 6.7017856E-03 , 6.7937374E-03 ,
 6.9382796E-03 , 7.1143066E-03 , 7.1388287E-03 , 7.2319014E-03 ,
 7.3899878E-03 , 7.3892646E-03 , 7.5926012E-03 , 1.9582467E-02 ,
 0.0           , 0.0           , 0.0           , 0.0           , 
 0.0           , 0.0           , 0.0           , 0.0           , 
 0.0           , 0.0           , 0.0           , 0.0           } ;


/* irlrs_dettype.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irlrs_dettype.dc2

Function:     irlrs_dettype

Purpose:      finds out what type a detector is.

Category:     IRAS LRS

File:         irlrs_const.c

Author:       P.R. Roelfsema

Use:          IRLRS_DETTYPE( DETNO )      Input   integer
                           
              IRLRS_DETTYPE - returns  
                          -1 if DETNO is not a legal LRS detector number
                           1 if DETNO is a short wavelength detector
                           2 if DETNO is a long wavelength detector
              DETNO         - detector number. Valid LRS detectors are
                              71, 72, 73 short wavelength and 74 and 75
                              long wavelength detectors.
                                

Updates:      Nov 27, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function irlrs_dettype( integer )

*/

fint irlrs_dettype_c( fint *detno ) 
{
   if ( *detno < FIRST_SW_DET || *detno > LAST_LW_DET ) {
      return( BAD_DETNO ) ;
   }
   if ( *detno < FIRST_LW_DET ){
      return( SW_DETECTOR ) ;
   }
   return( LW_DETECTOR ) ;
}



/* irlrs_dn2mv.c

#>            irlrs_dn2mv.dc2

Function:     irlrs_dn2mv

Purpose:      Converts unsigned LRS datanumbers to millivolts

Category:     LRS

File:         irlrs_const.c

Author:       P.R. Roelfsema

Use:          IRLRS_DN2MV( DATA     ,      In/Out   real( >= NDATA )
                           NDATA    )      Input    integer

              DATA         Input:  array containing datanumbers.
                           Output: array containing millivolts.
              NDATA        Number of ellements in DATA to be converted.

Description:

              IRLRS_DN2MV converts the first NDATA elements of the array
              DATA from unsigned data numbers (ranging from 0 to 255) 
              to millivolts.
              If the input contains a value outside the permitted range
              0.0 mV is returned for the entry. 

Updates:      Sep 19, 1990: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irlrs_dn2mv( real , integer )

*/

void irlrs_dn2mv_c( float *data, fint *ndata ) 
{
   fint i , intval ;

   float blank ;

   setfblank_c( &blank ) ;

   for ( i = 0 ; i < *ndata ; i++ ) {
      if( data[ i ] != blank ) {
         intval    = ( ( data[ i ] >= 0 ) && ( data[ i ] < MAXLRSINT ) ) ? 
                                    (int) data[ i ] : MAXLRSINT ;
         data[ i ] = lut[ intval ] * 1000 ;
      }
   }
}

 
/* irlrs_pwm2permv.c

#>            irlrs_pwm2permv.dc2

Function:     irlrs_pwm2permv

Purpose:      converts millivolts to pico Watt per square meter per micron.

Category:     IRAS LRS

File:         irlrs_const.c

Author:       P.R. Roelfsema

Use:          REAL IRLRS_PWM2PERMV( DETNO )   Input   integer

              IRLRS_PWM2PERMV - returns scale factor in 10^-12 Wm^-2mu^-2/mV 
                                which can be used to convert LRS data from 
                                detector DETNO from mV to Wm-2mu-2.
                                For in-valid DETNO 0.0 is returned.
              DETNO           - detector number. Valid LRS detectors are
                                71, 72, 73 short wavelength and 74 and 75
                                long wavelength detectors.

Updates:      Nov 27, 1990: PRR, Creation date

#<

Fortran to C interface:

@ real function irlrs_pwm2permv( integer )

*/

float irlrs_pwm2permv_c( fint *detno ) 
{
   float factor = 0.0 ;

   if ( irlrs_dettype_c( detno ) > 0 ) {
      factor = nefd[ *detno - FIRST_SW_DET ] / 
              ( rmsmv[ *detno - FIRST_SW_DET ] 
                * fudge[ *detno - FIRST_SW_DET ] 
                  * lmcon[ *detno - FIRST_SW_DET ] 
                    * lmcon[ *detno - FIRST_SW_DET ] ) ;
   }
   return( factor ) ;
}


/* irlrs_jyperpwm2.c

#>            irlrs_jyperpwm2.dc2

Function:     irlrs_jyperpwm2

Purpose:      converts pico Watt per square meter to Janskys.

Category:     IRAS LRS

File:         irlrs_const.c

Author:       P.R. Roelfsema

Use:          IRLRS_JYPERPWM2( LAMBDA  ,     Input   real( >= NDATA )
                               PWM2S    ,     Input   real( >= NDATA )
                               NDATA   ,     Input   integer
                               JANSKYS   )   Output  real( >= NDATA )


              LAMBDA  - array containing wavelengths (in micron) for
                        which the conversion is to be done.
              PWM2S   - array containing fluxes in pico Watts per square
                        meter to be converted to Jy.
              JANSKYS - array containing converted fluxes.
              NDATA   - number of elements to be converted.

Updates:      Nov 27, 1990: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irlrs_jyperpwm2( real , real , integer , real )

*/

void irlrs_jyperpwm2_c( float *lm , float *pwm2s , fint *ndata , float *jys ) 
{
   fint     n = 0 ;

   float    blank ;

   setfblank_c( &blank ) ;
   for ( n = 0 ; n < *ndata ; n++ ) {
      if ( ( lm[ n ] > 0 ) && ( pwm2s[ n ] != blank ) ) {
         jys[ n ] = 1e8 * pwm2s[ n ] * lm[ n ] / C ;
      } else {
         setfblank_c( &jys[ n ] ) ;
      }
   }
}



/* irlrs_pos2wave.c

#>            irlrs_pos2wave.dc2

Function:     irlrs_pos2wave

Purpose:      converts inscan position to wavelength.

Category:     IRAS LRS

File:         irlrs_const.c

Author:       P.R. Roelfsema

Use:          IRLRS_POS2WAVE( DETNO    ,      Input   integer
                              INSCAN   ,      Input   real( >= NDATA )
                              NDATA    ,      Input   integer
                              LAMBDA     )    Output  real( >= NDATA )

              IRLRS_POS2WAVE - returns
                    -1 DETNO is not a legal LRS detector.
                     1 OK, DETNO is short wave detector.
                     2 OK, DETNO is long wave detector.
              DETNO  - LRS detector number.
              INSCAN - array containing in-scan offset in degree w.r.t.
                       the center of the detector os defined in IRCC_MASK.
                       POSITIVE offsets are TOWARDS the bore-sight!
              NDATA  - number of elements to be converted from position
                       to wavelength.
              LAMBDA - array containing wavelengths corresponding to INSCAN.
                       If the calculation is not valid for a given INSCAN
                       the corresponding LAMBDA gets 0 returned.
              

Updates:      Nov 27, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function irlrs_pos2wave( integer , real , integer , real )

*/

fint irlrs_pos2wave_c( fint *detno , float *is , fint *ndata , float *lm ) 
{
   fint    r = OK ;
   fint    n = 0 ;

   double  relpos = 0 ;

   switch ( r = irlrs_dettype_c( detno ) ) {
      case BAD_DETNO   :
                break ;
      case SW_DETECTOR : 
                for( n = 0 ; n < *ndata ; n++ ) {
                   relpos  = SW_INSCAN_0 - is[ n ] * 60 ;
                   if ( relpos <= 0 ) {
                      lm[ n ] = SW_LAMBDA_0 - SW_AMPL * sqrt( -relpos ) ;
                   } else {
                      lm[ n ] = SW_LAMBDA_0 + SW_AMPL * sqrt( relpos ) ;
                   }
                }
                break ;
      case LW_DETECTOR : 
                for( n = 0 ; n < *ndata ; n++ ) {
                   relpos  = LW_INSCAN_0 - is[ n ] * 60 ;
                   if ( relpos <= 0 ) {
                      lm[ n ] = LW_LAMBDA_0 - LW_AMPL * sqrt( -relpos ) ;
                   } else {
                      lm[ n ] = LW_LAMBDA_0 + LW_AMPL * sqrt( relpos ) ;
                   }
                }
                break ;
      default : break ;
   }

   return( r ) ;
}


/* irlrs_lgain.c

#>            irlrs_lgain.dc2

Function:     irlrs_lgain

Purpose:      applies in-scan gain correction for LRS data.

Category:     IRAS LRS

File:         irlrs_const.c

Author:       P.R. Roelfsema

Use:          IRLRS_LGAIN( DETNO    ,      Input   integer
                           LAMBDA   ,      Input   real ( >= NDATA )
                           NDATA    ,      Input   integer
                           DATA      )     In/Out  real ( >= NDATA )

              IRLRS_LGAIN returns:
                    -1 DETNO is not a legal LRS detector.
                     1 OK, DETNO is short wave detector.
                     2 OK, DETNO is long wave detector.
              DETNO  - LRS detector number.
              LAMBDA - array containing wavelengths for which gain factors
                       are to be calculated.
              NDATA  - number of points for which gain is to be applied.
              DATA   - data array to which gain factors are applied.
              
Updates:      Nov 28, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function irlrs_lgain( integer , real , integer , real )

*/

fint irlrs_lgain_c( fint *detno , float *lm , fint *ndata , float *data ) 
{
   fint    r     = OK ;
   fint    n     = 0 ;
   fint    m     = 0 ;
   fint    lmind = 0 ;

   double  gain  = 0 ;

   switch ( r = irlrs_dettype_c( detno ) ) {
      case BAD_DETNO : 
                break ;
      case SW_DETECTOR : 
                for ( n = 0 ; n < *ndata ; n++ ) {
                   if ( ( lm[ n ] <  sw_lm[ 0 ]          ) ||  
                        ( lm[ n ] >= sw_lm[ SW_LAST_LM ] ) )  {
                      gain = 1 ;
                   } else {
                      if ( ( lmind != 0 ) && ( lm[ n ] < sw_lm[ lmind ] ) ) {
                         lmind = 0 ;
                      }
                      for ( m = lmind ; m < SW_LAST_LM ; m++ ){
                         if ( ( lm[ n ] >= sw_lm[ m ]      ) &&
                              ( lm[ n ] <  sw_lm[ m + 1 ]  ) ) {
                             lmind = m ;
                             break ;
                         }
                      }
                      gain = sw_lgain[ lmind ] + ( lm[ n ] - sw_lm[ lmind ] )
                               * ( sw_lgain[ lmind + 1 ] - sw_lgain[ lmind ] ) 
                                 / ( sw_lm[ lmind + 1 ] - sw_lm[ lmind ] ) ;
                   }
                   data[ n ] = data[ n ] * gain ;
                }
                break ;
      case LW_DETECTOR : 
                for ( n = 0 ; n < *ndata ; n++ ) {
                   if ( ( lm[ n ] <  lw_lm[ 0 ]          ) ||  
                        ( lm[ n ] >= lw_lm[ LW_LAST_LM ] ) )  {
                      gain = 1 ;
                   } else {
                      if ( ( lmind != 0 ) && ( lm[ n ] < lw_lm[ lmind ] ) ) {
                         lmind = 0 ;
                      }
                      for ( m = lmind ; m < LW_LAST_LM ; m++ ){
                         if ( ( lm[ n ] >= lw_lm[ m ]      ) &&
                              ( lm[ n ] <  lw_lm[ m + 1 ]  ) ) {
                             lmind = m ;
                             break ;
                         }
                      }
                      gain = lw_lgain[ lmind ] + ( lm[ n ] - lw_lm[ lmind ] )
                               * ( lw_lgain[ lmind + 1 ] - lw_lgain[ lmind ] )
                                 / ( lw_lm[ lmind + 1 ] - lw_lm[ lmind ] ) ;
                   }
                   data[ n ] = data[ n ] * gain ;
                }
                break ;
      default : break ;
   }

   return( r ) ;
}


/* irlrs_xgain.c

#>            irlrs_xgain.dc2

Function:     irlrs_xgain

Purpose:      applies cross-scan gain correction for LRS data.

Category:     IRAS LRS

File:         irlrs_const.c

Author:       P.R. Roelfsema

Use:          IRLRS_XGAIN( DETNO    ,      Input   integer
			   MXGAIN   ,      Input   real
                           XSCAN    ,      Input   real ( >= NDATA )
                           NDATA    ,      Input   integer
                           DATA      )     In/Out  real ( >= NDATA )

              IRLRS_XGAIN returns:
                    -1 DETNO is not a legal LRS detector.
                     1 OK, DETNO is short wave detector.
                     2 OK, DETNO is long wave detector.
              DETNO  - LRS detector number.
              MXGAIN - maximum cross-scan gain correction allowed.
                       If for a cross-scan offset XSCAN the gain is larger
                       than MXGAIN the corresponding element in DATA 
                       is set to UNDEFINED.
              XSCAN  - array containing cross-scan offsets in degrees w.r.t.
                       the center of DETNO as defined by IRCC_MASK for which 
                       gains are to be calculated. 
              NDATA  - number of points for which gain is to be applied.
              DATA   - data array to which gain factors are applied.
              
Updates:      Nov 28, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function irlrs_xgain( integer , real , real , integer , real )

*/

fint irlrs_xgain_c( fint *detno , float *mxgain , 
                    float *xs , fint *ndata , float *data ) 
{
   fint    r      = OK ;
   fint    n      = 0 ;
   fint    intloc = 0 ;
   fint    det    = 0 ;

   float   yloc , zloc , ysize , zsize ;
   float   fltloc = 0 , remloc = 0 ;

   float   blank ;

   double  gain   = 0 ;

   setfblank_c( &blank ) ;
   if ( ( r = irlrs_dettype_c( detno ) ) < 0 ) {
      return( r ) ;
   }
   (void) ircc_mask_c( detno , &yloc , &zloc , &ysize ,&zsize ) ;
   det = *detno - FIRST_SW_DET ;
   for ( n = 0 ; n < *ndata ; n++ ) {
      if ( fabs( xs[ n ] ) >= zsize / 120  ) {
         gain = 1000.0 ;
      } else {
         fltloc = ( (float) NR_XGAINS - 1 ) * 
                     ( -xs[ n ] / ( zsize / 60 ) + 0.5 ) ;  
         intloc = (int) fltloc ;
         remloc = fltloc - intloc ;
         gain   = xgain[ intloc ][ det ] + 
                      remloc * ( xgain[ intloc + 1 ][ det ]
                                   - xgain[ intloc ][ det ] ) ;
      }
      if( fabs( gain ) > *mxgain  ) {
         setfblank_c( &data[ n ] ) ;
      } else {
         data[ n ] = data[ n ] * gain ;
      }
   }

   return( r ) ;
}


/* irlrs_adroop.c

#>            irlrs_adroop.dc2

Function:     irlrs_adroop

Purpose:      applies anti-droop correction for LRS data.

Category:     IRAS LRS

File:         irlrs_const.c

Author:       P.R. Roelfsema

Use:          IRLRS_ADROOP( DETNO    ,      Input   integer
                            DATA     ,      Input   real ( >= NDATA )
                            NDATA    )      Input   integer

              IRLRS_adroop returns:
                     0 success.
                    -1 DETNO is not a legal LRS detector.
                    -5 could not get memory for internal buffers to 
                       interpolate over undefined values.
              DETNO  - LRS detector number.
              DATA   - array containing data ( in mV ) to be AD-corrected.
              NDATA  - number of points for which AD correction is 
                       to be applied.
              
                ADroop performs the anti droop correction by integrating 
              the signal in the DATA array starting when the data 
              value is larger than the droop limit (which can be set by 
              the user using ADLIM=).

                Before actually performing the anti droop correction, the
              data are checked on undefined values. If any undefined values 
              are found, the data are interpolated over the undefineds.

*** THPF=     High-pass filter time constants [ all 10 sec. ]

*** ADLIM=    Anti-droop limits [ all 0.01 mV. ]

Updates:      Dec 17, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function irlrs_adroop( integer , real , integer )

*/




fint irlrs_adroop_c( fint *detno , float *data , fint *nsamp )
{
   fint sample = 0 , nblank = 0 , sdet , error = 0 ;

   float taudroop[ NR_LRS_DETS ] = TAUDROOPS ; 
   float limit[ NR_LRS_DETS ]    = DROOPLIMITS ;
   float runint = 0 , current = 0 , *xi , *yi , blank ;

   sdet = *detno - FIRST_SW_DET + 1 ;
   if ( sdet < 1 || sdet > 5 ) {
      return( BAD_DETNO ) ;
   }

   setfblank_c( &blank ) ;
   for ( sample = 0 ; sample < *nsamp ; sample++ )
      if ( data[ sample ] == blank ) nblank += 1 ;
   if ( nblank != 0 ) {
      xi = (float *) malloc( ( *nsamp ) * sizeof( float ) ) ;
      yi = (float *) malloc( ( *nsamp ) * sizeof( float ) ) ;
      if( !xi || !yi ) return( NOMEMORY ) ;
      for ( sample = 0 ; sample < *nsamp ; sample++ ) {
         xi[ sample ] = sample ;
         yi[ sample ] = data[ sample ] ;
      }
      for( sample = 0 ; 
           sample < *nsamp , yi[ sample ] == blank ; sample++ ){
         yi[ sample ] = 0 ;
      }
      for( sample = *nsamp - 1 ; 
           sample > 0 , yi[ sample ] == blank ; sample-- ){
         yi[ sample ] = 0 ;
      }
      error = spline1_c( xi , yi , nsamp , xi , data , nsamp ) ;
      free( xi ) ;
      free( yi ) ;
      if ( error < 0 ) return( error ) ;
   }

   for ( sample = 0 ; sample < *nsamp ; sample++ ) {
      if( data[ sample ]  < limit[ sdet - 1 ] ) {
         runint = 0 ;
      }
      current        = data[ sample ] + 
                          runint / ( SAMPPERSAT * taudroop[ sdet - 1 ] ) ;
      runint         = runint + data[ sample ] ;
      data[ sample ] = current ;
   }

   return( OK ) ;
}


/* irlrs_cohen_gain.c

#>            irlrs_cohen_gain.dc2

Function:     irlrs_cohen_gain

Purpose:      applies Cohen gain correction factors for LRS data.

Category:     IRAS LRS

File:         irlrs_const.c

Author:       P.R. Roelfsema

Use:          IRLRS_COHEN_GAIN( LAMBDA   ,      Input   real ( >= NDATA )
                                NDATA    ,      Input   integer
                                DATA      )     In/Out  real ( >= NDATA )

              IRLRS_LGAIN returns:
                    -5 not enough memory for internal arrays.
                    -9 all data in DATA were undefined
                   -11 not enough memory for interpolation
                   -12 interpolation error
              LAMBDA - array containing wavelengths for which gain factors
                       are to be calculated.
              NDATA  - number of points for which gain is to be applied.
              DATA   - data array to which gain factors are applied.
              
Updates:      Jul 20, 1993: PRR, Creation date

#<

Fortran to C interface:

@ integer function irlrs_cohen_gain( real , integer , real )

*/

fint irlrs_cohen_gain_c( float *lm , fint *ndata , float *data ) 
{
   fint    r     = OK ;
   fint    n     = 0 ;

   float   *gain = NULL ;

   gain = calloc( *ndata , sizeof( float ) ) ;
   if ( gain == NULL ) {
      r = NOMEMORY ;
      return( r ) ;
   }

   n = NR_COHEN_GAINS ;
   r = spline1_c( lm_cohen , gn_cohen , &n , lm , gain , ndata ) ;
   if( r < 0 ){
      r = r - 10 ;
      free( gain ) ;
      return( r ) ;
   }
   if( r == *ndata ){
      r = UNDEFINED ;
      free( gain ) ;
      return( r ) ;
   }       

   for( n = 0 ; n < *ndata ; n++ ) {
      data[ n ] = data[ n ] / gain[ n ] ;
   }

   free( gain ) ;

   return( r ) ;
}


