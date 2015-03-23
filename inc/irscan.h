/*   irscan.h

           Copyright (c) 1992
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.


*/

#define SCAN_TYPE	1		/* signal scan_type has been defined */

#define	MAXCAL		3		/* maximum stripe power */
/* flags on the data samples */
#define OKFLAG          0               /* data is ok */
#define OUTAGEFLAG      1               /* outage or overflow: blank data */
#define SOURCEFLAG      2               /* point source */
#define TAILFLAG        4               /* tail of point source */
#define SPIKEFLAG       8               /* glitch */

/* get memory for arrays which is initialized to zero */
#define FLOAT_INIT( f, n ) \
	f = (float*)calloc( ( n ), sizeof( float ) ) ; assert( f )
#define INT_INIT( f, n ) \
	f = (int*)calloc( ( n ), sizeof( int ) ) ; assert( f )
#define DOUBLE_INIT( f, n ) \
	f = (double*)calloc( ( n ), sizeof( double ) ) ; assert( f )

/* free memory of the arrays above */
#define FREE( p )		free( p ) ; p = NULL 

typedef struct {
  fint		scannr ;	/* sequential number of this detector-snip */
  fint		sop ;		/* sop number of the scan */
  fint		att ; 		/* attitude block number */
  fint		leg ;		/* leg counter (AOs only; 0 otherwise) */
  float		speed ;		/* scan speed (AOs only; 1.0 otherwise) */
  fint		ndets ;		/* number of detectors */
  fint	 	rate ;	 	/* sample rate */
  fint		datlen ;	/* length of dat .. se */
} scan_id_type ; 

#define SCAN_ID_INIT( id ) \
	id = (scan_id_type*)malloc( sizeof( scan_id_type ) ) ; \
	assert( id ) 
#define SCAN_ID_FREE( id )	FREE( id )

typedef struct {
  fint		det ; 		/* IRAS detector number */
  float		cal[MAXCAL] ;	/* parameters of the stripe fit */
  int		ncal ;		/* power of the stripe */
  float 	timref ; 	/* zeropoint of slope in stripe */
  float 	scale ; 	/* standard deviation of fit */
} stripe_type ;

#define STRIPE_INIT( stripe ) \
	stripe = (stripe_type*)malloc( sizeof( stripe_type ) ) ; \
	assert( stripe )
#define STRIPE_FREE( stripe )	FREE( stripe )

typedef struct {
  float		*dat ;		/* intensities of the sample */
  float		noise ;		/* noise level in this detector snip */
  int		*flag ;		/* flag: OK=0, SOURCE=1, TAIL=2, SPIKE=4 */
  double	*x ;		/* projected x-position (horizontal) */
  double	*y ;		/* projected y-position (vertical) */
  float		*ce ;		/* cosine of projected twist angle */
  float		*se ;		/* sine of projected twist angle */
} sample_type ;

#define SAMPLE_INIT( sample, n ) \
		FLOAT_INIT( sample.dat, n ) ;\
		INT_INIT(  sample.flag, n ) ;\
		DOUBLE_INIT( sample.x, n ) ;\
		DOUBLE_INIT( sample.y, n ) ;\
		FLOAT_INIT( sample.ce, n ) ;\
		FLOAT_INIT( sample.se, n ) 
#define SAMPLE_FREE( sample )	\
		FREE( sample.dat ) ;\
		FREE( sample.flag) ;\
		FREE( sample.x   ) ;\
		FREE( sample.y   ) ;\
		FREE( sample.ce  ) ;\
		FREE( sample.se  )

typedef struct {
  scan_id_type	id ;		/* identification */
  stripe_type	stripe ;	/* destripe parameters */
  sample_type	sample ;	/* array of samples */
} scan_type ;

#define SCAN_INIT( scan ) \
	scan = (scan_type*)malloc( sizeof( scan_type ) ) ;\
	assert( scan )
#define SCAN_FREE( scan)	SAMPLE_FREE( scan->sample ) ;\
				STRIPE_FREE( scan->stripe ) ;\
				SCAN_ID_FREE( scan->id ) ; FREE( scan )

#undef FLOAT_INIT
#undef INT_INIT
#undef DOUBLE_INIT
#undef FREE

int irim_checkbphf(
        fchar   ,			/* irds */
        fint    ) ;			/* snipnr */
int irim_flagmask( 
	int ) ;				/* default flagmask */
void irim_flags(
	fchar,				/* instrument */
	scan_type* );			/* scan 	*/
int irim_rdsnip(			/* succes ? 	*/
	fchar,				/* irds name	*/
	fint,				/* snip number	*/
	fint,				/* seq det number 	*/
	fint,				/* coor nr of plate system */
	fint,				/* projection type */
	scan_type * );			/* scan		*/
