/* tracks

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>		tracks.dc1

Program:	tracks

Purpose:	Plots the track of a number of snips for a number
		of detectors.

Category:	IRAS

File:		tracks.c

Author: 	Fred Lahuis

Keywords:

	first the inputs on what should be displayed

    **IRSET=/IRDS=
		IRDS to be displayed.

(**)SNIP=	Snips to be displayed if none are		[all]
		entered via subsets.

(**)SDET=	Detectors to display if none are		  [0]
		entered via subsets.
		The default (0) corresponds to boresight.
		The boresight (0) cannot be entered via subsets,
		so if the boresight is wanted along with one or
		more detectors it has to be entered with SDET.

    COOR=	Coordinate system.			  [IRDS-coor]

    PROJ=	Projection type.			     [GNOMON]
		Entering an unknown type gives a list of possible
		projection types.

    POS=	Positions to be plotted, in		       [none]
		the coordinate system given at COOR.

  **CENTRE=/CENTER=
		lon & lat of plot center.		[IRDS-center]
		If a position is entered by the user it must be
		given in the coordinate system as given by COOR.

  **PRCEN=	Projection center.			[plot centre]

  **SIZE=	lon & lat size of the irds plate	  [IRDS-size]
		to be plotted, with respect to the plot centre.

	inputs on what the physical plot should look like

    GRDEVICE=	Graphics device.		     [list of options]

  **BOX=	The area out of the irds-plate		       [N/[Y]]
		at the chosen plot centre and size is drawn in the
		plot.

  **CHARSIZE=	Character sizes of the different	    [defaults]
		items in the plot.
		The defaults are given in the output area.
		The entries are in order:
			axis labels, coordinate labels,
			track labels, IR_Gipsy vignet,
			program id., info text.

  **FIRSTPOS=	The value of the first coordinate tick (the coordinate
		with the lowest value). The coordinates should be entered
		as e.g. 3d15m30 or 3h15m30.
		If one value is entered it is used as the first position
		for both x-axis. If two are entered the first is used
		for both x-axes and the second for both y-axes. If three
		are entered the last is used for both y-axes.

  **LINEW=	Width of the lines in units of pgplot's        [1,1,1]
		default linewidth. The first entry is used for the box,
		the second for the tracks and the last for all
		printed text.

  **LOOP=	Default TRACKS loops over	       [sdet/snip/no]
		the detectors if more than one snip is given,
		or one snip and one detector, i.e. that for each
		detector all given snips are plotted in a separate
		plot.
		If one snip is given and more than one detector
		the detectors for the snip are plotted, unless
		`sdet` is given as the argument for LOOP.
		If `sdet` is entered all snips are plotted for each
		detector in a single plot.
		If `snip` is entered the given detectors are plotted
		for each snip.
		IF `no` is entered all tracks for all given snips and
		detectors are plotted in one single plot.

  **NTICK=	Number of coordinate ticks for each	[4,4,4,4]
		axis. The first two are for the both longitude axes
		and the last two for both latitude axes.
		If two values are entered the first is assumed to
		be for the longitude axes and the second for both
		latitude axes.


  **PLOTEDGE=	Offset in cm of the lower and left axis with respect
		to the sides of the plot.

  **PLSIZE=	Size of the plot in centimeters.	     [default]
		The size corresponds to the area defined by SIZE,
		which is plotted as a dotted box if BOX=y.
		If a value is given here SCALE has no effect.

  **PROMPT=	Ask the user to plot the next page every time	N/[Y]
		during a loop (NEXTPAGE=).
		This keyword can be entered and thus changed at any
		time during a loop.
		See also LOOP.

(**)SCALE=	Scale of the plot in			 ['lon','lat']
		arcseconds per millimeter for longitude and latitude
		in the plate coordinate system.
		This keyword is not asked and has no effect if a
		physical size for the plot has already been given
		with PLSIZE.

  **TICK=	Separation between ticks (in the chosen coordinate
		system). The calculated defaults are shown in the
		output area. The separation should be entered
		as e.g. 3d15m30 or 3h15m30 or 3.5d or 3d10.5m.

  **VIGNET=	Plot an IR-Gipsy vignet.		       [Y/(N)]

  **FILENAME=	Give name for (CPLOT) recall file:	     [No file]
		Store start and end points (in degrees)
		of the plotted tracks in a file. This Ascii file can
		be used as a recall file for the CPLOT keyword PLLINE=
		CPLOT does not clip lines, so let TRACKS take care of
		the the right center (CENTER=) and size (SIZE=).

Description:

Comments:	Coordinates:
		      If an area near the pole is used, the plotted
		      coordinates are not to be trusted.
		Inputs:
		      Details on the input format of physical
		      coordinates can be found in the documentation of
		      the subroutine userangle.
		Track numbering:
		      The snip and or sdet numbers are plotted at the
		      start of each track.

Updates:	Jan  8 1992: FL  Document created.
		Sep 29 1992: FL  Minor changes and update documentation.
		Nov 13 1992: FL  Coordinate labeling highly improved.
		Jan 20 1993: VOG Keyword FILENAME= added.
		Mar  9 1993: FL  Correct direction longitude coordinates.
		Sep 17 1993: FL  Bug in coordinate plotting on HP's
				 localized and killed.
		Apr 14 1994: FL  HP bug fixed.
		Jul 20 1999: DK  Cstring[12] extended to 1000; they were
				 too short. Thanks Tin Kam Ho
#<
*/

#include "stdio.h"
#include "stdlib.h"
#include "gipsyc.h"
#include "cmain.h"
#include "string.h"
#include "math.h"
#include "limits.h"

#include "srvreq.h"
#include "userfio.h"
#include "userangle.h"
#include "userint.h"
#include "userreal.h"
#include "usertext.h"
#include "userlog.h"
#include "userchar.h"
#include "usercharu.h"
#include "gdsc_grid.h"
#include "gdsc_word.h"
#include "gdsd_rint.h"
#include "gdsinp.h"

#include "assert.h"
#include "nelc.h"
#include "hms.h"
#include "dms.h"

#include "irco.h"
#include "irco_deproject.h"
#include "ircc_bandnr.h"
#include "irds_basic.h"
#include "irds_rd_detpos.h"
#include "irus_coor.h"
#include "irpl_corners.h"

#include "pgask.h"
#include "pgbeg.h"
#include "pgbox.h"
#include "pgend.h"
#include "pgerrx.h"
#include "pgerry.h"
#include "pglab.h"
#include "pgline.h"
#include "pgmtxt.h"
#include "pgpage.h"
#include "pgpnts.h"
#include "pgqwin.h"
#include "pgsch.h"
#include "pgswin.h"
#include "pgvsiz.h"
#include "pgwnad.h"
#include "pgqinf.h"
#include "pgmove.h"
#include "pgdraw.h"
#include "pgptxt.h"
#include "pgqcf.h"
#include "pgqch.h"
#include "pgsch.h"
#include "pgscf.h"
#include "pgsls.h"
#include "pgslw.h"
#include "pgqlw.h"
#include "pgqvp.h"
#include "pgiden.h"

#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "gds_close.h"

#define  VERSION     "2.3"
#define  PROGRAM     "TRACKS"

#define IN_KEY		tofchar("IRDS=")
#define IN_MES		tofchar("IRDS set to display")
#define IR_KEY		tofchar("IRSET=")
#define IR_MES		tofchar("IRDS set to display")
#define BOX_KEY 	tofchar("BOX=")
#define BOX_MES 	tofchar("Plot a box around requested area: [N]/Y")
#define SNIP_KEY	tofchar("SNIP=")
#define SDET_KEY	tofchar("SDET=")
#define CENTRE_KEY	tofchar("CENTRE=")
#define CENTRE_MES	tofchar("lon & lat of plot centre [IRDS-centre]")
#define CENTER_KEY	tofchar("CENTER=")
#define CENTER_MES	tofchar("lon & lat of plot center [IRDS-center]")
#define SIZE_KEY	tofchar("SIZE=")
#define SIZE_MES	tofchar("lon & lat size of plot [IRDS-size]")
#define COOR_KEY	tofchar("COOR=")
#define COOR_MES	tofchar("Coordinate system [IRDS-coor]")
#define LOOP_KEY	tofchar("LOOP=")
#define LOOP_MES	tofchar("Loop over detectors or snips (sdet/snip)")
#define PROJ_KEY	tofchar("PROJ=")
#define PROJ_MES	tofchar("Projection type [GNOMON]")
#define PRCEN_KEY	tofchar("PRCEN=")
#define PRCEN_MES	tofchar("Projection centre [plot centre]")
#define SCALE_KEY	tofchar("SCALE=")
#define PLSIZE_KEY	tofchar("PLSIZE=")
#define PLSIZE_MES	tofchar("Size of the plot in cm. [12.5,12.5]")
#define PLEDGE_KEY	tofchar("PLOTEDGE=")
#define PLEDGE_MES	tofchar("Offset of the plot with respect to left and bottem side")
#define CHARS_KEY	tofchar("CHARSIZE=")
#define CHARS_MES	tofchar("Scaling of characters for labels/info [1,1]")
#define LINEW_KEY	tofchar("LINEW=")
#define LINEW_MES	tofchar("Linewidth for lines/text [1,1]")
#define VIG_KEY 	tofchar("VIGNET=")
#define VIG_MES 	tofchar("Plot an IR-Gipsy vignet [Y/(N)]")
#define POS_KEY 	tofchar("POS=")
#define POS_MES 	tofchar("Give positions to be plotted [stop]")
#define NTICK_KEY	tofchar("NTICK=")
#define NTICK_MES	tofchar("Number of coord. points [4,4]")
#define TICK_KEY	tofchar("TICK=")
#define TICK_MES	tofchar("Seperation between ticks")
#define FPOS_KEY	tofchar("FIRSTPOS=")
#define FPOS_MES	tofchar("Give the first position of each axis")
#define PROMPT_KEY	tofchar("PROMPT=")
#define PROMPT_MES	tofchar("Prompt for the next page Y/[N]")
#define FILENAME_KEY	tofchar("FILENAME=")
#define FILENAME_MES	tofchar("Give name for (CPLOT) recall file:           [No file]")


#define ECL		3			/* ecliptic coor system */
#define MISSION 	1983.5
#define PI		3.141592653589793
#define D2R		0.01745325292		/* degree to radian	*/

#define R2D		57.29578		/* radian to degree	*/
#define INCH		2.54
#define PLATESYS	"customplate_"		/* coordinate name of plate */
#define MAXTXTLEN	250
#define MAXSUB		1000
#define MAXPOS		500

static fint	none = 0, request = 1, request_exact = 5 ;
static fint	hidden = 2, exact_hidden = 6 ;
#define NONE		(&none)
#define REQUEST 	(&request)
#define REQUEST_EXACT	(&request_exact)
#define HIDDEN		(&hidden)
#define EXACT_HIDDEN	(&exact_hidden)
#define NO_GRIDS	1000			/* subdivision of axis for determining axis labels */
#define MAX_LABELS	25

static fint	TEST = 16 ;
#define testing(a)	anyout_c( &TEST, tofchar((a)) )
#define anyout(a)	anyout_c( &device, tofchar((a)) )
#define anyout_f(a)	anyout_c( &device, a )
#define anyoutC 	anyout_c( &device, tofchar(Cstring) )
#define max(a,b)	( ( (a) > (b) ) ? (a) : (b) )
#define min(a,b)	( ( (a) < (b) ) ? (a) : (b) )
#define nint(a) 	( (int)( (a) + 0.5 ) )
#define fmake(fchr,size){ \
			static char buff[size+1] ; \
			int i ; \
			for( i = 0 ; i < size ; buff[i++] = ' ') ; \
			buff[i] = 0 ; \
			fchr.a = buff ; \
			fchr.l = size ; \
			}

typedef struct{
	fint	pos_no ;
	float	pos_x[MAXPOS] ;
	float	pos_y[MAXPOS] ;
	fint	steps ;
	float	box_x[4*MAXPOS] ;
	float	box_y[4*MAXPOS] ;
	float	centre[2] ;
}pos_type ;
#define POS_INIT( pos ){ \
		pos = (pos_type*)malloc( sizeof(pos_type) ) ; \
		assert(pos) ; \
		pos->pos_no = 0 ; \
		pos->steps = MAXPOS ; \
	}
typedef struct {
	fchar	object ;	/* name of the map */
	fchar	instrume ;	/* instrument identification */
	fchar	coor_name ;
	fchar	proj_name ;
	fchar	plate_name ;
	int	equat ;
	float	epoch ;
	double	xcorner[4] ;
	double	ycorner[4] ;
	fint	coor ;		/* coordinate type number given by user */
	fint	plate ; 	/* coordinate type number of the plate system */
	fint	proj ;		/* projection type number */
	double	centre[2] ;	/* centre of the map		   [degree] */
	double	size[2] ;	/* length of the area		   [degree] */
	double	prcentre[2] ;	/* projected coordinates of the centre */
}image_type ;
#define IMAGE_INIT( image ){ \
	image = (image_type*)malloc( sizeof( image_type ) ) ; \
	assert( image ) ; \
	fmake( image->plate_name, MAXTXTLEN ) ; \
	fmake( image->proj_name, MAXTXTLEN ) ; \
	fmake( image->coor_name, MAXTXTLEN ) ; \
	fmake( image->object, MAXTXTLEN ) ; \
	fmake( image->instrume, MAXTXTLEN ) ; \
}
typedef struct {
	fchar	      name ;
	fchar	      coor_name ;
	fint	      coor ;	      /* coordinate type number of irds */
	fchar	      plate_name ;
	fint	      plate ;
	fint	      axis[4] ;
	double	      centre[2] ;     /* centre of the irds	    [degree] */
	double	      size[2] ;       /* length of the sizes	    [degree] */
	double	      prcentre[2] ;   /* projected coordinates of the centre */
} irds_type ;
#define SET_INIT( irds ) { \
	irds = (irds_type*)malloc( sizeof( irds_type ) ) ;\
	assert( irds ) ;\
	fmake( irds->name, MAXTXTLEN ) ;\
	fmake( irds->plate_name, MAXTXTLEN ) ;\
	fmake( irds->coor_name, MAXTXTLEN ) ;\
}
typedef struct{
	float	edge[4] ;	/* edge around plot in cm */
	float	scale[2] ;	/* scale of plot in arcsec/mm */
	float	size[2] ;	/* size of window in cm */
	double	dsize[2] ;	/*		in degrees */
	float	diff ;		/* seperation between lines in info in perc. of vertical axis */
	float	info_width ;	/* width of the info block */
	float	box[4] ;
	float	area[4] ;
	int	rect ;
	int	first ; 	/* first page? */
	fint	line[3] ;	/* line thickness line[0] for lines and line[1] for text */
}plot_type ;
#define PLOT_INIT(plot){\
	plot = (plot_type*)malloc(sizeof(plot_type)) ; \
	assert( plot ) ;\
	plot->scale[0] = 0.0 ;\
	plot->scale[1] = 0.0 ;\
	plot->first = 1 ; \
	plot->rect = 1 ; \
	plot->info_width = 5.5 ; \
}
typedef struct{ 		/* relative character sizes of different items */
	float	vignet ;	/* IR-Gipsy label */
	float	id ;		/* program name */
	float	info ;		/* info text */
	float	label ; 	/* axis labels */
	float	tracks ;
	float	coor ;		/* coordinates on axes */
	float	def[2] ;	/* scaling of rel. char. sizes */
	float	quot[2] ;	/* scaling by user of chosen defaults */
	float	size ;		/* vertical size of character in cm */
} chars_type ;
#define CHARS_INIT(chars){ \
	chars = (chars_type*)malloc(sizeof(chars_type)) ; \
	assert( chars ) ; \
	chars->info = 1.0 ; chars->id = 2.0 ; chars->vignet = 1.35 ; \
	chars->label = 1.75 ; chars->coor = 1.0 ; chars->tracks = 1.0 ; \
	chars->def[0] = 1.0 ; \
	chars->def[1] = 1.0 ; \
	chars->quot[0] = 1.0 ; \
	chars->quot[1] = 1.0 ; \
}
typedef struct{
	fchar	x, y ;
	double	xpos[8], ypos[8] ;
	fchar	coorx1, coorx2, coorx3, coorx4, coorx5, coorx6 ;
	fchar	coory1, coory2, coory3, coory4, coory5, coory6 ;
	fchar	vignet ;
	fchar	prog_id ;
}lab_type ;
#define LAB_INIT(lab){\
	lab = (lab_type*)malloc(sizeof(lab_type)) ;\
	assert(lab) ;\
	fmake(lab->coorx1, 30 ) ; fmake(lab->coorx2, 30 ) ; fmake(lab->coorx3, 30 ) ;\
	fmake(lab->coorx4, 30 ) ; fmake(lab->coorx5, 30 ) ; fmake(lab->coorx6, 30 ) ;\
	fmake(lab->coory1, 30 ) ; fmake(lab->coory2, 30 ) ; fmake(lab->coory3, 30 ) ;\
	fmake(lab->coory4, 30 ) ; fmake(lab->coory5, 30 ) ; fmake(lab->coory6, 30 ) ;\
	fmake(lab->x, 8 ) ; fmake(lab->y, 8 ) ;\
	fmake(lab->vignet, 8 ) ;\
	fmake(lab->prog_id, 13 ) ;\
	strncpy(lab->vignet.a, "IR-Gipsy", 8 ) ;\
	sprintf( lab->prog_id.a, "%s v. ", PROGRAM ) ;\
	strcat( lab->prog_id.a, VERSION ) ;\
}

static fint	nul = 0, one = 1, two = 2, three = 3, four = 4, six = 6 ;
static fint	device = 11 ;
static fint	k, n, no, nr ;
static char	Cstring[1000], Cstring2[1000] ;
static fchar	string, string2 ;
static float	fnul = 0.0 ;
static double	array[3] ;
FILE		*fp;

typedef struct	{
		double	xpos ;
		double	ypos ;
		double	pos ;
		char	lab[30] ;
}label_type ;

void plot_labels(	image_type	*image,
			plot_type	*plot,
			chars_type	*chars,
			label_type	*label,
			fint		*no_ticks
		)
{
float	disp, coord, fjust ;
float	x[2], y[2] ;
float	ticksize = 0.015 ;
double	boxsize[2] ;

/** write the coordinates alongside the axes **/
	boxsize[0] = plot->box[1] - plot->box[0] ;
	boxsize[1] = plot->box[3] - plot->box[2] ;
	pgslw_c( &(plot->line[2]) ) ;
	pgsch_c( &chars->coor ) ;
	disp = 1.25 ; fjust = 0.5 ;
	anyoutf( TEST, "x_labels") ;
	for( no = 0 ; no < no_ticks[0] ; no++ ){
		testing(label[no].lab) ;
/** x axis is reversed **/
		coord = (plot->box[1] - label[no].xpos)/boxsize[0] ;
		pgmtxt_c( tofchar("B"), &disp, &coord, &fjust, tofchar(label[no].lab) ) ;
		x[0] = x[1] = label[no].xpos ;
		y[0] = plot->box[2] ; y[1] = y[0] + ticksize*boxsize[1] ;
		pgline_c( &two, x, y ) ;
	}
	disp = 1.0 ;
	for( ; no < no_ticks[1] ; no++ ){
		testing(label[no].lab) ;
		coord = (plot->box[1] - label[no].xpos)/boxsize[0] ;
		pgmtxt_c( tofchar("T"), &disp, &coord, &fjust, tofchar(label[no].lab) ) ;
		x[0] = x[1] = label[no].xpos ;
		y[0] = plot->box[3] ; y[1] = y[0] - ticksize*boxsize[1] ;
		pgline_c( &two, x, y ) ;
	}
	disp = 1.0 ; fjust = 1.0 ;
	testing("y_labels 1") ;
	for( ; no < no_ticks[2] ; no++ ){
		testing(label[no].lab) ;
		coord = ( label[no].ypos - plot->box[2] )/boxsize[1] ;
		pgmtxt_c( tofchar("LV"), &disp, &coord, &fjust, tofchar(label[no].lab) ) ;
		x[0] = plot->box[0] ; x[1] = x[0] + ticksize*boxsize[0] ;
		y[0] = y[1] = label[no].ypos ;
		pgline_c( &two, x, y ) ;
	}
	testing("y_labels 2") ;
	disp = 1.0 ; fjust = 0.0 ;
	for( ; no < no_ticks[3] ; no++ ){
		testing(label[no].lab) ;
		coord = (label[no].ypos-plot->box[2])/boxsize[1] ;
		pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust, tofchar(label[no].lab) ) ;
		x[0] = plot->box[1] ; x[1] = x[0] - ticksize*boxsize[0] ;
		y[0] = y[1] = label[no].ypos ;
		pgline_c( &two, x, y ) ;
	}

	return ;
}

void plot_coor( image_type	*image,
		plot_type	*plot,
		chars_type	*chars
		)
{
double	step_in[] = {	10.0, 5.0, 3.0, 4.0, 2.0, 1.5, 1.0,
			50.0/60.0  , 40.0/60.0	, 30.0/60.0  , 20.0/60.0  ,
			15.0/60.0,   10.0/60.0	, 5.0/60.0  ,
			4.0/60.0   , 3.0/60.0	, 2.0/60.0   , 1.5/60.0   ,
			1.0/60.0   ,
			50.0/3600.0, 40.0/3600.0, 30.0/3600.0, 20.0/3600.0,
			10.0/3600.0, 5.0/3600.0,
			0.0 } ;
int	gridsep[4] ;
int	no_old, no, no_firstpos ;

fint	no_lab = 0, grids = NO_GRIDS ;
fint	no_steps = 0 ;

double	*step ;
double	lon[NO_GRIDS], lat[NO_GRIDS], xyz[3*NO_GRIDS] ;
double	dsize[4], ticksep[] = { 0.0, 0.0, 0.0, 0.0 } ;
double	firstpos[4], tickpos ;
double	corner[2][4] ;

fint	firsttick = 0 ;
static	fint	no_ticks[] = {4,4,4,4} ;
static	label_type	 label[10*MAX_LABELS] ;

fchar	Fstring ;

	fmake( Fstring, MAXTXTLEN ) ;

	if( plot->first ){
		while( step_in[no_steps] != 0.0 ) no_steps++ ;
		step = malloc( no_steps*sizeof(double) ) ;

		lon[0] = plot->box[0] ; lon[1] = plot->box[1] ;
		lon[2] = plot->box[0] ; lon[3] = plot->box[1] ;
		lat[0] = plot->box[2] ; lat[1] = plot->box[2] ;
		lat[2] = plot->box[3] ; lat[3] = plot->box[3] ;
		irco_deproject_c( &image->proj, lon, lat, xyz, &four ) ;
		irco_transform_c( xyz, &image->plate, xyz, &image->coor, &four ) ;
		irco_tospher_c( xyz, lon, lat, &four ) ;
		for( no = 0 ; no < 4 ; no++ )
			corner[0][no] = fmod( (lon[no]/D2R+360.0),360.0 ) ;
		for( no = 0 ; no < 4 ; no++ )
			corner[1][no] = lat[no]/D2R ;

		lon[0] = plot->area[0] ; lon[1] = plot->area[1] ;
		lon[2] = plot->area[0] ; lon[3] = plot->area[1] ;
		lat[0] = plot->area[2] ; lat[1] = plot->area[2] ;
		lat[2] = plot->area[3] ; lat[3] = plot->area[3] ;
		irco_deproject_c( &image->proj, lon, lat, xyz, &four ) ;
		irco_transform_c( xyz, &image->plate, xyz, &image->coor, &four ) ;
		irco_tospher_c( xyz, lon, lat, &four ) ;
		for( no = 0 ; no < 4 ; no++ ){
			lon[no] = fmod( (lon[no]/D2R+360.0),360.0 ) ;
			lat[no] /= D2R ;
		}
/* the sizes of the axes are determined in the chosen coordinate system */
/* these are used to determine the spacing between the labels */
		dsize[0] = fabs(lon[1] - lon[0]) ;
		dsize[1] = fabs(lon[3] - lon[2]) ;
		dsize[2] = fabs(lat[2] - lat[0]) ;
		dsize[3] = fabs(lat[3] - lat[1]) ;
		anyoutf( TEST, "	axes sizes: %.2f %.2f %.2f %.2f",
				dsize[0], dsize[1], dsize[2], dsize[3] ) ;

		no = userangle_c( ticksep, &four, HIDDEN, TICK_KEY, TICK_MES ) ;
		switch( no ){
		case 1: for( n = no ; n < 4 ; n++ ) ticksep[n] = ticksep[0] ;
			for( n = 0 ; n < 4 ; n++ )
				no_ticks[n] = (int)(dsize[n]/ticksep[n]) + 1 ;
			break ;
		case 2: ticksep[3] = ticksep[1] ;
			ticksep[2] = ticksep[3] ;
			ticksep[1] = ticksep[0] ;
			for( n = 0 ; n < 4 ; n++ )
				no_ticks[n] = (int)(dsize[n]/ticksep[n]) + 1 ;
			break ;
		case 3: ticksep[3] = ticksep[2] ;
			for( n = 0 ; n < 4 ; n++ )
				no_ticks[n] = (int)(dsize[n]/ticksep[n]) + 1 ;
			break ;
		case 4: break ;
		case 0: no = userint_c( no_ticks, &four, HIDDEN, NTICK_KEY, NTICK_MES ) ;
			if( no == 1 ) for( ; no < 4 ; no++ ) no_ticks[no] = no_ticks[0] ;
			if( no == 2 ){
				no_ticks[3] = no_ticks[1] ;
				no_ticks[2] = no_ticks[1] ;
				no_ticks[1] = no_ticks[0] ;
			}
			if( no == 3 ){
				no_ticks[3] = no_ticks[2] ;
			}
			break ;
		default:break ;
		}
		for( n = 0 ; n < 4 ; n++ ) no_ticks[n] = min( no_ticks[n], MAX_LABELS ) ;

		if( ticksep[0] == 0.0 ){						/* determine the seperartion between the tickmarcks */
			for( k = 0 ; k < 4 ; k++ ){
				if( k < 2 && image->equat )
					for( n = 0 ; n < no_steps ; n++ )
						step[n] = 15.0*step_in[n] ;
				else	for( n = 0 ; n < no_steps ; n++ )
						step[n] = step_in[n] ;
				for( n = 0 ; n < no_steps ; n++ ){
					if( 0.99*dsize[k] > (no_ticks[k]-1)*step[n]){
						ticksep[k] = step[n] ;
						break ;
					}
				}
			}
		}
		for( k = 0 ; k < 4 ; k++ ){
			if( ticksep[k] == 0.0 ){
				ticksep[k] = dsize[k]/no_ticks[k] ;
				gridsep[k] = nint( NO_GRIDS/(no_ticks[k]-1) ) ;
			}
			else gridsep[k] = nint( NO_GRIDS*ticksep[k]/dsize[k] ) ;
		}
		sprintf( Cstring, "     seperation between ticks %.2f %.2f %.2f %.2f degrees",
			ticksep[0], ticksep[1], ticksep[2], ticksep[3] ) ;
		anyoutC ;

		no_firstpos = userangle_c( firstpos, &four, HIDDEN, FPOS_KEY, FPOS_MES ) ;
		switch(no_firstpos){
		case 4: break ;
		case 3: firstpos[3] = firstpos[2] ;
			no_firstpos = 4 ;
			break ;
		case 2: firstpos[3] = firstpos[2] = firstpos[1] ;
			firstpos[1] = firstpos[0] ;
			no_firstpos = 4 ;
			break ;
		case 1: firstpos[1] = firstpos[0] ;
			no_firstpos = 2 ;
			break ;
		default:break ;
		}
												/* get the positions on the x axis */
		status_c( tofchar("getting positions on x-axis") ) ;
		if( image->equat ) for( n = 0 ; n < no_steps ; n++ ) step[n] = 15.0*step_in[n] ;
		else for( n = 0 ; n < no_steps ; n++ ) step[n] = step_in[n] ;
		no_lab = 0 ;
		for( k = 0 ; k < 2 ; k++ ){
			no_old = no_lab ;
			lat[0] = ( k == 0 ) ? plot->box[2] : plot->box[3] ;
			for( n = 0 ; n < NO_GRIDS ; n++ ){
				lon[n] = plot->area[0] +
					n * (plot->area[1]-plot->area[0])
							/
						(float)(NO_GRIDS-1) ;
				lat[n] = lat[0] ;
			}
			irco_deproject_c( &image->proj, lon, lat, xyz, &grids ) ;
			irco_transform_c( xyz, &image->plate, xyz, &image->coor, &grids ) ;
			irco_tospher_c( xyz, lon, lat, &grids ) ;
			anyoutf( TEST, "grids %d", grids ) ;
			for( no = 0 ; no < grids ; no++ ){
				lon[no] /= D2R ;
				lon[no] = fmod(lon[no]+360.0,360.0) ;
				lat[no] /= D2R ;
			}
			anyoutf( TEST, "min max %.2f %.2f", lon[0], lon[grids-1] ) ;
			if( !no_firstpos  ){

				firsttick =  (int)( 0.5 *
						( NO_GRIDS -
						  (no_ticks[k]-1)*gridsep[k]
						) ) ;
				firsttick = max(firsttick,1) ;
				sprintf( Cstring, "firsttick %d", firsttick ) ;
				testing(Cstring) ;
				tickpos = lon[firsttick-1] ;
				if( image->equat ) hms_c( &tickpos, Fstring, NULL, &one, &nul ) ;
				else dms_c( &tickpos, Fstring, NULL, &one, &nul ) ;
				anyout_c( &TEST, Fstring ) ;
				no = 0 ;
				do{
					firstpos[k] = step[no] * (
						nint( tickpos/step[no] )
						) ;
				}while( fabs(firstpos[k] - tickpos) > 0.05 * dsize[k] && ++no < no_steps ) ;
				if( image->equat ) hms_c( &firstpos[k], Fstring, NULL, &nul, &nul ) ;
				else dms_c( &firstpos[k], Fstring, NULL, &nul, &nul ) ;
				anyout_c( &TEST, Fstring ) ;
			}
/*
			firsttick = 0 ;
			while( lon[firsttick] < label[no_lab].xpos &&
				firsttick < grids-1 ) firsttick++ ;
*/
			for( no = 0 ; no < no_ticks[k] ; no++ ){
				label[no_lab].xpos = firstpos[k] + no*ticksep[k] ;
				label[no_lab].ypos = lat[firsttick+no*gridsep[k]] ;
				anyoutf( TEST, "%.2f %.2f",
					label[no_lab].xpos,
					label[no_lab].ypos ) ;
				no_lab++ ;
			}

/** if no or one label is found **/
			if( no_lab < no_old+2 ){
				testing("not enough x labels found") ;
				ticksep[k] = (plot->area[1]-plot->area[0])/(no_ticks[k]-1) ;
				lon[0] = plot->area[0] ;
				lat[0] = (k == 0) ? plot->box[2] : plot->box[3] ;
				for( n = 1 ; n < no_ticks[k] ; n++ ){
					lon[n] = lon[n-1] + ticksep[k] ;
					lat[n] = lat[0] ;
				}
				irco_deproject_c( &image->proj, lon, lat, xyz, &no_ticks[k] ) ;
				irco_transform_c( xyz, &image->plate, xyz, &image->coor, &no_ticks[k] ) ;
				irco_tospher_c( xyz, lon, lat, &no_ticks[k] ) ;
				for( no_lab = no_old, n = 0 ; n < no_ticks[k] ; n++ ){
					label[no_lab].xpos = fmod(lon[n]/D2R+360.0,360.0) ;
					label[no_lab].ypos = lat[n]/D2R ;
					no_lab++ ;
				}
			}
			no_ticks[k] = no_lab ;
		}

/** get the positions on the y axis **/
		status_c( tofchar("getting positions on y-axis") ) ;
		testing("getting positions on y-axis") ;
		for( n = 0 ; n < no_steps ; n++ ) step[n] = step_in[n] ;
		for( k = 2 ; k < 4 ; k++ ){
			no_old = no_lab ;
			lon[0] = ( k == 2 ) ? plot->box[0] : plot->box[1] ;
			for( n = 0 ; n < NO_GRIDS ; n++ ){
				lat[n] = plot->area[2] +
					n*(plot->area[3]-plot->area[2])
						/
					(double)(NO_GRIDS-1) ;
				lon[n] = lon[0] ;
			}

			irco_deproject_c( &image->proj, lon, lat, xyz, &grids ) ;
			irco_transform_c( xyz, &image->plate, xyz, &image->coor, &grids ) ;
			irco_tospher_c( xyz, lon, lat, &grids ) ;
			for( no = 0 ; no < grids ; no++ ){
				lon[no] /= D2R ;
				lon[no] = fmod(lon[no]+360.0,360.0) ;
				lat[no] /= D2R ;
			}

			if( no_firstpos != 4 ){
				firsttick =  (int)( 0.5 *
						( NO_GRIDS -
						  (no_ticks[k]-1)* gridsep[k]
						) ) ;
				firsttick = max(firsttick,1) ;
				sprintf( Cstring, "firsttick %d", firsttick ) ;
				testing(Cstring) ;

				tickpos = lat[firsttick-1] ;
				dms_c( &tickpos, Fstring, NULL, &nul, &nul ) ;
				anyout_c( &TEST, Fstring ) ;
				no = 0 ;
				do{
					firstpos[k] = (double)(
						nint(tickpos/step[no])
						) ;
					firstpos[k] *= step[no] ;
				}while( fabs(firstpos[k] - tickpos) > 0.05* dsize[k] && ++no < no_steps ) ;
				dms_c( &firstpos[k], Fstring, NULL, &nul, &nul ) ;
				anyout_c( &TEST, Fstring ) ;
			}
			for( n = 0 ; n < no_ticks[k] ; n++ ){
				label[no_lab].ypos = firstpos[k] + n*ticksep[k] ;
				label[no_lab].xpos = lon[firsttick+n*gridsep[k]] ;
				anyoutf( TEST, "%.2f %.2f",
					label[no_lab].xpos,
					label[no_lab].ypos ) ;
				no_lab++ ;
			}

			if( no_lab < no_old+2 ){
				testing("Not enough y label points found") ;

				ticksep[k] = (plot->area[3]-plot->area[2])/(no_ticks[k]-1) ;
				lat[0] = plot->area[2] ;
				lon[0] = (k == 2) ? plot->box[0] : plot->box[1] ;
				for( n = 1 ; n < no_ticks[k] ; n++ ){
					lon[n] = lon[n-1] ;
					lat[n] = lat[n-1] + ticksep[k];
				}
				irco_deproject_c( &image->proj, lon, lat, xyz, &no_ticks[k] ) ;
				irco_transform_c( xyz, &image->plate, xyz, &image->coor, &no_ticks[k] ) ;
				irco_tospher_c( xyz, lon, lat, &no_ticks[k] ) ;
				for( no_lab = no_old, n = 0 ; n < no_ticks[k] ; n++ ){
					label[no_lab].xpos = lon[n] ;
					label[no_lab].ypos = lat[n] ;
					anyoutf( TEST, "%.2f %.2f", lon[n], lat[n] ) ;
					no_lab++ ;
				}
			}
			no_ticks[k] = no_lab ;
		}

/** getting the label textstrings **/
		anyoutf( TEST, "Getting textstrings for the labels" ) ;
		for( no = 0 ; no < no_ticks[3] ; no++ ){
			if( no < no_ticks[1] ){
				anyoutf( TEST, "%.2f %.2f",
					label[no].xpos,
					label[no].ypos ) ;
				if( image->equat ) hms_c( &label[no].xpos, Fstring, NULL, &nul, &one ) ;
				else dms_c( &label[no].xpos, Fstring, NULL, &nul, &one ) ;
			}
			else{
				dms_c( &label[no].ypos, Fstring, NULL, &nul, &one ) ;
				anyoutf( TEST, "%.2f %.2f",
					label[no].xpos,
					label[no].ypos ) ;
			}
			strncpy( label[no].lab, Fstring.a, nelc_c(Fstring) ) ;
		}
/* get the label positions in the projected plate coordinate system */
		for( no = 0 ; no < no_ticks[3] ; no++ ){
			lon[no] = (label[no].xpos)*D2R ;
			lat[no] = (label[no].ypos)*D2R ;
		}
		irco_torect_c( lon, lat, xyz, &no_ticks[3] ) ;
		irco_transform_c( xyz, &image->coor, xyz, &image->plate, &no_ticks[3] ) ;
		irco_project_c( &image->proj, xyz, lon, lat, &no_ticks[3] ) ;
		for( no = 0 ; no < no_ticks[3] ; no++ ){
			label[no].xpos = lon[no] ;
			label[no].ypos = lat[no] ;
		}
	}
	plot_labels( image, plot, chars, label, no_ticks ) ;
	return ;
}

int init_irds(	irds_type	*irds,
		image_type	*image )
{
fint		no1, no2 = 0 ;
int		new_prcentre ;
fint		status = 0, naxis, ecl = ECL ;
float		midmission = MISSION, epoch ;
double		xyz[3] ;
fchar		coor_str ;
double		coord ;

	fmake( coor_str, MAXTXTLEN ) ;

	irco_precess_c( &ecl, &midmission, &ecl ) ;
	irds_enquire_c( irds->name, image->object, image->instrume, &naxis, irds->axis,
			irds->centre, irds->size, irds->coor_name, &epoch, &status ) ;

	anyout("") ;										/* some info for the user */
	sprintf( Cstring, "Header info: " ) ;
	anyoutC ;
	sprintf( Cstring, "     Set %.*s", nelc_c(irds->name), irds->name.a ) ;
	anyoutC ;
	sprintf( Cstring, "     Coordinate system: %.*s (%.1f)", nelc_c(irds->coor_name), irds->coor_name.a, epoch ) ;
	anyoutC ;
	if( !strncmp( irds->coor_name.a, "EQU", 3 ) )hms_c( &(irds->centre[0]), coor_str, NULL, &one, &nul ) ;
	else dms_c( &irds->centre[0], coor_str, NULL, &one, &nul ) ;
	sprintf( Cstring, "     IRDS centre: %.*s", nelc_c(coor_str), coor_str.a ) ;
	dms_c( &irds->centre[1], coor_str, NULL, &one, &nul ) ;
	sprintf( Cstring, "%s %.*s", Cstring, nelc_c(coor_str), coor_str.a ) ;
	anyoutC ;
	sprintf( Cstring, "     IRDS size: %.1fx%.1f deg.", irds->size[0], irds->size[1] ) ;

	irds->coor = irco_number_c( irds->coor_name, &epoch ) ; 				/* coordinate system */
	if ( irds->coor < 0 ){
		k = abs(irds->coor) ;
		irds->coor = 0 ;
		irco_precess_c( &k, &epoch, &irds->coor ) ;
	}
	image->coor = irus_coor_c( &irds->coor, REQUEST, COOR_KEY ) ;
	irco_namepoch_c( &image->coor, image->coor_name, &image->epoch ) ;
	image->equat = strncmp( image->coor_name.a, "EQU", 3 ) ? 0 : 1 ;			/* is the coordinate system equatorial */

	image->centre[0] = irds->centre[0] = irds->centre[0]*D2R ;				/* transform default centre to the chosen coordinate system */
	image->centre[1] = irds->centre[1] = irds->centre[1]*D2R ;
	if( irds->coor != image->coor ){
		irco_torect_c( &image->centre[0], &image->centre[1], xyz, &one ) ;
		irco_transform_c( xyz, &irds->coor, xyz, &image->coor, &one ) ;
		irco_tospher_c( xyz, &image->centre[0], &image->centre[1], &one ) ;
		image->centre[0] = fmod( (image->centre[0]+2*PI), 2*PI ) ;
		anyout("") ;
		sprintf( Cstring, "New coordinate system: %.*s (%.1f)",
			nelc_c(image->coor_name), image->coor_name.a, image->epoch ) ;
		anyoutC ;
	}

	no1 = userangle_c( image->centre, &two, EXACT_HIDDEN, CENTRE_KEY, CENTRE_MES ) ;
	if( !no1 ) no2 = userangle_c( image->centre, &two, EXACT_HIDDEN, CENTER_KEY, CENTER_MES ) ;
	if( no1 || no2 ){
		image->centre[0] *= D2R ;
		image->centre[0] = fmod( (image->centre[0]+2*PI), 2*PI ) ;
		if( no1 == 2 || no2 == 2 ) image->centre[1] *= D2R ;
	}
	coord = image->centre[0]/D2R ;								/* some more info to the user */
	if( image->equat ) hms_c( &coord, coor_str, NULL, &one, &nul ) ;
	else dms_c( &coord, coor_str, NULL, &one, &nul ) ;
	sprintf( Cstring, "     Plot centre: %.*s", nelc_c(coor_str), coor_str.a ) ;
	coord = image->centre[1]/D2R ;
	dms_c( &coord, coor_str, NULL, &one, &nul ) ;
	sprintf( Cstring, "%s %.*s", Cstring, nelc_c(coor_str), coor_str.a ) ;
	anyoutC ;

	image->size[0] = irds->size[0] ; image->size[1] = irds->size[1] ;			/* size of the plot */
	if ( userangle_c( image->size, &two, HIDDEN, SIZE_KEY, SIZE_MES ) == 1 )
		image->size[1] = image->size[0] ;
	image->size[0] *= D2R ; image->size[1] *= D2R ;

	while ( TRUE ) {									/* projection type */
		if( !usercharu_c(image->proj_name, &one, REQUEST, PROJ_KEY, PROJ_MES) )strcpy( image->proj_name.a, "GNOMON" ) ;
		image->proj = irco_prnumber_c( image->proj_name ) ;
		if( !image->proj && strncmp( image->proj_name.a, "NONE", 4 ) ){
			sprintf( Cstring, "Unknown projection type: %s", image->proj_name.a ) ;
			anyoutC ;
			anyout( "Options: NONE STEREO GNOMON AZEQD AZEQA  ORTHO " );
			anyout( "         CYLIND MERCAT SINUS AITOFF CYLEQD" );
			cancel_c( PROJ_KEY );
		}else break ;
	}
	if( image->proj )irco_prname_c( image->proj_name, &(image->proj) ) ;

	image->prcentre[0] = image->centre[0] ; image->prcentre[1] = image->centre[1] ; 	/* centre of projection */
	no = userangle_c( image->prcentre, &two, EXACT_HIDDEN, PRCEN_KEY, PRCEN_MES ) ;
	if( !no )new_prcentre = 0 ;
	else{
		image->prcentre[0] *= D2R ;
		image->prcentre[1] *= D2R ;
		new_prcentre = 1 ;
	}

	image->plate = 0 ;
	if( irds->coor != image->coor && !new_prcentre ){	/* transform def. proj. centre to chosen coor. system */
		irco_torect_c( &image->prcentre[0], &image->prcentre[1], xyz, &one ) ;
		irco_transform_c( xyz, &irds->coor, xyz, &image->coor, &one ) ;
		irco_tospher_c( xyz, &image->prcentre[0], &image->prcentre[1], &one ) ;
	}
	strcpy( image->plate_name.a, "image_" ) ;						/* make a plate system at the projection centre */
	strcat( image->plate_name.a, PLATESYS ) ;
	strncat( image->plate_name.a, image->object.a, nelc_c( image->object ) ) ;
	irco_plate_c( &image->coor, image->prcentre, image->plate_name, &image->plate ) ;

	strcpy( irds->plate_name.a, "irds_" ) ;							/* make an irds plate system at the plot centre */
	strcat( irds->plate_name.a, PLATESYS ) ;
	strncat( irds->plate_name.a, image->object.a, nelc_c( image->object ) ) ;
	irco_torect_c( &image->centre[0], &image->centre[1], xyz, &one ) ;
	irco_transform_c( xyz,&image->coor, xyz, &irds->coor, &one ) ;
	irco_tospher_c( xyz, &irds->prcentre[0], &irds->prcentre[1], &one ) ;
	irco_plate_c( &irds->coor, irds->prcentre, irds->plate_name, &(irds->plate) ) ;

	irco_torect_c( &image->centre[0], &image->centre[1], xyz, &one ) ;			/* project the image centre to a projection centred map  */
	irco_transform_c( xyz, &image->coor, xyz, &image->plate, &one ) ;
	irco_project_c( &(image->proj), xyz, &image->prcentre[0], &image->prcentre[1], &one ) ;

	irco_torect_c( &irds->centre[0], &irds->centre[1], xyz, &one ) ;			/* project the irds centre to a projection centred map	*/
	irco_transform_c( xyz, &irds->coor, xyz, &image->plate, &one ) ;
	irco_project_c( &image->proj, xyz, &irds->prcentre[0], &irds->prcentre[1], &nr ) ;
												/* check whether the requested map is within the limits of the irds  */
	if(	fabs( image->prcentre[0] - irds->prcentre[0] ) + image->size[0] > irds->size[0] ||
		fabs( image->prcentre[1] - irds->prcentre[1] ) + image->size[1] > irds->size[1]
	  ){
		anyout("") ;
		anyout( "(Part of) your area asks for data outside this IRDS" ) ;
	}
	return( 0 ) ;
}

void plot_box(	image_type	*image,
		plot_type	*plot,
		pos_type	*pos )
{
fint	side ;
float	xpos[MAXPOS], ypos[MAXPOS] ;

static	fint	do_box ;

	pgslw_c( &(plot->line[0]) ) ;
	pgsls_c( &four ) ;
	do_box = 1 ;
	no = userlog_c( &do_box, &one, HIDDEN, BOX_KEY, BOX_MES ) ;
	if( !do_box )return ;
	anyoutf( TEST, "corners %.2f %.2f %.2f %.2f",
		image->xcorner[0], image->xcorner[1],
		image->xcorner[2], image->xcorner[3] ) ;
	for( side = 0 ; side < 4 ; side++ ){
		switch( side ){
		case 0 :
			xpos[0] = image->xcorner[0] ;
			ypos[0] = image->ycorner[0] ;
			xpos[1] = image->xcorner[1] ;
			ypos[1] = image->ycorner[1] ;
			pgline_c( &two, xpos, ypos ) ;
			break ;
		case 1 :
			xpos[0] = image->xcorner[1] ;
			ypos[0] = image->ycorner[1] ;
			xpos[1] = image->xcorner[2] ;
			ypos[1] = image->ycorner[2] ;
			pgline_c( &two, xpos, ypos ) ;
			break ;
		case 2 :
			xpos[0] = image->xcorner[2] ;
			ypos[0] = image->ycorner[2] ;
			xpos[1] = image->xcorner[3] ;
			ypos[1] = image->ycorner[3] ;
			pgline_c( &two, xpos, ypos ) ;
			break ;
		case 3 :
			xpos[0] = image->xcorner[3] ;
			ypos[0] = image->ycorner[3] ;
			xpos[1] = image->xcorner[0] ;
			ypos[1] = image->ycorner[0] ;
			pgline_c( &two, xpos, ypos ) ;
			break ;
		}
	}
	pgsls_c( &one ) ;
	return ;
}

void plot_pos(	image_type	*image,
		chars_type	*chars,
		plot_type	*plot,
		pos_type	*pos )
{
double	xyz[3] ;
float	charsize ;
double	posin[2] ;

	if( plot->first ){						/* if first page, get the position info */
		pos->centre[0] = (float)image->prcentre[0] ;		/* centre of the plot in radians */
		pos->centre[1] = (float)image->prcentre[1] ;
		no = 1 ;
		while( no != 0 ){
			no = userangle_c( posin, &two, REQUEST_EXACT, POS_KEY, POS_MES ) ;
			switch( no ){
				case 0 :cancel_c( POS_KEY ) ;
					break ;
				case 1 :status_c( tofchar("please enter both coordinates ") ) ;
					cancel_c( POS_KEY ) ;
					break ;
				case 2 :
					posin[0] *= D2R ;
					posin[1] *= D2R ;
					irco_torect_c( &posin[0], &posin[1], xyz, &one ) ;
					irco_transform_c( xyz, &(image->coor), xyz, &(image->plate), &one ) ;
					irco_project_c( &(image->proj), xyz, &posin[0], &posin[1], &one ) ;
					pos->pos_x[pos->pos_no] = posin[0] ;
					pos->pos_y[pos->pos_no++] = posin[1] ;
					cancel_c( POS_KEY ) ;
					break ;
				case -2:status_c( tofchar("wrong format, see userangle.dc2") ) ;

					break ;
				case -3:status_c( tofchar("too many items") ) ;
					break ;
				default:break ;
			}
			if( pos->pos_no == MAXPOS ){
				status_c( tofchar("maximum number of positions entered") ) ;
				break ;
			}
		}
	}
	pgslw_c( &(plot->line[0]) ) ;
	charsize = chars->label * chars->def[1] ; pgsch_c( &charsize ) ;
	pgpnts_c( &one, &(pos->centre[0]), &(pos->centre[1]), &two, &one ) ;
	pgpnts_c( &(pos->pos_no), pos->pos_x, pos->pos_y, &two, &one ) ;
	return ;
}

void sort_array(char	*str,				/* makes a string of a random integer array */
		fint	*array, 			/* e.g. of the given snips */
		int	*no_array )
{
int	no = 0, add = 1, first, last ;
int	no_added = 0, new = 1 ;

	first = INT_MAX ;
	for( no = 0 ; no < *no_array ; no++ )first = min( first, array[no] ) ;
	sprintf( str, "%d", first ) ;
	last = first ;
	while( no_added < *no_array ){
		new = INT_MAX ;
		for( no = 0 ; no < *no_array ; no++ )
			if( array[no] > last && array[no] < new ) new = array[no] ;
		if( new - 1 == last ){
			if( add ){
				sprintf( str, "%s:", str ) ;
				add = 0 ;
			}
		}
		else{
			if( add ) sprintf( str, "%s", str ) ;
			if( last != first ) sprintf( str, "%s%d", str, last ) ;
			if( new != INT_MAX ){
				sprintf( str, "%s,%d", str, new ) ;
				first = new ;
				add = 1 ;
			}
			else break ;
		}
		last = new ;
	}
	return ;
}

void init_plot( image_type	*image,
		irds_type	*irds,
		plot_type	*plot,
		lab_type	*label,
		chars_type	*chars )
{
float	linewidth[] = {1.0,1.0,1.0};
float	x[2], y[2] ;
fchar	device_name ;
double	lon[4], lat[4] ;
float	charsize[6] ;
double	corner[12] ;
double	dev_size[2] ;

	fmake( device_name, 30 ) ;

	irpl_corners_c( image->size, corner ) ; 						/* corners of the plate-area */
	irco_transform_c( corner, &irds->plate, corner, &image->plate, &four ) ;		/* in the irds plate system */

	irco_tospher_c( corner, lon, lat, &four ) ;
	plot->dsize[0] =	max(max(lon[0],lon[1]),max(lon[2],lon[3])) -			/* size of the plate-area in radians */
				min(min(lon[0],lon[1]),min(lon[2],lon[3])) ;
	plot->dsize[1] =	max(max(lat[0],lat[1]),max(lat[2],lat[3])) -
				min(min(lat[0],lat[1]),min(lat[2],lat[3])) ;

	irco_project_c( &(image->proj), corner, image->xcorner, image->ycorner, &four ) ;
/* the area in which tracks will be plotted */
	plot->area[0] = min(min(image->xcorner[0],image->xcorner[1]),min(image->xcorner[2],image->xcorner[3])) ;
	plot->area[1] = max(max(image->xcorner[0],image->xcorner[1]),max(image->xcorner[2],image->xcorner[3])) ;
	plot->area[2] = min(min(image->ycorner[0],image->ycorner[1]),min(image->ycorner[2],image->ycorner[3])) ;
	plot->area[3] = max(max(image->ycorner[0],image->ycorner[1]),max(image->ycorner[2],image->ycorner[3])) ;

	plot->box[0] = plot->area[0] - 0.05*(plot->area[1]-plot->area[0]) ;
	plot->box[1] = plot->area[1] + 0.05*(plot->area[1]-plot->area[0]) ;
	plot->box[2] = plot->area[2] - 0.05*(plot->area[3]-plot->area[2]) ;
	plot->box[3] = plot->area[3] + 0.05*(plot->area[3]-plot->area[2]) ;

/**	labels for the axes,
	if the coor. system is equatorial RA Dec,
	else lon lat
**/
	if( image->equat ){
		strncpy( label->x.a, "R.A.", 4 ) ;
		strncpy( label->y.a, "Dec.", 4 ) ;
	}
	else{
		strncpy( label->x.a, "lon.", 4) ;
		strncpy( label->y.a, "lat.", 4) ;
	}

	strncpy( device_name.a, "?", 1 ) ;							/* initialise the output for the plot */
	if( pgbeg_c( &nul, device_name, &one, &one ) != 1 )finis_c() ;

	pgqvp_c( &two, &x[0], &x[1], &y[0], &y[1] ) ;
/* get device size */
	dev_size[0] = (x[1] - x[0])/10.0 ;
	dev_size[1] = (y[1] - y[0])/10.0 ;

/* determine the default plot size */
	plot->size[0] = 0.67*dev_size[0] ;
	plot->size[1] = plot->size[0]*plot->dsize[1]/plot->dsize[0] ;
	while( plot->size[1] > 0.75*dev_size[1] ){
		plot->size[0] *= 0.9 ;
		plot->size[1] *= 0.9 ;
	}

	no = userreal_c( plot->size, &two, HIDDEN, PLSIZE_KEY, PLSIZE_MES ) ;
	if( no == 1 )plot->size[1] = plot->size[0] ;
	plot->scale[0] = (plot->dsize[0]/D2R)*3600/(plot->size[0]*10.0) ;
	plot->scale[1] = (plot->dsize[1]/D2R)*3600/(plot->size[1]*10.0) ;
	if( !no ){
		sprintf( Cstring, "scale of plot axes [%.1f,%.1f]", plot->scale[0], plot->scale[1] ) ;
		no = userreal_c( plot->scale, &two, REQUEST, SCALE_KEY, tofchar(Cstring) ) ;
		if( no == 1 )plot->scale[1] = plot->scale[0] ;
/** scale in arcsec per mm **/
		plot->size[0] = 3600*(plot->dsize[0]/D2R)/(10*plot->scale[0]) ;
/** plotsize in cm **/
		plot->size[1] = 3600*(plot->dsize[1]/D2R)/(10*plot->scale[1]) ;
	}
	for( no = 0 ; no < 4 ; no++ ) plot->size[no] *= 1.1 ;
/** to get the border around the area **/

	pgwnad_c( &plot->box[1], &plot->box[0], &plot->box[2], &plot->box[3] ) ;
/** reverse x axis **/

	pgqlw_c( &plot->line[0] ) ;
/** linewidths **/
	no = userreal_c( linewidth, &three, HIDDEN, LINEW_KEY, LINEW_MES ) ;
	plot->line[2] = (int) plot->line[0]*linewidth[2] ;
	plot->line[1] = (int) plot->line[0]*linewidth[1] ;
	plot->line[0] = (int) plot->line[0]*linewidth[0] ;
	for( no = 0 ; no < 3 ; no++ ){
		if( plot->line[no] < 1 )plot->line[no] = 1 ;
		if( plot->line[no] > 201 )plot->line[no] = 201 ;
	}

	chars->size = dev_size[1]/40.0 ;
/** size of a the pgplot default charsize in cm **/

	chars->info = plot->info_width/(40*0.6*chars->size) ;
/** character sizes **/
	chars->id *= chars->info ;
	chars->vignet *= chars->info ;
	chars->coor = 0.025*max(plot->size[0],plot->size[1])/chars->size ;
	chars->label *= chars->coor ;
	chars->tracks *= chars->coor ;
	sprintf( Cstring, "     Default character size: %.2f %.2f %.2f %.2f %.2f %.2f",
		chars->label, chars->coor, chars->tracks, chars->vignet, chars->id, chars->info ) ;
	anyoutC ;
	no = userreal_c( charsize, &six, HIDDEN, CHARS_KEY, CHARS_MES ) ;
	for( n = 0 ; n < no ; n++ ){
		switch( n ){
		case 0 :chars->label = charsize[0] ;
			break ;
		case 1 :chars->coor = charsize[1] ;
			break ;
		case 2 :chars->tracks = charsize[2] ;
			break ;
		case 3 :chars->vignet = charsize[3] ;
			break ;
		case 4 :chars->id = charsize[4] ;
			break ;
		case 5 :chars->info = charsize[5] ;
			break ;
		default:break ;
		}
	}

/** position of lower left corner in cm **/
	plot->edge[0] = (0.6*12*chars->coor + 5*chars->label) * chars->size ;
	plot->edge[1] = (2*chars->coor + 5*chars->label) * chars->size ;
	no = userreal_c( plot->edge, &two, HIDDEN, PLEDGE_KEY, PLEDGE_MES ) ;
	if( no == 1 ) plot->edge[1] = plot->edge[0] ;
	plot->edge[0] /= INCH ;
	plot->edge[1] /= INCH ;
	plot->edge[2] = plot->edge[0] + plot->size[0]/INCH ;
	plot->edge[3] = plot->edge[1] + plot->size[1]/INCH ;
/** x-axis from 0->2 y-axis from 1->3 **/
	pgvsiz_c( &plot->edge[0], &plot->edge[2], &plot->edge[1], &plot->edge[3] ) ;

	plot->diff = 1.5*chars->size/plot->size[1] ;

	return ;
 }

int plot_snips( irds_type	*irds,
		image_type	*image,
		int		no_snip,
		fint		*snip,
		int		no_sdet,
		fint		*sdet )
{
int		noloop, loop1_no, loop2_no, loop_sdet = 1, loop[2] ;
int		sdet_no, snip_no, size ;
int		length ;

bool		prompt = tobool(1) ;

fint		level, detno, vignet = 1 ;
fint		tick = 1, nsamples, npoints, status ;
fint		font, font_vignet = 4 ;

float		*xout = NULL, *yout = NULL ;
float		angle, disp, disp0, coord, fjust, conv ;
float		x[2], y[2] ;

double		*lon_out, *lat_out, *twist ;
double		coordinate ;

chars_type	*chars ;
lab_type	*label ;
plot_type	*plot ;
pos_type	*pos ;


POS_INIT(pos) ;
PLOT_INIT(plot) ;
CHARS_INIT(chars) ;
LAB_INIT(label) ;

/* allocate memmory for the maximum number of samples to be found */
size = irds->axis[0]*irds->axis[1]*sizeof(double) ;
lon_out = malloc( size ) ; lat_out = malloc( size ) ; twist = malloc( size ) ;
size = irds->axis[0]*irds->axis[1]*sizeof(float) ;
xout = malloc( size ) ; yout = malloc( size ) ;


init_plot( image, irds, plot, label, chars ) ;
/* get the neccesary input and defaults for the plotting */

no = userlog_c( &vignet, &one, HIDDEN, VIG_KEY, VIG_MES ) ;

noloop = 0 ;
strncpy( string.a, "    ", 4 ) ;
/* loop over the detectors or over the snips */
no = usercharu_c( string, &one, HIDDEN, LOOP_KEY, LOOP_MES ) ;
if( no ){
	if( !strncmp( string.a, "SD", 2 ) ){
		loop_sdet = 1 ;   /* outer loop over the detectors */
		loop[0] = no_sdet ; loop[1] = no_snip ;
	}
	if( !strncmp( string.a, "SN", 2 ) ){
		loop_sdet = 0 ;   /* inner loop over the detectors */
		loop[0] = no_snip ; loop[1] = no_sdet ;
	}
	if( !strncmp( string.a, "N", 1 ) ){
		loop[0] = no_snip ; loop[1] = no_sdet ;
		noloop = 1 ;	/* noloop true means all det. and all */
				/* snips are plotted in one plot */
		loop_sdet = 0 ;
				/* if noloop, inner loop is over */
				/* the detectors */
	}
}
else{	if( no_snip > 1 || (no_snip == 1 && no_sdet == 1) ){
		loop_sdet = 1 ;
		loop[0] = no_sdet ; loop[1] = no_snip ;
	}
	else{	loop_sdet = 0 ;
		loop[0] = no_snip ; loop[1] = no_sdet ;
	}
}
sdet_no = snip_no = 0 ;

for( loop1_no = 0 ; loop1_no < loop[0] ; loop1_no++ ){
	if( plot->first || noloop == 0 ){
		pgpage_c() ;
		no = userlog_c( &prompt, &one, HIDDEN,
				PROMPT_KEY, PROMPT_MES ) ;
		if( no ){
			prompt = tobool(prompt) ;
			pgask_c(&prompt) ;
			cancel_c( PROMPT_KEY ) ;
			sprintf( Cstring, "<PLOT_SNIPS> prompt = %d",
				(int)prompt ) ;
			anyout_c( &TEST, tofchar(Cstring) ) ;
		}
		plot_box( image, plot, pos ) ;
		plot_pos( image, chars, plot, pos ) ;
		plot_coor( image, plot, chars ) ;

		pgslw_c( &(plot->line[0]) ) ;
		pgsch_c( &chars->coor ) ;
/* draw a box around the window */
		pgbox_c( tofchar("BC"), &fnul, &nul, tofchar("BC"),
				&fnul, &nul ) ;

/* plot the labels at the axes */
		disp = 2*chars->coor/chars->label ;
		pgslw_c( &(plot->line[2]) ) ; pgsch_c( &chars->label ) ;
		fjust = 0.5 ; disp = 2.5 ; coord = 0.55/1.1 ;
		pgmtxt_c( tofchar("B"), &disp, &coord, &fjust, label->x ) ;
		disp = 0.6*12*chars->coor/chars->label ;
		pgmtxt_c( tofchar("L"), &disp, &coord, &fjust, label->y ) ;

		disp0 = 0.6*12*chars->coor ;
		pgsch_c( &chars->vignet ) ;
		coord = 1.0 ;
		if( vignet )coord -= 3.5 * plot->diff * chars->vignet ;
		else coord -= 0.5 * plot->diff * chars->id ;
		pgsch_c( &chars->id ) ;
		disp = disp0/chars->id ; fjust = 0.0 ;
		pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust, label->prog_id ) ;

		pgsch_c( &chars->info ) ;
		disp = disp0/chars->info ;

/* plot object and instrument */
		sprintf( Cstring, "Object: %.*s with %.*s",
			nelc_c(image->object), image->object.a,
			nelc_c(image->instrume), image->instrume.a ) ;
		coord -= 2 * plot->diff * chars->info ;
		pgmtxt_c( tofchar("RV"), &disp, &coord,
			&fjust, tofchar(Cstring) ) ;

/* if looping over det's or snips, plot boresight, detector or snip no */
		if( !noloop ){
			coord -= plot->diff * chars->info ;
			if( loop_sdet ){
				sprintf( Cstring, "sdet = %d", sdet[sdet_no] ) ;
				anyout_c( &TEST, tofchar(Cstring) ) ;
				if( sdet[sdet_no] <= 0 ){
					if(	(sdet[sdet_no] !=
						-ircc_bandnr_c(image->instrume))
					) sprintf( Cstring,
							"        Boresight" ) ;
					else sprintf( Cstring,
							"        Boresight %.*s",
							nelc_c(image->instrume),
							image->instrume.a ) ;
				}
				else{
					level = status = 0 ;
					level  = gdsc_word_c( irds->name,
						&three, &sdet[sdet_no],
						&level, &status ) ;
					gdsd_rint_c( irds->name, tofchar("DETNO"),
						 &level, &detno, &status ) ;
					sprintf( Cstring, "        Det. %d",
						detno ) ;
				}
				pgmtxt_c( tofchar("RV"), &disp, &coord,
					&fjust, tofchar(Cstring) ) ;
			}
			else{
				sprintf( Cstring, "        Snip %d",
					snip[snip_no] ) ;
				pgmtxt_c( tofchar("RV"), &disp, &coord,
					&fjust, tofchar(Cstring) ) ;
			}
		}

/* plot coordinate system */
		sprintf( Cstring, "Coordinate system: %.*s",
			nelc_c(image->coor_name),

						image->coor_name.a ) ;
		if(	!strncmp(image->coor_name.a,"EQU",3) ||
			!strncmp(image->coor_name.a,"ECL",3)
		) sprintf( Cstring, "%s (%.1f)", Cstring, image->epoch ) ;
		coord -= plot->diff * chars->info ;
		pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust, tofchar(Cstring) ) ;

/* plot projection type */
		sprintf( Cstring, "Projection: %.*s", nelc_c(image->proj_name),
						image->proj_name.a ) ;
		coord -= plot->diff * chars->info ;
		pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust, tofchar(Cstring) ) ;

/* plot the set name with the plotted det's and snips */
		sprintf( Cstring, "Set: %.*s sdet ", nelc_c(irds->name),
					irds->name.a ) ;
		if( noloop == 1 ){
			sort_array( Cstring2, sdet, &no_sdet ) ;
			sprintf( Cstring, "%s%s snip ", Cstring, Cstring2 ) ;
			sort_array( Cstring2, snip, &no_snip ) ;
			sprintf( Cstring, "%s%s", Cstring, Cstring2 ) ;
		}
		else{
			if( loop_sdet ){
				sort_array( Cstring2, snip, &no_snip ) ;
				sprintf( Cstring, "%s%d snip %s", Cstring,
					sdet[sdet_no], Cstring2 ) ;
			}
			else{
				sort_array( Cstring2, sdet, &no_sdet ) ;
				sprintf( Cstring, "%s%s snip %d", Cstring,
					Cstring2, snip[snip_no] ) ;
			}
		}
		coord -= plot->diff * chars->info ;
		pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust,
				tofchar(Cstring) ) ;

/* plot centre */
		coordinate = image->centre[0]/D2R ;
		if( image->equat ) hms_c( &coordinate, string, NULL, &one, &nul ) ;
		else dms_c( &coordinate, string, NULL, &one, &nul ) ;
		coordinate = image->centre[1]/D2R ;
		dms_c( &coordinate, string2, NULL, &one, &nul ) ;
		strcpy( Cstring2, string2.a ) ;
		strcpy( Cstring, string.a ) ;
		length = max(nelc_c(tofchar(Cstring)),nelc_c(tofchar(Cstring2))) ;
		if( image->equat ){
			sprintf( Cstring, "Centre: RA.  %*s", length, string.a ) ;
			coord -= plot->diff*chars->info ;
			pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust, tofchar(Cstring) ) ;
			sprintf( Cstring, "        Dec. %*s", length, string2.a ) ;
			coord -= plot->diff*chars->info ;
			pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust, tofchar(Cstring) ) ;
		}
		else{
			sprintf( Cstring, "Centre: lon. %*s", length, string.a ) ;
			coord -= plot->diff*chars->info ;
			pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust, tofchar(Cstring) ) ;
			sprintf( Cstring, "        lat. %*s", length, string2.a ) ;
			coord -= plot->diff*chars->info ;
			pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust, tofchar(Cstring) ) ;
		}

/* plot the scale of the plot */
		sprintf( Cstring, "Scale: %.1fx%.1f arcsec/mm",
			plot->scale[0], plot->scale[1] ) ;
		coord -= plot->diff*chars->info ;
		pgmtxt_c( tofchar("RV"), &disp, &coord, &fjust,
			tofchar(Cstring) ) ;

		pgiden_c() ;
/* plot identification */

		if( vignet && plot->info_width > 0 ){
/* plot an IR-GIPSY label */
			int width ;
			pgslw_c( &(plot->line[0]) ) ;
			conv = (plot->box[1]-plot->box[0])/plot->size[0] ;
			pgsch_c( &chars->vignet ) ;
			x[1] = plot->box[0] -
				conv * (
					disp0*chars->size +
					0.6*12*chars->id*chars->size
				) ;
			y[1] = plot->box[3] ;
			width =  conv*9*0.6*chars->vignet*chars->size ;
			x[0] = x[1] + width ;
			y[0] = y[1] - conv*2*chars->vignet*chars->size ;
			pgmove_c( &x[0], &y[0] ) ;
			pgdraw_c( &x[1], &y[0] ) ; pgdraw_c( &x[1], &y[1] ) ;
/* this does not work!!! */
			pgdraw_c( &x[0], &y[1] ) ; pgdraw_c( &x[0], &y[0] ) ;
			pgqcf_c( &font ) ;
			font_vignet = 3 ;
			pgscf_c( &font_vignet ) ;
			pgslw_c( &(plot->line[2]) ) ;
			x[0] = 0.5 * (x[0]+x[1]) ;
			y[0] = 0.5 * (y[0]+y[1]) ;
			angle = 0.0 ; fjust = 0.5 ;
			pgptxt_c( &x[0], &y[0], &angle, &fjust, label->vignet ) ;
			pgscf_c( &font ) ;
		}
	}

	pgslw_c( &(plot->line[1]) ) ;
	pgsch_c( &chars->tracks ) ;
	if( loop_sdet ) snip_no = 0 ;
	else sdet_no = 0 ;
	for( loop2_no = 0 ; loop2_no < loop[1] ; loop2_no++ ){
		nsamples = irds->axis[0]*irds->axis[1] ;
		status = 0 ;
		irds_rd_detpos_c( irds->name, &snip[snip_no], &sdet[sdet_no],
			&tick, &(image->plate), &(image->proj), lon_out,
			lat_out, twist, &nsamples, &status ) ;
		if( status < 0 ){
			sprintf(Cstring, "Error in irds_rd_detpos, error value: %d", status ) ;
			anyoutC ;
			nsamples = 0 ;
		}
		else{
			if ( status == 1 ) {
				sprintf( Cstring, "Got INTENDED positions for %d points", nsamples ) ;
				anyoutC ;
			}
		}
		for( no = npoints = 0 ; no < nsamples ; no++ ){
			if(	lon_out[no] >= plot->area[0] &&
				lon_out[no] <= plot->area[1] &&
				lat_out[no] >= plot->area[2] &&
				lat_out[no] <= plot->area[3] ){
					xout[npoints] = lon_out[no] ;
					yout[npoints] = lat_out[no] ;
					npoints++ ;
			}
		}
		sprintf( Cstring, "Plotting %d points of snip %d, sdet %d",
					npoints, snip[snip_no], sdet[sdet_no] ) ;
		status_c( tofchar(Cstring) ) ;
		sprintf( Cstring, "Plotted %4d points for snip %d, sdet %d",
					npoints, snip[snip_no], sdet[sdet_no] ) ;
		anyoutC ;
		if( npoints ){
			k = npoints ;
			for( no = npoints = 0 ; no < k ; no += 10 ){
				xout[npoints] = xout[no] ;
				yout[npoints++] = yout[no] ;
			}
			xout[npoints] = xout[k-1] ;
			yout[npoints++] = yout[k-1] ;
			pgline_c( &npoints, xout, yout ) ;


			if (fp != NULL) {
			   /* Transform to long, lat in degrees */
#define NEWNUM	512

			   double  lon[NEWNUM], lat[NEWNUM], xyz[3*NEWNUM] ;
			   int jj;

			   for (jj = 0; jj < npoints; jj++) {
			      lon[jj] = (double) xout[jj];
			      lat[jj] = (double) yout[jj];
			   }
			   irco_deproject_c( &image->proj, lon, lat, xyz, &npoints ) ;
			   irco_transform_c( xyz, &image->plate, xyz, &image->coor, &npoints );
			   irco_tospher_c( xyz, lon, lat, &npoints ) ;

			   for (jj = 0; jj < npoints; jj++) {
			      lon[jj] *= R2D;
			      lat[jj] *= R2D;
			      while (lon[jj] < 0.0) lon[jj] += 360;
			      lon[jj] = fmod( lon[jj], 360.0 );
			    }
			    fprintf( fp, "U %f U %f U %f U %f\n",
				     lon[0], lat[0], lon[jj-1], lat[jj-1]);
			 }



/* put a snip and/or detector number at the beginning of a track */
			if( noloop ){
				sprintf( Cstring, "%d.%d",
					snip[snip_no], sdet[sdet_no] ) ;
			}
			else{
				if( !loop_sdet )
					sprintf( Cstring, "%d", sdet[sdet_no] ) ;
				else sprintf( Cstring, "%d", snip[snip_no] ) ;
			}
			fjust = (xout[0]<xout[npoints-1])?0.0:1.0 ;
			pgptxt_c( &xout[0], &yout[0], &fnul, &fjust,
					tofchar(Cstring) ) ;
		}
		if( loop_sdet ) snip_no++ ;
		else sdet_no++ ;
	}/* end of loop2 */
	if( loop_sdet ) sdet_no++ ;
	else snip_no++ ;

	plot->first = 0 ;
}/* end of loop1 */
	pgend_c() ;
	return( 0 ) ;
}

MAIN_PROGRAM_ENTRY
{
int		no_snip, no_sdet ;

fint		*snip = NULL, *sdet = NULL, snip_no, sdet_no ;
fint		irds_ss[MAXSUB], maxsub = MAXSUB, nsub ;
fint		subdim = 0, class = 1, status ;
fint		grid,  axnum[4] ;

image_type	*image ;
irds_type	*irds ;

	init_c() ;
	IDENTIFICATION( PROGRAM, VERSION ) ;

	fmake( string, MAXTXTLEN ) ;
	fmake( string2, 100 ) ;
	IMAGE_INIT( image ) ;
	SET_INIT( irds ) ;

	{
	  fchar   Filename;
	  fint	  n;
	  fmake(  Filename, 120 );
	  n = usertext_c( Filename, HIDDEN, FILENAME_KEY, FILENAME_MES );
	  if (n == 0) {
	    fp = NULL;
	  } else {
	    Filename.a[nelc_c(Filename)] = '\0';
	    strcat( Filename.a, ".rcl" );
	    fp = fopen( Filename.a, "w" );
	    if (fp == NULL) {
	       sprintf( Cstring, "Cannot open %s", Filename.a );
	       anyoutC ;
	       finis_c();
	       return(0);
	    }
	  }
	}

	nsub = gdsinp_c( irds->name, irds_ss, &maxsub, HIDDEN, IR_KEY, IR_MES,
			&device, axnum, irds->axis, &four, &class, &subdim ) ;
	if( nsub == 0 ){
		nsub = gdsinp_c( irds->name, irds_ss, &maxsub, NONE,
			IN_KEY, IN_MES, &device, axnum, irds->axis,
			&four, &class, &subdim ) ;
	}
	status = 0 ;
	if( irds_exist_c( irds->name, &status ) < 0 ){
		sprintf( Cstring, "<TRACKS> %.*s is not a proper irds.",
				nelc_c(irds->name), irds->name.a ) ;
		anyoutC ;
		finis_c() ;
	}
	status = 0 ;									/* get the proper axis lengths for the snip and sdet axes */
	gdsd_rint_c( irds->name, tofchar("NAXIS3"), &nul, &(irds->axis[2]), &status ) ;
	status = 0 ;
	gdsd_rint_c( irds->name, tofchar("NAXIS4"), &nul, &(irds->axis[3]), &status ) ;

	if( (snip = malloc(2*irds->axis[3]*sizeof(int))) == NULL ){			/* allocate space for the array to store the snips and sdets */
		anyout("error allocating memory for snip") ;
		finis_c() ;
	}
	if( (sdet = malloc(2*(irds->axis[2]+1)*sizeof(int))) == NULL ){
		anyout("error allocating memory for sdet") ;
		finis_c() ;
	}

	for( no = no_snip = no_sdet = 0 ; no < nsub ; no++ ){				/* get snips and sdets if they are given via subsets */
		status = 0 ;
		grid = gdsc_grid_c( irds->name, &four, &irds_ss[no], &status ) ;
		if( !status && grid > 0 && grid <= irds->axis[3] ){
			snip[no_snip++] = grid ;
			for( k = 0 ; k < no_snip-1 ; k++ ){
				if( grid == snip[k] ){
					no_snip-- ;
					break ;
				}
			}
		}
		status = 0 ;
		grid = gdsc_grid_c( irds->name, &three, &irds_ss[no], &status ) ;
		if( !status && grid >= 0 && grid <= irds->axis[2] ){
			sdet[no_sdet++] = grid ;
			for( k = 0 ; k < no_sdet-1 ; k++ ){
				if( grid == sdet[k] ){
					no_sdet-- ;
					break ;
				}
			}
		}
	}
	if( no_snip == 0 ){								/* if no snips were entered via subsets */
		sprintf( Cstring, "Snips to be plotted [1..%d]", irds->axis[3] ) ;
		no_snip = userint_c( snip, &irds->axis[3], REQUEST, SNIP_KEY, tofchar(Cstring) ) ;
		for( no = snip_no = 0 ; no < no_snip ; no++ ){
			if( snip[no] > 0 && snip[no] <= irds->axis[3] )snip[snip_no++] = snip[no] ;
		}
		if( no_snip == 0 ){							/* default all snips */
			for( snip_no = 0 ; snip_no < irds->axis[3] ; snip_no++ )
				snip[snip_no] = snip_no+1 ;
		}
		no_snip = snip_no ;
	}
	if( no_sdet == 0 ){								/* if no detectors were entered via subsets */
		sprintf( Cstring, "Detectors to be plotted (0..%d) [boresight(0)]", irds->axis[2] ) ;
		no = 2*irds->axis[2]+1 ;
		no_sdet = userint_c( sdet, &no, REQUEST, SDET_KEY, tofchar(Cstring) ) ;
		for( no = sdet_no = 0 ; no < no_sdet ; no++ ){
			if( abs(sdet[no]) <= irds->axis[2] )sdet[sdet_no++] = sdet[no] ;
		}
		if( no_sdet == 0 ){							/* default boresight */
			sdet[0] = 0 ;
			sdet_no = 1 ;
		}
		no_sdet = sdet_no ;
	}
	if( init_irds( irds, image ) )anyout("error reading inputs") ;
	if( plot_snips( irds, image, no_snip, snip, no_sdet, sdet ) )anyout("error while plotting") ;

	finis_c() ;
	return( 0 ) ;
}

