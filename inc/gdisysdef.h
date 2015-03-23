/* gdidef.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            gdisysdef.dc3

Header:       gdisysdef

Purpose:      Needed for low level Gispy Display Interface routines.

Category:     DISPLAY

File:         gdisysdef.h

Author:       K.G. Begeman

Description:  This header file defines the functions available in the
              Gipsy Display Interface LIBrary.

Updates:      Dec  2, 1990: KGB Document created
              Nov 20, 1997: KGB GDI_RESTORE/GDI_SAVE added.

#<

*/

typedef struct {			/* command structure */
   int	cmd;				/* the command */
   int	code;				/* encryption */
   int	nbytes;				/* number of bytes to expect */
} Command_struct;

#define	MAXSETNAMELEN	255		/* max. length GDS databasename */

/*
 * Define the options:
 */

#define	GDI_OPEN_INIT	1		/* INITIALIZATION only */
#define	GDI_OPEN_CLIENT	2		/* Open for a client */
#define	GDI_OPEN	0		/* Open display */
#define GDI_CLOSE	1		/* Close display */
#define	GDI_RESET	2		/* Reset display */
#define	GDI_UPDATE	3		/* Update display */
#define	GDI_CINFO	4		/* Obtain info about display colors */
#define	GDI_IINFO	5		/* Obtain info about image size */
#define	GDI_IMPUT	6		/* Puts image on display */
#define	GDI_IMGET	7		/* Gets image from display */
#define	GDI_COLPUT	8		/* Gets colors from client */
#define	GDI_COLGET	9		/* Puts colors to client */
#define	GDI_MINFO	10		/* Info about menu */
#define	GDI_CMENU	11		/* Creates a menu */
#define	GDI_DMENU	12		/* Deletes menu */
#define	GDI_GMENU	13		/* Gets menu option */
#define	GDI_MHEAD	14		/* Creates display header */
#define	GDI_IMWRITE	15		/* Writes display data */
#define	GDI_IMREAD	16		/* Reads display data */
#define	GDI_DEFIMG	17		/* Defines an image */
#define	GDI_RINFO	18		/* get info about recordings */
#define	GDI_RECORD	19		/* record an image */
#define	GDI_REMOVE	20		/* remove recorded image */
#define	GDI_RMASK	21		/* obtain mask of record images */
#define	GDI_SEQUENCE	22		/* define playback sequence */
#define	GDI_GINFO	23		/* obtain graphics info */
#define	GDI_GRCOL	24		/* set graphics color */
#define	GDI_GRON	25		/* turn graphics plane on */
#define	GDI_GROFF	26		/* turn graphics plane off */
#define	GDI_GRCLEAR	27		/* clear graphics plane */
#define	GDI_GRREAD	28		/* read graphics planes */
#define	GDI_GRREGION	29		/* define graphics region */
#define	GDI_IMMID	30		/* send main id */
#define	GDI_IMSID	31		/* send sub id */
#define	GDI_GRWRITE	32		/* write graphics planes */
#define	GDI_GDSID	33		/* set GDS id */
#define	GDI_PGPLOT	34		/* pgplot commands */
#define	GDI_FRAME	35		/* obtain current frame */
#define	GDI_BLANKCOL	36		/* set colors for blank */
#define	GDI_IDLEN	37		/* get length of text id */
#define	GDI_IINFO2	38		/* Obtain info about image size */
#define	GDI_GETLUT	39		/* Obtain current colors */
#define	GDI_SETXGRID	40		/* Set text for X grids */
#define	GDI_SETYGRID	41		/* Set text for Y grids */
#define	GDI_SAVE	42		/* Save the image currently displayed */
#define	GDI_RESTORE	43		/* Restore image previously saved */

/*
 * Define the errors:
 */

#define	GDIE_SUCCESS		0	/* no error */
#define	GDIE_NOT_IMPLEMENTED	-1	/* not implemented */
#define	GDIE_ERROR_UNKNOWN	-2	/* unknown error */
#define	GDIE_ALREADY_OPEN	-3	/* device already open */
#define	GDIE_ILLEGAL_ID		-4	/* illegal display id */
#define	GDIE_DEVICE_UNKNOWN	-5	/* unknown device */
#define	GDIE_NOT_OPEN		-6	/* device not open */
#define	GDIE_NO_SUCH_SET	-7	/* set does not exist */
#define	GDIE_GDS_ERROR		-8	/* GDS error */
#define	GDIE_MHEAD_DIMS		-9	/* wrong dimensions */
#define	GDIE_COLOR_RANGE	-10	/* color outside range */
#define	GDIE_IMWRITE_OVERFLOW	-11	/* data overflow */
#define	GDIE_TOO_LONG		-12	/* text overflow */
#define	GDIE_RECORD_RANGE	-13	/* record out of range */
#define	GDIE_RECORD_NO_IMAGE	-14	/* no image to record */
#define	GDIE_RECORD_ERROR	-15	/* error recording image */
#define	GDIE_RECORD_ILLEGAL	-16	/* illegal record */
#define	GDIE_GRAPHICS_RANGE	-17	/* illegal graphics planes */
#define	GDIE_OTHER_REQUEST	-18	/* already in wait state */
#define	GDIE_DATA_LEFT		-19	/* not enough data left */
#define	GDIE_NO_IMAGE		-20	/* no image defined */
#define	GDIE_C_SEND		-21	/* client send error */
#define	GDIE_C_RECEIVE		-22	/* client receive error */
#define	GDIE_C_FLUSH		-23	/* client flush error */
#define	GDIE_C_ALLOC		-24	/* client allocation error */
#define	GDIE_S_SEND		-51	/* server send error */
#define	GDIE_S_RECEIVE		-52	/* server receive error */
#define	GDIE_S_FLUSH		-53	/* server flush error */
#define	GDIE_S_ALLOC		-54	/* server allocation error */
#define	GDIE_GRWRITE_OVERFLOW	-55	/* data overflow */
#define	GDIE_SERVER_NOT_STARTED	-56	/* gids was not started */
#define	GDIE_NO_COM_FILE	-57	/* could not read com file */
#define	GDIE_NO_SETUP_SYMBOLS	-58	/* no gids_setup or gip_loc */
#define	GDIE_NO_SETUP_FILE	-59	/* cannot open setup file */
#define	GDIE_WRONG_COM_FILE	-60	/* wrong com file */
#define	GDIE_NO_SUCH_ERROR	-61	/* error from gdi_error */
#define	GDIE_S_NOOPEN		-62	/* cannot open display */
#define	GDIE_S_NOCFONT		-63	/* cannot get calibration font */
#define	GDIE_S_NOMFONT		-64	/* cannot get menu font */
#define	GDIE_S_NOCOLOR		-65	/* cannot initialize colors */
#define	GDIE_S_NOMENU		-66	/* cannot initialize menu */
#define	GDIE_S_NOSCALE		-67	/* cannot initialize scaling */
#define	GDIE_S_NOWEDGE		-68	/* cannot initialize wedge */
#define	GDIE_S_NODATA		-69	/* cannot initialize data */
#define	GDIE_S_NOCOORD		-70	/* cannot initialize coords */
#define	GDIE_S_NOCON		-71	/* cannot establish connections */
#define	GDIE_S_NOCOMFILE	-72	/* cannot create com. file */
#define	GDIE_RECORD_NOSAVE	-73	/* nothing saved */
