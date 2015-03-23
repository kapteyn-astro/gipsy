/*************************************************************
 *
 * Rheostat.h.
 * Public header file for Rheostat widget.
 *
 * Author: Joe English, joe@trystero.art.com
 *
 *************************************************************
 */

#ifndef  RHEOSTAT_H
#define  RHEOSTAT_H

extern WidgetClass xfwfRheostatWidgetClass;
typedef struct _XfwfRheostatClassRec *XfwfRheostatWidgetClass;
typedef struct _XfwfRheostatRec *XfwfRheostatWidget;

/* %%% For backward-compatibility -- to be removed next release */
#define rheostatWidgetClass xfwfRheostatWidgetClass
#define RheostatCallbackStruct XfwfRheostatCallbackStruct

/*
 * Resources:
 * Angles are specified in degrees, going clockwise from straight down.
 * 0 = down, 90 = left, 180 = up, 270 = right, 360 = down again.
 *
 * The Dial value is an integer, with minimumValue <= value <= maximumValue
 *
 * Setting minimumAngle=minimumValue=0 and maximumAngle=maximumValue=360 
 * will configure the Rheostat for specifying angles.
 *
 * If resizeArrow is True, then the arrow will be resized proportionally
 * when the Rheostat is resized.
 *
 * If tickGravity is True, then clicking near a Rheostat tick will snap
 * the indicator to that tick.
 *
 */ 

#define XtNmaximumAngle "maximumAngle"
#define XtNminimumAngle "minimumAngle"
#define XtNmaximumValue "maximumValue"
#define XtNminimumValue "minimumValue"
#define XtNnumberIntervals "numberIntervals"
#ifndef XtNvalue
# define XtNvalue "value"
#endif
#define XtNresizeArrow "resizeArrow"
#define XtNtickGravity "tickGravity"

/* 
 * Geometry-specifying resources (outside to inside):
 */

#define XtNouterMargin "outerMargin"
#define XtNtickLength "tickLength"
#define XtNdialThickness "dialThickness"
#define XtNinnerMargin "innerMargin"
#define XtNradius "radius"

/*
 * Appearance resources:
 */
#define XtNdialColor "dialColor"
#define XtNtickColor "tickColor"
#define XtNarrowColor "arrowColor"
#define XtNtickThickness "tickThickness"

/*
 * Arrow appearance:
 */
#define XtNarrowWidth "arrowWidth"
#define XtNinnerArrowLength "innerArrowLength"
#define XtNouterArrowLength "outerArrowLength"
#define XtNfillArrow "fillArrow"
#define XtNarrowThickness "arrowThickness"

/* if useShadowColors is True, then the Motif * topShadowColor and 
 * bottomShadowcolor resources will be used for dialColor and arrowColor.
 * Only valid in Motif version of Rheostat.
 */
#define XtNuseShadowColors "useShadowColors"


/*
 * Callbacks:
 */
#define XtNnotifyCallback "notifyCallback"
#define XtNsetCallback "setCallback"

#define XtCNumberIntervals "NumberIntervals"
#define XtCGravity "Gravity"
#define XtCMinimum "Minimum"
#define XtCMaximum "Maximum"

/*
 * Rheostat callback structure:
 */
typedef struct {
    int 	reason;		/* for Motif compatibility only */
    XEvent 	*event;		/* Ditto */
    int		value;		/* current dial value */
} XfwfRheostatCallbackStruct;

/*
 * Convenience callback function:
 * 'closure' must be an 'int *', into which is stored the current dial value.
 */

#if 0                                                   /* GIPSY: disabled */
extern void XfwfRheostatSetIntCallback(
#if NeedFunctionPrototypes
	Widget,		/* RheostatWidget */
	XtPointer, 	/* int *closure */
	XtPointer  	/* RheostatCallbackStruct *call_data */ 
#endif
);
#endif /* GIPSY */

#if 0                                                   /* GIPSY: disabled */
extern void XfwfDrawArrow(
#if NeedFunctionPrototypes
    Display *,
    Drawable d,
    GC gc,
    Position endx,
    Position endy,			/* position of arrow tip */
    int dx,
    int dy,				/* slope of arrow */
    Dimension outer_length,		/* distance tip->base */
    Dimension inner_length,		/* distance tip->inner */
    Dimension width,			/* distance base->outer points */
    Boolean fill 			/* True=>fill arrow,False=>outline */
#endif
);
#endif /* GIPSY */
#endif	/* RHEOSTAT_H */

