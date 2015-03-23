/***************************************************************************
 *
 * Slider2.h: Public header file for slider with 2 indicators
 *
 * Author   : Aziz Boxwala
 *            Dept of Radiation Oncology
 *            UNC Chapel Hill
 *
 **************************************************************************/

#ifndef  __SLIDER2_H__
#define  __SLIDER2_H__

/*
 * Resource string names
 */
#define XtNvalueChangedCallback  "value_changedCallback"
#define XtNdragCallback          "dragCallback"
#define XtNincrementCallback     "incrementCallback"
#define XtNdecrementCallback     "decrementCallback"
#define XtNselectCallback        "selectCallback"
#define XtNresetCallback         "resetCallback"
#define XtNlockSelectCallback    "lock_selectCallback"
#define XtNlockDragCallback      "lock_dragCallback"
#define XtNlockIncrementCallback "lock_incrementCallback"

#define XtNmarkers             "markers"
#define XtNindicatorColor      "indicatorColor"
#define XtNindicatorFillColor  "indicatorFillColor"
#define XtNindicatorThickness  "indicatorThickness"
#define XtNposition            "position"
#define XtNincrement           "increment"
#define XtNmarkerLength        "markerLength"
#define XtNshowLabel           "show_label"
#define XtNlabelColor          "label_color"
#define XtNlabelAlignment      "label_align"
#define XtNlabelPrecision      "label_precision"
#define XtNminimum             "minimum"
#define XtNmaximum             "maximum"
#define XtNlowValue            "lo_value"
#define XtNhighValue           "hi_value"
#define XtNprocessingDirection "processingDir"

#define XtCMarkers        "Markers"
#define XtCMin            "Min"
#define XtCMax            "Max"
#define XtCThick          "Thick"
#define XtCIncrement      "Increment"
#define XtCAlignment      "Alignment"
#define XtCPrecision      "Precision"
#define XtCDirection      "Direction"

enum {XabCR_VALUE_CHANGED, XabCR_DRAG, XabCR_INCREMENT,
	XabCR_DECREMENT, XabCR_RESET, XabCR_SELECT,
	XabCR_LOCK_SELECT, XabCR_LOCK_DRAG, XabCR_LOCK_INCREMENT};
enum {XabALIGN_CENTER, XabALIGN_LEFT, XabALIGN_RIGHT};

enum {XabVERTICAL, XabHORIZONTAL};
enum {XabMAX_ON_TOP, XabMAX_ON_BOTTOM, XabMAX_ON_LEFT, XabMAX_ON_RIGHT};

extern WidgetClass xabSlider2WidgetClass;
typedef struct _XabSlider2ClassRec * XabSlider2WidgetClass;
typedef struct _XabSlider2Rec      * XabSlider2Widget;

typedef struct {
  int     reason;     /* The reason why the callback was made            */
  XEvent *event;      /* The event structure that triggered the callback */
  int     low_value;  /* The new position of the low value indicator     */
  int     high_value; /* The new position of the high value indicator    */
}  XabSlider2CallbackStruct;

#endif /* __SLIDER2_H__ */
