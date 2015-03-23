/*
#>            irco.dc2

 Purpose:      Coordinate Transformations

 Category	WORLD COORDINATES, IRAS

 File:         irco.shl

 Author:       Do Kester

 Person resp.: Do Kester

 Address:      guspace!do or rugfx4!do         (uucp)
 
 Description:
 
 		Coordinate transformation
 
  The IRAS data need an efficient coordinate transformation system to
  transform each sample from sunreferenced coordinates to the system
  requested by the user. For each scan the sunreferenced coordinate 
  system is different as the earth has moved.
  
  We propose the use of three-dimensional unit vectors to represent
  longitude and latitude in a coordinate system. To get from longitude
  and latitude to rectangular coordinates use IRCO_TORECT, and 
  IRCO_TOSPHER for the reverse. IRCO_TWISTORECT and IRCO_RECTOTWIST
  do the same, however they include a twist angle too.
  Transformation now simplifies to the in-product with a proper 
  transformation matrix. Some of these transformation matrices can 
  be prepared in advance. See IRCO_TRANSFORM.
  
  Five standard systems are defined. The systems are, in this order,
  Equatorial, Galactic, Ecliptic, SuperGalactic and SunReferenced.
  The first and third have their epoch at 2000. The SunReferenced system
  has its `epoch' at solar elongation = 0. The north pole of the
  SunReferenced system will be at the sun while the zeropoint of
  longitude is at the ecliptic north pole. The ecliptic is assumed 
  to be fixed in time.
  
  The transformations between these 5 systems fill 11 different matrices: 
  one identity matrix and 5 * ( 5 - 1 ) / 2 = 10 others. 
  ( The transformation matrix of system K to system I is the 
  transpose of the matrix from I to K. ) Of these 11 matrices only
  6 are actually defined; the others are calculated when necessary.
  There is room for 5 more systems to be defined by the user, but
  with the limitation that there is only room for 14 more 
  transformation matrices. So not all conversions can be filled.
  It is left to the user to decide which transformations are indeed
  constructed. See IRCO_NEWCOOR.
  
  The Equatorial and Ecliptic systems can be precessed to any epoch,
  and from that epoch to any other. See IRCO_PRECESS.
  
  There is a special user-defined coordinate system which is used
  in the construction of a map. It is called the plate-system. Its
  zero point of longitude and latitude are in the projection center
  of the map while the meridians of the plate system are parallel to
  those of the reference system, i.e. the system the projection center
  is given in. See IRCO_PLATE.
  
  Shifting from one sunreferenced system to the next can be archieved
  by precessing the system. See IRCO_SUNREF.
  
  To project coordinates in the plate-system onto a map the routines
  IRCO_PROJECT and IRCO_PROTWIST are provided, the latter includes
  a projection of the twist angle. 
  
  Names and numbers of coordinate systems or projection types are
  connected with resp. IRCO_NAMEPOCH, IRCO_NUMBER and IRCO_PRNAME,
  IRCO_PRNUMBER. IRCO_PRAXNAME provides fits-like names for the
  combination of coordinate system and projection type.
  
  In the file irco.h all declaration files of these transformation and 
  projection routines are bundled.
#<
*/

#include "irco_transform.h"
#include "irco_connect.h"
#include "irco_precess.h"
#include "irco_newcoor.h"
#include "irco_namepoch.h"
#include "irco_sunref.h"
#include "irco_plate.h"
#include "irco_number.h"
#include "irco_torect.h"
#include "irco_tospher.h"
#include "irco_sunlong.h"
#include "irco_rectotwist.h"
#include "irco_twistorect.h"
#include "irco_project.h"
#include "irco_prname.h"
#include "irco_prnumber.h"
#include "irco_protwist.h"
#include "irco_praxname.h"

