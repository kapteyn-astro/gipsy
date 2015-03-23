Cpatch.f
C
C        Copyright (c) Kapteyn Laboratorium Groningen 1993
C        All rights reserved.
C
C#>            patch.dc1
C
CProgram:      PATCH
C
CPurpose:      Find data values at positions of blanks interpolating the
C              data in a small volume around the blanks.
C
CCategory:     CALCULATION, COMBINATION, MANIPULATION
C
CFile:         PATCH 
C
CAuthor:       F.J. Sicking
C
CKeywords:
C
C   INSET=     Set and subsets containing the data.
C
C   NOISET=    Set containing the errors on the data. [ask NOISE=]
C 
C   NOISE=     Give the noise on the data. [1.0]
C
C   OUTSET=    Set to write the output to. [input set]
C
C   BOX=       Area to replace blanks. For all subsets the same box is used.
C
C   BEAM=      The major and minor axes (FWHM in arcseconds) of an
C              elliptical beam that can be used for weighting of the data
C              used in the interpolation. [no beam]
C              Only asked if the dimension of the subsets is less or equal
C              than two. A round beam can be specified by supplying only
C              one number.
C  
C   BEAMPA=    The position angle of the beam. Only asked, if both, the
C              subsets are two dimensional, and two elements were supplied
C              at BEAM=.
C
C   BEAMCUT=   Cutoff for the beam. Positions where the value of the beam
C              (its central peak value is set to one) are lower than BEAMCUT=
C              discarded.
C
C   SIZE=      Size of the area to collect the data for each individual 
C              interpolation. The number of elements must be equal to the
C              number of subset axes. The values must be odd in order for
C              the routine to be able to centre the area on the position
C              of the blank. They must also be larger than one, since no
C              fitting can be done in a direction where the size of the
C              collecting area is only one pixel. If an interpolation
C              is wanted using only data from a lower dimensional structure
C              than the subset -for example a one dimensional interpolation
C              in a two dimensional subset- then the subset selection has
C              to be adapted.
C
C   NDATA=     Minimum number of data points present inside the area 
C              used for collecting the data. [0.0] The default is the
C              minimum amount of data possible depending on the dimension
C              and the degree of the polynomial.
C
C   ALL=       Only asked if one position is supplied at BOX=. [y]
C              Interpolate all blanks inside the area defined in SIZE= or
C              just the one position specified at BOX= ?
C              
C   DEG=       Degree of least squares polynomial used in the interpolation.
C              [0] Can be zero one or two.
C
C   ANOTHER=   Proceed with next box ? [n]
C
CDescription:
C              There are two ways to use PATCH. If at BOX= an area larger
C              than one position is given then for each blank inside this area
C              patch fits a polynomial through the data around it, ignoring
C              other blanks, and substitutes the blank by the value of the
C              polynomial. The data used in the fit are collected from an
C              area, which sizes can be specified using the keyword SIZE=,
C              around the blank. 
C              If only one position is given in BOX=, the keyword ALL= is
C              asked. If ALL=n PATCH proceeds the same way as described
C              above, but the area of operation is only one pixel. If
C              ALL=y, however,  PATCH fits only one polynomial, centered on
C              the position supplied at BOX=, but substitutes all blanks
C              inside the area specified by SIZE= --also used to collect
C              the data-- by the values of the  polynomial.
CUpdates:
C   
C#<
      PROGRAM PATCH
C     Declaration of variables. 
      IMPLICIT NONE
C     Variables concerning the sets the routine uses.
C     Maximum number of axes.
      INTEGER maxnax
      PARAMETER(maxnax=5)
C     Counter for the axes.
      INTEGER iax,jax
C     As a character too, to get descriptor items. 
      CHARACTER*1 ciax
C     Names of in- and out- sets.
      CHARACTER*80 iset,noiset
      CHARACTER*80 oset
C     The noise on the data.
      REAL noise
C     Axispermutations and lengths for input set, nois set and output set.
      INTEGER iaxperm(maxnax),iaxcount(maxnax)
      INTEGER noiaxperm(maxnax),noiaxcount(maxnax)
      INTEGER oaxperm(maxnax),oaxcount(maxnax)
C     Types of axes.
      CHARACTER*20 ctype(maxnax)
C     Type of axis, primarily used to find crota2.
      INTEGER axtyp
C     Units along subsetaxes.
      CHARACTER*20 cunit(maxnax),dunit(maxnax)
C     Conversion from arcseconds to units along the spatial axes.
      DOUBLE PRECISION funit(maxnax)
C     Pixelsize in each direction.
      DOUBLE PRECISION cdelt(maxnax),pixsize(maxnax)
C     Rotation angle of maps. Angle from the positive latitude axis of the maps
C     to the north. (Measured counterclockwise.)
      DOUBLE PRECISION crota2
C     Edges of subsets and the sizes.
      INTEGER flo(maxnax),fhi(maxnax),fsize(maxnax)
C     Edges of the area where the interpolation has to be done.
      INTEGER blo(maxnax),bhi(maxnax),bsize(maxnax)

C     Subset dependent variables.
C     Max. number of subsets  
      INTEGER maxnsubs
      PARAMETER(maxnsubs=128)
C     Number of and counter for subsets
      INTEGER nsubs,is
C     Dimension of subsets.
      INTEGER subdim
C     Subset coordinatewords.
      INTEGER isubs(maxnsubs),noisubs(maxnsubs)
      INTEGER osubs(maxnsubs)
C     Minima, maxima in the output for each subset.
      REAL odatmin(maxnsubs),odatmax(maxnsubs)
C     Number of blanks in the output and couter keeping track of the processed
C     datavalues.
      INTEGER onblank(maxnsubs),ocount(maxnsubs)

C     Variables dependent on user input.
C     In the case at BOX= one position has been specified all denotes if the
C     interpolation has to be done for all positions with blanks inside
C     the area where the data to do the interpolation have been collected.
      LOGICAL all
C     Next box or position.
      LOGICAL another

C     Size of area to collect data from as specified by the user.
      INTEGER size(maxnax)
C     Half the size as supplied by the user needed to restrict area of
C     operation.
      INTEGER sizeo2(maxnax)
C     Number of data values needed for one interpolation.
      INTEGER ndat
C     Increment along buffer for the weights for a shift of one grid.
C     The frame where the weights are defined is the same as defined by
C     size(maxnax). This increment is on a data buffer holding the data
C     from this frame.
      INTEGER incr(maxnax)
C     Increment for output.
      INTEGER oincr(maxnax)
C     The beam.
      DOUBLE PRECISION beam(2)
C     The position angle for the beam.
      DOUBLE PRECISION beampa
      DOUBLE PRECISION spa,cpa
C     Cutoff for the beam.
      REAL beamcut
C     Minimum number of data values needed for the patching.
      DOUBLE PRECISION ndata
C     Projections of position along the subset axes on the axes of the beam.
      DOUBLE PRECISION xbeam(2)
C     Position on data buffer of the weights of central position of the area
C     from where the data have to be used.
      INTEGER iwtc

C     Variables depending on the size of the data buffers.
C     Number of data values used in each direction if the subsets were two
C     dimensional which is the most common case. Maximum number of data values
C     on the buffers.
      INTEGER maxlen,maxndat
      PARAMETER (maxlen=257)
      PARAMETER (maxndat=maxlen*maxlen)
C     The number for the intermediate axis. The axis for which part of the
C     data needed for the interpolation of all data on that axis fit inside
C     the data cube. (If the subsets have a size in each direction of 255
C     this will be the second axis.)
      INTEGER medax
C     Maximum number of positions along the intermediate axis that fit inside
C     the data buffer.
      INTEGER maxmednp
C     Number of positions along the intermediate axis that will be read and
C     have been done.
      INTEGER mednpdo,mednpdone
C     Numner of data values on mednp positions on the intermediate axis.
      INTEGER medndat
      
C     Increment along data buffer for a shift of one grid in each coordinate.
      INTEGER bufincr(maxnax)
C     Lower and upper coordinatewords of read and write areas in subsets.
      INTEGER icwlo,noicwlo,ocwlo
      INTEGER icwhi,noicwhi,ocwhi
C     Upper and lower grids of read and write areas in subsets.
      INTEGER igridlo(maxnax),igridhi(maxnax)
      INTEGER ogridlo(maxnax),ogridhi(maxnax)
C     Size of read and write area in subsets.
      INTEGER isize(maxnax),osize(maxnax)
C     Dummy grid to store all kinds of grid positions.
      INTEGER grid(maxnax)
C     Dummy grid to store the position with respect to the position currnetly
C     being done.
      INTEGER q(maxnax)

C     Number and counter for positions along higher axes that need to be done.
      INTEGER hinpdo,hipdo
C     Data increment for one position on the highest subsetaxes.
      INTEGER hincr(maxnax)    
C     Lowest and highest positions on the data buffer that need to be done and
C     counter.
      INTEGER ndo,ido
C     Counter for the data in area needed for the interpolation.
      INTEGER jdat
C     The in and outputdatabuffers.
      REAL idat(maxndat),noidat(maxndat)
C     Positions on data buffer.
      INTEGER ibuf,jbuf
C     The output data buffer.
      REAL odat(maxndat)
C     Databuffer that contains weights for the data.
      REAL wdat(maxndat)
C     Counter along wdat.
      INTEGER iwt
C     In- output- transferidentifiers.
      INTEGER tidi,tidnoi,tido
C     Number of datavalues read and written.    
      INTEGER nread
C     First datavalue on buffer to be written number of data values to be
C     written.
      INTEGER iwrite,nwrite

C     Function that does the least squares fit.
      INTEGER lsqndim
C     Variables concerning the least squares polynomial.
      INTEGER deg
C     Maximum number of coefficients minus one.
      INTEGER maxnc
      PARAMETER(maxnc=maxnax+(maxnax+maxnax*maxnax)/2)
C     Number and counter for coefficients.
      INTEGER nc,ic
      DOUBLE PRECISION c(0:maxnc),cerr(0:maxnc)
C     Value of polynomial and its error.
      DOUBLE PRECISION talpol
      DOUBLE PRECISION shiwt,swt,echi2,chi2

C     Message to the user.
      CHARACTER*80 mes

C     Constants and undefined value.
      DOUBLE PRECISION pi
      PARAMETER(pi=3.141592653589793D0)
      DOUBLE PRECISION fwhm
      PARAMETER(fwhm=2.354820045D0)
      REAL blank

C     Dummy variables and gipsy system functions.

C     Contains returned errors.
      INTEGER err
C     Default specification.
      INTEGER def
C     General dummy integer variable.
      INTEGER ndum
C     General dummy double precision variable.
      DOUBLE PRECISION ddum
C     Gipsy functions.
      INTEGER gdsinp,gdsout
      INTEGER userint,userreal,userdble,userlog
      INTEGER gdsc_grid,gdsc_fill
      INTEGER axtype,factor
      LOGICAL insideptr

C     First box.
      DATA another/.false./

C     Contact Hermes.
      CALL init

      CALL setfblank(blank)

C     Get inputset and check if it is okay.
C     Subsets may have any dimension.
      subdim=0
      mes='Give input set and subsets.'
      def=0
      nsubs=gdsinp(iset,
     *             isubs,
     *             maxnsubs,
     *             def,
     *             'INSET=',
     *             mes,
     *             0,
     *             iaxperm,
     *             iaxcount,
     *             maxnax,
     *             1,
     *             subdim)
C     Check input set and get parameters.
      IF (subdim.EQ.0) CALL error(4,'Subset is single point.')
C     The descriptor items below are only used to be able to define the
C     beam to weight the data. So if the subsets are not one or two dimensional
C     they are virtually not needed.
      DO 1 iax=1,subdim
         WRITE(ciax,'(I1)')iaxperm(iax)
C        Get type of axis.
         CALL gdsd_rchar(iset,'CTYPE'//ciax,0,ctype(iax),err)
         IF (err.NE.0) 
     *   CALL error(4,'Header item CTYPE'//ciax//' not found.')
C        Get axis type number to be able to find crota2.
         axtyp=axtype(ctype(iax),cunit(iax),dunit(iax),ndum,ndum,ndum)
         IF (axtyp.EQ.2) THEN
C           Get crota2.
            CALL gdsd_rdble(iset,'CROTA'//ciax,0,crota2,err)
C           If crota2 not found set to zero.
            IF (err.NE.0) THEN 
               crota2=0.0D0
               err=0
            END IF
C           Convert crota2 to radians.
            crota2=crota2*pi/180.0D0
         END IF
C        Get increment along the axes.
         CALL gdsd_rdble(iset,'CDELT'//ciax,0,cdelt(iax),err)
         IF (err.NE.0) 
     *   CALL error(4,'Header item CDELT'//ciax//' not found.')
C        Get pixel size.
         pixsize(iax)=ABS(cdelt(iax))
C        Get units along the axes.
         CALL gdsd_rchar(iset,'CUNIT'//ciax,0,cunit(iax),err)
         IF (err.NE.0) 
     *   CALL error(4,'Header item CUNIT'//ciax//' not found.')
C        Get conversion from arcseconds to the units along the spatial axes.
         err=factor('ARCSEC',cunit(iax),funit(iax))
         IF (err.NE.0) CALL error(4,'Invalid axis units.')
1     CONTINUE

C     Ask for (sub)sets containing the noise on the data. The coordinate
C     system must be exactly the same as of the input set.
      mes='Give (sub)set(s) containing the noise. [ask noise]'
      def=5
      noiset=' '
      ndum=gdsinp(noiset,
     *            noisubs,
     *            nsubs,
     *            def,
     *            'NOISET=',
     *            mes,
     *            0,
     *            noiaxperm,
     *            noiaxcount,
     *            maxnax,
     *            1,
     *            subdim)
      IF (noiset.EQ.' ') THEN
C        Get the noise on the data.
         mes='Give the noise on the data. [1.0]'
         def=5
         noise=1.0
         ndum=userreal(noise,1,def,'NOISE=',mes)
         mes='Noise must be larger than zero.'
         IF (noise.LE.0.0) CALL error(4,mes)
         DO 3 ndum=1,maxndat
              noidat(ndum)=noise
3        CONTINUE
      END IF

C     Get output set.
C     Assign outputset to inputset. Class is 1 (for each inputsubset one
C     outputsubset).
      CALL gdsasn('INSET=','OUTSET=',1)
C     Get outputset.
      mes='Give (sub)set(s) for the output.'
      def=4
      ndum=gdsout(oset,
     *            osubs,
     *            nsubs,
     *            def,
     *            'OUTSET=',
     *            mes,
     *            0,
     *            oaxperm,
     *            oaxcount,
     *            maxnax)

C     Copy the data from input set to the output set. (Later on
C     only interpolated values will be written to the output set.)
      DO 5 is=1,nsubs
C        Initialize counter for the data values.
         ocount(is)=0
         tidi=0
         CALL gdsc_range(iset,isubs(is),icwlo,icwhi,err)
         tido=0
         CALL gdsc_range(oset,osubs(is),ocwlo,ocwhi,err)
4        CONTINUE
            CALL gdsi_read(iset,icwlo,icwhi,idat(1),maxndat,nread,tidi)
            CALL gdsi_write(oset,ocwlo,ocwhi,idat(1),nread,ndum,tido)
            CALL minmax3(idat(1),
     *                   nread,
     *                   odatmin(is),
     *                   odatmax(is),
     *                   onblank(is),
     *                   ocount(is))
         IF (tidi.NE.0) GOTO 4
         IF (tido.NE.0) CALL error(4,'tido <> 0')
5     CONTINUE
      
C     Get box of operation inside subset.
C     Get default size of box.
C     Control is returned here if the user, after having done a box wants to
C     to a new one. (Then another has become true.)
6     DO 7 iax=1,subdim
         bhi(iax)=1
7     CONTINUE 
      mes='Give area or position of operation.'
      def=1
      CALL gdsbox(blo,
     *            bhi,
     *            oset,
     *            osubs(1),
     *            def,
     *            'BOX=',
     *            mes,
     *            0,
     *            4)

C     If the subsets are less or two dimensional get the beam.
C     If the first value of the keyword BEAM=  remains its default, zero,
C     then no beam will be used. If already a box has been done the default
C     is set to the beam of the previous box.
      IF (subdim.LE.2) THEN
C        If first box the default is [0.0D0 0.0D0] which means no beam.
         IF (.NOT.another) THEN
            beam(1)=0.0D0
            beam(2)=0.0D0
            def=1
         ELSE
C           Already a beam has been used before. Convert it back to FWHM in
C           arcseconds.
            beam(1)=beam(1)*fwhm/funit(1)
            IF (subdim.EQ.2) beam(2)=beam(2)*fwhm/funit(2)
            WRITE(mes,'(''Give beam. ['',2F13.6,'']'')')
     *      (beam(iax),iax=1,subdim)
            def=1
         END IF
         IF(beam(1).EQ.0.0D0)mes='Give beam. (FWHM, arcsec) [no beam]'
C        Get the beam.
8        ndum=userdble(beam,subdim,def,'BEAM=',mes)
         IF (beam(1).NE.0.0D0) THEN
C           A beam will be used. Check if it is okay.
            IF (beam(1).LT.0.0D0.OR.
     *          beam(2).LT.0.0D0) THEN
               mes='Axes of beam must be greater than zero. [no beam]'
               def=1
               beam(1)=0.0D0
               beam(2)=0.0D0
               CALL cancel('BEAM=')
               GOTO 8
            END IF
            IF (subdim.EQ.2) THEN
               IF (beam(2).GT.beam(1)) THEN
C                 First element is not the largest.
                  mes='First element must be largest. [no beam]'
                  def=1
                  beam(1)=0.0D0
                  beam(2)=0.0D0
                  CALL cancel('BEAM=')
                  GOTO 8
               ELSE IF (beam(2).EQ.0.0D0) THEN
C                 A round beam since no second element is present.
                  beam(2)=beam(1)
                  spa=1.0D0
                  cpa=0.0D0
               ELSE
C                 Get position angle.
                  IF (.NOT.another) THEN
                     mes='Give position angle of the beam.'
                     def=4
                  ELSE
C                    The default is the beam of the previous box.
                     beampa=beampa*180.0D0/pi
                     WRITE(mes,
     *               '(''Give position angle of beam. ['',F9.3,'']'')')
     *              beampa
                    def=5
                  END IF
C                 Get position angle.
                  ndum=userdble(beampa,1,def,'BEAMPA=',mes)
C                 Convert to radians.
                  beampa=beampa*pi/180.0D0
C                 With respect to the second latitude axis.
                  beampa=beampa+crota2
                  spa=DSIN(beampa)
                  cpa=DCOS(beampa)
               END IF
               beam(2)=beam(2)*funit(2)/fwhm
            END IF
            beam(1)=beam(1)*funit(1)/fwhm
C           Get cutoff for the beam.
            IF (.NOT.another) THEN
               mes='Give relative cutoff for beam.'
               def=4
            ELSE 
C              Default from previous box present.
               WRITE(mes,
     *         '(''Give relative cutoff for beam. ['',F9.3,'']'')')
     *        beamcut
              def=5
            END IF
9           ndum=userreal(beamcut,1,def,'BEAMCUT=',mes)
            IF (beamcut.LT.0.0) THEN
               mes='Beamcut must be greater or equal zero.'
               def=4
               CALL cancel('BEAMCUT=')
               GOTO 9
            END IF
            IF (.NOT.another) THEN
               mes='Give minimum number of data values. [0.0]'
               def=5
               ndata=0.0D0
            ELSE
C              Default from the previous box.
               WRITE(mes,
     *         '(''Give minimum number of data values. ['',F9.3,'']'')')
     *         ndata
            END IF
            ndum=userdble(ndata,1,def,'NDATA=',mes)
         END IF
      END IF
C     If possible and wanted the beam is defined.
C     Define the default for SIZE=.
      IF (.NOT.another) THEN
C        If a beam is present, find the area needed to contain it.
C        collect the data from.
         IF (subdim.LE.2.AND.beam(1).NE.0.0D0) THEN
            DO 10 iax=1,subdim
               ddum=2.0D0*DSQRT(-2.0D0*DLOG(DBLE(beamcut)))*beam(iax)
               size(iax)=INT(ddum/pixsize(iax)+1.0D0)
               IF (MOD(size(iax),2).EQ.0) size(iax)=size(iax)+1
10          CONTINUE
C           Write a message to the user.
            WRITE(mes,'(''Give size of coll. area. ['',2I4,'']'')')
     *      (size(jax),jax=1,subdim)
            def=5
         ELSE
C           If it is the first box to be done and there is no beam no default
C           is present.
            mes='Give size of area used to collect data from.'
            def=4
         END IF
      ELSE
C        If it is not the first box the default is the same as before.
         WRITE(mes,'(''Give size of coll. area. ['',2I4,'']'')')
     *   (size(iax),iax=1,subdim)
         def=5     
      END IF
11    ndum=userint(size,subdim,def,'SIZE=',mes)
C     Check if the input on SIZE= is okay. 
      DO  12 iax=1,subdim
         sizeo2(iax)=(size(iax)-1)/2
         IF (MOD(size(iax),2).EQ.0.OR.size(iax).LE.1) THEN
            mes='Values must be odd and larger than one.'
            def=4
            CALL cancel('SIZE=')
            GOTO 11
         END IF
12    CONTINUE
C     Check if collecting area fits inside buffer.
C     Get increment for a shift of one grid in each coordinate along the
C     data needed for a single interpolation.
      incr(1)=1
      DO 13 iax=1,subdim-1
         incr(iax+1)=incr(iax)*size(iax)
13    CONTINUE
C     Get number of data values needed to do only one interpolation.
      ndat=incr(subdim)*size(subdim)
C     If the number of data values needed to do only one interpolation is
C     larger than the number that fits on the data buffer nothing can be done
C     and a larger databuffer is needed.
      mes='Collecting area too large for buffer.'
      IF (ndat.GT.maxndat) CALL error(4,mes)
C     Collecting area accepted.

C     Ask the way to do the interpolation.
C     Asking the keyword ALL= only if at BOX= only one position has been
C     specified.
      all=.true.
C     If only one position has been supplied the keyword ALL= has to be asked.
      DO 14 iax=1,subdim
         bsize(iax)=bhi(iax)-blo(iax)+1
         IF (bsize(iax).NE.1) all=.false.
14    CONTINUE
C     Ask the keyword ALL=
      mes='Interpolate all blanks inside collecting area ? [y]'
      def=5
      IF (all) ndum=userlog(all,1,def,'ALL=',mes)

C     Get degree for polynomial.
      IF (.NOT.another) THEN
C        If first box define default and message.
         mes='Give degree of polynomial. [0]'
         def=5
         deg=0
      ELSE
C        If not first box define message; a default is already present.
         WRITE(mes,'(''Give degree. ['',I4,'']'')')deg
         def=5
      END IF 
15    ndum=userint(deg,1,def,'DEG=',mes)
      IF (deg.LT.0.OR.2.LT.deg) THEN
         mes='Only the values 0,1 and 2 are allowed.'
         def=4
         CALL cancel('DEG=')
         GOTO 15
      END IF

C     Now all the input has been done.
C     Concern the weights of the data.
C     Get central position in the data buffer for the weights.
      iwtc=(ndat-1)/2+1
C     Loop over buffer containing the weights and store the weights.
      DO 101 iwt=1,ndat
C        If a beam is present its weights have to be calculated and stored.
         IF (subdim.LE.2.AND.beam(1).NE.0.0D0) THEN
            ndum=iwt-1
C           Get position.
            DO 100 iax=subdim,1,-1
               grid(iax)=ndum/incr(iax)
               ndum=ndum-grid(iax)*incr(iax)
               grid(iax)=grid(iax)-sizeo2(iax)
100         CONTINUE
C           Get projection of position along the axes of the beam.
            IF (subdim.EQ.1) THEN
               xbeam(1)=grid(1)*pixsize(1)/beam(1)
               wdat(iwt)=REAL(DEXP(-0.5D0*xbeam(1)**2))
             ELSE IF (subdim.EQ.2) THEN
               xbeam(1)=-spa*grid(1)*pixsize(1)+cpa*grid(2)*pixsize(2)
               xbeam(1)=xbeam(1)/beam(1)
               xbeam(2)= cpa*grid(1)*pixsize(1)+spa*grid(2)*pixsize(2)
               xbeam(2)=xbeam(2)/beam(2)
               wdat(iwt)=REAL(DEXP(-0.5D0*(xbeam(1)**2+xbeam(2)**2)))
            END IF
C           If weight too low; set to blank.
            IF (wdat(iwt).LT.beamcut) wdat(iwt)=blank
         ELSE
C           No beam; specified weights all equal one.
            wdat(iwt)=1.0
         END IF
101   CONTINUE

C     Get upper and lower coordinatewords of subset in order to get the
C     grids of the corners of the subsets.
      CALL gdsc_range(iset,isubs(1),icwlo,icwhi,err)
      IF (noiset.NE.' ') 
     *CALL gdsc_range(noiset,noisubs(1),noicwlo,noicwhi,err)

      DO 102 iax=1,subdim
C        Get upper and lowergrids of data that are needed to do the
C        interpolation and restrict area of operation to the area where the
C        calculation is possible considering the values for size.
C        (Not too close to the boundaries of the subsets.)
C        The variables blo and bhi contain the area where the interpolations
C        have to be done and the variables flo and fhi the area where the
C        data have to be collected from.
         flo(iax)=gdsc_grid(iset,iaxperm(iax),icwlo,err)
         blo(iax)=MAX(blo(iax),flo(iax)+sizeo2(iax))
         flo(iax)=blo(iax)-sizeo2(iax)
         fhi(iax)=gdsc_grid(iset,iaxperm(iax),icwhi,err)
         bhi(iax)=MIN(bhi(iax),fhi(iax)-sizeo2(iax))
         fhi(iax)=bhi(iax)+sizeo2(iax)
C        Get size of area where the data have to be collected from.
         fsize(iax)=fhi(iax)-flo(iax)+1
C        Get size of area that needs to be done.
         bsize(iax)=bhi(iax)-blo(iax)+1
         IF (noiset.NE.' ') THEN
            mes='Unusable frame of (sub)sets for the noise.'
            IF (gdsc_grid(noiset,noiaxperm(iax),noicwlo,err).GT.
     *          flo(iax)) CALL error(4,mes)
            IF (gdsc_grid(noiset,noiaxperm(iax),noicwhi,err).LT.
     *          fhi(iax)) CALL error(4,mes)
         END IF
102   CONTINUE

C     There are three kinds of axes:
C     1) The lowest, for which the data values that are needed to do the
C        interpolations for all positions along the entire axis fit inside 
C        the databuffer.
C     2) Intermediate, only the datavalues to do the interpolation for part
C        the axis fit inside the databuffer. There is only one axis of this
C        kind.
C     3) The highest for which only the datavalues needed to the interpolation
C        for one position on the axis fit inside the data buffer.

C     Find the intermediate axis.
C     Get the number of datavalues needed to do the interpolation for one
C     position on this intermediate axis.
      medax=1
      medndat=ndat
103   IF (medax.LT.subdim.AND.
     *    medndat*fsize(medax)/size(medax).LE.maxndat) THEN
         medndat=medndat*fsize(medax)/size(medax)
         medax=medax+1
         GOTO 103
      END IF
C     Get number of data values needed per position on medax.
      medndat=medndat/size(medax)
C     Get number of positions on this axis for which the data that fits
C     inside the data buffer.
      maxmednp=maxndat/medndat
C     Get number of positions on higher axes than medax.
      hinpdo=1
C     If higher axes than medax are present then get their data incrment and
C     their number of data positions.
      IF (medax+1.LE.subdim) THEN
         hincr(medax+1)=1
         DO 104 iax=medax+1,subdim-1
            hincr(iax+1)=hincr(iax)*bsize(iax)
104      CONTINUE
         hinpdo=hincr(subdim)*bsize(subdim)
      END IF
C     Get lower and upper grids of the data that will be read into the data
C     buffer. First for the axes that entirely fit inside the databuffer.
      DO 105 iax=1,medax-1
         igridlo(iax)=flo(iax)
         igridhi(iax)=fhi(iax)
         isize(iax)=igridhi(iax)-igridlo(iax)+1
         ogridlo(iax)=blo(iax)
         ogridhi(iax)=bhi(iax)
         osize(iax)=ogridhi(iax)-ogridlo(iax)+1
105   CONTINUE
   
C     Do all subsets.
      DO 500 is=1,nsubs
C        Do all positions on the axes higher than medax.
         DO 499 hipdo=1,hinpdo
C           Get position in data file of hip.
            ndum=hipdo-1
            DO 401 iax=subdim,medax+1,-1
               grid(iax)=ndum/hincr(iax)
               ndum=ndum-grid(iax)*hincr(iax)
               grid(iax)=grid(iax)+blo(iax)
               igridlo(iax)=grid(iax)-sizeo2(iax)
               igridhi(iax)=grid(iax)+sizeo2(iax)
               isize(iax)=igridhi(iax)-igridlo(iax)+1
               ogridlo(iax)=grid(iax)
               ogridhi(iax)=grid(iax)
               osize(iax)=ogridhi(iax)-ogridlo(iax)+1
401         CONTINUE
C           Initialize number of positions that are done on the intermediate
C           axis.
            mednpdone=0
C           Continue until all positions on medax have been done.
402         CONTINUE
               igridlo(medax)=flo(medax)+mednpdone
               ogridlo(medax)=blo(medax)+mednpdone
C              Number of positions along the intermediate axis that will be
C              done.
               mednpdo=MIN(maxmednp-size(medax)+1,
     *                     bhi(medax)-ogridlo(medax)+1)
C              Get higher grid that will be done and that will be read.
               ogridhi(medax)=ogridlo(medax)+mednpdo-1
               igridhi(medax)=ogridhi(medax)+sizeo2(medax)
               isize(medax)=igridhi(medax)-igridlo(medax)+1
               osize(medax)=ogridhi(medax)-ogridlo(medax)+1
               icwlo=gdsc_fill(iset,isubs(is),igridlo)
               icwhi=gdsc_fill(iset,isubs(is),igridhi)
               tidi=0
               CALL gdsi_read(iset,
     *                        icwlo,icwhi,
     *                        idat(1),
     *                        maxndat,
     *                        nread,
     *                        tidi)
               IF (tidi.NE.0) CALL error(4,'tidi <> 0')
C              Copy data that has been read.
               DO 403 ndum=1,nread
                  odat(ndum)=idat(ndum)
403            CONTINUE                           
               IF (noiset.NE.' ') THEN
                  noicwlo=gdsc_fill(noiset,noisubs(is),igridlo)
                  noicwhi=gdsc_fill(noiset,noisubs(is),igridhi)
                  tidnoi=0
                  CALL gdsi_read(noiset,
     *                           noicwlo,noicwhi,
     *                           noidat(1),
     *                           nread,
     *                           ndum,
     *                           tidnoi)
                  IF (tidnoi.NE.0) CALL error(4,'tidnoi <> 0')
               END IF
C              Get increments on data buffer corresponding to shift of one
C              grid for each axis.
C              Get increment along data that needs to be done.
               bufincr(1)=1
               oincr(1)=1
               DO 406 iax=1,subdim-1
                  bufincr(iax+1)=bufincr(iax)*isize(iax)
                  oincr(iax+1)=oincr(iax)*osize(iax)
406            CONTINUE
C              Get number of positions that need to be done.
               ndo=oincr(subdim)*osize(subdim)
C              For all positions on databuffer where derivatives have to be
C              calculated.
               DO 450 ido=1,ndo
                  ndum=ido-1
                  ibuf=1
C                 Get grid position in write frame.
                  DO 407 iax=subdim,1,-1
                     grid(iax)=ndum/oincr(iax)
                     ndum=ndum-grid(iax)*oincr(iax)
C                    Transfer to read frame.
                     grid(iax)=grid(iax)+sizeo2(iax)
                     ibuf=ibuf+grid(iax)*bufincr(iax)
407               CONTINUE
                  IF (idat(ibuf).EQ.blank.OR.all) THEN
C                    Set lower limit for the sum of the weights.
                     shiwt=ndata
C                    Indicate that the chi-square needs to be calculated.
                     chi2=0.0D0
C                    Do interpolation.
                     err=lsqndim(subdim,
     *                           ibuf,
     *                           isize,
     *                           idat(1),
     *                           noidat(1),
     *                           iwtc,
     *                           size,
     *                           wdat(1),
     *                           size,
     *                           deg,
     *                           c(0),
     *                           cerr(0),
     *                           swt,
     *                           shiwt,
     *                           chi2,
     *                           blank)
C                    Check if the interpolation was succesful.
                     IF (err.NE.0) GOTO 450
C                    Get theoretical chi2.
                     echi2=swt-shiwt
C                    Check the combination of the theorectical and actual chi2.
                     IF (echi2.LT.0.0D0) CALL error(4,'echi2 < 0')
                     IF (all) THEN
C                       Interpolate all blanks inside volume of interpolation.
                        DO 412 jdat=1,ndat
C                          Get grid etc.
                           ndum=jdat-1
                           jbuf=ibuf
C                          Get grid with respect to the position that has
C                          been done.
                           DO 409 iax=subdim,1,-1
                              q(iax)=ndum/incr(iax)
                              ndum=ndum-q(iax)*incr(iax)
                              q(iax)=q(iax)-grid(iax)
                              jbuf=jbuf+q(iax)*bufincr(iax)
409                        CONTINUE
C                          If blank do interpolation.
                           IF (idat(jbuf).EQ.blank) 
     *                     odat(jbuf)=REAL(talpol(deg,subdim,q,c))
412                     CONTINUE
C                       If all is true only one polynomial has been fitted.
C                       In this case output its coefficients and errors.
                        mes='The coefficients of the polynomial are:'
                        CALL anyout(0,mes)
                        nc=0
                        IF (deg.GT.0) nc=nc+subdim
                        IF (deg.GT.1) nc=nc+(subdim+subdim**2)/2
                        DO 414 ic=0,nc
                           WRITE(mes,99999)ic,c(ic),cerr(ic)
99999                      FORMAT(' c(',I2,')=',D14.7,' +/-',D14.7)
                           CALL anyout(0,mes)
414                     CONTINUE
C                       Output the chi-square.
                        WRITE(mes,'(''exp. chi-square: '',D14.7)')echi2
                        CALL anyout(0,mes)
                        WRITE(mes,'(''act. chi-square: '',D14.7)')chi2
                        CALL anyout(0,mes)
                     ELSE IF (.NOT.all) THEN
                        odat(ibuf)=REAL(c(0))
                     END IF
                  END IF
450            CONTINUE
C              Write data to output set.
               ndum=0
               IF (all) THEN
                  CALL initptr(igridlo,igridhi,
     *                         igridlo,igridhi,
     *                         subdim,nread,ndum)
                  ocwlo=gdsc_fill(oset,osubs(is),igridlo)
                  ocwhi=gdsc_fill(oset,osubs(is),igridhi)
               ELSE
                  CALL initptr(igridlo,igridhi,
     *                         ogridlo,ogridhi,
     *                         subdim,nread,ndum)
                  ocwlo=gdsc_fill(oset,osubs(is),ogridlo)
                  ocwhi=gdsc_fill(oset,osubs(is),ogridhi)
               END IF
               tido=0
460            IF (insideptr(iwrite,nwrite)) THEN
                  CALL gdsi_write(oset,
     *                            ocwlo,ocwhi,
     *                            odat(iwrite+1),
     *                            nwrite,
     *                            ndum,
     *                            tido)
                  CALL minmax3(odat(iwrite+1),
     *                         nwrite,
     *                         odatmin(is),
     *                         odatmax(is),
     *                         onblank(is),
     *                         ocount(is))
                  GOTO 460
               END IF
               IF (tido.NE.0) CALL error(4,'tido <> 0')
               mednpdone=mednpdone+mednpdo
            IF (mednpdone.LT.bsize(medax)) GOTO 402
C        Next position on the highest kind of axis.
499      CONTINUE
C     Next subset
500   CONTINUE

C     Does the user want another box.
      another=.false.
      mes='Do another box ? [n]'
      ndum=userlog(another,1,5,'ANOTHER=',mes)
      IF (another) THEN
         CALL cancel('ANOTHER=')
         CALL cancel('BOX=')
         CALL cancel('ALL=')
         CALL cancel('SIZE=')
         CALL cancel('BEAM=')
         CALL cancel('BEAMPA=')
         CALL cancel('BEAMCUT=')
         CALL cancel('NDATA=')
         CALL cancel('DEG=')
         GOTO 6
      END IF

C     Write minima, maxima and numbers of blanks to the subsets.
      CALL wminmax(oset,
     *             osubs,
     *             odatmin,
     *             odatmax,
     *             onblank,
     *             nsubs,
     *             1)

      CALL finis
      STOP
      END

CFunction:     LSQNDIM
C
CPurpose:      Fitting a polynomial through data inside a volume.
C
CFile:         lsqndim.f
C
CUse:          INTEGER LSQNDIM( DIM      , Input integer
C                               IBUF     , Input integer
C                               SIZEBUF  , Input integer array
C                               BUF      , Input real array
C                               ERRBUF   , Input real array
C                               IWTC     , Input integer
C                               SIZEWTS  , Input integer array
C                               WTS      , Input real array
C                               SIZE     , Input integer array
C                               DEG      , Input integer 
C                               C        , Output double precision array
C                               CERR     , Output double precision array
C                               SWT      , Output double precision
C                               SHIWT    , In/Output double precision
C                               CHI2     , In/Output double precision
C                               BLANK    ) Input real
C              LSQNDIM   Returns:
C                        0 Fitting succesful
C                        1 Cholesky decomposition of normal matrix failed.
C                        2 Not enough data.
C                        3 Dimension higher than parameter maxdim.
C                        4 Degree higher than two.
C                        5 Dimension of data collecting area lower than DIM.
C                        6 One of the sizes of the collecting area not odd.
C              DIM      Dimension of volume containing the data.
C              IBUF     Buffer position of centre of volume.
C              SIZEBUF  Sizes of volume in data buffer.
C              BUF      Buffer containing the data.
C              ERRBUF   Buffer containing errors on the data.
C              IWTC     Buffer position of the central weight.
C              SIZEWTS  Sizes of volume of weights.
C              WTS      Weights for the data.
C              SIZE     Sizes of volume in the fit.
C              DEG      Degree of polynomial must be lower than two.
C              C        Coefficients of the polynomial.
C              CERR     Formal errors on the coefficients.
C              SWT      Sum of weights in the fit.
C              SHIWT    On input:  minimum sum of weights in the fit.
C                       On output: sum of the n_parameter highest weights.
C              CHI2     Value of the chi-square. Not calculated if non zero
C                       on input.
C              BLANK    Undefined value.
CDescription:
C              Function to do the fitting of a linear (taylor) polynomial
C              through data in an n dimensional volume. The fitting is a least
C              squares fitting. The coefficients can be interpreted as the
C              derivatives through the data. For their order see function
C              TALPOL.

      INTEGER FUNCTION lsqndim(dim,
     *                         ibuf,
     *                         sizebuf,
     *                         buf,
     *                         errbuf,
     *                         iwtc,
     *                         sizewts,
     *                         wts,
     *                         size,
     *                         deg,
     *                         c,
     *                         cerr,
     *                         swt,
     *                         shiwt,
     *                         chi2,
     *                         blank)

      IMPLICIT NONE
C     Maximum dimension, maximum degree of the polynomial and the number of
C     coefficients minus one.
      INTEGER maxdim,maxdeg,maxm
      PARAMETER(maxdim=10)
      PARAMETER(maxdeg=2)
      PARAMETER(maxm=maxdim+(maxdim+maxdim*maxdim)/2)
C     Dimension of volume where the least squares fitting has to be done.
      INTEGER dim
C     Position on data buffer for central position of polynomial.
      INTEGER ibuf
C     Sizes, in all dimensions, of volume inside the databuffer.
      INTEGER sizebuf(*)
C     The data buffer and the buffer for the errors.
      REAL buf(*),errbuf(*)
C     Position on their buffer of the centre of the weights.
      INTEGER iwtc
C     Sizes, in all dimensions, of the volume where the weights are defined.
      INTEGER sizewts(*)
C     Weights for the data.
      REAL wts(*)
C     Sizes in all dimensions of volume where the data have to be used.
      INTEGER size(*)
C     Degree of polynomial.
      INTEGER deg
C     Coefficients of polynomial and their formal errors.
      DOUBLE PRECISION  c(0:*),cerr(0:*)
C     Amount of data, amount of parameter, and chi-square.      
      DOUBLE PRECISION swt,shiwt,chi2
C     Undefined value. 
      REAL blank
C     Counter for the axes.
      INTEGER iax,jax
C     Increment along data buffer, along buffer for the weights and along
c     the data used for a shift of one grid in each coordinate.
      INTEGER incrbuf(maxdim),incrwts(maxdim),incr(maxdim)
C     Half the edges of the volume that contains the data that will be used.
      INTEGER sizeo2(maxdim)
C     Number of data values that will be used.
      INTEGER ndat
C     Factor in front of the basis functions.
      DOUBLE PRECISION fac
C     Design matrix for the least squares problem.
      DOUBLE PRECISION ad(0:maxm)
C     Normal matrix for the least squares problem and its right hand side.
      DOUBLE PRECISION a(0:maxm,0:maxm),b(0:maxm)
      DOUBLE PRECISION p(0:maxm)
      DOUBLE PRECISION sum
C     Value of polynomial needed to calculate the chi-square.
      DOUBLE PRECISION talpol
C     Dummy equivalents for the data values the noise and the weights.
      DOUBLE PRECISION dat,sig,wt
C     Squares of the highest weights.
      DOUBLE PRECISION hiwt(0:maxm)
C     Counters for highest weights.
      INTEGER ihiwt,jhiwt
C     Counter and number of parameters except the constant.
      INTEGER im,m
C     Counter for elements of the normal matrix and its inverse.
      INTEGER i,j,k
C     Positions on the various data buffers.
      INTEGER jdat,jbuf,jwt
C     Grid position of current data value with respect to centre of polynomial.
      INTEGER grid(maxdim)
C     Dummy position on data buffer.
      INTEGER jdum
C     Function that does matrix cholesky decomposition.
      INTEGER chdcmp
C     Tolerances.

C     Check if dimension is not too high.
      IF (dim.GT.maxdim) THEN
         lsqndim=3
         RETURN
      END IF
C     Check if degree not too high.
      IF (deg.GT.2) THEN
         lsqndim=4
         RETURN
      END IF
C     Get half the sizes of the data area that is used in the interpolation
C     and check the size of the area.
      DO 1 iax=1,dim
         sizeo2(iax)=(size(iax)-1)/2
         IF (sizeo2(iax).LE.0) THEN
            lsqndim=5
            RETURN
         END IF
         IF (MOD(size(iax),2).EQ.0) THEN
            lsqndim=6
            RETURN
         END IF
1     CONTINUE
C     Initialization;
C     Get number of higher order terms than the constant term.
      m=0
      IF (deg.GT.0) THEN
C        Add number of linear terms.
         m=m+dim
         IF (deg.GT.1) THEN
C           Add number of quadratic terms.
            m=m+(dim+dim**2)/2
         END IF
      END IF
C     Initialize normal matrix and right hand side and highest weights.
      DO 3 i=0,m
         DO 2 j=0,m
            a(i,j)=0.0D0
2        CONTINUE
         b(i)=0.0D0
         hiwt(i)=0.0D0
3     CONTINUE
      swt=0.0D0
C     Initialize increment along databuffer and along the buffer for the 
C     weights for each coordinate and number of data values that will be
C     used and then get them all.
      incrbuf(1)=1
      incrwts(1)=1
      incr(1)=1
      DO 4 iax=1,dim-1
         incrbuf(iax+1)=incrbuf(iax)*sizebuf(iax)
         incrwts(iax+1)=incrwts(iax)*sizewts(iax)
         incr(iax+1)=incr(iax)*size(iax)
4     CONTINUE
C     Get number of datavalues used in the fit.
      ndat=incr(dim)*size(dim)
C     Interpolation;
C     Loop over all data that will be used in the interpolation.
      DO 18 jdat=1,ndat
C        Get grid position with respect to the centre of the interpolation.
C        Get position of current data value on all data buffers.
         jdum=jdat-1
         jbuf=ibuf
         jwt=iwtc
         DO 10 iax=dim,1,-1
            grid(iax)=jdum/incr(iax)
            jdum=jdum-grid(iax)*incr(iax)
            grid(iax)=grid(iax)-sizeo2(iax)
            jbuf=jbuf+grid(iax)*incrbuf(iax)
            jwt=jwt+grid(iax)*incrwts(iax)
10       CONTINUE
C        If blanks jump to next data value.
         IF (buf(jbuf).EQ.blank) GOTO 18
         IF (errbuf(jbuf).EQ.blank) GOTO 18
         IF (wts(jwt).EQ.blank) GOTO 18
C        Data value accepted.
C        Get design matrix for least squares solution.
C        Constant term.
         im=0
         ad(im)=1.0D0
C        Linear terms.
         IF (deg.GT.0) THEN
            DO 11 iax=1,dim
               im=im+1
               ad(im)=DBLE(grid(iax))
11          CONTINUE
C           Quadratic terms.
            IF (deg.GT.1) THEN
               DO 13 iax=1,dim
                  DO 12 jax=iax,dim
                     im=im+1
                     IF (iax.EQ.jax) THEN
                        fac=0.5D0
                     ELSE
                        fac=1.0D0
                     END IF
                     ad(im)=fac*DBLE(grid(iax)*grid(jax))
12                CONTINUE
13             CONTINUE
            END IF
         END IF
C        Get data value.
         dat=DBLE(buf(jbuf))
C        Get sigma on the data value.
         sig=DBLE(errbuf(jbuf))
C        Get weight for the data value.
         wt=DBLE(wts(jwt))
C        Calculate matrix of the normal equations.
         DO 15 i=0,m                    
            DO 14 j=i,m
               a(i,j)=a(i,j)+wt*ad(i)*ad(j)/sig**2
14          CONTINUE
            b(i)=b(i)+wt*ad(i)*dat/sig**2
15       CONTINUE
C        Store the m+1 highest weights.
         ihiwt=0
16       IF (wt.LE.hiwt(ihiwt).AND.ihiwt.LE.m) THEN
            ihiwt=ihiwt+1
            GOTO 16
         ELSE IF (ihiwt.LE.m) THEN
            DO 17 jhiwt=m,ihiwt+1,-1
               hiwt(jhiwt)=hiwt(jhiwt-1)
17          CONTINUE
            hiwt(ihiwt)=wt
         END IF
C        Update sum of weights.
         swt=swt+wt
C     Next data value.
18    CONTINUE

C     Check the amount of data present in the fit.
      IF (swt.LT.shiwt.OR.swt.EQ.0.0D0) THEN
         lsqndim=2
         RETURN
      END IF
C     Add the m+1 highest weights.
      shiwt=0.0D0
      DO 19 ihiwt=0,m
         shiwt=shiwt+hiwt(ihiwt)
19    CONTINUE

C     The choleski decomposition of the matrix a.
      lsqndim=chdcmp(maxm+1,m+1,a(0,0),p(0))
C     Check if the decomposition could be be done succesfully.
C     If not, lsqndim returns a non-zero value.
      IF (lsqndim.NE.0) RETURN
C     Get the coefficients of the least squares polynomial.
      CALL chbksb(maxm+1,m+1,a(0,0),p(0),b(0))
      DO 22 i=0,m
         c(i)=b(i)
22    CONTINUE
C     Store the inverse of the choleski decomposition in the upper triangular
C     of a. The normal matrix of the least squares problem is lost.
      DO 33 i=0,m
         a(i,i)=1.0D0/p(i)
         DO 32 j=i+1,m
            sum=0.0D0
            DO 31 k=i,j-1
               sum=sum-a(j,k)*a(i,k)
31          CONTINUE
            a(i,j)=sum/p(j)
32       CONTINUE
33    CONTINUE
C     Get lower triangle of covariance matrix, its diagonal elements are used
C     to get the errors on the parameters.
      DO 36 i=0,m
         DO 35 j=0,i
            sum=0.0D0
            DO 34 k=i,m
               sum=sum+a(i,k)*a(j,k)
34          CONTINUE
            IF (i.EQ.j) THEN
               p(i)=sum
               cerr(i)=DSQRT(sum)
            ELSE
               a(i,j)=sum
            END IF
35       CONTINUE
36    CONTINUE
C     Get the chi-square of the fit.
      IF (chi2.NE.0.0D0) RETURN
C     Again loop over all the datavalues that were used.
C     Get position with respect to interpolation centre.
      DO 53 jdat=1,ndat
         jdum=jdat-1
         jbuf=ibuf
         jwt=iwtc
C        Get position in data area.
         DO 50 iax=dim,1,-1
            grid(iax)=jdum/incr(iax)
            jdum=jdum-grid(iax)*incr(iax)
            grid(iax)=grid(iax)-sizeo2(iax)
            jbuf=jbuf+grid(iax)*incrbuf(iax)
            jwt=jwt+grid(iax)*incrwts(iax)
50       CONTINUE
C        If blanks jump to next data value.
         IF (buf(jbuf).EQ.blank) GOTO 53
         IF (errbuf(jbuf).EQ.blank) GOTO 53
         IF (wts(jwt).EQ.blank) GOTO 53
         dat=DBLE(buf(jbuf))
         sig=DBLE(errbuf(jbuf))
         wt=DBLE(wts(jwt))
C        Add to chi-square.
         chi2=chi2+wt*((dat-talpol(deg,dim,grid,c))/sig)**2
53    CONTINUE
      RETURN
      END

CFunction:     TALPOL
C
CPurpose:      Evaluation of a multidimensional taylor polynomial up to 
C              second degree.
C 
CUse:          DOUBLE PRECISION TALPOL( DEG   , Input integer
C                                       DIM   , Input integer
C                                       GRID  , Input integer array
C                                       C     ) Input double precision array
C 
C              TALPOL Returns: The value of the polynomial at the
C                     specified grid.
C              DEG    Degree of polynomial (0,1,2).
C              DIM    Dimension.
C              GRID   Grid position to do the evaluation.
C              C      Coefficients of the polynomial. (The derivatives at 
C                     grid position 0.)
C
C     f(x(1),....,x(dim)) =      
C     c(0)+
C     c(1)*x(1)+c(2)*x(2)+....................................+c(dim)*x(dim)+
C     0.5*c(dim+1)*x(1)**2+c(dim+2)*x(1)*x(2)+.........+c(2*dim)*x(1)*x(dim)+
C     0.5*c(2*dim+1)*x(2)**2+c(2*dim+2)*x(2)*x(3)....+c(3*dim-1)*x(2)*x(dim)+
C     .
C     .
C     .
C     0.5*c(dim*(dim-1))*c(dim)*c(dim)
C
      DOUBLE PRECISION FUNCTION talpol(deg,dim,grid,c)
      INTEGER deg,dim
      INTEGER grid(*)
      DOUBLE PRECISION c(0:*)
      INTEGER i
      INTEGER iax,jax

      talpol=c(0)
      i=0
      IF (deg.GT.0) THEN
         DO 10 iax=1,dim
            i=i+1
            talpol=talpol+c(i)*DBLE(grid(iax))
10       CONTINUE
      END IF
      IF (deg.GT.1) THEN
         DO 12 iax=1,dim
            i=i+1
            talpol=talpol+0.5D0*c(i)*DBLE(grid(iax)**2)
            DO 11 jax=iax+1,dim
               i=i+1
               talpol=talpol+c(i)*DBLE(grid(iax)*grid(jax))
11          CONTINUE
12       CONTINUE
      END IF
      RETURN
      END

C     This function does the choleski decomposition of matrix A, only the 
C     upper triangle of the matrix is needed and the result is stored into
C     the lower triangle and the vector P that contains the diagonal elements.
      INTEGER FUNCTION CHDCMP(NP,N,A,P)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (EPS=1.0D-6)
      DIMENSION A(NP,NP),P(NP)

C     Get a norm of the matrix.
      ANORM=0.0D0
      DO 5 I=1,N
         SUM=0.0D0
         DO 4 J=1,N
            SUM=SUM+DABS(A(I,J))
4        CONTINUE
         IF (SUM.GT.ANORM) ANORM=SUM
5     CONTINUE

C     Do the decomposition row by row.
      DO 14 I=1,N
         DO 13 J=I,N
            SUM=A(I,J)
            DO 12 K=I-1,1,-1
               SUM=SUM-A(I,K)*A(J,K)
12          CONTINUE
            IF (I.EQ.J) THEN
C              If not greater than zero, matrix singular. 
               IF (SUM/ANORM.LE.EPS) THEN
                  CHDCMP=1
                  RETURN
               END IF
               P(I)=DSQRT(SUM)
            ELSE
               A(J,I)=SUM/P(I)
            END IF
13       CONTINUE
14    CONTINUE
      CHDCMP=0
      RETURN
      END

C     Does the backward and formward substitution to solve a set of linear
C     equations using the Cholesky decomposition of their matrix.
      SUBROUTINE CHBKSB(NP,N,A,P,B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(NP,NP),P(NP),B(NP)
      DO 12 I=1,N
         SUM=B(I)
         DO 11 K=I-1,1,-1
            SUM=SUM-A(I,K)*B(K)
11       CONTINUE
         B(I)=SUM/P(I)
12    CONTINUE
      DO 14 I=N,1,-1
         SUM=B(I)
         DO 13 K=I+1,N
            SUM=SUM-A(K,I)*B(K)
13       CONTINUE
         B(I)=SUM/P(I)
14    CONTINUE
      RETURN
      END


