Cgalmod.f
C
C        Copyright (c) Kapteyn Laboratorium Groningen 2000
C        All Rights Reserved.
C
C#>            galmod.dc1
C
CProgram:      GALMOD
C
CPurpose:      Making model observations of the HI gas in a spiral galaxy.
C
CCategory:     MODELS
C
CFile:         galmod.f
C
CAuthor:       F.J. Sicking
C
CKeywords:
C
C   INSET=     Set to copy the coordinate system from. 
C              Usually a set with observations for which a model is wanted.
C              Maximum number of subsets is 2048.
C
C   BOX=       Area of operation. [entire subsets of INSET]
C              May exceed size of subsets of INSET. The subsets in outset
C              will have this size.
C
C   OUTSET=    Set to contain the model. 
C
C   POS=       Central position of galaxy.
C
C   VSYS=      Systemic velocity in (km/s). [0.0]
C
C   RADII=     Radii of rings in (arcsec).
C              The first ring is the inner boundary of the model. The outer
C              boundary will be the radius of the last ring. (The rings given
C              here are interpolated to rings with a much smaller separation.
C
C   EMPTY=     Leave innerpart of first ring empty ? [Y] 
C              (Only asked if the radius of the first ring is not equal to
C              zero.) If EMPTY=N galmod makes additional rings inside the
C              first ring. It keeps all the parameters for these additional
C              rings fixed at their values for the first ring.
C
C              If for a keyword to specify a `ring property' (such as VROT=)
C              there are less values present than the number of rings, for the
C              remaining outward rings, the value for the keyword is kept
C              fixed at its last specification. For example: a constant
C              inclination of 80 degrees can be specified by INCL=80.0.
C 
C   VROT=      Circular velocities of rings in (km/s).
C
C   VDISP=     To incorporate the velocity dispersion and the instrumental
C              broadening in the model in (km/s). See description.
C
C   DENS=      Surface density of the HI in the rings in (HI-atoms per cm2).
C
C   Z0=        Scaleheights of the HI-layer in (arcsec).
C
C   NRESTR=    Number of rings inside first specification of the inclination
C              and position angle. [0]
C              The first specification of inclination an position angle
C              is assigned to the (nrestr+1)th specification in RADII=.
C              For the first NRESTR rings the inclination and position angle
C              are kept constant at their first specification. The keyword
C              NRESTR makes specifying a large central disk with fixed
C              inclination and position angle (as in a spiral galaxy with
C              an HI layer that is warped in its outer parts) easier.
C
C   INCL=      Inclinations of rings in (degrees).
C
C   PA=        Position angles of rings in (degrees). Measured counter clock-
C              wise from the north.
C
C   LTYPE=     Layertype. The density profile in the direction perpendicular
C              to the plane of the rings. [list options]
C              Option 1: gaussian layer.
C                     2: sech2 layer.
C                     3: exponential layer.
C                     4: Lorentzian layer.
C                     5: box layer.
C
C   CMODE=     Determines the dependence of the number of clouds on the
C              surface density of the HI. [1]
C              The discretization noise in the maps made by galmod is
C              proportional to the square root of the number of random signal
C              contributions added into a pixel in the data cube times the
C              amount of signal per contribution. With this keyword the latter
C              can be influenced. If CMODE=0 then CDENS= is interpreted as
C              the number of clouds per area of a pixel in the plane of
C              the rings. In this case the amount of signal per cloud is
C              proportional to the surface density of the HI gas in the
C              plane of the ring. The discretization noise in the channel
C              maps then becomes proportional to the surface density of the
C              HI in the plane of the rings. It is however also proportional
C              to the square root of the number of contibutions added into
C              each pixel which is dependent on the distribution of the signal
C              from the ring over the data cube.
C              If CMODE=1, the default, the amount of signal per cloud is
C              constant. In this case the discretization noise in the maps
C              is proportional to the square root of the signal. This is
C              the only case where the dependence of the discretization
C              noise on the signal is not ambiguous and known. Therefore
C              this is the default and strongly recommended. If CMODE=2
C              then the amount of signal per cloud is inversly proportional
C              to the surface density of the HI gas. In this case the
C              discretization noise is independent on the surface density of
C              the HI in the plane of the rings but still on the distribution
C              of the signal over the data cube. This option can be used if
C              the surface density of the HI very strongly varies between
C              the rings. 
C                            
C   CDENS=     Surface density of clouds in the plane of the rings per area
C              of a pixel. [1]
C              The discretization noise in the maps is amongst others
C              inversly proportional to the square root of this number. The
C              way CDENS  is interpreted depends CMODE= but it is always
C              in units of 1E20 HI atoms per cm^2.
C
C   NV=        Number of subclouds in the velocity profile of a single `cloud'.
C              [number of subsets]
C
C** ISEED=     Number to initialize the random number generator [-1].
C              A negative integer may be specified here to initialize the
C              random number generator. Choosing a different number gives
C              a different sequence of random numbers. (For example, the
C              discretization noise in the maps made by galmod can be found
C              from the difference between two identical models with distinct
C              random number sequences.)
C
CDescription:
C              Galmod was originally developed by T.S. van Albada. This
C              is a version for GIPSY under UNIX. It makes a model
C              consisting of rings of different radii. The gas is assumed
C              to be on circular orbits on these rings. Each ring is
C              characterized by the following properties: a circular
C              velocity, a velocity dispersion, a column density, a
C              scaleheight perpendicular to the plane of the ring, an
C              inclination and a position angle. This kind of model is
C              often called a tilted ring model. 
C
C              First the routine 'regrids' the rings supplied by user
C              to `standard' rings with a width of 0.75 times the minimum
C              of the pixelsizes in each direction.
C              Making the model, the routine basically does a Monte Carlo
C              integration of the distribution of the HI-gas in space and
C              velocity over each pixel in the datacube. Proceeding as
C              follows: A loop is done over all the standard rings and each
C              ring is filled with a number of HI 'clouds' at random positions.
C              Their azimuths are randomly chosen from the interval [0,2*pi>.
C              Their radii too, within the width of the current `standard'
C              ring, but from a distribution of random numbers that is
C              proportional with radius, to obtain a uniform distribution of
C              clouds inside the plane of the ring. Finally, their heights
C              above the plane are chosen from a distribution with the same
C              shape and width as the density profile in this direction.
C              which allows modelling of a gas layer with thickness. From
C              the position of the cloud its systematic velocity, due to
C              the combination of systemic motion and rotation, is calculated.
C              Around this systematic velocity a  velocity profile is built.
C              For this purpose the cloud is divided into NV= subclouds and
C              each of these is assigned a velocity which is the sum of the
C              systematic velocity and a random contribution drawn from
C              a gaussian distribution with a width (dispersion) equal to
C              the value given at VDISP= which is closely related to the 
C              velocity dispersion. Now, having obtained a position and a
C              velocity, the routine calculates the corresponding pixel
C              in the data, does the (frequency dependent) conversion of
C              HI column density to 21cm radiation flux per pixel and
C              adds the (sub)cloud to the data.
C
C              The `beam' resulting from this process is the pixel in the
C              data cube. This is not as in real observations where pixels
C              are only to sample the the data. Consequently, the quantity
C              in the maps becomes flux per pixel expressed in W.U..
C
C              To smooth the maps to a real observational beam they have to
C              be convolved. To do this the task SMOOTH could be used.
C              As convolving function a gaussian could be used. Its FWHMs
C              along the axes of the maps should be: 
C 
C                CONBEAM = SQRT( beam%^2 - fwhm^2 * cdelt%^2 / 12 )
C
C              (The percent sign denotes the axis number.)
C              The value for CONPOSANG depends on which of the values
C              is the largest which will be taken as major axis of CONBEAM.
C              The scale factor should be:
C
C                SCALE = 2*pi*(beam1/fwhm)*(beam2/fwhm) / (cdelt1*cdelt2)
C
C              (fwhm = conversion from dispersion to FWHM = 2.354820045)
C              Basically, this process is equivalent to approximating a pixel
C              with a gaussian with the same normalization and dispersion.
C              This introduces errors which however will not be large if
C              the beam is large (twice) compared to the pixel. The quantity
C              in the maps remains flux per beam.
C
C              The choice of the value of VDISP should be such that 
C
C                VDISP^2 + chwidth^2 / 12 = sig_instr^2 + sig_v^2
C
C              VDISP     = The value for the keyword VDISP.
C              chwidth   = Velocity width of the channel maps.
C              sig_instr = The dispersion of the instrumental broadening.
C              sig_v     = Velocity dispersion of the random motions of the
C                          HI gas.
C  
C              Usually Hanning smoothing has been applied, and then the
C              instrumental profile can be approximated very well by a
C              gaussian with a FWHM of twice the channnel separation.
C              (If no Hanning smoothing has been done the instrumental profile
C              is a sinc function with a FWHM of 1.2 times the channel
C              speration. Galmod can not model this sinc function.)
C
C              It may be convenient to first make a rough model with small
C              values for CDENS and NV to check if the result of galmod
C              will be satisfactory.
C
C              Warning:
C                  An observed radial HI-profile is already convolved with the
C              observational beam. So if it is not deconvolved before using
C              GALMOD the resulting radial HI profile will be convolved twice.
C
C              Note:
C                  Galmod uses a random number generator and a function to
C              get random gaussian deviates drawn from: 
C              Press, Flannery, Teukolsky and Vetterling, Numerical Recipes.
CUpdates:
C              Dec -- 1992 : Document created.
C              Oct -- 1993 : Some changes in the code:
C                            Better handling of the coordinate system.
C                            Avoiding unnecessary calculations if a small box
C                            is used. Better noise distribution. Added keyword
C                            CMODE.
C              Aug -- 1994 : Corrected distribution of clouds inside radii,
C                            changed keywords PHI and INC into PA and INCL
C                            to match with other gipsy tasks.
C
C#<

      PROGRAM GALMOD

C     N.B. 1) All variables that deal with coordinates are of type
C             DOUBLE PRECISION and all variables that deal with data of
C             type REAL.
C          2) For positions and radii etc the units are in principle ARCMIN..
C             For angles such as inclinations and position angles: RADIANS
C             For frequency: Hz.
C             For velocity: M/S.
C             For surface/column density: 1E20 atoms per cm^2

C     Variables concerning the axes.
C     Max. number of axes.
      INTEGER maxnax
      PARAMETER(maxnax=5)
C     Counter for the axes.
      INTEGER iax
C     Also as a character to get descriptor items.
      CHARACTER*1 ciax 
C     Axispermutations and lengths.
      INTEGER inaxperm(maxnax),outaxperm(maxnax)
      INTEGER inaxcount(maxnax),outaxcount(maxnax)
C     Names of in- and output sets.
      CHARACTER*80 inset,outset
C     Units in the maps. Should be W.U..
      CHARACTER*20 bunit
C     Several header items.
C     Name of name of first axis that defines the subsets.
      CHARACTER*80 ctype3
C     Type of axis.
      INTEGER axtyp
C     Optical or radio definition of the vlocities resp. 1 2.
      INTEGER velsys
C     Frequency/velocity at reference pixel and increment between the subsets.
      DOUBLE PRECISION crval3,drval3,cdelt3
C     Reference pixel along fequency/velocity axis.
      DOUBLE PRECISION crpix3
C     Integer equivalent of reference pixel in frequency or velocity.
      INTEGER icrpix3
C     Units of frequency and velocity.
      CHARACTER*10 cunit3,dunit3
C     Rest frequency of spectral line. (1420.405751786E6 Hz)
      DOUBLE PRECISION freq0
C     Conversions of units.
C     Conversion to Hz.
      DOUBLE PRECISION hzconv
C     Conversion to M/S.
      DOUBLE PRECISION msconv
C     Rotation angle of maps and its sine and cosine.
C     This angle is from the poistive latitude axis in the map to the north,
C     measured counter clockwise. 
      DOUBLE PRECISION crota2
      DOUBLE PRECISION scrota2,ccrota2

C     Variables that concern the subsets.
C     Subset dimension.
      INTEGER subdim
      PARAMETER(subdim=2)
C     Subset axis names.
      CHARACTER*20 ctype(subdim)
C     Units along the axes. (dunit is a dummy variable)
      CHARACTER*20 cunit(subdim),dunit(subdim)
C     Increments along subset axes and conversion to arcmin.
      DOUBLE PRECISION cdelt(subdim),arcmconv(subdim)
C     Pixelsize and surface.
      DOUBLE PRECISION pixsize(subdim),pixarea
C     Box to be used.
      INTEGER blo(subdim),bhi(subdim),bsize(subdim)

C     Subsetdependent variables.
C     Maximum number of subsets.
      INTEGER maxnsubs
      PARAMETER(maxnsubs=2048)
C     Number of and counter for subsets.
      INTEGER nsubs,isubs
C     Subset coordinate words.
      INTEGER insubs(maxnsubs),outsubs(maxnsubs)
C     For each subset lower and upper coordinate words for the area to use.
      INTEGER cwlo(maxnsubs),cwhi(maxnsubs)
C     Mininmum and maximum grid of subsets and range.
      INTEGER minsubsgrid,maxsubsgrid,maxgridrange
      PARAMETER(minsubsgrid=-2*maxnsubs)
      PARAMETER(maxsubsgrid= 2*maxnsubs)
      PARAMETER(maxgridrange=maxsubsgrid-minsubsgrid+1)
C     Array that points to index of subset from its grid.
      INTEGER subsnuma(minsubsgrid:maxsubsgrid)
C     Conversion from HI-columndensity to intensity for each subset.
      REAL cd2i(maxnsubs)
C     Number of pixels processed and number of blanks.
      INTEGER count(maxnsubs),nblank(maxnsubs)
C     Minima and maxima in the output data.
      REAL datmax(maxnsubs),datmin(maxnsubs)
C     Frequency, wavelength and velocity of subset.
      DOUBLE PRECISION fsubs,labsubs,vsubs
   
C     Modelparameters.
C     Central position of galaxy.
      DOUBLE PRECISION pos(subdim)
C     Systemic velocity.
      DOUBLE PRECISION  vsystem
C     Maximum number of rings after regridding.
      INTEGER maxnr
      PARAMETER(maxnr=1024)
C     Separation between rings.
      DOUBLE PRECISION radsep
C     Number of regridded rings; and counter.
      INTEGER nr,ir
C     The rings; their array and a buffer variable.
      REAL radii(maxnr)
      DOUBLE PRECISION rtmp
      REAL vrot(maxnr),vdisp(maxnr)
      DOUBLE PRECISION vrottmp,vdisptmp
      REAL dens(maxnr)
      REAL z0(maxnr)
      DOUBLE PRECISION z0tmp
      REAL inc(maxnr),phi(maxnr),pa(maxnr)
      REAL sinc(maxnr),cinc(maxnr)
      DOUBLE PRECISION sinctmp,cinctmp
      REAL sphi(maxnr),cphi(maxnr)
      REAL spa(maxnr),cpa(maxnr)
      DOUBLE PRECISION spatmp,cpatmp
C     Layer type.
      INTEGER ltype
C     Determines the way the amount of signal per cloud is calculated. See
C     description.
      INTEGER cmode
C     Cloud density. (Definition depends on value of cmode.)
      REAL cdens
C     Number of subclouds in a velocity profile.
      INTEGER nv(maxnr),nvtmp
C     Integer to initialize the sequence of the random numbers. Thereafter
C     the seed for the random number generator. Iseed is kept and used
C     to reinitialize the random number generator for each new model.
      INTEGER iseed,isd

C     Number of distinct random number that can be generated.
C     This number is equal to 2**24. Choosing it close to (within a factor 2)
C     to the largest integer could result in integer overflow.
      INTEGER nran
      PARAMETER(nran=16777216)
      DOUBLE PRECISION dnran
      PARAMETER(dnran=16777216.0D0)
C     The random number generator is the integer function iran. It generates
C     a pseudo random integer between zero and nran-1 including the
C     boundaries. ([0,nran>)
      INTEGER iran
      EXTERNAL iran
C     The generator for the gaussian deviates.
      DOUBLE PRECISION gasdev
C     The function to generate the random deviates following the density
C     profile perpendicular to the plane of the gaslayer.
      DOUBLE PRECISION fdev
      
C     Variables concerning one single cloud.
C     Radius.
      DOUBLE PRECISION  r
C     Number of and counter for clouds.
      INTEGER nc,ic
C     Azimuth of cloud and its sine and cosine. (Measured counterclockwise 
C     from the receding half of the line of intersection of the plane of the
C     galaxy with the plane of the sky.)
      DOUBLE PRECISION az,saz,caz
C     Position in height above the ring.
      DOUBLE PRECISION z
C     Position of cloud in frame in the plane of the sky:
C     Positive x-axis is the receding half of the major axis.
C     Positive y-axis  perpendicular to x, right handed system.
      DOUBLE PRECISION x,y
C     Grid position of cloud within subset and grid of subset.
      INTEGER grid(subdim),subsgrid
C     Counter for subclouds inside velocity profile.
      INTEGER iv
C     Systematic motion of cloud. (Systemic motion and rotation.)
      DOUBLE PRECISION vsys
C     Velocity of subcloud. (Systematic and random motion.)
      DOUBLE PRECISION v
C     Grid in velocity of subcloud.
      DOUBLE PRECISION velgrid
C     Position of grid on the databuffer.
      INTEGER idat
C     HI atom flux of subcloud.
      REAL fluxsc
C     Variables for handling outputdata.
C     Buflen should be chosen as large as possible for the sake of speed.
      INTEGER buflen
      PARAMETER(buflen=512*512)
C      PARAMETER(buflen=16*512*512)
C     Data buffer.
      REAL datbuf(buflen)
C     Galmod principally calculates velocity profiles to fill its data
C     buffer.
C     Maximum number of velocity profiles that fits inside the data buffer.
      INTEGER nprofmax
C     The number that actually is inside the data buffer and counter.   
      INTEGER nprof,iprof
C     Number of velocity profiles done.
      INTEGER nprofdone

C     Starting position for each subset on the data buffer.
      INTEGER idat1
C     Output transfer identifiers.
      INTEGER tid(maxnsubs)

C     Gipsy functions.
      INTEGER gdsinp,gdsout,gdspos
      INTEGER gdsc_grid,gdsc_fill
      INTEGER userint,userreal,userdble,usertext
      INTEGER axtype,factor
C     Determines the possibilty of using default values for the input 
C     parameters.
      INTEGER def
C     Contains error codes.
      INTEGER err
C     Dummy variables.
      CHARACTER*80 mes
      INTEGER ndum
      DOUBLE PRECISION ddum

C     Costants.
C     Pi.
      DOUBLE PRECISION  pi,twopi
      PARAMETER(pi=3.141592653589793D0)
      PARAMETER(twopi=2.0D0*pi)
C     The Boltzmann constant in (J/K).
      DOUBLE PRECISION k
      PARAMETER(k=1.38062259D-23)
C     The conversion to W.U..
      DOUBLE PRECISION  wu
      PARAMETER(wu=5D-29)
C     The speed of light in (m/s).
      DOUBLE PRECISION c
      PARAMETER(c=2.99792458D8)

C     Initialization.
      DATA subsnuma/maxgridrange*0/
      DATA tid/maxnsubs*0/
      DATA count/maxnsubs*0/

C     Start executable code.
C     Contact hermes.
      CALL init

C     Get the inputset and its subsets.
      mes='Give input set and subsets.'
      def=0
      nsubs=gdsinp(inset,
     *             insubs,
     *             maxnsubs,
     *             def,
     *             'INSET=',
     *             mes,
     *             0,
     *             inaxperm,
     *             inaxcount,
     *             maxnax,
     *             1,
     *             subdim)

C     Get area of operation.
      mes='Give area of operation.'
      def=1
      CALL gdsbox(blo,
     *            bhi,
     *            inset,
     *            insubs(1),
     *            def,
     *            'BOX=',
     *            mes,
     *            0,
     *            1)

C     Get information about the axes of the subsets in inset and their units. 
      DO 1 iax=1,subdim
         WRITE(ciax,'(I1)')inaxperm(iax)
         err = 0
         CALL gdsd_rchar(inset,'CTYPE'//ciax,0,ctype(iax),err)
         IF (err.NE.0) CALL error(4,'CTYPE'//ciax//' not found.')
         axtyp=axtype(ctype(iax),cunit(iax),dunit(iax),ndum,ndum,velsys)
         IF (axtyp.EQ.2) THEN
C           Get rotationangle of maps. This angle is measured from the
C           positive latitude axis to the north.
            err = 0
            CALL gdsd_rdble(inset,'CROTA'//ciax,0,crota2,err)
            IF (err.NE.0) crota2=0.0D0
            err=0
            IF (crota2.NE.0) THEN
               WRITE(mes,'(''Maps are rotated, angle='',F10.5)')
     *         crota2
               CALL anyout(0,mes)
            END IF
C           Convert to radians and get sine and cosine. All these are also
C           necessary if crota2 is zero (Except the conversion to radians.)
            crota2=crota2*pi/180.0D0
            scrota2=DSIN(crota2)
            ccrota2=DCOS(crota2)
         END IF
         err = 0
         CALL gdsd_rchar(inset,'CUNIT'//ciax,0,cunit(iax),err)
         IF (err.NE.0) CALL error(4,'CUNIT'//ciax//' not found.')
C        Convert units to the ones used in GALMOD.
         err=factor(cunit(iax),'ARCMIN',arcmconv(iax))
         IF (err.NE.0) CALL error(4,'Invalid CUNIT'//ciax)
         err = 0
         CALL gdsd_rdble(inset,'CDELT'//ciax,0,cdelt(iax),err)            
         cdelt(iax)=cdelt(iax)*arcmconv(iax)
         pixsize(iax)=DABS(cdelt(iax))
         bsize(iax)=bhi(iax)-blo(iax)+1
1     CONTINUE
C     Get the area of a pixel.
      pixarea=pixsize(1)*pixsize(2)

C     Get information about the frequency/velocity axis.
C     Get rest frequency of spectral line.
      err = 0
      CALL gdsd_rdble(inset,'FREQ0',0,freq0,err)
      IF (err.NE.0) THEN
         CALL error(1,'Header item FREQ0 not found.')
         freq0=0.1420405751786D10
         WRITE(mes,'(D18.11,'' assumed.'')')freq0
         CALL anyout(0,mes)
      END IF
      WRITE(ciax,'(I1)')inaxperm(subdim+1)
C     Get axisinformation.
      err = 0
      CALL gdsd_rchar(inset,'CTYPE'//ciax,0,ctype3,err)
      mes='Header item CTYPE'//ciax//' not found.'
      IF (err.NE.0) CALL error(4,mes)
      axtyp=axtype(ctype3,cunit3,dunit3,ndum,ndum,velsys)
      err = 0
      CALL gdsd_rdble(inset,'CRVAL'//ciax,0,crval3,err)
      mes='Header item CRVAL'//ciax//' not found.'
      IF (err.NE.0) CALL error(4,mes)
      err = 0
      CALL gdsd_rdble(inset,'CRPIX'//ciax,0,crpix3,err)
      mes='Header item CRPIX'//ciax//' not found.'
      IF (err.NE.0) CALL error(4,mes)
      icrpix3=IDNINT(crpix3)
      err = 0
      CALL gdsd_rchar(inset,'CUNIT'//ciax,0,cunit3,err)
      mes='Header item CUNIT'//ciax//' not found.'
      IF (err.NE.0) CALL error(4,mes)
      err = 0
      CALL gdsd_rdble(inset,'CDELT'//ciax,0,cdelt3,err)
      mes='Header item CDELT'//ciax//' not found.'
      IF (err.NE.0) CALL error(4,mes)
      IF (axtyp.EQ.3) THEN
C        IF frequency axis get velocity at reference pixel.
         err = 0
         CALL gdsd_rdble(inset,'DRVAL'//ciax,0,drval3,err)
         IF (err.EQ.0) THEN
C           Secondary axis defined.
            err = 0
            CALL gdsd_rchar(inset,'DUNIT'//ciax,0,dunit3,err)
            mes='Header item DUNIT'//ciax//' not found.'
            IF (err.NE.0) CALL error(4,mes)
         ELSE
C           Secondary axis not defined; Try to get required items from
C           the user.
            mes='Give velocity at reference grid.'
            def=4
            ndum=userdble(drval3,1,def,'DRVAL3=',mes)
            mes='Give unit of velocity.'
            def=4
            ndum=usertext(dunit3,def,'DUNIT3=',mes)
         END IF
C        Convert units to the ones used in GALMOD.
         err=factor(cunit3,'HZ',hzconv)
         mes='Improper frequency units.'
         IF (err.NE.0) CALL error(4,mes)
         crval3=crval3*hzconv
         cdelt3=cdelt3*hzconv         
         err=factor(dunit3,'M/S',msconv)
         mes='Improper velocity units.'
         IF (err.NE.0) CALL error(4,mes)
         drval3=drval3*msconv
      ELSE IF (axtyp.EQ.4) THEN
C        Convert units to the ones used in GALMOD.
         err=factor(cunit3,'M/S',msconv)
         mes='Improper velocity units.'
         IF (err.NE.0) CALL error(4,mes)
         crval3=crval3*msconv
         cdelt3=cdelt3*msconv
         mes='Give frequency at reference grid in HZ.'
         def=4
         ndum=userdble(drval3,1,def,'DRVAL3=',mes)
      ELSE 
         CALL error(4,'No velocities along spectral axis.')
      END IF

C     Get units in the maps.
      err = 0
      CALL gdsd_rchar(inset,'BUNIT',insubs(1),bunit,err)
      IF (err.EQ.0.OR.err.EQ.insubs(1)) THEN
         IF (bunit(:4).NE.'W.U.'.AND.bunit(:4).NE.'w.u.')
     *   CALL error(1,'Units in the maps should be W.U..')
      ELSE
         CALL error(1,'No units in the maps found.')
      END IF
      err=0

C     Copy coordinate system to outset.
      CALL gdsasn('INSET=','OUTSET=',1)

C     Change frame of output set to the area defined at BOX=.
      CALL gdscss('OUTSET=',blo,bhi)

C     Get outputset.
      mes='Give (sub)set(s) to contain the model.'
      def=4
      ndum=gdsout(outset,
     *            outsubs,
     *            nsubs,
     *            def,
     *            'OUTSET=',
     *            mes,
     *            0,
     *            outaxperm,
     *            outaxcount,
     *            maxnax)

C     Get the subsetnumbers as a function of their grids with respect to
C     the reference grid. To transform a velocity to a subset, first
C     the transformation to a grid is done, then, using the array subsnuma,
C     the subset is found.
      DO 2 isubs=1,nsubs
         err = 0
         subsgrid=gdsc_grid(inset,
     *                      inaxperm(subdim+1),
     *                      insubs(isubs),
     *                      err)
C        Get grid with respect to reference grid.
         subsgrid=subsgrid-icrpix3
         IF (subsgrid.LT.minsubsgrid.OR.maxsubsgrid.LT.subsgrid) THEN
            CALL anyout(0,'Grid of subset out of range. Choose')
            CALL error(4,'reference pixel closer to the used subsets.')
         END IF
         subsnuma(subsgrid)=isubs
C        Get upper and lower coordinatewords of the outputsubsets of the
C        box that has to be done.
         cwlo(isubs)=gdsc_fill(outset,outsubs(isubs),blo)
         cwhi(isubs)=gdsc_fill(outset,outsubs(isubs),bhi)
2     CONTINUE

C     Get modelparameters for the galaxy.
C     Central position.
      mes='Give central position of galaxy.'
      def=0
      ndum=gdspos(pos,
     *            1,
     *            def,
     *            'POS=',
     *            mes,
     *            inset,
     *            insubs(1))

C     Close inset since it is no longer needed.
      err = 0
      CALL gds_close(inset,err)

C     Systematic velocity.
      mes='Give systemic velocity in (km/s).'
      def=4
      ndum=userdble(vsystem,1,def,'VSYS=',mes)
C     Convert to (m/s).
      vsystem=vsystem*1.0D3

C     Get rings and their parameters.
      radsep=0.75D0*DMIN1(pixsize(1),pixsize(2))
      CALL ringio(radsep,
     *            maxnr,
     *            nr,
     *            radii,
     *            vrot,
     *            vdisp,
     *            dens,
     *            z0,
     *            inc,
     *            phi)
      DO 3 ir=1,nr
C        The position angle, pa, is measured from the positive latitude axis
C        in the maps while phi is measured from the north.
         pa(ir)=phi(ir)+crota2
         sinc(ir)=SIN(inc(ir))
         cinc(ir)=COS(inc(ir))
         sphi(ir)=SIN(phi(ir))
         cphi(ir)=COS(phi(ir))
         spa(ir)=sphi(ir)*ccrota2+cphi(ir)*scrota2
         cpa(ir)=cphi(ir)*ccrota2-sphi(ir)*scrota2
3     CONTINUE
C     Get shape of density profile perpendicular to the plane of the rings.
      mes='Give type of layer. [list options]'
      def=5
      ltype=0
4     ndum=userint(ltype,1,def,'LTYPE=',mes)
      IF (ltype.LE.0.OR.5.LT.ltype) THEN
         CALL anyout(0,' Ltype:')
         CALL anyout(0,'       1 -- Gaussian layer.')
         CALL anyout(0,'       2 -- Sech2 layer.')
         CALL anyout(0,'       3 -- Exponential layer.')
         CALL anyout(0,'       4 -- Lorentzian layer.')
         CALL anyout(0,'       5 -- Box layer.')
         CALL cancel('LTYPE=')
         GOTO 4
      END IF
C     The way the clouds are distributed over the area and the signal.
      mes='Way to calculate cloud density. [1]'
      def=5
      cmode=1
5     ndum=userint(cmode,1,def,'CMODE=',mes)
      IF (cmode.LT.0.OR.2.LT.cmode) THEN
         mes='CMODE must be zero, one or two.'
         CALL cancel('CMODE=')
         GOTO 5
      END IF
C     The surface density of the clouds in the plane of the layer.
      mes='Cloud density. [1.0]'
      def=5
      cdens=1.0
6     ndum=userreal(cdens,1,def,'CDENS=',mes)
      IF (cdens.LE.0.0) THEN
         mes='CDENS must be greater than zero.'
         cdens=1.0
         CALL cancel('CDENS=')
         GOTO 6
      END IF
C     Number of subclouds per velocity profile.
      nvtmp=nsubs
      WRITE(mes,
     *'(''No. subclouds per velocity profile. ['',I4,'']'')')nvtmp
      def=5
7     ndum=userint(nvtmp,1,def,'NV=',mes)
      IF (nvtmp.LT.1) THEN
         mes='NV must be greater than zero.'
         nvtmp=nsubs
         CALL cancel('NV=')
         GOTO 7
      END IF
C     If the velocity dispersion is zero then the velocity profile has no 
C     width and only one point is needed to construct it.
      DO 9 ir=1,nr
         IF (vdisp(ir).EQ.0.0) THEN
            nv(ir)=1
         ELSE
            nv(ir)=nvtmp
         END IF
9     CONTINUE
C     Initialization of random number sequence.
       mes=
     *'Give negative integer to initialize random number sequence. [-1]'
      def=6
      iseed=-1
10    ndum=userint(iseed,1,def,'ISEED=',mes)
      IF (iseed.GE.0) THEN
         mes='Iseed must be negative.'
         def=5
         CALL cancel('ISEED=')
         GOTO 10
      END IF

C     Get conversion from column density of the HI and intensity of 21cm
C     radiation. (See Mihalas and Binney 'Galactic Astronomy'. p489)
C     N.B.:
C     The columndensity of the HI is in units of (1E20 atoms per cm2).
C     Velocities are in M/S and frequencies in HZ.
C     Solid angle is in steradians.
C     Intensity is flux per one steradian solid angle. In GALMOD it is
C     defined as the flux per beam of one square arcminute defined in W.U..
      DO 12 isubs=1,nsubs
         IF (axtyp.EQ.3) THEN
C           Freq-axis.
C           Get frequency of subset since the conversion is frequency
C           dependent.
            err = 0
            fsubs=crval3+cdelt3*gdsc_grid(outset,
     *                                    outaxperm(subdim+1),
     *                                    outsubs(isubs),
     *                                    err)
C           Get wavelength.
            labsubs=c/fsubs
C           Get conversion from column density to intensity in W.U..
            ddum=2D0*(k/wu)/(0.038475D-4*labsubs**2)
C           The frequency profile is a delta peak since all atoms are on one
C           and the same velocity/frequency. It has to be averaged over the
C           frequency range inside the subset. 
            ddum=ddum/ABS(cdelt3)
         ELSE IF (axtyp.EQ.4) THEN
C           Velo-axis.
C           Get the velocity of the subset.
            err = 0
            vsubs=crval3+cdelt3*gdsc_grid(outset,
     *                                    outaxperm(subdim+1),
     *                                    outsubs(isubs),
     *                                    err)
C           Get frequency of subset. (Optical definition) (drval3 contains
C           the reference frequency for a VELO axis.)
            fsubs=freq0*c*drval3/(drval3*(vsubs-crval3)+freq0*c)
C           Get wavelength of subset.
            labsubs=c/fsubs
C           Get conversion from column density to intensity.
            ddum=2D0*(k/wu)/(0.1823D-4*labsubs**2)
C           Again the velocity profile is a delta peak that has to be
C           averaged, but this time over the velocity range.
            ddum=ddum/DABS(cdelt3)
         END IF
C        The solid angle in the definition of the intensity is in steradians.
C        Convert this to square arcminutes.
         cd2i(isubs)=ddum*(pi/(180.0D0*60.0D0))**2
12    CONTINUE

C     Now all initializations have been done and galmod starts calculating
C     the model and writing the data to outset.

C     Get maximum number of velocity profiles that fits inside the databuffer.
      nprofmax=buflen/nsubs
C     Initialize the number of profiles that has been done.
      nprofdone=0
C     For each data buffer a full model is calculated and only the part that
C     fits inside the data buffer is used. After having done this control
C     is returned here.
100   CONTINUE
C        Reinitialize random number generator for each new model.
         isd=iseed
         CALL status('Calculating model.')
C        Get number of velocity profiles that currently will be done.
         nprof=MIN(nprofmax,bsize(1)*bsize(2)-nprofdone)
C        Initialize part of data buffer that is needed on zero.
         DO 101 idat=1,nprof*nsubs
            datbuf(idat)=0.0
101      CONTINUE
C        Loop over standard rings.
         DO 106 ir=1,nr
C           Get radius:
            rtmp=DBLE(radii(ir))
C           Get number of clouds inside ring.
            nc=IDNINT(cdens*dens(ir)**cmode*twopi*rtmp*radsep/pixarea)
            IF (nc.EQ.0) THEN
               WRITE(mes,'(''For radius:'',F12.4)')rtmp*60.0D0
               CALL anyout(0,mes)
               CALL anyout(0,'No clouds used. Choose higher CDENS=.')
C              Do next ring, jump to end of loop for rings.
               GOTO 106
            END IF
C           Get rotation velocity.
            vrottmp=DBLE(vrot(ir))
C           Get velocity dispersion.
            vdisptmp=DBLE(vdisp(ir))
C           Get thickness:
            z0tmp=DBLE(z0(ir))
C           Get sine and cosine of inclination.
            sinctmp=DBLE(sinc(ir))
            cinctmp=DBLE(cinc(ir))
C           Get sine and consine of position angle.
            spatmp=DBLE(spa(ir))
            cpatmp=DBLE(cpa(ir))
C           Get number of subclouds inside velocity profile.
            nvtmp=nv(ir)
C           The total amount of HI atom flux per pixel integrated over the
C           entire ring per subcloud.
            fluxsc=dens(ir)*twopi*rtmp*radsep/(nc*nvtmp)
C           Loop over clouds. 
            DO 105 ic=1,nc
C              Get radius inside ring. The range includes the inner boundary,
C              excludes the outer boundary. The probability of a radius inside
C              a ring is proportional to the total radius and thus the 
C              surface density of the clouds is constant over the area of
C              the ring.
               ddum=DBLE(iran(isd))/dnran
               r=DSQRT((rtmp-0.5D0*radsep)**2+2.0D0*radsep*rtmp*ddum)
C              Get azimuth and its sine and cosine.
               az=twopi*DBLE(iran(isd))/dnran
               saz=DSIN(az)
               caz=DCOS(az)
C              Get height above the plane of the ring using a random deviate
C              drawn from density profile of the laye.
               z=fdev(ltype,nran,iran,isd)*z0tmp
C              Get position in the plane of the sky with respect to the major
C              and minor axes of the spiral galaxy.
               x=r*caz
               y=r*saz*cinctmp-z*sinctmp
C              Get grid of this position, check if it is inside area of BOX=.
               grid(1)=IDNINT(pos(1)+(x*spatmp-y*cpatmp)/cdelt(1))
               IF (grid(1).LT.blo(1).OR.grid(1).GT.bhi(1)) GOTO 105
               grid(2)=IDNINT(pos(2)+(x*cpatmp+y*spatmp)/cdelt(2))
               IF (grid(2).LT.blo(2).OR.grid(2).GT.bhi(2)) GOTO 105
C              Get profile number of current pixel.
               iprof=(grid(2)-blo(2))*bsize(1)+grid(1)-blo(1)+1
C              Check if position is inside range of positions of profiles
C              that are currently being done. 
               IF (iprof.LE.nprofdone.OR.
     *             iprof.GT.nprofdone+nprof) GOTO 105
C              If outside any of the ranges, jump to next cloud.
C              Get systematic velocity of cloud.
               vsys=vsystem+vrottmp*caz*sinctmp
C              Build velocity profile.
               DO 104 iv=1,nvtmp
C                 Get deviate drawn from gaussian velocity profile and add
C                 to the systematic velocity.
                  v=vsys+gasdev(isd,nran,iran)*vdisptmp
C                 Get grid of velocity along FREQ-OHEL or VELO axis.
                  subsgrid=IDNINT(velgrid(axtyp,
     *                                    velsys,
     *                                    crval3,
     *                                    cdelt3,
     *                                    drval3,
     *                                    freq0,
     *                                    v))
C                 Get subsetnumber of grid.
                  isubs=subsnuma(subsgrid-icrpix3)
C                 For a grid that is not in the range of subsets in the
C                 outputset the value of subnuma is zero. If so jump
C                 to next velocity profile contribution.
                  IF (isubs.EQ.0) GOTO 104
                  idat=(iprof-nprofdone)+(isubs-1)*nprof
C                 Convert HI atom flux per pixel to flux per pixel of 21cm
C                 radiation expressed in W.U. and add subcloud to the data
C                 buffer.
                  datbuf(idat)=datbuf(idat)+fluxsc*cd2i(isubs)
104            CONTINUE
105         CONTINUE
106      CONTINUE
         CALL status('Writing data.')
C        Write data to output set.
         DO 201 isubs=1,nsubs
            idat1=1+(isubs-1)*nprof
            CALL gdsi_write(outset,
     *                      cwlo(isubs),
     *                      cwhi(isubs),
     *                      datbuf(idat1),
     *                      nprof,
     *                      ndum,
     *                      tid(isubs))
            CALL minmax3(datbuf(idat1),
     *                   nprof,
     *                   datmin(isubs),
     *                   datmax(isubs),
     *                   nblank(isubs),
     *                   count(isubs))
201      CONTINUE
C        Update number of profiles that has been done.
         nprofdone=nprofdone+nprof
C     Check if all profiles have been done.
      IF (nprofdone.LT.bsize(1)*bsize(2)) GOTO 100

C     Write maxima and minima to the header of the output set.
      CALL wminmax(outset,
     *             outsubs,
     *             datmin,
     *             datmax,
     *             nblank,
     *             nsubs,
     *             1)

C     Exit hermes.
      CALL finis

      STOP
      END

C     Function to transform a velocity to a grid.
      DOUBLE PRECISION FUNCTION velgrid(axtyp,
     *                                  velsys,
     *                                  crval3,
     *                                  cdelt3,
     *                                  drval3,
     *                                  freq0,
     *                                  v)
      INTEGER axtyp
      INTEGER velsys
      DOUBLE PRECISION crval3,cdelt3,drval3
      DOUBLE PRECISION freq0
      DOUBLE PRECISION v
      DOUBLE PRECISION fdv
      DOUBLE PRECISION c
      PARAMETER(c=2.99792458D8)

      IF (axtyp.EQ.3) THEN
C        Frequency axis.
         IF (velsys.EQ.1) THEN
C           Optical definition of velocities.
            fdv=(drval3-v)*crval3
            velgrid=crval3*fdv/((freq0*c-fdv)*cdelt3)
         ELSE IF (velsys.EQ.2) THEN
C           Radio definition of velocities.
            velgrid=(drval3-v)*freq0/(c*cdelt3)
         END IF
      ELSE IF (axtyp.EQ.4) THEN
C        Velocity axis.
         velgrid=(v-crval3)/cdelt3
      ELSE
C        Invalid axis.
         CALL error(4,'Invalid frequency/velocity system.')
      END IF
      RETURN
      END

C     Subroutine to get the rings and to 'regrid' them.
C     Output is a collection of nr rings with a separation radsep.
C     Lengths are in (arcmin)
C     Velocities are in (m/s)
C     Columndentities in units of (1E20 HI atoms per cm2)
C     Angles in (radians)
      SUBROUTINE ringio(radsep,
     *                  maxnr,
     *                  nr,
     *                  radii,
     *                  vrot,
     *                  vdisp,
     *                  dens,
     *                  z0,
     *                  inc,
     *                  phi)

      DOUBLE PRECISION radsep
      INTEGER maxnr
      INTEGER nr,ir
      REAL radii(maxnr)
      REAL vrot(maxnr)
      REAL vdisp(maxnr)
      REAL dens(maxnr)
      REAL z0(maxnr)
      REAL inc(maxnr)
      REAL phi(maxnr)

      INTEGER maxnur
      PARAMETER (maxnur=128)
      INTEGER nur,iur
      LOGICAL empty
      REAL uradii(maxnur)
      REAL uvrot(maxnur),uvdisp(maxnur)
      REAL udens(maxnur)
      REAL uz0(maxnur)
      INTEGER nrestr
      REAL uinc(maxnur),uphi(maxnur)

      REAL dr
      REAL dur
      REAL dvrotdr,dvdispdr
      REAL ddensdr,dz0dr
      REAL dincdr,dphidr

      INTEGER nel
      CHARACTER*80 mes

      INTEGER userint,userreal,userlog
      INTEGER def

      DOUBLE PRECISION  pi
      PARAMETER(pi=3.141592653589793D0)
      DOUBLE PRECISION deg2rad
      PARAMETER(deg2rad=pi/180.0D0)

      mes='Give radii of rings. (arcsec)'
      def=0
1     nur=userreal(uradii,
     *             maxnur,
     *             def,
     *             'RADII=',
     *             mes)
C     Convert to (arcmin)
      uradii(1)=uradii(1)/60.0
      DO 2 iur=2,nur
         uradii(iur)=uradii(iur)/60.0
         IF (uradii(iur-1)+radsep.GE.uradii(iur)) THEN
            IF (uradii(iur-1).GT.uradii(iur)) THEN
               mes='Radii not in increasing order.'
            ELSE
               mes='Radius separation too small.'
            END IF
            CALL cancel('RADII=')
            GOTO 1
         END IF
2     CONTINUE

      IF (uradii(1).NE.0.0) THEN
         mes='Leave innerpart of first ring empty ? [y]'
         def=1
         empty=.true.
         nel=userlog(empty,
     *               1,
     *               def,
     *               'EMPTY=',
     *               mes)
      END IF

      mes='Give circular velocities. (km/s)'
      def=0
      nel=userreal(uvrot,
     *             nur,
     *             def,
     *             'VROT=',
     *             mes)
      DO 3 iur=1,nur
              uvrot(iur)=uvrot(iur)*1.0E3
              IF (iur.GT.nel) uvrot(iur)=uvrot(nel)
3     CONTINUE

      mes='Give velocity dispersions. (km/s)'
      def=0
4     nel=userreal(uvdisp,
     *             nur,
     *             def,
     *             'VDISP=',
     *             mes)
      DO 5 iur=1,nur
        IF (iur.LE.nel) THEN
          IF (uvdisp(iur).LT.0.0) THEN
            mes='Negative velocity dispersion not allowed.'
            CALL cancel('VDISP=')
            GOTO 4
          END IF
        END IF
        uvdisp(iur)=uvdisp(iur)*1.0E3
        IF (iur.GT.nel) uvdisp(iur)=uvdisp(nel)
5     CONTINUE 

      mes='Give column-densities of rings. (HI atoms/cm2)'
      def=0
6     nel=userreal(udens,
     *             nur,
     *             def,
     *             'DENS=',
     *             mes)
      DO 7 iur=1,nur
        IF (iur.LE.nel) THEN
          IF (udens(iur).LT.0.0) THEN
            mes='Negative column-density not allowed.'
            CALL cancel('DENS=')
            GOTO 6
          END IF
        END IF
C       Convert to units of 1E20 HI atoms per cm2.
        udens(iur)=udens(iur)/1.0E20
        IF (iur.GT.nel) udens(iur)=udens(nel)
7     CONTINUE

      mes='Give scale-height (Z0) of rings. (arcsec)'
      def=0
8     nel=userreal(uz0,
     *             nur,
     *             def,
     *             'Z0=',
     *             mes)
      DO 9 iur=1,nur
        IF (iur.LE.nel) THEN
          IF (uz0(iur).LT.0.0) THEN
            mes='Negative scale height not allowed.'
            CALL cancel('Z0=')
            GOTO 8
          END IF
        END IF
C       Convert to (arcmin).
        uz0(iur)=uz0(iur)/60.0
        IF (iur.GT.nel) uz0(iur)=uz0(nel)
9     CONTINUE

      mes='Number of rings with fixed inc. and pa. [0]'
      def=1
10    nrestr=0
      nel=userint(nrestr,
     *            1,
     *            def,
     *            'NRESTR=',
     *            mes)
      IF (nrestr.LT.0) THEN
         mes='NRESTR must be positive.'
         CALL cancel('NRESTR=')
         GOTO 10
      END IF
      
      mes='Give inclinations of rings. (degrees)'
      def=0
      nel=userreal(uinc(nrestr+1),
     *             nur,
     *             def,
     *             'INCL=',
     *             mes)
      DO 11 iur=1,nur
         IF (iur.LE.nrestr) uinc(iur)=uinc(nrestr+1)
         uinc(iur)=uinc(iur)*deg2rad
         IF (iur.GE.nel+1+nrestr) uinc(iur)=uinc(nel)
11    CONTINUE

      mes='Give position angles of rings. (degrees)'
      def=0
      nel=userreal(uphi(nrestr+1),
     *             nur,
     *             def,
     *             'PA=',
     *             mes)
      DO 12 iur=1,nur
         IF (iur.LE.nrestr) uphi(iur)=uphi(nrestr+1)
         uphi(iur)=uphi(iur)*deg2rad
         IF (iur.GT.nel+nrestr) uphi(iur)=uphi(nel)
12    CONTINUE

      ir=1
      IF (uradii(1).NE.0..AND.empty) THEN
         radii(ir)=uradii(1)+radsep/2.0D0
      ELSE
         radii(ir)=radsep/2.0D0
100      IF (radii(ir).LT.uradii(1)) THEN
             vrot(ir)=uvrot(1)
             vdisp(ir)=uvdisp(1)
             dens(ir)=udens(1)
             z0(ir)=uz0(1)
             inc(ir)=uinc(1)
             phi(ir)=uphi(1)
             ir=ir+1
             radii(ir)=radii(ir-1)+radsep
             GOTO 100
         END IF
      END IF

      DO 120 iur=2,nur
         dur=uradii(iur)-uradii(iur-1)
         dvrotdr=(uvrot(iur)-uvrot(iur-1))/dur
         dvdispdr=(uvdisp(iur)-uvdisp(iur-1))/dur
         ddensdr=(udens(iur)-udens(iur-1))/dur
         dz0dr=(uz0(iur)-uz0(iur-1))/dur
         dincdr=(uinc(iur)-uinc(iur-1))/dur
         dphidr=(uphi(iur)-uphi(iur-1))/dur
110      IF (radii(ir).LT.uradii(iur)) THEN
            dr=radii(ir)-uradii(iur-1)
            vrot(ir)=uvrot(iur-1)+dvrotdr*dr
            vdisp(ir)=uvdisp(iur-1)+dvdispdr*dr
            dens(ir)=udens(iur-1)+ddensdr*dr
            z0(ir)=uz0(iur-1)+dz0dr*dr
            inc(ir)=uinc(iur-1)+dincdr*dr
            phi(ir)=uphi(iur-1)+dphidr*dr
            ir=ir+1
            radii(ir)=radii(ir-1)+radsep
            GOTO 110
         END IF
120   CONTINUE
      nr=ir-1
      RETURN
      END
C     
C     Function to get random deviates for various functions.
C     The double precision variable fdev contains the random deviate.
C     If nran is within a factor two of the largest possible integer
C     then integer overflow could occur.
      DOUBLE PRECISION FUNCTION fdev(option,nran,iran,idum)
C     Type of distribution function for the random deviates.
C     Options:
C              1 -- Gaussian deviates.
C              2 -- Sech2 deviates.
C              3 -- Exponential deviates.
C              4 -- Lorentzian deviates.
C              5 -- Box deviates.
C     
      INTEGER option
C     The range of the random number generator should be from zero to nran-1.
      INTEGER nran,iran,idum
C     The random number generator.
C     Iran is a random number generator; it should return an integer in the
C     range [0,nran>; its seed is idum.
      EXTERNAL iran
C     The function to generate the gaussian deviates.
      DOUBLE PRECISION gasdev
C     Constants.
      DOUBLE PRECISION pi,pio2
      PARAMETER(pi=3.141592653589793D0)
      PARAMETER(pio2=pi/2.0D0)
C     The inverse of the indefinite integral of the sech2 function.
      DOUBLE PRECISION x,atanh
      atanh(x)=DLOG(DSQRT((1.0D0+x)/(1.0D0-x)))
      IF (option.EQ.1) THEN
C        Gaussian function: exp(-0.5*x**2)
         fdev=gasdev(idum,nran,iran)
      ELSE
C        Get random deviate between -1 and 1. Not including the boundaries.
         x=DBLE(1+2*iran(idum)-nran)/DBLE(nran)
         IF (option.EQ.2) THEN
C           Sech2 function: sech2(x)
            fdev=atanh(x)
         ELSE IF (option.EQ.3) THEN
C          Exponential function: exp(-|x|)
           IF (x.GT.0.0D0) fdev=-DLOG(x)
           IF (x.LT.0.0D0) fdev= DLOG(-x)
         ELSE IF (option.EQ.4) THEN
C          Lorentzian function: 1/(1+x**2)
           fdev=DTAN(pio2*x)
         ELSE IF (option.EQ.5) THEN
C          Box function.
           fdev=x
         ELSE
           CALL error(4,'Unknown function.')
         END IF
      END IF
      RETURN
      END
C     Function to get random deviates from a gaussian distribution.
C     Drawn from 'Numerical Recipes' but modified a bit.
C     If nran is within a factor two of the largest possible integer
C     then integer overflow could occur.
      DOUBLE PRECISION FUNCTION GASDEV(IDUM,NRAN,IRAN)
      DOUBLE PRECISION V1,V2,R
      DOUBLE PRECISION FAC,GSET
      EXTERNAL IRAN
      SAVE
      DATA ISET/0/
      IF (ISET.EQ.0) THEN
1        V1=DBLE(1+2*IRAN(IDUM)-NRAN)/DBLE(NRAN)
         V2=DBLE(1+2*IRAN(IDUM)-NRAN)/DBLE(NRAN)
         R=V1*V1+V2*V2
         IF (R.GE.1.0D0.OR.R.EQ.0.0D0) GOTO 1
         FAC=DSQRT(-2.0D0*DLOG(R)/R)
         GSET=V1*FAC
         GASDEV=V2*FAC
         ISET=1
      ELSE
         GASDEV=GSET
         ISET=0
      END IF
      RETURN
      END
C     Random number generator due to Knuth.
C     The choices for MBIG and MSEED are not particularly important since the
C     they are only used 55 times in a linear congruential algorithm to
C     initialize the array MA.
      INTEGER FUNCTION IRAN(IDUM)
C     Concerning the precision of floating point values the value of
C     MBIG is 2**24. 
      PARAMETER(MBIG=16777216)
C     The returned integer is in the interval [MZ,MBIG>.
      PARAMETER(MSEED=1618033)
      PARAMETER(MZ=0)
C     All constants defined in parameter statements are free to choose, as
C     long as MBIG is bigger than the rest and MSEED is still big.
C     Other constants in the routine should not be modified. 
      DIMENSION MA(55)
      SAVE
C     For negative idum initialize.
      IF (IDUM.LT.0) THEN
C        Filling the array MA().
         IRAN=MSEED-IABS(IDUM)
         IRAN=MOD(IRAN,MBIG)
         MA(55)=IRAN
         MK=1
         DO 11 I=1,54
            II=MOD(21*I,55)
            MA(II)=MK
            MK=IRAN-MK
            IF (MK.LT.MZ) MK=MK+MBIG
            IRAN=MA(II)
11       CONTINUE
C        Warming up the generator. (Shuffling the array.)
         DO 13 K=1,4
            DO 12 I=1,55
               MA(I)=MA(I)-MA(1+MOD(I+30,55))
               IF (MA(I).LT.MZ) MA(I)=MA(I)+MBIG
12          CONTINUE
13       CONTINUE
         INEXT=0
         INEXTP=31
C        Set IDUM to not initializing.
         IDUM=1
      END IF
C     If not initializing start here.
      INEXT=INEXT+1
      IF (INEXT.EQ.56) INEXT=1
      INEXTP=INEXTP+1
      IF (INEXTP.EQ.56) INEXTP=1
      IRAN=MA(INEXT)-MA(INEXTP)
      IF (IRAN.LT.MZ) IRAN=IRAN+MBIG
      MA(INEXT)=IRAN
      RETURN
      END

