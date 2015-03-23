Cgausscube.f
C
C        Copyright (c) Kapteyn Laboratorium Groningen 1992
C        All Rights Reserved.
C
C#>            gausscube.dc1
C
CProgram:      GAUSSCUBE
C
CPurpose:      Making the full datacube of the results of a gaussfit.
C              
C
CCategory:     MODELS, FITTING
C
CFile:         gausscube.f
C
CAuthor:       F.J. Sicking
C
CKeywords:
C
C   INSET=     The set that contains the data. It is used to get the
C              coordinate system. The axis along which the subsets are
C              specified is used as 'gaussfit' axis.
C
C   PARSET=    The set containing the parameters found by the gaussfit.
C              It must be a set with the subsets defined along a 
C              parameter axis. The parameters are the baselevel (may be
C              absent), the normalization, the mean and the width of the
C              gaussian components. The subsets of this set must have the
C              same coordinate system and size as the subsets of inset.
C              The number of subsets determines the number of gauss
C              components and if the baselevel was fitted. As number of
C              components, the integer division of the number of subsets and
C              three is used. The remainder of this division indicates
C              if baselevels are present or not. If it is one, they are, if
C              zero, they are not, and otherwise an error message is returned. 
C              The subsets are used as follows:
C              (subset 1 :  baselevel. (It does not need to be in PARSET.
C                           If it is not the baselevels are assumed to be
C                           zero.))
C              subset 2 :   normalization of the first component.
C                           (If the baselevels are not present the
C                           normalization will be taken from subset 1 etc.)
C              subset 3 :   mean value of the first component.
C              subset 4 :   width of the first component.
C              subset 5 :   normalization of the second component. Etc.
C
C   BOX=       Area of operation. [entire subset]
C
C   OUTSET=    The set the routine uses for the output, the model datacube.
C
C   AMPLITUDE= Specifies whether the normalization is the amplitude [y] or the
C              intensity (n). (amplitude=intensity/(sqrt(2*pi)*sigma))
C
C   FWHM=      Specifies if the width is the full width half maximum [y] or the
C              dispersion (n). (FWHM=2.35482*sigma).
C
CDescription:  The general idea is making a data cube of a function that
C              depends on: the physical coordinate along which the subsets
C              are defined, the datavalues and npar parameters which may be
C              dependent on position in the subsets. The most common case,
C              for which this routine is made, is making the full data cube
C              of the results of a gaussfit. This datacube then can be
C              subtracted from the original data to get the residuals which
C              can be studied to see if the gaussfit was succesful.
C
CUpdates:      Jul 23, 1992: FJS, Document created.
C              Dec  1, 1992: VOG, Category added.
C              Sep 10, 1993: FJS, Box option implemented.
C#<

      PROGRAM GAUSSCUBE

C     Declaration of variables.
C     Maximum number of axes in inset outset and parset.
      INTEGER maxnax
      PARAMETER(maxnax=10)
C     Number of axes in the datasets and counter.
      INTEGER nax,iax
C     Axis permutations in sets and lengths of the axes.
      INTEGER inaxperm(maxnax),outaxperm(maxnax),paraxperm(maxnax)
      INTEGER inaxcount(maxnax),outaxcount(maxnax),paraxcount(maxnax)
C     Names of the inputset, the outputset and the set with the parameters
C     of the gaussian components.
      CHARACTER*80 inset, outset, parset
C     Names of the subsetaxes in inset and parset.
      CHARACTER*80 inaxnam,paraxnam

C     Dimension of subsets.
      INTEGER subdim
C     Sizes of subsets and area of operation.
      INTEGER flo(maxnax),fhi(maxnax)
      INTEGER blo(maxnax),bhi(maxnax)
C     Names of the axes in in- and par-set.
C     Maximum number of subsets in inset and outset.
      INTEGER maxnsubs
      PARAMETER(maxnsubs=256)
C     Maximum number of parameters in the function.
C     The value implies that the maximum number of gaussian components is
C     equal to: (maxnpar-1)/3 = 5.
      INTEGER maxnpar
      PARAMETER(maxnpar=16)
C     The number of subsets in inset,outset and parset and also the number
C     of parameters.
      INTEGER nsubs, npar
C     Counters for subsets and parameters.
      INTEGER isubs,ipar
C     Coordinate words of subsets.
      INTEGER insubs(maxnsubs),outsubs(maxnsubs),parsubs(maxnsubs)
C     Fixed physical coordinate of subsets.
      REAL phys(maxnsubs)
C     Lower and higher coordinate words for the frames in inset, outset and
C     parset that have to be used.
      INTEGER incwlo(maxnsubs),incwhi(maxnsubs)
      INTEGER outcwlo(maxnsubs),outcwhi(maxnsubs)
      INTEGER parcwlo(maxnsubs),parcwhi(maxnsubs)
C     Counters for data values in subsets, number of blanks and minimum and
C     maximum datavalues.
      INTEGER count(maxnsubs),nblank(maxnsubs)
      REAL datmax(maxnsubs),datmin(maxnsubs)

C     In and output transfer identifiers.
C      INTEGER tidi(maxnsubs)
      INTEGER tido(maxnsubs)
      INTEGER tidpar(maxnpar)
C     Maximum number of elements on databuffers.
      INTEGER maxndat
      PARAMETER(maxndat=2048)
C     Numnber of datavalues read or to be written.
      INTEGER nread
C     The data buffers.
C-C      REAL indat(maxndat)
      REAL outdat(maxndat),pardat(maxndat,maxnpar)

C     Keywords telling that the normailzation is the ampplitude of the
C     gaussian component and the width is the fwhm.
      LOGICAL amplitude,fwhm

C     Dummy variables.
C     General dummy integer.
      INTEGER ndum
C     Dummy grid for cotrans. Used to get the fixed physical coordinate of
C     the subsets in inset and outset.
      DOUBLE PRECISION dumgrid(maxnax),dumphys(maxnax)
C     Contains messages to the user.
      CHARACTER*80 message

C     Containes returned error by some gipsy functions. 
      INTEGER err
C     Used gipsy system functions.
      INTEGER gdsinp,gdsout
      INTEGER userlog
      INTEGER gdsc_ndims,gdsc_grid,gdsc_fill
      CHARACTER*80 gdsc_name
      INTEGER cotrans

C     Start of executable code.

      CALL init
C     Contacts hermes.

      subdim=0
C     Get inputset and subsets.
      message='Give input set and subsets.'
      nsubs=gdsinp(inset,
     *             insubs,
     *             maxnsubs,
     *             0,
     *             'INSET=',
     *             message,
     *             0,
     *             inaxperm,
     *             inaxcount,
     *             maxnax,
     *             1,
     *             subdim)
C     The value of subdim defines the dimension of the subsets from now on.
      nax=gdsc_ndims(inset,0)
      message='More than one or no fit axis in inset.'
      IF ((nax-subdim).NE.1) CALL error(4,message)

C     Get the set with the gaussfit, again one free subsetaxis is required.
      message='Give set and subsets with parameters of gaussfit.'
      npar=gdsinp(parset,
     *            parsubs,
     *            maxnpar,
     *            0,
     *            'PARSET=',
     *            message,
     *            0,
     *            paraxperm,
     *            paraxcount,
     *            maxnax,
     *            1,
     *            subdim)
C     Check if the number of subsets in parset is valid for this routine.
      message='Improper number of parameters.'
      IF (MOD(npar,3).EQ.2) CALL error(4,message)
     
C     Check if the subsets in inset and in parset are of the same size.
C     Check if the axes of the subsets have the same names.
      CALL gdsc_range(inset,insubs(1),incwlo(1),incwhi(1),err)
C     Get lower and higher coordinate words of the subsets in parset.
      CALL gdsc_range(parset,parsubs(1),parcwlo(1),parcwhi(1),err)
      DO 1 iax=1,subdim
         inaxnam=gdsc_name(inset,inaxperm(iax),err)
         paraxnam=gdsc_name(parset,paraxperm(iax),err)
         IF (paraxnam.NE.inaxnam) THEN
            WRITE(message,
     *      '(''Name axis '',I2,'' is not equal in IN and PARSET'')')iax
            CALL error(2,message)
         END IF
         flo(iax)=gdsc_grid(inset,iax,incwlo(1),err)
         fhi(iax)=gdsc_grid(inset,iax,incwhi(1),err)
         message='Subsets of IN- and PAR- SET have unequal size.'
         IF (flo(iax).NE.gdsc_grid(parset,iax,parcwlo(1),err)) 
     *   CALL error(4,message)
         IF (fhi(iax).NE.gdsc_grid(parset,iax,parcwhi(1),err))
     *   CALL error(4,message)
1     CONTINUE      

C     Get area of operation.
      message='Give area of operation. [entire subset]'
      CALL gdsbox(blo,
     *            bhi,
     *            inset,
     *            insubs(1),
     *            1,
     *            'BOX=',
     *            message,
     *            0,
     *            0)

C     Assigns inset to outset. Transfers the coordinate system of inset to
C     outset.
      CALL gdsasn('INSET=','OUTSET=',1)

C     Get the outputset from the user. The coordinatesystem and the number of
C     subsets should be the same as for inset.
C     Using the +4 for the default states that the number of subsets must be
C     exactly equal to nsubs.
      message='Give output (sub)sets.'
      ndum=gdsout(outset,
     *            outsubs,
     *            nsubs,
     *            4,
     *            'OUTSET=',
     *            message,
     *            0,
     *            outaxperm,
     *            outaxcount,
     *            maxnax)

C     Get, for each subset, the upper and lower coordinate words of the
C     area that will be read from parset and written to outset.
      DO 2 ipar=1,npar
         parcwlo(ipar)=gdsc_fill(parset,parsubs(ipar),blo)
         parcwhi(ipar)=gdsc_fill(parset,parsubs(ipar),bhi)
2     CONTINUE
      DO 3 isubs=1,nsubs
         count(isubs)=0
C-C         incwlo(isubs)=gdsc_fill(inset,insubs(isubs),blo)
C-C         incwhi(isubs)=gdsc_fill(inset,insubs(isubs),bhi)
         outcwlo(isubs)=gdsc_fill(outset,outsubs(isubs),blo)
         outcwhi(isubs)=gdsc_fill(outset,outsubs(isubs),bhi)
3     CONTINUE

      amplitude=.true.
      message='Normalization: amplitude [y], intensity (n)'
      ndum=userlog(amplitude,1,5,'AMPLITUDE=',message)

      fwhm=.true.
      message='Width: FWHM [y], dispersion (n)'
      ndum=userlog(fwhm,1,5,'FWHM=',message)

C     To calculate the values of the gaussian components the routine needs
C     the fixed physical coordinates of the subsets in inset. To find them
C     an arbitrary grid position in the subset (specified by its coordinate
C     word) is transformed to the physical coordinates.

C     Define arbitrary grid.
      DO 4 iax=1,subdim
         dumgrid(iax)=0.0D0
         dumphys(iax)=0.0D0
4     CONTINUE

C     Do the coordinate transformation for each subset.
      DO 5 isubs=1,nsubs
         err=cotrans(inset,
     *               insubs(isubs),
     *               dumgrid,
     *               dumphys,
     *               1)
C        Store the value of the fixed physical subset coordinate.
         phys(isubs)=REAL(dumphys(subdim+1))
5     CONTINUE
C     In this routine the function is not dependent on the data in inset. If it
C     would be then the content of the subroutine model should be changed and
C     comment that contains code, indicated by C-C, should be activated.

C     Set in and output transferidentifiers.
      DO 6 isubs=1,nsubs
C-C         tidi(isubs)=0
         tido(isubs)=0
6     CONTINUE
      DO 7 ipar=1,npar
         tidpar(ipar)=0
7     CONTINUE

C     Start of repeat until construction. Stopped if all data values have
C     been done.
10    CONTINUE

C     Read nread sets of prameters into pardat(1:nread,1:n_parsubs)
      DO 11 ipar=1,npar
         CALL gdsi_read(parset,
     *                  parcwlo(ipar),
     *                  parcwhi(ipar),
     *                  pardat(1,ipar),
     *                  maxndat,
     *                  nread,
     *                  tidpar(ipar))
11    CONTINUE

C     Loop over the subsets of inset, read for each subset as many datavalues
C     as sets of parameters read at the earlier reading of parset.
      DO 12 isubs=1,nsubs
C     By removing the comment specifications the function can become 
C     dependent on the datavalue in inset. 
C-C         CALL gdsi_read(inset,
C-C     *                  incwlo(isubs),
C-C     *                  incwhi(isubs),
C-C     *                  indat,
C-C     *                  nread,
C-C     *                  ndum,
C-C     *                  tidi(isubs))

C       The subroutine model does nread model function evaluations
C       at the subset coordinate 'phys(isubs)' and writes the
C       results to outdat.
        CALL model(phys(isubs),
     *             maxndat,
C-C     *             indat,
     *             outdat,
     *             nread,
     *             maxnpar,
     *             pardat,
     *             npar,
     *             amplitude,
     *             fwhm)

C       Write the results of model to outset.
        CALL gdsi_write(outset,
     *                  outcwlo(isubs),
     *                  outcwhi(isubs),
     *                  outdat,
     *                  nread,
     *                  ndum,
     *                  tido(isubs))

C       Do running min., max. and number of blanks
        CALL minmax3(outdat,
     *               nread,
     *               datmin(isubs),
     *               datmax(isubs),
     *               nblank(isubs),
     *               count(isubs))

C       Go to the next subset.
12    CONTINUE

C     Go to the next number of sets of parameters.
      IF (tidpar(1).NE.0) GOTO 10

C     Update the new min., max. and number of blanks
      CALL wminmax(outset,
     *             outsubs,
     *             datmin,
     *             datmax,
     *             nblank,
     *             nsubs,
     *             1)

      CALL finis

      STOP
      END

C     This subroutine does n function evaluations of a function that depends
C     npar parameters and a value. For each evaluation the npar parameters
C     may vary.
C     The values are stored in the array indat and the parameters in the
C     two dimensional array pardat.
C     The function is a sum of one or more gaussian components.

      SUBROUTINE model(coord,
     *                 maxn,
C-C     *                 indat,
     *                 outdat,
     *                 n,
     *                 maxnpar,
     *                 pardat,
     *                 npar,
     *                 amplitude,
     *                 fwhm)

C     The physical coordinate, remains the same for all n evaluations.
      REAL coord
C     Size of the databuffers indat and outdat and of the first axis of pardat.
      INTEGER maxn
C     The input an output databuffers.
C-C      REAL indat(maxn)
      REAL outdat(maxn)
C     Number of values on the databuffers, and requested number of function
C     evaluations
      INTEGER n
C     Size of second axis of pardat and the actual number of parameters.
      INTEGER maxnpar,npar
C     The data buffer containing the parameters.
      REAL pardat(maxn,maxnpar)
C     These parameters tell how the normalization and the width of the 
C     components are defined. If amplitude is true the normalization of
C     the components is the amplitude. If fwhm is true then the width is the
C     fwhm.
      LOGICAL amplitude, fwhm
C     Conversion of width to dispersion resp. FWHM
      DOUBLE PRECISION tosigma,tofwhm
C     Number of gaussian components. Base is zero means that there are no base-
C     levels in pardat, base is one means there are.
      INTEGER ngauss, base
C     Counter for gaussian components. Index on the databuffer of 'zeroth'
C     component.
      INTEGER igauss,pgauss
C     Counter for function evaluations.
      INTEGER i
C     Contains the value of the sum of the gaussian components.
      DOUBLE PRECISION sumgauss
C     Contains the three parameters of each component.
      REAL g(3)
C     Counter for the parameters.
      INTEGER ipar
C     Dummy variable in exponential: z=(coord-g(2))/g(3)
      REAL z

C     Some constants:
C     sq2pi is the value of the SQRT(2*pi)
      DOUBLE PRECISION sq2pi
      PARAMETER(sq2pi=2.506628275D0)
C     The conversion of sigma to the full width half maximum.
      PARAMETER(tofwhm=2.354820045D0)
C     The system blank.
      REAL blank

C     Get the systemblank.
      CALL setfblank(blank)
C     Set tosigma.
      tosigma=1.0D0
      IF (fwhm) tosigma=tosigma/tofwhm
C     Get the number of gausscomponents and find out if the baselevel is
C     in pardat.
      ngauss=npar/3
      base=npar-ngauss*3
      IF (base.EQ.2) CALL error(4,'Invalid number of parameters.')
C     Loop over function evaluations.
      DO 1 i=1,n
         outdat(i)=blank
C        If the baselevel or the first component is a blank then apperantly no
C        gaussfit has been done and consequently the output is a blank and 
C        the next function evaluation will be done.
         IF (pardat(i,1).EQ.blank) GOTO 1
C        If the baselevels are in the fit then sumgauss is set to it before
C        adding gaussian components otherwise the base level is assumed to
C        be zero and consequently sumgauss is set to zero too.
         IF (base.EQ.0) THEN
            sumgauss=0.0D0
         ELSE
            sumgauss=DBLE(pardat(i,1))
         END IF
         DO 11 igauss=1,ngauss
C           The variable pgauss contains the index just before the first
C           parameter of the current component on the data buffer.
            pgauss=base+3*(igauss-1)
C           Store the normalization, the mean and the width of the gaussians
C           in g(1:3). If one of the parameters of the component is a blank
C           the component cannot be used and the routine jumps to the next.
            DO 111 ipar=1,3
               g(ipar)=pardat(i,pgauss+ipar)
               IF (g(ipar).EQ.blank) GOTO 11
111         CONTINUE
C           Only components with normalization and width greater than
C           zero are accepted. If an invalid component is encountered the
C           routine jumps to the next.
            IF (g(1).LE.0.0.OR.g(3).LE.0.0) GOTO 11
C           Convert the width to sigma.
            g(3)=g(3)*tosigma
C           Convert the normalization to amplitude.
            IF (.not.amplitude) g(1)=g(1)/(sq2pi*g(3))
C           Calculate the gaussian.
            z=(coord-g(2))/g(3)
            sumgauss=sumgauss+DBLE(g(1)*EXP(-0.5*z**2))
11       CONTINUE
C        Function evaluation complete, store result on outdat.
         outdat(i)=REAL(sumgauss)
C     Go to next function evaluation.
1     CONTINUE

      RETURN
      END

