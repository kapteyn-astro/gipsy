      DOUBLE PRECISION FUNCTION randev( option,iseed )
C-------------------------------------------------------------
C#>            randev.dc2
C
CFunction:     RANDEV
C
CPurpose:      Generating random deviates from various distributions.
C
CFiles:        randev.f
C
CAuthor:       F.J. Sicking
C
CCategory:     MATH
C
CUse:          DOUBLE PRECISION RANDEV( option ,   Input  INTEGER
C                                       iseed  ,   Input  INTEGER
C                                     )
C
C              RANDEV  Returns a random deviate from the distribution 
C                      specified in option.
C
C              OPTION  Type of distribution:
C                      1 -- Gaussian:     exp(-0.5*x**2)
C                      2 -- Sech2:        1/cosh^2(x)
C                      3 -- Exponential:  exp(-|x|)
C                      4 -- Lorentz:      1/(1+x^2)
C                      5 -- Rectangular:  -1 < x < 1
C
C              ISEED   Seed for the random number generator. Should be
C                      negative on initialization !!!!
C
CDescription:
C              The random number generator and the function to get
C              the gaussian deviates are drawn from Numerical Recipes.
C
CUpdates:      Aug  4, 1994: FJS, Document created.
C              Ocf  5, 1994: KGB, Incompatible routine declaration removed.
C
C#<
C
C@ double precision function randev( integer, integer )
C     
      INTEGER option
C     The range of the random number generator should be from zero to nran-1.
      INTEGER nran,iran,iseed
      PARAMETER(nran=16777216)
      EXTERNAL iran
C     The random number generator.
C     Iran is a random number generator; it should return an integer in the
C     range [0,nran>; its seed is iseed.
C     The function to generate the gaussian deviates.
      DOUBLE PRECISION gasdev
C     Constants.
      DOUBLE PRECISION pi,pio2
      PARAMETER(pi=3.141592653589793D0)
      PARAMETER(pio2=pi/2.0D0)
C     The inverse of the indefinite integral of the sech2 function.
      DOUBLE PRECISION x,atanh
      atanh(x)=DLOG(DSQRT((1.0D0+x)/(1.0D0-x)))
C
      IF (option.EQ.1) THEN
C        Gaussian function: exp(-0.5*x**2)
         randev=gasdev(iseed)
      ELSE
C        Get random deviate between -1 and 1. Not including the boundaries.
         x=DBLE(1+2*iran(iseed)-nran)/DBLE(nran)
         IF (option.EQ.2) THEN
C           Sech2 function: sech2(x)
            randev=atanh(x)
         ELSE IF (option.EQ.3) THEN
C          Exponential function: exp(-|x|)
           IF (x.GT.0.0D0) randev=-DLOG(x)
           IF (x.LT.0.0D0) randev= DLOG(-x)
         ELSE IF (option.EQ.4) THEN
C          Lorentzian function: 1/(1+x**2)
           randev=DTAN(pio2*x)
         ELSE IF (option.EQ.5) THEN
C          Box function.
           randev=x
         ELSE
           CALL error(4,'Unknown function.')
         END IF
      END IF
      RETURN
      END

      DOUBLE PRECISION FUNCTION gasdev( iseed )
C-------------------------------------------------------------
C#>            gasdev.dc2
C
CFunction:     GASDEV
C
CPurpose:      Generating gaussian random deviates.
C
CFiles:        gasdev.f
C
CAuthor:       F.J. Sicking
C
CCategory:     MATH
C
CUse:          DOUBLE PRECISION GASDEV( iseed  ,   Input  INTEGER
C                                     )
C
C              GASDEV  Returns a gaussian random deviate.
C
C              ISEED   Seed for the random number generator. Should be
C                      negative on initialization !!!!
C
CDescription:
C              Drawn from numerical recipes. 
C
CUpdates:      Aug 4, 1994: FJS, Document created.
C
C#<
C
C@ double precision function gasdev( integer )
C     
      DOUBLE PRECISION V1,V2,R
      DOUBLE PRECISION FAC,GSET

      INTEGER NRAN
      PARAMETER(NRAN=16777216)

      SAVE
      DATA ISET/0/
      IF (ISET.EQ.0) THEN
1        V1=DBLE(1+2*IRAN(ISEED)-NRAN)/DBLE(NRAN)
         V2=DBLE(1+2*IRAN(ISEED)-NRAN)/DBLE(NRAN)
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

      INTEGER FUNCTION iran( iseed )
C-------------------------------------------------------------
C#>            iran.dc2
C
CFunction:     IRAN
C
CPurpose:      Generating random integer.
C
CFiles:        iran.f
C
CAuthor:       F.J. Sicking
C
CCategory:     MATH
C
CUse:          INTEGER IRAN( iseed  ,   Input  INTEGER
C                                     )
C
C              IRAN  Returns a random integer from the interval [0,2^24>.
C
C              ISEED   Seed for the random number generator. Should be
C                      negative on initialization !!!!
C
CDescription:
C              Drawn from numerical recipes. (Knuth)
C
CUpdates:      Aug 4, 1994: FJS, Document created.
C
C#<
C
C@ integer function iran( integer )
C
C     The choices for MBIG and MSEED are not particularly important since the
C     they are only used 55 times in a linear congruential algorithm to
C     initialize the array MA.
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
C     For negative iseed initialize.
      IF (ISEED.LT.0) THEN
C        Filling the array MA().
         IRAN=MSEED-IABS(ISEED)
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
C        Set iseed to not initializing.
         ISEED=1
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
