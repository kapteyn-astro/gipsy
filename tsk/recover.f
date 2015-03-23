*#>recover.dc1
*Program:      RECOVER
*
*Purpose:      Recover a corrupt GDS set
*
*Category:     UTILITY
*
*File:         recover.f
*
*Author:       J.P. Terlouw
*
*Keywords:     INSET=   Name of set to be recovered.
*
*Description:  RECOVER attempts to recover a corrupt descriptor file.
*              If recovery is not possible, the program exits with a
*              fatal error message.
*
*Warning:      This program may result in loss of data in the descriptor file.
*              It should only be used when it is impossible to retrieve a
*              correct version of the descriptor file, e.g. from a backup tape.
*
*Updates:      May  8, 1995: JPT, Document created.
*#<

      PROGRAM RECOVER

      CHARACTER*30 SET
      CHARACTER*50 GDS_ERRSTR
      INTEGER      USERCHAR, GDS_RECOVER

      CALL INIT
      N = USERCHAR(SET, 1, 0, 'INSET=', 'Name of set to be recovered')
      IERR = GDS_RECOVER(SET)
      IF (IERR.LT.0) CALL ERROR(4,GDS_ERRSTR(IERR))
      CALL FINIS
      END
