C----------------------------------------------------
      SUBROUTINE ADSCOR(II)
      INTEGER SAVAR(400)
      LOGICAL DEBUG
      COMMON  DEBUG, ISEED
      COMMON /BLOCK2/ SAVAR
      II = 0
           IF (SAVAR( 4) .EQ. 72) II=II+9
           IF (SAVAR( 7) .EQ. 72) II=II+10
           IF (SAVAR(12) .EQ. 72) II=II+10
           IF (SAVAR(17) .EQ. 72) II=II+10
           IF (SAVAR(18) .EQ. 81) II=II+1
           IF (SAVAR(19) .EQ. 72) II=II+10
           IF (SAVAR(23) .EQ. 72) II=II+10
           IF (SAVAR(24) .EQ. 72) II=II+10
           IF (SAVAR(28) .EQ. 72) II=II+10
           IF (SAVAR(29) .EQ. 72) II=II+10
           IF (SAVAR(30) .EQ. 72) II=II+10
C Score is dependent on the number of moves.
      ITEMP = SAVAR(90) - 250
      IF (ITEMP .GE. 0) II = II - (ITEMP/5)
      IF (DEBUG) WRITE(6,8001) II
 8001 FORMAT('0  RESULT OF "ADSCOR" IS:', I3)
      RETURN
      END
