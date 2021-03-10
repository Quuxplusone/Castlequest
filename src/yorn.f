      SUBROUTINE YORN(II)
      CHARACTER ANS
C
   10 WRITE(6,2000)
 2000 FORMAT(' ')
      READ(5,1001,END=30,ERR=30) ANS
 1001 FORMAT(A1)
C
      II = 1
      IF (ANS .EQ. 'Y') GOTO 100
      IF (ANS .NE. 'N') GOTO 30
           II = 0
           GOTO 100
   30 CONTINUE
      WRITE(6,2001)
 2001 FORMAT('0  Please answer YES or NO !')
      GOTO 10
C
  100 CONTINUE
      RETURN
      END
