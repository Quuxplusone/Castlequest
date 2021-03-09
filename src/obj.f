C--------------------------------------------------------------
      SUBROUTINE OBJ(ITEMS,ROOM,SCORE,VALUE)
      INTEGER ITEMS(30), ROOM, VALUE(30), SCORE
      COMMON /BLOCK2/ SAVAR(400)
C
      EQUIVALENCE (SAVAR(86), PREC)
      LOGICAL PREC
C
      IF (PREC) ITEMS( 9)=0
      IF (PREC) ITEMS(16)=0
      DO 10 I=1,30
           IF (ITEMS(I) .NE. ROOM) GOTO 10
                II = 600 + I
                SCORE = SCORE + VALUE(I)
                VALUE(I) = 0
                CALL DES(II)
   10 CONTINUE
      IF (.NOT. PREC) GOTO 20
           ITEMS( 9)=ROOM
           ITEMS(16)=ROOM
   20 CONTINUE
      RETURN
      END
