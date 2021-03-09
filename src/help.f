C--------------------------------------------------------------
      SUBROUTINE HELP(II,OBJECT)
C
      INTEGER INST(50,20), FMT(20), FORM(100,20), FORM2(60,20)
      INTEGER HINT(50,20), LONG(400,20), INDEX(100), SAVAR(400)
      INTEGER OBJECT
C
C
      INTEGER ITEMS(30), VALUE(30), WHER(10), DOOR(100)
C
      INTEGER   ROOM,  LROOM, SHUTTR,   ROPE,
     2         HUNCH,  MATCH, MASTER, NOTVAL, LOKVAL, BUTVAL,
     3        ROPVAL,   FIRE,  WIND1,  WIND2,  SCORE,
     4        NUMOVE,    SUN,   NUMB,   NOTE,  IPASS,   LAMP,
     5         MMOVE,  LMOVE,    BUT
C
      LOGICAL BOTTLE, BLOOD, WATER, DEBUG, WOLF, GNOME
      LOGICAL GUN, BAT, PREC, HOLE, MELT, TORCH
C
      COMMON  /BLOCK1/ FORM, FORM2, INST, LONG, HINT
      COMMON /BLOCK2/ SAVAR
C
      EQUIVALENCE (SAVAR(  1),ITEMS(1)),(SAVAR( 31), VALUE(1)),
     2            (SAVAR( 61), ROOM    ),(SAVAR( 62), LROOM  ),
     3            (SAVAR( 63), GUN     ),(SAVAR( 64), BAT    ),
     4            (SAVAR( 65), BUT    ),(SAVAR( 66), SHUTTR  ),
     5            (SAVAR( 67), ROPE   ),(SAVAR( 68), HUNCH   ),
     6            (SAVAR( 69), MATCH  ),(SAVAR( 70), MASTER  ),
     7            (SAVAR( 71), WHER(1)),(SAVAR( 81), NOTVAL  ),
     8            (SAVAR( 82), LOKVAL ),(SAVAR( 83), BUTVAL  ),
     9            (SAVAR( 84), ROPVAL ),(SAVAR( 85), FIRE    ),
     X            (SAVAR( 86), PREC   ),(SAVAR( 87), WIND1   ),
     1            (SAVAR( 88), WIND2  ),(SAVAR( 89), SCORE   ),
     2            (SAVAR( 90), NUMOVE ),(SAVAR( 91), BLOOD   ),
     3            (SAVAR( 92), BOTTLE ),(SAVAR( 93), WATER   ),
     4            (SAVAR( 94), SUN    ),(SAVAR( 95), NUMB    ),
     5            (SAVAR( 96), NOTE   ),(SAVAR( 97), IPASS   ),
     6            (SAVAR( 98), LAMP   ),(SAVAR( 99), MMOVE   ),
     7            (SAVAR(100), LMOVE  ),(SAVAR(101),DOOR(1)  ),
     8            (SAVAR(201), NDEATH ),(SAVAR(202),    MAX  ) ,
     9            (SAVAR(203), MAXSCR ),(SAVAR(204),IBRIEF),
     X            (SAVAR(205), GNOME  ),(SAVAR(206), WOLF )
C
      EQUIVALENCE (SAVAR(207),    HOLE),(SAVAR(208),    MELT),
     2            (SAVAR(209),   TORCH)
C
      DATA INDEX  / 29, 1, 0, 0, 0, 2, 0, 3, 4, 5,
     2               5, 0,25, 0, 0,26, 0, 6, 7, 0,
     3               0, 0, 0, 0, 8, 9,10, 0, 0, 0,
     4               0, 0, 0, 0, 0, 0,12, 0,13, 0,
     5               0, 0, 0, 0, 0,14, 0,15,16,17,
     6               0, 0, 0,18,19,20, 0, 0, 0, 0,
     7               10 * 0 ,
     8               0, 0, 0, 0, 0,11,24,23,27,28,
     9               20 * 0 /
C
C
C
      IF (II .GT. 0) GOTO 100
      DO 10 I=1,13
           DO 5 J=1,20
                FMT(J) = INST(I,J)
    5      CONTINUE
           WRITE(6,FMT)
   10 CONTINUE
      WRITE(6,1001)
      CALL YORN(JJ)
      IF (JJ .LT. 1) GOTO 900
C
   20 CONTINUE
      DO 27 I=14,19
           DO 25 J=1,20
                FMT(J) = INST(I,J)
   25      CONTINUE
                WRITE(6,FMT)
   27 CONTINUE
      WRITE(6,1001)
      CALL YORN(JJ)
      IF (JJ .EQ. 0) GOTO 900
C     INSERT THIRD LEVEL INSTRUCTIONS.
C
   28 DO 32 I=20,23
           DO 30 J=1,20
                FMT(J) = INST(I,J)
   30      CONTINUE
           WRITE(6,FMT)
   32 CONTINUE
      GOTO 900
C
  100 CONTINUE
      IF (OBJECT .EQ. 0) GOTO 28
      JJ = INDEX(OBJECT)
      IF (OBJECT .NE. 45) GOTO 108
           JJ = 0
           IF (ROOM .EQ. 20 .AND. BAT) JJ=19
           IF (ROOM .EQ.  1 .AND. WIND1 .EQ. 2) JJ=8
           IF (ROOM .GE. 57 .AND. ROOM .LE. 64) JJ=21
           IF (ROOM .EQ. 65 .AND. .NOT. MELT) JJ=22
           IF (ROOM .EQ. 86 .AND. .NOT. HOLE) JJ=23
  108 CONTINUE
      IF (JJ .EQ. 0) GOTO 115
      WRITE(6,1003)
      CALL YORN(II)
      IF (II .EQ. 0) GOTO 900
           DO 110 J=1,20
                FMT(J) = HINT(JJ,J)
  110      CONTINUE
           WRITE(6,FMT)
           SCORE = SCORE - 5
           GOTO 900
C
  115 CONTINUE
C     NO HINT AVAILABLE.
      WRITE(6,1002)
      II = 0
      GOTO 900
  900 CONTINUE
 1001 FORMAT('0  Would you like more detailed instructions?')
 1002 FORMAT('0  Sorry, not available.')
 1003 FORMAT('0  It will cost you five points.',/,
     2       '   Do you still want the hint?.')
      RETURN
      END
