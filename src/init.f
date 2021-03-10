C---------------------------------------------------------------
      SUBROUTINE INIT
C
      INTEGER FORM(100,20),FORM2(60,20),INST(50,20),LONG(400,20)
      INTEGER HINT(50,20), V(30), T(30), D(100), SAVAR(400), W(10)
      INTEGER SAT /'SAT '/, SUND /'SUN '/, OPN(2) /'09:0','0:00'/
      INTEGER TME(6),OPEN(2)/'21:0','0:00'/
      LOGICAL*1 WORD(5)/'M','U','T','Z',' '/,INPUT(5)
C
C
      INTEGER ITEMS(30), VALUE(30), WHER(10), DOOR(100)
C
      INTEGER   ROOM,  LROOM, SHUTTR,   ROPE,
     2         HUNCH,  MATCH, MASTER, NOTVAL, LOKVAL, BUTVAL,
     3        ROPVAL,  WIND1,  WIND2,  SCORE,
     4        NUMOVE,    SUN,   NUMB,   NOTE,  IPASS,   LAMP,
     5         MMOVE,  LMOVE,    BUT
C
      LOGICAL BOTTLE, BLOOD, WATER, DEBUG, WOLF, GNOME,
     2        GUN, BAT, PREC, HOLE, MELT, TORCH, WIZ,
     3        MASECT, FIRE, MAS1, MAS2, MAS3, MAS4
C
      DATA V /  0, 0, 0, 0, 0, 0,10, 0, 0, 0,
     2          0,10, 0, 0, 0, 0,20, 0,25, 0,
     3          0, 0,10,10, 0, 0, 0,10,10,10 /
C
      DATA T / 25, 1,33,-3,18,12,55,35,19, 3,
     2         15,88,36,32,13,11,29,31, 0, 3,
     3         22, 6,81,52,14, 0,10,77,94,89 /
C
      DATA D / -2, 0,-2, 2,-2, 0, 2, 5*-2, 2, 7*-2, 0,
     2         -2,-2, 56*-1,0,20*-1 /
C
      DATA W / 28,15,28,9,28,15,15,9,28,15 /
C
C
      EQUIVALENCE (SAVAR(  1),ITEMS(1)),(SAVAR( 31), VALUE(1)),
     2            (SAVAR( 61), ROOM    ),(SAVAR( 62), LROOM  ),
     3            (SAVAR( 63), GUN     ),(SAVAR( 64), BAT    ),
     4            (SAVAR( 65), BUT     ),(SAVAR( 66), SHUTTR ),
     5            (SAVAR( 67), ROPE   ),(SAVAR( 68), HUNCH   ),
     6            (SAVAR( 69), MATCH  ),(SAVAR( 70), MASTER  ),
     7            (SAVAR( 71), WHER(1)),(SAVAR( 81), NOTVAL  ),
     8            (SAVAR( 82), LOKVAL ),(SAVAR( 83), BUTVAL  ),
     9            (SAVAR( 84), ROPVAL ),(SAVAR( 85), FIRE    ),
     X            (SAVAR( 86),   PREC ),(SAVAR( 87), WIND1   ),
     1            (SAVAR( 88), WIND2  ),(SAVAR( 89), SCORE   ),
     2            (SAVAR( 90), NUMOVE ),(SAVAR( 91), BLOOD   ),
     3            (SAVAR( 92), BOTTLE ),(SAVAR( 93), WATER   ),
     4            (SAVAR( 94), SUN    ),(SAVAR( 95), NUMB    ),
     5            (SAVAR( 96), NOTE   ),(SAVAR( 97), IPASS   ),
     6            (SAVAR( 98), LAMP   ),(SAVAR( 99), MMOVE   ),
     7            (SAVAR(100), LMOVE  ),(SAVAR(101),DOOR(1)  ),
     8            (SAVAR(201), NDEATH ),(SAVAR(202), MAX     ),
     9            (SAVAR(203), MAXSCR ),(SAVAR(204),IBRIEF),
     X            (SAVAR(205), GNOME  ),(SAVAR(206), WOLF  )
C
      EQUIVALENCE (SAVAR(207),    HOLE),(SAVAR(208),    MELT),
     2            (SAVAR(209),   TORCH),(SAVAR(210),     WIZ),
     3            (SAVAR(211),  MASECT),(SAVAR(212),    MAS1),
     4            (SAVAR(213),    MAS2),(SAVAR(214),    MAS3),
     5            (SAVAR(215),    MAS4)
      COMMON /BLOCK1/ FORM, FORM2, INST, LONG, HINT
      COMMON /BLOCK2/ SAVAR
C
      COMMON DEBUG, ISEED
C     CALL CMDNOE('$CONTROL *MSOURCE* K=UC',23)
C     CALL CMDNOE('$C AWCC:CASTMES',15)
C     CALL CMS('TYPE    ','CASTMES ','MEMO    ')
C     CALL TIME(22,0,TME)
C     IF (TME(1) .EQ. SAT .OR. TME(1) .EQ. SUND) GOTO 15
C     IF (LCOMC(8,TME(5),OPEN) .EQ.  1) GOTO 15
C     IF (LCOMC(8,TME(5),OPN ) .EQ. -1) GOTO 15
C     WRITE(6,1002)
C     CALL YORN(M)
C     IF(M.EQ.0) STOP
C     WRITE(6,1003)
C     CALL CMDNOE('$CONTROL *MSINK* BLANK=6', 24)
C     READ(5,1004)(INPUT(L),L=1,5)
C     IF(LCOMC(3,TME(1),INPUT) .EQ. 0) GOTO 15
C     WRITE(6,1005)
C     STOP
  15  CONTINUE
C
      DO 18 I=1,400
           SAVAR(I) = 0
   18 CONTINUE
C     CALL GUINFO(44,ISEED)
C
      CALL RSTART(ISEED)
      ISEED = ISEED*2 + 1
      DO 16 I=1,30
           VALUE(I) = V(I)
           ITEMS(I) = T(I)
           DOOR(I)  = D(I)
           IF (I .LE. 10) WHER(I)=W(I)
   16 CONTINUE
      DO 17 I=31,100
           DOOR(I)  = D(I)
   17 CONTINUE
C
      DEBUG  = .FALSE.
      BOTTLE = .FALSE.
      BLOOD  = .FALSE.
      WATER  = .FALSE.
      GNOME  = .FALSE.
      WOLF   = .FALSE.
      HOLE   = .FALSE.
      MELT   = .FALSE.
      WIZ    = .TRUE.
      TORCH  = .FALSE.
      MASECT = .FALSE.
      ROPE   = 0
      ROOM   = 1
      LROOM  = 1
      IBRIEF = 0
      MATCH  = 0
      ROPVAL = 10
      BUTVAL = 5
      NOTVAL = 15
      LOKVAL = 5
      SUN    = 0
      MASTER = 0
      NUMB   = 0
      GUN    = .FALSE.
      PREC   = .FALSE.
      NOTE   = 0
      WIND1  = 0
      WIND2  = 0
      BUT    = 0
      SHUTTR = 0
      IPASS  = 0
      BAT    = .TRUE.
      LAMP   = 0
      NUMOVE = 0
      HUNCH  = 0
      MMOVE  = 0
      LMOVE  = 0
      SCORE  = 0
      FIRE   = .TRUE.
      MAS1   = .FALSE.
      MAS2   = .FALSE.
      MAS3   = .FALSE.
      MAS4   = .FALSE.
      NDEATH = 0
      MAX    = 99
      MAXSCR = 300
C
C     CALL FTNCMD('ASSIGN 5=*SOURCE*;')
C     CALL FTNCMD('ASSIGN 6=*SINK*;')
C     CALL FTNCMD('ASSIGN 8=AWCC:HINT;')
C     CALL FTNCMD('ASSIGN 9=AWCC:SHORT;')
C     CALL FTNCMD('ASSIGN 10=AWCC:OBJECT;')
C     CALL FTNCMD('ASSIGN 11=AWCC:INST;')
C     CALL FTNCMD('ASSIGN 12=AWCC:LONG;')
C***
      CALL CMS('FI      ','8       ','DISK    ','HINT    ','CQDATA  ',
     *'(       ','LRECL  ','80      ','RECFM   ','F       ')
      CALL CMS('FI      ','9       ','DISK    ','SHORT   ','CQDATA  ',
     *'(       ','LRECL  ','80      ','RECFM   ','F       ')
      CALL CMS('FI      ','10      ','DISK    ','OBJECT  ','CQDATA  ',
     *'(       ','LRECL  ','80      ','RECFM   ','F       ')
      CALL CMS('FI      ','11      ','DISK    ','INST    ','CQDATA  ',
     *'(       ','LRECL  ','80      ','RECFM   ','F       ')
      CALL CMS('FI      ','12      ','DISK    ','LONG    ','CQDATA  ',
     *'(       ','LRECL  ','80      ','RECFM   ','F       ')
C
      DO 20 I=1,100
           READ(9,1001,END=30,ERR=30)(FORM(I,J),J=1,20)
   20 CONTINUE
   30 DO 40 I=1,60
           READ(10,1001,END=50,ERR=50)(FORM2(I,J),J=1,20)
   40 CONTINUE
   50 CONTINUE
      DO 60 I=1,50
           READ(11,1001,END=80,ERR=80)(INST(I,J),J=1,20)
   60 CONTINUE
  80  CONTINUE
      DO 90 I=1,400
           READ(12,1001,END=100,ERR=100)(LONG(I,J),J=1,20)
   90 CONTINUE
  100 CONTINUE
      DO 110 I=1,50
           READ(8,1001,END=120,ERR=120)(HINT(I,J),J=1,20)
  110 CONTINUE
  120 CONTINUE
C     CALL FTNCMD('RELEASE 9;')
C     CALL FTNCMD('RELEASE 10;')
C     CALL FTNCMD('RELEASE 11;')
C     CALL FTNCMD('RELEASE 12;')
C     CALL FTNCMD('RELEASE 8;')
 1001 FORMAT(20A4)
 1002 FORMAT('0 I''M SORRY, BUT THE CASTLE IS CLOSED RIGHT NOW.'/
     1'       CASTLE HOURS ARE :'//'   MONDAY THRU FRIDAY-----'
     2,'21:00 TO 09:00'/'   SAT. AND SUN.     -----',
     3 'ALL DAY'/'0ONLY BARONS MAY PLAY AT THIS TIME.'/
     4'ARE YOU A BARON???')
 1003 FORMAT('  PROVE IT BY ENTERING THE SECRET WORD.')
 1004 FORMAT(5A1)
 1005 FORMAT('  THAT IS NOT THE SECRET WORD. BYE BYE.')
      RETURN
      END
