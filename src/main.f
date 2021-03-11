C
C     CASTLEQUEST BY MICHAEL S. HOLTZMAN/MARK A. KERSHENBLATT 2/80
C           Modified to run under IBM's CP/CMS, 9/81, by M. Holtzman
C
C--------------------------------------------------------------
C     ITEMS  = LOCATIONS OF THE ITEMS
C     VERBS  = LIST OF ALLOWABLE VERBS.
C     NOUNS  = LIST OF ALLOWABLE NOUNS.
C     ROOM   = CURRENT ROOM
C     LROOM  = LAST ROOM
C     NUMB   = NUMBER OF ITEMS BEING CARRIED.
C     GUN    = STAUS OF GUN (0=NOTHING,1=LOADED)
C     LOCK   = 0=LOCKED,1=1ST #,2=2ND #,3=OPEN.
C     WIND1  = WINDOW IN ROOM 1 (0=NAILED,1=BROKEN,2=BARRED,3=OPEN)
C     DOOR(I)= DOOR IN ROOM I (0=LOCKED,1=CLOSED,2=OPEN)
C     SHUTTR = STATUS OF SHUTTER (0=CLOSED, 1=OPEN)
C     BUT    = BUTLER(0=SLEEPING,1=AWAKE,2=HOLDING NOTE,3=GONE FOR GOOD,
C                     4=DEAD AS A DOORNAIL.
C     BAT    = STATUS OF BAT (0=GONE,1=BLOCKING WAY,HUNGRY)
C     NOTE   = NUMBER OF NOTE BUTLER IS HOLDING.
C     NDEATH = NUMBER OF TIMES PLAYER HAS DIED.
C     LAMP   =STATUS OF LAMP (0=OFF,1=ON,2=DIM,3=EMPTY)
C     ROPE   = (0=LOOSE,1=TIED TO BED,2=HANGING,-2=GONE,3=TIED TO HOOK)
C     VALUE  = POINT VALUE OF FINDING EACH TREASURE.
C     HUNCH  = HUNCHBACK(0=HUNGRY,1=FOLLOWING,2=GONE)
C     BOTTLE = BOTTLE FILLED?
C     BLOOD  = BLOOD IN BOTTLE?
C     WATER  = WATER IN BOTTLE?
C     MELT   = HOLE MELTED IN ICE?
C     HOLE   = CYCLOPS SHAPED HOLE IN DOOR?
C     WIZ    = WIZARD STILL AROUND?
C     TORCH  = IS TORCH LIT?
C     FIRE   = IS FIRE BURNING?
C     SUN    = IS SUN UP(0=UP,1=SETTING)
C     MASTER = (0=IN COFFIN,1=ASLEEP,2=PINNED,3=UP,4=DEAD)
C     LMOVE  = # MOVES WITH  LAMP   LIT.
C     MMOVE  = # MOVES WITH MATCHES LIT.
C     MATCH  = MATCHES(0=UNLIT,1=LIT,2=GONE)
C     SCORE  = ACCUMULATED SCORE.
C     MAS1,2,3,4 = MASTER SECTION STATUS (LOGICAL)
C--------------------------------------------------------------
C
      INTEGER ACTION(2), OBJECT, PREV(100) /100*-1/
      INTEGER INST(50,20), LEAVE(100), ENTER(100)
C
      INTEGER FORM(100,20),FORM2(60,20),LONG(400,20),HINT(50,20)
C
      INTEGER PW /'..  '/
C     INTEGER PARLEN,PARMAX/25/,NUMCOM/0/
C     INTEGER SIGID,AWCC/'AWCC'/,A6L2/'A6L2'/,A3TB/'A3TB'/
C
C
      INTEGER ITEMS(30), VALUE(30), WHER(10), DOOR(100), SAVAR(400)
C
      INTEGER   ROOM,  LROOM, SHUTTR,   ROPE,
     2         HUNCH,  MATCH, MASTER, NOTVAL, LOKVAL, BUTVAL,
     3        ROPVAL,  WIND1,  WIND2,  SCORE,    SUN,
     4        NUMOVE,   NUMB,   NOTE,  IPASS,   LAMP,
     5         MMOVE,  LMOVE,    MAX, MAXSCR,    BUT
C
      LOGICAL BOTTLE, BLOOD, WATER, DEBUG, GNOME,SAVE, WIZ
      LOGICAL WOLF, GUN, BAT, PREC, EQUC, HOLE, MELT, TORCH
      LOGICAL MASECT, FIRE, MAS1, MAS2, MAS3, MAS4
      LOGICAL*1 PARSTG(25),PAR1(8),PAR2(8),PAR3(8),COMMA/','/
      LOGICAL*1 DEBU(4)/'D','E','B','U'/,REST(4)/'R','E','S','T'/,
     1FAST(4)/'F','A','S','T'/
C
C
      EXTERNAL RDM
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
     9            (SAVAR(203), MAXSCR ),(SAVAR(204), IBRIEF  ),
     X            (SAVAR(205), GNOME  ),(SAVAR(206), WOLF    )
C
      EQUIVALENCE (SAVAR(207),    HOLE),(SAVAR(208),     MELT),
     X            (SAVAR(209),   TORCH),(SAVAR(210),     WIZ),
     2            (SAVAR(211),  MASECT),(SAVAR(212),     MAS1),
     3            (SAVAR(213),    MAS2),(SAVAR(214),     MAS3),
     4            (SAVAR(215),    MAS4)
      DATA NITEMS /30/
C
      DATA LEAVE /7,0,0,3,0,0,7,0,0,1,1,0,7,0,3,0,5,0,3,10,
     2            0,7,0,0,3,0,0,0,0,0,5,5,5,0,2,0,0,10,0,0,
     3            0,9,3,9,1,0,0,0,0,0,6,3,3,0,6,0,0,0,0,0,
     4            1,9,9,1,0,0,10,7,1,7,7,3,0,0,1,10,3,0,0,0,
     5            0,3,3,0,7,3,7,1,7,0,3,0,0,3,0,0,0,0,7,0  /
C
      DATA ENTER /0,0,1,0,0,3,0,0,0,0,0,3,10,0,0,0,10,1,0,1,
     2            3,0,0,7,0,0,0,7,0,0,0,0,0,0,0,8,0,0,0,0,
     3            7,0,0,5,0,0,0,1,0,0,2,0,7,0,0,0,0,0,0,0,
     4            19,9,1,1,1,0,7,3,3,0,3,5,0,1,0,0,0,0,7,9,
     5            0,3,0,3,0,5,1,0,0,9,1,0,7,0,0,0,0,0,7,0 /
C
      COMMON DEBUG, ISEED
      COMMON /BLOCK1/ FORM, FORM2, INST, LONG, HINT
      COMMON /BLOCK2/ SAVAR
C
      EQUIVALENCE (ACTION(2), OBJECT)
C
      CALL INIT
      SEED = FLOAT(ISEED)
C
      SAVE=.FALSE.
      MAXABS = MAXSCR
C***
Comment out the parameter fetch and check code:
C     CALL PAR(PARSTG,PARLEN,PARMAX,1,1)
C     DO 2 JJ=1,PARLEN
C        IF(EQUC(PARSTG(JJ),COMMA)) NUMCOM=NUMCOM+1
C        IF(EQUC(PARSTG(JJ),COMMA)) GOTO 2
C        GOTO(7,9),NUMCOM
C        PAR1(JJ)=PARSTG(JJ)
C        GOTO 2
C 7      CONTINUE
C        PAR2(JJ=FLAG)=PARSTG(JJ)
C        GOTO 2
C 9   CONTINUE
C        PAR3(JJ=FLAG)=PARSTG(JJ)
C 2   CONTINUE
C     IF(LCOMC(4,PAR1(1),DEBU(1)) .EQ.0.OR.
C    1LCOMC(4,PAR2(1),DEBU(1)) .EQ. 0 .OR.
C    2LCOMC(4,PAR3(1),DEBU(1)) .EQ. 0) DEBUG=.TRUE.
C     IF(LCOMC(4,PAR1(1),REST(1)) .EQ. 0.OR.
C    1LCOMC(4,PAR2(1),REST(1)) .EQ.0 .OR.
C    2LCOMC(4,PAR3(1),REST(1)) .EQ.0) GOTO 158
C*****
C
  1   WRITE(6,1001)
      CALL YORN(II)
      IF (II .EQ. 0) GOTO 10
           CALL HELP(0,ROOM)
   10 CONTINUE
C
C
   20 CONTINUE
      IF (DEBUG) WRITE(6,6301)
 6301 FORMAT('0  TOP OF LOOP (STATEMENT 20)')
C
      IF (ROOM    .LE.    40           .OR.
     2    ROOM    .GE.    95           .OR.
     3    ((LAMP.EQ.1.OR.LAMP.EQ.2) .AND.
     4                 (ITEMS(21).EQ.-1.OR.ITEMS(21).EQ.ROOM)) .OR.
     5    (MATCH.EQ.1               .AND.
     6                 (ITEMS(15).EQ.-1.OR.ITEMS(15).EQ.ROOM))) GOTO 22
           WRITE(6,1064)
           GOTO 25
 22   CONTINUE
      NUMOVE = NUMOVE + 1
      IF (DEBUG) WRITE(6,6302) NUMOVE
 6302 FORMAT('0  NUMBER OF MOVES IS ',I3)
      IF (ITEMS( 8) .GE. 57 .AND. ITEMS( 8) .LE. 64)
     2               ITEMS(8)=IFIX(RDM(SEED)*7. + 57.)
      IF (NUMOVE.GT.100.AND.SUN.EQ.0.AND.MASTER.LT.4) WRITE(6,1089)
      IF (NUMOVE .GT. 100) SUN = 1
      IF (MATCH .NE. 1) GOTO 404
           MMOVE = MMOVE + 1
           IF (MMOVE .LT. 10) GOTO 404
                MATCH = 2
                IF (ITEMS(15) .EQ. -1) NUMB = NUMB - 1
                ITEMS(15) = 0
                WRITE(6,1088)
Check to see if glacier is melted yet, and if cyclops has crashed
C through the door yet. (These both require the match). If not, decrease
C  MAX because match is needed to get them.  (MAX determines when
C  the player has found everything).
                IF (.NOT.  HOLE) MAX=MAX-10
                IF (.NOT.  MELT) MAX=MAX-10
                GOTO 20
  404 CONTINUE
      IF (LAMP .EQ. 0 .OR. LAMP .EQ. 3) GOTO 410
      LMOVE = LMOVE + 1
      IF (LMOVE .LT. 75) GOTO 410
           IF (LMOVE .NE. 100)GOTO 405
                LAMP = 3
                WRITE(6,1009)
                WRITE(6,1064)
                GOTO 25
  405 CONTINUE
      IF (LMOVE .NE. 75) GOTO 410
      LAMP = 2
      WRITE(6,1083)
      IF (ITEMS(1) .NE. -1 .OR. ITEMS(21) .NE. -1) GOTO 410
           WRITE(6,1085)
           ITEMS(1) = 0
           NUMB = NUMB - 1
           LMOVE = -75
           LAMP = 1
  410 CONTINUE
      II = ROOM + 200*ISIG(PREV(ROOM))
      IF (DEBUG) WRITE(6,6303) II
 6303 FORMAT('0  ABOUT TO CALL "DES": II IS ', I3)
      CALL DES(II)
      IF (ITEMS(8) .EQ. -1) CALL DES(421)
      IF (ROOM .EQ. 100) GOTO 9100
   23 CONTINUE
C     ...SPECIAL ROOM CONDITIONS...
      II = 0
      IF (ROOM .NE. 8) GOTO 501
      II = 400 + BUT
      GOTO 550
  501 CONTINUE
      IF (ROOM  .NE. 1) GOTO 505
      IF (SHUTTR .EQ. 0) GOTO 502
           II = WIND1 + 405
           CALL DES(II)
           IF (ITEMS(17) .EQ. 29) CALL DES(428)
           II=0
           GOTO 504
  502 CONTINUE
      CALL DES(417)
  504 IF (ROPE .EQ. 2) II=413
      GOTO 550
  505 CONTINUE
      IF (ROOM .NE. 10) GOTO 507
           II = 405 + WIND2
           GOTO 550
  507 CONTINUE
      IF (ROOM .NE. 13 .AND. ROOM .NE. 17) GOTO 509
           IF (ROOM .EQ. 13) WRITE(6,1098)
           II = 412 * IPASS
           GOTO 550
  509 CONTINUE
      IF (ROOM .NE. 29) GOTO 511
      IF (ROPE .NE.  2) GOTO 511
           II = 413
  511 CONTINUE
      IF (ROOM .NE. 43) GOTO 513
           II = 423 + MASTER
  513 CONTINUE
      IF (ROOM .NE. 47) GOTO 515
            IF (FIRE) WRITE(6,1100)
            GOTO 550
  515 CONTINUE
      IF (ROOM .NE. 40) GOTO 517
           IF (DOOR(80) .EQ. 2) WRITE(6,1110)
           GOTO 551
  517 CONTINUE
      IF (ROOM .NE. 83 .AND. ROOM .NE. 84) GOTO 519
           IF (PREC) WRITE(6,1011)
           GOTO 550
  519 CONTINUE
      IF (ROOM .NE. 65) GOTO 521
           IF (MELT) WRITE(6,1116)
           GOTO 550
  521 CONTINUE
C      ...CYCLOPS IN ROOM...
      IF (ROOM .NE. 86) GOTO 523
           IF (HOLE) WRITE(6,1117)
           IF (.NOT. HOLE .AND. ITEMS(27) .NE. -3) WRITE(6,1118)
           IF (.NOT. HOLE .AND. ITEMS(27) .EQ. -3) WRITE(6,1119)
           GOTO 550
  523 CONTINUE
      IF (ROOM .NE. 93) GOTO 525
           IF (WIZ) WRITE(6,1128)
           GOTO 550
  525 CONTINUE
      IF (ROOM .NE. 99) GOTO 527
           IF (MASECT ) WRITE(6,1136)
           GOTO 550
  527 CONTINUE
      IF (ITEMS(4) .GT. -2) NOTE = 1
  549 CONTINUE
      IF (ROOM .LE. 90 .OR. ROOM .GE. 94) GOTO 550
      IF (ITEMS(12).NE.-1 .OR. .NOT.WIZ)  GOTO 550
           CALL DES(318+ROOM)
  550 CONTINUE
      IF (II .EQ. 0) GOTO 551
      CALL DES(II)
  551 CONTINUE
      IF (ROOM .LE. 99 .AND. ROOM .GE. 95 .AND.
     2    ITEMS(3) .EQ. -1) WRITE(6,1141)
      II = 0
      CALL OBJ(ITEMS,ROOM,SCORE,VALUE)
      IF ((NUMOVE.GT.15 .AND. (ROOM.LE.25.AND.ROOM.GE.4))
     2           .OR. WOLF)      CALL WWOLF(II,WOLF)
      IF ((ROOM.GE.45.AND.ROOM.LE.92) .OR. GNOME) CALL GGNOME(II,GNOME)
      IF (II .GT. 0) GOTO 902
      IF (.NOT. WOLF .OR. ITEMS( 8) .NE. -1) GOTO 24
           CALL DES(429)
           HUNCH = 2
           ITEMS( 8) = 0
           WOLF = .FALSE.
           NUMB = NUMB - 1
   24 CONTINUE
      IF (RDM(SEED) .LT. 0.008) WRITE(6,1145)
   25 CALL INPUT(ACTION)
C
C     BRANCH DEPENDING ON VERB READ.
C
      J = ACTION(1)
   26 GOTO (101,101,101,101,101,101,101,101,101,101,
     2      111,112,113,114,115,116,117,118,119,120,
     3      121,122,123,124,125,126,127,128,129,130,
     4      131,132,133,134,135,136,137,138,139,140,
     5      141,142,143,144,145,146,147,148,149,150,
     6      151,152,153,154,155,156,157,158,159,160,
     7      161), J
C
  101 CONTINUE
      IF (ROOM .LE. 40 .OR. ROOM .GE. 95  .OR.
     2 ((LAMP.EQ. 1 .OR. LAMP.EQ. 2)
     3         .AND. (ITEMS(21).EQ. ROOM .OR. ITEMS(21).EQ.-1)))
     4      GOTO 420
      IF ((ITEMS(15) .EQ. -1 .OR. ITEMS(15) .EQ. ROOM) .AND.
     2     MATCH     .EQ.  1) GOTO 420
           WRITE(6,1077)
           GOTO 902
  420 II    = LROOM
      LROOM = ROOM
      CALL MOVE(ROOM, J)
  428 IF (DEBUG) WRITE(6,9501) ROOM,LROOM
 9501 FORMAT('0  RETURN FROM MOVE: ROOM=',I8,' LROOM =',I8)
      IF (ROOM.GE.57 .AND. ROOM.LE.64) LROOM=IFIX(RDM(SEED)*6.+58.)
      IF (ROOM .GE. 0) GOTO 102
           ROOM = (-ROOM)
      IF (ROOM .LE. 0) ROOM=0
      IF (ROOM .EQ. 0) GOTO 102
           IF ((LROOM .NE. 2 .OR. ROOM .NE. 4) .AND.
     2         (LROOM .NE. 4 .OR. ROOM .NE. 2)) GOTO 601
           IF (DOOR(2) .GE. 2) GOTO 103
           IF (DOOR(2) .EQ. 0) WRITE(6,1019)
           IF (DOOR(2) .EQ. 1) WRITE(6,1042)
           GOTO 106
  601 CONTINUE
C     ...KITCHEN...
      IF (ROOM .NE. 7) GOTO 605
           IF (DOOR(6) .EQ. 2) GOTO 103
           IF (DOOR(6) .EQ. 0) WRITE(6,1021)
           IF (DOOR(6) .EQ. 1) WRITE(6,1042)
           GOTO 106
  605 CONTINUE
C     ...SECRET PASSAGE...
      IF ((ROOM .NE. 13 .OR. LROOM .NE. 17) .AND.
     2    (ROOM .NE. 17 .OR. LROOM .NE. 13)) GOTO 609
           IF (IPASS .EQ. 0) ROOM=0
           GOTO 102
  609 CONTINUE
C     ...TRAP DOOR...
      IF (ROOM .NE. 2 .OR. LROOM .NE. 3) GOTO 613
      IF (ITEMS(4) .NE. -1) GOTO 103
           ROOM = 16
           LROOM= 0
           WRITE(6,1043)
           GOTO 103
  613 CONTINUE
C     ...ATTIC...
      IF ((ROOM .NE. 21 .OR. LROOM .NE. 20) .AND.
     2    (ROOM .NE. 20 .OR. LROOM .NE. 21)) GOTO 615
      IF (.NOT. BAT) GOTO 103
           WRITE(6,1049)
           GOTO 106
  615 CONTINUE
C     ...COMBINATION LOCK...
      IF (ROOM .NE. 23 .OR. LROOM .NE. 21) GOTO 617
            IF (LOCK .EQ. 3) GOTO 103
            WRITE(6,1066)
            GOTO 106
  617 CONTINUE
C     ...MIRROR ROOM...
      IF (LROOM .NE. 27) GOTO 619
           IF (RDM(SEED) .LT. 0.20) GOTO 618
           ROOM = 27
           LROOM= 27
           PREV(ROOM) = 1
           GOTO 20
  618 ROOM = WHER(IFIX(RDM(SEED)*9+1))
      GOTO 103
  619 CONTINUE
C     ...ROPE OUT WINDOW...
      IF ((ROOM .NE. 29 .OR. LROOM .NE.  1) .AND.
     2    (ROOM .NE.  1 .OR. LROOM .NE. 29)) GOTO 621
           IF (ROPE .EQ. 2) GOTO 103
           WRITE(6,1006)
           GOTO 106
  621 CONTINUE
C     ...JUMP FROM SMOKING ROOM...
      IF (ROOM .NE. 39 .OR. LROOM .NE. 10) GOTO 623
  622      IF (WIND2 .EQ. 3) LROOM = 0
           IF (WIND2 .EQ. 3) GOTO 103
                WRITE(6,1006)
                GOTO 106
  623 CONTINUE
C     ...FIRE...
      IF (ROOM .NE. 48 .OR. LROOM .NE. 47) GOTO 625
           IF (.NOT. FIRE) GOTO 103
                WRITE(6,1101)
                GOTO 106
  625 CONTINUE
C      ...END OF GAME...
      IF (ROOM .NE. 70 .OR. LROOM .NE. 71) GOTO 626
           DO 628 K=1,NITEMS
                 IF (ITEMS(K) .EQ. 71) ITEMS(K)=70
  628      CONTINUE
           GOTO 630
  626 IF (ROOM .NE. 71 .OR. LROOM .NE. 70) GOTO 627
           DO 632 K=1,NITEMS
                IF (ITEMS(K) .EQ. 70) ITEMS(K)=71
  632      CONTINUE
  630 CONTINUE
           CALL ADSCOR(II)
           IF (II .LT. MAX) GOTO 103
           IF (ITEMS( 3) .NE. -1) GOTO 103
                ROOM  = 99
                LROOM = 0
                GOTO 103
  627 CONTINUE
C      ...HATCH (GOING DOWN)...
       IF (ROOM .NE. 80 .OR. LROOM .NE. 40) GOTO 629
            IF (DOOR(80) .EQ. 2) GOTO 103
                ROOM = 0
               GOTO 102
  629 CONTINUE
C     ...HATCH (GOING UP)...
      IF (ROOM .NE. 40 .OR. LROOM .NE. 80) GOTO 631
           IF (DOOR(80) .EQ. 2) GOTO 103
                ROOM = 0
                WRITE(6,1042)
                GOTO 106
  631 CONTINUE
C     ...PRECIPICE...
      IF ((ROOM .NE. 83 .OR. LROOM .NE. 84) .AND.
     2    (ROOM .NE. 84 .OR. LROOM .NE. 83)) GOTO 633
           IF (.NOT. PREC) ROOM=0
           GOTO 102
  633 CONTINUE
C     ...MELT ICE ...
      IF (ROOM .NE. 90 .OR. LROOM .NE. 65) GOTO 635
           IF (.NOT. MELT) ROOM = 0
           GOTO 102
  635 CONTINUE
C     ...CYCLOPS SHAPED HOLE...
      IF (ROOM .NE. 88 .OR. LROOM .NE. 86) GOTO 637
           IF (.NOT. HOLE) ROOM = 0
           GOTO 102
  637 CONTINUE
      IF (ROOM .NE. 93 .OR. LROOM .NE. 92) GOTO 639
           IF (ITEMS(12) .EQ. -1) GOTO 103
                 ROOM = 0
                 WRITE(6,1127)
                 GOTO 106
  639 CONTINUE
C     ...WIZARD. . .
      IF (ROOM .NE. 94 .OR. LROOM .NE. 93) GOTO 641
           IF (.NOT. WIZ) GOTO 102
               WRITE(6,1128)
               ROOM = 0
               GOTO 106
  641 CONTINUE
      IF (ROOM .NE. 70 .OR. LROOM .NE. 71) GOTO 643
           IF (ITEMS(3) .NE. -1) GOTO 102
           CALL ADSCOR(II)
           IF (II .LT. MAX) GOTO 102
                ROOM = 99
                LROOM = 0
                GOTO 102
  643 CONTINUE
      IF (ROOM .NE. 95 .OR. LROOM .NE. 99) GOTO 645
            IF (MASECT) GOTO 102
                  ROOM = 0
                  GOTO 102
  645 CONTINUE
      WRITE(6,8005) LROOM, ROOM
 8005 FORMAT('0  SPECIAL MOVE FROM ',I3,' TO ',I3)
      GOTO 20
  102 IF (ROOM .NE. 0) GOTO 103
           WRITE(6,1006)
  106      ROOM = LROOM
           LROOM= II
           GOTO 25
  103 CONTINUE
C     ...BOAT IN CASTLE...
      IF((ROOM .NE. 8 .OR. LROOM .NE. 12) .AND.
     1   (ROOM .NE. 1 .OR. LROOM .NE. 29)) GOTO 28
      IF(ITEMS(14) .NE. -1) GOTO 29
      WRITE(6,1081)
      GOTO 106
   28 CONTINUE
C      ...MASTER AWAKE...
      IF (ROOM .NE. 41 .OR. LROOM .NE. 43) GOTO 29
           IF (MASTER .NE. 3) GOTO 29
                WRITE(6,1094)
                GOTO 902
   29 CONTINUE
      IF (MASTER .EQ. 1 .OR. MASTER .EQ. 2) MASTER = 3
      PREV(ROOM) = PREV(ROOM) + 1 + IBRIEF
      IF (PREV(ROOM) .GT. 5) PREV(ROOM)=0 + IBRIEF
      GOTO 20
C
  111 CONTINUE
C     ---TAKE---
           IF (NUMB.LT.10.OR.OBJECT.EQ.26.OR.OBJECT.EQ.5) GOTO 206
                WRITE(6,1012)
                GOTO 25
  206 IF (OBJECT .EQ. 31) GOTO 207
      IF (OBJECT .EQ. 8 .AND. HUNCH .EQ. 1) GOTO 204
      IF (OBJECT.EQ.46 .AND. (ROOM.NE.13.AND.ROOM.NE.10)) GOTO 201
      IF (OBJECT .EQ. 9 .AND. ROPE  .EQ. 2) GOTO 454
      IF (OBJECT .EQ. 9 .AND. ROPE .EQ. 3) GOTO 452
      IF (OBJECT .EQ.16 .AND. ROPE .EQ. 3) GOTO 452
      IF (OBJECT.EQ.26.AND.(ROOM.EQ.32.OR.ROOM.EQ.40.OR.ROOM.EQ.68
     2     .OR.ROOM.EQ.81)) GOTO 215
  203 IF (OBJECT .NE. 49) GOTO 766
           IF (BUT .NE. 4) WRITE(6,1034)
           IF (BUT .EQ. 4) WRITE(6,1146)
  766 IF (OBJECT .EQ. 46) WRITE(6,1033)
      IF (OBJECT .EQ. 52) WRITE(6,1045)
      IF (OBJECT .EQ.  8) WRITE(6,1093)
      IF (OBJECT .EQ. 49 .OR. OBJECT .EQ. 46 .OR.
     2    OBJECT .EQ. 52 .OR. OBJECT .EQ.  8) GOTO 25
      IF (OBJECT .GT. NITEMS) GOTO 730
      IF (ITEMS(OBJECT) .EQ. -1) GOTO 750
      IF (OBJECT .EQ. 20 .AND. ROOM .EQ. 3) GOTO 204
      IF (OBJECT .EQ. 6  .AND. ROOM .EQ. 12) GOTO 204
      IF (ITEMS(OBJECT).NE.ROOM .AND.ITEMS(OBJECT).NE.-2) GOTO 201
      IF (OBJECT .EQ. 26 .OR. OBJECT .EQ. 5) GOTO 215
  204      ITEMS(OBJECT) = -1
           NUMB = NUMB + 1
           GOTO 800
  201 CONTINUE
      WRITE(6,1003)
  205 GOTO 25
  454 ROPE = 0
      ITEMS(9) = -1
      NUMB = NUMB + 1
      GOTO 800
  452 CONTINUE
      IF (PREC .AND. ROOM .EQ. 83) GOTO 303
      IF (ITEMS(9).NE.ROOM .OR. ITEMS(16).NE.ROOM) GOTO 201
      ITEMS( 9) = -1
      ITEMS(16) = -1
      NUMB = NUMB + 2
      IF (PREC) PREC=.FALSE.
      GOTO 800
  207 CONTINUE
C TAKE ALL -- pick up everything in the room, except blood or water.
      DO 208 II=1,NITEMS
           IF (II .EQ. 5 .OR. II .EQ. 26) GOTO 208
           IF (ITEMS(II) .NE. ROOM) GOTO 208
                ITEMS(II) = -1
                NUMB = NUMB + 1
                IF (NUMB .LT. 10) GOTO 208
                     WRITE(6,1012)
                     GOTO 25
  208 CONTINUE
      GOTO 800
  215 CONTINUE
      IF (ITEMS(18) .NE. -1) GOTO 216
           IF (BOTTLE) GOTO 218
           ITEMS(OBJECT) = -1
           BOTTLE = .TRUE.
           IF (OBJECT .EQ.  5) BLOOD = .TRUE.
           IF (OBJECT .EQ. 26) WATER = .TRUE.
           GOTO 800
  216 WRITE(6,1074)
      GOTO 25
  218 WRITE(6,1099)
      GOTO 25
C
  112 CONTINUE
C     ---DROP---
      IF (OBJECT .EQ. 31) GOTO 211
      IF (OBJECT .GT. NITEMS) GOTO 700
      IF ((OBJECT.EQ.9.OR.OBJECT.EQ.16).AND.ROPE.EQ.3) GOTO 453
      IF (ITEMS(OBJECT) .NE. -1) GOTO 210
  217 ITEMS(OBJECT) = ROOM
C If in MIRROR MAZE, put object in the upstairs hallway
      IF (ROOM .EQ. 27) ITEMS(OBJECT) = 9
      IF (OBJECT .EQ.  5 .OR. OBJECT .EQ. 26) GOTO 220
      IF ((OBJECT.EQ.2.OR.OBJECT.EQ.20).AND.GUN) GOTO 221
           NUMB = NUMB - 1
      IF (OBJECT .EQ. 18 .AND. BOTTLE) GOTO 222
           IF (ROOM.NE. 8) GOTO 800
           IF (BUT .EQ. 0 .OR. BUT .GE. 3) GOTO 800
           IF (ITEMS(11) .NE. 8 .OR.
     2         ITEMS(10) .NE. 8)  GOTO 800
                   ITEMS(10)=-3
                   ITEMS(11)=-3
                   BUT      = 2
                   CALL DES(402)
                   GOTO 25
  210 WRITE(6,1004)
      GOTO 25
  453 IF (ITEMS(9).NE.-1 .OR. ITEMS(16).NE.-1) GOTO 720
      ITEMS( 9) = ROOM
      ITEMS(16) = ROOM
      NUMB = NUMB - 2
      IF (ROOM .NE. 84) WRITE(6,1112)
      IF (ROOM .EQ. 84) PREC=.TRUE.
      IF (ROOM .EQ. 84) WRITE(6,1011)
      GOTO 25
  211 CONTINUE
      DO 212 II=1,30
           IF (ITEMS(II) .NE. -1) GOTO 212
                 ITEMS(II) = ROOM
  212 CONTINUE
      IF (BOTTLE) BOTTLE=.FALSE.
      IF (WATER ) WATER =.FALSE.
      IF (BLOOD ) BLOOD =.FALSE.
      IF (GUN   ) GUN   =.FALSE.
      NUMB = 0
      GOTO 800
  220 BOTTLE = .FALSE.
      IF (OBJECT .EQ.  5) BLOOD = .FALSE.
      IF (OBJECT .EQ. 26) WATER = .FALSE.
      GOTO 800
  221 CONTINUE
      GUN = .FALSE.
      IF (OBJECT .EQ. 20) NUMB=NUMB-1
      IF (OBJECT .EQ. 20) ITEMS( 2)=ROOM
      GOTO 800
  222 IF (.NOT. BLOOD) GOTO 223
           BLOOD = .FALSE.
           BOTTLE= .FALSE.
           ITEMS( 5) = ROOM
           GOTO 800
  223 WATER  = .FALSE.
      BOTTLE = .FALSE.
      ITEMS(26) = ROOM
      GOTO 800
C
  113 CONTINUE
C     ---ENTER---
      J = ENTER(ROOM)
      IF (J .EQ. 0) GOTO 314
           NUMOVE = NUMOVE + 1
      GOTO 26
  114 CONTINUE
C     ---LEAVE---
      IF (LEAVE(ROOM) .EQ. 0) GOTO 140
           J=LEAVE(ROOM)
           NUMOVE = NUMOVE + 1
           GOTO 26
  115 CONTINUE
C     ---ATTACK---
      IF (GNOME .AND. OBJECT .EQ. 77) GOTO 760
      IF (OBJECT .NE. 76 .AND. OBJECT .NE. 49 .AND.
     2    OBJECT .NE. 55 ) GOTO 229
      IF (OBJECT.EQ.55.AND.(ROOM.NE.20.OR.(.NOT.BAT))) GOTO 201
      IF (OBJECT.EQ.49.AND.(ROOM.NE.8.OR.BUT.EQ.4))  GOTO 201
      IF (OBJECT .EQ. 76 .AND. (.NOT. WOLF)) GOTO 201
      WRITE(6,1039)
      CALL YORN(II)
      IF (II .EQ. 0) GOTO 25
           IF (OBJECT .EQ. 76) CALL DES(414)
           IF (OBJECT .EQ. 49) CALL DES(415)
           IF (OBJECT .EQ. 55) CALL DES(416)
            GOTO 25
  229 CONTINUE
      IF (OBJECT .NE.  8) GOTO 230
      WRITE(6,1039)
      CALL YORN(II)
      IF (II .EQ. 0) GOTO 25
           IF (ITEMS(8).NE.ROOM.AND.ITEMS(8).NE.-1)GOTO 201
           IF (ITEMS(8) .EQ. -1) NUMB=NUMB-1
           ITEMS(8) = 0
           CALL DES(422)
           GOTO 25
  230 CONTINUE
      IF (OBJECT .NE. 78) GOTO 231
      IF (ROOM .NE.  86 .OR. HOLE) GOTO 201
           WRITE(6,1122)
           GOTO 25
  231 CONTINUE
      IF (OBJECT .NE. 39) GOTO 234
      IF (MASTER .EQ. 4) GOTO 234
           WRITE(6,1039)
           CALL YORN(II)
           IF (II .EQ. 0) GOTO 25
                WRITE(6,1086)
                GOTO 25
  234 CONTINUE
      IF (OBJECT .NE. 80) GOTO 236
           IF (ROOM .NE. 93 .OR. .NOT. WIZ) GOTO 201
  235            WRITE(6,1129)
                 GOTO 25
  236 CONTINUE
      CALL DES(418)
      GOTO 25
  116 CONTINUE
C     ---KILL---
      GOTO 115
  117 CONTINUE
C     ---THROW---
      IF (ROOM .EQ. 47 .AND. OBJECT .EQ. 26) GOTO 391
      IF (OBJECT .EQ. 16) GOTO 241
      IF (OBJECT .EQ. 9) GOTO 240
      IF (OBJECT .GT. NITEMS) GOTO 740
      IF (ITEMS(OBJECT).NE.-1 .AND. ITEMS(OBJECT).NE.ROOM) GOTO 720
      IF (OBJECT .EQ. 12) GOTO 380
      IF (OBJECT .EQ. 3) GOTO 380
      IF (OBJECT .EQ. 6 .AND. ROOM .EQ. 43) GOTO 247
      IF (OBJECT .NE. 25) GOTO 240
           IF (ITEMS(25) .NE. -1) GOTO 720
           NUMB = NUMB - 1
           IF (ROOM  .NE. 1) GOTO 232
           IF (SHUTTR .EQ. 0) GOTO 238
               WIND1 = 3
               WRITE(6,1061)
                ITEMS(25) = 0
               GOTO 25
  232      IF (.NOT. WOLF) GOTO 233
               WRITE(6,1062)
               WOLF      = .FALSE.
               ITEMS(25) = 0
C              Reduce MAX by ten, because the acid is needed to
C              get passed the bars and retrieve the statue.
               MAX = MAX - 10
               GOTO 25
  233      CONTINUE
      IF (ROOM .NE. 10) GOTO 238
      IF (WIND2 .LT. 1) GOTO 238
           WIND2 = 3
           WRITE(6,1061)
           GOTO 25
  238      CONTINUE
           WRITE(6,1063)
           ITEMS(25) = 0
           MAX = MAX - 10
           GOTO 25
  240 CONTINUE
      IF (OBJECT .NE. 9) GOTO 241
      IF (ROPE .EQ. 3) GOTO 472
      IF (ROOM .NE. 1) GOTO 112
      IF (WIND1.NE. 3) GOTO 217
           IF (ROPE .GT. 0) GOTO 242
                ROPE = -2
                NUMB = NUMB - 1
                ITEMS(9) = 0
                WRITE(6,1071)
                GOTO 25
  242      IF (ROPE .GT. 1) GOTO 243
                ROPE = 2
                WRITE(6,1070)
                SCORE = SCORE + ROPVAL
                ROPVAL = 0
                NUMB = NUMB - 1
                ITEMS(9) = ROOM
                GOTO 25
  243      WRITE(6,1072)
           GOTO 25
  241 CONTINUE
      IF (OBJECT .NE. 16) GOTO 474
  472 IF (ROOM   .NE. 83) GOTO 112
           PREC = .TRUE.
           ITEMS(16) = 84
           ITEMS( 9) = 84
           NUMB = NUMB - 2
           WRITE(6,1011)
           GOTO 25
  474 CONTINUE
      IF (OBJECT .NE. 27) GOTO 475
           IF (ROOM .NE. 86 .OR. HOLE) GOTO 112
                ITEMS(27) = -3
                NUMB      = NUMB - 1
                WRITE(6,1119)
                GOTO 25
  475 CONTINUE
      GOTO 112
  380 CONTINUE
      IF (OBJECT.EQ.12 .AND. ROOM.EQ.99 .AND. (.NOT.MASECT)) GOTO 767
      IF (OBJECT.EQ.12 .AND. ROOM.EQ.93 .AND. WIZ) GOTO 765
      IF (GNOME) GOTO 770
      IF (OBJECT .EQ. 12) ITEMS(12)=ROOM
      IF (OBJECT .EQ. 3 .AND. ROOM.LT.95) ITEMS( 3) = ROOM
      NUMB = NUMB -1
      IF (WOLF) OBJECT=76
      IF (ITEMS( 8) .EQ. ROOM .OR. ITEMS( 8) .EQ. -1) OBJECT=8
      IF (ROOM .EQ. 8) OBJECT = 49
      IF (ROOM .EQ. 20) OBJECT=55
      IF (ROOM .EQ. 86) OBJECT=78
      IF (ROOM .EQ. 93 .AND. WIZ) GOTO 235
      IF (ROOM .EQ. 6) OBJECT = 56
      IF (ROOM .EQ. 6) GOTO 137
      GOTO 379
  118 CONTINUE
C     ---LOAD---
      IF (OBJECT .EQ. 0) OBJECT=20
      IF (OBJECT .NE. 20) GOTO 700
      IF (ITEMS(20) .NE. -1) GOTO 720
      IF (ITEMS( 2) .EQ. -1) GOTO 274
           WRITE (6,1007)
           GOTO 25
  274 IF (.NOT. GUN) NUMB=NUMB-1
      GUN = .TRUE.
      GOTO 800
  119 CONTINUE
C     ---FUCK---
      IF (OBJECT .NE. 0) GOTO 261
      WRITE(6,1047)
      GOTO 25
  261 WRITE(6,1079)
      GOTO 25
 120  CONTINUE
C     ---WAVE---
      IF (ITEMS(OBJECT) .NE. -1) GOTO 720
      IF (ROOM .LT. 95 .OR. OBJECT .NE. 3) GOTO 264
      IF (ROOM .EQ. 99) GOTO 264
           IF (ROOM .NE. 95) GOTO 813
                IF (MAS1) GOTO 262
                     WRITE(6,1137)
                     MAS1 = .TRUE.
                     SCORE= SCORE + 3
                     GOTO 25
  813      IF (ROOM .NE. 96) GOTO 814
                IF (MAS2) GOTO 262
                     WRITE(6,1138)
                     MAS2 = .TRUE.
                     SCORE= SCORE + 2
                     GOTO 25
  814      IF (ROOM .NE. 97) GOTO 815
                IF (MAS3) GOTO 262
                     WRITE(6,1139)
                     MAS3 = .TRUE.
                     SCORE= SCORE + 3
                     GOTO 25
  815           IF (MAS4) GOTO 262
                     WRITE(6,1140)
                     MAS4 = .TRUE.
                     SCORE= SCORE + 2
                     GOTO 25
  264 CONTINUE
      IF (ROOM .NE. 93) GOTO 263
      IF (.NOT. WIZ) GOTO 262
           WRITE(6,1134)
           WRITE(6,1135)
           WIZ = .FALSE.
           GOTO 25
  263 IF (ROOM .NE. 43) GOTO 262
      IF (MASTER.EQ.0 .OR. MASTER.EQ.4) GOTO 262
      IF (OBJECT .NE. 19) GOTO 262
      IF (ITEMS(19) .NE. -1) GOTO 720
            MASTER = 2
            CALL DES(425)
            GOTO 25
  262 WRITE(6,1095)
      GOTO 25
  121 CONTINUE
C     ---STAB---
      IF (OBJECT .EQ. 39) GOTO 247
      IF (OBJECT .EQ. 80) GOTO 765
      IF (ITEMS(12) .NE. -1) GOTO 246
      IF (GNOME .AND. OBJECT .EQ. 77) GOTO 770
      ITEMS(12) = ROOM
      NUMB = NUMB - 1
  379 IF (OBJECT .NE. 49 .AND. OBJECT .NE. 76 .AND.
     2    OBJECT .NE. 55 .AND. OBJECT .NE. 78 .AND.
     3    OBJECT .NE.  8)        GOTO 730
      IF (OBJECT .NE. 55) GOTO 381
           IF (ROOM.NE.20.OR.(.NOT.BAT)) GOTO 201
                CALL DES(419)
                GOTO 25
  381 CONTINUE
      IF (OBJECT .NE. 76) GOTO 382
           IF (.NOT. WOLF) GOTO 201
                CALL DES(420)
                GOTO 25
  382 CONTINUE
      IF (OBJECT .NE. 49) GOTO 244
      IF (ROOM .NE. 8 ) GOTO 201
      IF (BUT  .EQ. 4 ) GOTO 740
           BUT = 4
           WRITE(6,1055)
           GOTO 25
  244 IF (OBJECT .NE.  8) GOTO 266
           IF (ITEMS(8).NE.ROOM .AND. ITEMS(8).NE.-1) GOTO 201
                HUNCH = 2
                IF (ITEMS(8) .EQ. -1) NUMB=NUMB-1
           WRITE(6,1056)
  245 ITEMS(OBJECT) = 0
      GOTO 25
  266  IF (OBJECT .NE. 78) GOTO 730
           IF (ROOM .NE. 86 .OR. HOLE) GOTO 201
          WRITE(6,1123)
          GOTO 25
  246 WRITE(6,1060)
      GOTO 25
  247 CONTINUE
      IF (MASTER .EQ. 0) GOTO 201
      IF (MASTER .EQ. 4 .OR. ROOM .NE. 43) GOTO 740
      IF (ITEMS( 6) .NE. -1) GOTO 250
           WRITE(6,1087)
           MASTER = 4
           ITEMS( 6) = 0
           NUMB = NUMB - 1
           SCORE=SCORE+25
           DO 252 II=1,10
                WHER(II) = 2
  252      CONTINUE
           WRITE(6,1130)
           WRITE(6,1131)
           WRITE(6,1132)
           GOTO 25
  250 WRITE(6,1086)
      GOTO 25
  122 CONTINUE
C     ---FEED---
      IF (OBJECT .NE. 55) GOTO 251
      IF (ROOM .NE. 20) GOTO 201
           IF (ITEMS( 5) .EQ. -1) GOTO 248
           IF (ITEMS(22) .EQ. -1) GOTO 249
                 WRITE(6,1050)
                 GOTO 25
  248 ITEMS( 5) = 0
      BAT       = .FALSE.
      BOTTLE = .FALSE.
      BLOOD  = .FALSE.
      WRITE(6,1051)
      GOTO 25
  249 WRITE(6,1052)
      GOTO 25
  251 CONTINUE
      IF (ITEMS(22) .NE. -1) GOTO 224
      IF (OBJECT .NE. 8) GOTO 265
      IF (ITEMS(8) .NE. ROOM) GOTO 201
           HUNCH=1
           ITEMS(22) = 0
           NUMB      = NUMB - 1
           WRITE(6,1090)
           GOTO 25
  265 IF (OBJECT .NE. 76) GOTO 253
      IF (.NOT. WOLF) GOTO 201
      WRITE(6,1091)
      GOTO 25
  253 IF (OBJECT .NE. 49) GOTO 267
      IF (ROOM .NE. 8) GOTO 201
      IF (BUT .EQ. 4) GOTO 740
           WRITE(6,1092)
           GOTO 25
  267 IF (OBJECT .NE. 78) GOTO 700
      IF (ROOM .NE. 86 .OR. HOLE) GOTO 201
           WRITE(6,1124)
           GOTO 25
  224 WRITE(6,1097)
      GOTO 25
  123 CONTINUE
C     ---EAT---
      IF (OBJECT .NE. 22) GOTO 730
      IF (ITEMS(22).NE.-1 .AND. ITEMS(22).NE.ROOM) GOTO 720
      WRITE(6,1017)
      ITEMS(22) = -3
      GOTO 25
  124 CONTINUE
C     ---DRINK---
      IF (OBJECT .GT. NITEMS) GOTO 730
      IF (OBJECT.EQ.26.AND.(ROOM.EQ.32.OR.ROOM.EQ.40
     2     .OR. ROOM .EQ. 68 .OR. ROOM .EQ. 81)) GOTO 254
      IF (ITEMS(OBJECT) .NE. -1 .AND.
     2    ITEMS(OBJECT) .NE. ROOM) GOTO 720
      IF (OBJECT .EQ. 5) GOTO 730
      IF (OBJECT .NE.26) GOTO 385
           BOTTLE = .FALSE.
           WATER  = .FALSE.
           ITEMS(26) = 0
  254      WRITE(6,1018)
           GOTO 25
  385 CONTINUE
      IF(OBJECT .NE. 25) GOTO 260
      WRITE(6,1113)
      ITEMS(25)=0
      GOTO 25
  260 IF (OBJECT .NE. 7) GOTO 700
           WRITE(6,1104)
           GOTO 25
  125 CONTINUE
C     ---JUMP---
      IF(ROOM.EQ.42) GOTO 259
      IF (ROOM .NE. 1 .AND. ROOM .NE. 10) GOTO 258
      IF (ROOM .NE. 10) GOTO 256
      IF (WIND2 .NE. 3) GOTO 255
           ROOM = 39
          LROOM = 0
          GOTO 622
  255 WRITE(6,1080)
      GOTO 25
  256 IF (WIND1 .NE. 3) GOTO 255
           II = 801
           GOTO 900
  258 IF (ROOM .NE. 38) GOTO 383
           WRITE(6,1059)
           GOTO 902
  383 IF (ROOM .NE. 67 .AND. ROOM .NE. 76) GOTO 255
           WRITE(6,1105)
           WRITE(6,1106)
           GOTO 902
  259 CONTINUE
      II=805
      GOTO 900
  126 CONTINUE
C     ---INVENTORY---
      CALL INVENT(NITEMS)
      GOTO 25
  127 CONTINUE
C     ---OPEN---
      IF (ROOM .EQ. 86 .OR. ROOM .EQ. 88) GOTO 393
      IF (OBJECT .EQ. 0 .AND. ROOM .LE. 25) OBJECT = 47
      IF (OBJECT .EQ. 7) GOTO 385
      IF (OBJECT .NE. 47) GOTO 275
      IF (ROOM .EQ. 6) GOTO 272
      IF (DOOR(ROOM) .EQ. -2) GOTO 276
      IF (DOOR(ROOM) .LT. 0) GOTO 273
           IF (DOOR(ROOM) .EQ. 0) WRITE(6,1019)
  271      IF (DOOR(ROOM) .EQ. 2) WRITE(6,1020)
           IF (DOOR(ROOM) .NE. 1) GOTO 25
                DOOR(ROOM) =2
                GOTO 800
  393 CONTINUE
      WRITE(6,1125)
      GOTO 25
  272 IF(DOOR(6) .EQ. 0) WRITE(6,1021)
      IF(DOOR(6) .EQ. 2) WRITE(6,1020)
      IF(DOOR(6) .NE. 1) GOTO 25
            DOOR(6) = 2
            ROOM = 7
            LROOM= 6
            SCORE = SCORE + NOTVAL
            NOTVAL = 0
            GOTO 20
  273 WRITE(6,1025)
      GOTO 25
  276 WRITE(6,1036)
      GOTO 25
  275 IF (OBJECT .NE. 38) GOTO 277
      IF (ROOM .NE. 1) GOTO 201
      SHUTTR = 1
      WRITE(6,1022)
      CALL DES(428)
      GOTO 25
  277 IF (OBJECT .NE. 37) GOTO 278
      IF (ROOM .NE. 1 .AND. ROOM .NE. 10) GOTO 201
      IF (ROOM .EQ. 1 .AND. SHUTTR .EQ. 0) GOTO 201
      WRITE(6,1023)
      GOTO 25
  278 IF (OBJECT .NE. 48) GOTO 279
      IF (ROOM .NE. 1) GOTO  201
      IF (ITEMS(19) .EQ. 0) ITEMS(19)=1
      IF (ITEMS(19) .NE. 1) GOTO 800
           WRITE(6,1024)
           SCORE = SCORE + VALUE(19)
           VALUE(19) = 0
           GOTO 25
  279 IF (OBJECT .NE. 33) GOTO 280
           WRITE(6,1026)
           GOTO 25
  280 IF (OBJECT .NE. 46) GOTO 282
      IF (ROOM .NE. 13) GOTO 281
      IF (ITEMS(4) .EQ. -3) ITEMS(4) = ROOM
      IF (ITEMS(4) .NE. ROOM)GOTO 281
           WRITE(6,1027)
           GOTO 25
  281 WRITE(6,1028)
      GOTO 25
  282 IF (OBJECT .NE. 40) GOTO 285
           IF (ROOM .NE. 43) GOTO 314
           IF(MASTER.LT.4) MASTER = 1 + SUN*2
           II = 423 + MASTER
           CALL DES(II)
           GOTO 25
  285 GOTO 314
  128 CONTINUE
C     ---CLOSE---
      IF (OBJECT .EQ. 0 .AND. ROOM .LE. 25) OBJECT = 47
      IF (OBJECT .NE. 47) GOTO 284
      IF (ROOM .EQ. 7) GOTO 283
      IF (DOOR(ROOM) .EQ. -2) GOTO  276
      IF (DOOR(ROOM) .LT. 0) GOTO 272
      IF (DOOR(ROOM) .EQ. 2) DOOR(ROOM) = 1
      GOTO 800
  283 ROOM = 6
      LROOM = 0
      GOTO 800
  284 IF(OBJECT .NE. 38) GOTO 286
      IF (ROOM .NE. 1) GOTO 201
           SHUTTR = 0
           GOTO 800
  286 IF (OBJECT .NE. 37) GOTO 288
      IF (ROOM .NE. 1) GOTO 287
      IF (WIND1 .NE. 1) GOTO 800
           WRITE(6,1031)
           GOTO 25
  287 IF (ROOM .NE. 10) GOTO 201
      IF (WIND2 .NE. 1) GOTO 800
           WRITE(6,1031)
           GOTO 25
  288 IF (OBJECT .NE. 48) GOTO 29
      IF (ROOM .NE. 1) GOTO 201
      IF (ITEMS(19) .EQ. 1) ITEMS(19)=0
      GOTO 800
  290 IF (OBJECT .NE. 46) GOTO 289
           WRITE(6,1032)
           GOTO 25
  289 IF (OBJECT .NE. 40) GOTO 740
      IF (MASTER .GE.  3) GOTO 740
            IF(MASTER.LT.3) MASTER = 0
            GOTO 800
  129 CONTINUE
C     ---LOCK---
      IF (OBJECT .NE. 47) GOTO 700
      IF (ROOM .EQ. 21) GOTO 296
      IF (DOOR(ROOM) .EQ. -1) GOTO 273
      IF (DOOR(ROOM) .EQ. -2) GOTO 276
      IF (ITEMS(4)   .NE. -1) GOTO 292
           DOOR(ROOM) = 0
           WRITE(6,1019)
           GOTO 25
  296 LOCK = 0
      DOOR(21) = 0
      GOTO 800
  292 WRITE(6,1067)
      GOTO 25
  130 CONTINUE
C     ---UNLOCK---
      IF (OBJECT .EQ. 0 .AND. ROOM .LE. 25) OBJECT = 47
      IF (OBJECT .NE. 47) GOTO 730
      IF (ROOM   .EQ. 21) GOTO 294
      IF (DOOR(ROOM) .EQ. -1) GOTO 273
      IF (DOOR(ROOM) .EQ. -2) GOTO 271
      IF (ITEMS(4)   .NE. -1) GOTO 292
           DOOR(ROOM) = 1
           GOTO 800
  294 WRITE(6,1068)
      GOTO 25
  131 CONTINUE
C     ---ON---
      IF (OBJECT .EQ. 0) OBJECT = 21
  132 CONTINUE
C     ---LIGHT---
      IF (OBJECT .GT. NITEMS) GOTO 700
      IF (ITEMS(21) .EQ. ROOM .AND. OBJECT .EQ. 21) GOTO 319
      IF (OBJECT .EQ. 27 .AND. ROOM .EQ. 86) GOTO 324
      IF (ITEMS(OBJECT).NE.-1 .AND. ITEMS(OBJECT).NE.ROOM) GOTO 720
      IF (OBJECT .NE. 21) GOTO 320
  319 CONTINUE
      IF (LAMP .EQ. 3) GOTO 471
           IF (LAMP .EQ. 0) II=0
           LAMP = 1
           IF (LMOVE .GT. 75) LAMP = 2
           IF (II .EQ. 0 .AND. ROOM .GT. 40) GOTO 410
           GOTO 712
  471 WRITE(6,1009)
      GOTO 25
  320 CONTINUE
  321 IF (OBJECT .NE. 15) GOTO 323
          MATCH = 1
          IF (ROOM.GT.40 .AND. (LAMP.EQ.0.OR.LAMP.EQ.3)) GOTO 410
          GOTO 712
  323 IF (OBJECT .NE. 27) GOTO 477
  324      IF (ITEMS(15) .NE. -1) GOTO 322
           IF (MATCH     .NE.  1) GOTO 322
           IF (ROOM .EQ. 86 .AND. .NOT. HOLE) GOTO 476
             IF (ITEMS(27) .NE. -1) GOTO 700
                II = 803
                GOTO 900
  477 CONTINUE
      IF  (OBJECT .NE. 13) GOTO 730
           IF (MATCH .NE. 1) GOTO 322
           TORCH = .TRUE.
           WRITE(6,1121)
           GOTO 25
  322 CONTINUE
      WRITE(6,1075)
      GOTO 25
  476 CONTINUE
      IF (ITEMS(27) .NE. -3) GOTO 700
      WRITE(6,1120)
      WRITE(6,1117)
      ITEMS(27) = 0
      HOLE = .TRUE.
      GOTO 25
  133 CONTINUE
C     ---OFF---
      IF (OBJECT .EQ. 0) OBJECT = 21
  134 CONTINUE
C     ---EXTINGUISH---
      IF (OBJECT .EQ. 34) GOTO 391
      IF (OBJECT .GT. NITEMS) GOTO 700
      IF (ITEMS(OBJECT) .NE. -1 .AND.
     2    ITEMS(OBJECT) .NE. ROOM) GOTO 720
  293 IF (OBJECT .NE. 21) GOTO 295
           LAMP = 0
           GOTO 710
  295 CONTINUE
  298 IF (OBJECT .NE. 15) GOTO 399
           MATCH = 0
           GOTO 710
  399 IF (OBJECT .NE. 13) GOTO 730
           TORCH = .FALSE.
           GOTO 800
  391 CONTINUE
      IF (ITEMS(26) .NE. -1) GOTO 392
           ITEMS(26)=0
           BOTTLE = .FALSE.
           WATER  = .FALSE.
           FIRE    = .FALSE.
           WRITE(6,1103)
           GOTO 25
  394 IF (OBJECT .NE. 13) GOTO 392
      IF (ITEMS(13).NE.ROOM .AND. ITEMS(13).NE. -1) GOTO 201
           TORCH = .FALSE.
           GOTO 800
  392 WRITE(6,1102)
      GOTO 25
  135 CONTINUE
C     ---LOOK---
      PREV(ROOM) = 0
      GOTO 20
  136 CONTINUE
C     ---SCORE---
      CALL ADSCOR(II)
      II = SCORE + II
      WRITE(6,9998) NUMOVE, II
      GOTO 25
  137 CONTINUE
C     ---BREAK---
      IF (OBJECT .NE. 56) GOTO 305
      IF (ROOM .NE. 6) GOTO 201
           IF (ITEMS(3).NE.-1 .AND. ITEMS(3).NE.ROOM) GOTO 297
                WRITE(6,1041)
                DOOR(6) = 1
                GOTO 25
  297 WRITE(6,1039)
      CALL YORN(II)
      IF (II .EQ. 0) GOTO 25
           WRITE(6,1040)
           GOTO 25
  305 IF (OBJECT .NE. 37) GOTO 299
      IF (ROOM .NE. 1) GOTO 304
           IF (WIND1 .GE. 1) WRITE(6,1046)
           IF (WIND1 .GE. 1) GOTO 25
           WIND1 = 1
           GOTO 800
  304 CONTINUE
      IF (ROOM .NE. 10) GOTO 201
      IF (WIND2 .GE. 1) WRITE(6,1046)
      IF (WIND2 .GE. 1) GOTO 25
      WIND2 = 3
      GOTO 800
  299 CONTINUE
      IF (OBJECT .NE. 50) GOTO 309
           IF (ROOM .EQ. 55) GOTO 303
           IF (IPASS .EQ. 1 .AND. ROOM .EQ. 17) GOTO 201
           IF (ROOM .NE. 17) GOTO 308
           IPASS = 1
           SCORE = SCORE + 10
           CALL DES(412)
           GOTO 25
  308 CONTINUE
      IF (ROOM .NE. 27) GOTO 201
      WRITE(6,1048)
      GOTO 902
  309 CONTINUE
      IF (OBJECT.EQ.47 .AND. ROOM.EQ.99) GOTO 767
      WRITE(6,1084)
      GOTO 25
  303 WRITE(6,1111)
      GOTO 25
  138 CONTINUE
  139 CONTINUE
C     ---POUR---
      IF (OBJECT .GT. NITEMS) GOTO 740
      IF (ITEMS(OBJECT) .NE. -1) GOTO 201
      IF (OBJECT .EQ.  5 .OR. OBJECT .EQ. 25. OR.
     2    OBJECT .EQ. 26) GOTO 112
      IF (OBJECT .EQ. 7) GOTO 385
      IF (OBJECT .NE. 1) GOTO 740
           IF (ITEMS(21) .NE. -1) GOTO 112
                LMOVE = 0
                IF (LAMP .EQ. 2) LAMP = 1
                IF (LAMP .EQ. 3) LAMP = 0
                ITEMS(1) = 0
                NUMB = NUMB - 1
                GOTO 25
  140 CONTINUE
C     ---BACK---
      IF (LROOM .GT. 0) GOTO 291
           WRITE(6,1035)
           GOTO 25
  291 CONTINUE
      I    = ROOM
      ROOM  = LROOM
      LROOM = I
      NUMOVE = NUMOVE + 1
      GOTO 20
  141 CONTINUE
C     ---SWIM---
      IF (ROOM .NE. 32 .AND. ROOM .NE. 68 .AND.
     1ROOM .NE. 81 .AND. ROOM .NE. 40) GOTO 314
      WRITE(6,1076)
      GOTO 25
  142 CONTINUE
C     ---MELT---
      IF (OBJECT .NE. 79) GOTO 730
      IF (ROOM .NE. 65) GOTO 201
      IF (ITEMS(13) .NE. -1 .OR. .NOT. TORCH) GOTO 740
      IF (       MELT) GOTO 769
           MELT = .TRUE.
           WRITE(6,1116)
           IF (.NOT. WATER) ITEMS(26) = ROOM
           CALL OBJ(ITEMS,ROOM,SCORE,VALUE)
           GOTO 25
  143 CONTINUE
C     ---CROSS---
      IF (ROOM .NE. 32 .AND. ROOM .NE. 40) GOTO 351
           IF (ITEMS(14) .NE. -1) GOTO 352
                IF (ROOM .NE. 32) GOTO 350
                     ROOM = 40
                     LROOM= 0
                     GOTO 103
  350           ROOM = 32
                LROOM= 0
                GOTO 103
  351 CONTINUE
      IF (ROOM .NE. 68 .AND. ROOM .NE. 81) GOTO 314
           IF (ITEMS(14) .NE. -1) GOTO 352
                IF (ROOM .EQ. 68) GOTO 356
                     ROOM = 68
                     LROOM= 0
                     GOTO 103
  356 CONTINUE
      ROOM  = 81
      LROOM = 0
      GOTO 103
  352 WRITE(6,1082)
      GOTO 25
  144 CONTINUE
C     ---QUIT---
      GOTO 9000
  145 CONTINUE
C     ---HONK---
      IF (ROOM .LT. 95) GOTO  262
      IF (ROOM .NE. 99) GOTO 398
      IF (MAS1) SCORE=SCORE + 2
      IF (MAS2) SCORE=SCORE + 1
      IF (MAS3) SCORE=SCORE + 1
      IF (MAS4) SCORE=SCORE + 1
      ROOM = 100
      BRIEF= 0
      GOTO 20
  398 CONTINUE
      IF (ROOM.EQ.95 .OR. ROOM.EQ.97) WRITE(6,1142)
      IF (ROOM.EQ.96 .OR. ROOM.EQ.98) WRITE(6,1143)
      NDEATH = 3
      GOTO 902
  146 CONTINUE
C     ---TIE---
      IF (OBJECT .NE.  9) GOTO 409
      IF (ITEMS(9) .NE. -1) GOTO 201
      IF (ROOM     .NE.  1) GOTO 411
      IF (ROPE     .EQ.  3) GOTO 411
           ROPE = 1
           ITEMS(9) = 1
  353 WRITE(6,1069)
      GOTO 25
  409 CONTINUE
      IF (OBJECT .NE. 16) GOTO 700
  411      IF (ITEMS(16).NE.-1 .OR. ITEMS(9).NE.-1) GOTO 314
                ROPE = 3
                WRITE(6,1010)
                GOTO 25
  147 CONTINUE
C     ---UNTIE---
      IF (OBJECT .NE. 9) GOTO 406
      IF (ROPE .EQ. 3) GOTO 407
      IF (ITEMS(9) .NE. ROOM) GOTO 201
      IF (ROPE     .NE. 1) GOTO 317
           ROPE = 0
           GOTO 800
  317 IF (ROPE .NE. 2) GOTO 314
           ROPE = -2
           WRITE(6,1071)
Check if STATUE,MONEY, and SWORD found. If not, reduce MAX (which
C   determines when every treasure has been found) because the
C   rope is necessary to find each of these treasures.
           IF (VALUE(12) .NE. 0) MAX=MAX-10
           IF (VALUE(17) .NE. 0) MAX=MAX-10
           IF (VALUE(29) .NE. 0) MAX=MAX-10
           ITEMS(9) = 0
           GOTO 25
  406 CONTINUE
      IF (OBJECT .NE. 16) GOTO 700
  407 IF (ITEMS(16) .NE. -1 .AND. ITEMS(16) .NE. ROOM) GOTO 201
      IF (PREC) GOTO 408
           ROPE = 0
           GOTO 800
  408 WRITE(6,1071)
      ITEMS(9) = 83
      ROPE = 0
      GOTO 25
  148 CONTINUE
C     ---READ---
      IF (OBJECT .NE. 46) GOTO 300
      IF (ROOM .NE. 13 .AND. ROOM .NE. 10) GOTO 201
      GOTO 281
  300 IF (OBJECT .NE. 52) GOTO 700
      IF (BUT  .NE. 2) GOTO 201
           SCORE = SCORE + BUTVAL
           BUTVAL = 0
           IF (NOTE .GE. 1) GOTO 302
           NOTE = 1
           WRITE(6,1029)
           BUT = 0
           GOTO 25
  302 NOTE = 2
      WRITE(6,1030)
      BUT = 3
      GOTO 25
  149 CONTINUE
C     ---FILL---
      IF (OBJECT .GT. NITEMS) GOTO 700
      IF (ITEMS(OBJECT) .NE. -1) GOTO 720
      IF (OBJECT .NE. 18) GOTO 427
           IF (BOTTLE) GOTO 218
           IF ((ROOM.EQ.32.OR.ROOM.EQ.40.OR.ROOM.EQ.68) .OR.
     2       ITEMS(26) .EQ. ROOM) OBJECT = 26
      IF (ITEMS(5) .EQ. ROOM) OBJECT = 5
      IF (OBJECT .EQ. 18) GOTO 740
      GOTO 111
  427 IF (OBJECT .NE. 21) GOTO 740
           IF (ITEMS(1) .NE. -1) GOTO 740
                IF (LAMP .EQ. 2) LAMP=1
                IF (LAMP .EQ. 3) LAMP=0
                ITEMS(1) = 0
                NUMB = NUMB - 1
                LMOVE = -75
                GOTO 800
  150 CONTINUE
C     ---HINT---
      II = ROOM
      CALL HELP(II,OBJECT)
      GOTO 25
  151 CONTINUE
C     --GOTO--
C     CALL GUINFO(2,SIGID)
C     IF(SIGID.EQ.AWCC.OR.SIGID.EQ.A6L2.OR.
C    1SIGID.EQ.A3TB) GOTO 425
      WRITE(6,1114)
      GOTO 25
C425  CONTINUE
C     WRITE(6,1115)
C     READ(5,426) ROOM
C426  FORMAT(I3)
C     IF (ROOM .GT. 100) GOTO 425
C     GOTO 428
  152 CONTINUE
C     ---LEFT---
      IF (ROOM .NE. 21) GOTO 314
      IF (LOCK .GT. 0) GOTO 312
           IF (OBJECT .NE. 42) GOTO 316
           LOCK = 1
           GOTO 800
  312 IF (LOCK .EQ. 1) GOTO 316
      IF (OBJECT .NE. 44) GOTO 316
           LOCK = 3
           SCORE = SCORE + LOKVAL
           LOKVAL = 0
           DOOR(21) = 1
           WRITE(6,1015)
           GOTO 25
  153 CONTINUE
C     ---RIGHT---
      IF (ROOM .NE. 21) GOTO 314
      IF (LOCK .NE. 1 ) GOTO 316
      IF (OBJECT .NE. 43) GOTO 316
           LOCK = 2
           GOTO 800
  314 CONTINUE
      WRITE(6,1013)
      GOTO 25
  316 CONTINUE
      WRITE(6,1014)
      LOCK = 0
      GOTO 25
  154 CONTINUE
C     ---SHOOT---
      IF (ITEMS(20) .NE. -1) GOTO 700
      IF (GUN) GOTO 480
           WRITE(6,1054)
           GOTO 25
  480 CONTINUE
      GUN = .FALSE.
      IF (GNOME .AND. (OBJECT.EQ.0.OR.OBJECT.EQ.77)) GOTO 770
      IF (OBJECT .EQ. 76) GOTO 481
      IF (OBJECT .EQ.  8) GOTO 482
      IF (OBJECT .EQ. 49) GOTO 483
      IF (OBJECT .EQ. 55) GOTO 484
      IF (OBJECT .EQ. 78) GOTO 486
      IF (OBJECT .EQ. 80) GOTO 262
  481 IF (OBJECT .NE. 76 .AND. (.NOT. WOLF)) GOTO 482
           IF (.NOT. WOLF) GOTO 201
           IF (RDM(SEED) .LT. .15) GOTO 487
           WOLF      = .FALSE.
           ITEMS( 2) = ROOM
           WRITE(6,1053)
           CALL DES(602)
           GOTO 25
  482 CONTINUE
      IF (ITEMS(8) .NE. ROOM .AND. ITEMS(8) .NE. -1 .AND.
     2    OBJECT .NE. 8) GOTO 483
           IF (ITEMS(8).NE.ROOM .AND. ITEMS(8).NE.-1) GOTO 201
           IF (ITEMS(8) .EQ. -1) NUMB=NUMB-1
           ITEMS(8) = 0
           ITEMS(2) = 0
           HUNCH = 2
           GOTO 25
  483 IF (ROOM .NE. 8 .AND. OBJECT .NE. 49) GOTO 484
           IF (ROOM .NE. 8) GOTO 201
           IF (BUT .EQ. 4) GOTO 485
           BUT       = 4
           ITEMS( 2) = 0
            WRITE(6,1055)
           GOTO 25
  484 IF (ROOM .NE. 20 .AND. OBJECT .NE. 55) GOTO 486
               IF (.NOT. BAT) GOTO 485
              WRITE(6,1058)
           ITEMS(2) = ROOM
              GOTO 25
  485 WRITE(6,1057)
      GUN  = .TRUE.
      NUMB = NUMB + 1
      GOTO 25
  486 IF (OBJECT .NE. 78 .AND. ROOM .NE. 86) GOTO 485
      IF (HOLE) GOTO 485
           WRITE(6,1126)
           ITEMS( 2) = 0
           GOTO 25
  487 WRITE(6,1065)
      ITEMS(2) = ROOM
      II = 1
      CALL WWOLF(II,WOLF)
      IF (II .EQ. 0) GOTO 25
            GOTO 902
  155 CONTINUE
C     ---WAKE---
      IF (OBJECT .NE. 49) GOTO 490
      IF (BUT .LT. 3) GOTO 488
           WRITE(6,1044)
           GOTO 25
  488 BUT = 1
      IF (ITEMS(10) .EQ. -3) BUT=2
      IF (ITEMS(10) .EQ. 8 .AND. ITEMS(11) .EQ. 8) BUT = 2
      IF (BUT .NE. 2) GOTO 489
           ITEMS(10) = -3
           ITEMS(11) = -3
  489 CONTINUE
      II = 400 + BUT
      CALL DES(II)
      GOTO 25
  490 CONTINUE
      IF (OBJECT .NE. 39) GOTO 491
      IF (ROOM .NE. 43) GOTO 201
      IF (MASTER .EQ. 0 .OR. MASTER .EQ. 4) GOTO 740
           MASTER = 3
           CALL DES(426)
           GOTO 25
  491 CONTINUE
      GOTO 740
  156 CONTINUE
C     ---POOF---
      IF (ROOM .NE. 56 .AND. ROOM .NE. 15) GOTO 262
      IF (DOOR(2) .EQ. 0) GOTO 262
      IF (ITEMS( 8) .EQ. -1) ITEMS( 8)=57
      IF (ITEMS(14) .EQ. -1) ITEMS(14)=ROOM
      IF (ROOM .EQ. 56) GOTO 547
           ROOM  = 56
           LROOM = 0
           GOTO 20
  547 ROOM  = 15
      LROOM = 0
      GOTO 20
  157 CONTINUE
C     ---SUSPEND---
      II = 1
      SAVE=.TRUE.
      CALL SAVRES(II)
      GOTO 9000
  158 CONTINUE
C     ---RESTORE---
      II = 0
      CALL SAVRES(II)
      GOTO 20
  159 CONTINUE
C     ---DEBUG---
      WRITE(6,7101)
      READ (6,7102) II
      IF (II .NE. PW) GOTO 730
 7101 FORMAT('0  ENTER THE PASSWORD:')
 7102 FORMAT(A4)
      DEBUG = .TRUE.
      GOTO 800
  160 CONTINUE
C     ---VERBOSE---
      IBRIEF = 0
      DEBUG = .FALSE.
      GOTO 800
  161 CONTINUE
C     ---BRIEF---
      IBRIEF = 1
      WRITE(6,1073)
      GOTO 25
  700 CONTINUE
      WRITE(6,1002)
      GOTO 25
  710 WRITE(6,1037)
      IF (ROOM .GT. 40 .AND. ((LAMP.EQ.0.OR.LAMP.EQ.3)
     2                 .AND.   MATCH.EQ.0)) WRITE(6,1064)
      GOTO 25
  712 WRITE(6,1038)
      GOTO 25
  720 CONTINUE
      WRITE(6,1004)
      GOTO 25
  730 WRITE(6,1016)
      GOTO 25
  740 WRITE(6,1005)
      GOTO 25
  750 WRITE(6,1096)
      GOTO 25
  760 WRITE(6,1107)
      GOTO 25
  765 ITEMS(13) =86
      WRITE(6,1133)
      GOTO 902
  767 CONTINUE
      MASECT = .TRUE.
      WRITE(6,1136)
      GOTO 25
  769 WRITE(6,1144)
      GOTO 902
  770 IF (ACTION(1) .EQ. 54) GOTO 775
      VAL = RDM(SEED)
      IF (OBJECT .EQ. 3) ITEMS(3)=ROOM
      IF (OBJECT .EQ. 12 .OR. OBJECT .EQ. 77) ITEMS(12)=ROOM
      NUMB = NUMB - 1
      IF (VAL .LT. 0.8) GOTO 772
           WRITE(6,1109)
      II = 0
      CALL GGNOME(II,GNOME)
      IF (II .GT. 0) GOTO 902
           GOTO 25
  772 WRITE(6,1108)
      GNOME = .FALSE.
      CALL OBJ(ITEMS,ROOM,SCORE,VALUE)
      GOTO 25
  775 WRITE(6,1108)
      GNOME = .FALSE.
      ITEMS(2) = 44
      GOTO 25
  800 CONTINUE
      WRITE(6,1008)
      GOTO 25
  900 CONTINUE
C     ---DEATH AND REINCARNATION---
      CALL DES(II)
  902 NDEATH = NDEATH + 1
      LAMP = 0
      NUMB   = 0
      SCORE  = SCORE - 10
      GNOME  = .FALSE.
      WOLF   = .FALSE.
      IF (ITEMS(20) .EQ. -1) GUN   = .FALSE.
      IF (ITEMS(18) .EQ. -1) BOTTLE= .FALSE.
      IF (ITEMS(26) .EQ. -1) WATER = .FALSE.
      IF (ITEMS( 5) .EQ. -1) BLOOD = .FALSE.
      IF (ITEMS( 7) .EQ. -1) ITEMS( 7)=IFIX(RDM(SEED)*7+57)
      IF (ITEMS(12) .EQ. -1) ITEMS(12)=IFIX(RDM(SEED)*7+57)
      IF (ITEMS(17) .EQ. -1) ITEMS(17)=IFIX(RDM(SEED)*7+57)
      IF (ITEMS(19) .EQ. -1) ITEMS(19)=IFIX(RDM(SEED)*7+57)
      IF (ITEMS(23) .EQ. -1) ITEMS(23)=IFIX(RDM(SEED)*7+57)
      IF (ITEMS(24) .EQ. -1) ITEMS(24)=IFIX(RDM(SEED)*7+57)
      IF (ITEMS(28) .EQ. -1) ITEMS(28)=IFIX(RDM(SEED)*7+57)
      IF (ITEMS(29) .EQ. -1) ITEMS(29)=IFIX(RDM(SEED)*7+57)
      IF (ITEMS(30) .EQ. -1) ITEMS(30)=IFIX(RDM(SEED)*7+57)
      GOTO (905,920,935,9100), NDEATH
  905 CONTINUE
      CALL DES(850)
      CALL YORN(II)
      IF (II .EQ. 0) GOTO 9000
           CALL DES(851)
C     RESET LOCATION, ITEMS, ETC. AFTER DEATH.
           ROOM   = 1
           LROOM  = 0
           DO 906 II=1,NITEMS
                IF (ITEMS(II) .EQ. -1)
     2             ITEMS(II)=IFIX(RDM(SEED)*19+2)
  906      CONTINUE
           GOTO 20
  920 CONTINUE
      CALL DES(852)
      CALL YORN(II)
      IF (II .EQ. 0) GOTO 9000
            CALL DES(853)
            ROOM   = 9
            LROOM  = 0
            DO 922 II=1,NITEMS
                IF (ITEMS(II) .EQ. -1)
     2             ITEMS(II)=IFIX(RDM(SEED)*19+2)
  922       CONTINUE
            GOTO 20
  935 CONTINUE
      CALL DES(854)
C
 9000 CONTINUE
      IF(.NOT.SAVE) GOTO 9100
      CALL ADSCOR(II)
      II=SCORE+II
      WRITE(6,9998) NUMOVE, II
      GOTO 9900
C
 9100 CONTINUE
C     ---END OF GAME (WINNER)---
      CALL ADSCOR(II)
      II = SCORE + II
      WRITE(6,9997) II, MAXABS
      IF (II .GE. MAXABS) WRITE(6,7001)
      JJ = MAXABS / 4
      K = MAXABS - JJ
      IF (II .LT. MAXABS .AND. II .GE. K) WRITE(6,7002)
      L = K - JJ
      IF (II .LT. K      .AND. II .GE. L) WRITE(6,7003)
      K = L - JJ
      IF (II .LT. L      .AND. II .GE. K) WRITE(6,7004)
      L = K - JJ
      IF (II .LT. K      .AND. II .GE. L) WRITE(6,7005)
      IF (II .LT. L) WRITE(6,7006)
 9900 CONTINUE
C
C    *********
 1001 FORMAT('- Welcome to CASTLEQUEST!! Would you like instructions?')
 1002 FORMAT('0  That would be a neat trick.')
 1003 FORMAT('0  I don''t see that here.')
 1004 FORMAT('0  You don''t have it with you.')
 1005 FORMAT('0  I don''t think I can do that.')
 1006 FORMAT('0  There is no way to go in that direction.')
 1007 FORMAT('0  Load the gun with what?')
 1008 FORMAT('0  OK')
 1009 FORMAT('0  The lantern seems to be out of fluid.')
 1010 FORMAT('0  The rope is tied securely to the grappling hook.')
 1011 FORMAT('0  A rope is hooked to the top of the precipice.')
 1012 FORMAT('0  You can''t carry anything else.')
 1013 FORMAT('0  I don''t know how to apply that here.')
 1014 FORMAT('0  You blew it. I think you''d better start over.')
 1015 FORMAT('0  The lock is now open.')
 1016 FORMAT('0  You can''t be serious.')
 1017 FORMAT('0  YUMMY! That was good.')
 1018 FORMAT('0  GLUG GLUG GLUG GLUG BELCH!!')
 1019 FORMAT('0  The door is locked.')
 1020 FORMAT('0  The door is already open.')
 1021 FORMAT('0  The door is boarded up.')
 1022 FORMAT('0  The shutter is open, but there are bars over ',
     2 'the window.')
 1023 FORMAT('0  The window is nailed shut.')
 1024 FORMAT('0  There is a silver cross in the drawer!')
 1025 FORMAT('0  I see no door here.')
 1026 FORMAT('0  It wouldn''t be much of a lock if you could ',
     2 'just open it.')
 1027 FORMAT('0  A skeleton key falls out of the book.')
 1028 FORMAT('0  A literary classic, but we don''t have time to read.')
 1029 FORMAT('0  "The master loves Shakespeare".')
 1030 FORMAT('0  "Look behind the mirror."')
 1031 FORMAT('0  You can''t close a broken window.')
 1032 FORMAT('0  I never liked classics, anyway.')
 1033 FORMAT('0  Sorry, but you don''t have your library card.')
 1034 FORMAT('0  The butler is lame, and could not keep up with you.')
 1035 FORMAT('0  You can''t get back the way you came.')
 1036 FORMAT('0  The door will neither open nor close.')
 1037 FORMAT('0  The light is out.')
 1038 FORMAT('0  The light is burning dimly.')
 1039 FORMAT('0  With what?? Your bare hands??')
 1040 FORMAT('0  You just broke every bone in your hand.',/,
     2       '   You might try an axe or something....')
 1041 FORMAT('0  The door can now be opened.')
 1042 FORMAT('0  The door is closed.')
 1043 FORMAT('0  You have fallen through a trap door and find...')
 1044 FORMAT('0  The butler cannot be aroused.')
 1045 FORMAT('0  Take note of what??')
 1046 FORMAT('0  The window IS already broken.')
 1047 FORMAT('0  You had better watch your mouth.')
 1048 FORMAT('0  The mirrors shatter in an explosion of flying glass.')
 1049 FORMAT('0  A huge vampire bat hangs from the doorframe',
     2       ' and blocks your way.')
 1050 FORMAT('0  Feed the bat with what??')
 1051 FORMAT('0  The bat gulps down the blood and flitters away.')
 1052 FORMAT('0  Bats don''t eat food, they eat blood. Like yours.')
 1053 FORMAT('0  You killed a werewolf. An old gypsy woman', /,
     2          '   appears and drags away the body.')
 1054 FORMAT('0  Unfortunately the gun is not loaded.')
 1055 FORMAT('0  You killed a deaf-mute butler (Not very sporting',
     2          ' of you).')
 1056 FORMAT('0  You just murdered an innocent hunchback.')
 1057 FORMAT('0  There is nothing here to shoot.')
 1058 FORMAT('0  The bullet does not penetrate the bat''s thick hide.')
 1059 FORMAT('0  You swan dive off the tower and drown in the moat.')
 1060 FORMAT('0  You don''t have the sword with you.')
 1061 FORMAT('0  The acid dissolves the bars.  The window is clear.')
 1062 FORMAT('0  The werewolf howls in pain and runs away.')
 1063 FORMAT('0  The acid has burned a hole in the floor.')
 1064 FORMAT('0  It is now pitch dark. If you procede you may stumble'
     2       ' and fall.')
 1065 FORMAT('0  Your bullet misses.')
 1066 FORMAT('0  A combination lock bars the door.')
 1067 FORMAT('0  I''m afraid you don''t have the key.')
 1068 FORMAT('0  Unless you''re a safecracker, I suggest you ',
     2       'use the combination.')
 1069 FORMAT('0  The rope is anchored securely to the bed.')
 1070 FORMAT('0  The rope is dangling out the window.')
 1071 FORMAT('0  The rope slithers out the window and falls ',
     2       'out of reach.')
 1072 FORMAT('0  The rope is already out the window.')
 1073 FORMAT('0  OK, from now on I will give only short descriptions.')
 1074 FORMAT('0  You have nothing to carry it in.')
 1075 FORMAT('0  You have nothing to light it with.')
 1076 FORMAT('0  I hate to tell you this, but you can''t swim.')
 1077 FORMAT('0  You fall in the dark and break your neck.')
 1078 FORMAT('0  You haven''t any water.')
 1079 FORMAT('0  You bring it to the height of ecstasy.')
 1080 FORMAT('0  Jump from where??')
 1081 FORMAT('0  Something you''re carrying won''t fit into the house.')
 1082 FORMAT('0  You dont have a boat to cross in.')
 1083 FORMAT('0  You''re lamp is getting dim.  Perhaps you',/,
     2       '   should look for more fuel.')
 1084 FORMAT('0  You are not strong enough to break it.')
 1085 FORMAT('0  I took the liberty of filling the lamp with ',
     2       'kerosene.')
 1086 FORMAT('0  A vampire can only be killed with a wooden ',
     2       'stake.')
 1087 FORMAT('0  The vampire clutches at the stake and dies,',
     2       /, '   leaving only a pile of dust.')
 1088 FORMAT('0  Your match has burnt out.')
 1089 FORMAT('0  You''d better hurry.  The sun is setting.')
 1090 FORMAT('0  The hunchback gobbles down the food and',
     2       ' smiles at you.')
 1091 FORMAT('0  Werewolves eat only fresh meat.')
 1092 FORMAT('0  The butler is not allowed to eat.')
 1093 FORMAT('0  It''s not a good idea to take a hungry ',
     2       'hunchback.')
 1094 FORMAT('0  Count Vladimir clamps his fangs on your neck!!')
 1095 FORMAT('0  Nothing happens.')
 1096 FORMAT('0  You already have it.')
 1097 FORMAT('0  You don''t have any food.')
 1098 FORMAT('0  A copy of Shakespeare''s "HAMLET" lies on the ',
     2       'desk.')
 1099 FORMAT('0  The bottle is already full.')
 1100 FORMAT('0  A wall of fire bars the way to the NE.')
 1101 FORMAT('0  You can''t get through the fire.')
 1102 FORMAT('0  You don''t have any water.')
 1103 FORMAT('0  The fire smoulders and goes out.')
 1104 FORMAT('0  I''m sorry.  I don''t have a corkscrew.')
 1105 FORMAT('0  You plummet headlong into the crashing surf')
 1106 FORMAT('   and are *SPLATTERED* on the rocks below.')
 1107 FORMAT('0  The gnome is very nimble and dodges out of ',
     2       'your reach.')
 1108 FORMAT('0  You killed a dirty little gnome.')
 1109 FORMAT('0  You missed him, JERK!!')
 1110 FORMAT('0  There is a large opening in the ground.')
 1111 FORMAT('0  It is out of reach.')
 1112 FORMAT('0  The grappling hook and the rope',
     2      ' are lying on the ground.')
 1113 FORMAT('0  INDE--URP--GESTION !!!')
 1114 FORMAT('0  THAT IS A PRIVILEGED INSTRUCTION.')
 1115 FORMAT('0 ROOM #---')
 1116 FORMAT('0  Some ice has melted, leaving a large hole.')
 1117 FORMAT('0  There is a cyclops-shaped hole in the door.')
 1118 FORMAT('0  There is a fairly large cyclops staring at you.')
 1119 FORMAT('0  The cyclops turns to you and says:',/,
     2       '      "Hey buddy!.  Got a light??"')
 1120 FORMAT('0  The cyclops chokes from the rancid tobacco, and',/,
     2       '   crashes through the door in search of water.')
 1121 FORMAT('0  The torch is burning noisily.')
 1122 FORMAT('0  The cyclops hurls you against the wall ',
     2       'and chuckles quietly.')
 1123 FORMAT('0  The cyclops flings you across the room and ',
     2       'laughs hysterically.')
 1124 FORMAT('0  Boy are you dumb!  A cyclops doesn''t eat food.')
 1125 FORMAT('0  The door is way too heavy for you to move it.')
 1126 FORMAT('0  The cyclops does not even feel the impact of ',
     2       'the bullet.')
 1127 FORMAT('0  Some magical power will not let you pass.')
 1128 FORMAT('0  A powerful wizard blocks your way with his staff.')
 1129 FORMAT('0  The wizard''s eyes flare as he raises his staff.',/,
     2       '   His awesome magic prevents you from attacking.')
 1130 FORMAT('0  A note materializes on the wall which reads:')
 1131 FORMAT('0  EMERGENCY EXIT--The mirror maze will lead you')
 1132 FORMAT('0  to the locked door.  The exit lies within.')
 1133 FORMAT('0  The wizard raises his staff. You are'
     2       ' blinded by a sudden explosion of light.')
 1134 FORMAT('0  The walls of the cavern tremble as you unleash the',
     2  /,   '   terrible power contained in the sword.')
 1135 FORMAT('0  The wizard, sensing a stronger power than his own,'
     2 ,/,   '   flees in a blinding flash and a cloud of smoke.')
 1136 FORMAT('0  There is a passable hole in the door.')
 1137 FORMAT('0  The letter "H" appears for an instant on the wall.')
 1138 FORMAT('0  A mystical voice says "OH".')
 1139 FORMAT('0  The letter "N" forms out of mist.')
 1140 FORMAT('0  A large "K" emerges from the floor.')
 1141 FORMAT('0  Your axe is trembling slightly.')
 1142 FORMAT('0  The ceiling begins to vibrate and crumbles,',
     2  /,   '   crushing you under tons of concrete.')
 1143 FORMAT('0  The floor erupts violently, swallowing you',
     2       ' in a sea of molten lava.')
 1144 FORMAT('0  The glacier begins to melt in a torrential flood,',
     2  /,   '   and swallows you in a sea of icy cold water.')
 1145 FORMAT('0  I think I hear footsteps behind you.')
 1146 FORMAT('0  What do you want with a heavy, dead butler?')
C
 7001 FORMAT('0  This qualifies you as a "CLASS A" MASTER!')
 7002 FORMAT('0  You are a MASTER at CASTLEQUEST.')
 7003 FORMAT('0  You receive an EXPERT rating for your effort.')
 7004 FORMAT('0  You rate as a NOVICE EXPLORER for this game.')
 7005 FORMAT('0  You are a GREENHORN at this game!!')
 7006 FORMAT('0  You don''t deserve to WALK THE EARTH!!')
C
 9997 FORMAT('0  You scored ', I4, ' out of ', I4, ' points.')
 9998 FORMAT('0  You made ', I4, ' moves, and scored ',
     2       I4, ' points.')
 9999 FORMAT('0  THAT HAS NOT BEEN IMPLEMENTED YET.')
      STOP
      END
