C---------------------------------------------------
      SUBROUTINE WWOLF(II,WOLF)
      LOGICAL WOLF, DEBUG
      COMMON    DEBUG, ISEED
      SAVE XLIM
C
C     SEED = FLOAT(ISEED)
      VAL = RDM(SEED)
      IF (II .GT. 0) GOTO 75
      IF (WOLF) GOTO 50
      XLIM = .075
      IF (VAL .LE. .960) GOTO 800
           WOLF = .TRUE.
   50      WRITE(6,1001)
           VAL = RDM(SEED)
           IF (VAL .GT. .400) GOTO 900
   75      WRITE(6,1002)
           II = 0
           VAL = RDM(SEED)
           IF (VAL .GT. XLIM) GOTO 100
C GETS YOU.
                II = 1
                WRITE(6,1003)
                GOTO 900
  100 CONTINUE
C HE MISSES.
      XLIM = XLIM + .15
      WRITE(6,1004)
      GOTO 900
  800 CONTINUE
      II = 0
C SWIPE AND RUN AWAY.
      IF (VAL .LT. .900) GOTO 900
           WRITE(6,1005)
  900 CONTINUE
      IF (DEBUG) WRITE(6,8001) WOLF, XLIM
 8001 FORMAT('0  WOLF :', L2, 'XLIM:', F5.3)
 1001 FORMAT('0  There is a fearsome werewolf in the room with you!')
 1002 FORMAT('0  The werewolf attacks you with its sharp claw!')
 1003 FORMAT('0  It severs your jugular vein and mortally wounds you!')
 1004 FORMAT('0  It just misses your neck!')
 1005 FORMAT('0  A nasty werewolf lunges at you, takes a swipe at',/,
     2       '   your neck, misses and runs away.')
C
      RETURN
      END
