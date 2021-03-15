C---------------------------------------------------------------
      SUBROUTINE GGNOME(II,GNOME)
      LOGICAL GNOME, DEBUG
      COMMON DEBUG, ISEED
      SAVE XLIM
C
      SEED = FLOAT(ISEED)
      VAL = RDM(SEED)
      IF (GNOME) GOTO 50
      XLIM = .075
      IF (VAL .LE. .970) GOTO 900
      GNOME = .TRUE.
C          GNOME ATTACKS!
   50      WRITE(6,1001)
           IF (VAL .GT. 0.980) GOTO 900
           WRITE(6,1004)
           VAL = RDM(SEED)
           IF (VAL .GE. XLIM) GOTO 100
C                YOU DIE.
                 II = 1
                 WRITE(6,1002)
                 GOTO 900
  100 CONTINUE
      XLIM = XLIM + .20
      WRITE(6,1003)
  900 CONTINUE
      IF (DEBUG) WRITE(6,8001) GNOME, XLIM
 8001 FORMAT('0  GNOME:', L2,' XLIM:', F5.3)
 1001 FORMAT('0  There is an ugly little gnome in the room with you!')
 1002 FORMAT('0  IT GETS YOU!!')
 1003 FORMAT('0  It misses by an elf-hair!')
 1004 FORMAT('0  He shoots a poisoned dart at you!')
      RETURN
      END
