C--------------------------------------------------------------
      SUBROUTINE DES(ROOM)
C
      INTEGER FIRST(101), FF, LL, ROOM, INST(50,20), HINT(50,20)
      INTEGER FORM(100,20), FMT(20), FORM2(60,20), LONG(400,25)
      LOGICAL DEBUG, ISEED
      COMMON DEBUG
      COMMON /BLOCK1/ FORM, FORM2, INST, LONG, HINT
C
      DATA FIRST /1,6,8,12,16,19,23,26,31,33,37,40,
     2            42,45,49,51,52,54,56,58,61,64,66,67,71,73,76,
     3            79,81,83,85,89,92,95,97,101,104,106,110,112,
     4            117,124,129,135,140,142,145,149,154,156,
     5            158,160,163,165,168,173,176,177,178,179,
     6            180,181,182,183,184,190,193,203,206,211,214,
     7            217,222,228,231,236,240,244,247,250,255,260,
     8            262,266,269,273,275,283,287,291,293,300,
     9            303,309,312,317,323,328,332,333,341 /
C
      IF (DEBUG) WRITE(6,9797) ROOM
 9797 FORMAT('0  REQUEST FOR DESCRIPTION #', I4)
C
      IF (ROOM .EQ. 300) ROOM=100
C
      IF (ROOM .GT. 200 .AND. ROOM .LT. 300) GOTO 100
      IF (ROOM .GE. 400 .AND. ROOM .LT. 500) GOTO 190
      IF (ROOM .GT. 600 .AND. ROOM .LT. 700) GOTO 200
      IF (ROOM .EQ. 801) GOTO 50
      IF (ROOM .EQ. 803) GOTO 60
      IF (ROOM .EQ. 805) GOTO 64
      IF (ROOM .EQ. 850) GOTO 66
      IF (ROOM .EQ. 851) GOTO 68
      IF (ROOM .EQ. 852) GOTO 70
      IF (ROOM .EQ. 853) GOTO 72
      IF (ROOM .EQ. 854) GOTO 74
      FF = FIRST(ROOM)
      IF (FF .GT. 0) GOTO 3
           WRITE(6,2003) ROOM
           GOTO 900
    3 CONTINUE
      LL = FIRST(ROOM+1) - 1
      IF (DEBUG) WRITE(6,9898) FF, LL
 9898 FORMAT('0  THE FIRST AND LAST LINE OF LONG ARE: ',2I5)
      DO 10 II=FF,LL
        DO 5 J=1,20
             FMT(J) = LONG(II,J)
    5   CONTINUE
        WRITE(6,FMT)
   10 CONTINUE
      GOTO 900
   50 CONTINUE
      WRITE(6,7001)
      WRITE(6,7002)
      GOTO 900
   60 WRITE(6,7004)
      WRITE(6,7005)
      GOTO 900
   64 WRITE(6,7007)
      WRITE(6,7008)
      GOTO 900
   66 WRITE(6,7010)
      WRITE(6,7011)
      WRITE(6,7012)
      GOTO 900
   68 WRITE(6,7013)
      WRITE(6,7014)
      GOTO 900
   70 WRITE(6,7015)
      WRITE(6,7016)
      WRITE(6,7017)
      GOTO 900
   72 WRITE(6,7018)
      WRITE(6,7019)
      WRITE(6,7020)
      GOTO 900
   74 WRITE(6,7021)
      WRITE(6,7022)
      WRITE(6,7023)
      GOTO 900
C
  100 CONTINUE
      II = ROOM-200
      DO 110 J=1,20
           FMT(J) = FORM(II,J)
  110 CONTINUE
      WRITE(6,FMT)
      GOTO 900
C
  190 CONTINUE
      II = ROOM-369
      GOTO 205
  200 CONTINUE
      II = ROOM - 600
  205 DO 210 J=1,20
           FMT(J) = FORM2(II,J)
  210 CONTINUE
      WRITE(6,FMT)
C
  900 CONTINUE
C
 2001 FORMAT(' ', 72A1)
 2003 FORMAT('0  NO DESCRIPTION-MESSAGE NUMBER ', I4)
C
 7001 FORMAT('0  You leap through the open window and are')
 7002 FORMAT('   dashed into pieces on the rocks below.')
 7004 FORMAT('0  A thick black smoke fills the room, ',
     2       'engulfing you in')
 7005 FORMAT('   lethal choking fumes (smoking is bad for',
     2       ' your health).')
 7007 FORMAT('0  You leap into the pit and fall for hours.  You land ')
 7008 FORMAT('   on some moist "undead" bodies and are eaten alive.')
 7010 FORMAT('0  My, My.  You seem to have bitten the dust.')
 7011 FORMAT('   I can attempt to reincarnate you, but I''m')
 7012 FORMAT('   not very good at it. Should I try?')
 7013 FORMAT('0  You fall weightlessly through a thick mist.')
 7014 FORMAT('   Your head is spinning as you emerge and find',
     2       '...',/' ')
 7015 FORMAT('0  You seem to have died again.  I can try and ')
 7016 FORMAT('   reincarnate you, but you''re taxing my ',
     2       'patience.')
 7017 FORMAT('0  Would you like me to try??')
 7018 FORMAT('0  You float aimlessly through a green mist which')
 7019 FORMAT('   transcends time and space. You regain your ')
 7020 FORMAT('   senses and realize that ...',/' ')
 7021 FORMAT('0  You did it again, didn''t you? I''m afraid ',
     2      'that')
 7022 FORMAT('   all the mist has evaporated.  I''m so sorry,')
 7023 FORMAT('   but this means you are dead for good.')
 7024 FORMAT('0  You have fallen in the dark and broken ',
     2       'your neck.')
      RETURN
      END
