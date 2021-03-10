C________________________________________________
      SUBROUTINE INVENT(NITEMS)
C
C
      INTEGER NUMB,ITEMS(30),NITEMS
      COMPLEX*16 OBJ(30)
C
      COMPLEX*16 LOADED
      LOGICAL BLOOD, BOTTLE, WATER, GUN
      INTEGER SAVAR(400)
C
      COMMON /BLOCK2/ SAVAR
C
      EQUIVALENCE (SAVAR(91), BLOOD),(SAVAR(92), BOTTLE),
     2            (SAVAR(93), WATER),(SAVAR( 1), ITEMS(1)),
     3            (SAVAR(95), NUMB ),(SAVAR(63), GUN  )
C
      LOADED = TRANSFER('Bullet in gun', LOADED)
      OBJ(1) = TRANSFER('Kerosene', OBJ(1))
      OBJ(2) = TRANSFER('Silver bullet', OBJ(2))
      OBJ(3) = TRANSFER('Bloody hatchet', OBJ(3))
      OBJ(4) = TRANSFER('Skeleton key', OBJ(4))
      OBJ(5) = TRANSFER('Blood in bottle', OBJ(5))
      OBJ(6) = TRANSFER('Wooden stake', OBJ(6))
      OBJ(7) = TRANSFER('Champagne', OBJ(7))
      OBJ(8) = TRANSFER('Hunchback', OBJ(8))
      OBJ(9) = TRANSFER('Coil of rope', OBJ(9))
      OBJ(10) = TRANSFER('Writing paper', OBJ(10))
      OBJ(11) = TRANSFER('Quill pen', OBJ(11))
      OBJ(12) = TRANSFER('Ivory sword', OBJ(12))
      OBJ(13) = TRANSFER('Acetylene torch', OBJ(13))
      OBJ(14) = TRANSFER('Rowboat', OBJ(14))
      OBJ(15) = TRANSFER('Reusable match', OBJ(15))
      OBJ(16) = TRANSFER('Grappling Hook', OBJ(16))
      OBJ(17) = TRANSFER('Gold statue', OBJ(17))
      OBJ(18) = TRANSFER('Empty bottle', OBJ(18))
      OBJ(19) = TRANSFER('Silver cross', OBJ(19))
      OBJ(20) = TRANSFER('Old gun', OBJ(20))
      OBJ(21) = TRANSFER('Brass lantern', OBJ(21))
      OBJ(22) = TRANSFER('Tasty food', OBJ(22))
      OBJ(23) = TRANSFER('Large ruby', OBJ(23))
      OBJ(24) = TRANSFER('Jade figure', OBJ(24))
      OBJ(25) = TRANSFER('Flask of acid', OBJ(25))
      OBJ(26) = TRANSFER('Water in bottle', OBJ(26))
      OBJ(27) = TRANSFER('Cuban cigar', OBJ(27))
      OBJ(28) = TRANSFER('Sapphire', OBJ(28))
      OBJ(29) = TRANSFER('Lots of money', OBJ(29))
      OBJ(30) = TRANSFER('Crystal swan', OBJ(30))
      IF (NUMB .EQ. 0) GOTO 20
      IF (BOTTLE) ITEMS(18) = 0
      IF(NUMB. EQ. 1) WRITE(6,1010)
      IF(NUMB. EQ. 1) GOTO 100
      WRITE(6,1000) NUMB
  100 NUMB = 0
      IF (.NOT. GUN) GOTO 120
           ITEMS(20) = 0
           ITEMS( 2) = 0
           WRITE(6,2000) LOADED
  120 CONTINUE
      DO 10 II=1,NITEMS
         IF (ITEMS(II) .EQ. -1) WRITE(6,2000) OBJ(II)
         IF (ITEMS(II) .EQ. -1) NUMB = NUMB + 1
  10  CONTINUE
      IF (BOTTLE) ITEMS(18) = -1
      IF (.NOT. GUN) GOTO 15
           ITEMS(20) = -1
           ITEMS( 2) = -1
           NUMB = NUMB + 1
   15 CONTINUE
      IF (NUMB .GT. 0) RETURN
  20  CONTINUE
      WRITE(6,3000)
      RETURN
 1000 FORMAT('0  You are carrying the following ',I2,' objects:')
 1010  FORMAT('0  You are carrying the following object:')
 2000 FORMAT(6X,2A8)
 3000 FORMAT('0  You''re not carrying anything.')
      END
