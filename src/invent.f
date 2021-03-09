C________________________________________________
      SUBROUTINE INVENT(NITEMS)
C
C
      INTEGER NUMB,ITEMS(30),NITEMS
      COMPLEX*16 OBJ(30) /'Kerosene','Silver bullet','Bloody hatchet',
     2'Skeleton key','Blood in bottle','Wooden stake','Champagne',
     3'Hunchback','Coil of rope','Writing paper','Quill pen',
     4'Ivory sword','Acetylene torch','Rowboat','Reusable match',
     5'Grappling Hook','Gold statue','Empty bottle','Silver cross',
     6'Old gun','Brass lantern','Tasty food','Large ruby','Jade figure',
     7'Flask of acid','Water in bottle','Cuban cigar','Sapphire',
     8'Lots of money','Crystal swan' /
C
      COMPLEX*16 LOADED /'Bullet in gun'/
      LOGICAL BLOOD, BOTTLE, WATER, GUN
      INTEGER SAVAR(400)
C
      COMMON /BLOCK2/ SAVAR
C
      EQUIVALENCE (SAVAR(91), BLOOD),(SAVAR(92), BOTTLE),
     2            (SAVAR(93), WATER),(SAVAR( 1), ITEMS(1)),
     3            (SAVAR(95), NUMB ),(SAVAR(63), GUN  )
C
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
