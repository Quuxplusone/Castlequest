C---------------------------------------------
      SUBROUTINE SAVRES(II)
      INTEGER SAVAR(400)
      LOGICAL DEBUG, ISEED
C
      COMMON DEBUG
      COMMON /BLOCK2/ SAVAR
C
      IF (II .EQ. 0) GOTO 158
C     ---SUSPEND---
C     CALL CREATE('SAVEDQUEST ',1,0,256)
C     CALL PERMIT('SAVEDQUEST ',7,6,11,'AWCC:CASTLE',0)
C     CALL FTNCMD('ASSIGN 8=SAVEDQUEST;')
      CALL CMS('FI      ','13      ','DISK    ','QUEST   ','CQDATA  ',
     *'(       ','LRECL   ','80      ','RECFM   ','F       ')
      REWIND 13
      L = 1
      DO 3001 II=1,40
           K = L + 9
           WRITE(13,3002)(SAVAR(KK),KK=L,K)
           L = L + 10
 3001 CONTINUE
C     CALL FTNCMD('RELEASE 8;')
      GOTO 9000
  158 CONTINUE
C     ---RESTORE---
      IF (SAVAR(90) .GT. 1) GOTO 740
C     CALL FTNCMD('ASSIGN 8=SAVEDQUEST;')
      CALL CMS('FI      ','13      ','DISK    ','QUEST   ','CQDATA  ',
     *'(       ','LRECL   ','80      ','RECFM   ','F       ')
      REWIND 13
      L = 1
      DO 3003 II=1,40
           K = L + 9
           READ(13,3002)(SAVAR(KK),KK=L,K)
           L = L + 10
 3003 CONTINUE
C     CALL FTNCMD('RELEASE 8;')
      GOTO 9000
 3002 FORMAT(10Z8)
  740 WRITE(6,3005)
 3005 FORMAT('0  I can''t do that at this point in time.')
 9000 RETURN
      END
