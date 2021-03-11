C---------------------------------------------------------------------
      SUBROUTINE FREAD(IVERB,INOUN)
C
      CHARACTER*12 VERB, NOUN, BLANKS
      CHARACTER*20 LINE, HUH
      CHARACTER* 1 BLANK
      CHARACTER* 4 N, V
      INTEGER INOUN(3), IVERB(3), JNOUN(3), JVERB(3)
C
      EQUIVALENCE (NOUN, JNOUN), (VERB, JVERB)
C
      DATA  BLANK    /' '/,  BLANKS /'            '/
      DATA  HUH      /'I didn''t get that!! '/
C
      NOUN = BLANKS
      VERB = BLANKS
    5 READ(5,1001,ERR=30,END=30) LINE
 1001 FORMAT( A20 )
C
      DO 10 I=1,20
           ISAVE = I
           IF (LINE(I:I) .NE. BLANK) GOTO 11
   10 CONTINUE
      GOTO 30
C
   11 J     = 1
      ILAST = ISAVE + 11
      DO 12 II=ISAVE,ILAST
           IF (LINE(II:II) .NE. BLANK) THEN
                VERB(J:J) = LINE(II:II)
                J         = J + 1
           ELSE
               ISAVE = II
                GOTO 13
                ENDIF
   12 CONTINUE
      ISAVE = ILAST + 1
   13 CONTINUE
      DO 14 I=ISAVE,20
           IF (LINE(I:I) .NE. BLANK) GOTO 20
   14 CONTINUE
      GOTO 100
C
   20 CONTINUE
      J     = 1
      ILAST = I + 11
      DO 22 II=I,ILAST
           IF (LINE(II:II) .NE. BLANK) THEN
                NOUN(J:J) = LINE(II:II)
                J         = J + 1
           ELSE
                GOTO 23
           ENDIF
   22 CONTINUE
   23 GOTO 100
C
   30 CONTINUE
      WRITE(6,1002) HUH
      GOTO 5
 1002 FORMAT('0  ',A20,/,' ')
C
  100 CONTINUE
      DO 110 KK=1,3
           INOUN(KK) = JNOUN(KK)
           IVERB(KK) = JVERB(KK)
  110 CONTINUE
      RETURN
      END
