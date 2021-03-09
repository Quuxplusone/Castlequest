C------------------------------------------------------
      SUBROUTINE INPUT(ACTION)
      IMPLICIT INTEGER (A - Z)
      INTEGER ACTION(2), VERBS(2,80), NOUNS(2,76),
     2        BLANK /' '/, N, V, NVERBS /80/,
     3        NNOUNS /76/
C
      INTEGER NOUN(3), VERB(3)
C
      EQUIVALENCE (VERB(1), V), (NOUN(1), N)
C
      DATA VERBS  /'ATTA', 15, 'BACK', 40, 'BREA', 37, 'BRIE', 61,
     2             'CHOP', 37, 'CLIM',  9, 'CLOS', 28, 'CROS', 43,
     3             'D   ', 10, 'DEBU', 59, 'DOWN', 10, 'DRIN', 24,
     4             'DROP', 12, 'E   ',  3, 'EAST',  3, 'EAT ', 23,
     5             'ENTE', 13, 'EXIT', 14, 'EXTI', 34, 'FEED', 22,
     6             'FILL', 49, 'FIRE', 54, 'FUCK', 19, 'GOTO', 51,
     7             'HELP', 50, 'HINT', 50, 'HONK', 45, 'IN  ', 13,
     8             'INVE', 26, 'JUMP', 25, 'KILL', 16, 'L   ', 52,
     9             'LEAV', 14, 'LEFT', 52, 'LIGH', 32, 'LOAD', 18,
     X             'LOCK', 29, 'LONG', 60, 'LOOK', 35, 'MELT', 42,
     1             'N   ',  1, 'NE  ',  2, 'NORT',  1, 'NW  ',  8,
     2             'OFF ', 33, 'ON  ', 31, 'OPEN', 27, 'OUT ', 14,
     3             'POOF', 56, 'POUR', 39, 'QUIT', 44, 'R   ', 53,
     4             'READ', 48, 'REST', 58, 'RIGH', 53, 'S   ',  5,
     5             'SAVE', 57, 'SCOR', 36, 'SE  ',  4, 'SHOO', 54,
     6             'SHOW', 20, 'SOUT',  5, 'STAB', 21, 'SUSP', 57,
     7             'SW  ',  6, 'SWIM', 41, 'T   ', 11, 'TAKE', 11,
     8             'THRO', 17, 'TIE ', 46, 'U   ',  9, 'UNLO', 30,
     9             'UNTI', 47, 'UP  ',  9, 'VERB', 60, 'W   ',  7,
     X             'WAKE', 55, 'WATE', 38, 'WAVE', 20, 'WEST',  7/
C
      DATA NOUNS  /'ACID', 25, 'ALL ', 31, 'AXE ',  3, 'BARS', 54,
     2             'BAT ', 55, 'BLOO',  5, 'BOAR', 56, 'BOAT', 14,
     3             'BOOK', 46, 'BOTT', 18, 'BULL',  2, 'BUTL', 49,
     4             'CASK', 40, 'CHAM',  7, 'CIGA', 27, 'COFF', 40,
     5             'COMP', 57, 'COUN', 39, 'CROS', 19, 'CRYS', 30,
     6             'CYCL', 78, 'DOOR', 47, 'DRAW', 48, 'FIGU', 24,
     7             'FIRE', 34, 'FLAS', 25, 'FOOD', 22, 'GLAC', 79,
     8             'GNOM', 77, 'GRAP', 16, 'GUN ', 20, 'HATC',  3,
     9             'HOOK', 16, 'HUNC',  8, 'ICE ', 79, 'IVOR', 12,
     X             'JADE', 24, 'KERO',  1, 'KEY ',  4, 'LAMP', 21,
     1             'LANT', 21, 'LIGH', 21, 'LOCK', 33, 'MAST', 39,
     2             'MATC', 15, 'MIRR', 50, 'MOAT', 51, 'MONE', 29,
     3             'NOTE', 52, 'OIL ',  1, 'PAPE', 10, 'PEN ', 11,
     4             'PIST', 20, 'QUIL', 11, 'ROOM', 45, 'ROPE',  9,
     5             'ROWB', 14, 'RUBY', 23, 'SAPP', 28, 'SHUT', 38,
     6             'STAK',  6, 'STAT', 17, 'SWAN', 30, 'SWOR', 12,
     7             'TORC', 13, 'TUNN', 41, 'VAMP', 39, 'VLAD', 39,
     8             'WATE', 26, 'WERE', 76, 'WIND', 37, 'WIZA', 80,
     9             'WOLF', 76, '31  ', 43, '59  ', 44, '8   ', 42/
C
      ACTION(1) = 0
      ACTION(2) = 0
    3 WRITE(6,2004)
      CALL FREAD(VERB,NOUN)
C     CALL MOVEC(4,VERB,V,21)
C     CALL MOVEC(4,NOUN,N,21)
      LOW=1
      HIGH=NVERBS
  18  IF(HIGH.LT.LOW) GOTO 21
      I=(LOW+HIGH)/2
      IF(V-VERBS(1,I)) 19,22,20
  19  HIGH=I-1
      GOTO 18
  20  LOW=I+1
      GOTO 18
   21 CONTINUE
      IF (N .NE. BLANK) WRITE(6,2002) NOUN
      IF (N .EQ. BLANK) WRITE(6,2003)
      GOTO 3
C
C     LOOK FOR A NOUN....
C
   22 CONTINUE
      ACTION(1)=VERBS(2,I)
      IF (N .EQ. BLANK) GOTO 45
      LOW=1
      HIGH=NNOUNS
  26  IF(HIGH.LT.LOW) GOTO 30
      I=(LOW+HIGH)/2
      IF(N-NOUNS(1,I)) 27,40,28
  27  HIGH=I-1
      GOTO 26
  28  LOW=I+1
      GOTO 26
  30  CONTINUE
      IF (ACTION(1) .EQ. 19) GOTO 40
   31 CONTINUE
      WRITE(6,2001) VERB
      GOTO 3
   40 CONTINUE
      ACTION(2)=NOUNS(2,I)
   45 IF(ACTION(1).EQ.10 .AND. ACTION(2).NE.0) ACTION(1)=12
 2001 FORMAT('0  ', 2A4,A2, 'what???')
 2002 FORMAT('0  Do WHAT with the ',2A4,A2, '??')
 2003 FORMAT('0  I don''t think I understand.')
 2004 FORMAT(' ')
      RETURN
      END
