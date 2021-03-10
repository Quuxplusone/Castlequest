C------------------------------------------------------
      SUBROUTINE INPUT(ACTION)
      IMPLICIT INTEGER (A - Z)
      INTEGER ACTION(2), VERBS(2,80), NOUNS(2,76),
     2        BLANK, N, V, NVERBS /80/,
     3        NNOUNS /76/
C
      INTEGER NOUN(3), VERB(3)
C
      EQUIVALENCE (VERB(1), V), (NOUN(1), N)
C
      DATA VERBS  /4hATTA, 15, 4hBACK, 40, 4hBREA, 37, 4hBRIE, 61,
     2             4hCHOP, 37, 4hCLIM,  9, 4hCLOS, 28, 4hCROS, 43,
     3             4hD   , 10, 4hDEBU, 59, 4hDOWN, 10, 4hDRIN, 24,
     4             4hDROP, 12, 4hE   ,  3, 4hEAST,  3, 4hEAT , 23,
     5             4hENTE, 13, 4hEXIT, 14, 4hEXTI, 34, 4hFEED, 22,
     6             4hFILL, 49, 4hFIRE, 54, 4hFUCK, 19, 4hGOTO, 51,
     7             4hHELP, 50, 4hHINT, 50, 4hHONK, 45, 4hIN  , 13,
     8             4hINVE, 26, 4hJUMP, 25, 4hKILL, 16, 4hL   , 52,
     9             4hLEAV, 14, 4hLEFT, 52, 4hLIGH, 32, 4hLOAD, 18,
     X             4hLOCK, 29, 4hLONG, 60, 4hLOOK, 35, 4hMELT, 42,
     1             4hN   ,  1, 4hNE  ,  2, 4hNORT,  1, 4hNW  ,  8,
     2             4hOFF , 33, 4hON  , 31, 4hOPEN, 27, 4hOUT , 14,
     3             4hPOOF, 56, 4hPOUR, 39, 4hQUIT, 44, 4hR   , 53,
     4             4hREAD, 48, 4hREST, 58, 4hRIGH, 53, 4hS   ,  5,
     5             4hSAVE, 57, 4hSCOR, 36, 4hSE  ,  4, 4hSHOO, 54,
     6             4hSHOW, 20, 4hSOUT,  5, 4hSTAB, 21, 4hSUSP, 57,
     7             4hSW  ,  6, 4hSWIM, 41, 4hT   , 11, 4hTAKE, 11,
     8             4hTHRO, 17, 4hTIE , 46, 4hU   ,  9, 4hUNLO, 30,
     9             4hUNTI, 47, 4hUP  ,  9, 4hVERB, 60, 4hW   ,  7,
     X             4hWAKE, 55, 4hWATE, 38, 4hWAVE, 20, 4hWEST,  7/
C
      DATA NOUNS  /4hACID, 25, 4hALL , 31, 4hAXE ,  3, 4hBARS, 54,
     2             4hBAT , 55, 4hBLOO,  5, 4hBOAR, 56, 4hBOAT, 14,
     3             4hBOOK, 46, 4hBOTT, 18, 4hBULL,  2, 4hBUTL, 49,
     4             4hCASK, 40, 4hCHAM,  7, 4hCIGA, 27, 4hCOFF, 40,
     5             4hCOMP, 57, 4hCOUN, 39, 4hCROS, 19, 4hCRYS, 30,
     6             4hCYCL, 78, 4hDOOR, 47, 4hDRAW, 48, 4hFIGU, 24,
     7             4hFIRE, 34, 4hFLAS, 25, 4hFOOD, 22, 4hGLAC, 79,
     8             4hGNOM, 77, 4hGRAP, 16, 4hGUN , 20, 4hHATC,  3,
     9             4hHOOK, 16, 4hHUNC,  8, 4hICE , 79, 4hIVOR, 12,
     X             4hJADE, 24, 4hKERO,  1, 4hKEY ,  4, 4hLAMP, 21,
     1             4hLANT, 21, 4hLIGH, 21, 4hLOCK, 33, 4hMAST, 39,
     2             4hMATC, 15, 4hMIRR, 50, 4hMOAT, 51, 4hMONE, 29,
     3             4hNOTE, 52, 4hOIL ,  1, 4hPAPE, 10, 4hPEN , 11,
     4             4hPIST, 20, 4hQUIL, 11, 4hROOM, 45, 4hROPE,  9,
     5             4hROWB, 14, 4hRUBY, 23, 4hSAPP, 28, 4hSHUT, 38,
     6             4hSTAK,  6, 4hSTAT, 17, 4hSWAN, 30, 4hSWOR, 12,
     7             4hTORC, 13, 4hTUNN, 41, 4hVAMP, 39, 4hVLAD, 39,
     8             4hWATE, 26, 4hWERE, 76, 4hWIND, 37, 4hWIZA, 80,
     9             4hWOLF, 76, 4h31  , 43, 4h59  , 44, 4h8   , 42/
C
      BLANK = TRANSFER('    ', BLANK)
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
