C---------------------------------------------------------------
C
      SUBROUTINE RSTART(ISEED)
      RETURN
      END
C
      REAL FUNCTION RDM(SEED)
      REAL R
      CALL RANDOM_NUMBER(R)
      RDM = R
      RETURN
      END
