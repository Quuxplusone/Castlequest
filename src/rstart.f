C---------------------------------------------------------------
C
      SUBROUTINE RSTART(ISEED)
      INTEGER   S1(2)
      INTEGER*2 S2(4), I1(2), I2(2)
      EQUIVALENCE (S1(1), S2(1)), (INT1, I1(1)), (INT2,I2(1))
C
      CALL TOD(S1)
      I1(2) = S2(1)
      I2(2) = S2(4)
      ISEED = INT1 + INT2
      CALL RDMIN(ISEED)
      RETURN
      END
