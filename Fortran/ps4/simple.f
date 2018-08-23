      PROGRAM SIMPLE
      INTEGER I, IER, PGBEG
      REAL XR(100), YR(100)
      REAL XS(5), YS(5)
      DATA XS/1.,2.,3.,4.,5./
      DATA YS/1.,4.,9.,16.,25./
      IER = PGBEG(0,'?',1,1)
      IF (IER.NE.1) STOP
      CALL PGENV(0.,10.,0.,20.,0,1)
      CALL PGLAB('(x)', '(y)', 'A Simple Graph')
      CALL PGPT(5,XS,YS,9)
      DO 10 I=1,60
          XR(I) = 0.1*I
          YR(I) = XR(I)**2
   10 CONTINUE
      CALL PGLINE(60,XR,YR)
      CALL PGEND
      END
