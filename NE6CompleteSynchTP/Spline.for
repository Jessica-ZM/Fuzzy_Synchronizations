************************************************************************
*   FUNCTION:     Routines for setting up and evaluating a cubic       *
*                 interpolatory spline.                                *
*   AUTHORS:      Lawrence Shampine, Richard Allen, Steven Pruess  for *
*                 the text  Fundamentals of Numerical Computing        *
*   LAST CHANGE:  July 13, 1988                                        *
*   REVISION:     December 19, 1995                                    *
************************************************************************
      SUBROUTINE SPCOEF(N,X,F,B,C,D,FLAG)
C
C  Calculate coefficients defining a smooth cubic interpolatory spline.
C
      INTEGER FLAG,N
      DOUBLE PRECISION B(*),C(*),D(*),F(*),X(*)
C
C  Input parameters:
C    N    = number of data points.
C    X    = vector of values of the independent variable ordered
C           so that  X(I) < X(I+1)  for all I.
C    F    = vector of values of the dependent variable.
C  Output parameters:
C    B    = vector of S'(X(I)) values.
C    C    = vector of S"(X(I))/2 values.
C    D    = vector of S'''(X(I)+)/6 values (I .LT. N).
C    FLAG =  0  normal return;
C         = -1  input N .LE. 1;
C         = -2  X vector is incorrectly ordered.
C
C  The vectors X, F, B, C, D must be dimensioned at least N in the
C  calling program.
C
C  Local variables:
      INTEGER I,K
      DOUBLE PRECISION FP1,FPN,H,P,THREE,TWO,ZERO
      DATA THREE,TWO,ZERO/3.D0,2.D0,0.D0/
C
      IF (N .LE. 1) THEN
         FLAG = -1
         RETURN
      ENDIF
C
C     Calculate coefficients for the tri-diagonal system: store
C     sub-diagonal in B, diagonal in D, difference quotient in C.
C
      B(1) = X(2)-X(1)
      IF (B(1) .LE. ZERO) THEN
         FLAG = -2
         RETURN
      ENDIF
      C(1) = (F(2)-F(1))/B(1)
      IF (N .EQ. 2) THEN
         B(1) = C(1)
         C(1) = ZERO
         D(1) = ZERO
         B(2) = B(1)
         C(2) = ZERO
         FLAG = 0
         RETURN
      ENDIF
      D(1) = TWO*B(1)
      DO 20 I = 2,N-1
         B(I) = X(I+1)-X(I)
         IF (B(I) .LE. ZERO) THEN
            FLAG = -2
            RETURN
         ENDIF
         C(I) = (F(I+1)-F(I))/B(I)
         D(I) = TWO*(B(I)+B(I-1))
   20 CONTINUE
      D(N) = TWO*B(N-1)
C
C     Calculate estimates for the end slopes.  Use polynomials
C     interpolating data nearest the end.
C
      FP1 = C(1)-B(1)*(C(2)-C(1))/(B(1)+B(2))
      IF (N .GT. 3) FP1 = FP1+B(1)*((B(1)+B(2))*(C(3)-C(2))
     &  /(B(2)+B(3))-C(2)+C(1))/(X(4)-X(1))
      FPN = C(N-1)+B(N-1)*(C(N-1)-C(N-2))/(B(N-2)+B(N-1))
      IF (N .GT. 3) FPN = FPN+B(N-1)*(C(N-1)-C(N-2)-(B(N-2)
     &  +B(N-1))*(C(N-2)-C(N-3))/(B(N-2)+B(N-3)))/(X(N)-X(N-3))
C
C     Calculate the right-hand-side and store it in C.
C
      C(N) = THREE*(FPN-C(N-1))
      DO 30 K = 2,N-1
         I = N-K+1
         C(I) = THREE*(C(I)-C(I-1))
   30 CONTINUE
      C(1) = THREE*(C(1)-FP1)
C
C     Solve the tridiagonal system.
C
      DO 40 K = 2,N
         P = B(K-1)/D(K-1)
         D(K) = D(K)-P*B(K-1)
         C(K) = C(K)-P*C(K-1)
   40 CONTINUE
      C(N) = C(N)/D(N)
      DO 50 K = N-1,1,-1
         C(K) = (C(K)-B(K)*C(K+1))/D(K)
   50 CONTINUE
C
C     Calculate the coefficients defining the spline.
C
      DO 60 I = 1,N-1
         H = X(I+1)-X(I)
         D(I) = (C(I+1)-C(I))/(THREE*H)
         B(I) = (F(I+1)-F(I))/H-H*(C(I)+H*D(I))
   60 CONTINUE
      B(N) = B(N-1)+H*(TWO*C(N-1)+H*THREE*D(N-1))
      FLAG = 0
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SVALUE(N,X,F,B,C,D,T,INTERV,S,FLAG)
      INTEGER N,INTERV,FLAG
      DOUBLE PRECISION X(*),F(*),B(*),C(*),D(*),T,S
C
C  Evaluate the spline S at T using coefficients from SPCOEF.
C
C  Input parameters:
C    N, X, F, B, C, D are defined as in SPCOEF.
C    T      = point where spline is to be evaluated.
C  Output parameters:
C    INTERV = index satisfying  X(INTERV) .LE. T .LT. X(INTERV+1)
C             unless T is outside data interval (see FLAG).
C    S      = value of spline at T.
C    FLAG   =  0  normal return;
C           = -1  N .LE. 1;
C           =  1  T .LT. X(1);
C           =  2  T .GT. X(N).
C
C  Local variables:
      INTEGER J,LSTINT
      DOUBLE PRECISION DT
      SAVE LSTINT
      DATA LSTINT/1/
C
      IF (N .LE. 1) THEN
         FLAG = -1
         RETURN
      ENDIF
      FLAG = 0
C
C     Search for correct interval for T.
C
      IF (T .LT. X(1)) THEN
         INTERV = 1
         FLAG = 1
      ENDIF
      IF (T .GT. X(N)) THEN
         INTERV = N-1
         FLAG = 2
      ENDIF
      IF (FLAG .EQ. 0) THEN
         IF (T .GE. X(LSTINT)) THEN
            DO 10 J = LSTINT,N-1
               IF (T .LE. X(J+1)) THEN !cambio de .LT. a .LE.
                  INTERV = J
                  GOTO 30
               ENDIF
   10       CONTINUE
         ELSE
            DO 20 J = LSTINT-1,1,-1
               IF (T .GE. X(J)) THEN
                  INTERV = J
                  GOTO 30
               ENDIF
   20       CONTINUE
         ENDIF
      ENDIF
   30 LSTINT = INTERV
C
C     Evaluate cubic polynomial on [X(INTERV) , X(INTERV+1)].
C
      DT = T-X(INTERV)
      S = F(INTERV)+DT*(B(INTERV)+DT*(C(INTERV)+DT*D(INTERV)))
      RETURN
      END
C=======================================================================

