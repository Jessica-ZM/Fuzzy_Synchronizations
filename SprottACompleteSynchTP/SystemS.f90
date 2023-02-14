!******************************************
      !Subroutine SprottSystem: contiene el sistema de edo's del sistema de LE4
      subroutine SystemS(nuecdf,T, Y, DY, U, mv, vp, fm1, fm2)

     !*****************************************************************


      implicit none

      external SPCOEF  !para los splines
      external SVALUE  !para los splines
      external dgemv ! para multiplicar matrices en doble precision

      integer(kind = 4) :: nuecdf !numero de edos

      double precision :: a, bpar, d
      double precision :: A1(3, 3), A2(3, 3)
      double precision :: yD(3), yR(3), U1(3), U2(3), dyD(3), dyR(3)
      double precision :: aux1(3), aux2(3), aux3(3), aux4(3)
      double precision :: aux5(3), aux6(3), aux7(3), aux8(3), dyR1(3), dyR2(3)
      double precision :: par1(3), par2(3), par3(3), par4(3), aux9(3)
      double precision :: prod1(3), prod2(3), prod3(3), prod4(3)
      double precision :: u11(3), u12(3), u13(3), u14(3)
      double precision :: u21(3), u22(3), u23(3), u24(3)
      double precision :: M1D, M2D, M1R, M2R, F1(3,3), F2(3,3), B(3,3), b1(3), U(3)
      double precision :: vp(37), fm1(37), fm2(37)
      integer(kind = 4) :: mv
      DOUBLE PRECISION :: BV(37),CV(37),DV(37)
      integer :: flag, interv
      ! para la multiplicacion de matrices
      integer(kind = 4) :: mdim, ndim, LDA, INCX, INCY
      DOUBLE PRECISION :: ALPHA, BETA
      CHARACTER TRANS


      ! Coeficientes del sistema de LE4
      double precision :: T, Y(nuecdf), DY(nuecdf)
      integer(kind = 4) :: contadorEVal
      common/contadorEval/contadorEval

      ! Actualizamos el contador
      contadorEval = contadorEval + 1
      !print *, "contador"
      !print *, contadorEVal


      !para la multiplicacion de matrices
      ndim = 3
      mdim = 3
      LDA = 3
      INCX = 1
      INCY = 1
      ALPHA = 1.0d0
      BETA = 0.0d0
      TRANS = 'N'

      !Valor de los coeficientes del sistema de LE4
      ! no tiene

	  ! Parameter fuzzy model
	  d = 5.0d0

! Matrices originales
!	  A1(1,1) =  0.0d0
!	  A1(1,2) =  1.0d0
!	  A1(1,3) =  0.0d0
!	  A1(2,1) =  -1.0d0
!	  A1(2,2) =  0.0d0
!	  A1(2,3) =  d
!	  A1(3,1) =  0.0d0
!	  A1(3,2) =  -d
!	  A1(3,3) =  0.0d0
!
!
!	  A2(1,1) =  0.0d0
!	  A2(1,2) =  1.0d0
!	  A2(1,3) =  0.0d0
!	  A2(2,1) =  -1.0d0
!	  A2(2,2) =  0.0d0
!	  A2(2,3) =  -d
!	  A2(3,1) =  0.0d0
!	  A2(3,2) =  d
!	  A2(3,3) =  0.0d0

! Matrices SNNN
	  A1(1,1) =  0.0d0
	  A1(1,2) =  1.0d0
	  A1(1,3) =  0.0d0
	  A1(2,1) =  -1.0d0
	  A1(2,2) =  0.0d0
	  A1(2,3) =  23.0405920901439
	  A1(3,1) =  0.0d0
	  A1(3,2) =  -23.0405920901439
	  A1(3,3) =  0.0d0


	  A2(1,1) =  0.0d0
	  A2(1,2) =  1.0d0
	  A2(1,3) =  0.0d0
	  A2(2,1) =  -1.0d0
	  A2(2,2) =  0.0d0
	  A2(2,3) =  -d
	  A2(3,1) =  0.0d0
	  A2(3,2) =  d
	  A2(3,3) =  0.0d0

!! Matrices CNO
!	  A1(1,1) =  0.0d0
!	  A1(1,2) =  1.0d0
!	  A1(1,3) =  0.0d0
!	  A1(2,1) =  -1.0d0
!	  A1(2,2) =  0.0d0
!	  A1(2,3) =  -d
!	  A1(3,1) =  0.0d0
!	  A1(3,2) =  d
!	  A1(3,3) =  0.0d0
!
!
!	  A2(1,1) =  0.0d0
!	  A2(1,2) =  1.0d0
!	  A2(1,3) =  0.0d0
!	  A2(2,1) =  -1.0d0
!	  A2(2,2) =  0.0d0
!	  A2(2,3) =  d
!	  A2(3,1) =  0.0d0
!	  A2(3,2) =  -d
!	  A2(3,3) =  0.0d0


	  !write(*,"(3F7.3)") A1
	  !write(*,"(3F7.3)") A2

      ! vector constante
      b1(1) = 0.0d0
      b1(2) = 0.0d0
      b1(3) = 1.0d0


      yD(:) = Y(1:3)
      yR(:) = Y(4:6)
      !write(*,"(3F17.3)") yD
      !write(*,"(3F17.3)") yR

!	  !funciones difusas drive
!      M1D = (0.5)*(1+((Y(2))/d))
!      M2D = (0.5)*(1-((Y(2))/d))
!      !write(*,"(F7.3)") M1D
!	  !write(*,"(F7.3)") M2D
!
!      !funciones difusas drive
!      M1R = (0.5)*(1+((Y(5))/d))
!      M2R = (0.5)*(1-((Y(5))/d))

      !calculamos los valores de las funciones de membresia con los splines
      !Calculate coefficients defining the cubic spline.
      CALL SPCOEF(mv,vp,fm1,BV,CV,DV,FLAG)
      !SUBROUTINE SPCOEF(N,X,F,B,C,D,FLAG)
      !print *, "Salimos spcoef"
      !print *, flag
      !Maestro 1a funcion
      !print *, "y(2)",y(2)
      call SVALUE(mv,vp,fm1,BV,CV,DV,Y(2),INTERV,M1D,FLAG)
      !SUBROUTINE SVALUE(N,X,F,B,C,D,T,INTERV,S,FLAG)
      !print *, "Salimos svalue"
      PRINT *,' T =',Y(2),'  M1D =',M1D,'  FLAG =',FLAG
      !Esclavo 1a funcion
      call SVALUE(mv,vp,fm1,BV,CV,DV,Y(5),INTERV,M1R,FLAG)
      PRINT *,' T =',Y(5),'  M1R =',M1R,'  FLAG =',FLAG
      CALL SPCOEF(mv,vp,fm2,BV,CV,DV,FLAG)
      !Maestro 2a funcion
      call SVALUE(mv,vp,fm2,BV,CV,DV,Y(2),INTERV,M2D,FLAG)
      !Esclavo 2a funcion
      call SVALUE(mv,vp,fm2,BV,CV,DV,Y(5),INTERV,M2R,FLAG)
!      write(*,*) M1D
!	  write(*,*) M2D
       !print *, "Salimos spline"


!!Matrices de ganancias 1
!      F1(1,1) = 5.0000000e-01
!      F1(1,2) = 5.1603261e-16
!      F1(1,3) = -1.8400000e+00
!      F1(2,1) = -5.7491588e-17
!      F1(2,2) = 5.0000000e-01
!      F1(2,3) = 4.0000000e-01
!      F1(3,1) = -1.8400000e+00
!      F1(3,2) = 4.0000000e-01
!      F1(3,3) = -3.0000000e-01
!
!      F2(1,1) = 5.0000000e-01
!      F2(1,2) = -6.9845170e-16
!      F2(1,3) = 1.8400000e+00
!      F2(2,1) = -1.2492750e-16
!      F2(2,2) = 5.0000000e-01
!      F2(2,3) = -4.0000000e-01
!      F2(3,1) = 1.8400000e+00
!      F2(3,2) = -4.0000000e-01
!      F2(3,3) = 1.3000000e+00

!Matrices de ganancias SNNN
!      F1(1,1) = 5.0000000e-01
!      F1(1,2) = -4.5462988e-17
!      F1(1,3) = 2.0598364e-16
!      F1(2,1) = -4.5462988e-17
!      F1(2,2) = 5.0000000e-01
!      F1(2,3) = 8.7190629e-16
!      F1(3,1) = 2.0598364e-16
!      F1(3,2) = 8.7190629e-16
!      F1(3,3) = 5.0000000e-01
!
!      F2(1,1) = 5.0000000e-01
!      F2(1,2) = -3.8874386e-17
!      F2(1,3) = -2.5342769e-18
!      F2(2,1) = -3.8874386e-17
!      F2(2,2) = 5.0000000e-01
!      F2(2,3) = 5.8932157e-17
!      F2(3,1) = -2.5342769e-18
!      F2(3,2) = 5.8932157e-17
!      F2(3,3) = 5.0000000e-01

!!Matrices de ganancias CNO
      F1(1,1) = 5.0000000d-01
      F1(1,2) = -1.2394558d-17
      F1(1,3) = 1.0890905d-16
      F1(2,1) = -1.2394558d-17
      F1(2,2) = 5.0000000d-01
      F1(2,3) = 1.2933841d-17
      F1(3,1) = 1.0890905d-16
      F1(3,2) = 1.2933841d-17
      F1(3,3) = 5.0000000d-01

      F2(1,1) = 5.0000000d-01
      F2(1,2) = -6.6085452d-18
      F2(1,3) = 1.0923751d-20
      F2(2,1) = -6.6085452d-18
      F2(2,2) = 5.0000000d-01
      F2(2,3) = -1.4091066d-17
      F2(3,1) = 1.0923751d-20
      F2(3,2) = -1.4091066d-17
      F2(3,3) = 5.0000000d-01


      !Matriz identidad
      B(1,1) = 1.0d0
      B(1,2) = 0.0d0
      B(1,3) = 0.0d0
      B(2,1) = 0.0d0
      B(2,2) = 1.0d0
      B(2,3) = 0.0d0
      B(3,1) = 0.0d0
      B(3,2) = 0.0d0
      B(3,3) = 1.0d0

      call dgemv(TRANS,mdim,ndim,ALPHA,A1,LDA,yD,INCX,BETA,aux1,INCY)

      !aux1 = matmul(A1, yD)
      !write(*,"(3F17.3)") aux1
      aux1 = aux1 + b1
      aux2 = M1D*aux1
      !write(*,"(3F17.3)") aux2

      call dgemv(TRANS,mdim,ndim,ALPHA,A2,LDA,yD,INCX,BETA,aux3,INCY)
      !aux3 = matmul(A2, yD)
      !write(*,"(3F17.3)") aux3
      aux3 = aux3 + b1
      aux4 = M2D*aux3

      dyD = aux2 + aux4
      !dyD = M1D*(A1*yD+b1)+M2D*(A2*yD+b2)
      !write(*,"(3F17.3)") dyD

      call dgemv(TRANS,mdim,ndim,ALPHA,F1,LDA,yR,INCX,BETA,prod1,INCY)
      !prod1 = matmul(F1,yR)
      !print *, prod1
      par1 = prod1 + b1
      u11 = -M1R*par1

      call dgemv(TRANS,mdim,ndim,ALPHA,F1,LDA,yD,INCX,BETA,prod2,INCY)
      !prod2 = matmul(F1,yD)
      par2 = prod2 + b1
      u12 = M1D*par2

      U1 = u11 + u12
      !U1 = -M1R*(F1*yR+b1) + M1D*(F1*yD+b1)


      call dgemv(TRANS,mdim,ndim,ALPHA,F2,LDA,yR,INCX,BETA,prod3,INCY)
      !prod3 = matmul(F2,yR)
      par3 = prod3+b1
      u21 = -M2R*par3

      call dgemv(TRANS,mdim,ndim,ALPHA,F2,LDA,yD,INCX,BETA,prod4,INCY)
      !prod4 = matmul(F2,yD)
      par4 = prod4 + b1
      u22 = M2D*par4

      U2 = u21 + u22
      !U2 = -M2R*(F2*yR+b2) + M2D*(F2*yD+b2)

      U(:) = U1 + U2


      call dgemv(TRANS,mdim,ndim,ALPHA,A1,LDA,yR,INCX,BETA,aux5,INCY)
      !aux5 = matmul(A1,yR)
      aux6 = aux5 + b1
      dyR1 = M1R*aux6

      call dgemv(TRANS,mdim,ndim,ALPHA,A2,LDA,yR,INCX,BETA,aux7,INCY)
      !aux7 = matmul(A2,yR)
      aux8 = aux7 + b1
      dyR2 = M2R*aux8

      call dgemv(TRANS,mdim,ndim,ALPHA,B,LDA,U,INCX,BETA,aux9,INCY)
      !aux9 = matmul(B,U)

      dyR = dyR1 + dyR2 + aux9
      !M1R*(A1*yR+b1)+M2R*(A2*yR+b2)+ B*U

      DY(1:3) = dyD
      DY(4:6) = dyR



      return
      end
