!******************************************
      !Subroutine ChuaSystem: contiene el sistema de edo's del sistema de Chua
      subroutine ChuaSystem (nuecdf,T, Y, DY, U)
     !*****************************************************************


      implicit none
	  integer(kind = 4) :: nuecdf !numero de edos

      double precision :: alpha, beta, gama, d
      double precision :: A1(3, 3), A2(3, 3)
      double precision :: yD(3), yR(3), U1(3), U2(3), dyD(3), dyR(3)
      double precision :: aux1(3), aux2(3), aux3(3), aux4(3)
      double precision :: aux5(3), aux6(3), aux7(3), aux8(3), dyR1(3), dyR2(3)
      double precision :: par1(3), par2(3), par3(3), par4(3), aux9(3)
      double precision :: prod1(3), prod2(3), prod3(3), prod4(3)
      double precision :: u11(3), u12(3), u13(3), u14(3)
      double precision :: u21(3), u22(3), u23(3), u24(3)
      double precision :: M1D, M2D, M1R, M2R, F1(3,3), F2(3,3), B(3,3), b1(3), phiG, U(3)

      ! Coeficientes del sistema de Chua
      double precision :: T, Y(nuecdf), DY(nuecdf), G
      integer :: contadorEVal
      common/contadorEval/contadorEval

      ! Actualizamos el contador
      contadorEval = contadorEval + 1

      !Valor de los coeficientes del sistema de Chua
      !Classical
      !alpha = 9.3516
      !beta = 14.7903
      !gama = 0.0161

      !hidden
      alpha = 8.4562d0
	  beta = 12.0732d0
	  gama = 0.0052d0

	  ! Parameter fuzzy model
	  d = 1.1

! SNNN Matrix
!	  A1(1,1) = -8.67967906733454
!	  A1(1,2) =  alpha
!	  A1(1,3) =  0.0d0
!	  A1(2,1) =  1.0d0
!	  A1(2,2) =  -1.0d0
!	  A1(2,3) =  1.0d0
!	  A1(3,1) =  0.0d0
!	  A1(3,2) =  -beta
!	  A1(3,3) =  -gama
!
!
!	  A2(1,1) = -6.21546074909089
!	  A2(1,2) =  alpha
!	  A2(1,3) =  0.0d0
!	  A2(2,1) =  1.0d0
!	  A2(2,2) =  -1.0d0
!	  A2(2,3) =  1.0d0
!	  A2(3,1) =  0.0d0
!	  A2(3,2) =  -beta
!	  A2(3,3) =  -gama

! CNO Matrix
	  A1(1,1) =  -6.21546074909089
	  A1(1,2) =  alpha
	  A1(1,3) =  0.0d0
	  A1(2,1) =  1.0d0
	  A1(2,2) =  -1.0d0
	  A1(2,3) =  1.0d0
	  A1(3,1) =  0.0d0
	  A1(3,2) =  -beta
	  A1(3,3) =  -gama


	  A2(1,1) =  -6.96114384000000
	  A2(1,2) =  alpha
	  A2(1,3) =  0.0d0
	  A2(2,1) =  1.0d0
	  A2(2,2) =  -1.0d0
	  A2(2,3) =  1.0d0
	  A2(3,1) =  0.0d0
	  A2(3,2) =  -beta
	  A2(3,3) =  -gama



	  !write(*,"(3F7.3)") A1
	  !write(*,"(3F7.3)") A2

	  ! Las matrices estan bien declaradas falta depurar de aqui en adelante
	  yD(:) = Y(1:3)
      yR(:) = Y(4:6)
      !write(*,"(3F17.3)") yD
	  !write(*,"(3F17.3)") yR

	  !funciones difusas drive
      M1D = (0.5)*(1-(phiG(Y(1))/d))
      M2D = (0.5)*(1+(phiG(Y(1))/d))
      !write(*,"(F7.3)") phiG(Y(1))
      !write(*,"(F7.3)") M1D
	  !write(*,"(F7.3)") M2D

      !funciones difusas slave
      M1R = (0.5)*(1-(phiG(Y(4))/d))
      M2R = (0.5)*(1+(phiG(Y(4))/d))


!SNNN Model
!      F1(1,1) = -8.17967906733454
!      F1(1,2) = 4.72810000000000
!      F1(1,3) = 2.25645006185101e-15
!      F1(2,1) = 4.72810000000000
!      F1(2,2) = -0.499999999999998
!      F1(2,3) = -5.53660000000000
!      F1(3,1) = 1.70641487942310e-15
!      F1(3,2) = -5.53660000000000
!      F1(3,3) = 0.494800000000000
!
!      F2(1,1) = -5.71546074909089
!      F2(1,2) = 4.72810000000000
!      F2(1,3) = 1.48615618047102e-15
!      F2(2,1) = 4.72810000000000
!      F2(2,2) = -0.499999999999998
!      F2(2,3) = -5.53660000000000
!      F2(3,1) = 5.34189574550991e-16
!      F2(3,2) = -5.53660000000000
!      F2(3,3) = 0.494800000000000

!!CNO Model
      F1(1,1) = -5.71546074909095
      F1(1,2) = 4.72809999999991
      F1(1,3) = 1.39161859101452e-13
      F1(2,1) = 4.72810000000018
      F1(2,2) = -0.499999999999671
      F1(2,3) = -5.53659999999944
      F1(3,1) = 1.72427642462452e-13
      F1(3,2) = -5.53659999999999
      F1(3,3) = 0.494799999999830

      F2(1,1) = -6.46114384000005
      F2(1,2) = 4.72809999999990
      F2(1,3) = 1.34539159416344e-13
      F2(2,1) = 4.72810000000019
      F2(2,2) = -0.499999999999671
      F2(2,3) = -5.53659999999944
      F2(3,1) = 1.77405146637772e-13
      F2(3,2) = -5.53659999999999
      F2(3,3) = 0.494799999999830

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

      !Vector constante b1
      b1(1) = 0.0d0
      b1(2) = 0.0d0
      b1(3) = 0.0d0


      aux1 = matmul(A1, yD)
      !write(*,"(3F17.3)") aux1
      aux1 = aux1 + b1
      aux2 = M1D*aux1

      !write(*,"(3F17.3)") aux2

      aux3 = matmul(A2, yD)
      !write(*,"(3F17.3)") aux3
      aux3 = aux3 + b1
      aux4 = M2D*aux3


      dyD = aux2 + aux4
      !dyD = M1D*(A1*yD+b1)+M2D*(A2*yD+b2)
      !write(*,"(3F17.3)") dyD


      prod1 = matmul(F1,yR)
      par1 = prod1 + b1
      u11 = -M1R*par1


      prod2 = matmul(F1,yD)
      par2 = prod2 + b1
      u12 = M1D*par2

      U1 = u11 + u12
      !U1 = -M1R*(F1*yR+b1) + M1D*(F1*yD+b1)


      prod3 = matmul(F2,yR)
      par3 = prod3+b1
      u21 = -M2R*par3

      prod4 = matmul(F2,yD)
      par4 = prod4 + b1
      u22 = M2D*par4

      U2 = u21 + u22
      !U2 = -M2R*(F2*yR+b2) + M2D*(F2*yD+b2)

      U(:) = U1 + U2

      aux5 = matmul(A1,yR)
      aux6 = aux5 + b1
      dyR1 = M1R*aux6

      aux7 = matmul(A2,yR)
      aux8 = aux7 + b1
      dyR2 = M2R*aux8


      aux9 = matmul(B,U)

      dyR = dyR1 + dyR2 + aux9
      !M1R*(A1*yR+b1)+M2R*(A2*yR+b2)+ B*U

      DY(1:3) = dyD
      DY(4:6) = dyR



      return
      end
