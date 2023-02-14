!******************************************
      !Subroutine SprottASystem: contiene el sistema de edo's del sistema de SprottA
      subroutine SprottASystem (nuecdf,T, Y, DY, U)

     !*****************************************************************


      implicit none
	  integer(kind = 4) :: nuecdf !numero de edos

      double precision :: d, sum1(3), sum2(3)
      double precision :: A1(3, 3), A2(3, 3)
      double precision :: yD(3), yR(3), U1(3), U2(3), dyD(3), dyR(3)
      double precision :: aux1(3), aux2(3), aux3(3), aux4(3)
      double precision :: aux5(3), aux6(3), aux7(3), aux8(3), dyR1(3), dyR2(3)
      double precision :: par1(3), par2(3), par3(3), par4(3), aux9(3)
      double precision :: prod1(3), prod2(3), prod3(3), prod4(3)
      double precision :: u11(3), u12(3), u13(3), u14(3)
      double precision :: u21(3), u22(3), u23(3), u24(3)
      double precision :: M1D, M2D, M1R, M2R, F1(3,3), F2(3,3), B(3,3), b1(3), U(3)
      ! Coeficientes del sistema de SprottA
      double precision :: T, Y(nuecdf), DY(nuecdf)
      integer :: contadorEVal
      common/contadorEval/contadorEval

      ! Actualizamos el contador
      contadorEval = contadorEval + 1

      !Valor de los coeficientes del sistema de SprottA
      ! no tiene

	  ! Parameter fuzzy model
	  d = 5.0d0


	  A1(1,1) =  0.0d0
	  A1(1,2) =  1.0d0
	  A1(1,3) =  0.0d0
	  A1(2,1) =  -1.0d0
	  A1(2,2) =  0.0d0
	  A1(2,3) =  d
	  A1(3,1) =  0.0d0
	  A1(3,2) =  -d
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


	  !write(*,"(3F7.3)") A1
	  !write(*,"(3F7.3)") A2

	  !Vector constante b1
      b1(1) = 0.0d0
      b1(2) = 0.0d0
      b1(3) = 1.0d0


	  !funciones difusas drive
      M1D = (0.5)*(1+(Y(2))/d)
      M2D = (0.5)*(1-(Y(2))/d)
      !write(*,"(F7.3)") M1D
	  !write(*,"(F7.3)") M2D

      !funciones difusas drive
      M1R = (0.5)*(1+(Y(5))/d)
      M2R = (0.5)*(1-(Y(5))/d)

      F1(1,1) = 5.0000000e-01
      F1(1,2) = -1.2394558e-17
      F1(1,3) = -1.0890905e-16
      F1(2,1) = -1.2394558e-17
      F1(2,2) = 5.0000000e-01
      F1(2,3) = -1.2933841e-17
      F1(3,1) = -1.0890905e-16
      F1(3,2) = -1.2933841e-17
      F1(3,3) = 5.0000000e-01

      F2(1,1) = 5.0000000e-01
      F2(1,2) = -6.6085452e-18
      F2(1,3) = -1.0923751e-20
      F2(2,1) = -6.6085452e-18
      F2(2,2) = 5.0000000e-01
      F2(2,3) = 1.4091066e-17
      F2(3,1) = -1.0923751e-20
      F2(3,2) = 1.4091066e-17
      F2(3,3) = 5.0000000e-01

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


      yD(:) = Y(1:3)
      yR(:) = Y(4:6)
      !write(*,"(3F17.3)") yD
	  !write(*,"(3F17.3)") yR


      aux1 = matmul(A1, yD)
      !write(*,"(3F17.3)") aux1
      sum1 = aux1 + b1
      aux2 = M1D*sum1
      !write(*,"(3F17.3)") aux2

      aux3 = matmul(A2, yD)
      !write(*,"(3F17.3)") aux3
      sum2 = aux3 + b1
      aux4 = M2D*sum2

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
