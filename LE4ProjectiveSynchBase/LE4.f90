!******************************************
      !Subroutine LE4System: contiene el sistema de edo's del sistema de LE4
      subroutine LE4System (nuecdf,T, Y, DY, U)

     !*****************************************************************


      implicit none
	  integer(kind = 4) :: nuecdf !numero de edos

      double precision :: palpha, d, a, b
      double precision :: A1(3, 3), A2(3, 3)
      double precision :: yD(3), yR(3), U1(3), U2(3), dyD(3), dyR(3)
      double precision :: aux1(3), aux2(3), aux3(3), aux4(3)
      double precision :: aux5(3), aux6(3), aux7(3), aux8(3), dyR1(3), dyR2(3)
      double precision :: par1, par2, par3, par4
      double precision :: prod1(3), prod2(3), prod3(3), prod4(3)
      double precision :: u11(3), u12(3), u13(3), u14(3)
      double precision :: u21(3), u22(3), u23(3), u24(3)
      double precision :: M1D, M2D, pgamma, U(3)
      ! Coeficientes del sistema de LE4
      double precision :: T, Y(nuecdf), DY(nuecdf)
      integer :: contadorEVal
      common/contadorEval/contadorEval

      ! Actualizamos el contador
      contadorEval = contadorEval + 1

      !Valor de los coeficientes del sistema de LE4
      a = 4.0d0
	  b = 0.6d0

	  ! Parameter fuzzy model
	  d = 0.8d0

	  ! parameters of projective synch
	  palpha = 0.5d0

	  ! parameter for control
	  pgamma = 0.71d0


	  A1(1,1) =  0.0d0
	  A1(1,2) =  1.0d0
	  A1(1,3) =  0.0d0
	  A1(2,1) =  -1.0d0
	  A1(2,2) =  0.0d0
	  A1(2,3) =  d
	  A1(3,1) =  -d*(a+b)
	  A1(3,2) =  0.0d0
	  A1(3,3) =  -d


	  A2(1,1) =  0.0d0
	  A2(1,2) =  1.0d0
	  A2(1,3) =  0.0d0
	  A2(2,1) =  -1.0d0
	  A2(2,2) =  0.0d0
	  A2(2,3) =  -d
	  A2(3,1) =  d*(a+b)
	  A2(3,2) =  0.0d0
	  A2(3,3) =  d



	  !write(*,"(3F7.3)") A1
	  !write(*,"(3F7.3)") A2

	  yD(:) = Y(1:3)
      yR(:) = Y(4:6)
      !write(*,"(3F17.3)") yD
	  !write(*,"(3F17.3)") yR


      M1D = (0.5)*(1+(Y(2)/d))
      M2D = (0.5)*(1-(Y(2)/d))
      !write(*,"(F7.3)")

      !write(*,"(F7.3)") M1D
	  !write(*,"(F7.3)") M2D



      aux1 = matmul(A1, yD)
      !write(*,"(3F17.3)") aux1
      aux2 = M1D*aux1
      !write(*,"(3F17.3)") aux2

      aux3 = matmul(A2, yD)
      !write(*,"(3F17.3)") aux3
      aux4 = M2D*aux3

      dyD = aux2 + aux4
      !write(*,"(3F17.3)") dyD

      par1 = -M1D*pgamma
      prod1 = matmul(A1,yR)
      u11 = par1*prod1
      par2 = M1D*pgamma*palpha
      prod2 = matmul(A1,yD)
      u12 = par2*prod2
      u13 = par1*yR
      u14 = par2*yD

      U1 = u11 + u12 + u13 + u14
      !U1 = -M1D*pgamma*A1*yR + M1D*pgamma*a*A1*yD - M1D*pgamma*yR + M1D*pgamma*a*yD

      par3 = -M2D*pgamma
      prod3 = matmul(A2,yR)
      u21 = par3*prod3
      par4 = M2D*pgamma*palpha
      prod4 = matmul(A2,yD)
      u22 = par4*prod4
      u23 = par3*yR
      u24 = par4*yD

      U2 = u21 + u22 + u23 + u24
      !U2 = -M2D*pgamma*A2*yR + M2D*pgamma*a*A2*yD - M2D*pgamma*yR + M2D*pgamma*a*yD


      aux5 = matmul(A1,yR)
      aux6 = aux5 + U1
      dyR1 = M1D*aux6
      aux7 = matmul(A2,yR)
      aux8 = aux7 + U2
      dyR2 = M2D*aux8

      dyR = dyR1 + dyR2
      !dyR = M1D*(A1*yR+U1) + M2D*(A2*yR+U2)

      DY(1:3) = dyD
      DY(4:6) = dyR
      U(:) = U1 + U2


      return
      end
