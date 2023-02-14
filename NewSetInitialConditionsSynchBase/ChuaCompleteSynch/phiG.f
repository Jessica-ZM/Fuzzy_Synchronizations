!  Fixed-Format Fortran Source File
! Funcion para un oscilador caotico de 2 scrolls
! Pendientes y puntos de quiebre del articulo de kiseleva
! funcion de phi del modelado difuso
!******************************************
      !Funtion
      function phiG(x)
      implicit none
      double precision :: m0, m1, p
      double precision x, phiG,g

      !Classical
      !m0 = -1.1384d0
	  !m1 = -0.7225d0

	  !Hidden
	  m0 = -0.1768d0
	  m1 = -1.1468d0

	  p = 1.0d0


!     PieceWise Linear function
	  if (x == 0) then
            phiG = m0
	  else if (x > p)  then    !Cota superior excedida
            g = m1*x+(m0-m1)*1
			phiG = g/x
	  else if (x < -p) then   !Cota inferior excedida
            g = m1*x+(m0-m1)*(-1)
            phiG = g/x
	  else ! rango intermedio
            g = m1*x+(m0-m1)*x
			phiG = g/x
	  end if
      !return
      !write(*,"(F7.3)") x
      !write(*,"(F7.3)") phiG
      end function phiG


