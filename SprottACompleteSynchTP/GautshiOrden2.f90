!  Algoritmo polinomio trigonometrico Gautschi
! Explicito, orden algebraico 2, orden trigonometrico 1
!
! yn+2 = yn+1 + h(beta1 fn+1 + beta0 fn)
!
subroutine GautshiOrden2(dsedo, nuecdf, bandera, beta0, beta1, y1, dy1, sy, sdy, t, w, y, h, dy,u, mv, vp, fm1, fm2)

implicit none

external dsedo  ! modelo con las edo's

! Tipos de las variable

integer(kind = 4) :: i, bandera, nuecdf !bandera = 1 primera llamada
                                        !nuecdf = numero de edos

double precision :: beta1, beta0, h, t, w, nu, nu2, nu4
double precision :: y(nuecdf), dy(nuecdf), y1(nuecdf), dy1(nuecdf) !y's variables de estado
                                                                   !dy's derivadas
double precision :: sy(nuecdf), sdy(nuecdf) !variables auxiliares de almacenamiento

double precision :: u(3)

double precision :: vp(37), fm1(37), fm2(37)
integer(kind = 4) :: mv

!print *, "Entramos Gautshi"

if (bandera == 1) then
   ! Primera llamada al algoritmo
   ! Calculamos coeficientes del algoritmo
   nu = w * h
   nu2 = nu ** 2
   nu4 = nu ** 4
   ! Escalamos beta1 y beta0 por un factor de 1/120
   beta1 =   (1.0d0/80.0d0) * (120.0d0 - 30.0d0 * nu2 + nu4)
   beta0 =  -(1.0d0/240.0d0) * (120.0d0 + 10.0d0 * nu2 + nu4 )

   ! Actualizamos la derivada fn al tiempo t = tn
   call dsedo(nuecdf, t, y, dy,u, mv, vp, fm1, fm2)

   !Estimanos yn+1 por medio de un RK2: y = yn+1, fn = dy
   call drtrap(dsedo, nuecdf, y1, dy1, t, y, h, dy, u, mv, vp, fm1, fm2)

   ! Actualizamos el tiempo: t = tn+1
   t = t + h
   ! Salvamos los valores de yn+1 y fn
   do i = 1, nuecdf
      sy(i) = y(i)       !yn+1
	  sdy(i) = dy(i)     !fn
   enddo

   bandera = 2
else
   ! Llamadas posteriores al metodo
   ! Seccion para estimar yn+2
   ! Calculamos derivada fn+1, t = tn+1
   call dsedo(nuecdf, t, y, dy,u, mv, vp, fm1, fm2)  !dy = fn+1

   ! Algoritmo oscilatorio trigonometrico Gautshi explicito, orden algebraico 2
   do i = 1, nuecdf
      !yn+2 = yn+1 + h(b1 fn+1 + bo fn)
      y(i) = sy(i) + h * (beta1 * dy(i) + beta0 * sdy(i))
   enddo

   !Actualizamos tiempo t = tn+2
   t = t + h
   !Salvamos fn+1 y yn+1
   do i = 1, nuecdf
      sy(i) = y(i)
	  sdy(i) = dy(i)
   enddo
endif

return
end subroutine GautshiOrden2


