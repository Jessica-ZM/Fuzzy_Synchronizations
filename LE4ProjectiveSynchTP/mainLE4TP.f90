!  Programa principa para el algoritmo de oscilatorio
!  trigonometrico de Gautshi explicito segundo orden algebraico

program ChaosLE4System

implicit none

external LE4System  ! Sistema caotico dinamico


! Tipos de las variable
! La primera llamada al metodo integrador bandera = 1
integer(kind = 4) :: bandera = 1

! Contador de visualizacion de las solucion de la edo
integer(kind = 4) :: contadorV = 0

! Contador de llamada a la subroutine LE4
integer(kind = 4) :: contadorEVal = 0
common/contadorEval/contadorEval

! Numero de edos
integer(kind = 4), parameter :: nuecdf = 6
integer(kind = 4), parameter :: unidadEsc = 10
integer(kind = 4), parameter :: unidadLec = 15

integer(kind = 4) :: i, j, k, puntos, n, m, noIteraciones

real(kind = 8) :: beta1, beta0, h, t, w, nu, nu2, nu4, t0, tf
real(kind = 8) :: y(nuecdf), dy(nuecdf), y1(nuecdf), dy1(nuecdf)
real(kind = 8) :: sy(nuecdf), sdy(nuecdf), aux
real(kind = 8) :: u(3)
real(kind = 8) :: vp(37), f1(37), f2(37)
integer(kind = 4) :: mv

! Error
double precision :: er(3)

! parameters of projective synch
double precision :: palpha
palpha = 0.5d0



! Inicializmos los arreglos a cero
do i = 1, nuecdf
   y(i) = 0.0d0
   dy(i) = 0.0d0
   y1(i) = 0.0d0
   dy1(i) = 0.0d0
   sy(i) = 0.0d0
   sdy(i) = 0.0d0
enddo

er(1) = 0.0d0
er(2) = 0.0d0
er(3) = 0.0d0

u(1) = 0.0d0
u(2) = 0.0d0
u(3) = 0.0d0

! Condiciones iniciales, t = 0
!Master attractor oculto
y(1) = 0.2d0
y(2) = 0.7d0
y(3) = 0.0d0

! Slave
y(4) = -0.1d0
y(5) = 0.3d0
y(6) = 0.1d0

! Tiempo inicial y final
t0 = 0.0d0
tf = 150.0d0

! Frecuencia de la solucion de la edo
w = 1.0d0

! Paso de integracion
h = 1.0d0 / 128.0d0

! Numero de puntos de la solucion de la edo a visualizar
puntos = 19200

! Numero de puntos n de la malla en el intervalo
! [t0, tf] inducidos por el paso h
! n = (tf - t0) / h
n = anint((tf -t0) / h)
noIteraciones = n

! Calculamos el intervalo de tiempo para
! visualizar los puntos de la solucion
if (mod(n, puntos) == 0) then
   n = n / puntos
else
   aux =  n / puntos
   n = anint(aux)
endif
! Errores en t=0
er(1)=y(4)-palpha*y(1)
er(2)=y(5)-palpha*y(2)
er(3)=y(6)-palpha*y(3)


! * Se abre archivo de impresion de resultados.
open (unit = unidadEsc, file = 'chaosLE499_TP_CNO.txt', status = 'UNKNOWN')
 write(unidadEsc, 5000) t0, (y(j), j = 1, nuecdf), (er(k), k = 1, 3), (u(k), k = 1, 3), 0, contadorEVal ! condiciones iniciales

! Se abre el archivo que contiene los vectores de las funciones de mebresia
open (unit = unidadLec, file = 'FM_CNO.txt', status = 'UNKNOWN')
! se leen los puntos de las funciones de membresia

mv = 37
do m = 1, mv
    read(unidadLec, 10000) vp(m), f1(m), f2(m)
enddo

write(*, 10000) vp(1), f1(1), f2(1)
write(*, 10000) vp(20), f1(20), f2(20)
write(*, 10000) vp(37), f1(37), f2(37)

 ! Ciclo simulacion
do i = 1, noIteraciones
   call GautshiOrden2(LE4System, nuecdf, bandera, beta0, beta1, y1, dy1, sy, sdy, t, w, y, h, dy, u, mv, vp, f1,f2)

   ! se calcula el error
   er(1)=y(4)-palpha*y(1)
   er(2)=y(5)-palpha*y(2)
   er(3)=y(6)-palpha*y(3)

   ! Contadodr de visualizaciones
   contadorV = contadorV + 1
   if (contadorV == n) then
      !write(*, 5000) t, (y(j), j = 1, nuecdf), (er(k), k = 1, 3), (u(k), k = 1, 3), i, contadorEVal
	  write(unidadEsc, 5000) t, (y(j), j = 1, nuecdf), (er(k), k = 1, 3), (u(k), k = 1, 3), i, contadorEVal
	  ! Inicializamos los contadores
	  contadorV = 0
	  contadorEVal = 0
   endif
enddo

close(unit = unidadEsc)  ! cerramos archivo de escritura
close(unit = unidadLec)  ! cerramos archivo de lectura
call exit

! Formatos
!5000 format(4X, F10.3, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, I10, I10)
5000 format( F10.3, 12F15.5, I10, I10)
10000 format(3F17.7)

end program ChaosLE4System
