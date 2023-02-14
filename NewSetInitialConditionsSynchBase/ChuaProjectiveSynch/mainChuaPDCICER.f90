!  Programa principa para el algoritmo de oscilatorio
!  trigonometrico de Gautshi explicito segundo orden algebraico

program ChaosChuaHidden

implicit none

external ChuaSystem  ! Sistema caotico dinamico


! Tipos de las variable
! La primera llamada al metodo integrador bandera = 1
integer(kind = 4) :: bandera = 1

! Contador de visualizacion de las solucion de la edo
integer(kind = 4) :: contadorV = 0

! Contador de llamada a la subroutine Chua
integer(kind = 4) :: contadorEVal = 0
common/contadorEval/contadorEval

! Numero de edos
integer(kind = 4), parameter :: nuecdf = 6
integer(kind = 4), parameter :: unidadEsc = 10
integer(kind = 4), parameter :: unidadLec1 = 11
integer(kind = 4), parameter :: unidadLec2 = 12
integer(kind = 4), parameter :: unidadEsc2 = 13

integer(kind = 4) :: i, j, k, puntos, n, noIteraciones
integer(kind = 4) :: l, m, VecItera(4500), cont = 0, maxitera(1) = 0

real(kind = 8) :: beta1, beta0, h, t, w, nu, nu2, nu4, t0, tf
real(kind = 8) :: y(nuecdf), dy(nuecdf), y1(nuecdf), dy1(nuecdf)
real(kind = 8) :: sy(nuecdf), sdy(nuecdf), aux
real(kind = 8) :: u(3)

! Error
real(kind = 8) :: e(3), er(3), ea(3)

!Numero de condiciones iniciales
integer(kind = 4), parameter :: nuIC = 30

!Numero de alfas
integer(kind = 4), parameter :: nuAlphas = 6

! vectores para leer las Cond. iniciales y las alfas
double precision :: VecIC(nuIC,nuecdf), VecAlphas(nuAlphas)

! parameters of projective synch
double precision :: palpha


!Abrimos el archivo de las Cond. iniciales
open (unit = unidadLec1, file = 'CondicionesInicialesChua30.txt', status = 'OLD')
!leemos los datos
 do i=1,nuIC
    read(unidadLec1,*)(VecIC(i,j), j = 1, nuecdf)
    write(*,6000)(VecIC(i,j), j = 1, nuecdf)
 end do

!Abrimos el archivo de las alfas
open (unit = unidadLec2, file = 'Alfas.txt', status = 'OLD')
!leemos los datos
 do i=1,nuAlphas
    read(unidadLec2,7000)VecAlphas(i)
    write(*,7000) VecAlphas(i)
 end do

!Abrimos el archivo para guardar la primera iteracion de cada sincronizacion
! a partir de la cual el error se queda atrapado en una vecindad especifica
open (unit = unidadEsc2, file = 'EvaluationsProjectiveChua30.txt', status = 'UNKNOWN')

! Aqui inician los ciclos

do l = 1, nuAlphas

    ! parametro alfa
    palpha = VecAlphas(l)

    do m = 1, nuIC

        ! Inicializamos los arreglos a cero
        do i = 1, nuecdf
            y(i) = 0.0d0
            dy(i) = 0.0d0
            y1(i) = 0.0d0
            dy1(i) = 0.0d0
            sy(i) = 0.0d0
            sdy(i) = 0.0d0
        enddo

        do i = 1, 3
            e(i) = 0.0d0
            ea(i) = 0.0d0
            er(i) = 0.0d0
            u(i) = 0.0d0
        end do

        do i = 1, 4500
            VecItera(i) = 0
        end do

        ! condiciones iniciales
        do i = 1, nuecdf
            y(i) = VecIC(m,i)
        end do

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
        do j = 1, 3
            e(j) = y(j+3)-palpha*y(j)
        end do


        ! * Se abre archivo de impresion de resultados.
        !open (unit = unidadEsc, file = 'chaosChuaTest.txt', status = 'UNKNOWN')
        !write(unidadEsc, 5000) t0, (y(j), j = 1, nuecdf), (er(k), k = 1, 3), (u(k), k = 1, 3), 0, contadorEVal ! condiciones iniciales


        t = t0
        ! Ciclo simulacion
        do i = 1, noIteraciones
            call GautshiOrden2(ChuaSystem, nuecdf, bandera, beta0, beta1, y1, dy1, sy, sdy, t, w, y, h, dy, u, palpha)

            ! se calcula el error
            do j = 1, 3
                e(j)=y(j+3)-palpha*y(j)
            end do

            ! Contadodr de visualizaciones
            contadorV = contadorV + 1
            if (contadorV == n) then
                !write(*, 5000) t, (y(j), j = 1, nuecdf), (er(k), k = 1, 3), (u(k), k = 1, 3), i, contadorEVal
                !write(unidadEsc, 5000) t, (y(j), j = 1, nuecdf), (er(k), k = 1, 3), (u(k), k = 1, 3), i, contadorEVal
                ! Inicializamos los contadores
                contadorV = 0
                contadorEVal = 0
            endif

            ! se calcula el error absoluto
            do j = 1, 3
                ea(j) = abs(e(j))
            end do

            ! se calcula el error relativo verificando que el denominador no sea cero
            if (y(1) == 0.0d0) then
                er(1) = 0.0d0
            else
                er(1) = ea(1)/y(1)
            end if

            if (y(2) == 0.0d0) then
                er(2) = 0.0d0
            else
                er(2) = ea(2)/y(2)
            end if

            if (y(3) == 0.0d0) then
                er(3) = 0.0d0
            else
                er(3) = ea(3)/y(3)
            end if


            !se busca la iteracion a partir de la cual el error queda atrapado en una vecindad
            if (er(1) > 0.02 .or. er(2) > 0.02 .or. er(3) > 0.02) then
                cont = cont + 1
                VecItera(cont) = i
            end if

        enddo !ciclo integrador

        !close(unit = unidadEsc)  ! cerramos archivo de escritura

        maxitera = maxloc(VecItera) ! devuelve la posicion del maximo
        write(unidadEsc2, 8000) m, VecItera(maxitera)
        write(*, 8000) m, VecItera(maxitera)

        ! se reinician todo antes de entrar al ciclo principal de integracion
        bandera = 1
        contadorV = 0
        contadorEVal = 0
        cont = 0

    end do ! ciclo de las 25 condiciones iniciales


end do !ciclo de las alfas

close(unit = unidadEsc2) !cerramos archivo de las iteraciones
close(unit = unidadLec1) !cerramos archivo de las CI
close(unit = unidadLec2) !cerramos archivo de las alfas
call exit

! Formatos
!5000 format(4X, F10.3, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, F15.5, I10, I10)
5000 format( F10.3, 12F15.5, I10, I10)

6000 format( F15.5, F15.5, F15.5, F15.5, F15.5, F15.5)

7000 format( F10.3)

8000 format (I10, I10)

end program ChaosChuaHidden
