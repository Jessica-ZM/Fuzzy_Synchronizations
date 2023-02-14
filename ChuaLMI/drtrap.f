C      ***************************************************************
C
       SUBROUTINE DRTRAP (DSEDO,NUECDF,Y1,DY1,T,Y,H,DY,U)
C
C      ***************************************************************
C
C
C
C                    METODO DE LA REGLA TRAPEZOIDAL
C                   ( RUNGE-KUTTA DE SEGUNDO ORDEN )
C
C
C      Esta subrutina integra en doble precision, un sistema de
C      ecuaciones diferenciales ordinarias de primer orden de la
C      forma:
C
C        DY(I)/DT = F( t,y(1),y(2),...,y(nuecdf) ) ,I=1,NUECDF
C
C      donde : Y(I) es dado en T .
C
C      Esta subrutina ejecuta un paso de integracion .
C
C      ---------------------------------------------------------------
C
       implicit none
C      Los parametros son :
C
C      NUECDF  - Numero de ecuaciones diferenciales.
                 INTEGER NUECDF
C      DY      - Arreglo que contiene las derivadas.
                 DOUBLE PRECISION  DY(NUECDF)
C      DY1     - Arreglo auxiliar para almacenamiento
C              - de derivadas.
                 DOUBLE PRECISION  DY1(NUECDF)
C      H       - Tamano del paso.
                 DOUBLE PRECISION  H
C      I       - Variable de un "DO".
                 INTEGER I
C      DSEDO   - Subrutina que contiene las ecuaciones
C              - diferenciales a integrar.
                 EXTERNAL DSEDO
C      T       - Variable independiente.
                 DOUBLE PRECISION  T
C      Y       - Arreglo que contiene las variables
C              - dependientes.
                 DOUBLE PRECISION  Y(NUECDF)
C      Y1      - Arreglo auxiliar para almacenamiento
C              - de las variables dependientes.
                 DOUBLE PRECISION  Y1(NUECDF)
                 DOUBLE PRECISION  U(3)
C
C      ---------------------------------------------------------------
C
C      Los parametros de entrada son : H,NUECDF,DSEDO,T,Y.
C
C      Los parametros de salida son : Y.
C
C      ---------------------------------------------------------------
C
C      Este programa requiere de una subrutina llamada :
C
C                       DSEDO(nuecdf, T, Y, DY )
C
C      La subrutina DSEDO, evalua las derivadas de las variables depen-
C      dientes, las derivadas se almacenan en el arreglo DY(I), desde
C      I = 1, NUECDF; las y's se almacenan en el arreglo Y(I).
C
C      ***************************************************************
C
       DO 100 I = 1, NUECDF
          Y1(I) = Y(I) + H * DY(I)
100    CONTINUE
C      ----------------------------------------------
C      *  Se evalua la funcion F(tn + h , yn + k1)  *
C      ----------------------------------------------
       CALL DSEDO (nuecdf, T+H,Y1,DY1,U)
       DO 200 I = 1,NUECDF
C         ---------------------------------------
C         *  Algoritmo de la REGLA TRAPEZOIDAL  *
C         *   Yn+1 = Yn + (1/2) * ( K1 + K2 )   *
C         ---------------------------------------
          Y(I) = Y(I) + H * 0.5D0 * (DY(I) + DY1(I))
200    CONTINUE
       RETURN
       END

