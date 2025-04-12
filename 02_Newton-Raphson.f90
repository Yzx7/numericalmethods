module newton_module
   implicit none
contains

   !--------------------------------------------------------------------
   !> Función genérica para Newton–Raphson
    !! @param x0        Punto inicial
    !! @param tol       Tolerancia deseada para |f(x)|
    !! @param maxIter   Número máximo de iteraciones
    !! @param modPrint  Cada cuántas iteraciones imprimir el estado
    !! @param f         Puntero a la función f(x)
    !! @param df        Puntero a la derivada f'(x)
    !! @return          Aproximación de la raíz o valor negativo en error
   double precision function newton(x0, tol, maxIter, modPrint, f, df) result(root)
      implicit none
      double precision, intent(in) :: x0, tol
      integer, intent(in) :: maxIter, modPrint
      double precision, external :: f, df
      double precision :: xi, fi, dfi
      integer :: i

      xi = x0
      fi = f(xi)

      do i = 1, maxIter
         dfi = df(xi)
         if (dfi == 0.0d0) then
            print *, "Error: derivada cero en iteración ", i
            root = xi
            return
         end if

         ! Actualización de Newton–Raphson
         xi = xi - fi/dfi
         fi = f(xi)

         if (mod(i, modPrint) == 0) then
            print *, "[i:", i, "] x =", xi, " f(x) =", fi, " f'(x) =", dfi
         end if

         if (abs(fi) < tol) then
            root = xi
            return
         end if
      end do

      print *, "Advertencia: Máximo de iteraciones alcanzado, |f(x)| =", abs(fi)
      root = xi
   end function newton

end module newton_module
