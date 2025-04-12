module secant_module
   implicit none
contains

   !--------------------------------------------------------------------
   !> Método de la secante para encontrar una raíz de f(x)
   !! @param x0        Primer punto inicial
   !! @param x1        Segundo punto inicial
   !! @param tol       Tolerancia para |f(x)|
   !! @param maxIter   Máximo de iteraciones
   !! @param modPrint  Cada cuántas iteraciones imprimir estado
   !! @param f         Función f(x)
   !! @return          Raíz aproximada
   function secant(x0, x1, tol, maxIter, modPrint, f) result(root)
      implicit none
      double precision, intent(in) :: x0, x1, tol
      integer, intent(in) :: maxIter, modPrint
      double precision, external :: f
      double precision :: root
      double precision :: xi, xi_1, fxi, fxi_1, xi_next
      integer :: i

      xi = x1
      xi_1 = x0
      fxi = f(xi)
      fxi_1 = f(xi_1)

      do i = 1, maxIter
         if (fxi - fxi_1 == 0.0d0) then
            print *, "Error: División por cero en iteración", i
            root = xi
            return
         end if

         xi_next = xi - fxi*(xi - xi_1)/(fxi - fxi_1)

         if (mod(i, modPrint) == 0) then
            print *, "[i:", i, "] x =", xi, " f(x) =", fxi
         end if

         if (abs(f(xi_next)) < tol) then
            root = xi_next
            return
         end if

         xi_1 = xi
         fxi_1 = fxi
         xi = xi_next
         fxi = f(xi)
      end do

      print *, "Advertencia: Máximo de iteraciones alcanzado"
      root = xi
   end function secant

end module secant_module