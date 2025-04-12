module bissection_module
   implicit none
contains

   double precision function bissection(a, b, tol, maxIter, modPrint, f) result(retval)
      double precision, intent(in) :: a, b, tol
      double precision, external :: f
      integer, intent(in) :: maxIter, modPrint
      double precision :: fa, fb, fc, c, a_current, b_current
      integer :: i

      a_current = a
      b_current = b
      fa = f(a_current)
      fb = f(b_current)

      if (fa*fb >= 0.0d0) then
         print *, "Error: La función no cambia de signo en el intervalo"
         retval = -1.0d0  ! Valor de error numérico
         return
      end if

      do i = 0, maxIter
         c = (a_current + b_current)/2.0
         fc = f(c)
         if (mod(i, modPrint) == 0) then
            print *, "[i: ", i, "] a = ", a_current, "b = ", b_current, "c = ", c, "f(c) = ", fc
         end if
         if (abs(fc) < tol) then
            retval = c
            return
         end if

         if (fa*fc < 0.0d0) then
            b_current = c
            !fb = fc  ! Mantener actualizado el valor de f(b)
         else
            a_current = c
            fa = fc  ! Mantener actualizado el valor de f(a)
         end if

      end do

      print *, "Advertencia: Máximo de iteraciones alcanzado"
      retval = c

   end function
end module bissection_module
