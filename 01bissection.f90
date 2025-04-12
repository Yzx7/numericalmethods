function biss(a, b, tol, maxIter, modPrint, f) result(retval)
   double precision, intent(in) :: a, b, tol
   double precision, external :: f
   integer, intent(in) :: maxIter,modPrint
   double precision :: retval
   double precision :: fa, fb, fc, c, a_current, b_current

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

end function biss

function my_func(x) result(y)
   double precision, intent(in) :: x
   double precision :: y
   y = -0.4*x**2 + 2.3*x + 2.2
end function

program bissection
   implicit none
   double precision :: root
   double precision :: tol = 1d-20, a = 5.0d0, b = 8.0d0
   double precision, external :: my_func, biss
   integer :: maxIter = 60

   root = biss(a, b, tol, maxIter, 1, my_func)
   print *, "res:", root

end program

