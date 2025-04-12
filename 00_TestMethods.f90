double precision function my_func(x) result(y)
   implicit none
   double precision, intent(in) :: x
   y = -0.4d0*x**2 + 2.3d0*x + 2.2d0
end function my_func

double precision function my_func_deriv(x) result(dy)
   implicit none
   double precision, intent(in) :: x
   ! Derivada analítica: f'(x) = -0.8 x + 2.3
   dy = -0.8d0*x + 2.3d0
end function my_func_deriv

program principal
   use secant_module
   use newton_module
   use bissection_module
   implicit none

   double precision :: roota, rootb, rootc
   double precision :: x0, x1, tol
   integer :: maxIter, modPrint
   double precision, external :: my_func, my_func_deriv

   ! Parámetros
   x0 = 5.0d0       ! Primer punto inicial
   x1 = 8.0d0       ! Segundo punto inicial
   tol = 9d-16
   maxIter = 50
   modPrint = 1

   print *, "secant ========================================="
   roota = secant(x0, x1, tol, maxIter, modPrint, my_func)
   print *, "newton ========================================="
   rootb = newton(x0, tol, maxIter, modPrint, my_func, my_func_deriv)
   print *, "bissection ========================================="
   rootc = bissection(x0, x1, tol, maxIter, modPrint, my_func)

   print *, "Raíz aproximada:"
   print *, "secant:    ", roota
   print *, "newton:    ", rootb
   print *, "bissection:", rootc

end program principal
