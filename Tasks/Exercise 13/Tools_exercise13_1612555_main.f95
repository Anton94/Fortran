!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          Anton Dudov           !
!            1612555			 !
! Tools, homework 4, exercise 13 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! I used g95 compiler for Windows - http://www.fortran.com/the-fortran-company-homepage/whats-new/g95-windows-download/
! Note to myslef:
	! > g95 main.f95 -c ! -c -> without exe
	! > g95 module.f95 -c 
	! > g95 main.o module.o -o newtons
	! and now executing newtons

program newtons
	use polynomial

	implicit none

	type(polynom) :: poly


	! Sets the coefficients.
	poly%coefs = (/1.0, 2.0, 1.0, 0.0, 0.0/)
	poly%coefs = (/1.0, 0.0, 0.0, 1.0, 0.0/)
	

	print *, "Running some tests for myself..."
	call test() ! Run sum tests to check the functionality

contains

	! Just to myself some tests.
	subroutine test
		type(polynom) :: poly, derivativePoly
		real :: x, root, initial
		! Sets the coefficients.
		poly%coefs = (/1.0, 2.0, 1.0, 0.0, 0.0/)
		poly%coefs = (/1.0, 0.0, 0.0, 1.0, 0.0/)
		x = 2

		! Simple test for the @evaluate function
		write (*, "(A9, 5F5.2)", advance="no"), "Polynom: ", poly%coefs
		write (*, "(A18, F5.2, A4, F9.2)"), " evaluate for X = ", x, " -> ", evaluate(poly, x)

		! Simple test for the @derivative function.
		derivativePoly = derivative(poly)
		write (*, "(A9, 5F5.2)"), "Polynom: ", derivativePoly%coefs

		! Simple test for the @checkRoot function.
		poly%coefs = (/-3.0, 2.0, 1.0, 0.0, 0.0/)
		write (*, "(A9, 5F5.2)"), "Polynom: ", poly%coefs
		root = -1.0
		write(*, "(A3, F6.2, A23, I1)"), "Is ", root, " root of the polynom ? ", checkRoot(poly, root)
		root = 1.0
		write(*, "(A3, F6.2, A23, I1)"), "Is ", root, " root of the polynom ? ", checkRoot(poly, root)
		root = 2.0
		write(*, "(A3, F6.2, A23, I1)"), "Is ", root, " root of the polynom ? ", checkRoot(poly, root)
		root = -3.0
		write(*, "(A3, F6.2, A23, I1)"), "Is ", root, " root of the polynom ? ", checkRoot(poly, root)

		! Test Newton's method.
		poly%coefs = (/-3.0, 2.0, 1.0, 0.0, 0.0/)
		derivativePoly = derivative(poly)
		write (*, "(A9, 5F5.2)"), "Polynom: ", poly%coefs

		initial = 40010.0
		root = newtonsMethod(poly, derivativePoly, initial)
		write(*, "(A12, F9.2, A24, F16.2)"), "Root found: ", root, " starting from initial: ", initial
		write(*, "(A3, F9.2, A23, I1)"), "Is ", root, " root of the polynom ? ", checkRoot(poly, root)

		initial = -40010.0
		root = newtonsMethod(poly, derivativePoly, initial)
		write(*, "(A12, F9.2, A24, F16.2)"), "Root found: ", root, " starting from initial: ", initial
		write(*, "(A3, F9.2, A23, I1)"), "Is ", root, " root of the polynom ? ", checkRoot(poly, root)

	end subroutine test

end program newtons