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

module polynomial
	implicit none

	! Type polynom who contains a array with length 5 for the polynom's coefficients.
	type polynom
		real, dimension(5) :: coefs
	end type polynom

contains

	! Returns the evaluation of the given polynom for some x.
	function evaluate(poly, x) result(value)
		implicit none

		type(polynom), intent(in) :: poly
		real, intent(in) :: x

		real :: value, temp
		integer :: i

		! I will use temp to make bigger powers of X, to reuse the calculations so far.

		value = 0.0
		! First iteration temp will be x on the power of 0, which is 1.
		temp = 1.0
		i = 1
		do
			value = value + (poly%coefs(i))*temp

			temp = temp * x
			i = i + 1
			if (i > 5) exit
		end do
	end function evaluate

	! Returns the derivative polynom of the given one.
	function derivative(poly) result(newPoly)
		implicit none

		type(polynom), intent(in) :: poly
		type(polynom) :: newPoly

		integer :: i

		! The first coefficient is constant, so I don't care for him.

		i = 1
		do
			! The coefficient on position @i in the new polynom is the coefficient in the given one
			! multiplied by the power of the X on that position, which is exacly the @i
			newPoly%coefs(i) = poly%coefs(i + 1) * i

			i = i + 1
			if (i > 4) exit
		end do
		! The last coefficient in the derivative polynom is 0 (no x ^ 4).
		newPoly%coefs(5) = 0.0
	end function derivative

	! Returns 1 if the given root 0's the polynom(with the range of some little epsilon).
	! Otherwise returns 0.
	function checkRoot(poly, root) result(res)
		implicit none

		type(polynom), intent(in) :: poly
		real, intent(in) :: root
		integer :: res

		! For the floating point equality comparsion.
		real, save :: epsilon = 0.00000001
		integer :: i
		real :: valueOfFunctionWithTheRoot

		valueOfFunctionWithTheRoot = evaluate(poly, root)

		! res = abs(valueOfFunctionWithTheRoot) < epsilon

		if (abs(valueOfFunctionWithTheRoot) < epsilon) then
			res = 1
		else
			res = 0
		end if
		
	end function checkRoot

	! The Newton's method. @f - the polynom, @df - derivative of @f
	! and @x - the initial guess.
	! Returns the found root (or the inital guess if it can't find it
	! in the range of 100 or @n iterations.
	recursive function newtonsMethod(f, df, x, n) result(root)
		implicit none

		type(polynom), intent(in) :: f, df
		real, intent(in) :: x
		real :: newX
		real :: root
		integer, optional, intent(in) :: n
		integer, save :: depth = 1
		real, save :: initialX = 0.0
		integer :: limit

		! Keep the initial x.
		if (depth == 1) initialX = x

		! Set the limit. A little dummy way...
		if (present(n)) then
			limit = n
		else
			limit = 100
		end if

		! If the root is valid one.
		if (checkRoot(f, x) == 1) then
			depth = 0
			root = x
		! If the depth is still good(not over the limit, < because I check the root first).
		else if (depth < limit) then
			newX = x - evaluate(f, x) / evaluate(df, x)
			root = newtonsMethod(f, df, newX, n)
		else
			root = initialX
		end if
	end function newtonsMethod

end module polynomial