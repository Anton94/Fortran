!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          Anton Dudov           !
!            1612555			 !
! Tools, homework 4, exercise 12 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! I used g95 compiler for Windows - http://www.fortran.com/the-fortran-company-homepage/whats-new/g95-windows-download/
! Note to myslef:
	! > g95 main.f95 -c !!! -c -> without exe
	! > g95 module.f95 -c 
	! > g95 main.o module.o -o betting
	! and now executing with input file as inputStram for example martin.in: > betting < martin.in

module betting
	implicit none

	! Type tip for the goals of each team.
	type tip
		integer :: goalsTeamOne
		integer :: goalsTeamTwo
	end type tip

	! Type bet that represents all tips for a tournament of 12 matches.
	type bet
		character(64) :: name
		type(tip), dimension(12) :: tips
	end type bet
contains
	! Prints a tip to the output.
	subroutine printTip(t)
		type(tip), intent(in) :: t

		write (*, "(I1)", advance = "no"), t%goalsTeamOne
		write (*, "(A)", advance = "no"), " - "
		write (*, "(I1)", advance = "no"), t%goalsTeamTwo

	end subroutine printTip

	! Returns the points which the user takes from his bet.
	function calculatePointsFromMatchBet(userTip, realTip) result(points)
		implicit none

		type(tip), intent(in) :: userTip, realTip
		integer :: points

		! The goals for the user and real result(shorter names..).
		integer :: usrOne, usrTwo, realOne, realTwo

		usrOne  = userTip%goalsTeamOne
		usrTwo  = userTip%goalsTeamTwo
		realOne = realTip%goalsTeamOne
		realTwo = realTip%goalsTeamTwo
		points  = 0

		! The winner is right.
		if ( (usrOne == usrTwo .and. realOne == realTwo) .or. &
			&(usrOne > usrTwo .and. realOne > realTwo) .or. &
			&(usrOne < usrTwo .and. realOne < realTwo)) &
			& points = points + 3

		! Goal difference is right.
		if (abs(usrOne - usrTwo) == abs(realOne - realTwo)) points = points + 1

		! Total number of goals is right.
		if (usrOne + usrTwo == realOne + realTwo) points = points + 2

	end function calculatePointsFromMatchBet

	! Evaluates the user bet with the real scores and proper formats the output data. 
	subroutine evaluate(user, real)
		implicit none
		type(bet), intent(in) :: user, real
		
		integer :: totalScore, i, points
		
		totalScore = 0
		
		! Print the header line with the names.
		write (*, "(A)", advance="no"), "Results "
		write (*, "(A)", advance="no"), TRIM(real%name)
		write (*, "(A)", advance="no"), " -- "
		write (*, "(A)"), TRIM(user%name)
		i = 1
		do
			! Print single match stats.
			write (*, "(A, I2, A)", advance="no"), "Match ", i, ":   "

			! Print the tips.
			call printTip(user%tips(i))
			write(*, "(A4)", advance="no"), "   ("
			call printTip(real%tips(i))
			
			! Calculate the points.
			points = calculatePointsFromMatchBet(user%tips(i), real%tips(i))

			! Print the points and new line.
			write(*, "(A12, I1)"), ")   points: ", points

			! Add them to the total score.
			totalScore = totalScore + points
			i = i + 1
			if (i > 12) exit
		end do

		! Print the total score.
		write(*, "(A4)", advance="no"), "Sum:"
		print *, totalScore
	end subroutine evaluate

end module betting