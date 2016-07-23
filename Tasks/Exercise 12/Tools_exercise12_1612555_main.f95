!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          Anton Dudov           !
!            1612555			 !
! Tools, homework 4, exercise 12 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! I used g95 compiler for Windows - http://www.fortran.com/the-fortran-company-homepage/whats-new/g95-windows-download/
! Note to myslef:
	! > g95 main.f95 -c ! -c -> without exe
	! > g95 module.f95 -c 
	! > g95 main.o module.o -o betting
	! and now executing with input file as inputStram for example martin.in: > betting < martin.in

program bettingGame
	use betting

	implicit none

	
	type(bet) :: realBet, userBet
	integer :: i

	! Fill the name and scores with some values... 
	call fillRealBetDefaults(realBet)

	! Get the user name.
	write(*, "(A)", advance="no"), "Enter your name: "
	read(*, "(A)"), userBet%name

	! Get the user predictions.
	i = 1
	do
		write(*, "(A27, I2, A2)", advance="no"), "Enter the result for match ", i, ": "

		! I make the assumption that the bet of goals can`t be over 2 digit number...
		read(*, "(I2, I2)", advance = "no"), userBet%tips(i)%goalsTeamOne
		read(*, "(I2)"), userBet%tips(i)%goalsTeamTwo

		i = i + 1
		if (i > 12) exit
	end do

	! Clear the console.
	!call system('clear') ! For Linux
	call system('cls') ! For Windows

	! Output the result.
	call evaluate(userBet, realBet)

contains
	subroutine fillRealBetDefaults(realBet)
		type(bet) :: realBet

		type(tip) :: tempTip

		realBet%name = "UEFA Euro 2016"

		tempTip%goalsTeamOne = 2
		tempTip%goalsTeamTwo = 1

		realBet%tips(1)%goalsTeamOne = 2
		realBet%tips(1)%goalsTeamTwo = 1
		realBet%tips(2)%goalsTeamOne = 0
		realBet%tips(2)%goalsTeamTwo = 1
		realBet%tips(3)%goalsTeamOne = 2
		realBet%tips(3)%goalsTeamTwo = 1
		realBet%tips(4)%goalsTeamOne = 1
		realBet%tips(4)%goalsTeamTwo = 1
		realBet%tips(5)%goalsTeamOne = 1
		realBet%tips(5)%goalsTeamTwo = 0
		realBet%tips(6)%goalsTeamOne = 2
		realBet%tips(6)%goalsTeamTwo = 0
		realBet%tips(7)%goalsTeamOne = 0
		realBet%tips(7)%goalsTeamTwo = 1
		realBet%tips(8)%goalsTeamOne = 1
		realBet%tips(8)%goalsTeamTwo = 0
		realBet%tips(9)%goalsTeamOne = 1
		realBet%tips(9)%goalsTeamTwo = 1
		realBet%tips(10)%goalsTeamOne = 0
		realBet%tips(10)%goalsTeamTwo = 2
		realBet%tips(11)%goalsTeamOne = 0
		realBet%tips(11)%goalsTeamTwo = 2
		realBet%tips(12)%goalsTeamOne = 1
		realBet%tips(12)%goalsTeamTwo = 1

	end subroutine fillRealBetDefaults
	

end program bettingGame