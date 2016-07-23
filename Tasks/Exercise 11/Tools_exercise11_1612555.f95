!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          Anton Dudov           !
!            1612555			 !
! Tools, homework 4, exercise 11 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! I used g95 compiler for Windows - http://www.fortran.com/the-fortran-company-homepage/whats-new/g95-windows-download/
! Note to myslef:
	! > g95 main.f95 -c ! -c -> without exe
	! > g95 ww.f95 -c 
	! > g95 main.o ww.o -o wireworld
	! and now executing with input file for example OR gate: > wireworld or


program wireworld
	use ww
	implicit none
 	
 	character, allocatable, dimension(:, :) :: world
    character(len = 64) :: filePath

	! Get the file name from the function arguments(it's the first one).
  	CALL getarg(1, filePath)

  	! Read the world from the file.
	call readWorldFromFile(world, filePath)

	! Execute the rules.
	call executeRules(world)	

	! Free the world memory.
	deallocate(world)

end program wireworld