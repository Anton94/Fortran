!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!          Anton Dudov           !
!            1612555			 !
! Tools, homework 3, exercise 10 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! I used g95 compiler for Windows - http://www.fortran.com/the-fortran-company-homepage/whats-new/g95-windows-download/

! impicit none

program wireworld
	
	! Counter for tail neighbours.
	integer :: numberOfTailsInNeighborhood,i ,j, x, y	
	
	! The matrix for Clock
!	integer, parameter :: ROWS = 5
!	integer, parameter :: COLS = 9
!	character, dimension(5, 9) :: world
!	character(45) :: ww = '###########TH###### ##     ##  ##############'
!	character, dimension(45) :: www
!	forall (i=1:len(ww)) www(i) = ww(i:i)
!	world = transpose(reshape(www, (/9,5/)))

	! The matrix for OR-gate
!	integer, parameter :: ROWS = 7
!	integer, parameter :: COLS = 24
! 	character, dimension(7, 24) :: world
! 	character(24) :: w1 = '########################'
! 	character(24) :: w2 = '##TH###### ######HT   ##'
! 	character(24) :: w3 = '# ##             ##### #'
! 	character(24) :: w4 = '##  ###### ######     ##'
! 	character(24) :: w5 = '########## #############'
! 	character(24) :: w6 = '########## #############'
! 	character(24) :: w7 = '########## #############'
! 	character(168) :: ww
! 	character, dimension(168) :: www
! 	ww = w1 // w2 // w3 // w4 // w5 // w6 // w7
! 	forall (i=1:len(ww)) www(i) = ww(i:i)
! 	world = transpose(reshape(www, (/24,7/)))


	! The matrix for XOR-gate
	integer, parameter :: ROWS = 9
	integer, parameter :: COLS = 27
 	character, dimension(9, 27) :: world
 	character(27) :: w1 = '###########################'
 	character(27) :: w2 = '##TH #####   ##### HT    ##'
 	character(27) :: w3 = '# ###      #      ####### #'
 	character(27) :: w4 = '##   ##### # #####       ##'
 	character(27) :: w5 = '##########   ##############'
 	character(27) :: w6 = '########### ###############'
 	character(27) :: w7 = '########### ###############'
 	character(27) :: w8 = '########### ###############'
 	character(27) :: w9 = '########### ###############'
 	character(243) :: ww
 	character, dimension(243) :: www
 	ww = w1 // w2 // w3 // w4 // w5 // w6 // w7 // w8 // w9
 	forall (i=1:len(ww)) www(i) = ww(i:i)
 	world = transpose(reshape(www, (/27,9/)))

	do
		! Clear the last world
		!call system('clear') ! For Linux
		call system('cls') ! For Windows

		! Print the matrix (world)
		do i = 1, ROWS
			do j = 1, COLS
				write (*, "(A)",advance="no"), world(i, j)
			enddo
			write (*, *), ""
		enddo
		
		! Applying step one - All tails (T) are marked, become (X)
		! Applying step two - All heads (H) become (T) 
		! With elsewhere so it's simultaneously
		WHERE (world == 'T') 
			world = 'X' 		
		elsewhere (world == 'H') 
			world = 'T' 	
		endwhere

		! Applying step three - conductors (blank) become heads (H) if one or two cells in the 8-cell
		! neighborhood are now tails (T)
		! A border of non-conductive cells is always present
		! around the whole area, so that no border-checking needed.
		do i = 2, ROWS - 1
			do j = 2, COLS - 1	
				! Determinate how many (T)ails are in the current cell`s 8-cell neighborhood
				if (world(i, j) == ' ') then
					numberOfTailsInNeighborhood = 0
					do x = i - 1, i + 1
						do y = j - 1, j + 1
							! for x==i AND y==j I know that this cell(current one) is blank, so no need for checking
							if (world(x, y) == 'T') then
								numberOfTailsInNeighborhood = numberOfTailsInNeighborhood + 1
							endif
						enddo
					enddo

					if (numberOfTailsInNeighborhood == 1 .or. numberOfTailsInNeighborhood == 2) &
						world(i, j) = 'H'
				endif
			enddo
		enddo

		! Sleep for 1 second
		call sleep(1)
		! Applying step four - All marked tails (X) become conductors (blank)
		WHERE (world == 'X') world = ' '
	enddo

end program wireworld