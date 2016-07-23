module  ww
	implicit none

contains
	subroutine executeRules(world)
		implicit none
		character, allocatable, dimension(:, :) :: world
		
		integer :: i, j

		! Execute the rules.
		do
			! Clear the last world
			call system('clear') ! For Linux
			!call system('cls') ! For Windows

			! Print the matrix (world)
			do i = 1, size(world, 1)
				do j = 1, size(world, 2)
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
			do i = 2, size(world, 1) - 1
				do j = 2, size(world, 2) - 1
					! If the cell is blank - apply rule 3.
					if (world(i, j) == ' ')  world(i, j) = rule3(world, i, j)	
				enddo
			enddo

			! Sleep for 1 second
			call sleep(1)
			! Applying step four - All marked tails (X) become conductors (blank)
			WHERE (world == 'X') world = ' '
		enddo
	end subroutine executeRules

	! Applying rule three - conductors (blank) become heads (H) if one or two cells in the 8-cell
	! neighborhood are now tails (T)
	! A border of non-conductive cells is always present
	! around the whole area, so that no border-checking needed.
	function rule3(world, i, j) result(symbol)
		implicit none
		character, allocatable, dimension(:, :), intent(in) :: world
		integer, intent(in) :: i, j
		character :: symbol
		integer :: x, y, numberOfTailsInNeighborhood

		numberOfTailsInNeighborhood = 0
		symbol = ' '

		! Determinate how many (T)ails are in the current cell`s 8-cell neighborhood
		do x = i - 1, i + 1
			do y = j - 1, j + 1
				! for x==i AND y==j I know that this cell(current one) is blank, so no need for checking
				if (world(x, y) == 'T') then
					numberOfTailsInNeighborhood = numberOfTailsInNeighborhood + 1
				endif
			enddo
		enddo

		if (numberOfTailsInNeighborhood == 1 .or. numberOfTailsInNeighborhood == 2) &
			symbol = 'H'
	end function rule3

	! Reads the content of the file and fills it to the given unallocated allocatable 2D array.
	subroutine readWorldFromFile(world, path)
		implicit none
		character, allocatable, dimension(:, :) :: world
		character(len = *), intent(in) :: path

		integer :: readstat, rows, cols, i, j, counter
		character, allocatable, dimension(:) :: worldBuffer
		character(len = 100) buff
		
		! Open the file
		open(unit = 11, file = path, status = "old", action = "read")

		! Read the number of rows and columns.
		read (11, *, iostat = readstat) rows, cols
		if (readstat /= 0) return 

		! Allocate the memory. Transposed, so I can write the row from file in the column.
		! After that I will transpose it back.
		allocate(world(rows, cols))
		! Allocate and the buffer size.
		allocate(worldBuffer(rows * cols))

		! Read the file line by line and write it to the world.
		i = 1
		counter = 1
		do
			! Read a line
			read (11, '(A)', iostat = readstat) buff
			if (readstat /= 0) return

			j = 1
			do
				worldBuffer(counter) = buff(j:j)
				j = j + 1
				counter = counter + 1
				if (j > cols) exit
			end do

			i = i + 1
			if (i > rows) exit
		end do

		! Transpose the world matrix.
		world = transpose(reshape(worldBuffer, (/cols, rows/)))

		! Free the buffer memory.
		deallocate(worldBuffer)

		! Close the file.
		close(unit = 11)
	end subroutine readWorldFromFile

end module ww