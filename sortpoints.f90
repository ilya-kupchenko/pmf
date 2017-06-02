module sortpoints
use datatypes
implicit none


contains

	subroutine bubblesort(rcstep, npoints)

		type(window), intent(inout) :: rcstep
		integer, intent(in) :: npoints

		integer :: lsup
		integer :: j
		integer :: bubble

		real :: temp

		lsup = npoints

		do while (lsup > 1)
		bubble = 0 !bubble in the greatest element out of order
		do j = 1, (lsup-1)
			if (rcstep%rcpoint(j) > rcstep%rcpoint(j+1)) then
				temp = rcstep%rcpoint(j)
				rcstep%rcpoint(j) = rcstep%rcpoint(j+1)
				rcstep%rcpoint(j+1) = temp
				bubble = j
			endif 
		enddo
		lsup = bubble   
		enddo   
		

	end subroutine bubblesort

end module sortpoints
