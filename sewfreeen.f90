module sewfreeen
use datatypes
use linleastsqr
implicit none

contains

	subroutine sewfreepoints(rcstep, nrcsteps, nfreebins)

		integer, intent(in) :: nrcsteps
		integer, intent(in) :: nfreebins

		type(window), intent(inout), dimension (nrcsteps) :: rcstep


		integer :: i, j, k
		integer :: allstat
		integer :: counter
		integer :: nfreepoint = 0

		real :: deltafree12
		real :: p1freemid
		real :: p2freemid

		rcstep(1)%leftmid = rcstep(1)%rcleft
		rcstep(1)%rightmid = 0.5 * (rcstep(1)%rcright + rcstep(2)%rcleft)
		rcstep(nrcsteps)%leftmid = 0.5 * (rcstep(nrcsteps)%rcleft + &
		rcstep(nrcsteps - 1)%rcright)
		rcstep(nrcsteps)%rightmid = rcstep(nrcsteps)%rcright
		write (*,*) ' HEY: ' , nrcsteps, rcstep(nrcsteps)%rcright
		do i = 2, nrcsteps - 1
			rcstep(i)%leftmid = rcstep(i-1)%rightmid
			rcstep(i)%rightmid = 0.5 * (rcstep(i+1)%rcleft + rcstep(i)%rcright)
		end do

		do i = 1, nrcsteps

			call linintersect(rcstep(i), nfreebins)

		end do

		deltafree12 = 0.0 

		do i = 1, nrcsteps - 1

			p1freemid = rcstep(i)%blinpar * rcstep(i)%rightmid + rcstep(i)%alinpar
			p2freemid = rcstep(i+1)%blinpar * rcstep(i+1)%leftmid + rcstep(i+1)%alinpar
			deltafree12 = deltafree12 + p2freemid - p1freemid

			write (*,*) 'Freemid1: ', p1freemid, ' Freemid2: ', p2freemid, ' Delta: ', deltafree12

			write (*,*) p1freemid, p2freemid, deltafree12

			do j = 1, nfreebins
				
				rcstep(i+1)%freebin(j)%free = rcstep(i+1)%freebin(j)%free - deltafree12
				

			end do

		end do

	end subroutine sewfreepoints

end module sewfreeen
