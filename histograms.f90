module histograms
use datatypes
use sortpoints
implicit none

contains

	subroutine allothistograms(rcstep, nrcsteps, nfreebins)

		integer, intent(in) :: nrcsteps
		integer, intent(in) :: nfreebins

		type(window), intent(inout), dimension (nrcsteps) :: rcstep

		integer :: i, j, k
		integer :: npointsperbin

		real :: prcpoint
		real :: pk
		real :: pr0
		real :: prcleft
		real :: prcright
		real :: pdrc

		do i = 1, nrcsteps

			call bubblesort(rcstep(i), rcstep(i)%npoints)
		
			npointsperbin = nint(real(rcstep(i)%npoints) / real(nfreebins))

			if (npointsperbin < 5) then
				err_stat = .true.
				err_id = 8
			end if

			do k = 1, nfreebins

				prcleft = rcstep(i)%rcleft
				prcright = rcstep(i)%rcright
				pdrc = (rcstep(i)%rcright - rcstep(i)%rcleft) / nfreebins

				rcstep(i)%freebin(k)%rcleft = prcleft + (k - 1) * pdrc
				rcstep(i)%freebin(k)%rcright =  prcleft + k * pdrc

				do j = 1, rcstep(i)%npoints

					if (rcstep(i)%rcpoint(j) >= rcstep(i)%freebin(k)%rcleft .and. &
					rcstep(i)%rcpoint(j) < rcstep(i)%freebin(k)%rcright) then
					
						rcstep(i)%freebin(k)%npoints = &
						rcstep(i)%freebin(k)%npoints + 1
						rcstep(i)%freebin(k)%rcbin = &
                                                rcstep(i)%freebin(k)%rcbin + rcstep(i)%rcpoint(j)
					end if

				end do

				if (rcstep(i)%freebin(k)%npoints == 0) then

					rcstep(i)%freebin(k)%npoints = rcstep(i)%freebin(k)%npoints &
					+ 1
					rcstep(i)%npoints = rcstep(i)%npoints + 1

				end if

				rcstep(i)%freebin(k)%probability = real(rcstep(i)%freebin(k)%npoints) / &
				real(rcstep(i)%npoints)

				rcstep(i)%freebin(k)%rcbin = rcstep(i)%freebin(k)%rcbin / &
				real(rcstep(i)%freebin(k)%npoints)

				rcstep(i)%freebin(k)%ebias = rcstep(i)%kconstraint * ( &
				rcstep(i)%freebin(k)%rcbin - rcstep(i)%r0) ** 2
				
				rcstep(i)%freebin(k)%free = - kt * log(rcstep(i)%freebin(k)%probability) - &
				rcstep(i)%freebin(k)%ebias

			end do

		end do


	end subroutine allothistograms

end module histograms
