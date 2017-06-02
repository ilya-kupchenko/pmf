module plotfree
use datatypes
implicit none

contains

	subroutine plotfreecurve(rcstep, nrcsteps, nfreebins, nfreepoints, freepoint)

		integer, intent(in) :: nrcsteps
		integer, intent(in) :: nfreebins
		integer, intent(out) :: nfreepoints

		type(window), intent(in), dimension (nrcsteps) :: rcstep
		real, allocatable, intent(out), dimension (:, :) :: freepoint

		integer :: i, j
		integer :: counter = 0
		integer :: allstat

		nfreepoints = 0

		do i = 1, nrcsteps

			do j = 1, nfreebins

				if (rcstep(i)%freebin(j)%rcbin > rcstep(i)%leftmid .and. &
				rcstep(i)%freebin(j)%rcbin < rcstep(i)%rightmid) then

					nfreepoints = nfreepoints + 1


				end if

			end do

		end do

		allocate(freepoint(nfreepoints, 2), stat=allstat)

                if (allstat /= 0) then
                        err_stat = .true.
                        err_id = 9
                end if

		do i = 1, nrcsteps

                        do j = 1, nfreebins

                                if (rcstep(i)%freebin(j)%rcbin > rcstep(i)%leftmid .and. &
                                rcstep(i)%freebin(j)%rcbin < rcstep(i)%rightmid) then

					counter = counter + 1

					freepoint(counter, 1) = rcstep(i)%freebin(j)%rcbin
					freepoint(counter, 2) = rcstep(i)%freebin(j)%free

                                end if

                        end do

                end do

	end subroutine plotfreecurve

end module plotfree
