module readpoints
use datatypes
implicit none

contains

	subroutine readdatafile(rcdata, nfreebins, rcstep, nrcsteps)

		character(len=2048), intent(in) :: rcdata
		integer, intent(in) :: nfreebins
		type(window), allocatable, intent(out), dimension (:) :: rcstep
		integer, intent(out) :: nrcsteps

		integer :: dataunit = 2
		integer :: opstat 
		integer :: rdstat
		integer :: allstat

		integer :: i, j, k

		real :: tmp1, tmp2, tmp3
		real :: prcpoint
		real :: pdrc
		real :: prcleft
		real :: prcright
		real :: pk
		real :: pr0


		open(unit=dataunit, file=rcdata, action='READ', status='OLD', iostat=opstat)

		if (opstat /= 0) then
			err_stat = .true.
			err_id = 1
			return
		end if

		read (dataunit, *, iostat=rdstat) nrcsteps, tmp1

		if (rdstat > 0) then
			err_stat = .true.
			err_id = 1
			return
		end if

		allocate(rcstep(nrcsteps), stat=allstat)

		if (allstat /= 0) then
			err_stat = .true.
			err_id = 1
			return
		end if

		write (*,1000) nrcsteps
		1000 format(/, 'Number of windows: ', I10)

		do  i = 1, nrcsteps
			
			read (dataunit, *, iostat=rdstat)	rcstep(i)%npoints, &
								rcstep(i)%kconstraint, &
								rcstep(i)%r0
			if (rdstat > 0) then
				err_stat = .true.
				err_id = 4
			end if

			allocate(rcstep(i)%rcpoint(rcstep(i)%npoints), stat=allstat)

			if (allstat /= 0) then
				err_stat = .true.
				err_id = 4
			end if
		
			allocate(rcstep(i)%freebin(nfreebins), stat=allstat)

                        if (allstat /= 0) then
                                err_stat = .true.
                                err_id = 4
                        end if

			do j = 1, nfreebins

				rcstep(i)%freebin(j)%npoints = 0
				rcstep(i)%freebin(j)%ebias = 0.0
				rcstep(i)%freebin(j)%rcleft = 0.0
				rcstep(i)%freebin(j)%rcright = 0.0
				rcstep(i)%freebin(j)%probability = 0.0
				rcstep(i)%freebin(j)%rcbin = 0.0
				rcstep(i)%freebin(j)%free = 0.0

			end do

			do j = 1, rcstep(i)%npoints
				read (dataunit, *, iostat=rdstat)	tmp1, tmp2, tmp3, &
									prcpoint
				rcstep(i)%rcpoint(j)=prcpoint

				if (rdstat /= 0) then
					err_stat = .true.
					err_id = 4
				end if

				if (j == 1) then
					rcstep(i)%rcleft = prcpoint
					rcstep(i)%rcright = prcpoint
				else
					if (rcstep(i)%rcleft > prcpoint) then
						rcstep(i)%rcleft = prcpoint
					endif
					
					if (rcstep(i)%rcright < prcpoint) then
						rcstep(i)%rcright = prcpoint
					endif
				endif
			end do
		end do

	end subroutine readdatafile

end module readpoints
