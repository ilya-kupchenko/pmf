module linleastsqr
use datatypes

implicit none

contains

	subroutine linintersect(step, nfreebins)

		type(window), intent(inout) :: step

		integer, intent(in) :: nfreebins
		integer :: i
		
		real :: prc = 0.0
		real :: pfree = 0.0
		real :: meanfree = 0.0
		real :: meanrc = 0.0
		real :: meanfreerc = 0.0
		real :: meanrc2 = 0.0
		
		if(step%alinpar == 0.0 .and. step%blinpar == 0.0) then

			do i = 1, nfreebins

				prc = step%freebin(i)%rcbin
				pfree = step%freebin(i)%free

				meanfree = meanfree + pfree
				meanrc = meanrc + prc
				meanfreerc = meanfreerc + prc * pfree
				meanrc2 = meanrc2 + prc * prc
			end do


		meanfree = meanfree / nfreebins
		write (*,*) 'meanfree : ', meanfree
		meanrc = meanrc / nfreebins
		write (*,*) 'meanrc : ', meanrc
		meanfreerc = meanfreerc / nfreebins
		write (*,*) 'meanfreerc : ', meanfreerc
		meanrc2 = meanrc2 / nfreebins
		write (*,*) 'meanrc2 : ', meanrc2

		step%blinpar = (meanfreerc - meanrc * meanfree) / &
		(meanrc2 - meanrc * meanrc)
		write (*,*) 'b1 : ', step%blinpar

		step%alinpar = meanfree - step%blinpar * meanrc
		write (*,*) 'a1 : ', step%alinpar

		end if

		meanfree = 0.0
                meanrc = 0.0
                meanfreerc = 0.0
                meanrc2 = 0.0
	end subroutine linintersect

end module linleastsqr
