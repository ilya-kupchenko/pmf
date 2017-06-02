program pmf
use readpoints
use histograms
use sewfreeen
use plotfree
implicit none

character(len=2048) :: rcdatafile = "./rsts-w-h10-gm60-k100-l-nch.dat"

integer :: i
integer :: j
integer :: k

integer :: nrcsteps = 0
integer :: nfreepoints = 0
integer :: nfreebins = 20

real, allocatable, dimension (:, :) :: freepoint
type(window), allocatable, dimension (:) :: rcstep


write (*,*) 'YES'

call readdatafile(rcdatafile, nfreebins, rcstep, nrcsteps)

call allothistograms(rcstep, nrcsteps, nfreebins)

call sewfreepoints(rcstep, nrcsteps, nfreebins)

call plotfreecurve(rcstep, nrcsteps, nfreebins, nfreepoints, freepoint)

!call errorcatcher()

do k = 1, nrcsteps

	write (*,*) 'Step: ', k
	do i = 1, nfreebins
		write (*,*) rcstep(k)%freebin(i)%rcbin, rcstep(k)%freebin(i)%probability, &
		rcstep(k)%freebin(i)%ebias, rcstep(k)%freebin(i)%free
	end do

end do

write (*,*) '1l: ', rcstep(1)%rcleft, ' 1r: ', rcstep(1)%rcright, &
	' 1lm: ', rcstep(1)%leftmid, ' 1rm: ', rcstep(1)%rightmid, &
	'2l: ', rcstep(2)%rcleft, ' 2r: ', rcstep(2)%rcright, &
        ' 2lm: ', rcstep(2)%leftmid, ' 2rm: ', rcstep(2)%rightmid

write(*,*) 'Final plot:', nfreepoints

do k = 1, nfreepoints

	write (*,*) freepoint(k, 1), freepoint(k, 2)
	
end do

end
