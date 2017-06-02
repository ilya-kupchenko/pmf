module datatypes
implicit none

real, parameter :: kt = 0.593
real, parameter :: beta = 1.686

logical :: err_stat = .false.
integer :: err_id

type freebox	! Class for a single bin inside of trajectory
	integer :: npoints = 0		! Number of RC points
	real :: rcleft			! Left border
	real :: rcright			! Right border
	real :: ebias = 0.0		! Mean biasing energy of the bin
	real :: probability =0.0	! Probability (eq. N_bin/Sum(N_bin))
	real :: rcbin = 0.0		! Mean RC value of the bin
	real :: free = 0.0		! Unbiased free energy of the bin

	logical :: included = .false.
end type freebox

type window	! Class for a single trajectory
	integer :: npoints = 0		! Number of collected points

	real :: kconstraint = 0.0	! Constraint value
	real :: r0 = 0.0		! Constriant equilibrium RC 
	real :: rcleft			! Minimum RC value
	real :: rcright			! Maximum RC value

	real :: leftmid		! Max RC of the i-1 window
	real :: rightmid	! Max RC of the i+1 
	real :: alinpar = 0.0		! Least squares a
	real :: blinpar = 0.0		! Least squares b
	
	real, allocatable, dimension (:) :: rcpoint		! RC points
	real, allocatable, dimension (:) :: epoint		! Energies of points
	type(freebox), allocatable, dimension (:) :: freebin	! Bins of RC histograms
end type window


end module datatypes
