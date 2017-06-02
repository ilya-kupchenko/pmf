module errorcatch
use datatypes
implicit none

type errorrec ::

	integer :: error_id
	character :: error_text

end type errorrec

contains

	subroutine errorcatcher(err_stat, err_code)

		logical, intent(in) :: err_stat
		integer, intent(in) :: err_code

	end subroutine errorcatcher

end module errorcatch
