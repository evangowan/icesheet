!	copyright Evan. J. Gowan, 2013, 2016
!
!	This file is part of ICESHEET 1.0
!
!	ICESHEET is free software: you can redistribute it and/or modify
!	it under the terms of the GNU General Public License as published by
!	the Free Software Foundation, version 3 of the License
!
!	ICESHEET is distributed in the hope that it will be useful,
!	but WITHOUT ANY WARRANTY; without even the implied warranty of
!	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!	GNU General Public License for more details.
!
!	You should have received a copy of the GNU General Public License
!	along with ICESHEET.  If not, see <http://www.gnu.org/licenses/>.

module bicubic

! bicubic interpolation paramaters, taken from wikipedia
	integer, parameter, dimension(16,16) :: alpha_parameters =	reshape((/1, 0, -3, 2, 0, 0, 0, 0, -3, 0, 9, -6, 2, 0, -6, &
		4, 0, 0, 3, -2, 0, 0, 0, 0, 0, 0, -9, 6, 0, 0, 6, -4, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, -9, 6, -2, 0, 6, -4, 0, 0, 0, 0, &
		0, 0, 0, 0, 0, 0, 9, -6, 0, 0, -6, 4, 0, 1, -2, 1, 0, 0, 0, 0, 0, -3, 6, -3, 0, 2, -4, 2, 0, 0, -1, 1, 0, 0, 0, 0, &
		0, 0, 3, -3, 0, 0, -2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, -6, 3, 0, -2, 4, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, 3, 0,&
		0, 2, -2, 0, 0, 0, 0, 1, 0, -3, 2, -2, 0, 6, -4, 1, 0, -3, 2, 0, 0, 0, 0, 0, 0, 3, -2, 0, 0, -6, 4, 0, 0, 3, -2, 0,&
		0, 0, 0, 0, 0, 0, 0, -1, 0, 3, -2, 1, 0, -3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, 2, 0, 0, 3, -2, 0, 0, 0, 0, 0, 1,&
		-2, 1, 0, -2, 4, -2, 0, 1, -2, 1, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 2, -2, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1,&
		2, -1, 0, 1, -2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, -1, 1/), (/16,16/))


! storage of the x and y value arrays
	double precision, allocatable, dimension(:) :: x_values, y_values

! storage of the elevation values

	double precision, allocatable, dimension(:,:) :: elevation_values

! if elevation cannot be given for a point, this value is used

	double precision, parameter :: noval=-9999999.

! size of the arrays

	integer :: x_count, y_count

! grid resolution

	integer :: grid_spacing

! actual x and y spacing

	double precision :: dx, dy

! average of that, for use in the cubic interpolation

	double precision :: bi_grid_spacing


contains

subroutine bicubic_alpha(in_array, bi_alpha_array)

! I just used the algorithm derived from Wikipedia. Seems to work well. http://en.wikipedia.org/wiki/Bicubic_interpolation

	implicit none

	double precision, dimension(6,6), intent(in) :: in_array

	double precision, dimension(16), intent(out) :: bi_alpha_array
	double precision, dimension(16) :: f_array
	double precision, dimension(6,6) :: x_der, y_der, xy_der
	integer :: x_counter, y_counter, counter
	do x_counter = 2, 5
		do y_counter = 2, 5

			y_der(x_counter, y_counter) = ((in_array(x_counter+1,y_counter+1) + &
				2.d0*in_array(x_counter,y_counter+1) + in_array(x_counter-1,y_counter+1)) - &
				(in_array(x_counter+1,y_counter-1) + 2.d0*in_array(x_counter,y_counter-1) + &
				in_array(x_counter-1,y_counter-1)))/(8.d0*bi_grid_spacing) 

		end do
	end do

	! calculate the x and xy derivative

	do x_counter = 3, 4
		do y_counter = 3, 4

			x_der(x_counter, y_counter) = ((in_array(x_counter+1,y_counter+1) + &
				2.d0*in_array(x_counter+1,y_counter) + in_array(x_counter+1,y_counter-1)) - &
				(in_array(x_counter-1,y_counter+1) + 2.d0*in_array(x_counter-1,y_counter) + &
				in_array(x_counter-1,y_counter-1)))/(8.d0*bi_grid_spacing)

			xy_der(x_counter, y_counter) = ((y_der(x_counter+1,y_counter+1) + &
				2.d0*y_der(x_counter+1,y_counter) + y_der(x_counter+1,y_counter-1)) - &
				(y_der(x_counter-1,y_counter+1) + 2.d0*y_der(x_counter-1,y_counter) + &
				y_der(x_counter-1,y_counter-1)))/(8.d0*bi_grid_spacing)

		end do
	end do


	! next determine the f values. The will be multiplied with the alpha_parameters to find the alpha values

	f_array(1) = in_array(3,3) ! f[0,0]
	f_array(2) = in_array(4,3) ! f[1,0]
	f_array(3) = in_array(3,4) ! f[0,1]
	f_array(4) = in_array(4,4) ! f[1,1]
	f_array(5) = x_der(3,3) ! fx[0,0]
	f_array(6) = x_der(4,3) ! fx[1,0]
	f_array(7) = x_der(3,4) ! fx[0,1]
	f_array(8) = x_der(4,4) ! fx[1,1]
	f_array(9) = y_der(3,3) ! fy[0,0]
	f_array(10) = y_der(4,3) ! fy[1,0]
	f_array(11) = y_der(3,4) ! fy[0,1]
	f_array(12) = y_der(4,4) ! fy[1,1]
	f_array(13) = xy_der(3,3) ! fxy[0,0]
	f_array(14) = xy_der(4,3) ! fxy[1,0]
	f_array(15) = xy_der(3,4) ! fxy[0,1]
	f_array(16) = xy_der(4,4) ! fxy[1,1]

	! find alpha


	do counter = 1, 16

		bi_alpha_array(counter) = dot_product(alpha_parameters(counter,:), f_array)

	end do
end subroutine bicubic_alpha



double precision function elevation(x_coordinate, y_coordinate)

	!this subroutine takes a given distance point (even at a decimal scale) and returns the elevation at that point.


	implicit none

	double precision, intent(in) :: x_coordinate, y_coordinate

	integer :: x_index, y_index, x_start, y_start,  x_counter, y_counter

	double precision, dimension(6,6) :: bicubic_input_array

	logical :: out_of_bounds

	double precision, dimension(16) :: alpha_array

	double precision :: x_unit, y_unit
	

!	integer :: lat_count, long_count, record_num, x_factor_l,x_factor_h, y_factor_l, y_factor_h, check_counter, counter
!	integer ::, file_number_previous

!	integer :: file_number, istat
!
!	double precision :: check_var, x_unit, y_unit, lat_c, long_c, x_c, y_c
!	real :: bathy

	! first find the nearest index, which is rounded to the bottom left

	out_of_bounds = .false.

	x_index = int((x_coordinate - x_values(1))/dx) + 1

	! check to make sure it is correct

	do

		if(x_coordinate >= x_values(x_index)  .and. x_coordinate < x_values(x_index+1)) THEN
			exit
		else

			if (x_coordinate < x_values(x_index)) THEN
				x_index = x_index - 1
			else
				x_index = x_index + 1
			endif

			if(x_index < 1 .or. x_index > x_count) THEN

				write(6,*) "x value for bicubic interpolation out of bounds"
				write(6,*) x_coordinate, x_values(1), x_values(y_count)
				stop
			endif

		endif


	end do
	

	y_index = int((y_coordinate - y_values(1))/dy) + 1

	! check to make sure it is correct

	do

		if(y_coordinate >= y_values(y_index)  .and. y_coordinate < y_values(y_index+1)) THEN
			exit
		else

			if (y_coordinate < y_values(y_index)) THEN
				y_index = y_index - 1
			else
				y_index = y_index + 1
			endif

			if(y_index < 1 .or. y_index > y_count) THEN



				write(6,*) "y value for bicubic interpolation out of bounds"

				write(6,*) y_coordinate, y_values(1), y_values(y_count)
				stop
			endif

		endif


	end do


	x_start = x_index-2 ! two points away from the bottom left
	y_start = y_index-2 ! two points away from the bottom left




	if(x_start >= 1 .and. x_start+6 <= x_count .and. y_start >= 1 .and. y_start+6 <= y_count) then ! can calculate interpolation

		bicubic_input_array = elevation_values(x_start:x_start+6,y_start:y_start+6)

		! check if any of the values are out of range

		do  x_counter = 1, 6
			do y_counter = 1,6


				if(bicubic_input_array(x_counter,y_counter) == noval) THEN
					out_of_bounds = .true.

				endif
			end do

		end do

		if(.not. out_of_bounds) THEN

			call bicubic_alpha(bicubic_input_array, alpha_array)
		

			x_unit= (x_coordinate - x_values(x_index)) / (x_values(x_index+1)-x_values(x_index))
			y_unit= (y_coordinate - y_values(y_index)) / (y_values(y_index+1)-y_values(y_index))

			elevation = alpha_array(1) + alpha_array(2)*x_unit + alpha_array(3)*x_unit**2 + alpha_array(4)*x_unit**3 &
				+ alpha_array(5)*y_unit + alpha_array(6)*y_unit*x_unit +alpha_array(7)*x_unit**2*y_unit &
				+ alpha_array(8)*x_unit**3*y_unit + alpha_array(9)*y_unit**2 &
				+ alpha_array(10)*x_unit*y_unit**2 + alpha_array(11)*x_unit**2*y_unit**2 &
				+ alpha_array(12)*x_unit**3*y_unit**2 + alpha_array(13)*y_unit**3 &
				+ alpha_array(14)*x_unit*y_unit**3 + alpha_array(15)*x_unit**2*y_unit**3 &
				+ alpha_array(16)*x_unit**3*y_unit**3

		endif



	else
		out_of_bounds = .true.

	endif


	if (out_of_bounds) then
		elevation = noval
	endif




end function elevation


end module bicubic
