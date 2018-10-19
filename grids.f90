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


! module for handling the grids

! the values are calculated via bicubic interpolation. Without having a smooth surface function, the numerical integration is doomed to failure!

module grids
 
	implicit none


	! global elevation parameters

	integer, allocatable, save, dimension(:) :: elev_x_min, elev_x_max, elev_y_min, elev_y_max
	integer, save :: elev_xmin, elev_xstep, elev_xmax, elev_ymin, elev_ystep, elev_ymax


	integer, save :: elev_grid_spacing ! the grid space of the elevation files
	integer, parameter :: elev_ncunit = 11 ! make sure the file unit for this is not used elsewhere

	! save variables so that it doesn't recalculate the same thing twice in a row.

	integer, dimension(6) :: elev_x_points, elev_y_points
	double precision, dimension(16) :: elev_alpha_array


	character(len=120) :: elevation_parameters_filename, elevation_file


	! global shear stress parameters


	integer, allocatable, save, dimension(:) :: ss_x_min, ss_x_max, ss_y_min, ss_y_max
	integer, save :: ss_xmin, ss_xstep, ss_xmax, ss_ymin, ss_ystep, ss_ymax


	integer, save :: ss_grid_spacing ! the grid space of the elevation files
	integer, parameter :: ss_ncunit = 12 ! make sure the file unit for this is not used elsewhere

	! save variables so that it doesn't recalculate the same thing twice in a row. Probably mostly relevant once I get a coarser elevation grid

	integer, dimension(6) :: ss_x_points, ss_y_points
	double precision, dimension(16) :: ss_alpha_array


	character(len=120) :: shear_stress_parameters_filename, shear_stress_file

	integer, parameter :: header_offset = 896 / 4 ! header size of a standard GMT binary file


	! settings for whether or not you want to store the DEMs in memory or read from file

	logical, parameter :: store_dem = .true.

	real, allocatable, dimension(:) :: ss_grid_store, elev_grid_store


! bicubic interpolation paramaters
	integer, parameter, dimension(16,16) :: alpha_parameters =	reshape((/1, 0, -3, 2, 0, 0, 0, 0, -3, 0, 9, -6, 2, 0, -6, &
		4, 0, 0, 3, -2, 0, 0, 0, 0, 0, 0, -9, 6, 0, 0, 6, -4, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, -9, 6, -2, 0, 6, -4, 0, 0, 0, 0, &
		0, 0, 0, 0, 0, 0, 9, -6, 0, 0, -6, 4, 0, 1, -2, 1, 0, 0, 0, 0, 0, -3, 6, -3, 0, 2, -4, 2, 0, 0, -1, 1, 0, 0, 0, 0, &
		0, 0, 3, -3, 0, 0, -2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, -6, 3, 0, -2, 4, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, 3, 0,&
		0, 2, -2, 0, 0, 0, 0, 1, 0, -3, 2, -2, 0, 6, -4, 1, 0, -3, 2, 0, 0, 0, 0, 0, 0, 3, -2, 0, 0, -6, 4, 0, 0, 3, -2, 0,&
		0, 0, 0, 0, 0, 0, 0, -1, 0, 3, -2, 1, 0, -3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, 2, 0, 0, 3, -2, 0, 0, 0, 0, 0, 1,&
		-2, 1, 0, -2, 4, -2, 0, 1, -2, 1, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 2, -2, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1,&
		2, -1, 0, 1, -2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, -1, 1/), (/16,16/))


contains


subroutine read_elevation_files()

	! this subroutine must be run first to initialize the program to read elevation data

	implicit none

	integer :: number_elev_points, istat, record_number, file_record
	real :: grid_value

	open(unit=20, file=elevation_parameters_filename, form="formatted", access="sequential", status="old")

	read(20,'(A)') elevation_file

	! x and y limits for the grid, needs to be integers


	read(20,*) elev_xmin 
	read(20,*) elev_xmax 
	read(20,*) elev_ymin
	read(20,*) elev_ymax

	! grid spacing, again must be an integer

	read(20,*) elev_grid_spacing

	close(unit=20)

	! calculates the step size for the grid file

	elev_xstep = elev_xmax - elev_xmin + elev_grid_spacing
	elev_ystep = elev_ymax-elev_ymin + elev_grid_spacing

	write(6,*) "x and y step size:", elev_xstep, elev_ystep
	number_elev_points = ((elev_xmax-elev_xmin) / elev_grid_spacing)*((elev_ymax-elev_ymin)/elev_grid_spacing)
	write(6,*) "number of points in elev file:", number_elev_points

	! The elevation file is left open while the program is running. the subroutine "end_elev" should be called at the end to close the file

	! the elevation file is a binary file that contains elevation using four byte real numbers. 
	! This file should be in a GMT style binary file, which has a 896 byte header (the program takes into account the header)

	open(file=elevation_file, unit=elev_ncunit, access="direct", form='unformatted', action="read", RECL=4) 

	if(store_dem) THEN

		allocate(elev_grid_store(number_elev_points))

		do record_number = 1, number_elev_points, 1
			file_record = record_number + header_offset
			read(elev_ncunit, rec=file_record, iostat=istat) grid_value
			if(istat /=0) THEN ! out of bounds
				write(6,*) "elevation grid out of bounds during initial read"
				stop
			end if

			elev_grid_store(record_number) = grid_value
		end do

	endif

	! set the storage to be a small value.  The storage means that the program doesn't constantly have to recalculate the bicubic alpha parameters.
	elev_x_points = -99999999
	elev_y_points = -99999999

	write(6,*) elev_grid_spacing, elev_xstep,elev_ystep

end subroutine read_elevation_files

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine end_elev()

! call this at the end of the program to shut off the opened file unit
	implicit none

	close (unit=elev_ncunit)

	if(store_dem) THEN

		deallocate(elev_grid_store)
	end if

end subroutine end_elev

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine read_ss_files()

	! this subroutine must be run first to initialize the program to read elevation data

	implicit none

	integer :: number_ss_points, istat, record_number, file_record
	real :: grid_value

	open(unit=20, file=shear_stress_parameters_filename, form="formatted", access="sequential", status="old")

	read(20,'(A)') shear_stress_file

	! x and y limits for the grid, needs to be integers


	read(20,*) ss_xmin 
	read(20,*) ss_xmax 
	read(20,*) ss_ymin
	read(20,*) ss_ymax

	! grid spacing, again must be an integer

	read(20,*) ss_grid_spacing

	close(unit=20)

	! calculates the step size for the grid file

	ss_xstep = ss_xmax - ss_xmin + ss_grid_spacing
	ss_ystep = ss_ymax-ss_ymin + ss_grid_spacing
	write(6,*) "x and y step size:", ss_xstep, ss_ystep




	number_ss_points =  ((ss_xmax-ss_xmin) / ss_grid_spacing) * ((ss_ymax-ss_ymin)/ss_grid_spacing)
	write(6,*) "number of points in ss file:", number_ss_points

	! The shear stress file is left open while the program is running. the subroutine "end_ss" should be called at the end to close the file

	! the shear stress file is a binary file that contains elevation using four byte real numbers. 
	! This file should be in a GMT style binary file, which has a 896 byte header (the program takes into account the header)

	open(file=shear_stress_file, unit=ss_ncunit, access="direct", form='unformatted', action="read", RECL=4) 

	if(store_dem) THEN

		allocate(ss_grid_store(number_ss_points))

		do record_number = 1, number_ss_points, 1
			file_record = record_number+header_offset
			read(ss_ncunit, rec=file_record, iostat=istat) grid_value
			if(istat /=0) THEN ! out of bounds
				write(6,*) "elevation grid out of bounds during initial read"
				stop
			end if

			ss_grid_store(record_number) = grid_value
		end do

	endif


	! set the storage to be a small value. The storage means that the program doesn't constantly have to recalculate the bicubic alpha parameters.
	ss_x_points = -99999999
	ss_y_points = -99999999

	write(6,*) ss_grid_spacing, ss_xstep,ss_ystep

end subroutine read_ss_files

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine end_ss()

! call this at the end of the program to shut off the opened file unit
	implicit none

	close (unit=ss_ncunit)

	if(store_dem) THEN

		deallocate(ss_grid_store)
	end if

end subroutine end_ss


!!!!!!!!!!!!!!!!!!!!!!!!!!



! this particular version of the elevation subroutine gets the exact elevation of any point on the earth's surface, given a particular distance

! this function find elevation using a bicubic interpolation method that is found on Wikipedia. Hopefully this solves the dicontinuous elevation derivatives

double precision function get_tau(x_coordinate, y_coordinate)

	IMPLICIT NONE

	double precision, intent(in) :: x_coordinate, y_coordinate
	double precision :: tau_value

	 call get_grid_value(x_coordinate, y_coordinate, ss_ncunit, ss_grid_spacing, ss_x_points, ss_y_points, ss_xstep, &
		ss_ystep, ss_xmin, ss_ymax, ss_alpha_array, tau_value)

	get_tau = tau_value
	

end function get_tau


double precision function elevation(x_coordinate, y_coordinate)

	IMPLICIT NONE

	double precision, intent(in) :: x_coordinate, y_coordinate
	double precision :: elevation_value

	call get_grid_value(x_coordinate, y_coordinate, elev_ncunit, elev_grid_spacing, elev_x_points, elev_y_points, &
		elev_xstep, elev_ystep, elev_xmin, elev_ymax, elev_alpha_array, elevation_value)

	elevation = elevation_value

end function elevation
	
! not done yet, need to fix this:


subroutine get_grid_value(x_coordinate, y_coordinate, ncunit, grid_spacing, x_points, y_points, &
		xstep, ystep, xmin, ymax, alpha_array, result_value)

	IMPLICIT NONE

	double precision, intent(in) :: x_coordinate, y_coordinate
	integer, intent(in) :: xstep, ystep, xmin, ymax, ncunit, grid_spacing
	integer, dimension(6), intent(inout) :: x_points, y_points
	double precision, dimension(16), intent(inout) :: alpha_array
	double precision, intent(out) :: result_value

	integer :: x_start, y_start, x_counter, y_counter, record_number, istat, counter

	real :: grid_value

	double precision, dimension(6,6) :: grid_storage_array

	double precision :: x_unit, y_unit


	x_start = floor(x_coordinate/dble(grid_spacing)) * grid_spacing - grid_spacing*2 ! two points away from the bottom left
	y_start = floor(y_coordinate/dble(grid_spacing)) * grid_spacing - grid_spacing*2 ! two points away from the bottom left




	if( x_start /= x_points(1) .or. y_start /= y_points(1)) THEN ! the previous alpha parameters are stored, so that if they are the same as before, there is no need to recalculate them
		do counter = 1, 6

			x_points(counter) = x_start + grid_spacing * (counter-1)
			y_points(counter) = y_start + grid_spacing * (counter-1)

		end do


		do x_counter = 1, 6
			do y_counter = 1, 6

				! GMT apparently stores things from the top left corner of the map

				record_number =  (x_points(x_counter)-xmin)/grid_spacing + &
					((ymax -y_points(y_counter)))/grid_spacing * (xstep/grid_spacing) &
					 + header_offset

				if(record_number > 0) THEN

					if(store_dem) THEN
						record_number = record_number - header_offset
						if(ncunit == elev_ncunit) THEN
							grid_value = elev_grid_store(record_number)
						elseif(ncunit == ss_ncunit) THEN
							grid_value = ss_grid_store(record_number)
						endif

						istat = 0

					else
						read(ncunit, rec=record_number, iostat=istat) grid_value

					endif

					if(istat /=0) THEN ! happens if the value goes wildly outside of the boundary. Currently set to stop the program
						write(6,*) "istat:", istat
						write(6,*) "grid value for unit ", ncunit, " is out of bounds", record_number
						write(6,*) x_coordinate, y_coordinate
						write(6,*) x_points(x_counter), y_points(y_counter)
						write(6,*) ""

						write(6,*) elev_xmin, elev_ymin
						write(6,*) elev_xmax, elev_ymin
						write(6,*) elev_xmax, elev_ymax
						write(6,*) elev_xmin, elev_ymax
						write(6,*) elev_xmin, elev_ymin
						write(6,*) ""
						write(6,*) ss_xmin, ss_ymin
						write(6,*) ss_xmax, ss_ymin
						write(6,*) ss_xmax, ss_ymax
						write(6,*) ss_xmin, ss_ymax
						write(6,*) ss_xmin, ss_ymin

						stop
					endif

				else
					write(6,*) "grid value for unit ", ncunit, " is out of bounds", record_number
						write(6,*) x_coordinate, y_coordinate
						write(6,*) x_points(x_counter), y_points(y_counter)						
						write(6,*) ""

						write(6,*) elev_xmin, elev_ymin
						write(6,*) elev_xmax, elev_ymin
						write(6,*) elev_xmax, elev_ymax
						write(6,*) elev_xmin, elev_ymax
						write(6,*) elev_xmin, elev_ymin
						write(6,*) ""
						write(6,*) ss_xmin, ss_ymin
						write(6,*) ss_xmax, ss_ymin
						write(6,*) ss_xmax, ss_ymax
						write(6,*) ss_xmin, ss_ymax
						write(6,*) ss_xmin, ss_ymin
					stop
				endif

				grid_storage_array(x_counter, y_counter) = dble(grid_value)


			end do


		end do


		! calculate the y derivatives at 9 points rather than the four corners, since these are needed to find the cross derivatives

		call bicubic_alpha(grid_storage_array, grid_spacing, alpha_array)


	endif

	! complete the calculation

	x_unit= (x_coordinate - dble(x_points(3))) / dble(x_points(4)-x_points(3))
	y_unit= (y_coordinate - dble(y_points(3))) / dble(y_points(4)-y_points(3))

	result_value = alpha_array(1) + alpha_array(2)*x_unit + alpha_array(3)*x_unit**2 + alpha_array(4)*x_unit**3&
		+ alpha_array(5)*y_unit + alpha_array(6)*y_unit*x_unit +alpha_array(7)*x_unit**2*y_unit&
		+ alpha_array(8)*x_unit**3*y_unit + alpha_array(9)*y_unit**2 &
		+ alpha_array(10)*x_unit*y_unit**2 + alpha_array(11)*x_unit**2*y_unit**2 &
		+ alpha_array(12)*x_unit**3*y_unit**2 + alpha_array(13)*y_unit**3 &
		+ alpha_array(14)*x_unit*y_unit**3 + alpha_array(15)*x_unit**2*y_unit**3 &
		+ alpha_array(16)*x_unit**3*y_unit**3


end subroutine get_grid_value


subroutine bicubic_alpha(in_array, bi_grid_spacing, bi_alpha_array)

	! bicubic interpolation algorithm shamelessly lifted off of Wikipedia

	implicit none

	double precision, dimension(6,6), intent(in) :: in_array
	integer, intent(in) :: bi_grid_spacing
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



!!!!!!!!!!!!!!!!!!!!!!!!!!!




double precision function Hf_gradient(x, y, direction)

! this function takes in a particular x and y location, and accounting for the flowline direction, calculates the gradient with respect to flowline y
! rotation is the direction that "x_flowline" points
	use global_parameters

	implicit none

	double precision, intent(in) :: x, y, direction

	double precision :: Q11, Q12, Q21, Q22, length, x_temp, y_temp, latitude, longitude

!	open(unit=324, file="hf_gradient.out", access="sequential", form="formatted", status="replace")

	length = sqrt(2.d0 * dx_l**2)

!	write(92,*) x, y, get_tau(x, y), length

!	call flowline_location(rotation, x, y, -dx_l, -dx_l, x_temp, y_temp)
	x_temp = x + length*cos(5.d0*pi/4.d0+direction)
	y_temp = y + length*sin(5.d0*pi/4.d0+direction)


	Q11 = get_tau(x_temp, y_temp)
!	call convert_ll_3(x_temp, y_temp,latitude, longitude)
!	write(324,*) "Q11:", Q11, x_temp, y_temp,latitude, longitude

!	write(92,*) "q11", x_temp, y_temp, Q11

!	call flowline_location(rotation, x, y, -dx_l, dx_l, x_temp, y_temp)

	x_temp = x + length*cos(3.d0*pi/4.d0+direction)
	y_temp = y + length*sin(3.d0*pi/4.d0+direction)

	Q12 = get_tau(x_temp, y_temp)
!	call convert_ll_3(x_temp, y_temp,latitude, longitude)
!	write(324,*) "Q12:", Q12, x_temp, y_temp,latitude, longitude
!	write(92,*) "q12", x_temp, y_temp, Q12

!	call flowline_location(rotation, x, y, dx_l, -dx_l, x_temp, y_temp)

	x_temp = x + length*cos(-1.d0*pi/4.d0+direction)
	y_temp = y + length*sin(-1.d0*pi/4.d0+direction)

	Q21 = get_tau(x_temp, y_temp)
!	call convert_ll_3(x_temp, y_temp,latitude, longitude)
!	write(324,*) "Q21:", Q21, x_temp, y_temp,latitude, longitude
!	write(92,*) "q21", x_temp, y_temp, Q21

!	call flowline_location(rotation, x, y, dx_l, dx_l, x_temp, y_temp)

	x_temp = x + length*cos(1.d0*pi/4.d0+direction)
	y_temp = y + length*sin(1.d0*pi/4.d0+direction)

	Q22 = get_tau(x_temp, y_temp)
!	call convert_ll_3(x_temp, y_temp,latitude, longitude)
!	write(324,*) "Q22:", Q22, x_temp, y_temp,latitude, longitude
!	write(92,*) "q22", x_temp, y_temp, Q22

	Hf_gradient = (-Q11  - Q21 + Q12 + Q22) / ((4.d0 * dx_l) * rho_ice * g) 

!	call convert_ll_3(x, y,latitude, longitude)
!	write(324,*) "final:", Hf_gradient, x, y, latitude, longitude


!	close(unit=324)

!	write(92,*) "final gradient", Hf_gradient
!	write(92,*) ""

end function Hf_gradient

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function ss_finite_difference(x, y, direction)
! this function finds the finite difference of shear stress for a given point to approximate the shear stress gradient with respect to y

	use global_parameters
	implicit none

	double precision, intent(in) :: x, y, direction

	double precision :: length, x_temp, y_temp, pos_dir, neg_dir, perp_direction

	perp_direction = check_angle(direction + pi/2.d0)

	length = dx_l / 2.d0

	x_temp = x + length*cos(perp_direction)
	y_temp = y + length*sin(perp_direction)

	pos_dir = get_tau(x_temp, y_temp)

	x_temp = x - length*cos(perp_direction)
	y_temp = y - length*sin(perp_direction)

	neg_dir = get_tau(x_temp, y_temp)

	ss_finite_difference = (pos_dir - neg_dir) / (rho_ice*g)  / dx_l

end function ss_finite_difference

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function ss_second_difference(x, y, direction)
! this function finds the finite difference of shear stress for a given point to approximate the shear stress gradient with respect to y

	use global_parameters
	implicit none

	double precision, intent(in) :: x, y, direction

	double precision :: length, x_temp, y_temp, pos_dir, neg_dir, perp_direction

	perp_direction = check_angle(direction + pi/2.d0)

	length = dx_l

	x_temp = x + length*cos(perp_direction)
	y_temp = y + length*sin(perp_direction)

	pos_dir = get_tau(x_temp, y_temp)

	x_temp = x - length*cos(perp_direction)
	y_temp = y - length*sin(perp_direction)

	neg_dir = get_tau(x_temp, y_temp)

	ss_second_difference = (pos_dir - 2.d0*get_tau(x,y) + neg_dir) / length**2  / (rho_ice*g)

end function ss_second_difference

!!!!!!!!!!!!!!!!!!!!!!!!!!!


double precision function x_ss_finite_difference(x, y, direction)
! this function finds the finite difference of shear stress for a given point to approximate the shear stress gradient with respect to x

	use global_parameters
	implicit none

	double precision, intent(in) :: x, y, direction

	double precision :: length, x_temp, y_temp, pos_dir, neg_dir

	length = dx_l / 2.d0

	x_temp = x + length*cos(direction)
	y_temp = y + length*sin(direction)

	pos_dir = get_tau(x_temp, y_temp)

	x_temp = x - length*cos(direction)
	y_temp = y - length*sin(direction)

	neg_dir = get_tau(x_temp, y_temp)

	x_ss_finite_difference = (pos_dir - neg_dir) / dx_l

end function x_ss_finite_difference

!!!!!!!!!!!!!!!!!!!!!1
double precision function x_ss_second_difference(x, y, direction)
! this function finds the finite difference of shear stress gradient for a given point to approximate the shear stress gradient with respect to x and y

	use global_parameters
	implicit none

	double precision, intent(in) :: x, y, direction

	double precision :: length, x_temp, y_temp, pos_dir, neg_dir

	length = dx_l / 2.d0

	x_temp = x + length*cos(direction)
	y_temp = y + length*sin(direction)

	pos_dir = ss_finite_difference(x_temp, y_temp, direction)

	x_temp = x - length*cos(direction)
	y_temp = y - length*sin(direction)

	neg_dir = ss_finite_difference(x_temp, y_temp, direction)
	
	x_ss_second_difference = (pos_dir - neg_dir) / dx_l

end function x_ss_second_difference

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



double precision function y_gradient(x, y, direction)

! this function takes in a particular x and y location, and accounting for the flowline direction, calculates the gradient with respect to flowline y
! rotation is the direction that "x_flowline" points
	use global_parameters
	implicit none

	double precision, intent(in) :: x, y, direction

	double precision :: Q11, Q12, Q21, Q22, length, x_temp, y_temp

	length = sqrt(2.d0 * dx_l**2)
!	call flowline_location(rotation, x, y, -dx_l, -dx_l, x_temp, y_temp)
	x_temp = x + length*cos(5.d0*pi/4.d0+direction)
	y_temp = y + length*sin(5.d0*pi/4.d0+direction)


	Q11 = elevation(x_temp, y_temp)

!	call flowline_location(rotation, x, y, -dx_l, dx_l, x_temp, y_temp)

	x_temp = x + length*cos(3.d0*pi/4.d0+direction)
	y_temp = y + length*sin(3.d0*pi/4.d0+direction)

	Q12 = elevation(x_temp, y_temp)

!	call flowline_location(rotation, x, y, dx_l, -dx_l, x_temp, y_temp)

	x_temp = x + length*cos(-1.d0*pi/4.d0+direction)
	y_temp = y + length*sin(-1.d0*pi/4.d0+direction)

	Q21 = elevation(x_temp, y_temp)

!	call flowline_location(rotation, x, y, dx_l, dx_l, x_temp, y_temp)

	x_temp = x + length*cos(1.d0*pi/4.d0+direction)
	y_temp = y + length*sin(1.d0*pi/4.d0+direction)

	Q22 = elevation(x_temp, y_temp)

	y_gradient = (-Q11  - Q21 + Q12 + Q22) / (4.d0 * dx_l)

end function y_gradient

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function y_finite_difference(x, y, direction)
! this function finds the finite difference of elevation for a given point to approximate the elevation gradient with respect to y

	use global_parameters
	implicit none

	double precision, intent(in) :: x, y, direction



	double precision :: length, x_temp, y_temp, pos_dir, neg_dir, perp_direction, x_temp1, y_temp1

	double precision, dimension(3,3) :: elev_store

	integer :: i, j

	perp_direction = check_angle(direction + pi/2.d0)

!	length = dx_l 
	length = dx_l / 2.d0
!	length = 10.d0

!	write(103,*) ">"

	do i = 1, 3

		x_temp = x + dble(i-2) * length*cos(perp_direction)

		do j = 1, 3
			
			y_temp = y + dble(j-2) *length*sin(perp_direction)
	
			elev_store(i,j) = elevation(x_temp, y_temp)

!			write(103,*) i, j, x_temp, y_temp, elev_store

		end do

	end do


	! third order finite difference method, see Skidmore, 1989
	y_finite_difference = ((elev_store(3,3) + 2.d0*elev_store(2,3) + elev_store(1,3)) - (elev_store(3,1) + 2.d0*elev_store(2,1)&
		+ elev_store(1,1))) / (8.d0*length)


!	pos_dir = elevation(x_temp1, y_temp1)

!	x_temp = x - length*cos(perp_direction)
!	y_temp = y - length*sin(perp_direction)

!	neg_dir = elevation(x_temp, y_temp)

!	y_finite_difference = (pos_dir - neg_dir) / (length*2)

!	write(102,*) x, y, x_temp1, y_temp1, x_temp, y_temp,  y_finite_difference, pos_dir, elevation(x,y), neg_dir, dx_l

end function y_finite_difference

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


double precision function x_finite_difference(x, y, direction)
! this function finds the finite difference of elevation for a given point to approximate the elevation gradient with respect to x

	use global_parameters
	implicit none

	double precision, intent(in) :: x, y, direction

	double precision :: length, x_temp, y_temp, pos_dir, neg_dir, perp_direction, x_temp1, y_temp1

	double precision, dimension(3,3) :: elev_store

	integer :: i, j

	length = dx_l / 2.d0

	do i = 1, 3

		x_temp = x + dble(i-2) * length*cos(perp_direction)

		do j = 1, 3
			
			y_temp = y + dble(j-2) *length*sin(perp_direction)
	
			elev_store(i,j) = elevation(x_temp, y_temp)

!			write(103,*) i, j, x_temp, y_temp, elev_store

		end do

	end do


	! third order finite difference method, see Skidmore, 1989
	x_finite_difference = ((elev_store(3,3) + 2.d0*elev_store(3,2) + elev_store(3,1)) - (elev_store(1,3) + 2.d0*elev_store(1,2)&
		+ elev_store(1,1))) / (8.d0*length)


!	x_temp = x + length*cos(direction)
!	y_temp = y + length*sin(direction)

!	pos_dir = elevation(x_temp, y_temp)

!	x_temp = x - length*cos(direction)
!	y_temp = y - length*sin(direction)

!	neg_dir = elevation(x_temp, y_temp)

!	x_finite_difference = (pos_dir - neg_dir) / dx_l

end function x_finite_difference

!!!!!!!!!!!!!!!!!!!!!1
double precision function xy_difference(x, y, direction)
! this function finds the finite difference of elevation gradient for a given point to approximate the elevation gradient with respect to x and y

	use global_parameters
	implicit none

	double precision, intent(in) :: x, y, direction

	double precision :: length, x_temp, y_temp, pos_dir, neg_dir, perp_direction

	double precision, dimension(3,3) :: grad_store

	integer :: i, j

	perp_direction = check_angle(direction + pi/2.d0)
	length = dx_l / 2.d0

	do i = 1, 3

		x_temp = x + dble(i-2) * length*cos(perp_direction)

		do j = 1, 3
			
			y_temp = y + dble(j-2) *length*sin(perp_direction)
	
			grad_store(i,j) = y_finite_difference(x_temp, y_temp, direction)

!			write(103,*) i, j, x_temp, y_temp, elev_store

		end do

	end do


	! third order finite difference method, see Skidmore, 1989
	xy_difference = ((grad_store(3,3) + 2.d0*grad_store(3,2) + grad_store(3,1)) - (grad_store(1,3) + 2.d0*grad_store(1,2)&
		+ grad_store(1,1))) / (8.d0*length)



!	length = dx_l / 2.d0

!	x_temp = x + length*cos(direction)
!	y_temp = y + length*sin(direction)

!	pos_dir = y_finite_difference(x_temp, y_temp, direction)

!	x_temp = x - length*cos(direction)
!	y_temp = y - length*sin(direction)

!	neg_dir = y_finite_difference(x_temp, y_temp, direction)
	
!	xy_difference = (pos_dir - neg_dir) / dx_l

end function xy_difference

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

double precision function y_second_difference(x, y, direction)
! this function finds the finite difference of elevation for a given point to approximate the elevation gradient with respect to y

	use global_parameters
	implicit none

	double precision, intent(in) :: x, y, direction

	double precision :: length, x_temp, y_temp, pos_dir, neg_dir, perp_direction

	double precision, dimension(3,3) :: grad_store

	integer :: i, j

	perp_direction = check_angle(direction + pi/2.d0)
	length = dx_l / 2.d0

	do i = 1, 3

		x_temp = x + dble(i-2) * length*cos(perp_direction)

		do j = 1, 3
			
			y_temp = y + dble(j-2) *length*sin(perp_direction)
	
			grad_store(i,j) = y_finite_difference(x_temp, y_temp, direction)

!			write(103,*) i, j, x_temp, y_temp, elev_store

		end do

	end do

	! third order finite difference method, see Skidmore, 1989
	y_second_difference = ((grad_store(3,3) + 2.d0*grad_store(2,3) + grad_store(1,3)) - (grad_store(3,1) + 2.d0*grad_store(2,1)&
		+ grad_store(1,1))) / (8.d0*length)

!	length = dx_l

!	x_temp = x + length*cos(perp_direction)
!	y_temp = y + length*sin(perp_direction)

!	pos_dir = elevation(x_temp, y_temp)

!	x_temp = x - length*cos(perp_direction)
!	y_temp = y - length*sin(perp_direction)

!	neg_dir = elevation(x_temp, y_temp)

!	y_second_difference = (pos_dir - 2.d0*elevation(x,y) + neg_dir) / length**2

end function y_second_difference


end module grids
