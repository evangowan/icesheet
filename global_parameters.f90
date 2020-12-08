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

module global_parameters
	implicit none

	! storage for the boundary
	double precision, allocatable, dimension(:), save :: x_distance, y_distance, interior_direction

	INTEGER, save :: number_boundary_points, polygon_counter

	! initial integration distance interval, 0.1 works well, also used when calculating gradients
	double precision, parameter :: dx_l = 0.1

	! parameters for the ice sheet calculation

	double precision :: minimum_spacing, elevation_interval

	! constants

	double precision, parameter :: r_earth = 6371.0 ! in km
	double precision, parameter :: rho_ice = 917.0 
	double precision, parameter :: rho_water = 1025.0
	double precision, parameter :: g = 9.80665

	double precision, parameter :: pi = 3.141592653589793

	
	double precision, parameter :: max_elevation = 5000 ! highest elevation, in meters, really only used to present a maximum limit of the arrays
	double precision, parameter :: minimum_elevation = 0 ! given that the ice would float at any elevation below 0 m
	double precision, parameter :: minimum_thickness = 1 ! nominal minimum ice thickness at the boundary

contains




double precision function average_direction(direction1, direction2)


	implicit none

	double precision, intent(in) :: direction1, direction2


	if((direction1 > pi/2.d0 .and. direction2 <  -pi/2.d0) ) THEN
		average_direction = check_angle( (direction1 + (2.d0*pi+direction2))/2.d0)

	elseif (direction1 < -pi/2.d0 .and. direction2 > pi/2.d0) THEN

		average_direction = check_angle( (direction2 + (2.d0*pi+direction1))/2.d0)

	elseif (direction1 > pi/2.d0 .and. (direction2 >-pi/2.d0 .and. direction2 < 0.d0 )) THEN
		
		if (direction1-direction2 <= pi) THEN

			average_direction = (direction1 + direction2) / 2.d0

		else

			average_direction = check_angle( (direction1 + (2.d0*pi+direction2))/2.d0)


		endif

	elseif (direction2 > pi/2.d0 .and. (direction1 >-pi/2.d0 .and. direction1 < 0.d0 )) THEN

		if (direction2-direction1 <= pi) THEN

			average_direction = (direction1 + direction2) / 2.d0

		else

			average_direction = check_angle( (direction2 + (2.d0*pi+direction1))/2.d0)

		endif

	else 
		average_direction = (direction1 + direction2) / 2.d0


	endif

	

end function average_direction


!***********************************************************************************

logical function point_in_polygon(x_boundary, y_boundary, x, y, number_points)

! this function determines whether a give point (x,y) is within a polygon defined by x_boundary and y_boundary
	
	implicit none

	integer, intent(in) :: number_points
	double precision, intent(in) :: x, y
	double precision, dimension(number_points), intent(in) :: x_boundary, y_boundary

	integer :: current_point, next_point, last_point, crossover_counter, number_adjusted_points
	logical :: found_first, found_last, inside

	found_first = .false.
	found_last = .false.
	inside = .false.


	if(x_boundary(1) == x_boundary(number_points) .and. y_boundary(1) == y_boundary(number_points)) THEN
		number_adjusted_points = number_points - 1 ! if the last and first point of the polygon are the same, ignore the last point
	else
		number_adjusted_points = number_points
	end if

	current_point = 1
	search_boundary: do

		next_point = current_point + 1
	
		if (next_point > number_adjusted_points) THEN
			next_point = 1
			found_last = .true.
		endif

! even-odd rule algorithm to determine if the point is inside or outside

		if (min(y_boundary(current_point), y_boundary(next_point)) < y .and.&
		    max(y_boundary(current_point), y_boundary(next_point)) >= y) THEN

			if (x_boundary(current_point) + (y - y_boundary(current_point)) /&
			    (y_boundary(next_point) - y_boundary(current_point)) * &
			    (x_boundary(next_point) - x_boundary(current_point)) < x) THEN

				inside = .not.(inside)

			endif

		endif

		current_point = current_point + 1

		if (found_last) THEN
			exit search_boundary
		endif
		
	
	end do search_boundary

	point_in_polygon = inside

	return
end function point_in_polygon

double precision function check_angle(angle)

	implicit none
	double precision, intent(in) :: angle
!	double precision ::  check_angle

	if (angle > pi) THEN
		check_angle = angle - 2.d0 * pi
	elseif (angle < -pi) THEN
		check_angle = angle + 2.d0 * pi
	else
		check_angle = angle
	end if


end function check_angle


end module global_parameters
