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

program create_ss_grid

	! reads in the file with polygons "gmt_file.txt" and creates a grid with a given resolution set by the user
	implicit none

	double precision :: y_spacing, x_spacing, x, y, minimum_y, minimum_x
	double precision :: maximum_y, maximum_x
	double precision :: local_minimum_x, local_maximum_x, local_minimum_y, local_maximum_y
	double precision :: local_x, local_y

	character(len=255) :: input_parameter, dummy
	character(len=255), parameter :: input_file = "gmt_file.txt", output_file = "domains.txt"

	character(len=1) :: bracket

	integer :: istat, number_polygons, polygon_counter, point_counter, maximum_points, number_y, number_x
	integer :: start_y_index, start_x_index, end_y_index, end_x_index
	integer :: local_y_counter, local_x_counter, y_counter, x_counter
	integer, parameter :: input_file_unit=10, max_polygons = 10000, output_file_unit=20

	integer, dimension(max_polygons) :: polygon_point_size ! if you have more than 10000 polygons, you are ambitious!

	double precision, allocatable, dimension(:,:) :: y_array, x_array
	integer, allocatable, dimension(:) :: shear_stess_id_array
	integer, allocatable, dimension(:,:) :: grid

	logical :: inside

	character(len=50), parameter :: output_format = "(F12.3,1X,F12.3,1X,I5)"

	call getarg(1,input_parameter)
	read(input_parameter,*) y_spacing
	call getarg(2,input_parameter)
	read(input_parameter,*) x_spacing

	! first, find the number of polygons, and the maximum amount of points

	number_polygons = 0
	polygon_point_size = 0



	open(unit=input_file_unit, file=input_file, access="sequential", form="formatted", status="old")

	initial_read: do

		read(input_file_unit,'(A1,A)', iostat=istat) bracket, dummy
		if(istat /=0) THEN !hopefully end of file
			exit initial_read
		endif

		if(bracket == ">") then
			number_polygons = number_polygons + 1

			if(number_polygons > max_polygons) THEN
				write(6,*) "you have to increase the max_polygons parameter in create_ss_grid.f90, and recompile"
				stop
			endif

		else
			polygon_point_size(number_polygons) = polygon_point_size(number_polygons) + 1
		endif

	end do initial_read

	rewind(unit=input_file_unit)

	maximum_points = maxval(polygon_point_size)

	allocate(y_array(number_polygons,maximum_points), x_array(number_polygons,maximum_points),&
		shear_stess_id_array(number_polygons), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating arrays"
		stop
	endif


	minimum_y = 9999999999.
	minimum_x = 9999999999.
	maximum_y = -9999999999.
	maximum_x = -9999999999.

	do polygon_counter = 1, number_polygons, 1

		read(input_file_unit,*) bracket, shear_stess_id_array(polygon_counter)


		do point_counter = 1, polygon_point_size(polygon_counter), 1

			read(input_file_unit,*) x_array(polygon_counter,point_counter), &
						      y_array(polygon_counter,point_counter)

			if(y_array(polygon_counter,point_counter) < minimum_y) THEN
				minimum_y = y_array(polygon_counter,point_counter)
			endif

			if(y_array(polygon_counter,point_counter) > maximum_y) THEN
				maximum_y = y_array(polygon_counter,point_counter)
			endif

			if(x_array(polygon_counter,point_counter) < minimum_x) THEN
				minimum_x = x_array(polygon_counter,point_counter)
			endif

			if(x_array(polygon_counter,point_counter) > maximum_x) THEN
				maximum_x = x_array(polygon_counter,point_counter)
			endif

		end do
	end do

	close(input_file_unit)


	! expand the region a bit

	minimum_x = dble(floor(minimum_x/x_spacing))*x_spacing - 2. * x_spacing
	maximum_x = dble(ceiling(maximum_x/x_spacing))*x_spacing + 2. * x_spacing

	minimum_y = dble(floor(minimum_y/y_spacing))*y_spacing - 2. * y_spacing
	maximum_y = dble(ceiling(maximum_y/y_spacing))*y_spacing + 2. * y_spacing




	number_y = nint((maximum_y - minimum_y) / y_spacing) + 1
	number_x = nint((maximum_x - minimum_x) / x_spacing) + 1


	allocate(grid(number_x,number_y), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating arrays"
		stop
	endif

	! note that it is entirely possible that polgons will be skipped if your lat/long spacing is not dense enough

	! also, if your input shapefile has any gaps between polygons, they could also be skipped. 
	! If they polygons overlap, the values could be overwritten

	grid = 0

	do polygon_counter = 1, number_polygons, 1


		local_minimum_x = minval(x_array(polygon_counter,1:polygon_point_size(polygon_counter)))
		local_maximum_x = maxval(x_array(polygon_counter,1:polygon_point_size(polygon_counter)))

		local_minimum_y = minval(y_array(polygon_counter,1:polygon_point_size(polygon_counter)))
		local_maximum_y = maxval(y_array(polygon_counter,1:polygon_point_size(polygon_counter)))


		local_minimum_x = dble(floor(local_minimum_x/x_spacing)) * x_spacing
		local_maximum_x = dble(ceiling(local_maximum_x/x_spacing)) * x_spacing

		local_minimum_y = dble(floor(local_minimum_y/y_spacing)) * y_spacing
		local_maximum_y = dble(ceiling(local_maximum_y/y_spacing)) * y_spacing

		start_y_index = nint((local_minimum_y - minimum_y)/y_spacing) + 1
		start_x_index = nint((local_minimum_x - minimum_x)/x_spacing) + 1
		end_y_index = nint((local_maximum_y - minimum_y)/y_spacing) + 1
		end_x_index = nint((local_maximum_x - minimum_x)/x_spacing) + 1
		
		do  local_x_counter = start_x_index, end_x_index
			do local_y_counter = start_y_index, end_y_index

				
				local_x = minimum_x + dble(local_x_counter-1) * x_spacing
				local_y = minimum_y + dble(local_y_counter-1) * y_spacing

				inside = point_in_polygon(x_array(polygon_counter,1:polygon_point_size(polygon_counter)), &
				  y_array(polygon_counter,1:polygon_point_size(polygon_counter)), local_x, &
				  local_y, polygon_point_size(polygon_counter))

				if(inside) THEN
					grid(local_x_counter,local_y_counter) = shear_stess_id_array(polygon_counter)
				endif

			end do
		end do


	end do


	! output

	open(unit=output_file_unit, file=output_file, access="sequential", form="formatted", status="replace")
	do x_counter = 1, number_x, 1
		do y_counter = 1, number_y, 1
			local_x = minimum_x + dble(x_counter-1) * x_spacing
			local_y = minimum_y + dble(y_counter-1) * y_spacing

			write(output_file_unit,output_format) local_x, local_y, grid(x_counter,y_counter) 

		end do
	end do

	close(output_file_unit)


	deallocate(y_array, x_array,shear_stess_id_array, stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem deallocating arrays"
		stop
	endif

contains


! Written by Evan Gowan, last updated August 24, 2015


logical function point_in_polygon(x_boundary, y_boundary, x, y, number_points)

! this function determines whether a give point (x,y) is within a polygon defined by x_boundary and y_boundary
! number_points is the number of points in the polygon

	
	implicit none

	integer, intent(in) :: number_points
	double precision, intent(in) :: x, y
	double precision, dimension(number_points), intent(in) :: x_boundary, y_boundary

	integer :: current_point, next_point, last_point, crossover_counter
	logical :: found_first, found_last, inside

	found_first = .false.
	found_last = .false.
	inside = .false.

	current_point = 1
	search_boundary: do

		next_point = current_point + 1
	
		if (next_point == number_points) THEN

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
	


end program create_ss_grid
