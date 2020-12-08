module read_polygons

! reads the GMT formatted multisegment files.


	implicit none

	integer :: max_polygons
	integer, parameter ::  max_points = 1000000, file_unit = 20, param_unit=10
	integer, save :: number_polygons
	integer, dimension(:), allocatable :: polygon_points

	double precision, dimension(:,:), allocatable :: x_coordinates, y_coordinates
	double precision, save :: fining_increment



contains



subroutine read_polygons_init(file_name)

	implicit none

	character (len=255), intent(in) :: file_name

	integer :: istat, counter, polygon_counter, add_points, add_counter


	double precision :: x, y, distance, angle

	character (len=1) :: divider ! make sure that there are no spaces before the ">" character or this will mess up
	character (len=1), parameter :: divider_character = ">", ignore_character = "#"



	number_polygons = 0


	open(unit=file_unit, file=file_name, access="sequential", form="formatted", status="old")

	number_polygons = 0

	
	read_divider: do 
		read(file_unit,*, iostat=istat) divider
		if(istat /=0) THEN
			exit read_divider
		endif


		if(divider == divider_character) THEN
			number_polygons = number_polygons + 1

		endif

	end do read_divider

	rewind(file_unit)

	allocate(x_coordinates(number_polygons,max_points), y_coordinates(number_polygons,max_points), &
		   polygon_points(number_polygons), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating arrays in read_polygons"
		stop
	endif

	polygon_counter = 0


	read_polygons: do 
		
		read(file_unit,*, iostat=istat) divider
		if(istat /=0) THEN
			exit read_polygons
		endif

		if(divider == divider_character) THEN
			polygon_counter = polygon_counter + 1
			polygon_points(polygon_counter) = 0
			cycle read_polygons
		elseif (divider == ignore_character) THEN ! anything starting with '#' should be ignored
				
			cycle read_polygons
		else
			backspace(unit=file_unit)

			polygon_points(polygon_counter) = polygon_points(polygon_counter) + 1
			call check_array(polygon_points(polygon_counter))

			read(file_unit,*) x, y

			x_coordinates(polygon_counter,polygon_points(polygon_counter)) = x
			y_coordinates(polygon_counter,polygon_points(polygon_counter)) = y

		endif

	end do read_polygons


		close(unit=file_unit)


! check to see if the start point is the same as the end point. The point_in_polygon routine does not work if it is, so this loop removes it

	do counter = 1, 2, 1

		do polygon_counter = 1, number_polygons, 1

			if (x_coordinates(polygon_counter,1) == &
			    x_coordinates(polygon_counter,polygon_points(polygon_counter)) .and. &
			    y_coordinates(polygon_counter,1) == &
			    y_coordinates(polygon_counter,polygon_points(polygon_counter))) THEN

				polygon_points(polygon_counter) = polygon_points(polygon_counter) - 1

			endif

		end do

	end do

end subroutine read_polygons_init

subroutine read_polygons_clear()
! clears the memory from this subroutine

	implicit none

	integer :: istat

	deallocate(x_coordinates, y_coordinates, polygon_points, stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem deallocating arrays in read_polygons"
		stop
	endif

end subroutine read_polygons_clear

subroutine check_array(point_count)

	integer, intent(in) :: point_count

	if(point_count > max_points) THEN
		write(6,*) "Number of points in polygon exceeds internal memory"
		write(6,*) "If you want to proceed, you must increase max_points"
		write(6,*) "and recompile:", point_count, "> ", max_points
		close(unit=file_unit)
		stop
	end if

end subroutine check_array

subroutine polygon_within_bounds(polygon_index,bounding_polygon_x, bounding_polygon_y,bounding_polygon_size, all_outside)


	use global_parameters ! for point_in_polygon
	implicit none

	integer, intent(in) :: polygon_index, bounding_polygon_size
	double precision, dimension(bounding_polygon_size) :: bounding_polygon_x, bounding_polygon_y

	logical, dimension(max_points) :: inside
	integer :: counter, temp_counter
	logical :: cut, all_outside

	double precision, dimension(max_points) :: x_temp, y_temp


	! first determine which points are inside

	inside=.false.
	cut = .false.
	all_outside = .true.

	do counter = 1, polygon_points(polygon_index)

		inside(counter) = point_in_polygon(bounding_polygon_x, bounding_polygon_y, x_coordinates(polygon_index,counter), &
		  y_coordinates(polygon_index,counter), bounding_polygon_size)

		if(.not.inside(counter) ) THEN
			cut = .true.
		end if

		if(inside(counter) ) THEN
			all_outside = .false.
		end if

	end do

	! if the polygon needs cut, then add the crossover points with the bounding polygon, and eliminate the 
	temp_counter = 0

	if (cut) THEN

		do counter = 1, polygon_points(polygon_index)
		! TODO not done, right now I just completely eliminate the outside points, which is incorrect but easy to code

			if(inside(counter)) THEN
				temp_counter = temp_counter + 1
				x_temp(temp_counter) = x_coordinates(polygon_index,counter)
				y_temp(temp_counter) = y_coordinates(polygon_index,counter)
			endif

		end do

		polygon_points(polygon_index) = temp_counter
		x_coordinates(polygon_index,:) = x_temp
		y_coordinates(polygon_index,:) = y_temp

	end if


end subroutine polygon_within_bounds



subroutine crossover_point(a_x1, a_y1, a_x2, a_y2, b_x1, b_y1, b_x2, b_y2, crossover_x, crossover_y, is_crossover)

	! checks if the two given line segments overlap. If they do, this subroutine returns the crossover point

	implicit none

	double precision, intent(in) :: a_x1, a_y1, a_x2, a_y2, b_x1, b_y1, b_x2, b_y2
	double precision, intent(out) :: crossover_x, crossover_y
	logical, intent(out) :: is_crossover

	double precision :: slope1, slope2, intercept1, intercept2, a_min_cell_x, a_min_cell_y, a_max_cell_x, a_max_cell_y
	double precision :: b_min_cell_x, b_min_cell_y, b_max_cell_x, b_max_cell_y
	double precision :: temp_x, temp_y


	logical :: vertical_line1, vertical_line2

	if(a_x1 /= a_x2) THEN
		
		
		vertical_line1 = .false.
	else
		vertical_line1 = .true.
	endif


	a_min_cell_x = min(a_x1, a_x2)
	a_min_cell_y = min(a_y1, a_y2)
	a_max_cell_x = max(a_x1, a_x2)
	a_max_cell_y = max(a_y1, a_y2)

	b_min_cell_x = min(b_x1, b_x2)
	b_min_cell_y = min(b_y1, b_y2)
	b_max_cell_x = max(b_x1, b_x2)
	b_max_cell_y = max(b_y1, b_y2)


	is_crossover = .false.



	slope1 = (a_y2 - a_y1) / (a_x2 - a_x1) 
	intercept1 = a_y2 - a_x2 * slope1

	slope2 = (b_y2 - b_y1) / (b_x2 - b_x1) 
	intercept2 = b_y2 - b_x2 * slope2

	temp_x = (intercept2 - intercept1) / (slope1 - slope2)
	temp_y = slope1 * temp_x + intercept1

	if(a_x2 == a_x1) then
		temp_x = a_x2
		temp_y = slope2 * temp_x + intercept2
	elseif(b_x2 == b_x1) THEN
		temp_x = b_x2
		temp_y = slope1 * temp_x + intercept1
	endif


	is_crossover = .false.
	if(temp_x > a_min_cell_x) THEN
	  if(temp_x < a_max_cell_x) THEN
	     if(temp_x > b_min_cell_x) THEN
		 if(temp_x < b_max_cell_x) THEN
	   	   if(temp_y > a_min_cell_y) THEN
		     if(temp_y < a_max_cell_y) THEN
	   		 if(temp_y > b_min_cell_y) THEN
			   if(temp_y < b_max_cell_y) THEN

				is_crossover = .true.
			   endif
			 endif
		     endif
		   endif
		 endif
	     endif
	   endif
	endif





	crossover_x = temp_x
	crossover_y = temp_y
end subroutine crossover_point

end module read_polygons
