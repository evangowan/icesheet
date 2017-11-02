module read_polygons

! reads the GMT formatted multisegment files.


	implicit none

	integer :: max_polygons
	integer, parameter ::  max_points = 1000000, file_unit = 20, param_unit=10
	integer, save :: number_polygons
	integer, dimension(:,:), allocatable :: polygon_points

	double precision, dimension(:,:), allocatable :: x_coordinates, y_coordinates
	double precision, save :: fining_increment



contains



subroutine read_polygons_init(file_name)

	implicit none

	character (len=255), intent(in), file_name

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
			exit read_polygons
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

end module read_polygons
