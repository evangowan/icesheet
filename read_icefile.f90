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

SUBROUTINE read_icefile(filename)
	
	use global_parameters

	IMPLICIT NONE

	CHARACTER*80, INTENT(IN) :: filename
	integer, parameter :: ice_file_unit = 100

	double precision :: x, y, perimeter, start_distance, end_distance, direction, segment_distance, ax, ay, bx, by, cx, cy
	double precision :: dir1, dir2, check_x, check_y


	integer :: istat, number_boundary_points_temp, total_points, counter, counter2, counter3, counter4, points_for_array

	double precision, allocatable, dimension(:) :: x_distance_temp, y_distance_temp

	OPEN (unit = ice_file_unit, file=filename, access="sequential", status="old", form="formatted", POSITION="REWIND")

	number_boundary_points = 0
	number_boundary_points_temp = 1 ! start with 1, so that the first and last point are the same

	! first check to see how many boundary points there are in the file
	! this should be a plain x,y file in metres!

	do

		read(ice_file_unit,*, iostat = istat)  x, y

		if (istat < 0) THEN
			write(6,*) "reached end of file at ", number_boundary_points_temp
			exit
		else if (istat > 0) THEN
			WRITE(6,*) "There is something wrong with the input file: ", filename
			STOP
		end if

		number_boundary_points_temp = number_boundary_points_temp + 1

	end do



	! with the amount of boundary points known, we can allocate sizes to hold the data


	allocate(x_distance_temp(number_boundary_points_temp), y_distance_temp(number_boundary_points_temp), stat=istat)
	if (istat /= 0) THEN
		WRITE(6,*) "allocation error for x_distance_temp and y_distance_temp"
		stop
	ENDIF

	rewind(unit=ice_file_unit)

	! read in the ice margin values
	do counter=1, number_boundary_points_temp-1, 1

		read(ice_file_unit,*, iostat = istat) x, y
		
		if(istat /= 0) THEN
			write(6,*) "reached end of file in ice margin file"
			exit
		ENDIF


		x_distance_temp(counter) = x
		y_distance_temp(counter) = y

	end do

		x_distance_temp(number_boundary_points_temp) = x_distance_temp(1)
		y_distance_temp(number_boundary_points_temp) = y_distance_temp(1)

	close(unit=ice_file_unit)

	! next, simplify the boundary to "minimum_spacing" intervals. I'm using linear interpolation, because it is easy.

	! first, find the perimeter. Because the first and last points in the are the same, it makes things easy
	! to do in a loop.
	
	perimeter = 0.d0
	
	do counter2 = 1, number_boundary_points_temp-1, 1

		perimeter = perimeter + sqrt( (x_distance_temp(counter2+1)-x_distance_temp(counter2))**2 + &
				                  (y_distance_temp(counter2+1)-y_distance_temp(counter2))**2)

	end do

	number_boundary_points = int(perimeter / minimum_spacing) + 1 ! added the + 1, because the perimeter algorithm would not take into account the zero point

	write(6,*) "number of points", number_boundary_points, perimeter, minimum_spacing

	! next, create the x_distance and y_distance arrays

	allocate(x_distance(number_boundary_points), y_distance(number_boundary_points), stat=istat)
	if (istat /= 0) THEN
		WRITE(6,*) "allocation error for x_distance and y_distance"
		stop
	ENDIF


	start_distance = 0.d0
	x_distance(1) = x_distance_temp(1)
	y_distance(1) = y_distance_temp(1)


	total_points = 1
	original_boundary: do counter3 = 1, number_boundary_points_temp-1, 1 

		end_distance = sqrt( (x_distance_temp(counter3+1)-x_distance_temp(counter3))**2 + &
				                  (y_distance_temp(counter3+1)-y_distance_temp(counter3))**2) + start_distance

		points_for_array = int(end_distance/minimum_spacing) - int(start_distance/minimum_spacing) 


		if(points_for_array > 0) THEN

			direction = atan2(y_distance_temp(counter3+1)-y_distance_temp(counter3),&
					      x_distance_temp(counter3+1)-x_distance_temp(counter3))

			do counter4 = 1, points_for_array, 1
				total_points = total_points + 1
				segment_distance = int(start_distance/minimum_spacing + dble(counter4))*minimum_spacing-start_distance
				x_distance(total_points) = x_distance_temp(counter3) + segment_distance * cos(direction)
				y_distance(total_points) = y_distance_temp(counter3) + segment_distance * sin(direction)



			end do

		end if

		start_distance = end_distance

	end do original_boundary


	! the original boundary array is no longer needed

	deallocate (x_distance_temp, y_distance_temp)





	! the next step is to find the interior direction. This involves going through all the boundary points and going perpendicular
	! to the margin and determining which way the interior is. I reckon 5 m should be far enough to determine which way is in


	allocate(interior_direction(number_boundary_points), stat=istat)
	if (istat /= 0) THEN
		WRITE(6,*) "allocation error for interior_direction"
		stop
	ENDIF 

	do counter3 = 1, number_boundary_points, 1


		cx = x_distance(counter3)
		cy = y_distance(counter3)


		if(counter3 == 1) THEN
			bx = x_distance(number_boundary_points)
			by = y_distance(number_boundary_points)
		else
			bx = x_distance(counter3-1)
			by = y_distance(counter3-1)
		endif

		if(counter3 == number_boundary_points) THEN
			ax = x_distance(1)
			ay = y_distance(1)
		else
			ax = x_distance(counter3+1)
			ay = y_distance(counter3+1)
		endif

		dir1 = atan2(-cx+bx, by-cy)
		dir2 = atan2(-cx+ax, ay-cy)

		interior_direction(counter3) = average_direction(dir1, dir2)


		check_x = cx + 5. * sin(interior_direction(counter3))
		check_y = cy + 5. * cos(interior_direction(counter3))

		


		if (.not. point_in_polygon(x_distance, y_distance, check_x, check_y, number_boundary_points)) THEN ! have to turn the direction around

			interior_direction(counter3) = check_angle(interior_direction(counter3) + pi)
		endif

	
	end do



END SUBROUTINE read_icefile
