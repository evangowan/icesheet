program diff_map

use global_parameters
use read_polygons

	implicit none
	! constants
	integer, parameter :: param_diff_unit = 100, grid_unit=200, id_unit = 300, boundary_unit = 400, mean_gmt_unit=500,&
	 median_gmt_unit=600
	character(len=255), parameter :: param_file = "diff_map_params.txt", mean_gmt="mean_diff.gmt", median_gmt="median_diff.gmt"


	! variables
	integer :: x_min, x_max, y_min, y_max, grid_spacing, istat, num_x, num_y, counter, x_counter, y_counter, total_num, x, y
	integer :: x_index, y_index, poly_diff_counter, local_number_points, local_max_points
	integer :: start_y_index, start_x_index, end_y_index, end_x_index, local_x_counter, local_y_counter

	double precision :: difference
	double precision :: local_x_min, local_x_max, local_y_min, local_y_max, dble_grid_spacing
	double precision :: local_x, local_y, average_difference, median_difference

	double precision, dimension(4) :: bounds_polygon_x, bounds_polygon_y

	logical :: inside, check_polygon

	logical, dimension(max_points) :: outside_array

	character(len=255) :: shear_stress_boundaries_file, polygon_id_file, grid_file, dummy, dummy2

	
	! non-allocated arrays
	
	integer, parameter :: static_size = 10000

	integer, dimension(static_size) :: polygon_id_array

	! allocated memory

	double precision, dimension(:), allocatable :: diff_values
	logical, dimension(:), allocatable :: diff_mask
	double precision, dimension(:,:), allocatable :: diff_grid

	open (unit=param_diff_unit, file = param_file, status="old", form="formatted", access="sequential")

	read(param_diff_unit,*) x_min
	read(param_diff_unit,*) x_max
	read(param_diff_unit,*) y_min
	read(param_diff_unit,*) y_max
	read(param_diff_unit,*) grid_spacing
	read(param_diff_unit,*) shear_stress_boundaries_file
	read(param_diff_unit,*) polygon_id_file
	read(param_diff_unit,*) grid_file


	bounds_polygon_x(1) = x_min
	bounds_polygon_y(1) = y_min
	bounds_polygon_x(2) = x_max
	bounds_polygon_y(2) = y_min
	bounds_polygon_x(3) = x_max
	bounds_polygon_y(3) = y_max
	bounds_polygon_x(4) = x_min
	bounds_polygon_y(4) = y_max

	close(unit=param_diff_unit)

	dble_grid_spacing = dble(grid_spacing)

	! read in the boundary polygons

	call read_polygons_init(shear_stress_boundaries_file)

	outside_array = .false.

	! this loop prevents shear stress boundaries from going outside of the input grid
	do poly_diff_counter = 1, number_polygons, 1

		check_polygon = .false.

		do counter = 1, polygon_points(poly_diff_counter)

			if (x_coordinates(poly_diff_counter,counter) < x_min .or. x_coordinates(poly_diff_counter,counter) > x_max &
                      .or. y_coordinates(poly_diff_counter,counter) < y_min .or. &
 			    y_coordinates(poly_diff_counter,counter) > y_max) THEN
				check_polygon = .true.
			endif


		end do

		if(check_polygon) THEN

			call polygon_within_bounds(poly_diff_counter,bounds_polygon_x, bounds_polygon_y, 4, outside_array(poly_diff_counter))

		endif


	end do

	
	! read in the boundary polygon ids

	open (unit=id_unit, file = polygon_id_file, status="old", form="formatted", access="sequential")
	read_ids: do counter = 1, number_polygons

		read(id_unit,*, iostat=istat) polygon_id_array(counter)
		if(istat /= 0) THEN
			write(6,*) "the polygon file and the id file do not match"
			stop
		endif

	end do read_ids

	close(id_unit)

	! read in the grid file
	num_x = (x_max - x_min ) / grid_spacing + 1
	num_y = (y_max - y_min ) / grid_spacing + 1

	total_num = num_x * num_y

	allocate(diff_grid(num_x,num_y), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating diff_grid"
		stop
	endif

	open (unit=grid_unit, file = grid_file, status="old", form="formatted", access="sequential")

	do counter = 1, total_num
	
		read(grid_unit,*,iostat=istat) x, y, difference
		if(istat /= 0) THEN
			write(6,*) "number of points in grid file does not match input x and y ranges"
			stop
		endif

		x_index = (x - x_min) / grid_spacing + 1
		y_index = (y - y_min) / grid_spacing + 1

		diff_grid(x_index, y_index) = difference

	end do

	close(unit=grid_unit)


	! go through the polygons and find the median, mean, etc


	open (unit=median_gmt_unit, file = median_gmt, status="replace", form="formatted", access="sequential")
	open (unit=mean_gmt_unit, file = mean_gmt, status="replace", form="formatted", access="sequential")

	do poly_diff_counter = 1, number_polygons, 1



		local_number_points = 0

		if(.not.outside_array(poly_diff_counter)) THEN

			local_x_min = minval(x_coordinates(poly_diff_counter,1:polygon_points(poly_diff_counter)))
			local_x_max = maxval(x_coordinates(poly_diff_counter,1:polygon_points(poly_diff_counter)))

			local_y_min = minval(y_coordinates(poly_diff_counter,1:polygon_points(poly_diff_counter)))
			local_y_max = maxval(y_coordinates(poly_diff_counter,1:polygon_points(poly_diff_counter)))


			local_x_min = dble(floor(local_x_min/dble_grid_spacing)) * dble_grid_spacing
			local_x_max = dble(ceiling(local_x_max/dble_grid_spacing)) * dble_grid_spacing

			local_y_min = dble(floor(local_y_min/dble_grid_spacing)) * dble_grid_spacing
			local_y_max = dble(ceiling(local_y_max/dble_grid_spacing)) * dble_grid_spacing

			start_y_index = nint((local_y_min - y_min)/grid_spacing) + 1
			start_x_index = nint((local_x_min - x_min)/grid_spacing) + 1
			end_y_index = nint((local_y_max - y_min)/grid_spacing) + 1
			end_x_index = nint((local_x_max - x_min)/grid_spacing) + 1


			if(start_x_index < 1) THEN
				write(6,*) "check polygon: ", poly_diff_counter
				write(6,*) "x coordinate less than minimum"
				write(6,*) local_x_min
				stop
			end if

			if(start_y_index < 1) THEN
				write(6,*) "check polygon: ", poly_diff_counter
				write(6,*) "y coordinate less than minimum"
				write(6,*) local_y_min
				stop
			end if

			local_max_points = (end_x_index-start_x_index) * (end_y_index-start_y_index)


			allocate(diff_values(local_max_points), diff_mask(local_max_points), stat=istat)

			if(istat /=0) THEN
				write(6,*) "problem allocating diff_values"
				stop
			endif

			diff_values = 0
			diff_mask = .false.

		
			do  local_x_counter = start_x_index, end_x_index
				do local_y_counter = start_y_index, end_y_index

				
					local_x = dble(x_min) + dble(local_x_counter-1) * dble_grid_spacing
					local_y = dble(y_min) + dble(local_y_counter-1) * dble_grid_spacing

					inside = point_in_polygon(x_coordinates(poly_diff_counter,1:polygon_points(poly_diff_counter)), &
					  y_coordinates(poly_diff_counter,1:polygon_points(poly_diff_counter)), local_x, &
					  local_y, polygon_points(poly_diff_counter))

					if(inside) THEN
						local_number_points =  local_number_points + 1
						diff_values(local_number_points) = diff_grid(local_x_counter,local_y_counter)
						diff_mask(local_number_points) = .true.
					endif

				end do
			end do

		endif

		if (local_number_points > 0) then


			average_difference = sum(diff_values,diff_mask) / local_number_points

			median_difference = get_median(diff_values(1:local_number_points),local_number_points)

		else

			average_difference = 0
			median_difference = 0


		endif

		write(dummy,*) average_difference

		dummy2 = "> -Z" // trim(adjustl(dummy))

		write(mean_gmt_unit,'(A60)') dummy2


		write(dummy,*) median_difference

		dummy2 = "> -Z" // trim(adjustl(dummy))

		write(median_gmt_unit,'(A60)') dummy2


		do counter = 1, polygon_points(poly_diff_counter)

			write(median_gmt_unit,*) x_coordinates(poly_diff_counter,counter), y_coordinates(poly_diff_counter,counter)
			write(mean_gmt_unit,*) x_coordinates(poly_diff_counter,counter), y_coordinates(poly_diff_counter,counter)

		end do

		deallocate(diff_values, diff_mask, stat=istat)
		if(istat /=0) THEN
			write(6,*) "problem deallocating diff_values"
			stop
		endif
!		scale_down = 0.5

!		if (grid(poly_diff_counter) <= -250) THEN
			
!			multiplier(poly_diff_counter) = -0.1*scale_down
!		elseif (grid(poly_diff_counter) <= -100) THEN
!			multiplier(poly_diff_counter) = -0.05*scale_down
!		elseif (grid(poly_diff_counter) <= -50) THEN
!			multiplier(poly_diff_counter) = -0.02*scale_down
!		elseif (grid(poly_diff_counter) <= -20) THEN
!			multiplier(poly_diff_counter) = -0.01*scale_down
!		elseif (grid(poly_diff_counter) <= -10) THEN
!			multiplier(poly_diff_counter) = -0.005*scale_down
!		elseif (grid(poly_diff_counter) <= 10) THEN
!			multiplier(poly_diff_counter) = 0*scale_down
!		elseif (grid(poly_diff_counter) <= 20) THEN
!			multiplier(poly_diff_counter) = 0.005*scale_down
!		elseif (grid(poly_diff_counter) <= 50) THEN
!			multiplier(poly_diff_counter) = 0.01*scale_down
!		elseif (grid(poly_diff_counter) <= 100) THEN
!			multiplier(poly_diff_counter) = 0.02*scale_down
!		elseif (grid(poly_diff_counter) <= 250) THEN
!			multiplier(poly_diff_counter) = 0.05*scale_down
!		else
!			multiplier(poly_diff_counter) = 0.1*scale_down
!		endif

!		if (shear_stress(poly_diff_counter) < 500) THEN
!			multiplier(poly_diff_counter) = 0
!		endif

!		write(ss_mod_unit,*) multiplier(poly_diff_counter)

	end do

	close(unit=mean_gmt_unit)
	close(unit=median_gmt_unit)

	call read_polygons_clear()

contains


double precision function get_median(values_array,array_size)

	implicit none
	integer, intent(in) :: array_size
	double precision, dimension(array_size) :: values_array

	logical, dimension(array_size) :: mask_array

	logical :: even
	integer :: counter, half_size, temp_index
	double precision :: median_temp



	if(nint(dble(array_size)/2.0) == int(dble(array_size)/2.0)) THEN
		half_size = array_size / 2 
		even = .true.
	else
		half_size = array_size / 2 + 1
		even = .false.
	end if

	mask_array =  .true.
	if(array_size == 1) THEN
		get_median = values_array(1)
	else


		do counter = 1, half_size

			temp_index = minloc(values_array, 1, mask_array)
			median_temp = values_array(temp_index)

			mask_array(temp_index) = .false.


		end do

		if(even) then ! if there are an even number of points, take the average of the two middle points
			temp_index = minloc(values_array, 1, mask_array)
			median_temp = (median_temp + values_array(temp_index)) / 2.0
		end if

		get_median = median_temp

	end if



end function get_median

end program diff_map
