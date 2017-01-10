program create_ss_grid

	! Note, this program has been modified heavily from the Greenland example

	implicit none

	double precision, parameter :: minimum_shear_stress = 5000 ! value used outside of the domain

	double precision :: xmin, ymin, xmax, ymax, grid_spacing
	double precision :: y_spacing, x_spacing, x, y, minimum_y, minimum_x
	double precision :: maximum_y, maximum_x
	double precision :: local_minimum_x, local_maximum_x, local_minimum_y, local_maximum_y
	double precision :: local_x, local_y

	character(len=255) :: input_parameter, dummy, input_file, shear_stress_domains_file
	character(len=255), parameter ::  output_file = "shear_stress.xyz", param_file = "ss_grid_params.txt"

	character(len=1) :: bracket

	integer :: istat, number_polygons, polygon_counter, point_counter, maximum_points, number_y, number_x
	integer :: start_y_index, start_x_index, end_y_index, end_x_index, commandline_count
	integer :: local_y_counter, local_x_counter, y_counter, x_counter
	integer, parameter :: input_file_unit=10, max_polygons = 10000, output_file_unit=20, ss_unit = 30, param_unit = 40

	integer, dimension(max_polygons) :: polygon_point_size ! if you have more than 10000 polygons, you are ambitious!

	double precision, dimension(max_polygons) :: domain_ss_values
	integer :: number_domains, domain
	double precision :: shear_stress

	double precision, allocatable, dimension(:,:) :: y_array, x_array, shear_stress_grid
	integer, allocatable, dimension(:) :: shear_stess_id_array
	integer, allocatable, dimension(:,:) :: grid

	logical :: inside

	character(len=50), parameter :: output_format = "(F12.3,1X,F12.3,1X,F12.3)"

	! requires seven input parameters  gmt_file, xmin, xmax, ymin, and ymax, grid_spacing, shear_stress_domains



	open(unit=param_unit, file=param_file, access="sequential", form="formatted", status="old")

	read(param_unit,*) input_file

	read(param_unit,*) xmin

	read(param_unit,*) xmax

	read(param_unit,*) ymin

	read(param_unit,*) ymax

	read(param_unit,*) grid_spacing

	read(param_unit,'(A256)') shear_stress_domains_file

	close(unit=param_unit)


	! read in the shear stress values for the domains
	open(unit=ss_unit, file=shear_stress_domains_file, access="sequential", form="formatted", status="old")

	read_ss: do

		read(ss_unit, *, iostat=istat) domain, shear_stress
		if(istat /=0) THEN !hopefully end of file
			exit read_ss
		endif
		domain = domain+1
		domain_ss_values(domain) = shear_stress

	end do read_ss

	close(ss_unit)

	! first, find the number of polygons, and the maximum amount of points

	number_polygons = 0
	polygon_point_size = 0

	open(unit=input_file_unit, file=input_file, access="sequential", form="formatted", status="old")

	number_domains = 0
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



	do polygon_counter = 1, number_polygons, 1

		read(input_file_unit,*) bracket, shear_stess_id_array(polygon_counter)


		do point_counter = 1, polygon_point_size(polygon_counter), 1

			read(input_file_unit,*) x_array(polygon_counter,point_counter), &
						      y_array(polygon_counter,point_counter)


		end do
	end do

	close(input_file_unit)


	! if it isn't an even grid, it should give a warning

	minimum_x = dble(floor(xmin/grid_spacing))*grid_spacing
	maximum_x = dble(ceiling(xmax/grid_spacing))*grid_spacing

	minimum_y = dble(floor(ymin/grid_spacing))*grid_spacing
	maximum_y = dble(ceiling(ymax/grid_spacing))*grid_spacing

	if(minimum_x /= xmin) THEN
		write(6,*) "warning, input xmin does not conform to an even grid spacing"
	endif
	if(maximum_x /= xmax) THEN
		write(6,*) "warning, input xmax does not conform to an even grid spacing"
	endif
	if(minimum_y /= ymin) THEN
		write(6,*) "warning, input ymin does not conform to an even grid spacing"
	endif
	if(maximum_y /= ymax) THEN
		write(6,*) "warning, input ymax does not conform to an even grid spacing"
	endif


	number_y = nint((maximum_y - minimum_y) / grid_spacing) + 1
	number_x = nint((maximum_x - minimum_x) / grid_spacing) + 1


	allocate(shear_stress_grid(number_x,number_y), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating arrays"
		stop
	endif

	! note that it is entirely possible that polgons will be skipped if your spacing is not dense enough

	! also, if your input shapefile has any gaps between polygons, they could also be skipped. 
	! If they polygons overlap, the values could be overwritten

	shear_stress_grid = minimum_shear_stress

	do polygon_counter = 1, number_polygons, 1


		local_minimum_x = minval(x_array(polygon_counter,1:polygon_point_size(polygon_counter)))
		local_maximum_x = maxval(x_array(polygon_counter,1:polygon_point_size(polygon_counter)))

		local_minimum_y = minval(y_array(polygon_counter,1:polygon_point_size(polygon_counter)))
		local_maximum_y = maxval(y_array(polygon_counter,1:polygon_point_size(polygon_counter)))


		local_minimum_x = dble(floor(local_minimum_x/grid_spacing)) * grid_spacing
		local_maximum_x = dble(ceiling(local_maximum_x/grid_spacing)) * grid_spacing

		local_minimum_y = dble(floor(local_minimum_y/grid_spacing)) * grid_spacing
		local_maximum_y = dble(ceiling(local_maximum_y/grid_spacing)) * grid_spacing

		start_y_index = max(nint((local_minimum_y - minimum_y)/grid_spacing) + 1, 1)
		start_x_index = max(nint((local_minimum_x - minimum_x)/grid_spacing) + 1, 1)
		end_y_index = min(nint((local_maximum_y - minimum_y)/grid_spacing) + 1,number_y)
		end_x_index = min(nint((local_maximum_x - minimum_x)/grid_spacing) + 1,number_y)
		
		do  local_x_counter = start_x_index, end_x_index
			do local_y_counter = start_y_index, end_y_index

				
				local_x = minimum_x + dble(local_x_counter-1) * grid_spacing
				local_y = minimum_y + dble(local_y_counter-1) * grid_spacing

				inside = point_in_polygon(x_array(polygon_counter,1:polygon_point_size(polygon_counter)), &
				  y_array(polygon_counter,1:polygon_point_size(polygon_counter)), local_x, &
				  local_y, polygon_point_size(polygon_counter))

				if(inside) THEN
					shear_stress_grid(local_x_counter,local_y_counter) = &
					  domain_ss_values(shear_stess_id_array(polygon_counter)+1)
				endif

			end do
		end do


	end do


	! output

	open(unit=output_file_unit, file=output_file, access="sequential", form="formatted", status="replace")
	do x_counter = 1, number_x, 1
		do y_counter = 1, number_y, 1
			local_x = minimum_x + dble(x_counter-1) * grid_spacing
			local_y = minimum_y + dble(y_counter-1) * grid_spacing

			write(output_file_unit,output_format) local_x, local_y, shear_stress_grid(x_counter,y_counter) 

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
