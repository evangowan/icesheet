program create_ss_grid

	! reads in a GMT formatted file containing the shear stress domains, output from QGIS,
        ! and creates a regular grid from a given input file.


	! heavily modified from the version of create_ss_grid that I was using before

	use global_parameters
	! reads in the file with polygons "gmt_file.txt" and creates a grid with a given resolution set by the user
	implicit none

	integer :: grid_spacing, minimum_y, minimum_x, maximum_x, maximum_y, x_int, y_int
 	double precision :: x, y, x1, y1, x2, y2, slope, intercept, x_offset, y_offset
	integer :: local_minimum_x, local_maximum_x, local_minimum_y, local_maximum_y
	double precision :: local_x, local_y

	character(len=255) :: input_parameter, dummy, bin_file, polygon_file, domain_max_file, domain_adjust_file, info
	character(len=255), parameter ::  output_domains_file = "domains.txt"
	character(len=255), parameter :: grid_parameters_file="ss_parameters.txt" ! parameter file used in ICESHEET
	character(len=255), parameter :: output_grid_file = "shear_stress_grid.txt"

	character(len=1) :: bracket

	integer :: istat, number_polygons, ss_polygon_counter, point_counter, maximum_points, number_y, number_x, counter
	integer :: start_y_index, start_x_index, end_y_index, end_x_index, domain_id, shear_stress, max_domain_id, current_time
	integer :: local_y_counter, local_x_counter, y_counter, x_counter
	integer :: time_of_maximum_ss, time_of_minimum_ss, minimum_ss

	integer, parameter :: polygon_file_unit=10, max_polygons = 10000, output_domains_file_unit=20, grid_parameters_unit=30
	integer, parameter :: shear_stress_max_unit=40, adjust_unit=50, output_grid_unit = 60

	integer, dimension(max_polygons) :: polygon_point_size, polygon_domain_id ! if you have more than 10000 polygons, you are ambitious!

	double precision, allocatable, dimension(:,:) :: y_array, x_array
	integer, dimension(max_polygons) :: shear_stress_value_array
	integer, allocatable, dimension(:,:) :: grid

	logical :: inside, use_max_file, use_adjust_file

	integer, parameter :: nominal_shear_stress = 5000 ! default shear stress value

	character(len=50), parameter :: output_format = "(I9,1X,I9,1X,I6)"

	! requires up to three files from the command line, the GMT file is manditory

	call getarg(1,polygon_file)
!	read(input_parameter,*) 
	call getarg(2,domain_max_file)
!	read(input_parameter,*) 
	call getarg(3,domain_adjust_file)
!	read(input_parameter,*) 

	if(polygon_file == "") THEN
		write(6,*) "you need to include the shear stress GMT formatted polygon file as a command line argument"
		stop
	end if

	if(domain_max_file == "") THEN
		write(6,*) "using default shear stress values from the GMT file"
		use_max_file = .false.
	else
		write(6,*) "using user derived shear stress values"
		use_max_file = .true.
	endif

	if(domain_adjust_file == "") THEN
		write(6,*) "no time variable adjustments to shear stress requested"
		use_adjust_file = .false.
	else
		write(6,*) "using user derived time variable shear stress adjustments"
		use_adjust_file = .true.

		! read in the time value (in thosands of years before present so it is an integer)

		call getarg(4,dummy)

		read(dummy,*) current_time
	endif

	
	! the program expects a file called ss_parameters.txt that contains the grid parameters, x and y min/max and grid spacing

	open(unit=grid_parameters_unit, file=grid_parameters_file, access="sequential", form="formatted", status="old")
	read(grid_parameters_unit,*) bin_file ! not actually used at this point, will be created afterwards with GMT
	read(grid_parameters_unit,*) minimum_x
	read(grid_parameters_unit,*) maximum_x
	read(grid_parameters_unit,*) minimum_y
	read(grid_parameters_unit,*) maximum_y
	read(grid_parameters_unit,*) grid_spacing

	! if there is an offset to the grid (i.e. QGIS had the projection center at the center coordinate using the Lambert projection, while 
	! it is more convenient to use the bottom left corner as is default in GMT to keep it a regular grid
	! if there is no offset, do not include it in ss_parameters.txt

	read(grid_parameters_unit,*, iostat=istat)  x_offset, y_offset
	if(istat /= 0) THEN
		x_offset = 0.0
		y_offset = 0.0
	endif

	close(unit=grid_parameters_unit)




	! first, find the number of polygons, and the maximum amount of points

	number_polygons = 0
	polygon_point_size = 0

	shear_stress_value_array = nominal_shear_stress
	max_domain_id = 0
	open(unit=polygon_file_unit, file=polygon_file, access="sequential", form="formatted", status="old")

	initial_read: do

		read(polygon_file_unit,'(A1,A)', iostat=istat) bracket, dummy
		if(istat /=0) THEN !hopefully end of file
			exit initial_read
		endif

		if(bracket == ">") then
			number_polygons = number_polygons + 1

			if(number_polygons > max_polygons) THEN
				write(6,*) "you have to increase the max_polygons parameter in create_ss_grid.f90, and recompile"
				stop
			endif

			! the QGIS output file will immediately afterwards have a line containing the domain number and shear stress

			read(polygon_file_unit,*) dummy, info
			! replace the | character with spaces
			do counter=1,len_trim(info)
   				if (info(counter:counter) == "|") info(counter:counter) = " "
			end do

			read(info,*) dummy, domain_id, shear_stress

			if(domain_id > max_domain_id) THEN
				max_domain_id = domain_id
			end if

			shear_stress_value_array(domain_id) = shear_stress
			polygon_domain_id(number_polygons) = domain_id

		else if (bracket == "#") then
          		! skip
		else
			polygon_point_size(number_polygons) = polygon_point_size(number_polygons) + 1
		endif

	end do initial_read

	rewind(unit=polygon_file_unit)

	maximum_points = maxval(polygon_point_size)

	allocate(y_array(number_polygons,maximum_points), x_array(number_polygons,maximum_points),&
		 stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating arrays"
		stop
	endif



	! read in the polygons


	do ss_polygon_counter = 1, number_polygons, 1
		!write(6,*) ss_polygon_counter

		break_out: do
			read(polygon_file_unit,*) bracket, dummy
			if(bracket == ">" .or. bracket == "#") then
				cycle break_out
			else
				backspace polygon_file_unit
				exit break_out
			end if

		end do break_out

		do point_counter = 1, polygon_point_size(ss_polygon_counter), 1

			read(polygon_file_unit,*) x_array(ss_polygon_counter,point_counter), &
							y_array(ss_polygon_counter,point_counter)

			x_array(ss_polygon_counter,point_counter) = x_array(ss_polygon_counter,point_counter) + x_offset
			y_array(ss_polygon_counter,point_counter) = y_array(ss_polygon_counter,point_counter) + y_offset

		end do
	end do

	close(polygon_file_unit)


	if(use_max_file) THEN
		open(unit=shear_stress_max_unit, file=domain_max_file, access="sequential", form="formatted", status="old")

		read_max: do

			read(shear_stress_max_unit,*, iostat=istat)  domain_id, shear_stress
			if(istat /=0) THEN
				exit read_max
			endif	

			shear_stress_value_array(domain_id) = shear_stress
		end do read_max
		close(shear_stress_max_unit)
		
	end if


	if(use_adjust_file) THEN 

		! adjust the maximum shear stress values based on a format
		! time_of_maximum_ss time_of_minimum_ss minimum_ss

		! this will linearly go between those times and adjust the shear stress based on these values, the minimum time should probably represent an ice free time
		! or zero time

		open(unit=adjust_unit, file=domain_adjust_file, access="sequential", form="formatted", status="old")

		read_adjust: do

			read(adjust_unit,*, iostat=istat) domain_id, time_of_maximum_ss, time_of_minimum_ss, minimum_ss ! must be integers
			if(istat /=0) THEN
				exit read_adjust
			endif	

			! are adjustments to the current time necessary?

			if(current_time >= min(time_of_maximum_ss,time_of_minimum_ss) .and. &
      		   current_time <= max(time_of_maximum_ss,time_of_minimum_ss)) THEN

				! originally I thought of making the decay a 
				x1 = dble(time_of_minimum_ss)
				y1 = dble(minimum_ss)
				x2 = dble(time_of_maximum_ss)
				y2 = dble(shear_stress_value_array(domain_id))

				slope = (y2 - y1) / (x2 - x1)
				intercept = y1 - slope * x1


				shear_stress_value_array(domain_id) = nint(slope * current_time + intercept)

			end if

		end do read_adjust

		close(adjust_unit)

	end if


	! write out the domains file, not really used for anything else, just for checking

	open(unit=output_domains_file_unit, file=output_domains_file, access="sequential", form="formatted", status="replace")

	do counter = 1, max_domain_id, 1

		write(output_domains_file_unit,*) counter, shear_stress_value_array(counter)

	end do


	! allocate the output grid
	number_y = nint(dble(maximum_y - minimum_y) / dble(grid_spacing)) + 1
	number_x = nint(dble(maximum_x - minimum_x) / dble(grid_spacing)) + 1


	allocate(grid(number_x,number_y), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating arrays"
		stop
	endif

	! note that it is entirely possible that polgons will be skipped if your lat/long spacing is not dense enough

	! also, if your input shapefile has any gaps between polygons, they could also be skipped. 
	! If they polygons overlap, the values could be overwritten

	grid = nominal_shear_stress

	cycle_polygon: do ss_polygon_counter = 1, number_polygons, 1


		local_minimum_x = int(minval(x_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)))/dble(grid_spacing))&
		  *grid_spacing
		local_maximum_x = ceiling(maxval(x_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)))/dble(grid_spacing))&
		  *grid_spacing

		local_minimum_y = int(minval(y_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)))/dble(grid_spacing))&
		  *grid_spacing
		local_maximum_y = ceiling(maxval(y_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)))/dble(grid_spacing))&
		  *grid_spacing


		start_y_index = (local_minimum_y - minimum_y)/grid_spacing + 1
		start_x_index = (local_minimum_x - minimum_x)/grid_spacing + 1
		end_y_index = (local_maximum_y - minimum_y)/grid_spacing + 1
		end_x_index = (local_maximum_x - minimum_x)/grid_spacing + 1

		if(end_x_index < 1 .or. start_x_index > number_x .or. end_y_index < 1 .or. start_y_index > number_y) THEN ! polygon is outside of the grid
			cycle cycle_polygon
		end if

		if (start_x_index < 1) THEN
			start_x_index = 1
		endif
		if (end_x_index > number_x) THEN
			end_x_index = number_x
		endif

		if (start_y_index < 1) THEN
			start_y_index = 1
		endif
		if (end_y_index > number_y) THEN
			end_y_index = number_y
		endif
		
		do  local_x_counter = start_x_index, end_x_index
			do local_y_counter = start_y_index, end_y_index

				
				local_x = dble(minimum_x + (local_x_counter-1) * grid_spacing)
				local_y = dble(minimum_y + (local_y_counter-1) * grid_spacing)

				inside = point_in_polygon(x_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)), &
				  y_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)), local_x, &
				  local_y, polygon_point_size(ss_polygon_counter))

				if(inside) THEN
					grid(local_x_counter,local_y_counter) = shear_stress_value_array(polygon_domain_id(ss_polygon_counter))
				endif

			end do
		end do


	end do cycle_polygon


	! output

	open(unit=output_grid_unit, file=output_grid_file, access="sequential", form="formatted", status="replace")
	do x_counter = 1, number_x, 1
		do y_counter = 1, number_y, 1
			x_int = minimum_x + (x_counter-1) * grid_spacing
			y_int = minimum_y + (y_counter-1) * grid_spacing

			write(output_grid_unit,output_format) x_int, y_int, grid(x_counter,y_counter) 

		end do
	end do

	close(output_grid_unit)


	deallocate(y_array, x_array, grid, stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem deallocating arrays"
		stop
	endif

	write(6,*) "finished create_ss_grid"

end program create_ss_grid
