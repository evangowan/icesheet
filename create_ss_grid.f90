program create_ss_grid

	! reads in a GMT formatted file containing the shear stress domains, output from QGIS,
        ! and creates a regular grid from a given input file.


	! heavily modified from the version of create_ss_grid that I was using before

	use global_parameters
	! reads in the file with polygons "gmt_file.txt" and creates a grid with a given resolution set by the user
	implicit none

	integer :: grid_spacing, minimum_y, minimum_x, maximum_x, maximum_y ! in the original version of this program, 
 	double precision :: x, y
	double precision :: local_minimum_x, local_maximum_x, local_minimum_y, local_maximum_y
	double precision :: local_x, local_y

	character(len=255) :: input_parameter, dummy, bin_file, polygon_file, domain_max_file, domain_adjust_file
	character(len=255), parameter ::  output_file = "domains.txt"
	character(len=255), parameter :: grid_parameters_file="ss_parameters.txt" ! parameter file used in ICESHEET

	character(len=1) :: bracket

	integer :: istat, number_polygons, ss_polygon_counter, point_counter, maximum_points, number_y, number_x
	integer :: start_y_index, start_x_index, end_y_index, end_x_index
	integer :: local_y_counter, local_x_counter, y_counter, x_counter
	integer, parameter :: polygon_file_unit=10, max_polygons = 10000, output_file_unit=20, grid_parameters_unit=30

	integer, dimension(max_polygons) :: polygon_point_size ! if you have more than 10000 polygons, you are ambitious!

	double precision, allocatable, dimension(:,:) :: y_array, x_array
	integer, allocatable, dimension(max_polygons) :: shear_stress_value_array
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
	endif

	stop
	! the program expects a file called ss_parameters.txt that contains the grid parameters, x and y min/max and grid spacing

	open(unit=grid_parameters_unit, file=grid_parameters_file, access="sequential", form="formatted", status="old")
	read(grid_parameters_unit,*) bin_file ! not actually used at this point, will be created afterwards with GMT
	read(grid_parameters_unit,*) minimum_x
	read(grid_parameters_unit,*) maximum_x
	read(grid_parameters_unit,*) minimum_y
	read(grid_parameters_unit,*) maximum_y
	read(grid_parameters_unit,*) grid_spacing

	close(unit=grid_parameters_unit)




	! first, find the number of polygons, and the maximum amount of points

	number_polygons = 0
	polygon_point_size = 0

	shear_stress_value_array = nominal_shear_stress

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




	do ss_polygon_counter = 1, number_polygons, 1

		read(polygon_file_unit,*) bracket, shear_stress_value_array(ss_polygon_counter)


		do point_counter = 1, polygon_point_size(ss_polygon_counter), 1

			read(polygon_file_unit,*) x_array(ss_polygon_counter,point_counter), &
						      y_array(ss_polygon_counter,point_counter)



		end do
	end do

	close(polygon_file_unit)


	! expand the region a bit

!	minimum_x = dble(floor(minimum_x/grid_spacing))*grid_spacing - 2. * grid_spacing
!	maximum_x = dble(ceiling(maximum_x/grid_spacing))*grid_spacing + 2. * grid_spacing

!	minimum_y = dble(floor(minimum_y/grid_spacing))*grid_spacing - 2. * grid_spacing
!	maximum_y = dble(ceiling(maximum_y/grid_spacing))*grid_spacing + 2. * grid_spacing




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

	grid = 0

	do ss_polygon_counter = 1, number_polygons, 1


		local_minimum_x = minval(x_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)))
		local_maximum_x = maxval(x_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)))

		local_minimum_y = minval(y_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)))
		local_maximum_y = maxval(y_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)))


		local_minimum_x = dble(floor(local_minimum_x/grid_spacing)) * grid_spacing
		local_maximum_x = dble(ceiling(local_maximum_x/grid_spacing)) * grid_spacing

		local_minimum_y = dble(floor(local_minimum_y/grid_spacing)) * grid_spacing
		local_maximum_y = dble(ceiling(local_maximum_y/grid_spacing)) * grid_spacing

		start_y_index = nint((local_minimum_y - minimum_y)/grid_spacing) + 1
		start_x_index = nint((local_minimum_x - minimum_x)/grid_spacing) + 1
		end_y_index = nint((local_maximum_y - minimum_y)/grid_spacing) + 1
		end_x_index = nint((local_maximum_x - minimum_x)/grid_spacing) + 1
		
		do  local_x_counter = start_x_index, end_x_index
			do local_y_counter = start_y_index, end_y_index

				
				local_x = minimum_x + dble(local_x_counter-1) * grid_spacing
				local_y = minimum_y + dble(local_y_counter-1) * grid_spacing

				inside = point_in_polygon(x_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)), &
				  y_array(ss_polygon_counter,1:polygon_point_size(ss_polygon_counter)), local_x, &
				  local_y, polygon_point_size(ss_polygon_counter))

				if(inside) THEN
					grid(local_x_counter,local_y_counter) = shear_stress_value_array(ss_polygon_counter)
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

			write(output_file_unit,output_format) local_x, local_y, grid(x_counter,y_counter) 

		end do
	end do

	close(output_file_unit)


	deallocate(y_array, x_array,shear_stress_value_array, stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem deallocating arrays"
		stop
	endif



end program create_ss_grid
