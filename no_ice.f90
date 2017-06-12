! program to identify if a shear stress domain has ice on it or not

program no_ice

! requires two inputs: the shear stress file and the margin file

	use global_parameters
	implicit none

	integer :: number_polygons_margins, istat, maximum_points, point_counter, domain_id, shear_stress, max_domain_id
	integer :: number_polygons_ss, ss_polygon_counter, margin_polygon_counter, counter
	integer :: grid_spacing, minimum_y, minimum_x, maximum_x, maximum_y
	integer, parameter :: margin_unit = 10, ss_unit = 20, max_polygons = 10000, out_unit=30, grid_parameters_unit=40
	integer, dimension(max_polygons) :: polygon_point_size_margins, polygon_domain_id, polygon_point_size_ss

	character(len=1) :: bracket
	character(len=255) ::  ss_file, margin_file, dummy, info, bin_file
	character(len=256), parameter :: out_file = "no_ice.txt"
	character(len=255), parameter :: grid_parameters_file="ss_parameters.txt" ! parameter file used in ICESHEET

	double precision :: x, y, x_offset, y_offset

	double precision, allocatable, dimension(:) :: y_min_margins, x_min_margins,y_max_margins, x_max_margins
	double precision, allocatable, dimension(:,:) :: y_array_margins, x_array_margins

	double precision, allocatable, dimension(:) :: y_min_ss, x_min_ss,y_max_ss, x_max_ss
	double precision, allocatable, dimension(:,:) :: y_array_ss, x_array_ss
	logical, allocatable, dimension(:) :: inside_ss, domain_id_status

	call getarg(1,ss_file)
	call getarg(2,margin_file)

	if(ss_file == "") THEN
		write(6,*) "you need to include the shear stress GMT formatted polygon file as a command line argument"
		stop
	else
		write(6,*) "Shear Stress polygon file: ", trim(ss_file)
	end if

	if(margin_file == "") THEN
		write(6,*) "you need to include the ice margin GMT formatted polygon file as a command line argument"
		stop

	else
		write(6,*) "Ice margin polygon file: ", trim(margin_file)

	endif

	! read in the ice margin file

	number_polygons_margins = 0

	open(unit=margin_unit, file=margin_file, access="sequential", form="formatted", status="old")

	initial_read: do

		read(margin_unit,'(A1)', iostat=istat) bracket
		if(istat /=0) THEN !hopefully end of file
			exit initial_read
		endif

		if(bracket == ">") then
			number_polygons_margins = number_polygons_margins + 1

			if(number_polygons_margins > max_polygons) THEN
				write(6,*) "you have to increase the max_polygons parameter in no_ice.f90, and recompile"
				stop
			endif

		else if (bracket == "#") then
          		! skip
		else
			polygon_point_size_margins(number_polygons_margins) = polygon_point_size_margins(number_polygons_margins) + 1
		endif

	end do initial_read

	
	rewind(unit=margin_unit)

	maximum_points = maxval(polygon_point_size_margins)

	allocate(y_array_margins(number_polygons_margins,maximum_points), x_array_margins(number_polygons_margins,maximum_points),&
		x_min_margins(number_polygons_margins), y_min_margins(number_polygons_margins),&
		x_max_margins(number_polygons_margins), y_max_margins(number_polygons_margins), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating arrays"
		stop
	endif



	do margin_polygon_counter = 1, number_polygons_margins, 1

		break_out: do
			read(margin_unit,*) bracket

			if(bracket == ">" .or. bracket == "#") then
				cycle break_out
			else
			!	write(6,*) "should backspace"
				backspace (unit=margin_unit, iostat=istat) 
				if(istat /= 0) then
					write(6,*) "error in backspace: ", istat
					stop
				endif
			!	read(margin_unit,*) x, y
			!	write(6,*) x, y
			!	backspace(margin_unit)
				exit break_out
			end if

		end do break_out


		do point_counter = 1, polygon_point_size_margins(margin_polygon_counter), 1


			read(margin_unit,*) x_array_margins(margin_polygon_counter,point_counter), &
							y_array_margins(margin_polygon_counter,point_counter)
		!	write(6,*) point_counter, x_array_margins(margin_polygon_counter,point_counter), &
		!					y_array_margins(margin_polygon_counter,point_counter)
		end do

		! to reduce computation load, find the minimum and maximum x and y values

		x_min_margins(margin_polygon_counter) = &
		   minval(x_array_margins(margin_polygon_counter,1:polygon_point_size_margins(margin_polygon_counter)))
		y_min_margins(margin_polygon_counter) = &
		   minval(y_array_margins(margin_polygon_counter,1:polygon_point_size_margins(margin_polygon_counter)))
		x_max_margins(margin_polygon_counter) = &
		   maxval(x_array_margins(margin_polygon_counter,1:polygon_point_size_margins(margin_polygon_counter)))
		y_max_margins(margin_polygon_counter) = &
		   maxval(y_array_margins(margin_polygon_counter,1:polygon_point_size_margins(margin_polygon_counter)))

	end do

	close(margin_unit)



	! read in the shear stress file


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


	number_polygons_ss = 0
	polygon_point_size_ss = 0


	max_domain_id = 0
	open(unit=ss_unit, file=ss_file, access="sequential", form="formatted", status="old")

	initial_read2: do

		read(ss_unit,'(A1)', iostat=istat) bracket
		if(istat /=0) THEN !hopefully end of file
			exit initial_read2
		endif

		if(bracket == ">") then
			number_polygons_ss = number_polygons_ss + 1

			if(number_polygons_ss > max_polygons) THEN
				write(6,*) "you have to increase the max_polygons parameter in no_ice.f90, and recompile"
				stop
			endif

			! the QGIS output file will immediately afterwards have a line containing the domain number and shear stress

			read(ss_unit,*) dummy, info
			! replace the | character with spaces
			do counter=1,len_trim(info)
   				if (info(counter:counter) == "|") info(counter:counter) = " "
			end do

			read(info,*) dummy, domain_id, shear_stress

			if(domain_id > max_domain_id) THEN
				max_domain_id = domain_id
			end if


			polygon_domain_id(number_polygons_ss) = domain_id

		else if (bracket == "#") then
          		! skip
		else
			polygon_point_size_ss(number_polygons_ss) = polygon_point_size_ss(number_polygons_ss) + 1
		endif

	end do initial_read2

	rewind(unit=ss_unit)


	maximum_points = maxval(polygon_point_size_ss)

	allocate(y_array_ss(number_polygons_ss,maximum_points), x_array_ss(number_polygons_ss,maximum_points),&
		x_min_ss(number_polygons_ss), y_min_ss(number_polygons_ss),&
		x_max_ss(number_polygons_ss), y_max_ss(number_polygons_ss), inside_ss(number_polygons_ss), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating arrays"
		stop
	endif



	! read in the polygons


	do ss_polygon_counter = 1, number_polygons_ss, 1
		!write(6,*) ss_polygon_counter

		break_out2: do
			read(ss_unit,*) bracket
			if(bracket == ">" .or. bracket == "#") then
				cycle break_out2
			else
				backspace ss_unit
				exit break_out2
			end if

		end do break_out2

		do point_counter = 1, polygon_point_size_ss(ss_polygon_counter), 1

			read(ss_unit,*) x_array_ss(ss_polygon_counter,point_counter), &
							y_array_ss(ss_polygon_counter,point_counter)

			x_array_ss(ss_polygon_counter,point_counter) = x_array_ss(ss_polygon_counter,point_counter) + x_offset
			y_array_ss(ss_polygon_counter,point_counter) = y_array_ss(ss_polygon_counter,point_counter) + y_offset



		end do

		x_min_ss(ss_polygon_counter) = minval(x_array_ss(ss_polygon_counter,1:polygon_point_size_ss(ss_polygon_counter)))
		y_min_ss(ss_polygon_counter) = minval(y_array_ss(ss_polygon_counter,1:polygon_point_size_ss(ss_polygon_counter)))
		x_max_ss(ss_polygon_counter) = maxval(x_array_ss(ss_polygon_counter,1:polygon_point_size_ss(ss_polygon_counter)))
		y_max_ss(ss_polygon_counter) = maxval(y_array_ss(ss_polygon_counter,1:polygon_point_size_ss(ss_polygon_counter)))
	end do

	close(ss_unit)

	inside_ss = .false.


	! it should be quicker to check which margin points are inside shear stress polygons, because the shear stress polygons have
	! far less points (or they usually should)

	ss_loop: do ss_polygon_counter = 1, number_polygons_ss, 1

		do margin_polygon_counter = 1, number_polygons_margins

			do point_counter = 1, polygon_point_size_margins(margin_polygon_counter), 1

				inside_ss(ss_polygon_counter) = &
				  point_in_polygon(x_array_ss(ss_polygon_counter,1:polygon_point_size_ss(ss_polygon_counter)), &
				  y_array_ss(ss_polygon_counter,1:polygon_point_size_ss(ss_polygon_counter)), &
				  x_array_margins(margin_polygon_counter,point_counter), &
				  y_array_margins(margin_polygon_counter,point_counter), polygon_point_size_ss(ss_polygon_counter))

				if(inside_ss(ss_polygon_counter)) THEN ! confirmed that it is inside
			!		write(6,*) "primary: ", ss_polygon_counter, polygon_domain_id(ss_polygon_counter), margin_polygon_counter
					cycle ss_loop
				endif

			end do


		end do
	end do ss_loop


	! now do the opposite

	margin_loop: do margin_polygon_counter = 1, number_polygons_margins

		ss_loop2: do ss_polygon_counter = 1, number_polygons_ss, 1

			if(.not. inside_ss(ss_polygon_counter)) THEN ! check
				if(x_min_ss(ss_polygon_counter) < x_max_margins(margin_polygon_counter) .and. &
				   x_max_ss(ss_polygon_counter)  > x_min_margins(margin_polygon_counter) .and. &
				   y_min_ss(ss_polygon_counter) < y_max_margins(margin_polygon_counter) .and. &
				   y_max_ss(ss_polygon_counter)  > y_min_margins(margin_polygon_counter) ) THEN ! possible for this to be inside 

					do point_counter = 1, polygon_point_size_ss(ss_polygon_counter), 1


						inside_ss(ss_polygon_counter) = &
					  	  point_in_polygon(&
						   x_array_margins(margin_polygon_counter,1:polygon_point_size_margins(margin_polygon_counter)), &
					 	   y_array_margins(margin_polygon_counter,1:polygon_point_size_margins(margin_polygon_counter)), &
					  	   x_array_ss(ss_polygon_counter,point_counter), &
					  	   y_array_ss(ss_polygon_counter,point_counter), polygon_point_size_margins(margin_polygon_counter))

						if(inside_ss(ss_polygon_counter)) THEN

						!	write(6,*) "secondary: ", ss_polygon_counter, polygon_domain_id(ss_polygon_counter), &
						!		margin_polygon_counter
							cycle ss_loop2
						endif
					end do
				end if

			endif

		end do ss_loop2

	end do margin_loop

	! print results

	allocate(domain_id_status(maxval(polygon_domain_id(1:number_polygons_ss))), stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem allocating array"
		stop
	endif

	domain_id_status = .false.
	do ss_polygon_counter = 1, number_polygons_ss, 1

		if(inside_ss(ss_polygon_counter)) THEN

			domain_id_status(polygon_domain_id(ss_polygon_counter)) = .true.

		endif

	end do


	open(unit=out_unit, file=out_file, access="sequential", form="formatted", status="replace")

	do counter = 1, maxval(polygon_domain_id(1:number_polygons_ss)), 1

		if(.not.domain_id_status(counter)) THEN

			write(out_unit,*) counter

		end if
	
	end do
	close(unit=out_unit)


	deallocate(y_array_margins, x_array_margins,x_min_margins, y_min_margins,&
		x_max_margins, y_max_margins,y_array_ss, x_array_ss, x_min_ss, y_min_ss,&
		x_max_ss, y_max_ss, inside_ss, stat=istat)
	if(istat /=0) THEN
		write(6,*) "problem deallocating arrays"
		stop
	endif

end program no_ice
