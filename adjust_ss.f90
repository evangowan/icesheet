program adjust_ss

	implicit none


	character(len=255) :: diff_file, domain_min_file, domain_max_file
	integer, parameter :: diff_unit = 10, domain_min_unit=20, domain_max_unit=30

	integer :: number_domains, istat, domain_id, shear_stress, counter

	integer, dimension(:), allocatable :: shear_stress_min, shear_stress_max

	integer, parameter :: minimum_shear_stress = 5000

	double precision :: difference, multiplier
	double precision, parameter :: scale_down = 0.5


	call getarg(1,diff_file)

	call getarg(2,domain_min_file)

	call getarg(3,domain_max_file)


	! the domain max file should contain all of the domain numbers
	open(file=domain_max_file, unit=domain_max_unit, status="old", access="sequential", form="formatted")

	number_domains = 0
	read_max: do

		read(domain_max_unit,*,iostat=istat) domain_id, shear_stress
		if(istat /=0) THEN
			exit read_max
		endif

		if(domain_id > number_domains) THEN
			number_domains = domain_id
		endif

	end do read_max

	rewind(domain_max_unit)

	allocate(shear_stress_min(number_domains), shear_stress_max(number_domains))

	! read in the domains, if zero it is not set

	shear_stress_max = 0
	shear_stress_min = 0

	read_max2: do

		read(domain_max_unit,*,iostat=istat) domain_id, shear_stress
		if(istat /=0) THEN
			exit read_max2
		endif

		shear_stress_max(domain_id) = shear_stress

	end do read_max2

	close(unit=domain_max_unit)

	open(file=domain_min_file, unit=domain_min_unit, status="old", access="sequential", form="formatted")

	read_min: do

		read(domain_min_unit,*,iostat=istat) domain_id, shear_stress
		if(istat /=0) THEN
			exit read_min
		endif

		shear_stress_min(domain_id) = shear_stress

	end do read_min

	close(unit=domain_min_unit)


	! go through the difference file

	open(file=diff_file, unit=diff_unit, status="old", access="sequential", form="formatted")

	read_diff: do

		read(diff_unit,*,iostat=istat) domain_id, difference
		if(istat /=0) THEN


			exit read_diff
		endif



		if (difference <= -250) THEN
			
			multiplier = -0.1*scale_down
		elseif (difference <= -100) THEN
			multiplier = -0.05*scale_down
		elseif (difference <= -50) THEN
			multiplier = -0.02*scale_down
		elseif (difference <= -20) THEN
			multiplier = -0.01*scale_down
		elseif (difference <= -10) THEN
			multiplier = -0.005*scale_down
		elseif (difference <= 10) THEN
			multiplier = 0*scale_down
		elseif (difference <= 20) THEN
			multiplier = 0.005*scale_down
		elseif (difference <= 50) THEN
			multiplier = 0.01*scale_down
		elseif (difference <= 100) THEN
			multiplier = 0.02*scale_down
		elseif (difference <= 250) THEN
			multiplier = 0.05*scale_down
		else
			multiplier = 0.1*scale_down
		endif


		if(shear_stress_max(domain_id) > 0) THEN


			shear_stress_max(domain_id) = shear_stress_max(domain_id) + nint(dble(shear_stress_max(domain_id)) * multiplier)
			if (shear_stress_max(domain_id) < minimum_shear_stress) THEN
				shear_stress_max(domain_id) = minimum_shear_stress
			endif
		endif

		if(shear_stress_min(domain_id) > 0) THEN

			shear_stress_min(domain_id) = shear_stress_min(domain_id) + nint(dble(shear_stress_min(domain_id)) * multiplier)
			if (shear_stress_min(domain_id) < minimum_shear_stress) THEN
				shear_stress_min(domain_id) = minimum_shear_stress
			endif
		endif

	end do read_diff

	close (unit=diff_unit)

	! write out the shear stress
	open(file=domain_max_file, unit=domain_max_unit, status="replace", access="sequential", form="formatted")
	open(file=domain_min_file, unit=domain_min_unit, status="replace", access="sequential", form="formatted")

	do counter = 1, number_domains

		if(shear_stress_max(counter) > 0) THEN
			write(domain_max_unit, *) counter, shear_stress_max(counter)
		endif

		if(shear_stress_min(counter) > 0) THEN
			write(domain_min_unit, *) counter, shear_stress_min(counter)
		endif


	end do

	close(unit=domain_max_unit)
	close(unit=domain_min_unit)

end program adjust_ss
