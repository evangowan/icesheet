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

program convert_grid

	! reads in shear_stress_domain_values.txt (taken from command line so that it can be changed by the user) and converts
	! domains.txt to be a grid with shear stress values

	implicit none

	character(len=255), parameter :: input_file = "domains.txt", output_file = "domains_ss.txt"

	character(len=255) :: domain_values_file
	character(len=50), parameter :: input_format = "(F12.3,1X,F12.3,1X,I5)", output_format = "(F12.3,1X,F12.3,1X,F8.1)"

	integer, parameter :: domain_values_file_unit = 10, input_unit=20, output_unit=30
	integer :: number_domains, istat, domain
	
	double precision :: domain_shear_stress, longitude, latitude

	integer, parameter :: max_domains = 5000 ! if you have more than 5000 domains, you will need to increase this and recompile

	double precision, dimension(max_domains) :: domain_value_array
	logical, dimension(max_domains) :: domain_value_mask

	double precision, parameter :: zero_domain = 50000. ! this is the default shear stress value for areas outside the boundary.


	call getarg(1,domain_values_file)

	! open the parameter file

	number_domains = 0

	domain_value_array = zero_domain

	domain_value_mask = .false.

	open(unit=domain_values_file_unit, file=domain_values_file, access="sequential", form="formatted", status="old")

	initial_read: do

		read(domain_values_file_unit,*, iostat=istat) domain, domain_shear_stress
		if(istat /=0) THEN !hopefully end of file
			exit initial_read
		endif

		if(domain > max_domains) THEN
			write(6,*) "number of domains exceeds internal maximum"
			write(6,*) "increase the value in convert_grid.f90 and recompile"
			stop
		endif

		domain_value_array(domain) = domain_shear_stress
		domain_value_mask(domain) = .true.



	end do initial_read

	close(unit = domain_values_file_unit)

	! next open the input and output grids

	open(unit=input_unit, file=input_file, access="sequential", form="formatted", status="old")
	open(unit=output_unit, file=output_file, access="sequential", form="formatted", status="replace")


	! read in the input file, convert the domain value to shear stress value, and then write out

	second_read: do

		read(input_unit,input_format, iostat=istat) longitude, latitude, domain
		if(istat /=0) THEN !hopefully end of file
			exit second_read
		endif

		if(domain > 0) THEN
			if(domain_value_mask(domain)) then
				write(output_unit, output_format) longitude, latitude, domain_value_array(domain)
			else
				write(6,*) "domain number ", domain, "is not in the input domain id file!"
				stop
			endif

		else
			write(output_unit, output_format) longitude, latitude, zero_domain
		endif

	end do second_read

	close(unit=input_unit)
	close(unit=output_unit)

end program convert_grid
