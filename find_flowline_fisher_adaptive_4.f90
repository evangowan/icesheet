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


SUBROUTINE find_flowline()

! code written by Evan J. Gowan
! http://www.raisedbeaches.net

! note, if the code seems a bit sloppy, it should be kept in mind that various parts were written between the start of 2011 
! (when I was still a novice programmer) until the current release,which is:

! January 11, 2015

	use global_parameters
	use grids
	
	implicit none

! needed variables


	double precision :: E, ss, min_ss, distance_between_points, L, min_H, min_E


	integer :: initializing_counter, istat, current_step, min_index, number_to_check, boundary_counter
	integer :: check_counter 

	integer, dimension(2) :: check_index_values

! allocated arrays for find_flowline

	double precision, allocatable, dimension(:) :: E_array, q_array, x_contour, y_contour, ss_temp, base_topo_temp
	logical, allocatable, dimension(:) :: skip_point, check_mask


	integer :: test_min_x, test_max_x, test_min_y, test_max_y
	integer :: test_interval, test_x_counter, test_y_counter

	
	write(6,*) "entered find_flowline"

! if you are having problems with your elevation and shear stress grids, uncomment this code for diagnosis

!	test_interval = 5000

!	test_min_x = ceiling(minval(x_distance)/ dble(test_interval)) * test_interval
!	test_max_x = floor(maxval(x_distance) / dble(test_interval)) * test_interval

!	test_min_y = ceiling(minval(y_distance)/ dble(test_interval)) * test_interval
!	test_max_y = floor(maxval(y_distance) / dble(test_interval)) * test_interval

!	open(unit=888, file="test_gridding.txt", status="replace")

!	do test_x_counter = test_min_x, test_max_x, test_interval
!		do test_y_counter = test_min_y, test_max_y, test_interval

!			ss = get_tau(dble(test_x_counter),dble(test_y_counter))
!			E = elevation(dble(test_x_counter),dble(test_y_counter))
!			write(888,*) test_x_counter, test_y_counter, ss, E
!		end do
!	end do
!	close(unit=888)
!	stop


!	allocate the arrays


	allocate( E_array(number_boundary_points), &
		   q_array(number_boundary_points), x_contour(number_boundary_points),  y_contour(number_boundary_points), &
		   skip_point(number_boundary_points), ss_temp(number_boundary_points), check_mask(number_boundary_points),&
			base_topo_temp(number_boundary_points),stat = istat)
	if (istat /= 0) THEN
		write(6,*) "allocation error for arrays in find_flowline"
		stop
	ENDIF
	
	skip_point = .false.


! 	initialize the boundary conditions


	check_mask = .false.


	initialize_parameters: do initializing_counter = 1, number_boundary_points, 1


		x_contour(initializing_counter) = x_distance(initializing_counter)
		y_contour(initializing_counter) = y_distance(initializing_counter)

		E = elevation(x_distance(initializing_counter),y_distance(initializing_counter)) 
		base_topo_temp(initializing_counter) = E
		ss = get_tau(x_distance(initializing_counter),y_distance(initializing_counter)) 
		ss_temp(initializing_counter) = ss
		if (E < minimum_elevation ) THEN ! below sea level
			! assume that this is grounded ice at this point, and perfectly balanced with the water column

			! In reality, this would probably be attached to an ice shelf, so this is a crude approximation
			E = elevation(x_contour(initializing_counter),y_contour(initializing_counter)) * (1-rho_water/rho_ice  ) 
			
			check_mask(initializing_counter) = .true.
		endif

		E_array(initializing_counter) = E + minimum_thickness

		! note that q = 0 is not strictly true, but at the margin, it probably is pretty close because ice flow direction should be
		! roughly perpendicular to the margin
		q_array(initializing_counter) = 0.
 
	end do initialize_parameters



	! there needs to be a check to make sure that if the ice is offshore, that the elevation between two adjacent points is actually possible
	! to do this, you need to take the basal shear stress at adjacent points, and do the Nye equation. If the elevation gradient is too high,
	! the elevation at the lower point needs to be increased. Start at the lowest elevation, which will have the starting elevation


	number_to_check = count(check_mask)

	check_boundary: do boundary_counter = 1, number_to_check, 1

		min_index = minloc(base_topo_temp,1,check_mask)

		if(min_index == number_boundary_points) THEN
			check_index_values(1) = 1
		else
			check_index_values(1) = min_index+1
		endif

		if(min_index == 1) THEN
			check_index_values(2) = number_boundary_points
		else
			check_index_values(2) = min_index-1
		endif

		do check_counter = 1, 2, 1
			! check needs to be with the smaller shear stress value, which will have a lower profile
			min_ss = min(ss_temp(min_index),ss_temp(check_index_values(check_counter))) 

			distance_between_points = sqrt((x_contour(min_index)-x_contour(check_index_values(check_counter)))**2 + &
			  (y_contour(min_index)-y_contour(check_index_values(check_counter)))**2)

			! Nye formula for parabolic ice sheet if shear stress is uniform, L is the distance from the hypothetical center of the ice sheet to the margin
			! see Equation 4, Chapter 11 in Paterson, The Physics of Glaciers, Third Edition
			L =  (E_array(min_index)-base_topo_temp(min_index))**2 *  (rho_ice * g) / (2. * min_ss )

			! with L known, you can calculate the minimum elevation needed at the secondary point to conform to the minimum shear stress

			min_H = sqrt( 2. * min_ss / (rho_ice * g) * (L - distance_between_points))
			min_E = base_topo_temp(check_index_values(check_counter)) + min_H
			if(E_array(check_index_values(check_counter)) < min_E) THEN
				
				E_array(check_index_values(check_counter))  = min_E
			endif

		end do


	end do check_boundary

	deallocate(ss_temp, check_mask,base_topo_temp,stat = istat)
	if (istat /= 0) THEN
		write(6,*) "allocation error for arrays in find_flowline"
		stop
	ENDIF

      ! do the contours as a recursive subroutine.

	current_step = int(minval(E_array) / elevation_interval) 


	! open output files

	open(unit=500, file="contours.txt", access="sequential", form="formatted", status="replace")
	open(unit=501, file="contours-rejected.txt", access="sequential", form="formatted", status="replace")
	open(unit=502, file="oversample_points.txt", access="sequential", form="formatted", status="replace")

	write(6,*) "entering calculate_polygon5 for the first time"
	polygon_counter = 0
	call calculate_polygon5(number_boundary_points, E_array, x_contour, y_contour,interior_direction, current_step, skip_point)

	close(unit=500)
	close(unit=501)
	close(unit=502)

	deallocate(E_array, q_array, x_contour,  y_contour, stat = istat)
	if (istat /= 0) THEN
		write(6,*) "deallocation error for arrays in find_flowline"
		stop
	ENDIF

	RETURN
contains

recursive subroutine calculate_polygon5(current_boundary_points, E_array, x_contour, y_contour, &
				     interior_direction_current, current_step, skip_point)

	! this subroutine is recursive, because inevitably a polygon will pinch off, and I thought it would be easier to call the subroutine for each
	! polygon rather than create some sort of looping system each time a new polygon is formed

	use global_parameters
	implicit none

	integer, intent(inout) :: current_boundary_points, current_step
	double precision, dimension(current_boundary_points), intent(inout) ::  E_array, x_contour, y_contour, &
										 interior_direction_current
	logical, dimension(current_boundary_points), intent(inout) ::  skip_point
	double precision, dimension(:), allocatable :: x_contour_temp, y_contour_temp, interior_direction_temp_new, E_temp_new
	double precision, dimension(:), allocatable :: q_array_temp, x_contour_temp2, y_contour_temp2, interior_direction_temp2
	double precision, dimension(:), allocatable ::  x_contour_temp3, y_contour_temp3, interior_direction_temp3
	double precision, dimension(:), allocatable :: E_temp2, E_temp3
	logical, dimension(:), allocatable :: skip_point_temp, skip_point_temp2, skip_point_temp3

	double precision, dimension(:), allocatable :: x_contour_last, y_contour_last, interior_direction_last, E_array_last
	logical, dimension(:), allocatable :: skip_point_last

	integer :: elevation_step, istat, temp_count, temp_array_size, counter_next, first_boundary_point, t_counter, counter
	integer :: next_step, index_tracker2, next_index, oversample_count, oversample_array_size, boundary_counter2, current_index
	integer :: boundary_counter, counter2
	integer, parameter :: array_step = 2000
	
	integer, parameter :: distance_factor = 2 ! this is muliplied by the minimum_spacing input parameter, used to regulate spacing between points along the polygon. Experience shows that going with a factor of 1 is really harsh and makes simulation time a lot slower, since it adds way more points than necessary.


	double precision :: p, H_f, p_angle, q_angle, rotated_p, rotation_amount, t, B, x_last, y_last, current_elevation, E
	double precision :: slope_last, intercept_last, slope_current, intercept_current, x_cross, y_cross, E_next, q, q_next
	double precision :: dir_cur, y_next, x_next, lon, lat, angle_check, distance_tracker, shear_stress_check, elevation_check
	double precision :: angle_check2, a_s, b_s, c_s, distance, current_topo, ss
	logical :: skip_to_last, stop_now, exceeds_limits

	double precision :: ax, ay, bx, by, cx, cy, dir1, dir2, advance_x, advance_y, slope1, slope2

	double precision, allocatable, dimension(:) :: x_crossover_store, y_crossover_store,x_poly,y_poly,dir_poly,q_poly,E_poly
	double precision, allocatable, dimension(:) :: x_crossover_store_temp, y_crossover_store_temp
	double precision, allocatable, dimension(:) :: crossover_distance, crossover_distance_temp
	logical, allocatable, dimension(:) :: skip_point_poly, skip_points_dist
	double precision, allocatable, dimension(:) :: q_array_dist, E_array_dist, x_contour_dist, y_contour_dist
	double precision, allocatable, dimension(:) :: interior_direction_dist, x_contour_reduced, y_contour_reduced
	double precision, allocatable, dimension(:) :: E_array_reduced, q_array_reduced, interior_direction_reduced
	double precision, allocatable, dimension(:) :: check_x, check_y
	integer, allocatable, dimension(:) :: crossover_index1, crossover_index2, index_array_expanded, status_array, status_array2
	integer, allocatable, dimension(:) :: crossover_index1_temp, crossover_index2_temp
	logical, allocatable, dimension(:) :: crossover_point

	integer, allocatable, dimension(:) :: calculate_order
	logical, allocatable, dimension(:) :: calculate_order_mask, crossover_distance_mask,crossover_distance_mask_temp


	double precision :: oversample_x, oversample_y, oversample_E, reduction_factor, oversample_B, oversample_SS
	double precision :: oversample_distance, E_slope, E_intercept, estimated_distance_current
	double precision, dimension(3) :: m_vector, naught_vector, one_vector
	
	logical :: oversample_skip_point, rejected

	double precision, dimension(11) :: x_extra, y_extra, dir_extra


	double precision, allocatable, dimension(:) :: q_temp, E_temp, x_temp, y_temp, interior_direction_temp
	logical, allocatable, dimension(:) :: skip_point_temp_later,crossover_point_temp
	integer, allocatable, dimension(:) :: distance_index, index_array
	double precision, dimension(current_boundary_points) :: E_average, dir_new

	double precision :: ice_thickness, x_mid, y_mid, temp_angle1, temp_angle2, latitude, longitude, B1, B2
	integer :: write_counter, polygon_count, number_crossovers, counter1, checked_count
	integer :: current_step_temp, distance_counter, new_boundary_count, temp_points, extra_points, boundary_count
	integer :: u_bound, crossover_count, index1_store, index2_store, points_below, reduced_points, proceed_counter
	integer :: check_index, org_counter, new_counter, first_index, second_index, intervening_points, expand_points
	integer :: gap_counter, gap_index, counter_1, number_polygons, poly_counter, number_points
	integer :: crossover_array_size, remaining_crossovers, smallest_distance_index,smallest_distance_index_2, remove_index
	integer :: number_advanced, expanded_size
	double precision ::  crossover_x, crossover_y
	logical :: is_crossover, extra_skip_point, delete_first, oversample_done, skip_interpolation, end_step_elevation, connect

	character(len=30), dimension(30) :: status_values

	double precision :: x_loc, y_loc 



	! check to see if the analysis can be done
	proceed_counter = count(.not.skip_point)

	if(proceed_counter < 3) THEN ! no point of proceeding with the calculation if it is not a polygon

		write(6,*) "proceed_counter < 3"
		return
	endif

	! create an array with the initial values that can be changed within the loop

	allocate(x_contour_last(current_boundary_points),y_contour_last(current_boundary_points),&
		E_array_last(current_boundary_points),interior_direction_last(current_boundary_points), &
		skip_point_last(current_boundary_points), stat=istat)
	if(istat/=0) THEN
		write(6,*) "error allocating at start of old arrays"
		stop
	endif

	x_contour_last = x_contour
	y_contour_last = y_contour
	E_array_last = E_array
	interior_direction_last = interior_direction_current
	skip_point_last = skip_point


	! the first step is to advance every point along the polyline that is < current step


	current_elevation = dble(current_step * elevation_interval)

	if(current_elevation > max_elevation) THEN ! the subroutine has finished
		return
	endif


	write(6,*) "current elevation", current_elevation
	write(6,*) "current boundary points:", current_boundary_points

	allocate(x_contour_temp(current_boundary_points),y_contour_temp(current_boundary_points),&
		E_temp_new(current_boundary_points),interior_direction_temp_new(current_boundary_points), &
		skip_point_temp(current_boundary_points), status_array(current_boundary_points),stat=istat)
	if(istat/=0) THEN
		write(6,*) "error allocating at start of temp arrays"
		stop
	endif



	skip_point_temp = .false.
	x_contour_temp = x_contour_last
	y_contour_temp = y_contour_last
	E_temp_new = E_array_last
	interior_direction_temp_new = interior_direction_last


	! calculate the flowlines for each point along the boundary up to the next elevation contour

	number_advanced = 0
	boundary_search: do boundary_counter = 1, current_boundary_points, 1


		if(skip_point_last(boundary_counter)) then ! if there was some problem with the last contour, skip it

			x_contour_temp(boundary_counter) = x_contour_last(boundary_counter)
			y_contour_temp(boundary_counter) = y_contour_last(boundary_counter)
			E_temp_new(boundary_counter) = E_array_last(boundary_counter)
			interior_direction_temp_new(boundary_counter) = interior_direction_last(boundary_counter)
			skip_point_temp(boundary_counter) = .true.

		elseif(E_array_last(boundary_counter) > current_elevation) THEN ! do not need to calculate the flowline if it is above the current contour

				x_contour_temp(boundary_counter) = x_contour_last(boundary_counter)
				y_contour_temp(boundary_counter) = y_contour_last(boundary_counter)
				E_temp_new(boundary_counter) = E_array_last(boundary_counter)
				interior_direction_temp_new(boundary_counter) = interior_direction_last(boundary_counter)
				skip_point_temp(boundary_counter) = .false.

		else ! calculate the flowline
			number_advanced = number_advanced + 1
			x_loc = x_contour_last(boundary_counter)
			y_loc = y_contour_last(boundary_counter)
			dir_cur = interior_direction_last(boundary_counter)
			E = E_array_last(boundary_counter)
			q = 0. ! Initial q is not strictly 0, but with the local coordinate system oriented towards the maximum p, it should be relatively small

			! calculate the flowline
			call step_flow_line(x_loc, y_loc, dir_cur, q, E, dx_l, current_elevation, x_next,y_next, q_next, &
			 E_next)

			if (nint(E_next*10.) /= nint(current_elevation*10.)) THEN ! first point added no matter what

				x_contour_temp(boundary_counter) = x_loc
				y_contour_temp(boundary_counter) = y_loc
				E_temp_new(boundary_counter) = E
				interior_direction_temp_new(boundary_counter) = dir_cur
				skip_point_temp(boundary_counter) = .true.


			
			elseif(.not. point_in_polygon(x_contour_last,y_contour_last,x_next,y_next,current_boundary_points)) then ! outside of the limits of the last polygon, do not include

				x_contour_temp(boundary_counter) = x_loc
				y_contour_temp(boundary_counter) = y_loc
				E_temp_new(boundary_counter) = E
				interior_direction_temp_new(boundary_counter) = dir_cur
				skip_point_temp(boundary_counter) = .true.


			else

				H_f = get_tau(x_next, y_next) / (rho_ice * g)

				p = sqrt((H_f/(E_next-elevation(x_next,y_next)))**2 - q_next**2)

			! angle of p and q in cartesian coordinates

				p_angle = check_angle(-(dir_cur - pi/2.))
				q_angle = check_angle(p_angle - pi/2)

			! note that q = 0 is not strictly true in a full 2D situation, but if the flowline density is high enough it likely won't a be big issue

				rotated_p = sqrt(p**2 + q_next**2)
				rotation_amount = atan2(q_next,p)


				x_contour_temp(boundary_counter) = x_next
				y_contour_temp(boundary_counter) = y_next
				E_temp_new(boundary_counter) = E_next
				skip_point_temp(boundary_counter) = .false.
				q = 0.

				interior_direction_temp_new(boundary_counter) = check_angle((dir_cur+rotation_amount)) ! direction in azimuth
			endif
		endif

	end do boundary_search
	
	write(6,*) "finished boundary_search", number_advanced, "out of", current_boundary_points

	! after finding all the stepped up flowlines, the next step is to see if those points cross over one another. Using the general
	! rules of a motorcycle graph algorithm (e.g. http://arxiv.org/abs/1303.5958), crossover lines are eliminated. The rule
	! is that if a line crosses over another, the line that has the shorter distance to the crossover point is eliminated

	! worst case scenario is that there is approximately n*log(n) crossovers. This is unlikely to happen except near the end of the ice sheet calculation

	crossover_array_size = current_boundary_points * ceiling(log(dble(current_boundary_points)))

	! let's make it a minimum of array_step points, since for small polygons, it seems there are rounding errors

	if(crossover_array_size < array_step) THEN
		crossover_array_size = array_step
	endif

	allocate(crossover_index1(crossover_array_size), crossover_index2(crossover_array_size), &
	  crossover_distance(crossover_array_size*2), crossover_distance_mask(crossover_array_size*2),stat=istat)
	if(istat/=0) THEN
		write(6,*) "error allocating at start of crossover arrays"
		stop
	endif
	crossover_distance_mask = .false.
	crossover_count = 0
	crossover_search_start: do boundary_counter = 1, current_boundary_points-1

		if(skip_point_temp(boundary_counter)) then
			cycle crossover_search_start
		endif

		crossover_search_end: do boundary_counter2 = boundary_counter, current_boundary_points

			if(skip_point_temp(boundary_counter)) then
				cycle crossover_search_end
			endif

			call crossover_test(x_contour_last(boundary_counter), y_contour_last(boundary_counter), &
			  x_contour_temp(boundary_counter), y_contour_temp(boundary_counter),&
			  x_contour_last(boundary_counter2), y_contour_last(boundary_counter2), &
			  x_contour_temp(boundary_counter2), y_contour_temp(boundary_counter2),&
  			  is_crossover, crossover_x, crossover_y)

			if(is_crossover) THEN
				crossover_count = crossover_count + 1

				if(crossover_count > crossover_array_size) THEN ! expand the bounds of the array

					allocate(crossover_index1_temp(crossover_array_size), crossover_index2_temp(crossover_array_size), &
					  crossover_distance_temp(crossover_array_size*2), &
					  crossover_distance_mask_temp(crossover_array_size*2),stat=istat)
					if(istat/=0) THEN
						write(6,*) "error allocating of crossover temp arrays"
						stop
					endif

					crossover_index1_temp = crossover_index1
					crossover_index2_temp = crossover_index2
					crossover_distance_temp = crossover_distance
					crossover_distance_mask_temp = crossover_distance_mask

					deallocate(crossover_index1, crossover_index2, crossover_distance, &
					  crossover_distance_mask,stat=istat)
					if(istat/=0) THEN
						write(6,*) "error deallocating of crossover arrays"
						stop
					endif

					! add array_step to the array size
					crossover_array_size = crossover_array_size + array_step

					allocate(crossover_index1(crossover_array_size), crossover_index2(crossover_array_size), &
					  crossover_distance(crossover_array_size*2), crossover_distance_mask(crossover_array_size*2),&
					  stat=istat)
					if(istat/=0) THEN
						write(6,*) "error allocating of crossover arrays"
						stop
					endif

					crossover_index1(1:crossover_array_size-1000) = crossover_index1_temp
					crossover_index2(1:crossover_array_size-1000) = crossover_index2_temp
					crossover_distance(1:crossover_array_size-1000) = crossover_distance_temp
					crossover_distance_mask(1:crossover_array_size-1000) = crossover_distance_mask_temp

					deallocate(crossover_index1_temp, crossover_index2_temp, &
					  crossover_distance_temp, &
					  crossover_distance_mask_temp,stat=istat)
					if(istat/=0) THEN
						write(6,*) "error allocating of crossover temp arrays"
						stop
					endif


				endif

				crossover_index1(crossover_count) = boundary_counter
				crossover_index2(crossover_count) = boundary_counter2
			
				crossover_distance(crossover_count*2-1) = sqrt((x_contour_last(boundary_counter)-crossover_x)**2 + &
					(y_contour_last(boundary_counter)-crossover_y)**2)
				crossover_distance(crossover_count*2) = sqrt((x_contour_last(boundary_counter2)-crossover_x)**2 + &
					(y_contour_last(boundary_counter2)-crossover_y)**2)

			endif

		end do crossover_search_end
	end do crossover_search_start
	write(6,*) "finished crossover_search_start"
	crossover_distance_mask(1:crossover_count*2) = .true.

	! with all the crossovers found, eliminate lines according to the motorcycle graph algorithm

	if(crossover_count > 0) THEN
		remaining_crossovers = crossover_count

		eliminate_loop: do while(remaining_crossovers > 0)

			smallest_distance_index = minloc(crossover_distance,1,crossover_distance_mask)

			smallest_distance_index_2 = (smallest_distance_index+1) / 2

			! eliminate the smaller value

			if(int(smallest_distance_index) == int(smallest_distance_index+1) ) THEN ! first array

				remove_index = crossover_index1(smallest_distance_index_2)
				if(skip_point_temp(crossover_index2(smallest_distance_index_2))) THEN ! if the larger distance line has already been eliminated, keep the current one
					crossover_distance_mask(smallest_distance_index) = .true.
					remaining_crossovers = remaining_crossovers - 1
					cycle eliminate_loop
				endif
			else ! second array
				remove_index = crossover_index2(smallest_distance_index_2)
				if(skip_point_temp(crossover_index1(smallest_distance_index_2))) THEN ! if the larger distance line has already been eliminated, keep the current one
					crossover_distance_mask(smallest_distance_index) = .true.
					remaining_crossovers = remaining_crossovers - 1
					cycle eliminate_loop
				endif
			endif

			skip_point_temp(remove_index) = .true.
			crossover_distance_mask(smallest_distance_index) = .true.
			remaining_crossovers = remaining_crossovers - 1


		end do eliminate_loop

	endif 



	write(6,*) "finished eliminate_loop"
	deallocate(crossover_index1, crossover_index2, crossover_distance, crossover_distance_mask,stat=istat)
	if(istat/=0) THEN
		write(6,*) "error deallocating at start of crossover arrays"
		stop
	endif

	! now compress the temporary array

	! find the amount of elements
	write(6,*) "# skip_point_temp:", count(skip_point_temp)
	write(6,*) "# .not. skip_point_temp:", count(.not.skip_point_temp)
	temp_array_size = count(.not.skip_point_temp)

	if(temp_array_size > 0) THEN ! if it is zero, I guess it should be rejected





		write(6,*) "allocating"
		allocate(x_contour_temp2(temp_array_size),y_contour_temp2(temp_array_size),&
			E_temp2(temp_array_size),interior_direction_temp2(temp_array_size),stat=istat)
		if(istat/=0) THEN
			write(6,*) "error allocating at start of temp2 arrays"
			stop
		endif

		! assign the shrunk down array



		x_contour_temp2 = pack(x_contour_temp,.not.skip_point_temp)
		y_contour_temp2 = pack(y_contour_temp,.not.skip_point_temp)
		E_temp2 = pack(E_temp_new,.not.skip_point_temp)
		interior_direction_temp2 = pack(interior_direction_temp_new,.not.skip_point_temp)



		! don't need the temporary1 arrays anymore
		deallocate(x_contour_temp,y_contour_temp,E_temp_new,interior_direction_temp_new, skip_point_temp, &
		   status_array,stat=istat)
		if(istat/=0) THEN
			write(6,*) "error deallocating at start of temp arrays"
			stop
		endif


		! at this point, the temp2 arrays are polygons that do not cross over the previous contour, though it is possible that it is not a simple polygon.
		! this is where interior_direction_temp2 must be used - the first point should be able to be used to find the initial starting direction,
		! and the rest are calculated by moving along the line

		!again, this is a worst case n*log(n) problem
		crossover_array_size = temp_array_size * ceiling(log(dble(temp_array_size)))

		! and again, make sure that this doesn't die when the array is small

		if(crossover_array_size < array_step) THEN
			crossover_array_size = array_step
		endif

		allocate(x_crossover_store(crossover_array_size), y_crossover_store(crossover_array_size),&
		  crossover_index1(crossover_array_size), crossover_index2(crossover_array_size), stat=istat)
		if(istat/=0) THEN
			write(6,*) "error allocating at start of crossover arrays"
			stop
		endif


		run_contour_crossover: do
			call contour_crossover(temp_array_size, x_contour_temp2, y_contour_temp2, number_crossovers, &
			  x_crossover_store, y_crossover_store, crossover_index1, crossover_index2,crossover_array_size,exceeds_limits)

			if(exceeds_limits) THEN ! increase array size and rerun

				allocate(x_crossover_store_temp(crossover_array_size), y_crossover_store_temp(crossover_array_size),&
				  crossover_index1_temp(crossover_array_size), crossover_index2_temp(crossover_array_size), stat=istat)
				if(istat/=0) THEN
					write(6,*) "error allocating of temp crossover arrays"
					stop
				endif

				x_crossover_store_temp = x_crossover_store
				y_crossover_store_temp = y_crossover_store
				crossover_index1_temp = crossover_index1
				crossover_index2_temp = crossover_index2

				deallocate(x_crossover_store, y_crossover_store,crossover_index1, crossover_index2, stat=istat)
				if(istat/=0) THEN
					write(6,*) "error deallocating of crossover arrays"
					stop
				endif

				crossover_array_size = crossover_array_size + array_step

				allocate(x_crossover_store(crossover_array_size), y_crossover_store(crossover_array_size),&
				  crossover_index1(crossover_array_size), crossover_index2(crossover_array_size), stat=istat)
				if(istat/=0) THEN
					write(6,*) "error allocating of crossover arrays"
					stop
				endif


				deallocate(x_crossover_store_temp, y_crossover_store_temp, crossover_index1_temp, &
				  crossover_index2_temp, stat=istat)
				if(istat/=0) THEN
					write(6,*) "error deallocating of temp crossover arrays"
					stop
				endif


			else
				exit run_contour_crossover
			endif

		end do run_contour_crossover

		! the amount of polygons depends on the amount of crossovers. This loop numbers each point based on which polygon each point belongs to

		write(6,*) "temp_array_size", temp_array_size

		allocate(index_array(temp_array_size),stat=istat)
		if(istat/=0) THEN
			write(6,*) "error allocating index_arrays"
			stop
		endif

		index_array = 0


		write(6,*) "number crossovers: ", number_crossovers

		if(number_crossovers > crossover_array_size) THEN ! probably something has messed up
			write(6,*) "too many crossovers", number_crossovers, crossover_array_size
			stop
		endif

		if(number_crossovers == 0) THEN

			index_array = 1

		else

			do polygon_count = number_crossovers, 1, -1


				do counter1 = crossover_index1(polygon_count)+1, crossover_index2(polygon_count)

					index_array(counter1) = polygon_count+1
				end do

			end do

		endif


		number_polygons = maxval(index_array)

		write(6,*) "finished finding crossovers", number_crossovers, temp_array_size
	! insert crossover points
		expanded_size = temp_array_size+number_crossovers*2
		allocate (x_poly(expanded_size), &
			    y_poly(expanded_size), &
			    dir_poly(expanded_size), &
			    index_array_expanded(expanded_size), &
			    q_poly(expanded_size), &
			    E_poly(expanded_size), crossover_point(expanded_size), &
			    skip_point_poly(expanded_size),stat=istat)
		if(istat/=0) THEN
			write(6,*) "error allocating at poly arrays"
			stop
		endif


		x_poly(1:temp_array_size) = x_contour_temp2(1:temp_array_size)
		y_poly(1:temp_array_size) = y_contour_temp2(1:temp_array_size)
		E_poly(1:temp_array_size) = E_temp2(1:temp_array_size)

		dir_poly(1:temp_array_size) = interior_direction_temp2(1:temp_array_size)
		index_array_expanded(1:temp_array_size) = index_array(1:temp_array_size)
		skip_point_poly = .false.

		deallocate(x_contour_temp2,y_contour_temp2,E_temp2,interior_direction_temp2,index_array,stat=istat)
		if(istat/=0) THEN
			write(6,*) "error deallocating at temp2 arrays"
			stop
		endif




		checked_count = temp_array_size

	! if the polygon is not simple, it is necessary to insert the crossover points and assign them to a polygon
		if(number_polygons > 1) THEN
			do crossover_count = 1, number_crossovers, 1

				! insert point after section
				x_poly(crossover_index2(crossover_count)+2:checked_count+1) = &
				  x_poly(crossover_index2(crossover_count)+1:checked_count)
				y_poly(crossover_index2(crossover_count)+2:checked_count+1) = &
				  y_poly(crossover_index2(crossover_count)+1:checked_count)
				E_poly(crossover_index2(crossover_count)+2:checked_count+1) = &
				  E_poly(crossover_index2(crossover_count)+1:checked_count)
				dir_poly(crossover_index2(crossover_count)+2:checked_count+1) = &
				  dir_poly(crossover_index2(crossover_count)+1:checked_count)


				checked_count = checked_count + 1

				x_poly(crossover_index2(crossover_count) + 1) = x_crossover_store(crossover_count)
				y_poly(crossover_index2(crossover_count) + 1) = y_crossover_store(crossover_count)
				E_poly(crossover_index2(crossover_count) + 1) = current_elevation
				dir_poly(crossover_index2(crossover_count) + 1) = 0 ! direction will be calculated later
				skip_point_poly(crossover_index2(crossover_count) + 1) = .true. ! this may be changed at a later time, but for now, skip using the crossover points for calculations because it could be messy



				! insert point before section


				x_poly(crossover_index1(crossover_count)+2:checked_count+1) = &
				  x_poly(crossover_index1(crossover_count)+1:checked_count)
				y_poly(crossover_index1(crossover_count)+2:checked_count+1) = &
				  y_poly(crossover_index1(crossover_count)+1:checked_count)
				E_poly(crossover_index1(crossover_count)+2:checked_count+1) = &
				  E_poly(crossover_index1(crossover_count)+1:checked_count)
				dir_poly(crossover_index1(crossover_count)+2:checked_count+1) = &
				  dir_poly(crossover_index1(crossover_count)+1:checked_count)

				checked_count = checked_count + 1

				x_poly(crossover_index1(crossover_count) + 1) = x_crossover_store(crossover_count)
				y_poly(crossover_index1(crossover_count) + 1) = y_crossover_store(crossover_count)
				E_poly(crossover_index1(crossover_count) + 1) = current_elevation
				dir_poly(crossover_index1(crossover_count) + 1) = 0 ! direction will be calculated later
				skip_point_poly(crossover_index1(crossover_count) + 1) = .true. ! this may be changed at a later time, but for now, skip using the crossover points for calculations because it could be messy


				! increase the subsequent crossover indices as appropriate

				do counter1 = 1, number_crossovers, 1

					if (counter1 /= crossover_count) THEN
						if(crossover_index1(counter1) > crossover_index1(crossover_count)) THEN

							crossover_index1(counter1) = crossover_index1(counter1) + 1

						endif

						if(crossover_index1(counter1) >= crossover_index2(crossover_count)) THEN

							crossover_index1(counter1) = crossover_index1(counter1) + 1

						endif

						if(crossover_index2(counter1) > crossover_index2(crossover_count)) THEN

							crossover_index2(counter1) = crossover_index2(counter1) + 2

						elseif (crossover_index2(counter1) == crossover_index2(crossover_count) .and. &
							counter1 > crossover_count) THEN

							crossover_index2(counter1) = crossover_index2(counter1) + 2

						elseif (crossover_index2(counter1) > crossover_index1(crossover_count)) THEN

							crossover_index2(counter1) = crossover_index2(counter1) + 1

						endif
					endif
				end do



				crossover_index1(crossover_count) = crossover_index1(crossover_count)+1
				crossover_index2(crossover_count) = crossover_index2(crossover_count)+2

			end do

		! sort the crossover indices. This is necessary sometimes if there is more than one crossover on a single line segment
			do crossover_count = 1, number_crossovers-1, 1
				do counter1 = crossover_count+1, number_crossovers

					if(crossover_index1(counter1) < crossover_index1(crossover_count)) THEN ! reorganize

						index1_store = crossover_index1(crossover_count)
						index2_store = crossover_index2(crossover_count)

						! swap
						crossover_index1(crossover_count) = crossover_index1(counter1)
						crossover_index2(crossover_count) = crossover_index2(counter1)

						crossover_index1(counter1) = index1_store
						crossover_index2(counter1) = index2_store
	
					endif
				end do
			end do

			index_array_expanded = 1
			do crossover_count = 1, number_crossovers, 1

				do counter1 = crossover_index1(crossover_count), crossover_index2(crossover_count) - 1, 1

					index_array_expanded(counter1) = crossover_count+1
				end do

			end do



		endif

		number_polygons = maxval(index_array_expanded)
		write(6,*) "finished inserting crossover points"



		if(number_polygons < 1) THEN
			write(6,*) "number of polygons < 1, something wrong"
			write(6,*) " maxval(index_array_expanded)",  maxval(index_array_expanded)
			write(6,*) " maxval(index_array)",  maxval(index_array)
			stop
		endif

		! create temporary arrays to contain the polygons. Will never be larger than checked_count.
		allocate(x_contour_temp(checked_count),y_contour_temp(checked_count),&
			E_temp_new(checked_count),interior_direction_temp_new(checked_count), &
			skip_point_temp(checked_count), status_array(checked_count),stat=istat)
		if(istat/=0) THEN
			write(6,*) "error allocating temp arrays"
			stop
		endif

		rejected = .false.

	else

		! create dummy values
		write(6,*) "creating dummy values"
		number_polygons = 1

		checked_count = current_boundary_points


		expanded_size = current_boundary_points

		allocate (x_poly(expanded_size))

		allocate (y_poly(expanded_size))

		allocate (dir_poly(expanded_size))

		allocate (index_array_expanded(expanded_size))

		allocate (q_poly(expanded_size))

		allocate (E_poly(expanded_size))

		allocate (crossover_point(expanded_size))

		allocate (skip_point_poly(expanded_size))

		index_array_expanded = 1
		rejected = .true.


	endif

	write(6,*) "number_polygons, ", number_polygons

	do poly_counter = 1, number_polygons, 1

		! first step is to  search the old polygon and put each part into its own array
		if(.not.rejected) THEN
			number_points = 0
			do counter = 1, checked_count
				if(index_array_expanded(counter) == poly_counter) THEN
					number_points = number_points + 1

					x_contour_temp(number_points) = x_poly(counter)
					y_contour_temp(number_points) = y_poly(counter)
					E_temp_new(number_points) = E_poly(counter)
					interior_direction_temp_new(number_points) = dir_poly(counter)! direction will be calculated later
					skip_point_temp(number_points) = skip_point_poly(counter) 
				endif
			end do
		else
			number_points = current_boundary_points

		endif


		! check if the polygon should be rejected

		if(.not. check_polygon2(number_points, x_contour_temp(1:number_points), y_contour_temp(1:number_points), &
		  interior_direction_temp_new(1:number_points), E_temp_new(1:number_points), current_elevation, &
		  skip_point_temp(1:number_points)) .and. .not. rejected) THEN

			! if not rejected, write out the results
			polygon_counter = polygon_counter + 1
			write(500,*) "> -Z", current_elevation, polygon_counter

			do write_counter = 1, number_points
				current_topo = elevation(x_contour_temp(write_counter),y_contour_temp(write_counter))
				ss = get_tau(x_contour_temp(write_counter),y_contour_temp(write_counter))
				ice_thickness = E_temp_new(write_counter) - current_topo

				write(500,'((F10.1,1X,F10.1,1X,F10.4,1X,F10.4,1X,F10.4,1X,L1,1X,F12.4))') &
					x_contour_temp(write_counter),y_contour_temp(write_counter), & 
					E_temp_new(write_counter), ice_thickness, current_topo,&
					skip_point_temp(write_counter), ss

			end do


		! resample the array and call the subroutine again

			oversample_array_size = number_points + array_step
			allocate(x_contour_temp3(oversample_array_size),y_contour_temp3(oversample_array_size),&
				E_temp3(oversample_array_size),interior_direction_temp3(oversample_array_size),&
				skip_point_temp3(oversample_array_size), stat=istat)
			if(istat/=0) THEN
				write(6,*) "error allocating at temp"
				stop
			endif



			write(502,*) "> -Z", current_elevation, polygon_counter

			! to make it so we don't have to an if statement in the oversample loop, have the starting point be the last one

			x_contour_temp3(1) = x_contour_temp(number_points)
			y_contour_temp3(1) = y_contour_temp(number_points)
			E_temp3(1) = E_temp_new(number_points)
			skip_point_temp3(1) = skip_point_temp(number_points)

			oversample_count = 1


			oversample_loop: do counter = 1, number_points, 1


				! estimate how far from the last point location to the next elevation value is. 
				! See equation 4, chapter 11 in Paterson, Physics of Glaciers, third edition

				if(nint(E_temp3(oversample_count)) <= nint(current_elevation+elevation_interval)) THEN

					current_topo = elevation(x_contour_temp3(oversample_count),y_contour_temp3(oversample_count))
					ss = get_tau(x_contour_temp3(oversample_count),y_contour_temp3(oversample_count))
					estimated_distance_current = (((current_elevation+elevation_interval)-current_topo)**2-&
					  (E_temp3(oversample_count)-current_topo)**2) * rho_ice / (2. * ss) 
				else
					
					estimated_distance_current = 0
				endif

				one_vector(1) = x_contour_temp(counter)
				one_vector(2) = y_contour_temp(counter)
				one_vector(3) = E_temp_new(counter)

				reduction_factor = 1. ! initial guess is that the next point is fine
				check_interval: do

					if(reduction_factor < 0.0000001) THEN ! something has likely messed up
						write(6,*) "problem with resampling step"
						write(6,*) x_contour_temp(counter), y_contour_temp(counter), E_temp_new(counter), &
							get_tau(x_contour_temp(counter), y_contour_temp(counter))
						write(6,*) x_contour_temp3(oversample_count), y_contour_temp3(oversample_count), &
							E_temp3(oversample_count), get_tau(x_contour_temp3(counter), y_contour_temp3(counter))

						stop
					endif

					naught_vector(1) = x_contour_temp3(oversample_count)
					naught_vector(2) = y_contour_temp3(oversample_count)
					naught_vector(3) = E_temp3(oversample_count)
					m_vector = one_vector - naught_vector

					oversample_x = reduction_factor * m_vector(1) + naught_vector(1)
					oversample_y = reduction_factor * m_vector(2) + naught_vector(2)

					! first check if the distance between the points is under the threshold

					distance = sqrt((x_contour_temp3(oversample_count)-oversample_x)**2+&
					  (y_contour_temp3(oversample_count)-oversample_y)**2)

					if(nint(distance)>nint(minimum_spacing)*distance_factor) THEN
						reduction_factor = reduction_factor / 2.

						cycle check_interval
					endif
					
					! if it is under the distance between point threshold, then also check if the estimated 
					! next flowline interval is also below the threshold

					oversample_E = reduction_factor * m_vector(3) + naught_vector(3)
					oversample_B = elevation(oversample_x,oversample_y)
					oversample_SS = get_tau(oversample_x,oversample_y)

					if(nint(oversample_E) < nint(current_elevation+elevation_interval) &
					  .and. oversample_E > oversample_B .and. (current_elevation+elevation_interval)>oversample_B &
					  .and. nint(oversample_E-oversample_B) > minimum_thickness ) THEN ! added this line
						oversample_distance = (((current_elevation+elevation_interval)-oversample_B)**2-&
					 	 (oversample_E-oversample_B)**2) * rho_ice / (2. * oversample_SS) 

					else
						oversample_distance = 0.
					endif



					if(abs(nint(oversample_distance-estimated_distance_current))>&
					   nint(minimum_spacing)*distance_factor) THEN
						reduction_factor = reduction_factor / 2.


						cycle check_interval
					endif
					
					! if it passes those two tests, you can add the next point

					oversample_count = oversample_count + 1

					if(oversample_count > oversample_array_size) THEN ! increase the size of the array if it has grown too large
						
						! create temporary arrays for storage
						allocate(x_contour_temp2(oversample_array_size),y_contour_temp2(oversample_array_size),&
							E_temp2(oversample_array_size),interior_direction_temp2(oversample_array_size),&
							skip_point_temp2(oversample_array_size), stat=istat)
						if(istat/=0) THEN
							write(6,*) "error allocating at temp2 1", istat, oversample_array_size
							stop
						endif
						! transfer data to temporary arrays
						x_contour_temp2 = x_contour_temp3
						y_contour_temp2 = y_contour_temp3
						E_temp2 = E_temp3
						interior_direction_temp2 = interior_direction_temp3
						skip_point_temp2 = skip_point_temp3

						! deallocate the original arrays
						deallocate(x_contour_temp3,y_contour_temp3, E_temp3,interior_direction_temp3,skip_point_temp3, &
							stat=istat)
						if(istat/=0) THEN
							write(6,*) "error deallocating at temp"
							stop
						endif

						! increase size of array by array_step

						oversample_array_size = oversample_array_size + array_step

						allocate(x_contour_temp3(oversample_array_size),y_contour_temp3(oversample_array_size),&
							E_temp3(oversample_array_size),interior_direction_temp3(oversample_array_size),&
							skip_point_temp3(oversample_array_size), stat=istat)
						if(istat/=0) THEN
							write(6,*) "error allocating at temp"
							stop
						endif

						! transfer the data from the temporary arrays to the original arrays
						x_contour_temp3(1:oversample_array_size-array_step) = x_contour_temp2
						y_contour_temp3(1:oversample_array_size-array_step) = y_contour_temp2
						E_temp3(1:oversample_array_size-array_step) = E_temp2
						interior_direction_temp3(1:oversample_array_size-array_step) = interior_direction_temp2
						skip_point_temp3(1:oversample_array_size-array_step) = skip_point_temp2
						! free up memory
						deallocate(x_contour_temp2,y_contour_temp2, E_temp2,interior_direction_temp2,skip_point_temp2, &
							 stat=istat)
						if(istat/=0) THEN
							write(6,*) "error deallocating at temp2"
							stop
						endif

					endif

					! add the point to the oversampling polygon array
					if(reduction_factor < 1.) THEN

						x_contour_temp3(oversample_count) = oversample_x
						y_contour_temp3(oversample_count) = oversample_y

						if (oversample_E > oversample_B) THEN
							E_temp3(oversample_count) = oversample_E
							skip_point_temp3(oversample_count) = .false.
						else
							E_temp3(oversample_count) = oversample_B
							skip_point_temp3(oversample_count) = .true.
						endif

						estimated_distance_current = oversample_distance
						write(502,*) x_contour_temp3(oversample_count), y_contour_temp3(oversample_count), &
							E_temp3(oversample_count)
						reduction_factor = 1.
					else

						x_contour_temp3(oversample_count) = x_contour_temp(counter)
						y_contour_temp3(oversample_count) = y_contour_temp(counter)
						skip_point_temp3(oversample_count) = skip_point_temp(counter)
						E_temp3(oversample_count) =  E_temp_new(counter)
						reduction_factor = 1.
						exit check_interval

					end if

				end do check_interval
			end do oversample_loop

			! ignore last point, because it was already added prior to search
			oversample_count = oversample_count - 1
			write(6,*) "# added points: ", oversample_count - number_points

			! assign directions to all points. Assumed that the direction will be the bisector of the 
			! margin line segments on either side of the point. For increased accuracy, decrease the distance
			! threshold in the params.txt file.
			do counter2 = 1, oversample_count, 1


				if(counter2 == 1) THEN
					slope1 = atan2(y_contour_temp3(counter2)-y_contour_temp3(oversample_count), &
						x_contour_temp3(counter2)-x_contour_temp3(oversample_count))
					slope2 = atan2(y_contour_temp3(counter2)-y_contour_temp3(counter2+1), &
						x_contour_temp3(counter2)-x_contour_temp3(counter2+1))
				elseif(counter2 == oversample_count) THEN
					slope1 = atan2(y_contour_temp3(counter2)-y_contour_temp3(counter2-1), &
						x_contour_temp3(counter2)-x_contour_temp3(counter2-1))
					slope2 = atan2(y_contour_temp3(counter2)-y_contour_temp3(1), &
						x_contour_temp3(counter2)-x_contour_temp3(1))
				else

					slope1 = atan2(y_contour_temp3(counter2)-y_contour_temp3(counter2-1), &
						x_contour_temp3(counter2)-x_contour_temp3(counter2-1))
					slope2 = atan2(y_contour_temp3(counter2)-y_contour_temp3(counter2+1), &
						x_contour_temp3(counter2)-x_contour_temp3(counter2+1))
				endif

				cx = x_contour_temp3(counter2)
				cy = y_contour_temp3(counter2)

				if(counter2 == 1) THEN
					bx = x_contour_temp3(oversample_count)
					by = y_contour_temp3(oversample_count)
				else
					bx = x_contour_temp3(counter2-1)
					by = y_contour_temp3(counter2-1)
				endif

				if(counter2 == oversample_count) THEN
					ax = x_contour_temp3(1)
					ay = y_contour_temp3(1)
				else
					ax = x_contour_temp3(counter2+1)
					ay = y_contour_temp3(counter2+1)
				endif

				dir1 = atan2(-cx+bx, by-cy)
				dir2 = atan2(-cx+ax, ay-cy)


				interior_direction_temp3(counter2) = average_direction(dir1, dir2)

				advance_x = cx + 1. * sin(interior_direction_temp3(counter2))
				advance_y = cy + 1. * cos(interior_direction_temp3(counter2))

				if (.not. point_in_polygon(x_contour_temp3(1:oversample_count), y_contour_temp3(1:oversample_count),&
					 advance_x, advance_y, oversample_count)) THEN

					interior_direction_temp3(counter2) = check_angle(interior_direction_temp3(counter2) + pi)
				endif


			end do


			next_step = current_step+1

			call calculate_polygon5(oversample_count, E_temp3(1:oversample_count), x_contour_temp3(1:oversample_count), &
				 y_contour_temp3(1:oversample_count), interior_direction_temp3(1:oversample_count), next_step,&
				skip_point_temp3(1:oversample_count))

			deallocate(x_contour_temp3, y_contour_temp3, E_temp3, interior_direction_temp3, skip_point_temp3, stat=istat)
			if(istat/=0) THEN
				write(6,*) "error deallocating at temp3"
				stop
			endif

		else

			! polygon is rejected
			write(501,*) "> -Z", current_elevation

			if(rejected) THEN

				write(6,*) "x_contour_temp", allocated(x_contour_temp)
				write(6,*) "y_contour_temp", allocated(y_contour_temp)
				write(6,*) "E_temp_new", allocated(E_temp_new)
				write(6,*) "skip_point_temp", allocated(skip_point_temp)
				write(6,*) "number_points", number_points
				write(6,*) "size(x_contour_temp)", size(x_contour_temp)
			endif
			do write_counter = 1, number_points



				current_topo = elevation(x_contour_temp(write_counter),y_contour_temp(write_counter))
				ss = get_tau(x_contour_temp(write_counter),y_contour_temp(write_counter))
				ice_thickness = E_temp_new(write_counter) - current_topo


				write(501,'((F10.1,1X,F10.1,1X,F10.4,1X,F10.4,1X,F10.4,1X,L1,1X,F12.4))') &
					x_contour_temp(write_counter),y_contour_temp(write_counter), & 
					E_temp_new(write_counter), ice_thickness, current_topo,&
					skip_point_temp(write_counter), ss


			end do

		endif

	end do

	write(6,*) "finished elevation", current_elevation

! deallocate everything
	deallocate(x_contour_temp,y_contour_temp,E_temp_new,interior_direction_temp_new, skip_point_temp, status_array,stat=istat)
	if(istat/=0) THEN
		write(6,*) "error deallocating temp arrays"
		stop
	endif

	deallocate (x_poly, y_poly, dir_poly, index_array_expanded, q_poly, E_poly, crossover_point, skip_point_poly,stat=istat)
	if(istat/=0) THEN
		write(6,*) "error deallocating at poly arrays"
		stop
	endif

	deallocate(x_contour_last,y_contour_last,E_array_last,interior_direction_last, skip_point_last, stat=istat)
	if(istat/=0) THEN
		write(6,*) "error deallocating old arrays"
		stop
	endif

end subroutine calculate_polygon5

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1



subroutine step_flow_line(x_start_initial, y_start_initial, current_interior_direction, q, E, dx_l_current, current_elevation, &
				  x_next, y_next, q_next, E_next) 

	! this subroutine takes an arbitrary starting point and elevation, and steps up to the "current_elevation"
	
	use grids
	implicit none
	double precision, intent(in) ::  x_start_initial, y_start_initial, q, E, dx_l_current, &
				   current_elevation
	double precision, intent(out) :: x_next, y_next, q_next, E_next 
	double precision, intent(inout) :: current_interior_direction
!	logical, intent(out) :: error
	double precision :: x_l, y_l, lat, long, temp_slope, temp_intercept, old_direction, p_next, rotation_amount

	double precision :: q_out, y_out, E_out, q_out_hs1, y_out_hs1, E_out_hs1, q_out_hs2, y_out_hs2, E_out_hs2
	double precision ::  x_current1, y_current1, x_current2, y_current2, x_current3, y_current3
	double precision :: dHf_dy, dB_dy, dx_l_adapt, dx_l_tracker
	double precision :: dB_dy_temp, dHf_dy_temp, maximum_error
	double precision :: E_temp, B_standard, x_start, y_start
	double precision :: H_f, dx_l_adapt_2, x_l_temp, y_l_temp, rotation_amount1
	logical :: error1, error2, error3, failed
	integer :: fail, counter1, counter2, counter3
!	double precision, parameter :: y_standard = 1, E_standard = 1, q_standard = 0.001, tolerance_level = 1, expand_condition = 0.0006 ! original settings
!	double precision, parameter :: y_standard = 2, E_standard = 2, q_standard = 0.002, tolerance_level = 2, expand_condition = 0.0006 ! the first test, which was largely successful
!	double precision, parameter :: y_standard = 5, E_standard = 5, q_standard = 0.005, tolerance_level = 5, expand_condition = 0.0006 ! and extreme test - note it didn't work very well compared to the second test
!	double precision, parameter :: y_standard = 5, E_standard = 5, q_standard = 0.002, tolerance_level = 2, expand_condition = 0.0006 ! lower tolerance and q_standard - reached an infinite loop? Don't know the result
!	double precision, parameter :: y_standard = 5, E_standard = 5, q_standard = 0.001, tolerance_level = 1, expand_condition = 0.0006 ! lower tolerance again, and lower q_standard
	double precision, parameter :: y_standard = 1.d0, E_standard = 1.d0, q_standard = 0.001d0, tolerance_level = 2.d0, &
		expand_condition = 0.0006d0 ! these values work fairly well most of the time
!	double precision, parameter :: y_standard = 1.d0, E_standard = 1.d0, q_standard = 0.001d0, tolerance_level = 2.d0, &
!		expand_condition = 0.06d0 ! these values work fairly well most of the time
	double precision, parameter :: safety = 0.9d0, exp_grow = -0.2d0, exp_shrink = -0.25d0, zero=0.d0, twenty=20000
	double precision, parameter :: stop_value = 5.d0 !stop_value = 0.1d0

	double precision, parameter ::  y_l_increment = 0.1 ! this value seemed to work well before


	if (x_start_initial == 0 .and. y_start_initial == 0) THEN
		write(6,*) "error in the initialization steps"
		stop
	endif
	failed = .false.
!	error = .false.
	dx_l_adapt = dx_l_current
	old_direction = current_interior_direction
	x_start = x_start_initial
	y_start = y_start_initial


	call flowline_location(current_interior_direction, x_start, y_start, zero, zero, x_current1, y_current1)

	! for debugging
	call flowline_location(current_interior_direction, x_start, y_start, twenty, zero, x_current2, y_current2)


	dB_dy = y_gradient(x_current1, y_current1, current_interior_direction)


	dHf_dy = Hf_gradient(x_current1, y_current1, current_interior_direction)


	counter3=0
	x_l = 0.d0
	y_l = 0.d0
	q_next = q
	E_next = E
	fail = 0
	dx_l_tracker = dx_l_current




	step: do 


		if (dx_l_adapt == 0.) THEN
			stop
		endif
		call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l, x_next, y_next)



		if(sqrt(x_l**2 + y_l**2) > 1000000) THEN
			write(6,*) "Could not calculate to next step, too long, skipping"
			x_next = x_start_initial
			y_next = y_start_initial
			q_next = q
			E_next = E
			stop
			return
		endif

		error1 = .false.
		error2 = .false.

		error3 = .false.

		! the first step involves taking the full step

		call RK4(x_l, y_l, q_next, E_next, dB_dy, dHf_dy, dx_l_adapt, y_out, q_out, &
			   E_out,error1, x_start, y_start, current_interior_direction) 

		if(error1) THEN

			if (dx_l_tracker > stop_value) THEN
				dx_l_tracker = dx_l_tracker/2.d0
				dx_l_adapt = dx_l_tracker

				cycle step

			else
			

				counter3 = counter3+1

		call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l, x_next, y_next)

!					write(6,*) "error during first RK4 step"

					if(E_next-elevation(x_next,y_next) <=0 .or. x_l == 0.) THEN
						failed = .true.
						exit step
					endif


				rotation_amount1 = -sign(1.d0,q_next)*pi/2.d0

				call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l, x_current1, y_current1)

				x_start = x_current1
				y_start = y_current1
				x_l = 0.d0
				y_l = 0.d0

				current_interior_direction = check_angle(current_interior_direction + rotation_amount1)

				q_next = 0.d0
				dx_l_adapt = dx_l_current
				dx_l_tracker = dx_l_current
				cycle step
			endif
		endif

		dx_l_adapt_2 = dx_l_adapt / 2.d0

		! next do it in two steps
		call RK4(x_l, y_l, q_next, E_next, dB_dy, dHf_dy, dx_l_adapt_2, &
			   y_out_hs1, q_out_hs1, E_out_hs1,error2, x_start, y_start, current_interior_direction)

		if(error2) THEN
			if (dx_l_tracker > stop_value) THEN
				dx_l_tracker = dx_l_tracker/2.d0
				dx_l_adapt = dx_l_tracker

				cycle step

			else


				counter3 = counter3+1


				call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l, x_next, y_next)



				if(E_next-elevation(x_next,y_next) <=0 .or. x_l == 0.) THEN
					failed = .true.
					write(6,*) "error during second RK4 step"
					exit step
				endif

				rotation_amount1 = -sign(1.d0,q_next)*pi/2.d0

				call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l, x_current1, y_current1)

				x_start = x_current1
				y_start = y_current1
				x_l = 0.d0
				y_l = 0.d0

				current_interior_direction = check_angle(current_interior_direction + rotation_amount1)

				q_next = 0.d0
				dx_l_adapt = dx_l_current
				dx_l_tracker = dx_l_current
				cycle step
			endif
		endif


		x_l_temp = x_l + dx_l_adapt/2.d0
		y_l_temp = y_l + y_out_hs1 + y_l_increment
		call flowline_location(current_interior_direction, x_start, y_start, x_l_temp, y_l_temp, &
					     x_current1, y_current1)


		dB_dy = y_gradient(x_current1, y_current1, current_interior_direction)



		dHf_dy_temp = Hf_gradient(x_current1, y_current1, current_interior_direction)



		call RK4(x_l+dx_l_adapt_2, y_out_hs1, q_out_hs1, E_out_hs1, dB_dy, dHf_dy, dx_l_adapt_2,&
			   y_out_hs2, q_out_hs2, E_out_hs2, error3, x_start, y_start, current_interior_direction)

		if(error3) THEN
			if (dx_l_tracker > stop_value) THEN
				dx_l_tracker = dx_l_tracker/2.d0
				dx_l_adapt = dx_l_tracker

				cycle step

			else

				counter3 = counter3+1

				call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l, x_next, y_next)



				if(E_next-elevation(x_next,y_next) <=0 .or. x_l == 0.) THEN
					failed = .true.
					write(6,*) "error during third RK4 step"
					exit step
				endif

				rotation_amount1 = -sign(1.d0,q_next)*pi/2.d0

				call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l, x_current1, y_current1)
				x_start = x_current1
				y_start = y_current1
				x_l = 0.d0
				y_l = 0.d0

				current_interior_direction = check_angle(current_interior_direction + rotation_amount1)

				q_next = 0.d0
				dx_l_adapt = dx_l_current
				dx_l_tracker = dx_l_current
				cycle step
			endif
		endif

		! the program has now calculated the next step, find if the error

		maximum_error = 0


		maximum_error = max(maximum_error, abs((y_out-y_out_hs2)/y_standard))
		maximum_error = max(maximum_error, abs((E_out-E_out_hs2)/E_standard))
		maximum_error = max(maximum_error, abs((q_out-q_out_hs2)/q_standard))

		maximum_error = maximum_error / tolerance_level

		E_temp = E_out_hs2 + (E_out-E_out_hs2) / 15.d0

		if (maximum_error > 1 ) THEN ! have to change the stepsize



			dx_l_adapt = safety * dx_l_adapt * maximum_error ** exp_shrink
			cycle step


		elseif (nint(E_temp*10.) > nint(current_elevation*10.)) THEN ! reduce step size because it has gone too far

			dx_l_adapt = dx_l_adapt / 2.d0
			cycle step

		else ! don't have to decrease the stepsize



			x_l = x_l + dx_l_adapt

			if (maximum_error > expand_condition) THEN ! the error is not small enough to justify increasing the step size significantly

				dx_l_adapt = safety * dx_l_adapt * maximum_error ** exp_grow
			else

				dx_l_adapt = 4.d0 * dx_l_adapt
			endif

		endif

		! finalize the fifth order Runge-Kutta calculation.

		y_l = y_out_hs2 + (y_out-y_out_hs2) / 15.d0
		q_next = q_out_hs2 + (q_out-q_out_hs2) / 15.d0
		E_next = E_temp

		if (nint(E_temp*10.) == nint(current_elevation*10.)) THEN ! the flowline calculation has hit the required elevation for this step
!			write(6,*) "should exit"
			exit step
		endif

		y_l_temp = y_l + y_l_increment

		call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l_temp, &
					     x_current1, y_current1)


		dB_dy = y_gradient(x_current1, y_current1, current_interior_direction)



		dHf_dy = Hf_gradient(x_current1, y_current1, current_interior_direction)
		dx_l_tracker = dx_l_adapt
	end do step


	call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l, x_next, y_next)


	return
end subroutine step_flow_line


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



	subroutine RK4(x_l, y_l, q, E, dB_dy, dHf_dy, dx_l_current, y_out, q_out, E_out,error,&
			   x_start, y_start, current_interior_direction)
		
		use grids
		implicit none
		double precision, intent(in) :: x_l, y_l, q, E, dB_dy, dHf_dy,  x_start, y_start, &
					  current_interior_direction
		double precision, intent(out) :: y_out, q_out, E_out
		double precision, intent(inout) :: dx_l_current
		double precision :: y_temp1, E_temp1, q_temp1, y_temp2, E_temp2, q_temp2, y_temp3, E_temp3, q_temp3, B, B_temp1, &
			B_temp2, B_temp3
		double precision :: Hf_temp1, Hf_temp2, Hf_temp3
		double precision :: dB_dy_temp1, dB_dy_temp2, dB_dy_temp3, dB_dy_temp4
		double precision :: dHf_dy_temp1, dHf_dy_temp2, dHf_dy_temp3, dHf_dy_temp4
		double precision :: k_y1, k_E1, k_q1, k_y2, k_E2, k_q2, k_y3, k_E3, k_q3, k_y4, k_E4, k_q4
		double precision :: x_current, y_current, x_grid_hold, y_grid_naught
		double precision :: x_current1, y_current1, x_current2, y_current2
		double precision :: H_f, dE_ds, B_next, dx_temp, dx_temp1, dx_temp2, q_p, x_l_temp, y_l_temp
		double precision :: a, bb, c
		logical, intent(out) :: error
		double precision, parameter :: rho_ice = 920.d0
		double precision, parameter :: g = 9.80665d0, zero=0.d0
		error = .false.



		call flowline_location(current_interior_direction, x_start, y_start, x_l, y_l, x_current, y_current)
		B = elevation(x_current,y_current)

		H_f = get_tau(x_current,y_current) / (rho_ice*g)




		k_y1 = y_prime(q, H_f, E, B) * dx_l_current
		k_E1 = E_prime(q, H_f, E, B) * dx_l_current
		k_q1 = q_prime_fisher(q, H_f, E, B, dB_dy, dHf_dy) * dx_l_current



		if (isnan(k_y1) .or. isnan(k_E1) .or. isnan(k_q1) .or. k_q1 == -999999) THEN
			error = .true.

			return
		endif

		! with the k1 values calculated, find temporary midpoint y, E and q values so that we can calculate the k2 values

		y_temp1 = y_l +  k_y1 / 2.d0
		E_temp1 = E +  k_E1 / 2.d0
		q_temp1 = q +  k_q1 / 2.d0


		! with temporary midpoint values, k2 can be calculated

		x_l_temp = x_l + dx_l_current/2.

		call flowline_location(current_interior_direction, x_start, y_start, x_l_temp, y_temp1, x_current, &
					     y_current)
		B_temp1 = elevation(x_current,y_current)

		Hf_temp1 = get_tau(x_current,y_current) / (rho_ice*g)



		x_L_temp = x_l + dx_l_current/2.

		call flowline_location(current_interior_direction, x_start, y_start, x_l_temp, zero, &
					     x_current1, y_current1)


		dB_dy_temp1 = y_gradient(x_current1, y_current1, current_interior_direction)


		dHf_dy_temp1 = Hf_gradient(x_current1, y_current1, current_interior_direction)

		if((Hf_temp1/(E_temp1-B_temp1))**2 < q_temp1**2) THEN ! failure expected

			q_p = q_prime_fisher(q, H_f, E, B, dB_dy, dHf_dy)


			a = q_p**2 / 4.d0
			bb = q_p * q / 2.d0
			c = q**2 - (Hf_temp1 / (E_temp1-B_temp1))**2



			if(bb**2 > 4*a*c) THEN

				dx_temp1 = (-bb + sqrt(bb**2 - 4.d0*a*c)) / (2.d0 * a)
				dx_temp2 = (-bb - sqrt(bb**2 - 4.d0*a*c)) / (2.d0 * a)

				if(dx_temp1 > 0) THEN
					dx_temp = dx_temp1
				elseif(dx_temp2 > 0) then
					dx_temp = dx_temp2
				else


					dx_temp = minval((/dx_temp1, dx_temp2/))
				endif
			else

				dx_l_current = dx_l_current / 2.d0
				return
			endif


			dx_l_current = abs(dx_temp)
			error = .true.
			return

!			stop
		endif

		k_y2 = y_prime(q_temp1, Hf_temp1, E_temp1, B_temp1) * dx_l_current
		k_E2 = E_prime(q_temp1, Hf_temp1, E_temp1, B_temp1) * dx_l_current
		k_q2 = q_prime_fisher(q_temp1, Hf_temp1, E_temp1, B_temp1, dB_dy_temp1, dHf_dy_temp1) * dx_l_current

		if (isnan(k_y2) .or. isnan(k_E2) .or. isnan(k_q2) .or. k_q2 == -999999) THEN
			error = .true.
			return
		endif

		! now the second set of midpoint values can be calculated

		y_temp2 = y_l +  k_y2 / 2.d0
		E_temp2 = E +  k_E2 / 2.d0
		q_temp2 = q +  k_q2 / 2.d0



		! with the second set of temporary midpoint values, k3 can be calculated
		x_l_temp = x_l + dx_l_current/2.

		call flowline_location(current_interior_direction, x_start, y_start, x_l_temp, y_temp2, x_current, &
					     y_current)
		B_temp2 = elevation(x_current,y_current)

		Hf_temp2 = get_tau(x_current,y_current) / (rho_ice*g)



		x_l_temp = x_l + dx_l_current/2.

		call flowline_location(current_interior_direction, x_start, y_start, x_l_temp, y_temp2, &
					     x_current1, y_current1)


		dB_dy_temp2 = y_gradient(x_current1, y_current1, current_interior_direction)


		dHf_dy_temp2 = Hf_gradient(x_current1, y_current1, current_interior_direction)




		if((Hf_temp2/(E_temp2-B_temp2))**2 < q_temp2**2) THEN ! failure expected


			q_p = q_prime_fisher(q_temp1, Hf_temp1, E_temp1, B_temp1, dB_dy_temp1, dHf_dy_temp1)

			a = q_p**2 / 4.d0
			bb = q_p * q / 2.d0
			c = q**2 - (Hf_temp2 / (E_temp2-B_temp2))**2


			if(bb**2 > 4.d0*a*c) THEN

				dx_temp1 = (-bb + sqrt(bb**2 - 4.d0*a*c)) / (2.d0 * a)
				dx_temp2 = (-bb - sqrt(bb**2 - 4.d0*a*c)) / (2.d0 * a)

				if(dx_temp1 > 0) THEN
					dx_temp = dx_temp1
				elseif(dx_temp2 > 0) then
					dx_temp = dx_temp2
				else

					dx_temp = minval((/dx_temp1, dx_temp2/))
				endif
			else

				dx_l_current = dx_l_current / 2.d0
				return
			endif


			dx_l_current = abs(dx_temp)
			error = .true.
			return
!			stop
		endif

		k_y3 = y_prime(q_temp2, Hf_temp2, E_temp2, B_temp2)  * dx_l_current
		k_E3 = E_prime(q_temp2, Hf_temp2, E_temp2, B_temp2) * dx_l_current
		k_q3 = q_prime_fisher(q_temp2, Hf_temp2, E_temp2, B_temp2, dB_dy_temp2, dHf_dy_temp2) * dx_l_current

		if (isnan(k_y3) .or. isnan(k_E3) .or. isnan(k_q3) .or. k_q3 == -999999) THEN
			error = .true.

			return
		endif

		! now with the third step, a prediction of the third interval is made

		y_temp3 = y_l +  k_y3
		E_temp3 = E +  k_E3
		q_temp3 = q +  k_q3


		! with those values, the final slope estimate is made

		x_l_temp = x_l + dx_l_current

		call flowline_location(current_interior_direction, x_start, y_start, x_l_temp, y_temp3, &
					     x_current, y_current)

		B_temp3 = elevation(x_current,y_current)

		Hf_temp3 = get_tau(x_current,y_current) / (rho_ice*g)


		call flowline_location(current_interior_direction, x_start, y_start, x_l_temp, y_temp3, &
					     x_current1, y_current1)

		dB_dy_temp3 = y_gradient(x_current1, y_current1, current_interior_direction)

		dHf_dy_temp3 = Hf_gradient(x_current1, y_current1, current_interior_direction)



		if((Hf_temp3/(E_temp3-B_temp3))**2 < q_temp3**2) THEN ! failure expected


			q_p = q_prime_fisher(q_temp2, Hf_temp2, E_temp2, B_temp2, dB_dy_temp2, dHf_dy_temp2)

			a = q_p**2 / 4.
			bb = q_p * q / 2.

			c = q**2 - (Hf_temp3 / (E_temp3-B_temp3))**2


			if(bb**2 > 4*a*c) THEN

				dx_temp1 = (-bb + sqrt(bb**2 - 4*a*c)) / (2 * a)
				dx_temp2 = (-bb - sqrt(bb**2 - 4*a*c)) / (2 * a)

				if(dx_temp1 > 0) THEN
					dx_temp = dx_temp1
				elseif(dx_temp2 > 0) then
					dx_temp = dx_temp2
				else

					dx_temp = minval((/dx_temp1, dx_temp2/))
				endif
			else
				dx_l_current = dx_l_current / 2
				return
			endif


			dx_l_current = abs(dx_temp)
			error = .true.
			return
!			stop
		endif


		k_y4 = y_prime(q_temp3, Hf_temp3, E_temp3, B_temp3)  * dx_l_current
		k_E4 = E_prime(q_temp3, Hf_temp3, E_temp3, B_temp3)  * dx_l_current
		k_q4 = q_prime_fisher(q_temp3, Hf_temp3, E_temp3, B_temp3, dB_dy_temp3, dHf_dy_temp3)  * dx_l_current

		if (isnan(k_y4) .or. isnan(k_E4) .or. isnan(k_q4)) THEN
			error = .true.
			return
		endif


		! with all the k values calculated, the values for E, q, and y for the next interval of flowline can be calculated

		y_out = inc_next(y_l, k_y1, k_y2, k_y3, k_y4, dx_l_current)
		E_out = inc_next(E, k_E1, k_E2, k_E3, k_E4, dx_l_current)
		q_out = inc_next(q, k_q1, k_q2, k_q3, k_q4, dx_l_current)

		x_l_temp = x_l + dx_l_current

		call flowline_location(current_interior_direction, x_start, y_start, x_l_temp, y_out, &
					     x_current1, y_current1)
		B_next = elevation(x_current1, y_current1)

		H_f = get_tau(x_current1,y_current1) / (rho_ice*g)
		dE_ds = H_f / (E_out - B_next)

		if (abs(dE_ds) < abs(q_out)) THEN
			error = .true.
			return
		endif

		return
	end subroutine RK4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1


	double precision function y_prime(q, Hf, E, B)
!		use convert
		implicit none
		double precision, intent(in) :: q, hf, E, B

		y_prime = q / sqrt( (Hf / (E-B))**2 - q**2)


	end function y_prime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	double precision function E_prime(q, Hf, E, B)

		implicit none
		double precision, intent(in) :: q, hf, E, B

		E_prime = Hf**2 / ( (E-B)**2 * sqrt( (Hf / (E-B))**2 - q**2))


	end function E_prime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	double precision function q_prime(q, Hf, E, B, dB_dy)

! if using a constant shear stress, this function is used.

		implicit none
		double precision, intent(in) :: q, hf, E, B, dB_dy
		double precision :: p

		p = sqrt( (Hf / (E-B))**2 - q**2)


		q_prime = Hf**2 * (dB_dy - q) / ( (E-B)**3 * p) 


	end function q_prime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	double precision function q_prime_fisher(q, Hf, E, B, dB_dy, dHf_dy)

! if using a variable shear stress, this function is used. From Fisher's equation A8

		implicit none
		double precision, intent(in) :: q, Hf, E, B, dB_dy, dHf_dy
		double precision :: p, l, r

		l = (Hf / (E-B))**2
		r = q**2

		p = sqrt(l - r)

		q_prime_fisher = Hf**2 * (dB_dy - q) / ( (E-B)**3 * p) + Hf / (p * (E-B)**2) * dHf_dy

		if(r >= l  ) THEN ! it seems that ifort doesn't send out a NaN in this case
			q_prime_fisher = -999999
		endif

	end function q_prime_fisher

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	double precision function inc_next(yi, k1, k2, k3, k4, h)

		implicit none
		double precision, intent(in) :: yi, k1, k2, k3, k4, h

		inc_next = yi + 1.d0/6.d0 * (k1 + 2.d0*k2 + 2.d0*k3 + k4)

	end function inc_next

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine find_violations(number_points, contour_previous_x, contour_previous_y, contour_next_x, contour_next_y,  &
				   number_crossovers, crossover_index)

! this subroutine searches the flowlines that connect two flowlines and looks for crossovers with the previous boundary
!
! requires that the contours don't have any gaps (i.e. it has been shrunk down)
!
! it returns the array index of the second line (i.e. the lines of counter-1 and counter)
	use global_parameters
	implicit none

	integer, intent(in) :: number_points
	double precision, dimension(number_points), intent(in) :: contour_previous_x, contour_previous_y, contour_next_x, &
		contour_next_y
	integer, intent(out) :: number_crossovers
	integer, dimension(number_points), intent(out) :: crossover_index

	integer :: counter, l_bound, u_bound
	double precision ::  x_temp, y_temp
	logical :: is_crossover

	number_crossovers = 0

	check_points: do counter = 1, number_points, 1


		check_crossings: do l_bound = 1, number_points, 1

			if(l_bound == number_points) THEN
				u_bound = 1
			else
				u_bound = l_bound + 1
			endif

			call crossover_test(contour_previous_x(counter), contour_previous_y(counter), &
						  contour_next_x(counter), contour_next_y(counter), contour_previous_x(l_bound),&
						  contour_previous_y(l_bound), contour_previous_x(u_bound), &
						  contour_previous_y(u_bound), is_crossover, x_temp, y_temp)


			if(is_crossover ) THEN ! there is a crossover

				number_crossovers = number_crossovers + 1
				crossover_index(number_crossovers) = counter
				exit check_crossings

			endif

		end do check_crossings

	end do check_points

!	write(6,*) "number of violations", number_crossovers, " out of", number_points

	return


end subroutine find_violations
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine find_crossovers(number_points, contour_previous_x, contour_previous_y, contour_next_x, contour_next_y,  &
				   number_crossovers, crossover_index)

! this subroutine searches the flowlines that connect two flowlines and looks for crossovers
!
! requires that the contours don't have any gaps (i.e. it has been shrunk down)
!
! it returns the array index of the second line (i.e. the lines of counter-1 and counter)

	implicit none

	integer, intent(in) :: number_points
	double precision, dimension(number_points), intent(in) :: contour_previous_x, contour_previous_y, contour_next_x, &
		contour_next_y
	integer, intent(out) :: number_crossovers
	integer, dimension(number_points), intent(out) :: crossover_index

	integer :: counter
	double precision ::  x_temp, y_temp
	logical :: is_crossover

	number_crossovers = 0

	do counter = 1, number_points, 1

		if(counter == 1) THEN

			call crossover_test(contour_previous_x(number_points), contour_previous_y(number_points), &
						  contour_next_x(number_points), contour_next_y(number_points), contour_previous_x(counter), &
						  contour_previous_y(counter), contour_next_x(counter), contour_next_y(counter), &
					        is_crossover, x_temp, y_temp)

		else

			call crossover_test(contour_previous_x(counter-1), contour_previous_y(counter-1), &
						  contour_next_x(counter-1), contour_next_y(counter-1), contour_previous_x(counter), &
						  contour_previous_y(counter), contour_next_x(counter), contour_next_y(counter), &
					        is_crossover, x_temp, y_temp)
		endif


		if(is_crossover) THEN ! there is a crossover

			number_crossovers = number_crossovers + 1

			crossover_index(number_crossovers) = counter

		endif


	end do

	return


end subroutine find_crossovers

!!!!!!!!!!!!!!!!!!!!

subroutine contour_crossover(number_points, contour_x, contour_y, number_crossovers, x_crossover_store, &
				     y_crossover_store, crossover_index1, crossover_index2,crossover_array_size,exceeds_limits)

! this subroutine determines if an input polygon is simple or not, and if not, it returns the array index of the point before the
! crossover, and also the coordinates of the crossover

! note that on July 12, I added the input variable crossover_array_size to this subroutine (the size of the output arrays), so
! it is no longer compatible with the the original "step_flow_line" subroutine

! July 25: added a logical expression to the argument list to throw an error if the amount of crossovers exceeds the
! storage array size. It should loop in the main driver subroutine to increase the available memory if it runs into this problem.

	implicit none

	integer, intent(in) :: number_points, crossover_array_size
	double precision, dimension(number_points), intent(in) :: contour_x, contour_y

	integer, intent(out) :: number_crossovers
	logical, intent(out) :: exceeds_limits
	double precision, dimension(crossover_array_size), intent(out) :: x_crossover_store, y_crossover_store
	integer, dimension(crossover_array_size), intent(out) :: crossover_index1, crossover_index2

	integer :: initial_segment_counter, compare_segment_counter, counter
	double precision :: x_temp, y_temp
	logical :: is_crossover

! find the crossovers

	exceeds_limits = .false.

	number_crossovers = 0

	initial_loop: do initial_segment_counter = 1, number_points - 2, 1

		do compare_segment_counter = initial_segment_counter+2, number_points, 1

			if (compare_segment_counter < number_points) THEN
				call crossover_test(contour_x(initial_segment_counter), contour_y(initial_segment_counter), &
					 	   contour_x(initial_segment_counter+1), contour_y(initial_segment_counter+1), &
						   contour_x(compare_segment_counter), contour_y(compare_segment_counter), &
						   contour_x(compare_segment_counter+1), contour_y(compare_segment_counter+1), &
					 	   is_crossover, x_temp, y_temp)

			else
				call crossover_test(contour_x(initial_segment_counter), contour_y(initial_segment_counter), &
					 	   contour_x(initial_segment_counter+1), contour_y(initial_segment_counter+1), &
						   contour_x(compare_segment_counter), contour_y(compare_segment_counter), &
						   contour_x(1), contour_y(1), &
					 	   is_crossover, x_temp, y_temp)

			endif



			if(is_crossover) THEN

				number_crossovers = number_crossovers + 1

				if(number_crossovers > crossover_array_size) THEN
					exceeds_limits = .true.
					exit initial_loop
				endif
				crossover_index1(number_crossovers) = initial_segment_counter
				crossover_index2(number_crossovers) = compare_segment_counter 
				x_crossover_store(number_crossovers) = x_temp
				y_crossover_store(number_crossovers) = y_temp

			endif



		end do

	end do initial_loop


	return

end subroutine contour_crossover


!!!!!!!!!!!!!!!!!!!!

logical function polygon_test(number_points, contour_previous_x, contour_previous_y, contour_next_x, &
					contour_next_y)

! this function tests the input polygon to see whether or not it should be rejected based on overlap of the current contour
! with lower elevations. If the function returns true, then the polygon should be rejected



	implicit none

	integer, intent(in) :: number_points

	double precision, dimension(number_points), intent(in) :: contour_next_x, contour_next_y, contour_previous_x, &
		contour_previous_y

	integer :: boundary_counter_next, boundary_counter_previous, boundary_counter
	double precision :: temp_x, temp_y
	logical :: is_crossover

	check_boundary_previous: do boundary_counter_previous = 1, number_points, 1

		! don't bother checking points that haven't moved, as that is likely the polygon crossover
		if (contour_previous_x(boundary_counter_previous) /= contour_next_x(boundary_counter_previous) .and. &
		    contour_previous_y(boundary_counter_previous) /= contour_next_y(boundary_counter_previous)) THEN 

			check_boundary_next: do boundary_counter_next = 1, number_points, 1

				if (boundary_counter_previous /= boundary_counter_next .and. &
					boundary_counter_previous /= boundary_counter_next+1) THEN

					if (boundary_counter_next < number_points ) THEN

						call crossover_test(contour_previous_x(boundary_counter_previous), &
							contour_previous_y(boundary_counter_previous),  &
							contour_next_x(boundary_counter_previous), contour_next_y(boundary_counter_previous), &
							contour_next_x(boundary_counter_next), contour_next_y(boundary_counter_next), &
							contour_next_x(boundary_counter_next+1), contour_next_y(boundary_counter_next+1), &
							is_crossover, temp_x, temp_y)

						if(is_crossover) THEN


							exit check_boundary_previous
						endif
					else

						call crossover_test(contour_previous_x(boundary_counter_previous), &
							contour_previous_y(boundary_counter_previous),  &
							contour_next_x(boundary_counter_previous), contour_next_y(boundary_counter_previous), &
							contour_next_x(boundary_counter_next), contour_next_y(boundary_counter_next), &
							contour_next_x(1), contour_next_y(1), &
							is_crossover, temp_x, temp_y)
						if(is_crossover) THEN

							exit check_boundary_previous
						endif

					endif

					if(is_crossover) THEN
						exit check_boundary_previous
					endif
				endif

			end do check_boundary_next
		endif

	end do check_boundary_previous
!	write(6,*) "crossover check resulted in:", is_crossover
	polygon_test = is_crossover

end function polygon_test


!!!!!!!!!!!!!!!!!!!!


logical function crossover_array(x_array,y_array,array_size,x1,y1,x2,y2,connect)

	! this function takes an input line (x1,y1,x2,y2) and checks to see if it crosses over the input array. Assumes the last and first point of the array are connected. Returns .true. if a crossover happens
	implicit none
	integer, intent(in) :: array_size
	double precision, intent(in), dimension(array_size) :: x_array, y_array
	double precision, intent(in)  :: x1,y1,x2,y2
	logical, intent(in) :: connect

	integer :: counter, next_index, end_index
	logical :: is_crossover
	double precision :: crossover_x, crossover_y


	if(connect) THEN
		end_index = array_size
	else
		end_index = array_size - 1
	endif

	crossover_array = .false.
	check: do counter = 1, end_index

		if(counter == array_size) THEN
			next_index = 1
		else
			next_index = counter+1
		end if

		call crossover_test(x_array(counter), y_array(counter), x_array(next_index), y_array(next_index), x1,y1,x2,y2, &
		  is_crossover, crossover_x, crossover_y)

		if(is_crossover) THEN
			crossover_array = .true.
			exit check
		endif

	end do check


end function crossover_array

subroutine crossover_test(line1_x1, line1_y1, line1_x2, line1_y2, line2_x1, line2_y1, line2_x2, line2_y2, &
				  is_crossover, crossover_x, crossover_y)

! this subroutine takes two line segments as input, and returns the crossover point coordinates, and whether or not the crossover
! occurs on the line segments

	implicit none

	double precision, intent(in) :: line1_x1, line1_y1, line1_x2, line1_y2, line2_x1, line2_y1, line2_x2, line2_y2
	double precision, intent(out) :: crossover_x, crossover_y
	logical, intent(out) :: is_crossover

	double precision :: slope1, slope2, intercept1, intercept2




	if(line1_x2 == line1_x1 .and. line1_y2 == line1_y1) THEN ! the points are identical
		is_crossover = .false.
		return
	endif

	if(line2_x2 == line2_x1 .and. line2_y2 == line2_y1) THEN ! the points are identical
		is_crossover = .false.
		return
	endif

	!I assumed this check would not be necessary (and it shouldn't be), but it is returning true when it shouldn't be
	! if the crossover point is at the endpoints, it should be returning false

	if(line1_x1 == line2_x1 .and. line1_y1 == line2_y1) THEN
		is_crossover = .false.
		return
	endif

	if(line1_x1 == line2_x2 .and. line1_y1 == line2_y2) THEN
		is_crossover = .false.
		return
	endif

	if(line1_x2 == line2_x1 .and. line1_y2 == line2_y1) THEN
		is_crossover = .false.
		return
	endif

	if(line1_x2 == line2_x2 .and. line1_y2 == line2_y2) THEN
		is_crossover = .false.
		return
	endif


	slope1 = (line1_y2 - line1_y1) / (line1_x2 - line1_x1)
	intercept1 = line1_y2 - slope1 * line1_x2 

	slope2 = (line2_y2 - line2_y1) / (line2_x2 - line2_x1)
	intercept2 = line2_y2 - slope2 * line2_x2 



	if (slope1 == slope2) THEN ! lines are parallel
		is_crossover = .false.
		return
	endif

	crossover_x = (intercept1-intercept2) / (slope2-slope1)
	crossover_y = slope1 * crossover_x + intercept1

	is_crossover = .false.
	if(crossover_x > min(line1_x1, line1_x2)) THEN
	  if(crossover_x < max(line1_x1, line1_x2)) THEN
	     if(crossover_x > min(line2_x1, line2_x2)) THEN
		 if(crossover_x < max(line2_x1, line2_x2)) THEN
	   	   if(crossover_y > min(line1_y1, line1_y2)) THEN
		     if(crossover_y < max(line1_y1, line1_y2)) THEN
	   		 if(crossover_y > min(line2_y1, line2_y2)) THEN
			   if(crossover_y < max(line2_y1, line2_y2)) THEN

				is_crossover = .true.
			   endif
			 endif
		     endif
		   endif
		 endif
	     endif
	   endif
	endif





end subroutine crossover_test

!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine splitpoints(x_contour_next, y_contour_next, current_boundary_points, distance_counter, &
			     distance_index, E, current_elevation) 
	use global_parameters
	implicit none

	double precision, intent(in) :: current_elevation
	integer, intent(in) :: current_boundary_points
	double precision, dimension(current_boundary_points), intent(in) :: x_contour_next, y_contour_next, E
	integer, intent(out) :: distance_counter
	integer, dimension(current_boundary_points), intent(out) :: distance_index

	integer :: counter, u_bound
	double precision :: distance

	distance_index = 0
	distance_counter = 0

	do counter = 1, current_boundary_points, 1

		if (counter == current_boundary_points) THEN
			u_bound = 1
		else
			u_bound = counter + 1
		endif

		distance = sqrt((x_contour_next(u_bound) - x_contour_next(counter))**2 + &
				(y_contour_next(u_bound) - y_contour_next(counter))**2)

		if ((distance)/minimum_spacing > minimum_spacing .and. dble(nint(E(counter))) == current_elevation .and. &
			dble(nint(E(u_bound))) == current_elevation ) THEN ! split line

			distance_counter = distance_counter + 1
			distance_index(distance_counter) = counter

		endif

	end do
	
	

end subroutine splitpoints
!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!

logical function check_polygon2(number_points, x, y, direction, E, current_elevation, crossover_point)

! this function returns true if the polygon is rejected

! this function shrinks the polygon by a small amount, and checks that the points are within the old polygon. If not, the direction is the wrong way.

	integer, intent(in) :: number_points
	double precision, dimension(number_points), intent(in) :: x, y, direction, E
	logical, dimension(number_points), intent(in) :: crossover_point
	double precision, intent(in) :: current_elevation
	

	integer :: counter, number_failures, crossovers
	double precision :: area1, area2
	double precision :: x_expand, y_expand, cart_dir
	
	double precision, parameter :: expand_factor = 10 ! 10 m


	if(count(.not.crossover_point) < 3 .or. number_points < 3) THEN !it won't work
		check_polygon2 = .true.
		return
	endif

	crossovers = 0

	number_failures = 0
	do counter = 1, number_points, 1
		! direction is stored as azimuth
		cart_dir = -(direction(counter) - pi/2.)
		x_expand = x(counter) + expand_factor * cos(cart_dir)
		y_expand = y(counter) + expand_factor * sin(cart_dir)



		if (.not. crossover_point(counter)) THEN


			if( .not. point_in_polygon(x, y, x_expand, y_expand, number_points)) THEN


				number_failures = number_failures + 1


			endif

		else
			crossovers = crossovers + 1

		endif

	end do



	if (dble(number_failures) / (dble(number_points-crossovers)) < 0.25d0 .and. number_points > 2) THEN ! don't reject

		check_polygon2 = .false.

	else

		check_polygon2 = .true.

	endif

end function check_polygon2


logical function check_inside(number_points,x, y, x_point, y_point, direction)

! this function returns true if the point is rejected

! this function shrinks the point by a small amount, and checks that the point is within the old polygon. If not, the direction is the wrong way.

	integer, intent(in) :: number_points
	double precision, dimension(number_points), intent(in) :: x, y
	double precision, intent(in) :: x_point, y_point, direction

	double precision, dimension(4) :: x_test, y_test

	double precision :: x_expand, y_expand, cart_dir
	
	double precision, parameter :: expand_factor = 10000 ! 10 m

	integer :: counter
		! direction is stored as azimuth
	cart_dir = -(direction - pi/2.)
	x_expand = x_point + expand_factor * cos(cart_dir)
	y_expand = y_point + expand_factor * sin(cart_dir)



	if( point_in_polygon(x(1:number_points), y(1:number_points), x_expand, y_expand, number_points)) THEN

		check_inside = .false.
	else
		check_inside = .true.
	endif


end function check_inside

!!!!!!!!!!!!!!!!!!!!!!!!

subroutine step_elevation(x, y, interior_direction, x_next, y_next, E_next, current_elevation)

! this subroutine is called when the elevation of the boundary is at some point defined to be too low to actually have grounded ice
! (i.e. off the continental shelf)

!	use global_parameters
	use grids
	implicit none

	double precision, intent(in) :: x, y, interior_direction,  current_elevation
	double precision, intent(out) :: x_next, y_next, E_next
	double precision, parameter :: step = 10.d0 ! distance increment
	double precision :: cos_angle, sin_angle

	cos_angle = step * cos(interior_direction)
	sin_angle = step * sin(interior_direction)

	E_next = elevation(x, y)

	x_next = x
	y_next = y

	do while (E_next < current_elevation)

		x_next = x_next + cos_angle
		y_next = y_next + sin_angle

		E_next = elevation(x_next, y_next)

	end do


end subroutine step_elevation

!!!!!!!!!!!!!!!!!!!!!!!!



END SUBROUTINE find_flowline
