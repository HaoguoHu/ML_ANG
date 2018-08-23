	!% This function returns a discretized value (a number) for a continuous
	!% state vector. Currently x is divided into 3 "boxes", x_dot into 3,
	!% theta into 6 and theta_dot into 3. A finer discretization produces a
	!% larger state space, but allows a better policy.
	subroutine  get_state(x, x_dot, theta, theta_dot, state)
	implicit none

	integer, parameter::total_states=163
	real:: x, x_dot, theta, theta_dot
	!% Parameters for state discretization in get_state
	real::one_degree = 0.0174532	
	real::six_degrees = 0.1047192
	real::twelve_degrees = 0.2094384
	real::fifty_degrees = 0.87266

	integer:: state
	
	
if (x < -2.4 .or. x > 2.4  .or. theta < -twelve_degrees .or. theta > twelve_degrees)then
  	state = total_states - 1  
else
  	if (x < -1.5)then
    	state = 0
  	elseif (x < 1.5)then
    	state = 1
  	else		  
    	state = 2
  	endif
  
  	if (x_dot < -0.5) then		       
    
  	elseif (x_dot < 0.5)then
    	state = state + 3
  	else
    	state = state + 6
  	endif
     
  	if (theta < -six_degrees)then 	       
    
  	elseif (theta < -one_degree)then
    	state = state + 9
  	elseif (theta < 0)then
    	state = state + 18
  	elseif (theta < one_degree)then
    	state = state + 27
  	elseif (theta < six_degrees)then
    	state = state + 36
  	else
    	state = state + 45
  	endif
  
  	if (theta_dot < -fifty_degrees)then    
  	elseif (theta_dot < fifty_degrees) then 
    	state = state + 54
  	else
    	state = state + 108
  	endif
  
endif

	state = state + 1
	
	end subroutine get_state
