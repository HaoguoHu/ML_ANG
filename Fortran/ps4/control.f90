	program control
	implicit none
	
	integer,parameter:: max_failures = 500
	real,parameter::pause_time=0.001,  GAMMA=0.995, TOLERANCE=0.01
	real::x, x_dot, theta, theta_dot, R, rom
	
	
	integer, dimension(max_failures)::time_steps_to_failure 	
	integer::time, NO_LEARNING_THRESHOLD 	
	integer::time_at_start_of_current_trial
	integer::num_failures, NUM_STATES, min_trial_length_to_start_display	
	integer::state, action, new_state, display_started 
	
	display_started=0
	time = 0
	NO_LEARNING_THRESHOLD = 20 		
	time_steps_to_failure = 0
	time_at_start_of_current_trial = 0	
	min_trial_length_to_start_display = 0
	num_failures=0
	NUM_STATES = 163
	 
	x = 0.0 
	x_dot = 0.0 
	theta = 0.
	theta_dot = 0.0

	call get_state(x, x_dot, theta, theta_dot, state)
	write(*,*)'state=', state

	if(min_trial_length_to_start_display == 0 .or. display_started == 1)then
  		call show_cart(x, x_dot, theta, theta_dot, pause_time)
	endif

!    read(*,*)
!%%%% END YOUR CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%
do while(num_failures < max_failures)

  	if (theta < 0) then
    	action = 1
  	else
    	action = 2
  	endif  
!%%% END YOUR CODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
 	call cart_pole(action, x, x_dot, theta, theta_dot)

  	time = time + 1 

	call get_state(x, x_dot, theta, theta_dot, new_state)

  	if(display_started == 1)then
    	call show_cart(x, x_dot, theta, theta_dot, pause_time)
  	endif
 
 
  	if (new_state == NUM_STATES) then
    	R = -1
  	else
!        R=-abs(theta)/2.0
    	R = 0
  	endif


!  %%% CODE HERE: Perform updates %%%%%%%%% 
!  % Recompute MDP model whenever pole falls
!  % Compute the value function V for the new model

  if (new_state == NUM_STATES) then

!    % Update MDP model using the current accumulated statistics about the
!    % MDP - transitions and rewards.
!    % Make sure you account for the case when total_count is 0, i.e., a
!    % state-action pair has never been tried before, or the state has
!    % never been visited before. In that case, you must not change that
!    % component (and thus keep it at the initialized uniform distribution).
 
!    % Perform value iteration using the new estimated model for the MDP
!    % The convergence criterion should be based on TOLERANCE as described
!    % at the top of the file.
!    % If it converges within one iteration, you may want to update your
!    % variable that checks when the whole simulation must end
		write(*,*)'Paused. Press enter to continue'
 		read(*,*)   
  endif
    

  	if (new_state == NUM_STATES)then
    	num_failures = num_failures + 1
    	time_steps_to_failure(num_failures) = time - time_at_start_of_current_trial
    	time_at_start_of_current_trial = time

!    	time_steps_to_failure(num_failures)

    	if (time_steps_to_failure(num_failures) >  min_trial_length_to_start_display)then
      		display_started = 1
    	endif
    
		call random_number(rom)		
    	x = -1.1 + rom * 2.2
	
   		x_dot = 0.0 
		theta = 0.0 
		theta_dot = 0.0
    	call get_state(x, x_dot, theta, theta_dot, state)
  	else 
    	state = new_state
  	endif
  
enddo

	end

!plot_learning_curve
