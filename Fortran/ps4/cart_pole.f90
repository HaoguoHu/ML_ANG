!function [new_x, new_x_dot, new_theta, new_theta_dot] = cart_pole(action, x, x_dot, theta, theta_dot)
	subroutine cart_pole(action, x, x_dot, theta, theta_dot)
	implicit none
	
	integer::action
	real:: x, x_dot, theta, theta_dot
	real::new_x, new_x_dot, new_theta, new_theta_dot

	real::GRAVITY = 9.82
	real::MASSCART = 1.0
	real::MASSPOLE = 0.3
	real::TOTAL_MASS  
	real::LENGTH = 0.7    !%  actually half the pole's length
	real::POLEMASS_LENGTH 
	real::FORCE_MAG = 10.0 
	real::TAU = 0.01   
	real::FOURTHIRDS = 4./3.
	
	real::xacc, temp,  sintheta, thetaacc, force, action_flip_prob, costheta
	real::force_noise_factor,  no_control_prob, rom

	TOTAL_MASS = MASSPOLE + MASSCART
	POLEMASS_LENGTH = MASSPOLE * LENGTH

	action_flip_prob = 0.00
	force_noise_factor = 0.0   !% multiplied by between 1-.. and 1+.. 
	no_control_prob = 0.00   !% Force is 0 with this probability

	action = action - 1

!% Flip action with action_flip_prob
	call random_number(rom)
	if (rom < action_flip_prob)then
  		action = 1 - action
	endif
  
	if (action > 0)then
  		force =  FORCE_MAG
	else
  		force = -FORCE_MAG
	endif

	force = force * (1 - force_noise_factor + rom * 2 * force_noise_factor)

	call random_number(rom)
	if (rom < no_control_prob)then
  		force = 0
	endif

	costheta = cos(theta)
	sintheta = sin(theta)

	temp = (force + POLEMASS_LENGTH * theta_dot * theta_dot * sintheta) / TOTAL_MASS

	thetaacc=(GRAVITY*sintheta-costheta*temp)/(LENGTH*(FOURTHIRDS-MASSPOLE*costheta*costheta/TOTAL_MASS))

	xacc  = temp - POLEMASS_LENGTH * thetaacc* costheta / TOTAL_MASS

!% Return the new state variables (using Euler's method)

	new_x  = x + TAU * x_dot
	new_x_dot = x_dot + TAU * xacc
	new_theta = theta + TAU * theta_dot
	new_theta_dot = theta_dot + TAU * thetaacc
	
	x = new_x
	x_dot = new_x_dot
	theta = new_theta
	theta_dot = new_theta_dot
	
	end subroutine cart_pole
