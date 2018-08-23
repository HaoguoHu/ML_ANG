      subroutine stochasiticGradientDescent(M, alpha, iterations, X, y, theta)
	implicit none
	
	integer:: i, M
	real,dimension(2):: theta
	real,dimension(M):: X, y
		
	
	      theta(1)  = 0
    	      theta(2)  = 0   
    	do i = 1, M
        	theta(1) = theta(1) - (alpha)*(theta(1) + theta(2) * X(i) - y(i)) * 1.
        	theta(2) = theta(2) - (alpha)*(theta(1) + theta(2) * X(i) - y(i)) * X(i);
    	enddo
		
		
	
	end subroutine stochasiticGradientDescent
