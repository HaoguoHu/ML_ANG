    subroutine batchGradientDescent(M, alpha, iterations, X, y, theta)
	implicit none
	
	integer:: i, M, iter, iterations
	real::temp0, temp1, alpha
	real,dimension(2):: theta
	real,dimension(M):: X, y
	
	do iter = 1, iterations    
    	
		temp0 = 0
    	temp1 = 0   
    	do i = 1, M
        	temp0 = temp0 + (theta(1) + theta(2) * X(i) - y(i)) * 1.
        	temp1 = temp1 + (theta(1) + theta(2) * X(i) - y(i)) * X(i);
    	enddo
    
    	   theta(1) = theta(1) - (alpha/m) * temp0
    	   theta(2) = theta(2) - (alpha/m) * temp1
	enddo
	
	end subroutine batchGradientDescent
