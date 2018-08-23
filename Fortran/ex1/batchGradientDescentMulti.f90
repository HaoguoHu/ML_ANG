      subroutine batchGradientDescentMulti(M, N, alpha, iterations, X, y, theta)
	implicit none
	
	integer:: i,j,  M, N, iter, iterations
	real::temp0, temp1, alpha, thetaX
	real,dimension(N):: theta, temp
	real,dimension(N-1, M):: X
	real,dimension(N, M):: XX
	real,dimension(M):: y
	
	
		    XX(1, :) =1
		do j = 2, N
		    XX(j, :) = X(j-1, :)
		enddo
	
	do iter = 1, iterations  	  
	        temp  = 0  
    	    do i = 1, M	
	        thetaX = 0       		 
	        do j = 1, N
		     thetaX = thetaX + theta(j)*XX(j, i)
		  enddo	
        	  temp(:) = temp(:) + (thetaX - y(i)) * XX(:,i)		
    	     enddo        
    	     theta = theta - (alpha/m) * temp	   
	enddo
	
	end subroutine batchGradientDescentMulti
