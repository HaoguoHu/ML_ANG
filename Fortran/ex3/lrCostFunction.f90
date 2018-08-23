	subroutine lrCostFunction(theta, y, JCost, grad)
	use mod_common
	implicit none
	
	double precision,dimension(N,1):: theta, grad
	double precision :: JCost
	double precision :: sigmoid, g, TX, theta2
	double precision,dimension(M,1)::y
	integer::i, j
	
	theta2 = 0
	grad = 0
	JCost =0
	
	do i = 1, M
	        TX = 1
		 do j = 1, N
		   TX = TX + X(i,j)*theta(j,1)
		 enddo
		g = sigmoid( TX)	
				 
		JCost = JCost - y(i,1) * log(g)- (1- y(i,1))* log(1- g)
		
		do j = 1, N
		   grad(j,1) = grad(j,1) + X(i,j)* (g - y(i,1))
		enddo
	enddo	
	
		do j = 1, N
		   theta2 = theta2 + theta(j,1)**2
		 enddo
		 
	  JCost = JCost/m + lambda/2/m *theta2
	  
	           grad = grad/m
		do j = 1, N
		      grad(j,1) = grad(j,1) + lambda /m *theta(j,1)
		enddo
		
	end
