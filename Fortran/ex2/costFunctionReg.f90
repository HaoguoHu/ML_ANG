	subroutine costFunctionReg(M, N, X, y, theta, JCost, grad)
	implicit none
	integer::i,j,  M, N
	double precision,dimension(M,N) :: X
	double precision,dimension(M,1) :: y
	double precision,dimension(N,1) :: theta, grad
	double precision :: JCost
	double precision :: sigmoid,  g, lambda, TX, theta2
	
	lambda = 1.
	
	theta2 = 0
	grad = 0
	JCost =0
	
	
	do i = 1, M
		   TX =0
		 do j = 1, N
		   TX = TX + theta(j,1)*X(i, j)
		 enddo
		g = sigmoid( TX)		
				 
		JCost = JCost - y(i,1) * log(g)- (1- y(i,1))* log(1- g)
		
		do j = 1, N
		   grad(j,1) = grad(j,1) + X(i,j)* (g - y(i,1))
		enddo
	enddo	
	
		do j = 1, N
		   theta2 = theta2 + theta(j, 1)**2
		 enddo
		 
	  JCost = JCost/m + lambda/2/m *theta2
	  
	           grad = grad/m
		do j = 1, N
		      grad(j,1) = grad(j, 1) + lambda /m *theta(j, 1)
		enddo
	end
