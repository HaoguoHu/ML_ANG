	subroutine linearRegCostFunction(M, N, X, y, theta, lambda, Jcost, grad)
	implicit none
	
	integer::M, N
	double precision,dimension(M):: y, h
	double precision,dimension(M, N)::X
	double precision,dimension(N)::theta, grad
	double precision::lambda, Jcost 
	integer::i,j
	
	
	Jcost = 0
	do i =1, M
		h(i) = 0.
		do j = 1, N
			h(i) = h(i) + X(i,j)*theta(j)
		enddo
		
		Jcost = Jcost + (h(i)-y(i))**2./M/2.
	enddo
	
	do j=2,N
		Jcost = Jcost + theta(j)**2 *lambda/M/2.
	enddo
	
	
		grad = 0.
	do i=1, M
		grad(1) = grad(1)+ (h(i)-y(i))*X(i,1)/M
	enddo
		
	do j=2,N
		do i=1, M
		grad(j) = grad(j)+ (h(i)-y(i))*X(i,j)/M + theta(j)*lambda/M
		enddo
	enddo
	
	end subroutine linearRegCostFunction
