	subroutine costFunction(M, theta, X, y, Jcost)
	implicit none
	
	integer::i, M
	real::Jcost
	real,dimension(2)::theta
	real,dimension(M)::X, y
	
	JCost = 0.
	do i = 1, M
		JCost = JCost + (theta(1) + theta(2)* X(i) - y(i))**2
	enddo

	JCost = JCost/2./m

	end subroutine  
