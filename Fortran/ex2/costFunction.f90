	subroutine costFunction(m, theta, X, y, JCost, grad)
	implicit none
	integer::i, m
	double precision,dimension(2, m) :: X
	integer,dimension(m) :: y
	double precision,dimension(3) :: theta, grad
	double precision :: JCost
	double precision :: sigmoid, g
		
	
	do i = 1, m
		g = sigmoid( theta(1)+ theta(2)*X(1,i) + theta(3)*X(2,i))
		JCost = JCost - y(i) * log(g)- (1- y(i))* log(1- g)
		
		grad(1) = grad(1) - 1.0000*(g -y(i))
		grad(2) = grad(2) - X(1,i)*(g -y(i))
		grad(3) = grad(3) - X(2,i)*(g -y(i))
	enddo
	
	JCost = JCost/m
	grad = grad/m
	
	end
