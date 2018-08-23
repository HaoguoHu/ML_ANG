	subroutine trainLinearReg(M, N, X, y, lambda, theta)
	implicit none
	
	integer::M,  N
	double precision,dimension(M)::y
	double precision,dimension(M, N)::X
	double precision,dimension(N)::theta, grad
	double precision::lambda, Jcost 
	integer::i,j
	
	theta = 0	
	call fmincg(M, N, theta, X, y,lambda, 200);
	
	end subroutine trainLinearReg
