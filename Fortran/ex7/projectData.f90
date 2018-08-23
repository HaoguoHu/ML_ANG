	subroutine projectData(M, N, X, U, K, Z)
	implicit none
	
	integer::M, N, K
	double precision, dimension(M,N)::X
	double precision, dimension(N,N):: U
	double precision, dimension(M,K)::Z
	
	
	Z = matmul(X , U(:, 1:K))
	
	
	
	end subroutine projectData
