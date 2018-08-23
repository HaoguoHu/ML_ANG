	subroutine recoverData(M, N, Z, U, K, X_rec)
	implicit none
	
	integer::M, N, K
	double precision, dimension(M,N)::X_rec
	double precision, dimension(N,N):: U
	double precision, dimension(M,K)::Z
	
	
	X_rec = matmul(Z, transpose( U(:, 1:K)))
	
	end subroutine recoverData
