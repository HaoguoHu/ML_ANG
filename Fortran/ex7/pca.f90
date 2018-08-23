	subroutine pca(M, N, X, U, S)
	implicit none
	
	integer::M, N, i, stat
	double precision, dimension(M,N)::X
	double precision, dimension( N)::S, work
	double precision, dimension(N,N)::sigma, V, U
	
	U = 0
	S = 0
	
	sigma = matmul(transpose(X), X )/M
	
	U = sigma
	
	call svd(N, N, N, N, U, S, V, work, stat)
	
	write(*,*)'stat=',stat
	
	if(stat > 0)then
		write(*,*)'convergence failed, stopped'
		stop
	else if(stat < 0)then
		write(*,*)' X is wrong shape, stopped.'
		stop
	endif
	
	
	end subroutine pca
