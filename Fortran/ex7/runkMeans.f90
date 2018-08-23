	subroutine runkMeans(M, N, K, X, initial_centroids, max_iters, ture)
	implicit none
	
	double precision,dimension(M,N)::X
	double precision,dimension(K,N)::initial_centroids, centroids, previous_centroids
	double precision,dimension(M)::idx
	
	integer::max_iters, M, N, K, ture
	integer::i
	
	write(*,*)M, N, K
	
	centroids = initial_centroids
	previous_centroids =centroids
	
	do i=1, max_iters
		write(*,*)'K-Means iteration ...', i, max_iters
		
		call findClosestCentroids(M, N, K, X, centroids, idx)
		
		if(ture == 1)then
		call plotProgresskMeans(M, N, X, centroids, previous_centroids, idx, K, i)
		endif
		
		 previous_centroids = centroids
		
		call computeCentroids(M, N, X, idx, K, centroids)
		
!		write(*,*)'paused'
!		read(*,*)
		
	enddo
		initial_centroids = centroids
	
	call system('rm -f plot04.dat')
	
	end subroutine runkMeans
