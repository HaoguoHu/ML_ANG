	subroutine plotProgresskMeans(M, N, X, centroids, previous, idx, K, i)
	implicit none
	
	integer::M, N, K, i, j
	double precision,dimension(M,N)::X
	double precision,dimension(K,N)::initial_centroids, centroids, previous
	double precision,dimension(M)::idx
	
	call plotDataPoint(M, N, X, idx, K, centroids, previous )
	
!	do j=1, K
!		drawLine(centroids(j,:), previous(j, :))
!	enddo
	
	
	
	end subroutine plotProgresskMeans
