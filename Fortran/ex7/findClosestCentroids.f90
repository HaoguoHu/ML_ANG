	subroutine findClosestCentroids(M, N, K, X, centroids, idx)
	USE ieee_arithmetic
	implicit none
	
	integer:: i, j, M, N, K, kk
	double precision,dimension(M,N)::X
	double precision,dimension(K,N)::centroids
	double precision,dimension(M)::idx
	double precision:: mindist, dist 
	
	do i=1, M
		mindist = huge(0.0d0)
!		write(*,*)mindist
		do j=1, K
			dist = 0
			do kk=1,N
				dist = dist + (X(i, kk) - centroids(j, kk))**2 
			enddo
			
			if(dist < mindist)then
				mindist = dist
				idx(i) = j
			endif
		enddo
	enddo
		
	
	end subroutine findClosestCentroids
