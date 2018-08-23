	subroutine computeCentroids(M, N, X, idx, K, centroids)
	implicit none
		
	integer::i, j, M, N, K, count
	double precision,dimension(M,N)::X
	double precision,dimension(K,N):: centroids
	double precision,dimension(M)::idx
	double precision,dimension(K)::ck
	
	ck = 0
	centroids = 0
	
!	do i=1, M
!	do j=1, K
!		if(j == idx(i))then		
!			centroids(j,:) = centroids(j, :) + X(i, :)
!			ck(j) = ck(j) + 1
!		endif
!	enddo
!	enddo
!	
!	do j=1, K
!		centroids(j,:) = centroids(j,:)/ck(j)
!	enddo	
	
	
	do j=1, K
		count = 0
		do i=1, M
			if(j == idx(i))then		
			centroids(j,:) = centroids(j, :) + X(i, :)
			count = count + 1	
			endif
		enddo
			centroids(j,:) = centroids(j,:)/count
	enddo
		
	end subroutine computeCentroids
