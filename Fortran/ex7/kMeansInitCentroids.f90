	subroutine kMeansInitCentroids(mp, K, A, KK, centroids)
	implicit none
	
	integer::mp, K, KK
	double precision,dimension(KK, K):: centroids
	double precision,dimension(mp, K)::A
	double precision:: random
	integer::i, j, randidx
	

	do j=1, KK
		call  random_number(random)
		randidx = ceiling(mp *random)
		centroids(j, :) = A(randidx, :)
	enddo
	
	
	
	end subroutine kMeansInitCentroids
