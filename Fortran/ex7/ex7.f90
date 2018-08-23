

	program ex7
	implicit none
	
	integer,parameter::M=300, N=2, K=3,  KK=16, mp=16384, IM=128, JM=128 !128*128=16384
	integer::nfile=20, nf=20
	double precision,dimension(M,N)::X
	double precision,dimension(K,N)::initial_centroids, centroids
	double precision,dimension(M)::idx
	
	integer::i, j, max_iters, ture, false, itmp, jtmp
	double precision,dimension(mp, K):: A
	double precision,dimension(KK,K)::ini_centroids 
	double precision,dimension(mp)::idxx
	
	open(nfile,file='ex7data2.txt',status='old')
	do i=1, M
		read(nfile,*)X(i,1), X(i,2)
	enddo
	close(nfile)
	
	initial_centroids = transpose(reshape((/3, 3, 6, 2, 8, 5/), (/N, K/)))

	call findClosestCentroids(M, N, K, X, initial_centroids, idx)
	write(*,*)'Closest centroids for the first 3 examples:', int(idx(1:3))
	write(*,*)'(the closest centroids should be 1, 3, 2 respectively)'
	write(*,*)
	
	call computeCentroids(M, N, X, idx, K, centroids)
	write(*,*)'Centroids computed after initial finding of closest centroids: '
	do i=1, K
	write(*,*) (centroids(i,j),j=1,N)
	enddo
	write(*,*)'(the centroids should be:)'
	write(*,*)'   [ 2.428301 3.157924 ]'
	write(*,*)'   [ 5.813503 2.633656 ]'
	write(*,*)'   [ 7.119387 3.616684 ]'
	write(*,*)

	max_iters = 10
	ture = 1; false = 0
	call runkMeans(M, N, K, X, initial_centroids, max_iters, false)
	
	open(nfile,file='bird_small.txt',status='old')
	do i = 1, mp
		read(nfile, *)itmp, jtmp, A(i,1),A(i,2),A(i,3)
	enddo
	close(nfile)

	open(nf,file='commandbird00.txt', status='replace')
	 	write(nf,'(a)')'rgb(r,g,b) = 65536 * int(r) + 256 * int(g) + int(b)'
		write(nf,'(a)')'plot "bird_small.txt" using 1:2:(rgb($3,$4,$5)) with points pt 7 ps 1 lc rgb variable'		
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist commandbird00.txt')
!	call system('gnuplot -persist plot "bird_small.png"  binary filetype=png with rgbimage')


	A = A /255
	call kMeansInitCentroids(mp, K, A, KK, ini_centroids)
	call runkMeans(mp, K, KK, A, ini_centroids, max_iters, false)
	call findClosestCentroids(mp, K, KK, A, ini_centroids, idxx)	

	do i=1, mp
	do j=1, KK
	if(idxx(i) == j) A(i,:) = ini_centroids(j,:)
	enddo
	enddo
	
	A = int(A * 255)
	open(nfile,file='bird_small2.txt',status='unknown')
	do i = 1, IM
	do j = 1, JM
		write(nfile, *)i, j, (A((i-1)*IM+j,itmp),itmp=1,K)
	enddo
	enddo
	close(nfile)
	
	
	open(nf,file='commandbird02.txt', status='replace')
	 	write(nf,'(a)')'rgb(r,g,b) = 65536 * int(r) + 256 * int(g) + int(b)'
		write(nf,'(a)')'plot "bird_small2.txt" using 1:2:(rgb($3,$4,$5)) with points pt 7 ps 1 lc rgb variable'		
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist commandbird02.txt')


	end
