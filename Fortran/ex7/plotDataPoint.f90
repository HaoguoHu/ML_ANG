	subroutine plotDataPoint(M, N, X, idx, K, centroids, previous)
	implicit none
	
	integer::M, N, K
	double precision,dimension(M,N)::X, X1, X2, X3
	double precision,dimension(K,N)::centroids, previous
	double precision,dimension(M)::idx
	integer::nf=20, i1=0, i2=0, i3=0, ii, i, j
	character(len=2)::fn
	
	do II =1, K
	write(fn, '(i2.2)')II	
!	write(*,*)'plot'//fn//'.dat'
	open(nf,file='plot'//fn//'.dat', status='replace') !plot01.dat
	do i=1, M
		if(idx(i) == II)then
			write(nf,*)(X(i,j),j=1,N)
		endif
	enddo
	close(nf)
	enddo
			
	
	write(fn, '(i2.2)')K+1
	open(nf,file='plot'//fn//'.dat', position="append")
	do i=1, K
		write(nf,*)(centroids(i,j),j=1,N)
		write(nf,*)( previous(i,j),j=1,N)
		write(nf,*)
	enddo
	close(nf)
	
	
	open(nf,file='command.txt', status='replace')
	 	write(nf,'(a)')'set title "Iteration numbers" '
		write(nf,'(a)')'plot "plot01.dat" using 1:2 with points pointtype 6 pointsize 2 lt rgb "red",  \'
		write(nf,'(a)')' 	 "plot02.dat" using 1:2 with points pointtype 6 pointsize 2 lt rgb "green",\'
		write(nf,'(a)')'  	 "plot03.dat" using 1:2 with points pointtype 6 pointsize 2 lt rgb "purple", \'
		write(nf,'(a)')'  	 "plot04.dat" using 1:2 with linespoints pointtype 1 pointsize 2 lt rgb "blue"'
		write(nf,'(a)')'q'
	close(nf) 
	
	
	call system('gnuplot -persist command.txt')

	
	end subroutine plotDataPoint
