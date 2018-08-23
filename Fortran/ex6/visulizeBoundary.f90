	subroutine visualizeBoundary(M, N, X, y, model, idx, kernelFunction)
	implicit none
	
	integer, parameter:: Ngrid = 100, nf = 20
	integer::M, N, idx, i, j
	double precision,dimension(M,N)::X
	double precision,dimension(M)::y
	
	double precision,dimension(Ngrid)::x1plot, x2plot
	double precision,dimension(Ngrid,N)::thisX
	double precision,dimension(Ngrid,Ngrid)::X1, X2, vals
	
	character(len=20)::kernelFunction
	
	type::modelL
		double precision, allocatable, dimension(:, :):: X1
		double precision, allocatable, dimension(:)::y, alphas
		character(len=20)::kernelFunction
		double precision::b
		double precision, allocatable, dimension(:)::w 
	end type modelL
	type(modelL)::model
	
	
	do i=1, Ngrid
		x1plot(i) = minval(X(:,1)) + (i-1)*(maxval(X(:,1))-minval(X(:,1)))/(Ngrid-1)		
	enddo
	do i=1,Ngrid
		X1(i, :) = x1plot(:)
	enddo
	
	do i=1, Ngrid
		x2plot(i) = minval(X(:,2)) + (i-1)*(maxval(X(:,2))-minval(X(:,2)))/(Ngrid-1)		
	enddo
	do j=1,Ngrid
		X2(:, j) = x2plot(:)
	enddo
	
	
	vals = 0
	do j = 1, Ngrid
		thisX(:,1) = X1(:, j)
		thisX(:,2) = X2(:, j)
		call svmPredict(Ngrid, N, idx,  model, thisX, vals(:,j), kernelFunction)
	enddo

	open(nf, file='ex6data2con.txt',status='replace')
	do i=1,Ngrid
	do j=1,Ngrid
		write(nf,*)x1(i,j),x2(i,j),vals(i,j)
	enddo
	enddo
	close(nf)
	
	open(nf,file='cmddata2.txt', status='replace')
! 		write(nf,'(a)')'rgb(r,g,b) = 65536 * int(r) + 256 * int(g) + int(b)'
		write(nf,'(a)')'symbol(z) = "o+"[int(z):int(z)]'		
!		write(nf,'(a)')'plot"ex6data2.txt" using 1:2:(symbol($3+1)) with labels textcolor lt 1'
		write(nf,'(a)')'set dgrid3d 100,100 '	
		write(nf,'(a)')'set contour base '
		write(nf,'(a)')'set view map'
		write(nf,'(a)')'unset surface'	
!		write(nf,'(a)')'plot "ex6data2.txt" using 1:2:(symbol($3+1)) with labels textcolor lt 1,\'	
		write(nf,'(a)')' splot "ex6data2con.txt" using 1:2:3 w l'	
	
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist cmddata2.txt')

	
	end subroutine visualizeBoundary
