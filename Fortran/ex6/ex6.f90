	program ex6
	implicit none
	
	integer,parameter::M=51, N=2, M1=3, M2=863
	integer::fnum=20, i, max_passes
	double precision,dimension(M,N)::X
	double precision,dimension(M)::y
	double precision,dimension(M1)::x11, x12
	double precision::sigma, sim, C, gaussianKernel
	character(len=20)::kernelFunction
	
	integer::nf=20, idx
	double precision,dimension(M2,N)::X2
	double precision,dimension(M2)::y2
	
	type::modelL
		double precision, allocatable, dimension(:, :)::X
		double precision, allocatable, dimension(:)::y, alphas
		character(len=12)::kernelFunction
		double precision::b
		double precision, allocatable, dimension(:)::w 
	end type modelL
	type(modelL)::model
	
	double precision::tol

!	goto 1000
	
	open(fnum, file='ex6data1_2.txt',status='old')
	do i=1, M
		read(fnum,*)X(i,1), X(i,2), y(i)
	enddo
	close(fnum)
		

	C = 1.
	tol = 1.e-3
	max_passes = 20
	kernelFunction = 'linearKernel'
	call svmTrain(M, N, X, y, C, kernelFunction, tol, max_passes, model, idx)
	
	deallocate(model%X)
	deallocate(model%y)
	deallocate(model%alphas)
	deallocate(model%w)
	
	
!	write(*,*)model%b, model%w !-10.34, 1.40, 2.13
	
	open(nf,file='cmddata1.txt', status='replace')
! 		write(nf,'(a)')'rgb(r,g,b) = 65536 * int(r) + 256 * int(g) + int(b)'
!		write(nf,'(a)')'plot "ex6data1_2.txt" using 1:2:(rgb($3*250,50,0)) with points pt 7 ps 1 lc rgb variable'
		write(nf,'(a)')'symbol(z) = "o+"[int(z):int(z)]'
		write(nf,'(a)')'plot "ex6data1_2.txt" using 1:2:(symbol($3+1)) with labels textcolor lt 1, \'	
		write(nf,'(a)')'   -(1.40 * x - 10.34)/2.13 '
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist cmddata1.txt')
	
	
	x11 = (/1, 2,  1/)
	x12 = (/0, 4, -1/)
	sigma = 2.	
	sim = gaussianKernel(M1, x11, x12, sigma)	
	write(*,*)'Gaussian Kernel between x1 = [1; 2; 1], x2 = [0; 4; -1], sigma = 2'
	write(*,'(f10.6)') sim
    write(*,*)'(this value should be about 0.324652)' 
1000 continue	


	open(fnum, file='ex6data2.txt',status='old')
	do i=1, M2
		read(fnum,*)X2(i,1), X2(i,2), y2(i)
	enddo
	close(fnum)
	
	open(nf,file='cmddata2.txt', status='replace')
! 		write(nf,'(a)')'rgb(r,g,b) = 65536 * int(r) + 256 * int(g) + int(b)'
		write(nf,'(a)')'symbol(z) = "o+"[int(z):int(z)]'		
		write(nf,'(a)')'plot"ex6data2.txt" using 1:2:(symbol($3+1)) with labels textcolor lt 1'		
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist cmddata2.txt')
	

	C = 1.
	sigma = 0.1
	tol = 1.e-3
	max_passes = 5
	kernelFunction = 'gaussianKernel'
	call svmTrain(M2, N, X2, y2, C, kernelFunction, tol, max_passes, model, idx)
	write(*,*) model%w, model%b
	
	call visualizeBoundary(M2, N, X2, y2, model, idx, kernelFunction)

	deallocate(model%X)
	deallocate(model%y)
	deallocate(model%alphas)
	deallocate(model%w)

	
	end

	
	
