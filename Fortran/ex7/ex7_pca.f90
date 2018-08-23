	program ex7_pca
	implicit none
	
	integer, parameter::M=50, N=2, nf=20, Mf=5000, NN=1024
	double precision, dimension(M,N)::X, X_norm, X_rec, Xt
	double precision, dimension(N,N):: U
	double precision, dimension(N)::S
	double precision, dimension(M,1)::Z	
	integer::i, j, jj, kk, ima, K
	double precision,dimension(N):: mu, sigma
	
	double precision, dimension(Mf,NN)::Xf, Xft, Xf_rec
	double precision,dimension(NN):: muf, sigmaf
	double precision, dimension(NN,NN):: Uf
	double precision, dimension(NN,36):: Uf36
	double precision, dimension(36,NN):: Uf36t
	double precision, dimension(NN)::Sf
	double precision, dimension(Mf,100)::Zf	
	
	open(nf,file='ex7data1.txt',status='old')
	do i=1,M
		read(nf,*)X(i,1), X(i,2)
	enddo
	close(nf)
		
	Xt = X  !
	call featureNormalize(M, N, Xt, mu, sigma) !Xt => X_norm, Xt being updated
	
	X_norm = Xt
!	write(*,*)'mu=', mu, 'sigma=',sigma
	
	
	call pca(M, N, X_norm, U, S)
	
	write(*,*)'Top eigenvector: '
	write(*,'(a,2f10.6)')' U(:,1) = ', U(1,1), U(2,1)
	write(*,*)'(you should expect to see -0.707107 -0.707107)'
	write(*,*)
!	write(*,*)'s1=', S(1), 's2=', S(2)
	
	open(nf, file='ex7data1_line.txt',status='unknown')
	write(nf,*)mu(1), mu(2)
	write(nf,*)mu(1)+1.5*S(1)*U(1,1), mu(2)+1.5*S(1)*U(2,1)
	write(nf,*)
	write(nf,*)mu(1), mu(2)
	write(nf,*)mu(1)+1.5*S(2)*U(1,2), mu(2)+1.5*S(2)*U(2,2)
	close(nf)
	
	
	open(nf,file='commanddata1.txt', status='replace')
		write(nf,'(a)')' set size ratio -1 '
		write(nf,'(a)')' set xr [0.5:6.5]'
      	write(nf,'(a)')' set yr [2: 8] '
		write(nf,'(a)')'plot "ex7data1.txt" using 1:2 with points pt 6 ps 1, "ex7data1_line.txt"  u 1:2 with linespoints lw 2  '
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist commanddata1.txt')
	
	call projectData(M, N, X_norm, U, 1, Z)
	
	call recoverData(M, N, Z, U, 1, X_rec)
	
	write(*,'(a,2f10.6)')'Approximation of the first example: ', X_rec(1, 1), X_rec(1, 2)
	write(*,*)'(this value should be about  -1.047419 -1.047419)'
	write(*,*)
	
	open(nf, file='ex7data1_rec.txt',status='unknown')
	do i=1, M
	write(nf,*)X_rec(i,1), X_rec(i,2)
	enddo
	close(nf)
	
	open(nf, file='ex7data1_line2.txt',status='unknown')
	do i=1, M
	write(nf,*)X_norm(i,1), X_norm(i,2)
	write(nf,*)X_rec(i,1), X_rec(i,2)
	write(nf,*)
	enddo
	close(nf)
	
	open(nf,file='commanddata1.txt', status='replace')
		write(nf,'(a)')' set size ratio -1 '
		write(nf,'(a)')' set xr [-4:3]'
      	write(nf,'(a)')' set yr [-4:3] '
		write(nf,'(a)')'plot "ex7data1_rec.txt" using 1:2 with points pt 6 ps 1, \' 
		write(nf,'(a)')'     "ex7data1_line2.txt"  u 1:2 with linespoints dashtype 3  '
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist commanddata1.txt')
	


!!----Faces data set---	
		
	open(nf,file='ex7faces.txt',status='old')
!	do i=1,Mf
		read(nf,*)((Xf(i,j),j=1,nn),i=1,Mf)
!		read(nf,*)((Xf(i,j),i=1,Mf),j=1,nn)
!	enddo
	close(nf)
	write(*,*)'read faces OK'
	
	ima = 10
	open(nf,file='ex7faces2.txt',status='unknown')
	do kk=1,ima
	do jj=1,32
		write(nf,*)((Xf(i,j),j=(jj-1)*32+1,jj*32),i=(kk-1)*ima+1,kk*ima)
!		write(nf,*)((Xf(i,j),i=(kk-1)*ima+1,kk*ima),j=(jj-1)*32+1,jj*32)
	enddo
	enddo
	close(nf)

	open(nf,file='commandfaces.txt', status='replace')
	    write(nf,'(a)')' set size ratio -1 '
		write(nf,'(a)')' set palette grey  '
      	write(nf,'(a)')' plot "ex7faces2.txt" matrix with image'
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist commandfaces.txt')
	write(*,*)'show faces OK'
	write(*,*)'Paused, Press enter to continue'
	read(*,*)

 	Xft = Xf
	call featureNormalize(Mf, NN, Xft, muf, sigmaf) !Xft => X_norm, Xt being updated	
	write(*,*)'Feature faces OK'
	call pca(Mf, NN, Xft, Uf, Sf)
	write(*,*)'PCA faces OK'
	
	ima = 6
	Uf36 = Uf(:,1:36)
	Uf36t = transpose(Uf36)
	open(nf,file='ex7faces36.txt',status='unknown')
	do kk=1,ima
	do jj=1,32
		write(nf,*)((Uf36t(i,j),j=(jj-1)*32+1,jj*32),i=(kk-1)*ima+1,kk*ima)
	enddo
	enddo
	close(nf)

	open(nf,file='commandfaces36.txt', status='replace')
	    write(nf,'(a)')' set size ratio -1 '
		write(nf,'(a)')' set palette grey  '
      	write(nf,'(a)')' plot "ex7faces36.txt" matrix with image   '
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist commandfaces36.txt')
	
   	K = 100
	call projectData(Mf, NN, Xft, Uf, K, Zf)
	write(*,*)'The projected data Z has a size of: ', shape(Zf)
	call recoverData(Mf, NN, Zf, Uf, K, Xf_rec)
	
	ima = 10
	open(nf,file='ex7facesRec.txt',status='unknown')
	do kk=1,ima
	do jj=1,32
		write(nf,*)((Xf_rec(i,j),j=(jj-1)*32+1,jj*32),i=(kk-1)*ima+1,kk*ima)
	enddo
	enddo
	close(nf)

	open(nf,file='commandfacesRec.txt', status='replace')
	    write(nf,'(a)')' set size ratio -1 '
		write(nf,'(a)')' set palette grey  '
      	write(nf,'(a)')' plot "ex7facesRec.txt" matrix with image   '
		write(nf,'(a)')'q'
	close(nf) 		
	call system('gnuplot -persist commandfacesRec.txt')
	write(*,*)'show Recovered faces OK'


	
	end
