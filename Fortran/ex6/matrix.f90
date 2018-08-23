	subroutine transpose(X, M, N, XT) !transpose matrix from  X(M,N) to matrix XT(N,M)
	implicit none
	integer:: i, j, M, N
	real, dimension(M, N):: X
	real, dimension(N, M):: XT
	
	do i=1,N
	do j=1,M
	   XT(i,j)=X(j,i)
	enddo
	enddo
	end subroutine transpose
	
	subroutine multiply(X, M, N, L, Y, XML) !multiply matrix  X(M,N) to matrix XT(N,M)
	implicit none
	integer:: i, j, k, M, N, L
	real, dimension(M, N):: X
	real, dimension(N, L):: Y
	real, dimension(M, L):: XML
	
	do i=1,M
	do j=1,L
	    XML(i,j)=0.
	   do k=1,N
	     XML(i,j)=XML(i,j) + X(i,k)*Y(k,j)
	   enddo
	enddo
	enddo
	end subroutine multiply
	
		
	
	subroutine LMS(X,Y,M,theta)
	implicit none
	real :: a1=0, b1=0, c1=0
	real :: a2=0, b2=0, c2=0
	integer:: i, m
	real,dimension(2) :: theta
	real,dimension(m) :: X, y
	
	theta = 0
		
		a1 = m
	do i = 1, m
		b1 = b1 + X(i)
		c1 = c1 + y(i)
		b2 = b2 + X(i)*X(i)
		c2 = c2 + X(i)*y(i)
	enddo
		a2 = b1
	
	theta(2) = (a2*c1-a1*c2)/(a2*b1-a1*b2)
	theta(1) = (b2*c1-b1*c2)/(a1*b2-a2*b1)	
		
	end subroutine LMS 
	
	
	
	subroutine inverse(a,c,n)
!============================================================
! Inverse matrix
! Method: Based on Doolittle LU factorization for Ax=b
! Alex G. December 2009
!-----------------------------------------------------------
! input ...
! a(n,n) - array of coefficients for matrix A
! n      - dimension
! output ...
! c(n,n) - inverse matrix of A
! comments ...
! the original matrix a(n,n) will be destroyed 
! during the calculation
!===========================================================
implicit none 
integer n
real,dimension(n,n):: a, c, L, U, aa
real,dimension(n):: b, d, x
real:: coeff
integer:: i, j, k

! step 0: initialization for matrices L and U and b
! Fortran 90/95 aloows such operations on matrices
L=0.0
U=0.0
b=0.0

! step 1: forward elimination
do k=1, n-1
   do i=k+1,n
      coeff=a(i,k)/a(k,k)
      L(i,k) = coeff
      do j=k+1,n
         a(i,j) = a(i,j)-coeff*a(k,j)
      end do
   end do
end do

! Step 2: prepare L and U matrices 
! L matrix is a matrix of the elimination coefficient
! + the diagonal elements are 1.0
do i=1,n
  L(i,i) = 1.0
end do
! U matrix is the upper triangular part of A
do j=1,n
  do i=1,j
    U(i,j) = a(i,j)
  end do
end do

! Step 3: compute columns of the inverse matrix C
do k=1,n
  b(k)=1.0
  d(1) = b(1)
! Step 3a: Solve Ld=b using the forward substitution
  do i=2,n
    d(i)=b(i)
    do j=1,i-1
      d(i) = d(i) - L(i,j)*d(j)
    end do
  end do
! Step 3b: Solve Ux=d using the back substitution
  x(n)=d(n)/U(n,n)
  do i = n-1,1,-1
    x(i) = d(i)
    do j=n,i+1,-1
      x(i)=x(i)-U(i,j)*x(j)
    end do
    x(i) = x(i)/u(i,i)
  end do
! Step 3c: fill the solutions x(n) into column k of C
  do i=1,n
    c(i,k) = x(i)
  end do
  b(k)=0.0
end do
end subroutine inverse

