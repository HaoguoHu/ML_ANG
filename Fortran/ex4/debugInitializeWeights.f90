	subroutine debugInitializeWeights(fan_out, fan_in, W)
	implicit none

	integer:: i, j, k;
	integer::fan_out, fan_in
	double precision, dimension(fan_out, fan_in):: W
	double precision, dimension(fan_out *(fan_in+1)):: T
	
	W = 0

! reshape W

	i = 1
	do j=1, fan_out 
	do k=1, fan_in+1
		T(i)= sin(W(j,k))/10.
		i = i+ 1
	enddo	
	enddo	
	
	i = 1
	do k=1, fan_in+1
	do j=1, fan_out 
		W(j,k) = T(i)
		i = i + 1
	enddo
	enddo		

	end subroutine debugInitializeWeights
