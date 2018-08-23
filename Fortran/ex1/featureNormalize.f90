	subroutine featureNormalize(M, X, X_norm, mu, sigma)
	implicit none
	
	integer:: i, j, M
	real,dimension(2, M)::X, X_norm 
	real,dimension(2):: mu, sigma
		
	
	do j = 1, 2
		mu(j) = sum(X(j,:))/M
		sigma(j) = sqrt((sum(X(j,:)**2)-sum(X(j,:))**2/real(M))/real(M-1))
		X_norm(j,:) = (X(j,:) - mu(j)) / sigma(j)
	enddo
	
	end subroutine featureNormalize
