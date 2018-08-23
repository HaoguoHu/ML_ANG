subroutine featureNormalize(M, N, X, mu, sigma)

	integer:: i, j, M, N
	double precision:: X(M,N), mu(N), sigma(N)
	
	call meanN(M, N, X, mu)

	call bsxfunM(M, N, X, mu)
	
			
	call std(M, N, X, sigma)	

	call bsxfunRD(M, N, X, sigma)
	

end subroutine  featureNormalize
