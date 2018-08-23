	subroutine  computeNumericalGradient(M,N,M1,N1,M2,N2,nn_params, X, y, lambda, numgrad)
	implicit none
	
	integer::M,N,M1,N1,M2,N2
	double precision, dimension(M,N):: X
	double precision, dimension(M):: y
	double precision, dimension(M1*N1+M2*N2):: numgrad, nn_params, grad
	double precision:: lambda, e = 1e-4, loss1, loss2
		
	double precision, dimension(M1, N1):: grad1, theta1
	double precision, dimension(M2, N2):: grad2, theta2
	integer:: i, j, k, nn, p
	integer:: it, jt


	do p=1,M1*N1 + M2*N2
		nn_params(p) = nn_params(p) - e
		call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, loss1, grad)
		nn_params(p) = nn_params(p) + 2.* e
		call nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, loss2, grad)
		nn_params(p) = nn_params(p) - e

		numgrad(p) = (loss2 - loss1) / (2.* e)
	enddo

end subroutine computeNumericalGradient
