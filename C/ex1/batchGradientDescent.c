int batchGradientDescent(int M, int iterations, double alpha,  double X[], double Y[], double theta[])
{
	int i, iter;
	double temp0, temp1;
	
	for( iter = 0; iter <  iterations; iter++){   
    	    temp0 = 0;
    	    temp1 = 0;
    
    	    for(i=0; i < M; i++){
        	temp0 = temp0 + (theta[0] + theta[1] * X[i] - Y[i]) * 1;
        	temp1 = temp1 + (theta[0] + theta[1] * X[i] - Y[i]) * X[i];
    	    }
    
    	    theta[0] = theta[0] - (alpha/M) * temp0;
    	    theta[1] = theta[1] - (alpha/M) * temp1;
	}
	
	return 0;
}
