int batchGradientDescentMulti(int M, int N, double alpha, int iterations, double X[M][2], double y[M], double theta[N])
{
	
	int i,j,  iter;
	double thetaX;
	double temp[N];
	double XX[M][N];
	
		for(i=0; i < M; i++)
		        XX[i][0] = 1;
		    
		for( j = 1; j < N; j++){
		   for(i=0; i < M; i++)
		        XX[i][j] = X[i][j-1];
		}
	
	 for( iter = 0; iter < iterations ; iter++){	  
	        temp[0]  = 0 ;
		  temp[1]  = 0 ;
		  temp[2]  = 0 ;
		  
    	    for( i = 0; i< M ; i++){
	        thetaX = 0 ;     		 
	        for( j = 0; j < N; j++)
		     thetaX = thetaX + theta[j]*XX[i][j];
		     
		  for( j = 0; j < N; j++)
        	  temp[j] = temp[j] + (thetaX - y[i]) * XX[i][j];	
    	     }
	     
	        for( j = 0; j < N; j++)      
    	          theta[j] = theta[j] - (alpha/M) * temp[j];	   
	}
	
	return 0;
	
}
