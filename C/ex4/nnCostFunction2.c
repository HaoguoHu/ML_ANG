#include <stdio.h>
#include <stdio.h>
#include <math.h>

double sigmoid(double z);
double sigmoidGradient(double z);
int  unroll(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2]);
int reshape(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2]);

double nnCostFunction2(int M,int N,int M1,int N1,int M2,int N2,double nn_params[M1*N1+M2*N2], 
	double X[M][N],double y[M],double lambda, double grad[M1*N1+M2*N2])
{

      double TX, Jcost, atheta1, atheta2;
      int i, j, k;  
	  double  X1[M][N1];
	  double  sg[M][N2];
	  double sig[M][M2], yy[M][M2];
	  double z2[M][N2], theta1_grad[M1][N1], theta2_grad[M2][N2], theta1[M1][N1], theta2[M2][N2];
	  
  
	//int M = 5000, N = 400, M1 = 25, N1 = 401, M2 = 10, N2 = 26;
	reshape(M1,N1,M2,N2,theta1, theta2, nn_params);
	  
	  for(i=0; i <M; i++){
	       X1[i][0] = 1;
		 for(j=1; j < N1; j++)
		    X1[i][j] = X[i][j-1];
	   }
	   	
			 	  
	  for(i=0; i<M; i++){
	  	for(k=0; k< M2; k++){
		        yy[i][k] = 0;
	  	    if(y[i] == (double)k+1){   //from 1,2..9,10 while not 10, 1,2,...9//
			  yy[i][k] = 1;
		    }
		}
	  }	
	      	  
	  
        for(i=0; i < M; i++){	      
	       for(k=0; k <M1; k++){
	           TX = 0;
                 for(j=0; j < N1; j++)
                    TX += X1[i][j] * theta1[k][j];
			  
		         z2[i][k+1] = TX;
                 sg[i][k+1] = sigmoid(TX);
		 }		 		
	             sg[i][0] = 1;
		         z2[i][0] = 1;
        }
	
	  
	  for(i=0; i < M; i++){	      
	       for(k=0; k <M2; k++){
	           TX = 0;
                 for(j=0; j < N2; j++)
                    TX += sg[i][j] * theta2[k][j];
		     
                 sig[i][k] = sigmoid(TX);
		 }		 		
        }
		
	   for(j=0; j< M1; j++){
	  	for(k=1; k< N1; k++)
			atheta1 += theta1[j][k] * theta1[j][k];
	  }
	  
	  for(j=0; j< M2; j++){
	  	for(k=1; k< N2; k++)
			atheta2 += theta2[j][k] * theta2[j][k];
	  }
	  
	  Jcost = 0;	
	  for(i=0; i < M; i++){	        		  
	       for(k=0; k < M2; k++){
                  Jcost += -yy[i][k] * log(sig[i][k]) - ( 1.-yy[i][k]) * log(1.-sig[i][k]);
	       }		     
        }	  
	  Jcost = Jcost/M + lambda/2/M * (atheta1 + atheta2);
	  	  
	double a1[M][N1];
	double a2[M][N2], d2[M][N2];
	double a3[M][M2], yt[M][M2], d3[M][M2];
	
	for(i=0; i<M; i++){
	   for(j=0; j< N1; j++){
		a1[i][j] = X1[i][j];
	   }
	}

	for(i=0; i<M; i++){
	   for(j=0; j< N2; j++){
		a2[i][j] = sg[i][j];
	   }
	}
	
	for(i=0; i<M; i++){
	   for(j=0; j< M2; j++){
		a3[i][j] = sig[i][j];
		yt[i][j]  = yy[i][j];
		d3[i][j] =  a3[i][j] - yt[i][j] ;
	   }
	}
	
	for(i=0; i < M; i++){	      
	       for(j=0; j <N2; j++){
	             TX = 0;
                 for(k=0; k < M2; k++){
                    TX += d3[i][k] * theta2[k][j];
			     }
			  
	            d2[i][j] = TX * sigmoidGradient(z2[i][j]);
         }
	}

	
	double delta1[M1][N1], delta2[M2][N2];
	
	
	for(i=0; i<M1; i++){
	     for(j=0; j<N1; j++){
	        delta1[i][j] = 0;
		    for(k=0; k<M; k++)
		        delta1[i][j] +=  d2[k][i+1]* a1[k][j];
	     }
	}
	
	
	for(i=0; i<M2; i++){
	     for(j=0; j<N2; j++){
	        delta2[i][j] = 0;
		    for(k=0; k<M; k++)
		     	delta2[i][j] +=  d3[k][i]* a2[k][j];
	     }
	}
	
	for(i=0; i<M1; i++){
	     for(j=0; j<N1; j++)
	        theta1_grad[i][j] = delta1[i][j]/M + lambda/M* atheta1;
	}
	
	for(i=0; i<M2; i++){
	     for(j=0; j<N2; j++)
	        theta2_grad[i][j] = delta2[i][j]/M + lambda/M* atheta2;
	}
	 
	 
	unroll(M1,N1,M2,N2,theta1_grad, theta2_grad, grad);
	
	
    return Jcost ;
}
