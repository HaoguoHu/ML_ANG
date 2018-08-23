#include <stdio.h>
#include <stdio.h>
#include <math.h>

double sigmoid(double z);
double sigmoidGradient(double z);
int  unroll(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2]);
int reshape(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2]);

double nnCostFunction(int M,int N,int M1,int N1,int M2,int N2,double nn_params[M1*N1+M2*N2], 
	double X[M][N],double y[M],double lambda, double grad[M1*N1+M2*N2])
{

	double TX, Jcost, atheta1, atheta2;
	int i, j, k;  
	static double  X1[5000][401];
	static double  sg[5000][26];
	static double sig[5000][10], yy[5000][10];
	double  grad1[M1][N1], grad2[M2][N2], theta1[M1][N1], theta2[M2][N2];
	  
  
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
			  
			sg[i][k+1] = sigmoid(TX);
		}		 		
			sg[i][0] = 1;
	}
	
	  
	for(i=0; i < M; i++){	      
		for(k=0; k <M2; k++){
				TX = 0;
			for(j=0; j < N2; j++)
				TX += sg[i][j] * theta2[k][j];
		     
 			sig[i][k] = sigmoid(TX);
		}		 		
	}
	
			atheta1 = 0 ;	
	for(j=0; j< M1; j++){
		for(k=1; k< N1; k++)
			atheta1 += theta1[j][k] * theta1[j][k];
	}
	 
			atheta2 = 0; 
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
	
	

	double a1[N1];
	double a2[N2],  z2[N2], tx[N2], d2[N2];
	double a3[M2], yt[M2], d3[M2];
	double z2t[M1], d2t[M1];
	double delta1[M1][N1], delta2[M2][N2];
	int t;

	
	for(i=0; i<M1; i++){
	     for(j=0; j<N1; j++)
		 	delta1[i][j] = 0;
	}
	for(i=0; i<M2; i++){
	     for(j=0; j<N2; j++)
		 	delta2[i][j] = 0;
	}
	
	  	
	for(t=0; t<M; t++){
		for(j=0; j<N1; j++){
		     a1[j] = X1[t][j];
		}
	
		for(j=0; j<N2; j++){
		     a2[j] = sg[t][j];
		}
		
		for(j=0; j<M2; j++){
		       a3[j] = sig[t][j];
			   yt[j] =  yy[t][j];
			   d3[j] =  a3[j] - yt[j];
		}
		

		for(j=0; j<M1; j++){
				z2[j] =0;
			for(k=0; k<N1; k++)
				z2[j] += theta1[j][k] *a1[k];
		}
		
			z2t[0] = 1;
		for(j=1; j<N2; j++)
			z2t[j] = z2[j-1];
	
	
		
		for(j=0; j<N2; j++){
				tx[j] = 0;			
			for(k=0; k<M2; k++){
			   	tx[j] += d3[k] * theta2[k][j];
			}
			d2[j] = tx[j] * sigmoidGradient(z2t[j]);
		}
		
		
		for(j=0; j<M1; j++){	
			d2t[j] = d2[j+1];
		}
		
		for(i=0; i<M1; i++){
	     	for(j=0; j<N1; j++)
		        delta1[i][j] +=  d2t[i]* a1[j];
		}
	
	
		for(i=0; i<M2; i++){
	     	for(j=0; j<N2; j++)
		     	delta2[i][j] +=  d3[i]* a2[j];
		}		
	}
	
			
	for(i=0; i<M1; i++){
			grad1[i][0] = delta1[i][0]/M ;
		for(j=1; j<N1; j++)
	    	grad1[i][j] = delta1[i][j]/M + lambda/M* theta1[i][j];
	}
	
	for(i=0; i<M2; i++){
			grad2[i][0] = delta2[i][0]/M ;
	    for(j=1; j<N2; j++)
	    	grad2[i][j] = delta2[i][j]/M + lambda/M* theta2[i][j];
	}
	 
	 
	unroll(M1,N1,M2,N2,grad1, grad2, grad);
	
	
    return Jcost ;
}
