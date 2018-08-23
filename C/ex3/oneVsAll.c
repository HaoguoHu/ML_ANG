#include<stdio.h>
#include<math.h>
#include<float.h>

#define COST_FUNC_DATATYPE double
#define COST_FUNC_DATATYPE_MIN (FLT_MIN*100)

double lrCostFunction(int M, int N, double ini_theta[N], double X[M][N], double yk[M], double grad[N], double lambda);
int fmincg(double (*lrCostFunction)(int M, int N, double theta[N], double X[M][N], double y[M], double grad[N],
double lambda), int M, int N, double X[M][N], double yk[M], double theta[N], double lambda, int maxCostCalls);

int oneVsAll(int M, int N, double X[M][N], double y[M], int num_labels, double lambda, double all_theta[num_labels][N])
{ 
	int k, j, i, kk;
	double theta[N], yk[M];
	
	for(k=0; k < num_labels; k++){
 
	    kk = k;
	    
        if(kk == 0){
		    kk = 10;
		}
		 
        for(i=0; i < M; i++) {   	    
	    	if( y[i] ==  (double)kk){
	            yk[i]=1;
			}else{
	 	      	yk[i]=0;
			}
	   	} 
	   
	   
		for(j=0; j < N; j++)
 	     	theta[j] = 0;	
	     
		
		fmincg(lrCostFunction, M, N, X, yk, theta, lambda, 50);  
		for(j=0; j < N; j++){
	    	all_theta[k][j] = theta[j];	   
      	}  
	
	
		printf(" num_labels =  %d \n", k);
	
	} 

 	return 0;
}
