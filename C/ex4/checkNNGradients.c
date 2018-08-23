#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double nnCostFunction(int M,int N,int M1,int N1,int M2,int N2,double nn_params[M1*N1+M2*N2],
	                   double X[M][N], double y[M], double lambda, double grad[M1*N1+M2*N2]);
					   
void computeNumericalGradient(int M,int N,int M1,int N1,int M2,int N2,double nn_params[M1*N1+M2*N2],
	double X[M][N], double y[M], double lambda, double numgrad[M1*N1+M2*N2]);

int debugInitializeWeights(int fan_out, int fan_in, double W[fan_out][fan_in+1]);

int  unroll(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2]);
int reshape(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2]);

void checkNNGradients()
{
    double lambda;

	int input_layer_size = 3;
	int hidden_layer_size = 5;
	int num_labels = 3;
	int M = 5;
	double X[M][input_layer_size];
	double y[M];
	
	int N =input_layer_size;
	int N1=input_layer_size+1;
	int M1=hidden_layer_size;
	int M2=num_labels;
	int N2=hidden_layer_size+1;
	
	int i, j;

	double diff, tmp ;
	int MN = M1*N1 + M2*N2;
	double nn_params[MN],grad[MN], numgrad[MN]; 
	double theta1[M1][N1], theta2[M2][N2];
	   
	debugInitializeWeights(M1, N1, theta1); 
	debugInitializeWeights(M2, N2, theta2);     
	debugInitializeWeights(M,  N, X);

    
	for(i=0; i<M; i++){
		y[i] = 1. + (i+1)%num_labels;
	}
    
	 
	unroll(M1,N1,M2,N2,theta1, theta2, nn_params);
	
	lambda =0;	
               nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, grad);	
	 computeNumericalGradient(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, numgrad);

	diff = -1;	   
	for(i=0; i< MN; i++){
			printf("%d   %e   %e\n", i, numgrad[i], grad[i]);
			tmp = (numgrad[i]-grad[i])/(numgrad[i]+grad[i]);
			if(tmp > diff)
				diff = tmp;
	}	
	
	printf("The above two columns you get should be very similar.\n" );
    printf("(Left-Your Numerical Gradient, Right-Analytical Gradient)\n\n");
	getchar();

	printf("If your backpropagation implementation is correct, then \n");
    printf("the relative difference will be small (less than 1e-9). \n\n");
    printf("Relative Difference: %e \n", diff);
}
