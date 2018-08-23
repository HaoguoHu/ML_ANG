//// Machine Learning Online Class - Exercise 4 Neural Network Learning
//  Instructions
//  This file contains code that helps you get started on the
//  linear exercise. You will need to complete the following functions 
//  in this exericse:
//
//     sigmoidGradient.c
//     randInitializeWeights.c
//     nnCostFunction.c
//
//  For this exercise, you will not need to change any code in this file,
//  or any other files other than those mentioned above.
//
// Initialization
//clear ; close all; clc
// Setup the parameters you will use for this exercise

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int fmincg(double (*nnCostFunction)(int M,int N,int M1,int N1,int M2,int N2,double nn_params[M1*N1+M2*N2],
double X[M][N],double y[M],double lambda, double grad[M1*N1+M2*N2]),int M,int N,int M1,int N1,int M2,int N2,
double nn_params[M1*N1+M2*N2],double X[M][N],double y[M],double lambda, int maxCostCalls);

double nnCostFunction(int M,int N,int M1,int N1,int M2,int N2,double theta[M1*N1+M2*N2],
	double X[M][N], double y[M], double lambda, double grad[M1*N1+M2*N2]);

double sigmoidGradient(double z);
int randInitializeWeights(int L_in, int L_out, double W[L_out][ L_in + 1]);
void checkNNGradients();

int predict(int M, int N, int M1, int N1, int M2, int N2, double X[M][N], double theta1[M1][N1],
	double theta2[M2][N2], double g[M]);
	
int  unroll(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2]);
int reshape(int M1,int N1,int M2,int N2,double theta1[M1][N1],double theta2[M2][N2],double nn_params[M1*N1+M2*N2]);

int main()
{

	int M = 5000, N = 400, M1 = 25, N1 = 401, M2 = 10, N2 = 26;
	int input_layer_size  = N;  // 20x20 Input Images of Digits
	int hidden_layer_size = M1;   // 25 hidden units
	int num_labels = M2;          // 10 labels, from 1 to 10   // (note that we have mapped "0" to label 10)		

	FILE *fp;
	char line[6017];  //401*15 + 2 /// 2='\n'
	char line2[392];  //26*15 + 2 /// 2='\n'
	int i, j, k, ri;	
	static double X[5000][400] ;  //array is large then have to use "static"
	static double y[5000] ;
	double sel[100][N];
	int index[100];
	double lambda;	
	double theta1[25][401], theta2[10][26];
	double Jcost;

	double  gs[5];

	printf("Loading and Visualizing Data ...\n");

	if((fp = fopen ("ex4data1.txt", "r")) == NULL){
		printf("open wrong \n");
	}else{
	       i = 0;
	       while(fgets(line, sizeof line, fp) != NULL){
//		   printf("line[%d]=  %s \n", i, line); 		   
		   X[i][0] = (double)atof(strtok (line, " "));
		   	
		   for(j=1; j < N; j++)	   	
    	         X[i][j] = (double)atof(strtok (NULL, " "));
		  
		   y[i] = (double)atof(strtok (NULL, " "));			
		   
//		   printf("%15.10f, %15.10f, %.0f\n", X[i][0], X[i][1],  y[i]);
//               getchar();

		   i++;	   
	       }
   	   fclose(fp);
	}
	 

// Randomly select 100 data points to display
/*
index[0] = 0;
for(i=0; i < 100; i++){
     ri = rand()%M;     
     for(j=0; j < i; j++){
     	  if(index[j] != ri){
	  	 index[i] = ri;
		 
	  	for(k=0; k < N; k++)
     	         sel[i][k] = X[ri][k];	     
	  }
     }
}
*/
//displayData(sel);
//displayData(X(sel, :));

	printf("\nLoading Saved Neural Network Parameters ...\n");
	if((fp = fopen ("ex4weights.txt", "r")) == NULL){
		printf("open wrong 2\n");
	}else{

		 for(i=0; i < M1; i++){
	           fgets(line, sizeof line, fp);
//		     printf("line[%d]=  %s \n", i, line); 
//			getchar();		   
		     theta1[i][0] = (double)atof(strtok (line, " "));	   	
		     for(j=1; j < N1; j++)	   	
    	                theta1[i][j] = (double)atof(strtok (NULL, " "));		  				   
		 }
			
		 for(i=0; i < M2; i++){	 
		      fgets(line2, sizeof line2, fp);
//			 printf("line2[%d]=  %s \n", i, line2); 
//			 getchar();
			 theta2[i][0] = (double)atof(strtok (line2, " "));	 
		      for(j=1; j < N2; j++)	   	
    	                theta2[i][j] = (double)atof(strtok (NULL, " "));		  				   
		 }		    	   
	
        fclose(fp);
	}

	double nn_params[M1*N1+M2*N2], grad[M1*N1+M2*N2],initial_nn_params[M1*N1+M2*N2];
	unroll(M1,N1,M2,N2,theta1, theta2, nn_params);
	
	printf("\nFeedforward Using Neural Network ...\n");
	lambda = 0;

	Jcost = nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, grad);

	printf("Cost at parameters (loaded from ex4weights) %lf \n", Jcost);
	printf("(this value should be about 0.287629)\n");


	printf("\nChecking Cost Function (w/ Regularization) ... \n");
	lambda = 1. ;
	Jcost = nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, grad);

	printf("Cost at parameters (loaded from ex4weights) %lf \n", Jcost);
	printf("(this value should be about 0.383770)\n");
	

	printf("\nEvaluating sigmoid gradient...\n");
	double z[5] = {1, -0.5, 0, 0.5, 1};
	printf("Sigmoid gradient evaluated at [1 -0.5 0 0.5 1]:\n  ");
	
	
	for(i=0; i< 5; i++)
    	      printf("%f ", sigmoidGradient(z[i]));
	printf("\n\n");

	printf("\nChecking Backpropagation... \n");
	lambda =0;
//	checkNNGradients(lambda);

	printf("\nChecking Backpropagation (w/ Regularization) ... \n");
	lambda = 3;
//	checkNNGradients(lambda);

	double debug_J;
    debug_J = nnCostFunction(M,N,M1,N1,M2,N2, nn_params, X, y, lambda, grad);
   	printf("\nCost at (fixed) debugging parameters (w/lambda = 3): %lf  \n ", debug_J);
    printf("(this value should be about 0.576051) \n");


	printf("\nTraining Neural Network... \n");
	
	printf("\nInitializing Neural Network Parameters ...\n");
	double initial_theta1[M1][N1];
	double initial_theta2[M2][N2];

	randInitializeWeights(N1, M1, initial_theta1);
	randInitializeWeights(N2, M2, initial_theta2);
	
	unroll(M1,N1,M2,N2,initial_theta1, initial_theta2, nn_params);
	
	lambda = 1.;
	fmincg(nnCostFunction,M,N,M1,N1,M2,N2, nn_params,X, y,lambda, 50);
	reshape(M1,N1,M2,N2,theta1, theta2, nn_params);
	

//	printf("\nVisualizing Neural Network... \n");

//displayData(theta1(:, 2:end));


//// ================= Part 10: Implement Predict =================



	int ta =0;
	double g[M];
	
    predict(M, N, M1, N1, M2, N2, X, theta1, theta2, g);

	for(i=0; i < M; i++){
//		   printf("%d  %lf  %lf\n", i, g[i], y[i]);
	   if(fabs(y[i] - g[i]) <= 0.1)
	   	ta++;
	}
	printf("Train Accuracy: %d   %d  %f%\n", ta, M,  (double)ta/(double)M * 100);



  exit(0);
}
