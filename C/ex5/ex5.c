#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

double linearRegCostFunction(int M,int N,double X[M][N], double y[M], double theta[N], double lambda, double grad[N]);
int trainLinearReg(int M, int N, double X[M][N],double y[N], double lambda, double theta[N]);
int fmincg(double (*linearRegCostFunction)(int M,int N,double X[M][N], double y[M],double theta[N], double lambda, double grad[N]),
	int M,int N, double nn_params[N],double X[M][N],double y[M],double lambda, int maxCostCalls);
int learningCurve(int M,int N,int MV, double X[M][N], double y[M],
	double Xval[MV][N], double yval[MV], double lambda, double error_train[M], double error_val[M]);
	
int polyFeatures(int M, double X[M], int N, double Xp[M][N]);
int featureNormalize(int M, int N, double X[M][N], double mu[N], double sigma[N]);

//void std(int M, int N, double X[M][N], double std[N]);
void bsxfunM(int M, int N, double X[M][N], double y[M]);
void bsxfunRD(int M, int N, double X[M][N], double y[M]);
void ones(int M, int N, double X[M][N], double X1[M][N+1]);
//void eql(int M, int N, double X1[M][N], double X2[M][N]);

	
int main()
{
	int M=12, N=2, MV=21;
	double X[M], y[M];
	FILE *fp;
	char line[15*M+2];
	int i, j;
	double Jcost, lambda;
	double theta[N], grad[N], ini_theta[N];
	double X1[M][N];
	double Xval[MV], yval[MV], Xtest[MV];
	double Xval1[MV][N];
	double error_train[M], error_val[M];
	
	printf("Loading and Visualizing Data ...\n");

	if((fp = fopen ("ex5data1.txt", "r")) == NULL){
		printf("open wrong \n");
	}else{
	       i = 0;
	       while(fgets(line, sizeof line, fp) != NULL){
//		   printf("line[%d]=  %s \n", i, line); 
			if(i < M){		   
		   		X[i] = (double)atof(strtok (line, " "));		    	
		   		y[i] = (double)atof(strtok (NULL, " "));
			}else{
				Xval[i-M] = (double)atof(strtok (line, " "));		    	
		   		yval[i-M] = (double)atof(strtok (NULL, " "));
			   Xtest[i-M] = (double)atof(strtok (NULL, " "));
			}					   
//		   printf("%15.10f, %15.10f \n", X[i],  y[i]);
//           getchar();
		   i++;	   
	       }
   	   fclose(fp);
	}
	
	theta[0] = 1.; 
	theta[1] = 1.;
	for(i=0; i<M; i++){
		X1[i][0] = 1.;
		X1[i][1] = X[i];
	}
	lambda = 1.;
	Jcost = linearRegCostFunction(M, N, X1, y, theta, lambda, grad);
	printf("Cost at theta = [1 ; 1]: %lf \n", Jcost);
    printf("(this value should be about 303.993192)\n");
//	getchar();
	
	
	printf("Gradient at theta = [1 ; 1]:  %f   %f \n", grad[0], grad[1]);
    printf("this value should be about [-15.303016; 598.250744])\n");
//	getchar();
	 	
	lambda = 0.;
	trainLinearReg(M, N, X1, y,lambda, theta);
	
	printf("theta=  %f   %f \n", theta[0], theta[1]);
	printf("(theta should be about  13.087904 ,0.367779 ) \n");	
//	getchar();
	
	lambda = 0;
	for(i=0; i<MV; i++){ Xval1[i][0] = 1.;  Xval1[i][1] = Xval[i];}
	learningCurve(M, N, MV, X1, y, Xval1, yval, lambda, error_train, error_val);
	
	printf("# Training Examples\tTrain Error\tCross Validation Error\n");
	
	for(i=0;i<M;i++){
		printf("%d \t %f \t %f  \n", i, error_train[i], error_val[i]);
	}
	
//   getchar();
   
   
	int p =8;
	double X_poly [M][p];
	double X_poly1[M][p+1]; 
	double mu[p], sigma[p];	
// Map X onto Polynomial Features and Normalize	

	polyFeatures(M, X, p, X_poly);				
	featureNormalize(M, p, X_poly, mu, sigma);		 
	ones(M, p, X_poly, X_poly1);

	for(j=0; j<p+1; j++)printf(" %.6f   \n", X_poly1[0][j]);		
	printf("Program paused. Press enter to continue.\n");
	getchar();
	
	
// Map X_poly_test and normalize (using mu and sigma)
	double  X_poly_test [MV][p];	
	double  X_poly_test1[MV][p+1]; 		
//	polyFeatures(MV, Xtest, p, X_poly_test);
//	bsxfunM(MV, p, X_poly_test, mu);								
//	bsxfunRD(MV, p, X_poly_test, sigma);	
//	ones(MV, p, X_poly_test, X_poly_test1);



// Map X_poly_val and normalize (using mu and sigma)
	double  X_poly_val [MV][p];	
	double  X_poly_val1[MV][p+1]; 		
	polyFeatures(MV, Xval, p, X_poly_val);	
	 bsxfunM(MV, p, X_poly_val, mu);								
	bsxfunRD(MV, p, X_poly_val, sigma);
	ones(MV, p, X_poly_val, X_poly_val1);


	lambda = 0;
//	trainLinearReg(M, p+1, X_poly1, y, lambda, theta);	
	learningCurve(M, p+1, MV, X_poly1, y, X_poly_val1, yval, lambda, error_train, error_val);
	
	printf("Polynomial Regression (lambda = %f) \n", lambda);
	printf("# Training Examples\tTrain Error\tCross Validation Error\n");

	for(i=0;i<M;i++){
		printf("%d \t %f \t %f  \n", i, error_train[i], error_val[i]);
	}		
	printf("Program paused. Press enter to continue.\n");
	getchar();
	
	
	exit(0);
}
