/*
Copyright (C) 2001 and 2002 by Carl Edward Rasmussen. Date 2002-02-13
%
%
% (C) Copyright 1999, 2000 & 2001, Carl Edward Rasmussen
% 
% Permission is granted for anyone to copy, use, or modify these
% programs and accompanying documents for purposes of research or
% education, provided this copyright notice is retained, and note is
% made of any changes that have been made.
% 
% These programs and documents are distributed without any warranty,
% express or implied.  As the programs were written for research
% purposes only, they have not been tested to the degree that would be
% advisable in any important application.  All use of these programs is
% entirely at the user's own risk.

[ Sagar GV, 2013, sagar.writeme@gmail.com ] Changes Made:
- Ported to C
*/
//#include "fmincg.h"

#include<math.h>
#include<stdio.h>
#include<float.h>

//#define COST_FUNC_DATATYPE double
//#define COST_FUNC_DATATYPE_MIN (FLT_MIN*100)

#define RHO 0.01f
#define SIG 0.5f
#define INT 0.1f
#define EXT 3.0f
#define MAX 20
#define RATIO 100.0f

int fmincg(double (*lrCostFunction)(int M, int N, double theta[N], double X[M][N], double y[M], double grad[N], double lambda), int M, int N, double X[M][N], double y[M], double theta[N], double lambda, int maxCostCalls)
{
	int i,success = 0,costFuncCount=0,lineSearchFuncCount=0;
	double ls_failed,Cost1,d1,z1,f0,Cost2,d2,f3,d3,z3,limit,z2,A,B,C;
	double grad1[N],s[N],x0[N],df0[N],grad2[N],tmp[N];
	
	int j;

	ls_failed = 0;

	if(costFuncCount >= maxCostCalls) return 1; else costFuncCount++;
	Cost1 = lrCostFunction(M, N, theta, X, y, grad1, lambda);
	

	for(i=0;i<N;i++)
		s[i] = -grad1[i];
	
	d1 = 0;
	for(i=0;i<N;i++)
		d1 += -s[i]*s[i];

	z1 = 1.0f / (1 - d1);
	
	while(1)
	{
		for(i=0;i<N;i++){
			x0[i] = theta[i];
			df0[i] = grad1[i];
		}
		f0 = Cost1;
		
		for(i=0;i<N;i++){
			theta[i] = theta[i] + (z1)*s[i];
		}
		if(costFuncCount >= maxCostCalls) return 1; else costFuncCount++;
		Cost2 = lrCostFunction(M, N, theta, X, y, grad2, lambda);


		d2 = 0;
		for(i=0;i<N;i++)
			d2 += grad2[i]*s[i];
					
		f3 = Cost1;
		d3 = d1;
		z3 = -z1;
		
		success = 0; 
		limit = -1;
		lineSearchFuncCount = 0;
		// begin line search
		while(1){
			while((( (Cost2) > ((Cost1) + RHO*(z1)*(d1))) || ( (d2) > -SIG*(d1) )) && lineSearchFuncCount < MAX){
				limit = z1;
				if( (Cost2) > (Cost1) ){
					z2 = z3 - (0.5f*(d3)*(z3)*(z3))/((d3)*(z3)+(Cost2)-(f3));
				}
				else{
					A = 6*((Cost2)-(f3))/(z3)+3*((d2)+(d3));
					B = 3*((f3)-(Cost2))-(z3)*((d3)+2*(d2));
					z2 = (sqrt(B*B-A*(d2)*(z3)*(z3))-B)/A;
				}
				if(isnan(z2) || isinf(z2)){
					z2 = (z3) * 0.5f;
				}
				
				A = ((z2 < INT*(z3)) ? z2 : INT*(z3));
				B = (1-INT)*(z3);
				z2 = A > B ? A : B;
				z1 = z1 + z2;
				
				for(i=0;i<N;i++)
					theta[i] += (z2)*s[i];

				if(costFuncCount >= maxCostCalls) return 1; else costFuncCount++;
				lineSearchFuncCount++;
				Cost2 = lrCostFunction(M, N, theta, X, y, grad2, lambda);

				d2 = 0;
				for(i=0;i<N;i++)
					d2 += grad2[i] * s[i];
				
				z3 = z3 - z2;
			}

			if( (Cost2 > Cost1 + (z1)*RHO*(d1)) || ((d2) > -SIG*(d1)) ){
				break; //failure
			}else if( d2 > SIG*(d1) ){
				success = 1; break; 
			}else if(lineSearchFuncCount >= MAX){
				break;
			}
			
			A = 6*(Cost2-f3)/z3+3*(d2+d3);
			B = 3*(f3-Cost2)-z3*(d3+2*d2);
			z2 = -d2*z3*z3/(B+sqrt(B*B-A*d2*z3*z3));
			if(!(B*B-A*d2*z3*z3 >= 0) || isnan(z2) || isinf(z2) || z2 < 0){
				if(limit < -0.5f){
					z2 = z1 * (EXT-1);
				}else{
					z2 = (limit-z1)/2;
				}
			}else if((limit > -0.5) && (z2+z1 > limit)){
				z2 = (limit-z1)/2; 
			}else if((limit < -0.5) && (z2+z1 > z1*EXT)){
				z2 = z1*(EXT-1.0);
			}else if(z2 < -z3*INT){
				z2 = -z3*INT;
			}else if((limit > -0.5) && (z2 < (limit-z1)*(1.0-INT))){
				z2 = (limit-z1)*(1.0-INT);
			}
			
			f3 = Cost2; d3 = d2; z3 = -z2;
			z1 = z1 + z2;
			for(i=0;i<N;i++)
				theta[i] += z2*s[i];
			
			if(costFuncCount >= maxCostCalls) return 1; else costFuncCount++;
			lineSearchFuncCount++;
			Cost2 = lrCostFunction(M, N, theta, X, y, grad2, lambda);
			d2 = 0;
			
			for(i=0;i<N;i++)
				d2 += grad2[i]*s[i];
			
		}
		// line search ended
		if(success){
			Cost1 = Cost2;
//			printf("Cost: %e\n", Cost1);
			
			A = 0;
			B = 0;
			C = 0;
			for(i=0;i<N;i++){
				A += grad1[i]*grad1[i];
				B += grad2[i]*grad2[i];
				C += grad1[i]*grad2[i];
			}
			for(i=0;i<N;i++){
				s[i] = ((B-C)/A)*s[i] - grad2[i];
			}
			for(i=0;i<N;i++){
				tmp[i] = grad1[i]; grad1[i] = grad2[i]; grad2[i] = tmp[i];
			}
			d2 = 0;
			for(i=0;i<N;i++)
				d2 += grad1[i] * s[i];
			
			if(d2 > 0){
				for(i=0;i<N;i++)
					s[i] = -grad1[i];
				
				d2 = 0;
				for(i=0;i<N;i++)
					d2 += -s[i]*s[i];				
			}
			
//			A = d1/(d2-COST_FUNC_DATATYPE_MIN);
			A = d1/(d2- 0.);
			z1 = z1 * ((RATIO < A) ? RATIO : A);
			d1 = d2;
			ls_failed = 0;
		}else{
			Cost1 = f0;
			for(i=0;i<N;i++){
				theta[i] = x0[i];
				grad1[i] = df0[i];
			}
			
			if(ls_failed){
				break;
			}
			
			for(i=0;i<N;i++)
				tmp[i] = grad1[i]; grad1[i] = grad2[i]; grad2[i] = tmp[i];
			
			for(i=0;i<N;i++)
				s[i] = -grad1[i]; 
			
			d1 = 0;
			for(i=0;i<N;i++)
				d1 += -s[i]*s[i];
			
			z1 = 1/(1-d1);
			ls_failed = 1;
		}
	}
	return 2;
}
