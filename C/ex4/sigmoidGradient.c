#include <math.h>
double sigmoid(double z);

double sigmoidGradient(double z)
{
    int i;
    double g;
    
    	  g = sigmoid(z) * (1 - sigmoid(z));
    
    return g;
}
