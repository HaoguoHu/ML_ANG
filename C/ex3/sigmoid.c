#include <math.h>

double sigmoid(double z)
{
    double sigmoid;
    
    sigmoid = 1. / (1 + exp(-1 * z));
    
    return (sigmoid);
}
