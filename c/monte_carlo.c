#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<limits.h>

double randRange(double min, double max) {
    double range = max - min;
    double div = RAND_MAX / range;
    return min + (rand() / div);
}



double mc_integral_simple(double minx, 
                          double maxx,
                          double miny,
                          double maxy,
                          double area, 
                          double (*f)(double)) 
{
    unsigned long count = 0;
    unsigned long hit = 0;
    double x;
    double y;

    for (count = 0; count < 1000000; count++) {
        x = randRange(minx, maxx);
        y = randRange(miny, maxy);

        if ( y < f(x) ) {
            hit++;
        }
    }
    return ((double) hit / (double) count) * area;
}

double mc_integral_complex(double minx,
                           double maxx,
                           double (*f)(double))
{
    unsigned long count = 0;
    double sum = 0;
    double avg = 0;
    double x;

    for (count = 0; count < 1000000; count++) {
        x = randRange(minx, maxx);
        sum += f(x);
    }
    avg = sum / 1000000;

    return (maxx - minx) * avg;
    /*
     for(i = 0; i < dimensions; i++) {
        product *= maxx[i] - minx[i];
     }
    
    */
}
 

static unsigned long ix = 42;

double Random(void) {
    ix = ix * 69069u + 1u;
    return ix / ((double) ULONG_MAX + 1.0);
}

int main(void) {
    double result = Random();
    for (int i = 0; i < 42; i++) {
        result = Random();
        //double result = mc_integral_complex(0, 2, log10);
        printf("Result is: %f\n",result);
    }
}
