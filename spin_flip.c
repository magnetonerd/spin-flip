#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mersenne.h"

#define MAX 200
#define MAXITER 10000
#define SEED 1324

double ising_energy(int spin[], double J, double B);

int main(int argc, char* argv[])
{
  FILE *ofp,*ofp1;
  char *write = "w";
  char filename[] = "Hysteresis.txt", filename1[] = "Correlation.txt";
  
  int i, element, IVAR;
  
  double J, B, new_ergy, old_ergy, kt, exactenergy;
  double total;

  J = 1.0;
  kt = 0.1;

  int spin[MAX], isign, n, C[MAX], l, total1;

  isign = 1;

  for(i = 0; i < MAX; ++i){
    spin[i] = pow(-1,i);
  }

  for(l = 0; l < MAX; ++l){
    total1 = 0.0;
    for(i = 0; i < MAX; ++i){
      total1 = total1+spin[i]*spin[l];
      printf("total1: %d\n", total1);
    }
    C[l] = total1;
  }

  ofp1 = fopen(filename1,write);

  for(l = 0; l < MAX; ++l)
    fprintf(ofp1,"%d %d\n",l,C[l]);

  fclose(ofp1);

  ofp = fopen(filename,write);
  
 HERE:for(IVAR = -20; IVAR <= 20; ++IVAR){
    B = 0.0 + IVAR*0.1;
    B = B*isign;
    
    total = 0.0;
    sgenrand(SEED);
    for(i = 0; i < MAXITER; ++i){
      old_ergy = ising_energy(spin,J,B);
      element = int_genrand()%200;
      spin[element] = spin[element]*(-1);
      new_ergy = ising_energy(spin,J,B);
      if((new_ergy < old_ergy) && exp(-(new_ergy + old_ergy)/kt) < double_genrand()){
	spin[element]=spin[element]*(-1);
      }
    }
    for(n = 0; n < MAX; ++n)
      total = total + spin[n];
    
    fprintf(ofp,"%5f %5f\n", B, total); //B-file on x-axis Magnetization on y-axis
    
    /* if(isign == 1){
      isign = -1;
      goto HERE;
      }*/
  }

  if(isign == 1){
    isign = -1;
    goto HERE;
  }
  fclose(ofp);
  return 0;
}

double ising_energy(int spin[], double J, double B)
{
  double ergy = 0.0;
  int i;

  for(i = 0; i < MAX-1; ++i){
    ergy = ergy - J*spin[i]*spin[i+1] - B*spin[i];
  }

  ergy = ergy - B*spin[MAX];
}
