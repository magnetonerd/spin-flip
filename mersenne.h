/*This is a Mersenne Twist Library. It comes from M.Matsumoto and Takuji Nishimura's
  paper "Mersenne Twist: a 623-dimensionally equidistriduted uniform number generator"
  Published in ACM Transactions on Modeling and Computer Simulations: Special Issue
  on Uniform Random Number Generators in 1998*/

#include <stdio.h>

/*Period Parameters*/
#define N 624
#define M 397
#define MATRIX_A 0x9908b0bf /*constant vector a */
#define UPPER_MASK 0x80000000 /* most significant w-r bits */
#define LOWER_MASK 0x7fffffff /* least significant r bits*/

/*Tempering Parameters*/
#define TEMPERING_MASK_B 0x9d2c5680
#define TEMPERING_MASK_C 0xefc60000
#define TEMPERING_SHIFT_U(y) (y >> 11)
#define TEMPERING_SHIFT_S(Y) (Y << 7)
#define TEMPERING_SHIFT_T(y) (y << 15)
#define TEMPERING_SHIFT_L(y) (y >> 18)

static unsigned long mt[N]; /*The array for the state vector*/
static int mti = N+1; /*mti == N+1 means mt[N] is not initialized*/

void sgenrand(seed);
double double_genrand();
unsigned long int_genrand();

void sgenrand(seed)
     unsigned long seed;
{
  /* setting initial seeds to mt[N} using
     the generator line 25 of table 1 in
     [KNUTH 1981, The Art of Computer Programming
     Vol. 2 (2nd Ed.), pp102} */

  mt[0] = seed & 0xffffffff;
  for(mti = 1; mti < N; ++mti)
    mt[mti] = (69069 * mt[mti-1]) & 0xffffffff;
}

//double genrand()//generate reals
unsigned long int_genrand() //generate integers
{
  unsigned long y;
  static unsigned long mag01[2] = {0x0,MATRIX_A};
  //mag01[x] = x * MATRIX_A for x=0,1

  if(mti >= N){//generate N words at one time
    int kk;

    if(mti == N+1)//if sgenrand() has not been called
      sgenrand(4357);

    for(kk = 0; kk < N-M; ++kk){
      y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
      mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
    }

    for(;kk < N-1; ++kk){
      y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
      mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
    }
    y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
    mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

    mti = 0;
  }

  y = mt[mti++];
  y ^= TEMPERING_SHIFT_U(y);
  y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
  y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
  y ^= TEMPERING_SHIFT_L(y);

  //return ((double)y/(unsigned long)0xffffffff);//reals
  return y; //for integer generation
}

double double_genrand()//generate reals
//unsigned long int_genrand() //generate integers
{
  unsigned long y;
  static unsigned long mag01[2] = {0x0,MATRIX_A};
  //mag01[x] = x * MATRIX_A for x=0,1

  if(mti >= N){//generate N words at one time
    int kk;

    if(mti == N+1)//if sgenrand() has not been called
      sgenrand(4357);

    for(kk = 0; kk < N-M; ++kk){
      y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
      mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
    }

    for(;kk < N-1; ++kk){
      y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
      mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
    }
    y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
    mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

    mti = 0;
  }

  y = mt[mti++];
  y ^= TEMPERING_SHIFT_U(y);
  y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
  y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
  y ^= TEMPERING_SHIFT_L(y);

  return ((double)y/(unsigned long)0xffffffff);//reals
  //return y; //for integer generation
}
