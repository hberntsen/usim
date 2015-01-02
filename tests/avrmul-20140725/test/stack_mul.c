#include <stdio.h>
#include "avr.h"
#include "print.h"
#include "randombytes.h"


extern void karatsuba48_branched(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba48_branchfree(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba64_branched(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba64_branchfree(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba80_branched(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba80_branchfree(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba96_branched(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba96_branchfree(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba128_branched(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba128_branchfree(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba160_branched(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba160_branchfree(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba192_branched(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba192_branchfree(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba256_branched(unsigned char *r, const unsigned char *a, const unsigned char *b);
extern void karatsuba256_branchfree(unsigned char *r, const unsigned char *a, const unsigned char *b);


#define STACK_CANARY 0x42
extern unsigned char _end; 
extern unsigned char __stack; 

char str[8];
unsigned int ctr;

unsigned char r[64],x[32],y[32];

static unsigned int stack_count(unsigned char canary)
{
  const unsigned char *p = &_end;
  unsigned int c = 0;
  while(*p == canary && p <= &__stack)
  {
    p++;
    c++;
  }
  return c;
} 

static void write_canary(volatile unsigned char *p)
{
  while(p >= &_end)
    *(p--) = STACK_CANARY;
}


static void stack_mul(void (*func)(unsigned char *, const unsigned char *, const unsigned char *))
{
  volatile unsigned char a; /* Mark the beginning of the stack */

  write_canary(&a);

  (*func)(r,x,y);

  ctr = (int)&a - (int)&_end - stack_count(STACK_CANARY) - 21; /* 21 is 18 pushed caller registers + 3 from function call */
  sprintf(str, "%7d", ctr);
  print(str);
  print(" bytes\n");
}


int main()
{
  /* 48 bits */
  print("karatsuba48_branched:   ");
  stack_mul(karatsuba48_branched);
  print("karatsuba48_branchfree: ");
  stack_mul(karatsuba48_branchfree);

  /* 64 bits */
  print("karatsuba64_branched:   ");
  stack_mul(karatsuba64_branched);
  print("karatsuba64_branchfree: ");
  stack_mul(karatsuba64_branchfree);
 
  /* 80 bits */
  print("karatsuba80_branched:   ");
  stack_mul(karatsuba80_branched);
  print("karatsuba80_branchfree: ");
  stack_mul(karatsuba80_branchfree);
 
  /* 96 bits */
  print("karatsuba96_branched:   ");
  stack_mul(karatsuba96_branched);
  print("karatsuba96_branchfree: ");
  stack_mul(karatsuba96_branchfree);
 
  /* 128 bits */
  print("karatsuba128_branched:   ");
  stack_mul(karatsuba128_branched);
  print("karatsuba128_branchfree: ");
  stack_mul(karatsuba128_branchfree);
 
  /* 160 bits */
  print("karatsuba160_branched:   ");
  stack_mul(karatsuba160_branched);
  print("karatsuba160_branchfree: ");
  stack_mul(karatsuba160_branchfree);
 
  /* 192 bits */
  print("karatsuba192_branched:   ");
  stack_mul(karatsuba192_branched);
  print("karatsuba192_branchfree: ");
  stack_mul(karatsuba192_branchfree);
 
  /* 256 bits */
  print("karatsuba256_branched:   ");
  stack_mul(karatsuba256_branched);
  print("karatsuba256_branchfree: ");
  stack_mul(karatsuba256_branchfree);
  
  avr_end();
  return 0;
}
