#include <stdio.h>
#include "avr.h"
#include "print.h"

extern void mul_asm(unsigned char *r, const unsigned char *a, const unsigned char *b);

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
  print("mul_asm: ");
  stack_mul(mul_asm);

  avr_end();
  return 0;
}
