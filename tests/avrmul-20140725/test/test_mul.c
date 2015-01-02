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

static void test_mul(void (*func)(unsigned char *, const unsigned char *, const unsigned char *), unsigned int inbytes)
{
  unsigned char a[inbytes];
  unsigned char b[inbytes];
  unsigned char r[2*inbytes];
  unsigned int n,i;

  for(n=0;n<NTESTS;n++)
  {
    randombytes(a,inbytes);
    randombytes(b,inbytes);
    (*func)(r,a,b);

    bigint_print(a,inbytes);
    print("*");
    bigint_print(b,inbytes);
    print("-");
    bigint_print(r,2*inbytes);
    print("\n");
  }

  //multiply maximal values
  for(i=0;i<inbytes;i++)
    a[i] = b[i] = 0xff;
  (*func)(r,a,b);

  bigint_print(a,inbytes);
  print("*");
  bigint_print(b,inbytes);
  print("-");
  bigint_print(r,2*inbytes);
  print("\n");
}


int main()
{
  //48 bits
  test_mul(karatsuba48_branched, 6);
  test_mul(karatsuba48_branchfree, 6);

  //64 bits
  test_mul(karatsuba64_branched, 8);
  test_mul(karatsuba64_branchfree, 8);

  //64 bits
  test_mul(karatsuba80_branched, 10);
  test_mul(karatsuba80_branchfree, 10);

  //96 bits
  test_mul(karatsuba96_branched, 12);
  test_mul(karatsuba96_branchfree, 12);

  //128 bits
  test_mul(karatsuba128_branched, 16);
  test_mul(karatsuba128_branchfree, 16);

  //160 bits
  test_mul(karatsuba160_branched, 20);
  test_mul(karatsuba160_branchfree, 20);

  //192 bits
  test_mul(karatsuba192_branched, 24);
  test_mul(karatsuba192_branchfree, 24);

  //256 bits
  test_mul(karatsuba256_branched, 32);
  test_mul(karatsuba256_branchfree, 32);

  avr_end();
  return 0;
}
