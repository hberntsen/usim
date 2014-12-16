#include "print.h"

#define UDR (*((char*)0x04))

void serial_init(void)
{
}

void write_byte(unsigned char c)
{
  UDR = c;
}

void print(const char *s)
{
  while(*s != 0)
  {
    write_byte(*s);
    s++;
  }
}

