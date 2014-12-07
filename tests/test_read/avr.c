#include "avr.h"
#include "print.h"

void avr_end()
{
  write_byte(4);
  while(1) {};
}
