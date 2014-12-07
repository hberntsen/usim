#include "read.h"
#include <stdio.h>
#include <avr/io.h>

#ifndef F_CPU
#warning "F_CPU is not defined, set to 16MHz per default."
#define F_CPU 16000000
#endif

//#define BAUD 57600
#define BAUD 38400
#include <util/setbaud.h>

#ifndef UCSRB
# ifndef UDRE
# define UDRE UDRE0
# define RXEN RXEN0
# define TXEN TXEN0
# define RXCN RXC0
# endif
# ifdef UCSR0A /* ATmega128 */
# define UCSRA UCSR0A
# define UCSRB UCSR0B
# define UBRRL UBRR0L
# define UBRRH UBRR0H
# define UDR UDR0
# else /* ATmega8 */
# define UCSRA USR
# define UCSRB UCR
# endif
#endif


#ifndef UBRR
# define UBRR UBRRL
#endif

static char serial_initialized = 0;

//void serial_init(void)
//{
//  UBRRH = UBRRH_VALUE;
//  UBRRL = UBRRL_VALUE;
//  /* Enable */
//  UCSRB = (1 << RXEN) | (1 << TXEN);
//}

unsigned char read_byte(void)
{
  if(!serial_initialized)
  {
    serial_init();
    serial_initialized = 1;
  }
  while (!(UCSRA & (1 << RXCN))){};
  return UDR;
}
