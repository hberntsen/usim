#ifndef PRINT_H
#define PRINT_H

void serial_init(void);

void write_byte(unsigned char c);

void print(const char *s);

void bigint_print(const unsigned char *x, unsigned char xlen);

void printllu(unsigned long long x);

void bigint_print_hex(const unsigned char *x, unsigned char xlen);


#endif
