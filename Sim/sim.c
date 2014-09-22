#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define BIT0 0x01 // Carry flag
#define BIT1 0x02 // Zero flag
#define BIT2 0x04 // Interrupt flag
#define BIT3 0x08 // Decimal mode flag
#define BIT4 0x10 // BRK flag
#define BIT5 0x20 // Unused, should always be 1
#define BIT6 0x40 // Overflow flag
#define BIT7 0x80 // Sign flag

#define SET_BIT(bit) status = status | bit
#define CLEAR_BIT(bit) status = status & (bit ^ 0xFF);

unsigned char memory[65536];

unsigned char ac, xr, yr, sp = 0;

unsigned char status = 0x20; // Bit 5 should always be 1

unsigned short int pc = 0;

void set_sign(unsigned char src) {
  if ((src & BIT7) > 0)
    SET_BIT(BIT7);
  else
    CLEAR_BIT(BIT7);
}

void set_zero(unsigned char src) {
  if (src == 0)
    SET_BIT(BIT1);
  else
    CLEAR_BIT(BIT1);
}

void s_inx() {
  unsigned char src = xr;
  src = (src + 1) & 0xff;
  set_sign(src);
  set_zero(src);
  xr = src;
}

void print_registers() {
  printf("xr = %d, yr = %d\nac = %d, sp = %d\nstatus = %#x\n", xr, yr, ac, sp, status);
}

void main_loop() {
  switch (memory[pc]) {
  case 0xE8: // INX
    s_inx();
    break;
  default:
    fprintf(stderr, "ERROR: No parse!\n");
    print_registers();
    exit(1);
  }
}

int main() {
  memory[0] = 0xE8;
  status = 0x80;
  xr = 0x7E;

  printf("memory[0] = %x, ac = %x\n", memory[0], ac);
  print_registers();

  main_loop();

  print_registers();

  s_inx();

  print_registers();
  return 0;
}
