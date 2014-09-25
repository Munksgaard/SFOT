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
#define GET_BIT(bit) status & bit

typedef unsigned char word8_t;
typedef unsigned short int word16_t;

word8_t memory[65536];

word8_t ac = (word8_t)0;
word8_t xr = (word8_t)0;
word8_t yr = (word8_t)0;
word8_t sp = (word8_t)0;

word8_t status = (word8_t)0x20; // Bit 5 should always be 1

word16_t pc = (word16_t)0;

void set_sign(word8_t src) {
  if ((src & BIT7) > 0)
    SET_BIT(BIT7);
  else
    CLEAR_BIT(BIT7);
}

void set_zero(word8_t src) {
  if (src == 0)
    SET_BIT(BIT1);
  else
    CLEAR_BIT(BIT1);
}

int if_zero() {
  return GET_BIT(BIT2);
}

void s_beq() {
  if (IF_ZERO()) {
    pc += memory[pc-1];
  }
}

void s_inx() {
  word8_t src = xr;
  src = (src + 1) & 0xff;
  set_sign(src);
  set_zero(src);
  xr = src;
}

void s_sta(word16_t addr) {
  word8_t src = ac;
  memory[addr] = src;
}

void print_registers() {
  printf("xr = %d, yr = %d, ac = %d, sp = %d, pc = %#x, status = %#x\n",
         xr, yr, ac, sp, pc, status);
}

word8_t addr_Z() {
  pc += 2;
  return memory[pc-1];
}

word8_t addr_ZX() {
  pc += 2;
  return memory[pc-1] + xr;
}

word16_t addr_A() {
  pc += 3;
  return (memory[pc-1] << 8) + memory[pc-2];
}

word16_t addr_AX() {
  pc += 3;
  return (memory[pc-1] << 8) + memory[pc-2] + xr;
}

word16_t addr_AY() {
  pc += 3;
  return (memory[pc-1] << 8) + memory[pc-2] + yr;
}

word16_t addr_IX() {
  pc += 2;
  return memory[memory[pc-1] + xr];
}

word16_t addr_IY() {
  pc += 2;
  return memory[memory[pc-1]] + yr;
}

void main_loop() {
  while (1) {
    switch (memory[pc]) {
    case 0xE8: // INX
      pc++;
      s_inx();
      break;
    case 0xF0: // BEQ
      pc += 2;
      s_beq();
      break;
    case 0x85: // STA_Z
      s_sta(addr_Z());
      break;
    case 0x95: // STA_ZX
      s_sta(addr_ZX());
      break;
    case 0x8D: // STA_A
      s_sta(addr_A());
      break;
    case 0x9D: // STA_AX
      s_sta(addr_AX());
      break;
    case 0x99: // STA_AY
      s_sta(addr_AY());
      break;
    case 0x81: // STA_IX
      s_sta(addr_IX());
      break;
    case 0x91: // STA_IY
      s_sta(addr_IY());
      break;
    default:
      fprintf(stderr, "ERROR: No parse!\n");
      print_registers();
      return;
    }
  }
}

int main() {
  main_loop();

  return 0;
}
