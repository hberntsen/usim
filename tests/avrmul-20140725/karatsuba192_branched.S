; Authors: Michael Hutter and Peter Schwabe
; Version: 2014-07-25
; Public domain

#include <avr/io.h>

  .global karatsuba192_branched
  .type karatsuba192_branched, @function

; operand b: r21:r20
; operand a: r23:r22
; operand r: r25:r24

karatsuba192_branched:

  PUSH R2
  PUSH R3
  PUSH R4
  PUSH R5
  PUSH R6
  PUSH R7
  PUSH R8
  PUSH R9
  PUSH R10
  PUSH R11
  PUSH R12
  PUSH R13
  PUSH R14
  PUSH R15
  PUSH R16
  PUSH R17
  PUSH R28
  PUSH R29

  MOVW R30, R24
  MOVW R28, R20
  MOVW R26, R22

  ;------ level 1: compute L ------

  ; init zero registers
  CLR R20
  CLR R21
  MOVW R22, R20
  MOVW R24, R20
  
  ;--- level 2: compute L ---
  LD R2, X+
  LDD R8,  Y+0
  LDD R9,  Y+1
  LDD R10, Y+2
  LDD R11, Y+3
  LDD R12, Y+4
  LDD R13, Y+5

  MUL R2, R10 ;a0 * b2
  MOVW R16, R0
  MUL R2, R8 ;a0 * b0
  MOVW R14, R0
  MUL R2, R9 ;a0 * b1
  ADD R15, R0
  ADC R16, R1
  ADC R17, R25
  MUL R2, R12 ;a0 * b4
  MOVW R18, R0
  MUL R2, R11 ;a0 * b3
  ADD R17, R0
  ADC R18, R1
  ADC R19, R25
  MUL R2, R13 ;a0 * b5
  ADD R19, R0
  ADC R20, R1

  LD R3, X+
  MUL R3, R10 ;a1 * b2
  MOVW R6, R0
  MUL R3, R8 ;a1 * b0
  ADD R15, R0
  ADC R16, R1
  ADC R17, R6
  ADC R7, R25
  MUL R3, R9 ;a1 * b1
  ADD R16, R0
  ADC R17, R1
  ADC R7, R25
  MUL R3, R12 ;a1 * b4
  ADD R18, R7
  ADC R19, R0
  ADC R20, R1
  ADC R21, R25
  MUL R3, R11 ;a1 * b3
  MOVW R6, R0
  MUL R3, R13 ;a1 * b5
  ADD R18, R6
  ADC R19, R7
  ADC R20, R0
  ADC R21, R1

  LD R4, X+
  MUL R4, R10 ;a2 * b2
  MOVW R6, R0
  MUL R4, R8 ;a2 * b0
  ADD R16, R0
  ADC R17, R1
  ADC R18, R6
  ADC R7, R25
  MUL R4, R9 ;a2 * b1
  ADD R17, R0
  ADC R18, R1
  ADC R7, R25
  MUL R4, R12 ;a2 * b4
  ADD R19, R7
  ADC R20, R0
  ADC R21, R1
  ADC R22, R25
  MUL R4, R11 ;a2 * b3
  MOVW R6, R0
  MUL R4, R13 ;a2 * b5
  ADD R19, R6
  ADC R20, R7
  ADC R21, R0
  ADC R22, R1
  STD Z+0, R14
  STD Z+1, R15
  STD Z+2, R16

  LD R5, X+
  MUL R5, R10 ;a3 * b2
  MOVW R14, R0
  MUL R5, R8 ;a3 * b0
  ADD R17, R0
  ADC R18, R1
  ADC R19, R14
  ADC R15, R25
  MUL R5, R9 ;a3 * b1
  ADD R18, R0
  ADC R19, R1
  ADC R15, R25
  MUL R5, R12 ;a3 * b4
  ADD R20, R15
  ADC R21, R0
  ADC R22, R1
  ADC R23, R25
  MUL R5, R11 ;a3 * b3
  MOVW R14, R0
  MUL R5, R13 ;a3 * b5
  ADD R20, R14
  ADC R21, R15
  ADC R22, R0
  ADC R23, R1

  LD R6, X+
  MUL R6, R10 ;a4 * b2
  MOVW R14, R0
  MUL R6, R8 ;a4 * b0
  ADD R18, R0
  ADC R19, R1
  ADC R20, R14
  ADC R15, R25
  MUL R6, R9 ;a4 * b1
  ADD R19, R0
  ADC R20, R1
  ADC R15, R25
  MUL R6, R12 ;a4 * b4
  ADD R21, R15
  ADC R22, R0
  ADC R23, R1
  ADC R24, R25
  MUL R6, R11 ;a4 * b3
  MOVW R14, R0
  MUL R6, R13 ;a4 * b5
  ADD R21, R14
  ADC R22, R15
  ADC R23, R0
  ADC R24, R1

  LD R7, X+
  MUL R7, R10 ;a5 * b2
  MOVW R14, R0
  MUL R7, R8 ;a5 * b0
  ADD R19, R0
  ADC R20, R1
  ADC R21, R14
  ADC R15, R25
  MUL R7, R9 ;a5 * b1
  ADD R20, R0
  ADC R21, R1
  ADC R15, R25
  MUL R7, R12 ;a5 * b4
  ADD R22, R15
  ADC R23, R0
  ADC R24, R1
  ADC R25, R25
  MUL R7, R11 ;a5 * b3
  MOVW R14, R0
  MUL R7, R13 ;a5 * b5
  ADD R22, R14
  ADC R23, R15
  ADC R24, R0
  ADC R25, R1
  STD Z+3, R17
  STD Z+4, R18
  STD Z+5, R19

  ;--- load a6..a11 and b6..b11 ---
  LD R14, X+
  LD R15, X+
  LD R16, X+
  LD R17, X+
  LD R18, X+
  LD R19, X+

  PUSH R26 ; save X register
  PUSH R27
  PUSH R28 ; save Y register
  PUSH R29
  PUSH R30 ; save Z register
  PUSH R31

  LDD R8,  Y+6
  LDD R9,  Y+7
  LDD R10, Y+8
  LDD R11, Y+9
  LDD R12, Y+10
  LDD R13, Y+11
  
  ;--- Compute H + (l6,l7,l8,l9,l10,l11) ---
  CLR R30
  CLR R31
  MOVW R2, R30
  MOVW R4, R30
  MOVW R6, R30

  MUL R8, R14
  ADD R20, R0   
  ADC R21, R1   
  ADC R22, R31
  ADC R6, R31  

  MUL R8, R15
  ADD R21, R0
  ADC R22, R1
  ADC R6, R31
  MUL R9, R14
  ADD R21, R0
  ADC R22, R1
  ADC R23, R6
  ADC R7, R31

  CLR R6
  MUL R8, R16
  ADD R22, R0 
  ADC R23, R1
  ADC R7, R31  
  MUL R9, R15
  ADD R22, R0
  ADC R23, R1
  ADC R7, R31
  MUL R10, R14
  ADD R22, R0
  ADC R23, R1
  ADC R24, R7
  ADC R6, R31

  CLR R7
  MUL R8, R17
  ADD R23, R0
  ADC R24, R1
  ADC R6, R31
  MUL R9, R16
  ADD R23, R0
  ADC R24, R1
  ADC R6, R31
  MUL R10, R15
  ADD R23, R0
  ADC R24, R1
  ADC R6, R31
  MUL R11, R14
  ADD R23, R0
  ADC R24, R1
  ADC R25, R6
  ADC R7, R31  

  CLR R6
  MUL R8, R18
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R9, R17
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R10, R16
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R11, R15
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R12, R14
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31

  CLR R6
  MUL R8, R19
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R9, R18
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R10, R17
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R11, R16
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R12, R15
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R13, R14
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31

  MUL R15, R13
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R16, R12
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R17, R11
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R18, R10
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R19, R9
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31

  MUL R16, R13
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31
  MUL R17, R12
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31
  MUL R18, R11
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31
  MUL R19, R10
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31

  MUL R17, R13
  ADD R2, R0
  ADC R3, R1
  ADC R4, R31
  MUL R18, R12
  ADD R2, R0
  ADC R3, R1
  ADC R4, R31
  MUL R19, R11
  ADD R2, R0
  ADC R3, R1
  ADC R4, R31

  MUL R18, R13
  ADD R3, R0
  ADC R4, R1
  ADC R5, R31
  MUL R19, R12
  ADD R3, R0
  ADC R4, R1
  ADC R5, R31

  MUL R19, R13
  ADD R4, R0
  ADC R5, R1
  
  ; push h6 and h7 on stack
  PUSH R6
  PUSH R7
  
  ;--- subtract a0-a5 ---
  SBIW R26, 12
  LD R0, X+
  SUB R14, R0
  LD R0, X+
  SBC R15, R0
  LD R0, X+
  SBC R16, R0
  LD R0, X+
  SBC R17, R0
  LD R0, X+
  SBC R18, R0
  LD R0, X+
  SBC R19, R0
  ; 0xff if carry and 0x00 if no carry
  SBC R0, R0

  ;--- subtract b0-b5 ---
  LDD R1, Y+0
  SUB R8, R1
  LDD R1, Y+1
  SBC R9, R1
  LDD R1, Y+2
  SBC R10, R1
  LDD R1, Y+3
  SBC R11, R1
  LDD R1, Y+4
  SBC R12, R1
  LDD R1, Y+5
  SBC R13, R1
  ; 0xff if carry and 0x00 if no carry
  SBC R1, R1

  ;--- absolute values ---    
  EOR R14, R0
  EOR R15, R0
  EOR R16, R0
  EOR R17, R0
  EOR R18, R0
  EOR R19, R0  
  EOR R8,  R1
  EOR R9,  R1
  EOR R10, R1
  EOR R11, R1
  EOR R12, R1
  EOR R13, R1
  NEG R0
  NEG R1
  ADD R14, R0
  ADC R15, R31
  ADC R16, R31
  ADC R17, R31
  ADC R18, R31
  ADC R19, R31
  ADD R8,  R1
  ADC R9,  R31
  ADC R10, R31
  ADC R11, R31
  ADC R12, R31
  ADC R13, R31
  EOR R0, R1
  BST R0, 0   
  
  ;--- Compute M ---
  MOVW R26, R30
  MOVW R28, R30  
  
  MUL R14, R8
  MOVW R6, R0
  
  MUL R14, R9
  ADD R7, R0
  ADC R26, R1
  MUL R15, R8
  ADD R7, R0
  ADC R26, R1
  ADC R27, R31
  
  MUL R14, R10
  ADD R26, R0
  ADC R27, R1
  ADC R28, R31
  MUL R15, R9
  ADD R26, R0
  ADC R27, R1
  ADC R28, R31
  MUL R16, R8
  ADD R26, R0
  ADC R27, R1
  ADC R28, R31
  
  MUL R14, R11
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  MUL R15, R10
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  MUL R16, R9
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  MUL R17, R8
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  
  MUL R14, R12
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R15, R11
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R16, R10
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R17, R9
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R18, R8
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  
  MUL R14, R13
  CLR R14
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R15, R12
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R16, R11
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R17, R10
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R18, R9
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R19, R8
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  
  CLR R8
  MUL R15, R13
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R16, R12
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R17, R11
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R18, R10
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R19, R9
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  
  CLR R9
  MUL R16, R13
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  MUL R17, R12
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  MUL R18, R11
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  MUL R19, R10
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  
  CLR R10
  MUL R17, R13
  ADD R8, R0
  ADC R9, R1
  ADC R10, R14
  MUL R18, R12
  ADD R8, R0
  ADC R9, R1
  ADC R10, R14
  MUL R19, R11
  ADD R8, R0
  ADC R9, R1
  ADC R10, R14
  
  CLR R11
  MUL R18, R13
  ADD R9, R0
  ADC R10, R1
  ADC R11, R14
  MUL R19, R12
  ADD R9, R0
  ADC R10, R1
  ADC R11, R14
  
  MUL R19, R13
  ADD R10, R0
  ADC R11, R1
  
  MOVW R12, R30
  
  ; restore h6 and h7 and Z register
  POP R0
  POP R1
  POP R31
  POP R30
  
  ;--- add l5+h0 to l0 and h5 ---
  LDD R14, Z+0
  LDD R15, Z+1
  LDD R16, Z+2
  LDD R17, Z+3
  LDD R18, Z+4
  LDD R19, Z+5
  ADD R14, R20
  ADC R15, R21
  ADC R16, R22
  ADC R17, R23
  ADC R18, R24
  ADC R19, R25
  ADC R20, R0
  ADC R21, R1
  ADC R22, R2
  ADC R23, R3
  ADC R24, R4
  ADC R25, R5
  ; store carry in T register
      
  ;--- process sign bit ---  
  BRTS add_M
  ROL R5
  BST R5, 0
  ROR R5
  
  ; subtract M
  SUB R14, R6
  SBC R15, R7
  SBC R16, R26
  SBC R17, R27
  SBC R18, R28
  SBC R19, R29
  SBC R20, R12
  SBC R21, R13
  SBC R22, R8
  SBC R23, R9
  SBC R24, R10
  SBC R25, R11
  SBC R6, R6
  SBC R7, R7  
  ; R7:R6 is -1,0, or 1
  RJMP final_L

add_M: 
  ROL R5
  BST R5, 0
  ROR R5
  
  ADD R14, R6
  ADC R15, R7
  ADC R16, R26
  ADC R17, R27
  ADC R18, R28
  ADC R19, R29  
  ADC R20, R12
  ADC R21, R13
  ADC R22, R8
  ADC R23, R9
  ADC R24, R10
  ADC R25, R11
  CLR R6
  CLR R7
  ADC R6, R6

final_L:
  BLD R8, 0
  ASR R8
  ;--- propagate carry to end ---
  ADC R0, R6
  ADC R1, R7
  ADC R2, R7
  ADC R3, R7
  ADC R4, R7
  ADC R5, R7

  STD Z+6, R14
  STD Z+7, R15
  STD Z+8, R16
  STD Z+9, R17
  STD Z+10, R18
  STD Z+11, R19
  STD Z+12, R20
  STD Z+13, R21
  STD Z+14, R22
  STD Z+15, R23
  STD Z+16, R24
  STD Z+17, R25
  STD Z+18, R0
  STD Z+19, R1
  STD Z+20, R2
  STD Z+21, R3
  STD Z+22, R4
  STD Z+23, R5
  
  POP R29
  POP R28
  POP R27
  POP R26

  ;------ level 1: compute H ------

  ;init zero registers
  CLR R20
  CLR R21
  MOVW R22, R20
  MOVW R24, R20
  
  ;--- level 2: Compute L ---
  LD R2, X+
  LDD R8,  Y+12
  LDD R9,  Y+13
  LDD R10, Y+14
  LDD R11, Y+15
  LDD R12, Y+16
  LDD R13, Y+17

  MUL R2, R10 ;a0 * b2
  MOVW R16, R0
  MUL R2, R8 ;a0 * b0
  MOVW R14, R0
  MUL R2, R9 ;a0 * b1
  ADD R15, R0
  ADC R16, R1
  ADC R17, R25
  MUL R2, R12 ;a0 * b4
  MOVW R18, R0
  MUL R2, R11 ;a0 * b3
  ADD R17, R0
  ADC R18, R1
  ADC R19, R25
  MUL R2, R13 ;a0 * b5
  ADD R19, R0
  ADC R20, R1

  LD R3, X+
  MUL R3, R10 ;a1 * b2
  MOVW R6, R0
  MUL R3, R8 ;a1 * b0
  ADD R15, R0
  ADC R16, R1
  ADC R17, R6
  ADC R7, R25
  MUL R3, R9 ;a1 * b1
  ADD R16, R0
  ADC R17, R1
  ADC R7, R25
  MUL R3, R12 ;a1 * b4
  ADD R18, R7
  ADC R19, R0
  ADC R20, R1
  ADC R21, R25
  MUL R3, R11 ;a1 * b3
  MOVW R6, R0
  MUL R3, R13 ;a1 * b5
  ADD R18, R6
  ADC R19, R7
  ADC R20, R0
  ADC R21, R1

  LD R4, X+
  MUL R4, R10 ;a2 * b2
  MOVW R6, R0
  MUL R4, R8 ;a2 * b0
  ADD R16, R0
  ADC R17, R1
  ADC R18, R6
  ADC R7, R25
  MUL R4, R9 ;a2 * b1
  ADD R17, R0
  ADC R18, R1
  ADC R7, R25
  MUL R4, R12 ;a2 * b4
  ADD R19, R7
  ADC R20, R0
  ADC R21, R1
  ADC R22, R25
  MUL R4, R11 ;a2 * b3
  MOVW R6, R0
  MUL R4, R13 ;a2 * b5
  ADD R19, R6
  ADC R20, R7
  ADC R21, R0
  ADC R22, R1
  STD Z+24, R14
  STD Z+25, R15
  STD Z+26, R16

  LD R5, X+
  MUL R5, R10 ;a3 * b2
  MOVW R14, R0
  MUL R5, R8 ;a3 * b0
  ADD R17, R0
  ADC R18, R1
  ADC R19, R14
  ADC R15, R25
  MUL R5, R9 ;a3 * b1
  ADD R18, R0
  ADC R19, R1
  ADC R15, R25
  MUL R5, R12 ;a3 * b4
  ADD R20, R15
  ADC R21, R0
  ADC R22, R1
  ADC R23, R25
  MUL R5, R11 ;a3 * b3
  MOVW R14, R0
  MUL R5, R13 ;a3 * b5
  ADD R20, R14
  ADC R21, R15
  ADC R22, R0
  ADC R23, R1

  LD R6, X+
  MUL R6, R10 ;a4 * b2
  MOVW R14, R0
  MUL R6, R8 ;a4 * b0
  ADD R18, R0
  ADC R19, R1
  ADC R20, R14
  ADC R15, R25
  MUL R6, R9 ;a4 * b1
  ADD R19, R0
  ADC R20, R1
  ADC R15, R25
  MUL R6, R12 ;a4 * b4
  ADD R21, R15
  ADC R22, R0
  ADC R23, R1
  ADC R24, R25
  MUL R6, R11 ;a4 * b3
  MOVW R14, R0
  MUL R6, R13 ;a4 * b5
  ADD R21, R14
  ADC R22, R15
  ADC R23, R0
  ADC R24, R1

  LD R7, X+
  MUL R7, R10 ;a5 * b2
  MOVW R14, R0
  MUL R7, R8 ;a5 * b0
  ADD R19, R0
  ADC R20, R1
  ADC R21, R14
  ADC R15, R25
  MUL R7, R9 ;a5 * b1
  ADD R20, R0
  ADC R21, R1
  ADC R15, R25
  MUL R7, R12 ;a5 * b4
  ADD R22, R15
  ADC R23, R0
  ADC R24, R1
  ADC R25, R25
  MUL R7, R11 ;a5 * b3
  MOVW R14, R0
  MUL R7, R13 ;a5 * b5
  ADD R22, R14
  ADC R23, R15
  ADC R24, R0
  ADC R25, R1
  STD Z+27, R17
  STD Z+28, R18
  STD Z+29, R19

  ;--- load a6..a11 and b6..b11 ---
  LD R14, X+
  LD R15, X+
  LD R16, X+
  LD R17, X+
  LD R18, X+
  LD R19, X+
  
  PUSH R26 ; save X register
  PUSH R27
  PUSH R28 ; save Y register
  PUSH R29
  PUSH R30 ; save Z register
  PUSH R31

  LDD R8,  Y+18
  LDD R9,  Y+19
  LDD R10, Y+20
  LDD R11, Y+21
  LDD R12, Y+22
  LDD R13, Y+23
  
  ;--- Compute H + (l6,l7,l8,l9,l10,l11) ---
  CLR R30
  CLR R31
  MOVW R2, R30
  MOVW R4, R30
  MOVW R6, R30

  MUL R8, R14
  ADD R20, R0   
  ADC R21, R1   
  ADC R22, R31
  ADC R6, R31  

  MUL R8, R15
  ADD R21, R0
  ADC R22, R1
  ADC R6, R31
  MUL R9, R14
  ADD R21, R0
  ADC R22, R1
  ADC R23, R6
  ADC R7, R31

  CLR R6
  MUL R8, R16
  ADD R22, R0 
  ADC R23, R1
  ADC R7, R31  
  MUL R9, R15
  ADD R22, R0
  ADC R23, R1
  ADC R7, R31
  MUL R10, R14
  ADD R22, R0
  ADC R23, R1
  ADC R24, R7
  ADC R6, R31

  CLR R7
  MUL R8, R17
  ADD R23, R0
  ADC R24, R1
  ADC R6, R31
  MUL R9, R16
  ADD R23, R0
  ADC R24, R1
  ADC R6, R31
  MUL R10, R15
  ADD R23, R0
  ADC R24, R1
  ADC R6, R31
  MUL R11, R14
  ADD R23, R0
  ADC R24, R1
  ADC R25, R6
  ADC R7, R31  

  CLR R6
  MUL R8, R18
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R9, R17
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R10, R16
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R11, R15
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R12, R14
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31

  CLR R6
  MUL R8, R19
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R9, R18
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R10, R17
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R11, R16
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R12, R15
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R13, R14
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31

  MUL R15, R13
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R16, R12
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R17, R11
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R18, R10
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R19, R9
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31

  MUL R16, R13
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31
  MUL R17, R12
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31
  MUL R18, R11
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31
  MUL R19, R10
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31

  MUL R17, R13
  ADD R2, R0
  ADC R3, R1
  ADC R4, R31
  MUL R18, R12
  ADD R2, R0
  ADC R3, R1
  ADC R4, R31
  MUL R19, R11
  ADD R2, R0
  ADC R3, R1
  ADC R4, R31

  MUL R18, R13
  ADD R3, R0
  ADC R4, R1
  ADC R5, R31
  MUL R19, R12
  ADD R3, R0
  ADC R4, R1
  ADC R5, R31

  MUL R19, R13
  ADD R4, R0
  ADC R5, R1
  
  ; push h6 and h7 on stack
  PUSH R6
  PUSH R7
  
  ;--- subtract a0-a5 ---
  SBIW R26, 12
  LD R0, X+
  SUB R14, R0
  LD R0, X+
  SBC R15, R0
  LD R0, X+
  SBC R16, R0
  LD R0, X+
  SBC R17, R0
  LD R0, X+
  SBC R18, R0
  LD R0, X+
  SBC R19, R0
  ; 0xff if carry and 0x00 if no carry
  SBC R0, R0

  ;--- subtract b0-b5 ---
  LDD R1, Y+12
  SUB R8, R1
  LDD R1, Y+13
  SBC R9, R1
  LDD R1, Y+14
  SBC R10, R1
  LDD R1, Y+15
  SBC R11, R1
  LDD R1, Y+16
  SBC R12, R1
  LDD R1, Y+17
  SBC R13, R1
  ; 0xff if carry and 0x00 if no carry
  SBC R1, R1

  ;--- absolute values ---    
  EOR R14, R0
  EOR R15, R0
  EOR R16, R0
  EOR R17, R0
  EOR R18, R0
  EOR R19, R0  
  EOR R8,  R1
  EOR R9,  R1
  EOR R10, R1
  EOR R11, R1
  EOR R12, R1
  EOR R13, R1
  NEG R0
  NEG R1
  ADD R14, R0
  ADC R15, R31
  ADC R16, R31
  ADC R17, R31
  ADC R18, R31
  ADC R19, R31
  ADD R8,  R1
  ADC R9,  R31
  ADC R10, R31
  ADC R11, R31
  ADC R12, R31
  ADC R13, R31
  EOR R0, R1
  BST R0, 0   
  
  ;--- Compute M ---
  MOVW R26, R30
  MOVW R28, R30  
  
  MUL R14, R8
  MOVW R6, R0
  
  MUL R14, R9
  ADD R7, R0
  ADC R26, R1
  MUL R15, R8
  ADD R7, R0
  ADC R26, R1
  ADC R27, R31
  
  MUL R14, R10
  ADD R26, R0
  ADC R27, R1
  ADC R28, R31
  MUL R15, R9
  ADD R26, R0
  ADC R27, R1
  ADC R28, R31
  MUL R16, R8
  ADD R26, R0
  ADC R27, R1
  ADC R28, R31
  
  MUL R14, R11
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  MUL R15, R10
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  MUL R16, R9
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  MUL R17, R8
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  
  MUL R14, R12
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R15, R11
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R16, R10
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R17, R9
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R18, R8
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  
  MUL R14, R13
  CLR R14
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R15, R12
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R16, R11
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R17, R10
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R18, R9
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R19, R8
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  
  CLR R8
  MUL R15, R13
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R16, R12
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R17, R11
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R18, R10
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R19, R9
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  
  CLR R9
  MUL R16, R13
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  MUL R17, R12
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  MUL R18, R11
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  MUL R19, R10
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  
  CLR R10
  MUL R17, R13
  ADD R8, R0
  ADC R9, R1
  ADC R10, R14
  MUL R18, R12
  ADD R8, R0
  ADC R9, R1
  ADC R10, R14
  MUL R19, R11
  ADD R8, R0
  ADC R9, R1
  ADC R10, R14
  
  CLR R11
  MUL R18, R13
  ADD R9, R0
  ADC R10, R1
  ADC R11, R14
  MUL R19, R12
  ADD R9, R0
  ADC R10, R1
  ADC R11, R14
  
  MUL R19, R13
  ADD R10, R0
  ADC R11, R1
  
  MOVW R12, R30
  
  ; restore h6 and h7 and Z register
  POP R0
  POP R1
  POP R31
  POP R30
  
  ;--- add l5+h0 to l0 and h5 ---
  LDD R14, Z+24
  LDD R15, Z+25
  LDD R16, Z+26
  LDD R17, Z+27
  LDD R18, Z+28
  LDD R19, Z+29
  ADD R14, R20
  ADC R15, R21
  ADC R16, R22
  ADC R17, R23
  ADC R18, R24
  ADC R19, R25
  ADC R20, R0
  ADC R21, R1
  ADC R22, R2
  ADC R23, R3
  ADC R24, R4
  ADC R25, R5
  ; store carry in T register
      
  ;--- process sign bit ---  
  BRTS add_M_H
  ROL R5
  BST R5, 0
  ROR R5
  
  ; subtract M
  SUB R14, R6
  SBC R15, R7
  SBC R16, R26
  SBC R17, R27
  SBC R18, R28
  SBC R19, R29
  SBC R20, R12
  SBC R21, R13
  SBC R22, R8
  SBC R23, R9
  SBC R24, R10
  SBC R25, R11
  SBC R6, R6
  SBC R7, R7  
  ; R7:R6 is -1,0, or 1
  RJMP final_H

add_M_H: 
  ROL R5
  BST R5, 0
  ROR R5
  
  ADD R14, R6
  ADC R15, R7
  ADC R16, R26
  ADC R17, R27
  ADC R18, R28
  ADC R19, R29  
  ADC R20, R12
  ADC R21, R13
  ADC R22, R8
  ADC R23, R9
  ADC R24, R10
  ADC R25, R11
  CLR R6
  CLR R7
  ADC R6, R6

final_H:
  BLD R8, 0
  ASR R8
  ;--- propagate carry to end ---
  ADC R0, R6
  ADC R1, R7
  ADC R2, R7
  ADC R3, R7
  ADC R4, R7
  ADC R5, R7

  STD Z+36, R20
  STD Z+37, R21
  STD Z+38, R22
  STD Z+39, R23
  STD Z+40, R24
  STD Z+41, R25
  STD Z+42, R0
  STD Z+43, R1
  STD Z+44, R2
  STD Z+45, R3
  STD Z+46, R4
  STD Z+47, R5
  
  POP R29
  POP R28
  POP R27
  POP R26

  ;------ level 1: combine L and H ------
  LDD R6, Z+24
  LDD R7, Z+25
  LDD R8, Z+26
  LDD R9, Z+27
  LDD R10, Z+28
  LDD R11, Z+29

  ; H is stored in: 6,7,8,9,10,11,14,15,16,17,18,19,20,21,22,23,24,25,0,1,2,3,4,5
  ; add HIGH(L)
  LDD R12, Z+12
  ADD R6, R12
  LDD R12, Z+13
  ADC R7, R12
  LDD R12, Z+14
  ADC R8, R12
  LDD R12, Z+15
  ADC R9, R12
  LDD R12, Z+16
  ADC R10, R12
  LDD R12, Z+17
  ADC R11, R12
  LDD R12, Z+18
  ADC R14, R12
  LDD R12, Z+19
  ADC R15, R12
  LDD R12, Z+20
  ADC R16, R12
  LDD R12, Z+21
  ADC R17, R12
  LDD R12, Z+22
  ADC R18, R12
  LDD R12, Z+23
  ADC R19, R12
  ; store carry on stack
  CLR R12
  CLR R13
  ADC R13, R12
  ; propagate carry to end
  ADD R20, R13
  ADC R21, R12
  ADC R22, R12
  ADC R23, R12
  ADC R24, R12
  ADC R25, R12
  ADC R0,  R12
  ADC R1,  R12
  ADC R2,  R12
  ADC R3,  R12
  ADC R4,  R12
  ADC R5,  R12

  LDD R12, Z+0
  ADD R12, R6
  STD Z+12, R12
  LDD R12, Z+1
  ADC R12, R7
  STD Z+13, R12
  LDD R12, Z+2
  ADC R12, R8
  STD Z+14, R12
  LDD R12, Z+3
  ADC R12, R9
  STD Z+15, R12
  LDD R12, Z+4
  ADC R12, R10
  STD Z+16, R12
  LDD R12, Z+5
  ADC R12, R11
  STD Z+17, R12
  LDD R12, Z+6
  ADC R12, R14
  STD Z+18, R12
  LDD R12, Z+7
  ADC R12, R15
  STD Z+19, R12
  LDD R12, Z+8
  ADC R12, R16
  STD Z+20, R12
  LDD R12, Z+9
  ADC R12, R17
  STD Z+21, R12
  LDD R12, Z+10
  ADC R12, R18
  STD Z+22, R12
  LDD R12, Z+11
  ADC R12, R19
  STD Z+23, R12

  ADC R20, R6
  STD Z+24, R20
  ADC R21, R7
  STD Z+25, R21
  ADC R22, R8
  STD Z+26, R22
  ADC R23, R9
  STD Z+27, R23
  ADC R24, R10
  STD Z+28, R24
  ADC R25, R11
  STD Z+29, R25
  ADC R0, R14
  STD Z+30, R0
  ADC R1, R15
  STD Z+31, R1
  ADC R2, R16
  STD Z+32, R2
  ADC R3, R17
  STD Z+33, R3
  ADC R4, R18
  STD Z+34, R4
  ADC R5, R19
  STD Z+35, R5
  ; store carry in R13
  CLR R12
  ADC R13, R12
  PUSH R13

  ;------ level 1: subtract a0-a12 ------
  SBIW R26, 24
  LD R2,  X+
  LD R3,  X+
  LD R4,  X+
  LD R5,  X+
  LD R6,  X+
  LD R7,  X+
  LD R14, X+
  LD R15, X+
  LD R16, X+
  LD R17, X+
  LD R18, X+
  LD R19, X+
  LD R8,  X+
  LD R9,  X+
  LD R10, X+
  LD R11, X+
  LD R12, X+
  LD R13, X+
  LD R20, X+
  LD R21, X+
  LD R22, X+
  LD R23, X+
  LD R24, X+
  LD R25, X+
  SUB R2,  R8
  SBC R3,  R9
  SBC R4,  R10
  SBC R5,  R11
  SBC R6,  R12
  SBC R7,  R13
  SBC R14, R20
  SBC R15, R21
  SBC R16, R22
  SBC R17, R23
  SBC R18, R24
  SBC R19, R25
  ; 0xff if carry and 0x00 if no carry
  SBC R0,  R0

  ;------ level 1: absolute values ------
  EOR R2,  R0
  EOR R3,  R0
  EOR R4,  R0
  EOR R5,  R0
  EOR R6,  R0
  EOR R7,  R0
  EOR R14, R0
  EOR R15, R0
  EOR R16, R0
  EOR R17, R0
  EOR R18, R0
  EOR R19, R0
  NEG R0
  CLR R25
  ADD R2,  R0
  ADC R3,  R25
  ADC R4,  R25
  ADC R5,  R25
  ADC R6,  R25
  ADC R7,  R25
  ADC R14, R25
  ADC R15, R25
  ADC R16, R25
  ADC R17, R25
  ADC R18, R25
  ADC R19, R25
  PUSH R19
  PUSH R18
  PUSH R17
  PUSH R16
  PUSH R15
  PUSH R14
  PUSH R7 
  PUSH R6 
  PUSH R5 
  PUSH R4 
  PUSH R3 
  PUSH R2 

  ;------ level 1: subtract b0-b12 ------
  LDD R8,  Y+0
  LDD R9,  Y+1
  LDD R10, Y+2
  LDD R11, Y+3
  LDD R12, Y+4
  LDD R13, Y+5
  LDD R14, Y+6
  LDD R15, Y+7
  LDD R16, Y+8
  LDD R17, Y+9
  LDD R18, Y+10
  LDD R19, Y+11
  LDD R20, Y+12  
  SUB R8,  R20
  LDD R20, Y+13
  SBC R9,  R20
  LDD R20, Y+14
  SBC R10, R20
  LDD R20, Y+15
  SBC R11, R20
  LDD R20, Y+16
  SBC R12, R20
  LDD R20, Y+17
  SBC R13, R20
  LDD R20, Y+18
  SBC R14, R20
  LDD R20, Y+19
  SBC R15, R20
  LDD R20, Y+20
  SBC R16, R20
  LDD R20, Y+21
  SBC R17, R20
  LDD R20, Y+22
  SBC R18, R20
  LDD R20, Y+23
  SBC R19, R20
  ; 0xff if carry and 0x00 if no carry
  SBC R1,  R1

  ;------ level 1: absolute values ------
  EOR R8,  R1
  EOR R9,  R1
  EOR R10, R1
  EOR R11, R1
  EOR R12, R1
  EOR R13, R1
  EOR R14, R1
  EOR R15, R1
  EOR R16, R1
  EOR R17, R1
  EOR R18, R1
  EOR R19, R1
  NEG R1
  ADD R8,  R1
  ADC R9,  R25
  ADC R10, R25
  ADC R11, R25
  ADC R12, R25
  ADC R13, R25
  ADC R14, R25
  ADC R15, R25
  ADC R16, R25
  ADC R17, R25
  ADC R18, R25
  ADC R19, R25
  PUSH R19 
  PUSH R18
  PUSH R17
  PUSH R16 
  PUSH R15 
  PUSH R14 
  PUSH R13 
  PUSH R12 
  PUSH R11 
  PUSH R10
  PUSH R9
  IN R28, _SFR_IO_ADDR(SPL)
  IN R29, _SFR_IO_ADDR(SPH) 
  PUSH R8
  EOR R0, R1
  PUSH R0
    
  PUSH R30 ; save Z register
  PUSH R31

  ; now load stack pointer in R31:30
  IN R30, _SFR_IO_ADDR(SPL)
  IN R31, _SFR_IO_ADDR(SPH) 
  SBIW R30, 7

  ;------ level 1: compute M ------

  ; init zero registers
  CLR R20
  CLR R21
  MOVW R22, R20
  MOVW R24, R20
  
  ;--- level 2: compute L ---
  LDD R8,  Y+0
  LDD R9,  Y+1
  LDD R10, Y+2
  LDD R11, Y+3
  LDD R12, Y+4
  LDD R13, Y+5

  MUL R2, R10 ;a0 * b2
  MOVW R16, R0
  MUL R2, R8 ;a0 * b0
  MOVW R14, R0
  MUL R2, R9 ;a0 * b1
  ADD R15, R0
  ADC R16, R1
  ADC R17, R25
  MUL R2, R12 ;a0 * b4
  MOVW R18, R0
  MUL R2, R11 ;a0 * b3
  ADD R17, R0
  ADC R18, R1
  ADC R19, R25
  MUL R2, R13 ;a0 * b5
  ADD R19, R0
  ADC R20, R1

  MUL R3, R10 ;a1 * b2
  MOVW R26, R0
  MUL R3, R8 ;a1 * b0
  ADD R15, R0
  ADC R16, R1
  ADC R17, R26
  ADC R27, R25
  MUL R3, R9 ;a1 * b1
  ADD R16, R0
  ADC R17, R1
  ADC R27, R25
  MUL R3, R12 ;a1 * b4
  ADD R18, R27
  ADC R19, R0
  ADC R20, R1
  ADC R21, R25
  MUL R3, R11 ;a1 * b3
  MOVW R26, R0
  MUL R3, R13 ;a1 * b5
  ADD R18, R26
  ADC R19, R27
  ADC R20, R0
  ADC R21, R1

  MUL R4, R10 ;a2 * b2
  MOVW R26, R0
  MUL R4, R8 ;a2 * b0
  ADD R16, R0
  ADC R17, R1
  ADC R18, R26
  ADC R27, R25
  MUL R4, R9 ;a2 * b1
  ADD R17, R0
  ADC R18, R1
  ADC R27, R25
  MUL R4, R12 ;a2 * b4
  ADD R19, R27
  ADC R20, R0
  ADC R21, R1
  ADC R22, R25
  MUL R4, R11 ;a2 * b3
  MOVW R26, R0
  MUL R4, R13 ;a2 * b5
  ADD R19, R26
  ADC R20, R27
  ADC R21, R0
  ADC R22, R1
  STD Z+0, R14
  STD Z+1, R15
  STD Z+2, R16

  MUL R5, R10 ;a3 * b2
  MOVW R14, R0
  MUL R5, R8 ;a3 * b0
  ADD R17, R0
  ADC R18, R1
  ADC R19, R14
  ADC R15, R25
  MUL R5, R9 ;a3 * b1
  ADD R18, R0
  ADC R19, R1
  ADC R15, R25
  MUL R5, R12 ;a3 * b4
  ADD R20, R15
  ADC R21, R0
  ADC R22, R1
  ADC R23, R25
  MUL R5, R11 ;a3 * b3
  MOVW R14, R0
  MUL R5, R13 ;a3 * b5
  ADD R20, R14
  ADC R21, R15
  ADC R22, R0
  ADC R23, R1

  MUL R6, R10 ;a4 * b2
  MOVW R14, R0
  MUL R6, R8 ;a4 * b0
  ADD R18, R0
  ADC R19, R1
  ADC R20, R14
  ADC R15, R25
  MUL R6, R9 ;a4 * b1
  ADD R19, R0
  ADC R20, R1
  ADC R15, R25
  MUL R6, R12 ;a4 * b4
  ADD R21, R15
  ADC R22, R0
  ADC R23, R1
  ADC R24, R25
  MUL R6, R11 ;a4 * b3
  MOVW R14, R0
  MUL R6, R13 ;a4 * b5
  ADD R21, R14
  ADC R22, R15
  ADC R23, R0
  ADC R24, R1

  MUL R7, R10 ;a5 * b2
  MOVW R14, R0
  MUL R7, R8 ;a5 * b0
  ADD R19, R0
  ADC R20, R1
  ADC R21, R14
  ADC R15, R25
  MUL R7, R9 ;a5 * b1
  ADD R20, R0
  ADC R21, R1
  ADC R15, R25
  MUL R7, R12 ;a5 * b4
  ADD R22, R15
  ADC R23, R0
  ADC R24, R1
  ADC R25, R25
  MUL R7, R11 ;a5 * b3
  MOVW R14, R0
  MUL R7, R13 ;a5 * b5
  ADD R22, R14
  ADC R23, R15
  ADC R24, R0
  ADC R25, R1
  STD Z+3, R17
  STD Z+4, R18
  STD Z+5, R19

  ;--- load a6..a11 and b6..b11 ---
  LDD R14, Y+18
  LDD R15, Y+19
  LDD R16, Y+20
  LDD R17, Y+21
  LDD R18, Y+22
  LDD R19, Y+23
  LDD R8,  Y+6
  LDD R9,  Y+7
  LDD R10, Y+8
  LDD R11, Y+9
  LDD R12, Y+10
  LDD R13, Y+11
  
  ;--- Compute H + (l6,l7,l8,l9,l10,l11) ---
  CLR R30
  CLR R31
  MOVW R2, R30
  MOVW R4, R30
  MOVW R6, R30

  MUL R8, R14
  ADD R20, R0   
  ADC R21, R1   
  ADC R22, R31
  ADC R6, R31  

  MUL R8, R15
  ADD R21, R0
  ADC R22, R1
  ADC R6, R31
  MUL R9, R14
  ADD R21, R0
  ADC R22, R1
  ADC R23, R6
  ADC R7, R31

  CLR R6
  MUL R8, R16
  ADD R22, R0 
  ADC R23, R1
  ADC R7, R31  
  MUL R9, R15
  ADD R22, R0
  ADC R23, R1
  ADC R7, R31
  MUL R10, R14
  ADD R22, R0
  ADC R23, R1
  ADC R24, R7
  ADC R6, R31

  CLR R7
  MUL R8, R17
  ADD R23, R0
  ADC R24, R1
  ADC R6, R31
  MUL R9, R16
  ADD R23, R0
  ADC R24, R1
  ADC R6, R31
  MUL R10, R15
  ADD R23, R0
  ADC R24, R1
  ADC R6, R31
  MUL R11, R14
  ADD R23, R0
  ADC R24, R1
  ADC R25, R6
  ADC R7, R31  

  CLR R6
  MUL R8, R18
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R9, R17
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R10, R16
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R11, R15
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31
  MUL R12, R14
  ADD R24, R0
  ADC R25, R1
  ADC R7, R31

  CLR R6
  MUL R8, R19
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R9, R18
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R10, R17
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R11, R16
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R12, R15
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31
  MUL R13, R14
  ADD R25, R0
  ADC R7, R1
  ADC R6, R31

  MUL R15, R13
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R16, R12
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R17, R11
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R18, R10
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31
  MUL R19, R9
  ADD R7, R0
  ADC R6, R1
  ADC R2, R31

  MUL R16, R13
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31
  MUL R17, R12
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31
  MUL R18, R11
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31
  MUL R19, R10
  ADD R6, R0
  ADC R2, R1
  ADC R3, R31

  MUL R17, R13
  ADD R2, R0
  ADC R3, R1
  ADC R4, R31
  MUL R18, R12
  ADD R2, R0
  ADC R3, R1
  ADC R4, R31
  MUL R19, R11
  ADD R2, R0
  ADC R3, R1
  ADC R4, R31

  MUL R18, R13
  ADD R3, R0
  ADC R4, R1
  ADC R5, R31
  MUL R19, R12
  ADD R3, R0
  ADC R4, R1
  ADC R5, R31

  MUL R19, R13
  ADD R4, R0
  ADC R5, R1

  ; push h6 and h7 on stack
  PUSH R6
  PUSH R7
  
  ;--- subtract a0-a5 ---
  LDD R0, Y+12
  SUB R14, R0
  LDD R0, Y+13
  SBC R15, R0
  LDD R0, Y+14
  SBC R16, R0
  LDD R0, Y+15
  SBC R17, R0
  LDD R0, Y+16
  SBC R18, R0
  LDD R0, Y+17
  SBC R19, R0
  ; 0xff if carry and 0x00 if no carry
  SBC R0, R0

  ;--- subtract b0-b5 ---
  LDD R1, Y+0
  SUB R8, R1
  LDD R1, Y+1
  SBC R9, R1
  LDD R1, Y+2
  SBC R10, R1
  LDD R1, Y+3
  SBC R11, R1
  LDD R1, Y+4
  SBC R12, R1
  LDD R1, Y+5
  SBC R13, R1
  ; 0xff if carry and 0x00 if no carry
  SBC R1, R1

  ;--- absolute values ---    
  EOR R14, R0
  EOR R15, R0
  EOR R16, R0
  EOR R17, R0
  EOR R18, R0
  EOR R19, R0
  EOR R8,  R1
  EOR R9,  R1
  EOR R10, R1
  EOR R11, R1
  EOR R12, R1
  EOR R13, R1
  NEG R0
  NEG R1
  ADD R14, R0
  ADC R15, R31
  ADC R16, R31
  ADC R17, R31
  ADC R18, R31
  ADC R19, R31
  ADD R8,  R1
  ADC R9,  R31
  ADC R10, R31
  ADC R11, R31
  ADC R12, R31
  ADC R13, R31
  EOR R0, R1
  BST R0, 0   
  
  ;--- Compute M ---
  MOVW R26, R30
  MOVW R28, R30
  
  MUL R14, R8
  MOVW R6, R0
  
  MUL R14, R9
  ADD R7, R0
  ADC R26, R1
  MUL R15, R8
  ADD R7, R0
  ADC R26, R1
  ADC R27, R31
  
  MUL R14, R10
  ADD R26, R0
  ADC R27, R1
  ADC R28, R31
  MUL R15, R9
  ADD R26, R0
  ADC R27, R1
  ADC R28, R31
  MUL R16, R8
  ADD R26, R0
  ADC R27, R1
  ADC R28, R31
  
  MUL R14, R11
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  MUL R15, R10
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  MUL R16, R9
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31
  MUL R17, R8
  ADD R27, R0
  ADC R28, R1
  ADC R29, R31

  MUL R14, R12
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R15, R11
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R16, R10
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R17, R9
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31
  MUL R18, R8
  ADD R28, R0
  ADC R29, R1
  ADC R30, R31

  MUL R14, R13
  CLR R14
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R15, R12
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R16, R11
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R17, R10
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R18, R9
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14
  MUL R19, R8
  ADD R29, R0
  ADC R30, R1
  ADC R31, R14

  CLR R8
  MUL R15, R13
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R16, R12
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R17, R11
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R18, R10
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14
  MUL R19, R9
  ADD R30, R0
  ADC R31, R1
  ADC R8, R14

  CLR R9
  MUL R16, R13
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  MUL R17, R12
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  MUL R18, R11
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14
  MUL R19, R10
  ADD R31, R0
  ADC R8, R1
  ADC R9, R14

  CLR R10
  MUL R17, R13
  ADD R8, R0
  ADC R9, R1
  ADC R10, R14
  MUL R18, R12
  ADD R8, R0
  ADC R9, R1
  ADC R10, R14
  MUL R19, R11
  ADD R8, R0
  ADC R9, R1
  ADC R10, R14

  CLR R11
  MUL R18, R13
  ADD R9, R0
  ADC R10, R1
  ADC R11, R14
  MUL R19, R12
  ADD R9, R0
  ADC R10, R1
  ADC R11, R14

  MUL R19, R13
  ADD R10, R0
  ADC R11, R1

  MOVW R12, R30
  
  POP R0
  POP R1

  ; now load stack pointer in R31:30
  IN R30, _SFR_IO_ADDR(SPL)
  IN R31, _SFR_IO_ADDR(SPH) 
  SBIW R30, 7

  ;--- add l5+h0 to l0 and h5 ---
  LDD R14, Z+0
  LDD R15, Z+1
  LDD R16, Z+2
  LDD R17, Z+3
  LDD R18, Z+4
  LDD R19, Z+5
  POP R31  ;load Z register
  POP R30
  ADD R14, R20
  ADC R15, R21
  ADC R16, R22
  ADC R17, R23
  ADC R18, R24
  ADC R19, R25
  ADC R20, R0
  ADC R21, R1
  ADC R22, R2
  ADC R23, R3
  ADC R24, R4
  ADC R25, R5
  ; store carry in T register
      
  ;--- process sign bit ---  
  BRTS add_M_M
  ROL R5
  BST R5, 0
  ROR R5
  
  ;subtract M
  SUB R14, R6
  SBC R15, R7
  SBC R16, R26
  SBC R17, R27
  SBC R18, R28
  SBC R19, R29
  SBC R20, R12
  SBC R21, R13
  SBC R22, R8
  SBC R23, R9
  SBC R24, R10
  SBC R25, R11
  SBC R6, R6
  SBC R7, R7
  ; R7:R6 is -1,0, or 1
  RJMP final_M

add_M_M: 
  ROL R5
  BST R5, 0
  ROR R5
  
  ADD R14, R6
  ADC R15, R7
  ADC R16, R26
  ADC R17, R27
  ADC R18, R28
  ADC R19, R29
  ADC R20, R12
  ADC R21, R13
  ADC R22, R8
  ADC R23, R9
  ADC R24, R10
  ADC R25, R11
  CLR R6
  CLR R7
  ADC R6, R6

final_M:
  BLD R8, 0
  ASR R8
  ;--- propagate carry to end ---
  ADC R0, R6
  ADC R1, R7
  ADC R2, R7
  ADC R3, R7
  ADC R4, R7
  ADC R5, R7

  ; now load stack pointer in R29:28
  IN R28, _SFR_IO_ADDR(SPL)
  IN R29, _SFR_IO_ADDR(SPH) 
  SBIW R28, 9

  ;------ level 1: combine L, H, and M ------

  ; load m0..m5 in 8,9,10,11,12,13
  LDD R8,  Y+0
  LDD R9,  Y+1
  LDD R10, Y+2
  LDD R11, Y+3
  LDD R12, Y+4
  LDD R13, Y+5
  ; M is stored in: 8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,0,1,2,3,4,5

  ;--- process sign bit ---  
  POP R7
  TST R7
  
  BREQ final_subtract
  RJMP final_addition

final_subtract:  
  LDD R6, Z+12
  SUB R6, R8
  STD Z+12, R6
  LDD R6, Z+13
  SBC R6, R9
  STD Z+13, R6
  LDD R6, Z+14
  SBC R6, R10
  STD Z+14, R6
  LDD R6, Z+15
  SBC R6, R11
  STD Z+15, R6
  LDD R6, Z+16
  SBC R6, R12
  STD Z+16, R6
  LDD R6, Z+17
  SBC R6, R13
  STD Z+17, R6
  LDD R6, Z+18
  SBC R6, R14
  STD Z+18, R6
  LDD R6, Z+19
  SBC R6, R15
  STD Z+19, R6
  LDD R6, Z+20
  SBC R6, R16
  STD Z+20, R6
  LDD R6, Z+21
  SBC R6, R17
  STD Z+21, R6
  LDD R6, Z+22
  SBC R6, R18
  STD Z+22, R6
  LDD R6, Z+23
  SBC R6, R19
  STD Z+23, R6
  LDD R6, Z+24
  SBC R6, R20
  STD Z+24, R6
  LDD R6, Z+25
  SBC R6, R21
  STD Z+25, R6
  LDD R6, Z+26
  SBC R6, R22
  STD Z+26, R6
  LDD R6, Z+27
  SBC R6, R23
  STD Z+27, R6
  LDD R6, Z+28
  SBC R6, R24
  STD Z+28, R6
  LDD R6, Z+29
  SBC R6, R25
  STD Z+29, R6
  LDD R6, Z+30
  SBC R6, R0
  STD Z+30, R6

  LDD R6, Z+31
  SBC R6, R1
  STD Z+31, R6
  LDD R6, Z+32
  SBC R6, R2
  STD Z+32, R6
  LDD R6, Z+33
  SBC R6, R3
  STD Z+33, R6
  LDD R6, Z+34
  SBC R6, R4
  STD Z+34, R6
  LDD R6, Z+35
  SBC R6, R5
  STD Z+35, R6
  SBC R6, R6
  SBC R7, R7
  ; R7:R6 is -1,0, or 1  
  RJMP final

final_addition:
  LDD R6, Z+12
  ADD R6, R8
  STD Z+12, R6
  LDD R6, Z+13
  ADC R6, R9
  STD Z+13, R6
  LDD R6, Z+14
  ADC R6, R10
  STD Z+14, R6
  LDD R6, Z+15
  ADC R6, R11
  STD Z+15, R6
  LDD R6, Z+16
  ADC R6, R12
  STD Z+16, R6
  LDD R6, Z+17
  ADC R6, R13
  STD Z+17, R6
  LDD R6, Z+18
  ADC R6, R14
  STD Z+18, R6
  LDD R6, Z+19
  ADC R6, R15
  STD Z+19, R6
  LDD R6, Z+20
  ADC R6, R16
  STD Z+20, R6
  LDD R6, Z+21
  ADC R6, R17
  STD Z+21, R6
  LDD R6, Z+22
  ADC R6, R18
  STD Z+22, R6
  LDD R6, Z+23
  ADC R6, R19
  STD Z+23, R6
  LDD R6, Z+24
  ADC R6, R20
  STD Z+24, R6
  LDD R6, Z+25
  ADC R6, R21
  STD Z+25, R6
  LDD R6, Z+26
  ADC R6, R22
  STD Z+26, R6
  LDD R6, Z+27
  ADC R6, R23
  STD Z+27, R6
  LDD R6, Z+28
  ADC R6, R24
  STD Z+28, R6
  LDD R6, Z+29
  ADC R6, R25
  STD Z+29, R6
  LDD R6, Z+30
  ADC R6, R0
  STD Z+30, R6
  LDD R6, Z+31
  ADC R6, R1
  STD Z+31, R6
  LDD R6, Z+32
  ADC R6, R2
  STD Z+32, R6
  LDD R6, Z+33
  ADC R6, R3
  STD Z+33, R6
  LDD R6, Z+34
  ADC R6, R4
  STD Z+34, R6
  LDD R6, Z+35
  ADC R6, R5
  STD Z+35, R6
  CLR R6
  CLR R7
  ADC R6, R6

final:
  ; load carry from stack
  LDD R26, Y+35
  ADIW R28, 35
  OUT _SFR_IO_ADDR(SPL), R28
  OUT _SFR_IO_ADDR(SPH), R29
  CLR R0
  ADD R6, R26
  ADC R7, R0

  ;--- propagate carry to end ---
  LDD R12, Z+36
  ADD R12, R6
  STD Z+36, R12
  LDD R12, Z+37
  ADC R12, R7
  STD Z+37, R12
  LDD R12, Z+38
  ADC R12, R7
  STD Z+38, R12
  LDD R12, Z+39
  ADC R12, R7
  STD Z+39, R12
  LDD R12, Z+40
  ADC R12, R7
  STD Z+40, R12
  LDD R12, Z+41
  ADC R12, R7
  STD Z+41, R12
  LDD R12, Z+42
  ADC R12, R7
  STD Z+42, R12
  LDD R12, Z+43
  ADC R12, R7
  STD Z+43, R12
  LDD R12, Z+44
  ADC R12, R7
  STD Z+44, R12
  LDD R12, Z+45
  ADC R12, R7
  STD Z+45, R12
  LDD R12, Z+46
  ADC R12, R7
  STD Z+46, R12
  LDD R12, Z+47
  ADC R12, R7
  STD Z+47, R12

  CLR R1
  POP R29
  POP R28
  POP R17
  POP R16
  POP R15
  POP R14
  POP R13
  POP R12
  POP R11
  POP R10
  POP R9
  POP R8
  POP R7
  POP R6
  POP R5
  POP R4
  POP R3
  POP R2
  RET
