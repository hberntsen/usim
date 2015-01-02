; Authors: Michael Hutter and Peter Schwabe
; Version: 2014-07-25
; Public domain

#include <avr/io.h>

  .global karatsuba160_branchfree
  .type karatsuba160_branchfree, @function

; operand b: r21:r20
; operand a: r23:r22
; operand r: r25:r24

karatsuba160_branchfree:

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
  CLR R18
  CLR R19
  MOVW R20, R18

  ;--- level 2: compute L ---
  LD R2, X+
  LD R3, X+
  LD R4, X+
  LD R5, X+
  LD R6, X+
  LDD R7, Y+0
  LDD R8, Y+1
  LDD R9, Y+2
  LDD R10, Y+3
  LDD R11, Y+4

  MUL R2, R9
  MOVW R14, R0
  MUL R2, R7
  MOVW R12, R0
  MUL R2, R8
  ADD R13, R0
  ADC R14, R1
  ADC R15, R21
  MUL R2, R11
  MOVW R16, R0
  MUL R2, R10
  ADD R15, R0
  ADC R16, R1
  ADC R17, R21

  MUL R3, R9
  MOVW R24, R0
  MUL R3, R7
  ADD R13, R0
  ADC R14, R1
  ADC R15, R24
  ADC R25, R21
  MUL R3, R8
  ADD R14, R0
  ADC R15, R1
  ADC R25, R21
  MUL R3, R11
  ADD R16, R25
  ADC R17, R0
  ADC R18, R1
  MUL R3, R10
  ADD R16, R0
  ADC R17, R1
  ADC R18, R21

  MUL R4, R9
  MOVW R24, R0
  MUL R4, R7
  ADD R14, R0
  ADC R15, R1
  ADC R16, R24
  ADC R25, R21
  MUL R4, R8
  ADD R15, R0
  ADC R16, R1
  ADC R25, R21
  MUL R4, R11
  ADD R17, R25
  ADC R18, R0
  ADC R19, R1
  MUL R4, R10
  ADD R17, R0
  ADC R18, R1
  ADC R19, R21

  MUL R5, R9
  MOVW R24, R0
  MUL R5, R7
  ADD R15, R0
  ADC R16, R1
  ADC R17, R24
  ADC R25, R21
  MUL R5, R8
  ADD R16, R0
  ADC R17, R1
  ADC R25, R21
  MUL R5, R11
  ADD R18, R25
  ADC R19, R0
  ADC R20, R1
  MUL R5, R10
  ADD R18, R0
  ADC R19, R1
  ADC R20, R21

  MUL R6, R9
  MOVW R24, R0
  MUL R6, R7
  ADD R16, R0
  ADC R17, R1
  ADC R18, R24
  ADC R25, R21
  MUL R6, R8
  ADD R17, R0
  ADC R18, R1
  ADC R25, R21
  MUL R6, R10
  ADD R19, R0
  ADC R20, R1
  ADC R21, R21

  MUL R6, R11
  ADD R19, R25
  ADC R20, R0
  ADC R21, R1
  STD Z+0, R12
  STD Z+1, R13
  STD Z+2, R14
  STD Z+3, R15
  STD Z+4, R16

  ;--- load a5..a9 and b5..b9 ---
  LD R12, X+
  LD R13, X+
  LD R14, X+
  LD R15, X+
  LD R16, X+

  PUSH R26
  PUSH R27
  PUSH R28
  PUSH R29

  LDD R23, Y+5
  LDD R24, Y+6
  LDD R25, Y+7
  LDD R26, Y+8
  LDD R27, Y+9

  ;--- subtract a0-a5 ---
  SUB R2, R12
  SBC R3, R13
  SBC R4, R14
  SBC R5, R15
  SBC R6, R16
  ; 0xff if carry and 0x00 if no carry
  SBC R0, R0
  
  ;--- subtract b0-b5 ---
  SUB R7, R23
  SBC R8, R24
  SBC R9, R25
  SBC R10, R26
  SBC R11, R27
  ; 0xff if carry and 0x00 if no carry
  SBC R1, R1

  ;--- absolute values ---    
  EOR R2, R0
  EOR R3, R0
  EOR R4, R0
  EOR R5, R0
  EOR R6, R0
  EOR R7, R1
  EOR R8, R1
  EOR R9, R1
  EOR R10, R1
  EOR R11, R1
  NEG R0
  NEG R1
  CLR R22
  ADD R2, R0
  ADC R3, R22
  ADC R4, R22
  ADC R5, R22
  ADC R6, R22
  ADD R7, R1
  ADC R8, R22
  ADC R9, R22
  ADC R10, R22
  ADC R11, R22  
  EOR R0, R1
  BST R0, 0   

  ;--- Compute H + (l5,l6,l7,l8,l9) ---
  MUL R23, R14
  MOVW R28, R0
  MUL R23, R12
  ADD R17, R0 
  ADC R18, R1
  ADC R19, R28
  ADC R29, R22
  MUL R23, R13
  ADD R18, R0
  ADC R19, R1
  ADC R29, R22
  MUL R23, R15
  ADD R20, R0
  ADC R21, R1
  ADC R22, R22
  MUL R23, R16
  ADD R20, R29
  ADC R21, R0
  ADC R22, R1

  CLR R23
  MUL R24, R14
  MOVW R28, R0
  MUL R24, R12
  ADD R18, R0
  ADC R19, R1
  ADC R20, R28
  ADC R29, R23
  MUL R24, R13
  ADD R19, R0
  ADC R20, R1
  ADC R29, R23
  MUL R24, R15
  ADD R21, R0
  ADC R22, R1
  ADC R23, R23
  MUL R24, R16
  ADD R21, R29
  ADC R22, R0
  ADC R23, R1
  
  CLR R24
  MUL R25, R14
  MOVW R28, R0
  MUL R25, R12
  ADD R19, R0
  ADC R20, R1
  ADC R21, R28
  ADC R29, R24
  MUL R25, R13
  ADD R20, R0
  ADC R21, R1
  ADC R29, R24
  MUL R25, R15
  ADD R22, R0
  ADC R23, R1
  ADC R24, R24
  MUL R25, R16
  ADD R22, R29
  ADC R23, R0
  ADC R24, R1
  
  CLR R25
  MUL R26, R14
  MOVW R28, R0
  MUL R26, R12
  ADD R20, R0
  ADC R21, R1
  ADC R22, R28
  ADC R29, R25
  MUL R26, R13
  ADD R21, R0
  ADC R22, R1
  ADC R29, R25
  MUL R26, R15
  ADD R23, R0
  ADC R24, R1
  ADC R25, R25
  MUL R26, R16
  ADD R23, R29
  ADC R24, R0
  ADC R25, R1
  
  CLR R26
  MUL R27, R14
  MOVW R28, R0
  MUL R27, R12
  ADD R21, R0
  ADC R22, R1
  ADC R23, R28
  ADC R29, R26
  MUL R27, R13
  ADD R22, R0
  ADC R23, R1
  ADC R29, R26
  MUL R27, R15
  ADD R24, R0
  ADC R25, R1
  ADC R26, R26
  MUL R27, R16
  ADD R24, R29
  ADC R25, R0
  ADC R26, R1

  ;--- Compute M ---  
  CLR R27
  MUL R2, R9
  MOVW R14, R0
  MUL R2, R7
  MOVW R12, R0
  MUL R2, R8
  ADD R13, R0
  ADC R14, R1
  ADC R15, R27
  MUL R2, R11
  MOV R16, R0
  MOV R27, R1
  MUL R2, R10
  ADD R15, R0
  ADC R16, R1
  CLR R2
  ADC R27, R2
    
  MUL R3, R9
  MOVW R28, R0
  MUL R3, R7
  ADD R13, R0
  ADC R14, R1
  ADC R15, R28
  ADC R29, R2
  MUL R3, R8
  ADD R14, R0
  ADC R15, R1
  ADC R29, R2
  MUL R3, R10
  ADD R16, R0
  ADC R27, R1
  ADC R2, R2
  MUL R3, R11
  ADD R16, R29
  ADC R27, R0
  ADC R2, R1
  
  CLR R3
  MUL R4, R9
  MOVW R28, R0
  MUL R4, R7
  ADD R14, R0
  ADC R15, R1
  ADC R16, R28
  ADC R29, R3
  MUL R4, R8
  ADD R15, R0
  ADC R16, R1
  ADC R29, R3
  MUL R4, R10
  ADD R27, R0
  ADC R2, R1
  ADC R3, R3
  MUL R4, R11
  ADD R27, R29
  ADC R2, R0
  ADC R3, R1

  CLR R4
  MUL R5, R9
  MOVW R28, R0
  MUL R5, R7
  ADD R15, R0
  ADC R16, R1
  ADC R27, R28
  ADC R29, R4
  MUL R5, R8
  ADD R16, R0
  ADC R27, R1
  ADC R29, R4
  MUL R5, R10
  ADD R2, R0
  ADC R3, R1
  ADC R4, R4
  MUL R5, R11
  ADD R2, R29
  ADC R3, R0
  ADC R4, R1
  
  CLR R5
  MUL R6, R9
  MOVW R28, R0
  MUL R6, R7
  ADD R16, R0
  ADC R27, R1
  ADC R2, R28
  ADC R29, R5
  MUL R6, R8
  ADD R27, R0
  ADC R2, R1
  ADC R29, R5
  MUL R6, R10
  ADD R3, R0
  ADC R4, R1
  ADC R5, R5
  MUL R6, R11
  ADD R3, R29
  ADC R4, R0
  ADC R5, R1
   
  ;--- add l5+h0 to h5 etc. ---
  LDD R7, Z+0
  LDD R8, Z+1
  LDD R9, Z+2
  LDD R10, Z+3
  LDD R11, Z+4
  
  ADD R7, R17
  ADC R8, R18
  ADC R9, R19
  ADC R10, R20
  ADC R11, R21
  ADC R17, R22
  ADC R18, R23
  ADC R19, R24
  ADC R20, R25
  ADC R21, R26
  ; store carry in R28
  
  ;--- process sign bit ---  
  CLR R28
  CLR R29
  BLD R29, 0
  DEC R29

  ; merge carry and borrow
  ADC R28, R29
  MOV R0, R28
  ASR R0

  ; invert all bits or do nothing
  EOR R12, R29
  EOR R13, R29
  EOR R14, R29
  EOR R15, R29
  EOR R16, R29
  EOR R27, R29
  EOR R2, R29
  EOR R3, R29
  EOR R4, R29
  EOR R5, R29
  ADD R29, R29 ; sets carry flag if R29 = 0xff

  ; add in M
  ADC R7, R12
  ADC R8, R13
  ADC R9, R14
  ADC R10, R15
  ADC R11, R16
  ADC R17, R27
  ADC R18, R2
  ADC R19, R3
  ADC R20, R4
  ADC R21, R5

  ; propagate carry/borrow
  ADC R22, R28
  ADC R23, R0
  ADC R24, R0
  ADC R25, R0
  ADC R26, R0

  STD Z+5, R7
  STD Z+6, R8
  STD Z+7, R9
  STD Z+8, R10
  STD Z+9, R11




  ; push h9 on stack
  PUSH R26

  IN R28, _SFR_IO_ADDR(SPL)
  IN R29, _SFR_IO_ADDR(SPH)
  LDD R1, Y+2
  LDD R0, Y+3
  LDD R27, Y+4
  LDD R26, Y+5
  MOVW R28, R0
  
  ; push h5,h6,h7 on stack
  PUSH R19
  PUSH R18
  PUSH R17

  ;------ level 1: compute H ------

  ; init zero registers
  CLR R14
  CLR R15
  MOVW R16, R14
  MOVW R18, R14

  ;--- level 2: compute L ---
  LD R2, X+
  LD R3, X+
  LD R4, X+
  LD R5, X+
  LD R6, X+
  LDD R7, Y+10
  LDD R8, Y+11
  LDD R9, Y+12
  LDD R10, Y+13
  LDD R11, Y+14
 
  MUL R2, R7
  MOVW R12, R0
 

  MUL R2, R8
  ADD R13, R0
  ADC R14, R1
  MUL R3, R7
  ADD R13, R0
  ADC R14, R1
  ADC R15, R19

  MUL R2, R9
  ADD R14, R0
  ADC R15, R1
  ADC R16, R19
  MUL R3, R8
  ADD R14, R0
  ADC R15, R1
  ADC R16, R19
  MUL R4, R7
  ADD R14, R0
  ADC R15, R1
  ADC R16, R19

  MUL R2, R10
  ADD R15, R0
  ADC R16, R1
  ADC R17, R19
  MUL R3, R9
  ADD R15, R0
  ADC R16, R1
  ADC R17, R19
  MUL R4, R8
  ADD R15, R0
  ADC R16, R1
  ADC R17, R19
  MUL R5, R7
  ADD R15, R0
  ADC R16, R1
  ADC R17, R19

  MUL R2, R11
  ADD R16, R0
  ADC R17, R1
  ADC R18, R19
  MUL R3, R10
  ADD R16, R0
  ADC R17, R1
  ADC R18, R19
  MUL R4, R9
  ADD R16, R0
  ADC R17, R1
  ADC R18, R19
  MUL R5, R8
  ADD R16, R0
  ADC R17, R1
  ADC R18, R19
  MUL R6, R7
  ADD R16, R0
  ADC R17, R1
  ADC R18, R19

  ;--- now add h0+l10 and h0+l14 ---
  POP R0
  ADD R0, R12
  STD Z+20, R0
  POP R0
  ADC R0, R13
  STD Z+21, R0
  POP R0
  ADC R0, R14
  STD Z+22, R0
  ADC R20, R15
  ADC R21, R16
  STD Z+23, R20
  STD Z+24, R21
  ADC R12, R22
  ADC R13, R23
  ADC R14, R24
  ADC R15, R25
  POP R0
  ADC R16, R0
  STD Z+15, R12
  STD Z+16, R13
  STD Z+17, R14
  STD Z+18, R15
  STD Z+19, R16
  ; push carry on stack
  SBC R0, R0
  PUSH R0

  ;--- continue ---
  CLR R20
  CLR R21
  CLR R22
  MUL R3, R11
  ADD R17, R0
  ADC R18, R1
  ADC R19, R22
  MUL R4, R10
  ADD R17, R0
  ADC R18, R1
  ADC R19, R22
  MUL R5, R9
  ADD R17, R0
  ADC R18, R1
  ADC R19, R22
  MUL R6, R8
  ADD R17, R0
  ADC R18, R1
  ADC R19, R22
  
  MUL R4, R11
  ADD R18, R0
  ADC R19, R1
  ADC R20, R22
  MUL R5, R10
  ADD R18, R0
  ADC R19, R1
  ADC R20, R22
  MUL R6, R9
  ADD R18, R0
  ADC R19, R1
  ADC R20, R22
  
  MUL R5, R11
  ADD R19, R0
  ADC R20, R1
  ADC R21, R22
  MUL R6, R10
  ADD R19, R0
  ADC R20, R1
  ADC R21, R22
  
  MUL R6, R11
  ADD R20, R0
  ADC R21, R1
    
  ;--- load a5..a9 and b5..b9 ---
  LD R12, X+
  LD R13, X+
  LD R14, X+
  LD R15, X+
  LD R16, X+
  LDD R23, Y+15
  LDD R24, Y+16
  LDD R25, Y+17
  LDD R26, Y+18
  LDD R27, Y+19

  ;--- subtract a0-a5 ---
  SUB R2, R12
  SBC R3, R13
  SBC R4, R14
  SBC R5, R15
  SBC R6, R16
  ; 0xff if carry and 0x00 if no carry
  SBC R0, R0
  
  ;--- subtract b0-b5 ---
  SUB R7, R23
  SBC R8, R24
  SBC R9, R25
  SBC R10, R26
  SBC R11, R27
  ; 0xff if carry and 0x00 if no carry
  SBC R1, R1

  ;--- absolute values ---    
  EOR R2, R0
  EOR R3, R0
  EOR R4, R0
  EOR R5, R0
  EOR R6, R0
  EOR R7, R1
  EOR R8, R1
  EOR R9, R1
  EOR R10, R1
  EOR R11, R1
  NEG R0
  NEG R1
  ADD R2, R0
  ADC R3, R22
  ADC R4, R22
  ADC R5, R22
  ADC R6, R22
  ADD R7, R1
  ADC R8, R22
  ADC R9, R22
  ADC R10, R22
  ADC R11, R22  
  EOR R0, R1
  BST R0, 0   

  ;--- Compute H + (l5,l6,l7,l8,l9) ---
  MUL R23, R14
  MOVW R28, R0
  MUL R23, R12
  ADD R17, R0 
  ADC R18, R1
  ADC R19, R28
  ADC R29, R22
  MUL R23, R13
  ADD R18, R0
  ADC R19, R1
  ADC R29, R22
  MUL R23, R15
  ADD R20, R0
  ADC R21, R1
  ADC R22, R22
  MUL R23, R16
  ADD R20, R29
  ADC R21, R0
  ADC R22, R1

  CLR R23
  MUL R24, R14
  MOVW R28, R0
  MUL R24, R12
  ADD R18, R0
  ADC R19, R1
  ADC R20, R28
  ADC R29, R23
  MUL R24, R13
  ADD R19, R0
  ADC R20, R1
  ADC R29, R23
  MUL R24, R15
  ADD R21, R0
  ADC R22, R1
  ADC R23, R23
  MUL R24, R16
  ADD R21, R29
  ADC R22, R0
  ADC R23, R1
  
  CLR R24
  MUL R25, R14
  MOVW R28, R0
  MUL R25, R12
  ADD R19, R0
  ADC R20, R1
  ADC R21, R28
  ADC R29, R24
  MUL R25, R13
  ADD R20, R0
  ADC R21, R1
  ADC R29, R24
  MUL R25, R15
  ADD R22, R0
  ADC R23, R1
  ADC R24, R24
  MUL R25, R16
  ADD R22, R29
  ADC R23, R0
  ADC R24, R1
  
  CLR R25
  MUL R26, R14
  MOVW R28, R0
  MUL R26, R12
  ADD R20, R0
  ADC R21, R1
  ADC R22, R28
  ADC R29, R25
  MUL R26, R13
  ADD R21, R0
  ADC R22, R1
  ADC R29, R25
  MUL R26, R15
  ADD R23, R0
  ADC R24, R1
  ADC R25, R25
  MUL R26, R16
  ADD R23, R29
  ADC R24, R0
  ADC R25, R1
  
  CLR R26
  MUL R27, R14
  MOVW R28, R0
  MUL R27, R12
  ADD R21, R0
  ADC R22, R1
  ADC R23, R28
  ADC R29, R26
  MUL R27, R13
  ADD R22, R0
  ADC R23, R1
  ADC R29, R26
  MUL R27, R15
  ADD R24, R0
  ADC R25, R1
  ADC R26, R26
  MUL R27, R16
  ADD R24, R29
  ADC R25, R0
  ADC R26, R1

  ;--- Compute M ---  
  CLR R27
  MUL R2, R9
  MOVW R14, R0
  MUL R2, R7
  MOVW R12, R0
  MUL R2, R8
  ADD R13, R0
  ADC R14, R1
  ADC R15, R27
  MUL R2, R11
  MOV R16, R0
  MOV R27, R1
  MUL R2, R10
  ADD R15, R0
  ADC R16, R1
  CLR R2
  ADC R27, R2
    
  MUL R3, R9
  MOVW R28, R0
  MUL R3, R7
  ADD R13, R0
  ADC R14, R1
  ADC R15, R28
  ADC R29, R2
  MUL R3, R8
  ADD R14, R0
  ADC R15, R1
  ADC R29, R2
  MUL R3, R10
  ADD R16, R0
  ADC R27, R1
  ADC R2, R2
  MUL R3, R11
  ADD R16, R29
  ADC R27, R0
  ADC R2, R1
  
  CLR R3
  MUL R4, R9
  MOVW R28, R0
  MUL R4, R7
  ADD R14, R0
  ADC R15, R1
  ADC R16, R28
  ADC R29, R3
  MUL R4, R8
  ADD R15, R0
  ADC R16, R1
  ADC R29, R3
  MUL R4, R10
  ADD R27, R0
  ADC R2, R1
  ADC R3, R3
  MUL R4, R11
  ADD R27, R29
  ADC R2, R0
  ADC R3, R1

  CLR R4
  MUL R5, R9
  MOVW R28, R0
  MUL R5, R7
  ADD R15, R0
  ADC R16, R1
  ADC R27, R28
  ADC R29, R4
  MUL R5, R8
  ADD R16, R0
  ADC R27, R1
  ADC R29, R4
  MUL R5, R10
  ADD R2, R0
  ADC R3, R1
  ADC R4, R4
  MUL R5, R11
  ADD R2, R29
  ADC R3, R0
  ADC R4, R1
  
  CLR R5
  MUL R6, R9
  MOVW R28, R0
  MUL R6, R7
  ADD R16, R0
  ADC R27, R1
  ADC R2, R28
  ADC R29, R5
  MUL R6, R8
  ADD R27, R0
  ADC R2, R1
  ADC R29, R5
  MUL R6, R10
  ADD R3, R0
  ADC R4, R1
  ADC R5, R5
  MUL R6, R11
  ADD R3, R29
  ADC R4, R0
  ADC R5, R1
   
  ;--- load h0 and add it ---
  LDD R7, Z+15
  LDD R8, Z+16
  LDD R9, Z+17
  LDD R10, Z+18
  LDD R11, Z+19
  
  ADD R7, R17
  ADC R8, R18
  ADC R9, R19
  ADC R10, R20
  ADC R11, R21
  ADC R17, R22
  ADC R18, R23
  ADC R19, R24
  ADC R20, R25
  ADC R21, R26
  ; store carry in R28
  CLR R28
  ADC R28, R28

  ;--- propagate carry ---
  POP R0
  NEG R0
  LSR R0
  ADC R17, R0
  ADC R18, R0
  ADC R19, R0
  ADC R20, R0
  ADC R21, R0
  ; store carry in R28
  
  ;--- process sign bit ---    
  CLR R29
  BLD R29, 0
  DEC R29

  ; merge carry and borrow
  ADC R28, R29
  MOV R0, R28
  ASR R0

  ; invert all bits or do nothing
  EOR R12, R29
  EOR R13, R29
  EOR R14, R29
  EOR R15, R29
  EOR R16, R29
  EOR R27, R29
  EOR R2,  R29
  EOR R3,  R29
  EOR R4,  R29
  EOR R5,  R29
  ADD R29, R29 ; sets carry flag if R29 = 0xff

  ; add in M
  ADC R7, R12
  ADC R8, R13
  ADC R9, R14
  ADC R10, R15
  ADC R11, R16
  ADC R17, R27
  ADC R18, R2
  ADC R19, R3
  ADC R20, R4
  ADC R21, R5

  ; propagate carry/borrow
  ADC R22, R28
  ADC R23, R0
  ADC R24, R0
  ADC R25, R0
  ADC R26, R0

  STD Z+25, R7
  STD Z+26, R8
  STD Z+27, R9
  STD Z+28, R10
  STD Z+29, R11
  STD Z+30, R17
  STD Z+31, R18
  STD Z+32, R19
  STD Z+33, R20
  STD Z+34, R21
  STD Z+35, R22
  STD Z+36, R23
  STD Z+37, R24
  STD Z+38, R25
  STD Z+39, R26

  POP R29
  POP R28
  POP R27
  POP R26

  ;------ level 1: prepare stack-pointer address ------
  ; store address of result on stack
  PUSH R30
  PUSH R31
  ; use Z as stack-pointer base address
  IN R30, _SFR_IO_ADDR(SPL)
  IN R31, _SFR_IO_ADDR(SPH)
  SBIW R30, 16

  ;------ level 1: subtract a0-a7 ------
  SBIW R26, 10
  LD R2, X+
  LD R3, X+
  LD R4, X+
  LD R5, X+
  LD R6, X+
  LD R22, X+
  LD R23, X+
  LD R24, X+
  LD R25, X+
  LD R16, X+
  LD R0, X+
  SUB R2, R0
  LD R0, X+
  SBC R3, R0
  LD R0, X+
  SBC R4, R0
  LD R0, X+
  SBC R5, R0
  LD R0, X+
  SBC R6, R0
  LD R0, X+
  SBC R22, R0
  LD R0, X+
  SBC R23, R0
  LD R0, X+
  SBC R24, R0
  LD R0, X+
  SBC R25, R0
  LD R0, X+
  SBC R16, R0
  ; 0xff if carry and 0x00 if no carry
  SBC R0, R0

  ;------ level 1: absolute values ------
  EOR R2, R0
  EOR R3, R0
  EOR R4, R0
  EOR R5, R0
  EOR R6, R0
  EOR R22, R0
  EOR R23, R0
  EOR R24, R0
  EOR R25, R0
  EOR R16, R0
  NEG R0
  CLR R18
  ADD R2, R0
  ADC R3, R18
  ADC R4, R18
  ADC R5, R18
  ADC R6, R18
  ADC R22, R18
  ADC R23, R18
  ADC R24, R18
  ADC R15, R18
  ADC R16, R18
  STD Z+0, R12
  STD Z+1, R13
  STD Z+2, R14
  STD Z+3, R15
  STD Z+4, R16

  ;------ level 1: subtract b0-b7 ------
  LDD R7, Y+0
  LDD R8, Y+1
  LDD R9, Y+2
  LDD R10, Y+3
  LDD R11, Y+4
  LDD R12, Y+5
  LDD R13, Y+6
  LDD R14, Y+7
  LDD R15, Y+8
  LDD R16, Y+9
  LDD R1, Y+10
  SUB R7, R1
  LDD R1, Y+11
  SBC R8, R1
  LDD R1, Y+12
  SBC R9, R1
  LDD R1, Y+13
  SBC R10, R1
  LDD R1, Y+14
  SBC R11, R1
  LDD R1, Y+15
  SBC R12, R1
  LDD R1, Y+16
  SBC R13, R1
  LDD R1, Y+17
  SBC R14, R1
  LDD R1, Y+18
  SBC R15, R1
  LDD R1, Y+19
  SBC R16, R1
  ; 0xff if carry and 0x00 if no carry
  SBC R1, R1

  ;------ level 1: absolute values ------
  EOR R7, R1
  EOR R8, R1
  EOR R9, R1
  EOR R10, R1
  EOR R11, R1
  EOR R12, R1
  EOR R13, R1
  EOR R14, R1
  EOR R15, R1
  EOR R16, R1
  NEG R1
  ADD R7, R1
  ADC R8, R18
  ADC R9, R18
  ADC R10, R18
  ADC R11, R18
  ADC R12, R18
  ADC R13, R18
  ADC R14, R18
  ADC R15, R18
  ADC R16, R18
  STD Z+5, R12
  STD Z+6, R13
  STD Z+7, R14
  STD Z+8, R15
  STD Z+9, R16
  EOR R0, R1
  PUSH R0

  ;------ level 1: compute M ------
  CLR R19

  MUL R2, R9
  MOVW R14, R0
  MUL R2, R7
  MOVW R12, R0
  MUL R2, R8
  ADD R13, R0
  ADC R14, R1
  ADC R15, R19
  MUL R2, R11
  MOVW R16, R0
  MUL R2, R10
  ADD R15, R0
  ADC R16, R1
  ADC R17, R19

  MUL R3, R9
  MOVW R20, R0
  MUL R3, R7
  ADD R13, R0
  ADC R14, R1
  ADC R15, R20
  ADC R21, R19
  MUL R3, R8
  ADD R14, R0
  ADC R15, R1
  ADC R21, R19
  MUL R3, R11
  ADD R16, R21
  ADC R17, R0
  ADC R18, R1
  MUL R3, R10
  ADD R16, R0
  ADC R17, R1
  ADC R18, R19

  MUL R4, R9
  MOVW R20, R0
  MUL R4, R7
  ADD R14, R0
  ADC R15, R1
  ADC R16, R20
  ADC R21, R19
  MUL R4, R8
  ADD R15, R0
  ADC R16, R1
  ADC R21, R19
  MUL R4, R10
  ADD R17, R0
  ADC R18, R1
  ADC R19, R19
  MUL R4, R11
  ADD R17, R21
  ADC R18, R0
  ADC R19, R1  
  STD Z+10, R12
  STD Z+11, R13
  STD Z+12, R14
  MOVW R12, R22
  MOV R14, R24
  CLR R20
  CLR R21

  MUL R5, R9
  MOVW R22, R0
  MUL R5, R7
  ADD R15, R0
  ADC R16, R1
  ADC R17, R22
  ADC R23, R21
  MUL R5, R8
  ADD R16, R0
  ADC R17, R1
  ADC R23, R21
  MUL R5, R11
  ADD R18, R23
  ADC R19, R0
  ADC R20, R1
  MUL R5, R10
  ADD R18, R0
  ADC R19, R1
  ADC R20, R21

  MUL R6, R9
  MOVW R22, R0
  MUL R6, R7
  ADD R16, R0
  ADC R17, R1
  ADC R18, R22
  ADC R23, R21
  MUL R6, R8
  ADD R17, R0
  ADC R18, R1
  ADC R23, R21
  MUL R6, R10
  ADD R19, R0
  ADC R20, R1
  ADC R21, R21
  MUL R6, R11
  ADD R19, R23
  ADC R20, R0
  ADC R21, R1 
  STD Z+13, R15
  STD Z+14, R16
  MOV R15, R25

  ;--- load missing operands ---
  LDD R16, Z+4
  LDD R23, Z+5
  LDD R24, Z+6
  LDD R25, Z+7
  LDD R26, Z+8
  LDD R27, Z+9

  ;--- subtract a0-a5 ---
  SUB R2, R12
  SBC R3, R13
  SBC R4, R14
  SBC R5, R15
  SBC R6, R16
  ; 0xff if carry and 0x00 if no carry
  SBC R0, R0
  
  ;--- subtract b0-b5 ---
  SUB R7, R23
  SBC R8, R24
  SBC R9, R25
  SBC R10, R26
  SBC R11, R27
  ; 0xff if carry and 0x00 if no carry
  SBC R1, R1

  ;--- absolute values ---    
  EOR R2, R0
  EOR R3, R0
  EOR R4, R0
  EOR R5, R0
  EOR R6, R0  
  EOR R7, R1
  EOR R8, R1
  EOR R9, R1
  EOR R10, R1
  EOR R11, R1
  NEG R0
  NEG R1
  PUSH R30
  CLR R30  
  ADD R2, R0
  ADC R3, R30
  ADC R4, R30
  ADC R5, R30
  ADC R6, R30
  ADD R7, R1
  ADC R8, R30
  ADC R9, R30
  ADC R10, R30
  ADC R11, R30
  EOR R0, R1
  BST R0, 0   

  ;--- Compute H + (l5,l6,l7,l8,l9) ---
  MUL R23, R14
  MOVW R28, R0
  MUL R23, R12
  ADD R17, R0 
  ADC R18, R1
  ADC R19, R28
  ADC R29, R30
  MUL R23, R13
  ADD R18, R0
  ADC R19, R1
  ADC R29, R30
  MUL R23, R16
  ADD R20, R29
  ADC R21, R0
  CLR R22
  ADC R22, R1
  MUL R23, R15
  ADD R20, R0
  ADC R21, R1
  ADC R22, R30

  MUL R24, R14
  MOVW R28, R0
  MUL R24, R12
  ADD R18, R0
  ADC R19, R1
  ADC R20, R28
  ADC R29, R30
  MUL R24, R13
  ADD R19, R0
  ADC R20, R1
  ADC R29, R30
  MUL R24, R16
  ADD R21, R29
  ADC R22, R0
  CLR R23
  ADC R23, R1
  MUL R24, R15
  ADD R21, R0
  ADC R22, R1
  ADC R23, R30

  MUL R25, R14
  MOVW R28, R0
  MUL R25, R12
  ADD R19, R0
  ADC R20, R1
  ADC R21, R28
  ADC R29, R30
  MUL R25, R13
  ADD R20, R0
  ADC R21, R1
  ADC R29, R30
  MUL R25, R16
  ADD R22, R29
  ADC R23, R0
  CLR R24
  ADC R24, R1
  MUL R25, R15
  ADD R22, R0
  ADC R23, R1
  ADC R24, R30

  MUL R26, R14
  MOVW R28, R0
  MUL R26, R12
  ADD R20, R0
  ADC R21, R1
  ADC R22, R28
  ADC R29, R30
  MUL R26, R13
  ADD R21, R0
  ADC R22, R1
  ADC R29, R30
  MUL R26, R16
  ADD R23, R29
  ADC R24, R0
  CLR R25
  ADC R25, R1  
  MUL R26, R15
  ADD R23, R0
  ADC R24, R1
  ADC R25, R30

  MUL R27, R14
  MOVW R28, R0
  MUL R27, R12
  ADD R21, R0
  ADC R22, R1
  ADC R23, R28
  ADC R29, R30
  MUL R27, R13
  ADD R22, R0
  ADC R23, R1
  ADC R29, R30
  MUL R27, R16
  ADD R24, R29
  ADC R25, R0
  CLR R26
  ADC R26, R1
  MUL R27, R15
  ADD R24, R0
  ADC R25, R1
  ADC R26, R30

  ;--- Compute M ---    
  MUL R2, R9
  MOVW R14, R0
  MUL R2, R7
  MOVW R12, R0
  MUL R2, R8
  ADD R13, R0
  ADC R14, R1
  ADC R15, R30
  MUL R2, R11
  MOV R16, R0
  CLR R27
  MOV R27, R1
  MUL R2, R10
  ADD R15, R0
  ADC R16, R1
  ADC R27, R30
  
  MUL R3, R9
  MOVW R28, R0
  MUL R3, R7
  ADD R13, R0
  ADC R14, R1
  ADC R15, R28
  ADC R29, R30
  MUL R3, R8
  ADD R14, R0
  ADC R15, R1
  ADC R29, R30
  MUL R3, R11
  ADD R16, R29
  ADC R27, R0
  CLR R2
  ADC R2, R1
  MUL R3, R10
  ADD R16, R0
  ADC R27, R1
  ADC R2, R30

  MUL R4, R9
  MOVW R28, R0
  MUL R4, R7
  ADD R14, R0
  ADC R15, R1
  ADC R16, R28
  ADC R29, R30
  MUL R4, R8
  ADD R15, R0
  ADC R16, R1
  ADC R29, R30
  MUL R4, R11
  ADD R27, R29
  ADC R2, R0
  CLR R3
  ADC R3, R1  
  MUL R4, R10
  ADD R27, R0
  ADC R2, R1
  ADC R3, R30

  MUL R5, R9
  MOVW R28, R0
  MUL R5, R7
  ADD R15, R0
  ADC R16, R1
  ADC R27, R28
  ADC R29, R30
  MUL R5, R8
  ADD R16, R0
  ADC R27, R1
  ADC R29, R30
  MUL R5, R11
  ADD R2, R29
  ADC R3, R0
  CLR R4
  ADC R4, R1
  MUL R5, R10
  ADD R2, R0
  ADC R3, R1
  ADC R4, R30

  MUL R6, R9
  MOVW R28, R0
  MUL R6, R7
  ADD R16, R0
  ADC R27, R1
  ADC R2, R28
  ADC R29, R30
  MUL R6, R8
  ADD R27, R0
  ADC R2, R1
  ADC R29, R30
  MUL R6, R11
  ADD R3, R29
  ADC R4, R0
  CLR R5
  ADC R5, R1
  MUL R6, R10
  ADD R3, R0
  ADC R4, R1
  ADC R5, R30
  
  ;--- add l5+h0 to h5 etc. ---
  POP R30
  LDD R7, Z+10
  LDD R8, Z+11
  LDD R9, Z+12
  LDD R10, Z+13
  LDD R11, Z+14
  ADD R7, R17
  ADC R8, R18
  ADC R9, R19
  ADC R10, R20
  ADC R11, R21
  ADC R17, R22
  ADC R18, R23
  ADC R19, R24
  ADC R20, R25
  ADC R21, R26
  ; store carry in R28
  
  ;--- process sign bit ---    
  CLR R28
  CLR R29
  BLD R29, 0
  DEC R29

  ; merge carry and borrow
  ADC R28, R29
  MOV R0, R28
  ASR R0

  ; invert all bits or do nothing
  EOR R12, R29
  EOR R13, R29
  EOR R14, R29
  EOR R15, R29
  EOR R16, R29
  EOR R27, R29
  EOR R2,  R29
  EOR R3,  R29
  EOR R4,  R29
  EOR R5,  R29
  ADD R29, R29 ; sets carry flag if R29 = 0xff

  ; add in M
  ADC R7, R12
  ADC R8, R13
  ADC R9, R14
  ADC R10, R15
  ADC R11, R16
  ADC R17, R27
  ADC R18, R2
  ADC R19, R3
  ADC R20, R4
  ADC R21, R5

  ; propagate carry/borrow
  ADC R22, R28
  ADC R23, R0
  ADC R24, R0
  ADC R25, R0
  ADC R26, R0
  
  LDD R2, Z+10
  LDD R3, Z+11
  LDD R4, Z+12
  LDD R5, Z+13
  LDD R6, Z+14

  ; M is stored in: 2,3,4,5,6,7,8,9,10,11,17,18,19,20,21,22,23,24,25,26

  ;------ level 1: combine L, H, and M ------
  POP R0 
  POP R31
  POP R30
  PUSH R24
  PUSH R25

  LDD R24, Z+0
  LDD R25, Z+1
  LDD R12, Z+2
  LDD R13, Z+3
  LDD R14, Z+4
  LDD R15, Z+5
  LDD R16, Z+6
  LDD R27, Z+7
  LDD R28, Z+8
  LDD R29, Z+9

  ;--- process sign bit ---  
  DEC R0

  ; merge carry and borrow
  MOV R1, R0
  ASR R1

  ; invert all bits or do nothing
  EOR R2, R0
  EOR R3, R0
  EOR R4, R0
  EOR R5, R0
  EOR R6, R0
  EOR R7, R0
  EOR R8, R0
  EOR R9, R0
  EOR R10, R0
  EOR R11, R0
  ADD R0, R0 ; sets carry flag if R0 = 0xff
  
  ; add in M 
  ADC R24, R2
  ADC R25, R3
  ADC R12, R4
  ADC R13, R5
  ADC R14, R6
  ADC R15, R7
  ADC R16, R8
  ADC R27, R9
  ADC R28, R10
  ADC R29, R11
  ; store carry in T register
  SBC R2, R2
  BST R2, 0
  
  LDD R2, Z+20  
  LDD R3, Z+21
  ADD R24, R2
  ADC R25, R3
  STD Z+10, R24
  STD Z+11, R25
  LDD R24, Z+22 
  LDD R25, Z+23
  ADC R12, R24
  ADC R13, R25
  STD Z+12, R12
  STD Z+13, R13
  MOVW R12, R24
  LDD R24, Z+24
  LDD R25, Z+25
  ADC R14, R24
  ADC R15, R25
  STD Z+14, R14
  STD Z+15, R15
  MOVW R14, R24
  LDD R24, Z+26
  LDD R25, Z+27
  ADC R16, R24
  ADC R27, R25
  STD Z+16, R16
  STD Z+17, R27
  MOV R16, R24
  MOV R27, R25
  LDD R24, Z+28
  LDD R25, Z+29
  ADC R28, R24
  ADC R29, R25
  STD Z+18, R28
  STD Z+19, R29
  MOV R28, R24
  MOV R29, R25
  ; store carry in R24
  SBC R24, R24
  
  POP R10
  POP R9

  ; merge carry and borrow
  ASR R0

  ; invert all bits or do nothing
  EOR R17, R0
  EOR R18, R0
  EOR R19, R0
  EOR R20, R0
  EOR R21, R0
  EOR R22, R0
  EOR R23, R0
  EOR R9, R0
  EOR R10, R0
  EOR R26, R0
  BLD R25, 0 ; sets carry flag if R25 = 0xff
  LSR R25

  ; add in M 
  ADC R2, R17
  ADC R3, R18
  ADC R12, R19
  ADC R13, R20
  ADC R14, R21
  ADC R15, R22
  ADC R16, R23
  ADC R27, R9
  ADC R28, R10
  ADC R29, R26
  ; store carry in R1:R0
  BST R24,0
  CLR R25
  ADC R0, R25
  ADC R1, R25

  LDD R24, Z+30
  LDD R25, Z+31
  LDD R4, Z+32
  LDD R5, Z+33
  LDD R6, Z+34
  LDD R7, Z+35
  LDD R8, Z+36
  LDD R9, Z+37
  LDD R10, Z+38
  LDD R11, Z+39

  BLD R19, 0
  LSR R19
  ADC R2, R24
  ADC R3, R25
  ADC R12, R4
  ADC R13, R5
  ADC R14, R6
  ADC R15, R7
  ADC R16, R8
  ADC R27, R9
  ADC R28, R10
  ADC R29, R11
  ; store borrow in R25:R24
  CLR R19
  ADC R24, R19
  ADC R25, R19

  STD Z+20, R2
  STD Z+21, R3
  STD Z+22, R12
  STD Z+23, R13
  STD Z+24, R14
  STD Z+25, R15
  STD Z+26, R16
  STD Z+27, R27
  STD Z+28, R28
  STD Z+29, R29

  ;--- propagate carry to end ---
  ADD R24, R0
  ADC R25, R1
  ADC R4, R1
  ADC R5, R1
  ADC R6, R1
  ADC R7, R1
  ADC R8, R1
  ADC R9, R1
  ADC R10, R1
  ADC R11, R1

  STD Z+30, R24
  STD Z+31, R25
  STD Z+32, R4
  STD Z+33, R5
  STD Z+34, R6
  STD Z+35, R7
  STD Z+36, R8
  STD Z+37, R9
  STD Z+38, R10
  STD Z+39, R11
  
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
