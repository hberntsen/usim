; Authors: Michael Hutter and Peter Schwabe
; Version: 2014-07-25
; Public domain

  .global karatsuba80_branched
  .type karatsuba80_branched, @function

; operand b: r21:r20
; operand a: r23:r22
; operand r: r25:r24

karatsuba80_branched:

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

  ; init zero registers
  CLR R18
  CLR R19
  MOVW R20, R18

  ;--- Compute L ---
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
   
  ;--- add l5+h0 to l0 and h5 ---
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
  CLR R28
  ADC R28, R28
  
  ;--- process sign bit ---  
  BRTS add_M

  ; subtract M
  SUB R7, R12
  SBC R8, R13
  SBC R9, R14
  SBC R10, R15
  SBC R11, R16
  SBC R17, R27
  SBC R18, R2
  SBC R19, R3
  SBC R20, R4
  SBC R21, R5
  SBCI R28, 0
  SBC R29, R29 
  ; R29:R28 is -1,0, or 1
  RJMP final

add_M: 
  ADD R7, R12
  ADC R8, R13
  ADC R9, R14
  ADC R10, R15
  ADC R11, R16
  ADC R17, R27
  ADC R18, R2
  ADC R19, R3
  ADC R20, R4
  ADC R21, R5
  CLR R29
  ADC R28, R29
  NOP ; constant time (DO NOT REMOVE!)

final:
  STD Z+5, R7
  STD Z+6, R8
  STD Z+7, R9
  STD Z+8, R10
  STD Z+9, R11
  STD Z+10, R17
  STD Z+11, R18
  STD Z+12, R19
  STD Z+13, R20
  STD Z+14, R21

  ;--- propagate carry to end ---
  ADD R22, R28
  ADC R23, R29
  ADC R24, R29
  ADC R25, R29
  ADC R26, R29

  STD Z+15, R22
  STD Z+16, R23
  STD Z+17, R24
  STD Z+18, R25
  STD Z+19, R26

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
