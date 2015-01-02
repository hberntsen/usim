; Authors: Michael Hutter and Peter Schwabe
; Version: 2014-07-25
; Public domain

.global karatsuba64_branched
  .type karatsuba64_branched, @function

; operand b: r21:r20
; operand a: r23:r22
; operand r: r25:r24

karatsuba64_branched:

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
  CLR R20
  CLR R21
  MOVW R16, R20
  
  ;--- Compute L ---
  LD R2, X+
  LD R3, X+
  LD R4, X+
  LD R5, X+
  LDD R6, Y+0
  LDD R7, Y+1
  LDD R8, Y+2
  LDD R9, Y+3

  MUL R2, R8 ;a0*b2
  MOVW R12, R0
  MUL R2, R6 ;a0*b0
  MOVW R10, R0
  MUL R2, R7 ;a0*b1
  ADD R11, R0
  ADC R12, R1
  ADC R13, R21
  MUL R3, R9 ;a1*b3
  MOVW R14, R0

  MUL R2, R9 ;a0*b3
  MOVW R18, R0
  MUL R3, R6 ;a1*b0
  ADD R11, R0
  ADC R12, R1
  ADC R13, R18
  ADC R19, R21
  MUL R3, R7 ;a1*b1
  ADD R12, R0
  ADC R13, R1
  ADC R19, R21
  MUL R4, R9 ;a2*b3
  ADD R14, R19
  ADC R15, R0
  ADC R16, R1

  MUL R4, R8 ;a2*b2
  MOVW R18, R0
  MUL R4, R6 ;a2*b0
  ADD R12, R0
  ADC R13, R1
  ADC R14, R18
  ADC R19, R21
  MUL R3, R8 ;a1*b2
  ADD R13, R0
  ADC R14, R1
  ADC R19, R21
  MUL R5, R9 ;a3*b3
  ADD R15, R19
  ADC R16, R0
  ADC R17, R1

  MUL R5, R7 ;a3*b1
  MOVW R18, R0
  MUL R4, R7 ;a2*b1
  ADD R13, R0
  ADC R18, R1
  ADC R19, R21
  MUL R5, R6 ;a3*b0
  ADD R13, R0
  ADC R18, R1
  ADC R19, R21
  MUL R5, R8 ;a3*b2
  ADD R14, R18
  ADC R0, R19
  ADC R1, R21
  ADD R15, R0
  ADC R16, R1
  ADC R17, R21
  STD Z+0, R10
  STD Z+1, R11
  STD Z+2, R12
  STD Z+3, R13
  
  ;--- load a4..a7 and b4..b7 ---
  LDD R22, Y+4
  LDD R23, Y+5
  LDD R24, Y+6
  LDD R25, Y+7
  MOVW R28, R20
  LD R18, X+
  LD R19, X+
  LD R20, X+
  LD R21, X+ 
  MOVW R26, R28

  ;--- subtract a0-a4 ---  
  SUB R2, R18
  SBC R3, R19
  SBC R4, R20
  SBC R5, R21
  ; 0xff if carry and 0x00 if no carry
  SBC R0, R0
  
  ;--- subtract b0-b4 ---
  SUB R6, R22
  SBC R7, R23
  SBC R8, R24
  SBC R9, R25
  ; 0xff if carry and 0x00 if no carry
  SBC R1, R1

  ;--- absolute values ---
  EOR R2, R0
  EOR R3, R0
  EOR R4, R0
  EOR R5, R0
  EOR R6, R1
  EOR R7, R1
  EOR R8, R1
  EOR R9, R1
  NEG R0
  NEG R1
  ADD R2, R0
  ADC R3, R26
  ADC R4, R26
  ADC R5, R26
  ADD R6, R1
  ADC R7, R26
  ADC R8, R26
  ADC R9, R26
  EOR R0, R1
  BST R0, 0 

  ;--- Compute H + (l4,l5,l6,l7) ---
  MUL R18, R22
  ADD R14, R0
  ADC R15, R1
  ADC R16, R26
  ADC R29, R26  

  MUL R18, R23
  ADD R15, R0
  ADC R16, R1
  ADC R29, R26
  MUL R19, R22
  ADD R15, R0
  ADC R16, R1
  ADC R17, R29
  ADC R28, R26

  MUL R18, R24
  ADD R16, R0 
  ADC R17, R1
  ADC R28, R26
  MUL R19, R23
  ADD R16, R0
  ADC R17, R1
  ADC R28, R26
  MUL R20, R22
  ADD R16, R0
  ADC R17, R1
  ADC R28, R26

  CLR R29
  MUL R18, R25
  ADD R17, R0
  ADC R28, R1
  ADC R29, R26
  MUL R19, R24
  ADD R17, R0
  ADC R28, R1
  ADC R29, R26
  MUL R20, R23
  ADD R17, R0
  ADC R28, R1
  ADC R29, R26
  MUL R21, R22
  ADD R17, R0
  ADC R28, R1
  ADC R29, R26

  MUL R19, R25
  MOVW R18, R26
  ADD R28, R0
  ADC R29, R1
  ADC R18, R26
  MUL R20, R24
  ADD R28, R0
  ADC R29, R1
  ADC R18, R26
  MUL R21, R23
  ADD R28, R0
  ADC R29, R1
  ADC R18, R26

  MUL R20, R25
  ADD R29, R0
  ADC R18, R1
  ADC R19, R26
  MUL R21, R24
  ADD R29, R0
  ADC R18, R1
  ADC R19, R26

  MUL R21, R25
  ADD R18, R0
  ADC R19, R1

  ;--- Compute M ---
  MUL R2, R6
  MOVW R20, R0

  MOVW R22, R26
  MUL R2, R7
  ADD R21, R0
  ADC R22, R1
  MUL R3, R6
  ADD R21, R0
  ADC R22, R1
  ADC R23, R26

  MOVW R24, R26
  MUL R2, R8
  ADD R22, R0
  ADC R23, R1
  ADC R24, R26
  MUL R3, R7
  ADD R22, R0
  ADC R23, R1
  ADC R24, R26
  MUL R4, R6
  ADD R22, R0
  ADC R23, R1
  ADC R24, R26

  MUL R2, R9
  ADD R23, R0
  ADC R24, R1
  ADC R25, R26
  MUL R3, R8
  ADD R23, R0
  ADC R24, R1
  ADC R25, R26
  MUL R4, R7
  ADD R23, R0
  ADC R24, R1
  ADC R25, R26
  MUL R5, R6
  ADD R23, R0
  ADC R24, R1
  ADC R25, R26

  MUL R3, R9
  MOVW R2, R26
  ADD R24, R0
  ADC R25, R1
  ADC R2, R27
  MUL R4, R8
  ADD R24, R0
  ADC R25, R1
  ADC R2, R27
  MUL R5, R7
  ADD R24, R0
  ADC R25, R1
  ADC R2, R27

  MUL R4, R9
  ADD R25, R0
  ADC R2, R1
  ADC R3, R27
  MUL R5, R8
  ADD R25, R0
  ADC R2, R1
  ADC R3, R27

  MUL R5, R9
  ADD R2, R0
  ADC R3, R1

  ;--- add l4+h0 to l0 and h4 ---
  ADD R10, R14
  ADC R11, R15
  ADC R12, R16
  ADC R13, R17  
  ADC R14, R28
  ADC R15, R29
  ADC R16, R18
  ADC R17, R19
  ; store carry in R26
  ADC R26, R26
  
  ;--- process sign bit ---  
  BRTS add_M

  ; subtract M
  SUB R10, R20
  SBC R11, R21
  SBC R12, R22
  SBC R13, R23
  SBC R14, R24
  SBC R15, R25
  SBC R16, R2
  SBC R17, R3
  SBCI R26, 0
  SBC R27, R27 
  ; R27:R26 is -1,0, or 1
  RJMP final

add_M: 
  ADD R10, R20
  ADC R11, R21
  ADC R12, R22
  ADC R13, R23  
  ADC R14, R24
  ADC R15, R25
  ADC R16, R2
  ADC R17, R3
  CLR R27
  ADC R26, R27
  NOP ; constant time (DO NOT REMOVE!)

final:
  STD Z+4, R10
  STD Z+5, R11
  STD Z+6, R12
  STD Z+7, R13
  STD Z+8, R14
  STD Z+9, R15
  STD Z+10, R16
  STD Z+11, R17

  ;--- propagate carry to end ---
  ADD R28, R26
  ADC R29, R27
  ADC R18, R27
  ADC R19, R27
  
  STD Z+12, R28
  STD Z+13, R29
  STD Z+14, R18
  STD Z+15, R19

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