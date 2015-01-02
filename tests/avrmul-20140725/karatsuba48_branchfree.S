; Authors: Michael Hutter and Peter Schwabe
; Version: 2014-07-25
; Public domain

.global karatsuba48_branchfree
  .type karatsuba48_branchfree, @function

; operand b: r21:r20
; operand a: r23:r22
; operand r: r25:r24

karatsuba48_branchfree:

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
  CLR R22
  CLR R23
  MOVW R12, R22
  MOVW R20, R22
  
  ;--- Compute L ---
  LD R2, X+
  LD R3, X+
  LD R4, X+
  LDD R5, Y+0
  LDD R6, Y+1
  LDD R7, Y+2
  
  MUL R2, R7 ;a0*b2
  MOVW R10, R0
  MUL R2, R5 ;a0*b0
  MOVW R8, R0
  MUL R2, R6 ;a0*b1
  ADD R9, R0
  ADC R10, R1
  ADC R11, R23

  MUL R3, R7 ;a1*b2
  MOVW R14, R0
  MUL R3, R5 ;a1*b0
  ADD R9, R0
  ADC R10, R1
  ADC R11, R14
  ADC R15, R23
  MUL R3, R6 ;a1*b1
  ADD R10, R0
  ADC R11, R1
  ADC R12, R15
  
  MUL R4, R7 ;a2*b2
  MOVW R14, R0
  MUL R4, R5 ;a2*b0
  ADD R10, R0
  ADC R11, R1
  ADC R12, R14
  ADC R15, R23
  MUL R4, R6 ;a2*b1
  ADD R11, R0
  ADC R12, R1
  ADC R13, R15
  STD Z+0, R8
  STD Z+1, R9
  STD Z+2, R10

  ;--- load a3..a5 and b3..b5 ---
  LD R14, X+
  LD R15, X+
  LD R16, X+
  LDD R17, Y+3
  LDD R18, Y+4
  LDD R19, Y+5
  
  ;--- subtract a0-a3 ---
  SUB R2, R14
  SBC R3, R15
  SBC R4, R16
  ; 0xff if carry and 0x00 if no carry
  SBC R26, R26
  
  ;--- subtract b0-b3 ---
  SUB R5, R17
  SBC R6, R18
  SBC R7, R19
  ; 0xff if carry and 0x00 if no carry
  SBC R27, R27

  ;--- absolute values ---
  EOR R2, R26
  EOR R3, R26
  EOR R4, R26
  EOR R5, R27
  EOR R6, R27
  EOR R7, R27
  NEG R26
  NEG R27
  ADD R2, R26
  ADC R3, R23
  ADC R4, R23
  ADD R5, R27
  ADC R6, R23
  ADC R7, R23
  
  ;--- Compute H + (l3,l4,l5) ---
  MUL R14, R19 ;a0*b2
  MOVW R24, R0
  MUL R14, R17 ;a0*b0
  ADD R11, R0
  ADC R12, R1
  ADC R13, R24
  ADC R25, R23
  MUL R14, R18 ;a0*b1
  ADD R12, R0
  ADC R13, R1
  ADC R20, R25

  MUL R15, R19 ;a1*b2
  MOVW R24, R0
  MUL R15, R17 ;a1*b0
  ADD R12, R0
  ADC R13, R1
  ADC R20, R24
  ADC R25, R23
  MUL R15, R18 ;a1*b1
  ADD R13, R0
  ADC R20, R1
  ADC R21, R25
  
  MUL R16, R19 ;a2*b2
  MOVW R24, R0
  MUL R16, R17 ;a2*b0
  ADD R13, R0
  ADC R20, R1
  ADC R21, R24
  ADC R25, R23
  MUL R16, R18 ;a2*b1
  MOVW R18,R22
  ADD R20, R0
  ADC R21, R1
  ADC R22, R25
  
  ;--- Compute M ---
  MUL R2, R7 ;a0*b2
  MOVW R16, R0
  MUL R2, R5 ;a0*b0
  MOVW R14, R0
  MUL R2, R6 ;a0*b1
  ADD R15, R0
  ADC R16, R1
  ADC R17, R23

  MUL R3, R7 ;a1*b2
  MOVW R24, R0
  MUL R3, R5 ;a1*b0
  ADD R15, R0
  ADC R16, R1
  ADC R17, R24
  ADC R25, R23
  MUL R3, R6 ;a1*b1
  ADD R16, R0
  ADC R17, R1
  ADC R18, R25
  
  MUL R4, R7 ;a2*b2
  MOVW R24, R0
  MUL R4, R5 ;a2*b0
  ADD R16, R0
  ADC R17, R1
  ADC R18, R24
  ADC R25, R23
  MUL R4, R6 ;a2*b1
  ADD R17, R0
  ADC R18, R1
  ADC R19, R25

  ;--- add l3+h0 to l0 and h3 ---
  ADD R8, R11
  ADC R9, R12
  ADC R10, R13  
  ADC R11, R20
  ADC R12, R21
  ADC R13, R22
  ; store carry in R23
  
  ;--- process sign bit ---
  EOR R26, R27
  DEC R26

  ; merge carry and borrow
  ADC R23, R26
  MOV R0, R23
  ASR R0

  ; invert all bits or do nothing
  EOR R14, R26
  EOR R15, R26
  EOR R16, R26
  EOR R17, R26
  EOR R18, R26
  EOR R19, R26
  ADD R26, R26 ; sets carry if R26 = 0xff

  ; add in M
  ADC  R8, R14   
  ADC  R9, R15
  ADC R10, R16
  ADC R11, R17
  ADC R12, R18
  ADC R13, R19

  ; propagate carry/borrow
  ADC R20, R23
  ADC R21, R0
  ADC R22, R0
  
  STD Z+3, R8
  STD Z+4, R9
  STD Z+5, R10
  STD Z+6, R11
  STD Z+7, R12
  STD Z+8, R13
  STD Z+9, R20
  STD Z+10, R21
  STD Z+11, R22

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
