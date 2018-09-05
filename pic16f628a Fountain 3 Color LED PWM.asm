;*******************************************************************************
;                                                                              *
;    Microchip licenses this software to you solely for use with Microchip     *
;    products. The software is owned by Microchip and/or its licensors, and is *
;    protected under applicable copyright laws.  All rights reserved.          *
;                                                                              *
;    This software and any accompanying information is for suggestion only.    *
;    It shall not be deemed to modify Microchip?s standard warranty for its    *
;    products.  It is your responsibility to ensure that this software meets   *
;    your requirements.                                                        *
;                                                                              *
;    SOFTWARE IS PROVIDED "AS IS".  MICROCHIP AND ITS LICENSORS EXPRESSLY      *
;    DISCLAIM ANY WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING  *
;    BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS    *
;    FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL          *
;    MICROCHIP OR ITS LICENSORS BE LIABLE FOR ANY INCIDENTAL, SPECIAL,         *
;    INDIRECT OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA, HARM TO     *
;    YOUR EQUIPMENT, COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR    *
;    SERVICES, ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY   *
;    DEFENSE THEREOF), ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER      *
;    SIMILAR COSTS.                                                            *
;                                                                              *
;    To the fullest extend allowed by law, Microchip and its licensors         *
;    liability shall not exceed the amount of fee, if any, that you have paid  *
;    directly to Microchip to use this software.                               *
;                                                                              *
;    MICROCHIP PROVIDES THIS SOFTWARE CONDITIONALLY UPON YOUR ACCEPTANCE OF    *
;    THESE TERMS.                                                              *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Filename:                                                                 *
;    Date:                                                                     *
;    File Version:                                                             *
;    Author:                                                                   *
;    Company:                                                                  *
;    Description:                                                              *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Notes: In the MPLAB X Help, refer to the MPASM Assembler documentation    *
;    for information on assembly instructions.                                 *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Known Issues: This template is designed for relocatable code.  As such,   *
;    build errors such as "Directive only allowed when generating an object    *
;    file" will result when the 'Build in Absolute Mode' checkbox is selected  *
;    in the project properties.  Designing code in absolute mode is            *
;    antiquated - use relocatable mode.                                        *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Revision History:                                                         *
;                                                                              *
;*******************************************************************************



;*******************************************************************************
; Processor Inclusion
;
; TODO Step #1 Open the task list under Window > Tasks.  Include your
; device .inc file - e.g. #include <device_name>.inc.  Available
; include files are in C:\Program Files\Microchip\MPLABX\mpasmx
; assuming the default installation path for MPLAB X.  You may manually find
; the appropriate include file for your device here and include it, or
; simply copy the include generated by the configuration bits
; generator (see Step #2).
;
;*******************************************************************************

    LIST p=16F628A
    #include <p16F628A.inc>
        
;*******************************************************************************
;
; TODO Step #2 - Configuration Word Setup
;
; The 'CONFIG' directive is used to embed the configuration word within the
; .asm file. MPLAB X requires users to embed their configuration words
; into source code.  See the device datasheet for additional information
; on configuration word settings.  Device configuration bits descriptions
; are in C:\Program Files\Microchip\MPLABX\mpasmx\P<device_name>.inc
; (may change depending on your MPLAB X installation directory).
;
; MPLAB X has a feature which generates configuration bits source code.  Go to
; Window > PIC Memory Views > Configuration Bits.  Configure each field as
; needed and select 'Generate Source Code to Output'.  The resulting code which
; appears in the 'Output Window' > 'Config Bits Source' tab may be copied
; below.
;
;*******************************************************************************

; CONFIG
; __config 0x3F71
 __CONFIG _FOSC_INTOSCCLK & _WDTE_OFF & _PWRTE_ON & _MCLRE_ON & _BOREN_ON & _LVP_OFF & _CPD_OFF & _CP_OFF

;*******************************************************************************
;
; TODO Step #3 - Variable Definitions
;
; Refer to datasheet for available data memory (RAM) organization assuming
; relocatible code organization (which is an option in project
; properties > mpasm (Global Options)).  Absolute mode generally should
; be used sparingly.
;
; Example of using GPR Uninitialized Data
; GPR = General Purpose Registers 
;
;   GPR_VAR        UDATA
;   MYVAR1         RES        1      ; User variable linker places
;   MYVAR2         RES        1      ; User variable linker places
;   MYVAR3         RES        1      ; User variable linker places
;
;   ; Example of using Access Uninitialized Data Section (when available)
;   ; The variables for the context saving in the device datasheet may need
;   ; memory reserved here.
;   INT_VAR        UDATA_ACS
;   W_TEMP         RES        1      ; w register for context saving (ACCESS)
;   STATUS_TEMP    RES        1      ; status used for context saving
;   BSR_TEMP       RES        1      ; bank select used for ISR context saving
;
;*******************************************************************************

GPR_VAR	UDATA 0x20
vco_r	RES 1		;stores RED Channel VCO Value 0 to 7
vco_b	RES 1		;stores BLUE Channel VCO Value 0 to 7
vco_g	RES 1		;stores GREEN Channel VCO Value 0 to 7
slot_cntr RES 1         ;provides 2.5mS period to increment pwm slot
delay_cntr RES 1        ;provides 80 x 2.5mS = 500mS delay to change color_ptr values 
ph_cntr	RES 1		;stores phase counter 0 to 7 increments until  period 2.5mS
color_ptr RES 1		;stores pointer to internal EEPROM color lookup table addresses 0 to 55
temp1   RES 1           ;temporary value 1
temp2   RES 1           ;temporary value 2
tempB   RES 1           ;stores PWB Channels to be transferred to PORTB
                        ;bit 2=G, bit 1=R, bit 0=B
tempM   RES 1           ;temporary variable for multiplication			
tempR   RES 1           ;multiplication result
   
DATAEE    CODE  0x2100
;56 Values for Color Table
 DE  0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07
 DE  0x07,0x0e,0x15,0x1c,0x23,0x2a,0x31,0x38
 DE  0x38,0x39,0x3a,0x3b,0x3c,0x3d,0x3e,0x3f
 DE  0x3f,0x76,0xad,0xe4,0x1b,0x52,0x49,0xc0
 DE  0xc0,0xc8,0xd0,0xd8,0xe0,0xe8,0xf0,0xf8
 DE  0xf8,0xf9,0xfa,0xfb,0xfc,0xfd,0xfe,0xff
 DE  0xff,0xB6,0x6d,0x24,0xdb,0x92,0x49,0x00
;64 Values for PWM Table - ---> Column= Intensity, Line= Phase Value 0 to 7
 DE  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
 DE  0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00
 DE  0x01,0x01,0x00,0x00,0x00,0x00,0x00,0x00
 DE  0x01,0x01,0x01,0x00,0x00,0x00,0x00,0x00
 DE  0x01,0x01,0x01,0x01,0x00,0x00,0x00,0x00
 DE  0x01,0x01,0x01,0x01,0x01,0x00,0x00,0x00
 DE  0x01,0x01,0x01,0x01,0x01,0x01,0x00,0x00
 DE  0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x00
 
;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
 GOTO    START                      ; go to beginning of program

;*******************************************************************************
; TODO Step #4 - Interrupt Service Routines
;
; There are a few different ways to structure interrupt routines in the 8
; bit device families.  On PIC18's the high priority and low priority
; interrupts are located at 0x0008 and 0x0018, respectively.  On PIC16's and
; lower the interrupt is at 0x0004.  Between device families there is subtle
; variation in the both the hardware supporting the ISR (for restoring
; interrupt context) as well as the software used to restore the context
; (without corrupting the STATUS bits).
;
; General formats are shown below in relocatible format.
;
;------------------------------PIC16's and below--------------------------------
;
; ISR       CODE    0x0004           ; interrupt vector location
;
;     <Search the device datasheet for 'context' and copy interrupt
;     context saving code here.  Older devices need context saving code,
;     but newer devices like the 16F#### don't need context saving code.>
;
;     RETFIE
;
;----------------------------------PIC18's--------------------------------------
;
; ISRHV     CODE    0x0008
;     GOTO    HIGH_ISR
; ISRLV     CODE    0x0018
;     GOTO    LOW_ISR
;
; ISRH      CODE                     ; let linker place high ISR routine
; HIGH_ISR
;     <Insert High Priority ISR Here - no SW context saving>
;     RETFIE  FAST
;
; ISRL      CODE                     ; let linker place low ISR routine
; LOW_ISR
;       <Search the device datasheet for 'context' and copy interrupt
;       context saving code here>
;     RETFIE
;
;*******************************************************************************

; TODO INSERT ISR HERE

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

MAIN_PROG CODE                      ; let linker place main program

START
 ; Initialize Port B RB2 RB1 RB0 as outputs
 ; 1 = input, 0 = output 
 MOVLW b'11111000'
 ; Select Bank 1
 BCF STATUS, RP1 
 BSF STATUS, RP0
 MOVWF TRISB
 ; Select Bank 0
 BCF STATUS, RP1 
 BCF STATUS, RP0
 ; Initialize PORTB and its clone
 CLRF tempB
 CLRF PORTB
 ; Initialize color_ptr
 CLRF color_ptr
 ; Initialize vco values 
 CLRF vco_g
 CLRF vco_r
 CLRF vco_b 
 ; Initialize phase counter
 CLRF ph_cntr
 ; Initialize color_ptr delay counter with .200 .8 for debug
 ; DEBUG ONLY -----------------------------------------------
 MOVLW .200
 MOVWF delay_cntr
 
LOOP
 ;Loads PWM Values in PORTB bits 2 1 0. Value depends on phase counter ph_cntr(0..7) 
 ;and vco_g(Green), vco_r(Red) and vco_b(Blue) intensities.
 CALL VCOG
 ;Waits until slot_cntr counts to 2,5mS
 CALL WAITPWMSLOT
 ;Cycles phase counter, each slot lasts 2,5mS
 CALL NEXTPHASE
 ;Decrements delay_cntr. If not 200 (200*2,5mS=0.5s) continue with same color
 DECFSZ delay_cntr, 1
 GOTO LOOP
 ;Initialize delay_cntr with 200 again - set to 8 for debug
 ;DEBUG ONLY ------------------------------------------------------------------
 MOVLW .200
 MOVWF delay_cntr
 ;Initialize ph_cntr
 CLRF ph_cntr
 ;Increments color_ptr to next color lookup_table value
 INCF color_ptr, 1
 ;Updates Color Values as a function of color_ptr, spans from 0 to 55 (56 values)
 CALL LOAD_VCO_VAL
 ;Checks if color_ptr = 56, if is clears and goto LOOP
 MOVF color_ptr, W
 ; Decimal operand preceded by dot 
 SUBLW .56
 BTFSC STATUS, Z
 GOTO is56
 ; Is not 56 
 GOTO LOOP 
is56
 ; color_ptr = 56, resets color_ptr and loops
 CLRF color_ptr
 GOTO LOOP
 
LOAD_VCO_VAL
 ; Sets vco_g, vco_r and vco_b Values as a function of color_ptr
 ; color_ptr points to EEPROM addresses 0 to 55 (56 values)
 ; vco_g, vco_r and vco_b values are 3 bits ranging from 0 to 7
 ; EEPROM stores 2 bits for vco_g, and 3 for vco_r and vco_b total of 8 bits 
 ; vco_g bit 2 is set when color_ptr >= 28 and reset when color_ptr >= 52
 ; vco_g bits 1 and 0 corresponds to EEPROM bits 7 and 6
 ; vco_r corresponds to EEPROM bits 5, 4 and 3
 ; vco_b corresponds to EEPROM bits 2, 1 and 0 
 ; Begin by clearing vco_g
 CLRF vco_g 
 ; Checks if color_ptr > =28, if is set vco_g bit 2
 MOVF color_ptr, w
 ; Decimal operand preceded by dot 
 SUBLW .28
 BTFSC STATUS, Z
 GOTO AREEQUAL28
 BTFSC STATUS, C
 GOTO NOT28YET
 ; color_ptr > 28, sets vco_g bit 2
 BSF vco_g, 2
 GOTO LOAD_VCO_VAL1
AREEQUAL28
 ; color_ptr = 28, sets vco_g bit 2
 BSF vco_g, 2
 GOTO LOAD_VCO_VAL2
NOT28YET
 ; color_ptr < 28, clears vco_g bit 2
 BCF vco_g, 2
 GOTO LOAD_VCO_VAL2 
LOAD_VCO_VAL1
 ; Checks if color_ptr > =52, if is clear vco_g bit 2
 MOVF color_ptr, w
 ; Decimal operand preceded by dot 
 SUBLW .52
 BTFSC STATUS, Z
 GOTO AREEQUAL52
 BTFSC STATUS, C
 GOTO NOT52YET
 ; color_ptr > 52, clears vco_g bit 2
 BCF vco_g, 2
 GOTO LOAD_VCO_VAL2
AREEQUAL52
 ; color_ptr = 52, clears vco_g bit 2
 BCF vco_g, 2
 GOTO LOAD_VCO_VAL2
NOT52YET
 ; color_ptr < 52, sets vco_g bit 2
 BSF vco_g, 2
LOAD_VCO_VAL2
 ; Load W with color_ptr in Bank 0 BEFORE switching to Bank 1
 MOVF color_ptr, W 
 ; Read content from EEPROM pointed by color_ptr
 ; Select Bank 1
 BCF STATUS, RP1 
 BSF STATUS, RP0
 ; Load Pointer to EEPROM Memory
 MOVWF EEADR
 ; EE Read into W
 BSF EECON1, RD 
 MOVF EEDATA, W
 ; Select Bank 0
 BCF STATUS, RP1 
 BCF STATUS, RP0
 ; Saves W in temp1 and temp2
 MOVWF temp1
 MOVWF temp2 
 ; W Bits 2, 1 and 0 read from EEPROM are vco_b
 ANDLW 0x07
 MOVWF vco_b
 ; temp1 bits 5, 4, 3 are vco_r
 RRF temp1, 1
 RRF temp1, 1
 RRF temp1, 1
 MOVF temp1, W
 ANDLW 0x07
 MOVWF vco_r
 ; temp2 bits 7, 6 are vco_g bits 1 and 0
 RLF temp2, 1
 RLF temp2, 1
 RLF temp2, 1
 MOVF temp2, W
 ANDLW 0x03
 ADDWF vco_g, 1 
 RETURN 

VCOG
 ;Sets PORTB bits 2, 1 and 0 according to PWM Values for vco_g, vco_r and vco_b respectively.
 ; PWM values are in a lookup table in EEPROM from position 56 to (56+64-1). 
 ; Each line represents color intensity(0..7), and
 ; each column represents PWM cycle(0..7). Value is pointed by 56+(vco_x*8)+ph_cntr, 
 ; and shifted accordingly, result stored in tempB then updating PORTB at the end.
 ;clear PWM values temporary variable
 CLRF tempB
 ;Clear Multiplication result
 CLRF tempR
 ;Calculates green PWM value
 MOVF vco_g, W
 ;Load number of times to add 8
 MOVWF tempM
 ;Check if Zero
 MOVF tempM, W
 BTFSC STATUS, Z
 ;vco_g is zero, get look_up table straight
 GOTO VCOG2
VCOG1 
 MOVLW .8
 ADDWF tempR, 1
 DECFSZ tempM, 1
 GOTO VCOG1
VCOG2
 MOVLW .56 ;Endereco Inicial da Tabela
 ADDWF tempR, W ;Soma Multiplicacao
 ADDWF ph_cntr, W ;Offset
 ;W contains EEPROM Address
 ; Select Bank 1
 BCF STATUS, RP1 
 BSF STATUS, RP0
 ; Load Pointer to EEPROM Memory
 MOVWF EEADR
 ; EE Read into W
 BSF EECON1, RD 
 MOVF EEDATA, W
 ; Select Bank 0
 BCF STATUS, RP1 
 BCF STATUS, RP0
 ;W contains PWM bit value for vco_g
 ADDLW 0
 BTFSC STATUS, Z
 GOTO VCOG3
 ;Not 0, set tempB bit 2
 BSF tempB, 2
 GOTO VCOG4
VCOG3
 BCF tempB, 2
VCOG4
 ;Clear Multiplication result
 CLRF tempR
 ;Calculates Red PWM value
 MOVF vco_r, W
 ;Load number of times to add 8
 MOVWF tempM
 ;Check if Zero
 MOVF tempM, W
 BTFSC STATUS, Z
 ;vco_r is zero, get look_up table straight
 GOTO VCOG6
VCOG5 
 MOVLW .8
 ADDWF tempR, 1
 DECFSZ tempM, 1
 GOTO VCOG5
VCOG6
 MOVLW .56 ;Endereco Inicial da Tabela
 ADDWF tempR, W ;Soma Multiplicacao
 ADDWF ph_cntr, W ;Offset
 ;W contains EEPROM Address
 ; Select Bank 1
 BCF STATUS, RP1 
 BSF STATUS, RP0
 ; Load Pointer to EEPROM Memory
 MOVWF EEADR
 ; EE Read into W
 BSF EECON1, RD 
 MOVF EEDATA, W
 ; Select Bank 0
 BCF STATUS, RP1 
 BCF STATUS, RP0
 ;W contains PWM bit value for vco_b
 ADDLW 0
 BTFSC STATUS, Z
 GOTO VCOG7
 ;Not 0, set tempB bit 1
 BSF tempB, 1
 GOTO VCOG8
VCOG7
 BCF tempB, 1
VCOG8
 ;Clear Multiplication result
 CLRF tempR
 ;Calculates blue PWM value
 MOVF vco_b, W
 ;Load number of times to add 8
 MOVWF tempM
 ;Check if Zero
 MOVF tempM, W
 BTFSC STATUS, Z
 ;vco_b is zero, get look_up table straight
 GOTO VCOG10
VCOG9
 MOVLW .8
 ADDWF tempR, 1
 DECFSZ tempM, 1
 GOTO VCOG9
VCOG10
 MOVLW .56 ;Endereco Inicial da Tabela
 ADDWF tempR, W ;Soma Multiplicacao
 ADDWF ph_cntr, W ;Offset
 ;W contains EEPROM Address
 ; Select Bank 1
 BCF STATUS, RP1 
 BSF STATUS, RP0
 ; Load Pointer to EEPROM Memory
 MOVWF EEADR
 ; EE Read into W
 BSF EECON1, RD 
 MOVF EEDATA, W
 ; Select Bank 0
 BCF STATUS, RP1 
 BCF STATUS, RP0
 ;W contains PWM bit value for vco_b
 ADDLW 0
 BTFSC STATUS, Z
 GOTO VCOG11
 ;Not 0, set tempB bit 0
 BSF tempB, 0
 GOTO VCOG12
VCOG11
 BCF tempB, 0
VCOG12
 ;Sets PORTB Bit values
 MOVF tempB, W
 ANDLW 7
 MOVWF PORTB
 RETURN
  
NEXTPHASE
 ;Cycles pwm cycle counter ph_cntr from 0 to 7
 INCF ph_cntr, 1
 MOVF ph_cntr, W
 SUBLW .8
 BTFSS STATUS, Z
 RETURN
 ;is 8, clear ph_cntr
 CLRF ph_cntr
 RETURN
 
WAITPWMSLOT
;DEBUG ONLY ------------------------------------------------------------
;RETURN
 ;Waits 2,5mS on a 4Mhz Clock
 MOVLW .255
 MOVWF slot_cntr
WAITPWMSLOT1 
 DECFSZ slot_cntr, 1
 GOTO WAITPWMSLOT1
 RETURN
 
 END

