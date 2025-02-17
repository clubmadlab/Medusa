;***************************************************************************
; FILE:      medusa.asm											*
; CONTENTS:  Medusa												*
; COPYRIGHT: MadLab Ltd. 2025										*
; AUTHOR:    James Hutchby										*
; UPDATED:   16/02/25											*
;***************************************************************************

; mux could limit number of leds on at a time to 4 say

	processor 12F1840

	include "p12f1840.inc"

	ifdef __DEBUG
	__config _CONFIG1, _FOSC_INTOSC&_WDTE_OFF&_PWRTE_OFF&_MCLRE_ON&_CP_OFF&_CPD_OFF&_BOREN_OFF&_CLKOUTEN_OFF&_IESO_OFF&_FCMEN_ON&h'3fff'
	__config _CONFIG2, _WRT_OFF&_PLLEN_OFF&_STVREN_ON&_BORV_19&_LVP_OFF&h'3fff'
	else
	__config _CONFIG1, _FOSC_INTOSC&_WDTE_ON&_PWRTE_ON&_MCLRE_OFF&_CP_OFF&_CPD_OFF&_BOREN_OFF&_CLKOUTEN_OFF&_IESO_OFF&_FCMEN_ON&h'3fff'
	__config _CONFIG2, _WRT_ALL&_PLLEN_OFF&_STVREN_ON&_BORV_19&_LVP_OFF&h'3fff'
	endif

;	__idlocs h''

	errorlevel -207,-302,-305,-311


;***************************************************************************
;															*
; Specification												*
;															*
;***************************************************************************

; power-up self-test - LED noodles lit in turn

; patterns displayed at random


;***************************************************************************
;															*
; Port assignments												*
;															*
;***************************************************************************

PORTA_IO		equ	b'111111'		; PORTA status

BUTTON		equ	3			; pushbutton

LED_MASK		equ	b'110011'		; charlieplexed LEDs

MUX1			equ	5			; R1
MUX2			equ	4			; R2
MUX3			equ	1			; R3
MUX4			equ	0			; R4

LED1_TRIS		equ	PORTA_IO^(1<<MUX4)^(1<<MUX2)
LED1_LAT		equ	1<<MUX4
LED2_TRIS		equ	PORTA_IO^(1<<MUX2)^(1<<MUX4)
LED2_LAT		equ	1<<MUX2

LED3_TRIS		equ	PORTA_IO^(1<<MUX2)^(1<<MUX3)
LED3_LAT		equ	1<<MUX2
LED4_TRIS		equ	PORTA_IO^(1<<MUX3)^(1<<MUX2)
LED4_LAT		equ	1<<MUX3

LED5_TRIS		equ	PORTA_IO^(1<<MUX4)^(1<<MUX3)
LED5_LAT		equ	1<<MUX4
LED6_TRIS		equ	PORTA_IO^(1<<MUX3)^(1<<MUX4)
LED6_LAT		equ	1<<MUX3

LED7_TRIS		equ	PORTA_IO^(1<<MUX1)^(1<<MUX3)
LED7_LAT		equ	1<<MUX1
LED8_TRIS		equ	PORTA_IO^(1<<MUX3)^(1<<MUX1)
LED8_LAT		equ	1<<MUX3

LED9_TRIS		equ	PORTA_IO^(1<<MUX1)^(1<<MUX2)
LED9_LAT		equ	1<<MUX1
LED10_TRIS	equ	PORTA_IO^(1<<MUX2)^(1<<MUX1)
LED10_LAT		equ	1<<MUX2

LED11_TRIS	equ	PORTA_IO^(1<<MUX4)^(1<<MUX1)
LED11_LAT		equ	1<<MUX4
LED12_TRIS	equ	PORTA_IO^(1<<MUX1)^(1<<MUX4)
LED12_LAT		equ	1<<MUX1


;***************************************************************************
;															*
; Constants and timings											*
;															*
;***************************************************************************

NUM_LEDS		equ	d'12'		; number of LEDs

CLOCK		equ	d'16000000'	; processor clock frequency in Hz

NUM_PATTERNS	equ	d'8'			; number of patterns
PATTERN_REPS	equ	d'4'			; number of repeats


;***************************************************************************
;															*
; File register usage											*
;															*
;***************************************************************************

	cblock h'20'				; bank 0
	rand_l, rand_h				; random number
	pattern					; current pattern
	pattern_pnt:2				; pattern pointer
	repeat, loop				; loop counters
	endc

	cblock h'70'				; common RAM
	flags					; various flags
	phase					; LED PWM phase
	duty:NUM_LEDS				; LED duty cycles
	work, work1				; work registers
	endc

duty1	equ	duty+d'0'
duty2	equ	duty+d'1'
duty3	equ	duty+d'2'
duty4	equ	duty+d'3'
duty5	equ	duty+d'4'
duty6	equ	duty+d'5'
duty7	equ	duty+d'6'
duty8	equ	duty+d'7'
duty9	equ	duty+d'8'
duty10	equ	duty+d'9'
duty11	equ	duty+d'10'
duty12	equ	duty+d'11'


;***************************************************************************
;															*
; Macros														*
;															*
;***************************************************************************

routine	macro label			; routine
label
		endm

label	macro label			; label
label
		endm

table	macro label			; lookup table
label	brw
		endm

entry     macro value              ; table entry
		retlw value
		endm

index     macro label              ; index table
		call label
		endm

tstw		macro				; test w
		iorlw 0
		endm

movff     macro f1,f2              ; move file to file
		movfw f1
		movwf f2
		endm

movlf	macro n,f				; move literal to file
		movlw n
		movwf f
		endm

negw		macro				; negate w
		sublw 0
		endm


;---------------------------------------------------------------------------
; reset and interrupt vectors
;---------------------------------------------------------------------------

		org 0

		goto main_entry

		org 4

		goto main_entry


;***************************************************************************
;															*
; Procedures													*
;															*
;***************************************************************************

;---------------------------------------------------------------------------
; delay
;---------------------------------------------------------------------------

delay128	goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
delay64	goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
		goto $+1						; [8]
delay16   return						; [8]

delay     macro cycles					; delay for a number of cycles

		if (cycles) < 0
		error "Delay cycles negative"
		else

		variable i = cycles

		i = ((i+2)>>2)<<2

		while i > d'128'
		call delay128					; [128]
		i -= d'128'
		endw

		while i > d'64'
		call delay64					; [64]
		i -= d'64'
		endw

		if i >= d'16'
		variable n = (i-d'16')/d'8'
		call delay16-n					; [8+8n+8]
		i -= (n*d'8')+d'16'
		endif

		while i >= d'4'
		nop							; [4]
		i -= d'4'
		endw

		endif

		endm


;---------------------------------------------------------------------------
; multiplexes the LEDs, fed with the wait in 1/100s in the w reg
;---------------------------------------------------------------------------

		routine mux_leds

led	 	macro TRIS_,LAT_,duty_

		local mux0

;		tstf duty_
;		bz mux0

		movlb 2						; [4]
		movlw ~LED_MASK				; [4]
		andwf LATA					; [4]

		movlb 1						; [4]
		movlf TRIS_,TRISA				; [8]

		movlb 2						; [4]
		movfw duty_					; [4]
		subwf phase,w					; [4]
		movlw LAT_					; [4]
		skpc							; [4/8]
		iorwf LATA					; [4/0]

		movlb 0						; [4]

ITERS	equ	d'100'

		delay CLOCK/(d'100'*NUM_LEDS*ITERS)-d'56'

mux0		clrwdt						; [4]

		endm

		movwf work

mux1		movlf ITERS,work1

mux2		led LED1_TRIS,LED1_LAT,duty1
		led LED2_TRIS,LED2_LAT,duty2
		led LED3_TRIS,LED3_LAT,duty3
		led LED4_TRIS,LED4_LAT,duty4
		led LED5_TRIS,LED5_LAT,duty5
		led LED6_TRIS,LED6_LAT,duty6
		led LED7_TRIS,LED7_LAT,duty7
		led LED8_TRIS,LED8_LAT,duty8
		led LED9_TRIS,LED9_LAT,duty9
		led LED10_TRIS,LED10_LAT,duty10
		led LED11_TRIS,LED11_LAT,duty11
		led LED12_TRIS,LED12_LAT,duty12

		incf phase

		decfsz work1
		goto mux2

		decfsz work
		goto mux1

		movlb 2
		movlw ~LED_MASK
		andwf LATA
		movlb 0

		return


;---------------------------------------------------------------------------
; waits, fed with the wait in 1/100s in the w reg
;---------------------------------------------------------------------------

		routine wait

		movwf loop

wait1    	movlf CLOCK/(d'100'*d'16'*d'256'),work

wait2	clrf work1
wait3    	clrwdt                        	; [4]
		decfsz work1	             	     ; [4]
		goto wait3              	     	; [8]

		decfsz work
		goto wait2

		decfsz loop
		goto wait1

		return


;---------------------------------------------------------------------------
; generates a pseudo random number
;---------------------------------------------------------------------------

		routine get_random

		movfw rand_l
		iorwf rand_h,w
		bnz getr1

		movfw TMR0					; seed generator
		movwf rand_l
		xorlw h'ff'
		movwf rand_h

getr1	rlf rand_h,w					; calculate next in sequence
		xorwf rand_h,w
		movwf work1					; msb <= Q15 ^ Q14
		swapf rand_l,w
		btfsc rand_h,4
		xorlw h'80'					; msb <= Q12 ^ Q3
		xorwf work1
		rlf work1
		rlf rand_l
		rlf rand_h					; << 1 + (Q15 ^ Q14 ^ Q12 ^ Q3)

		movfw rand_l

		return


;---------------------------------------------------------------------------
; initialises the hardware
;---------------------------------------------------------------------------

		routine init_hardware

		movlb 0						; bank 0

		movlp 0						; page 0

		movlb 2						; initialise port
		clrf LATA
		movlb 1
		movlf PORTA_IO,TRISA
		movlb 0

		movlb 1						; weak pull-ups enabled
		movlf b'00001000',OPTION_REG
		movlb 0

		movlb 4
		movlf 1<<BUTTON,WPUA
		movlb 0

		movlb 1						; 16MHz
		movlf b'01111000',OSCCON
		clrf OSCTUNE
		movlb 0

		clrf INTCON					; disable interrupts
		movlb 1
		clrf PIE1
		clrf PIE2
		movlb 0
		clrf PIR1
		clrf PIR2

		movlb 1						; watchdog timer - 64ms
		movlf b'00110'<<1,WDTCON
		movlb 0

		movlb 2						; SR latch off
		clrf SRCON0
		clrf SRCON1
		movlb 0

		movlb 2						; comparator off
		clrf CM1CON0
		movlb 0

		movlb 2						; voltage reference off
		clrf FVRCON
		movlb 0

		movlb 5						; PWM off
		clrf CCP1CON
		clrf CCPR1L
		clrf CCP1AS
		movlb 0

		movlb 7						; no interrupt-on-change
		clrf IOCAP
		clrf IOCAN
		clrf IOCAF
		movlb 0

		movlb 3						; UART off
		clrf TXSTA
		clrf RCSTA
		movlb 0

		movlb 4						; SPI off
		clrf SSP1STAT
		clrf SSP1CON1
		clrf SSP1CON2
		clrf SSP1CON3
		movlb 0

		movlb 1						; ADC off
		clrf ADCON0
		clrf ADCON1
		movlb 0

		movlb 3						; no analogue
		clrf ANSELA
		movlb 0

		movlb 7						; reference clock off
		clrf CLKRCON
		movlb 0

		movlb 2						; DAC off
		clrf DACCON0
		movlb 0

		clrf CPSCON0					; capsense off

		movlb 7						; modulation control off
		clrf MDCON
		movlb 0

		clrf T1CON					; Timer1 off
		clrf T1GCON

		clrf T2CON					; Timer2 off

		return


;---------------------------------------------------------------------------
; clears all LEDs
;---------------------------------------------------------------------------

		routine clear_all

		clrf duty1
		clrf duty2
		clrf duty3
		clrf duty4
		clrf duty5
		clrf duty6
		clrf duty7
		clrf duty8
		clrf duty9
		clrf duty10
		clrf duty11
		clrf duty12

		clrf phase

		movlb 2
		clrf LATA
		movlb 1
		movlf PORTA_IO,TRISA
		movlb 0

		return


;---------------------------------------------------------------------------
; displays patterns
;---------------------------------------------------------------------------

		routine do_patterns

		movlb 3						; program memory
		bsf EECON1,EEPGD
		bcf EECON1,CFGS
		movlb 0

		movlf PATTERN_REPS,repeat

		call get_random				; random pattern
		andlw NUM_PATTERNS-1
		movwf pattern

dop1		lslf pattern,w
		index patterns
		movwf pattern_pnt+0

		lslf pattern,w
		incf WREG
		index patterns
		movwf pattern_pnt+1

dop2		call read_					; delay
		movwf work

		incf work,w					; end of pattern ?
		bz dop3						; branch if yes

		call read_
		movwf duty1
		call read_
		movwf duty2
		call read_
		movwf duty3
		call read_
 		movwf duty4
		call read_
 		movwf duty5
		call read_
 		movwf duty6
		call read_
		movwf duty7
		call read_
		movwf duty8
		call read_
		movwf duty9
		call read_
 		movwf duty10
		call read_
 		movwf duty11
		call read_
 		movwf duty12

		movfw work
		call mux_leds

		bra dop2

dop3		decfsz repeat
		bra dop1

		bra do_patterns


read_
		movfw pattern_pnt+0
		movlb 3
		movwf EEADRH
		movlb 0
		movfw pattern_pnt+1
		movlb 3
		movwf EEADRL

		bsf EECON1,RD					; read program memory
		nop
		nop

		movfw EEDATL

		movlb 0

		incf pattern_pnt+1				; step pointer
		skpnz
		incf pattern_pnt+0

		return


;---------------------------------------------------------------------------
; main entry point
;---------------------------------------------------------------------------

		routine main_entry

		clrwdt

		movlb d'31'					; reset stack
		movlf h'1f',STKPTR
		movlb 0

		call init_hardware

		clrf flags
		clrf phase
		clrf rand_l
		clrf rand_h
		clrf pattern

test		macro tris,lat

		movlb 2
		movlw ~LED_MASK
		andwf LATA

		movlb 1
		movlf tris,TRISA

		movlb 2
		movlw lat
		iorwf LATA

		movlb 0
		clrwdt

		movlw d'20'
		call wait

		endm

		test LED1_TRIS,LED1_LAT			; test all LEDs
		test LED2_TRIS,LED2_LAT
		test LED3_TRIS,LED3_LAT
		test LED4_TRIS,LED4_LAT
		test LED5_TRIS,LED5_LAT
		test LED6_TRIS,LED6_LAT
		test LED7_TRIS,LED7_LAT
		test LED8_TRIS,LED8_LAT
		test LED9_TRIS,LED9_LAT
		test LED10_TRIS,LED10_LAT
		test LED11_TRIS,LED11_LAT
		test LED12_TRIS,LED12_LAT

		call clear_all

		bra main_loop


;---------------------------------------------------------------------------
; main loop
;---------------------------------------------------------------------------

		routine main_loop

		movlb d'31'					; reset stack
		movlf h'1f',STKPTR
		movlb 0

		call clear_all

		goto do_patterns


;***************************************************************************
;															*
; Lookup tables												*
;															*
;***************************************************************************

		table patterns

pnt		macro addr
		entry high addr
		entry low addr
		endm

		pnt pattern1
		pnt pattern2
		pnt pattern3
		pnt pattern4
		pnt pattern5
		pnt pattern6
		pnt pattern7
		pnt pattern8


pattern_	macro d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,delay

		entry delay

		entry d1
		entry d2
		entry d3
		entry d4
		entry d5
		entry d6
		entry d7
		entry d8
		entry d9
		entry d10
		entry d11
		entry d12

		endm


		label pattern1

SPEED1	equ	d'5'

		pattern_ h'ff',h'80',h'40',h'20',h'10',h'08',h'04',h'02',h'01',h'00',h'00',h'00',SPEED1
		pattern_ h'80',h'40',h'20',h'10',h'08',h'04',h'02',h'01',h'00',h'00',h'00',h'ff',SPEED1
		pattern_ h'40',h'20',h'10',h'08',h'04',h'02',h'01',h'00',h'00',h'00',h'ff',h'80',SPEED1
		pattern_ h'20',h'10',h'08',h'04',h'02',h'01',h'00',h'00',h'00',h'ff',h'80',h'40',SPEED1
		pattern_ h'10',h'08',h'04',h'02',h'01',h'00',h'00',h'00',h'ff',h'80',h'40',h'20',SPEED1
		pattern_ h'08',h'04',h'02',h'01',h'00',h'00',h'00',h'ff',h'80',h'40',h'20',h'10',SPEED1
		pattern_ h'04',h'02',h'01',h'00',h'00',h'00',h'ff',h'80',h'40',h'20',h'10',h'08',SPEED1
		pattern_ h'02',h'01',h'00',h'00',h'00',h'ff',h'80',h'40',h'20',h'10',h'08',h'04',SPEED1
		pattern_ h'01',h'00',h'00',h'00',h'ff',h'80',h'40',h'20',h'10',h'08',h'04',h'02',SPEED1
		pattern_ h'00',h'00',h'00',h'ff',h'80',h'40',h'20',h'10',h'08',h'04',h'02',h'01',SPEED1
		pattern_ h'00',h'00',h'ff',h'80',h'40',h'20',h'10',h'08',h'04',h'02',h'01',h'00',SPEED1
		pattern_ h'00',h'ff',h'80',h'40',h'20',h'10',h'08',h'04',h'02',h'01',h'00',h'00',SPEED1

		entry -1


		label pattern2

SPEED2	equ	d'4'

		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED2
		pattern_ h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',SPEED2
		pattern_ h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',SPEED2
		pattern_ h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',SPEED2
		pattern_ h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',SPEED2
		pattern_ h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',SPEED2
		pattern_ h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',SPEED2
		pattern_ h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',SPEED2
		pattern_ h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',SPEED2
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED2
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED2
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED2

		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED2
		pattern_ h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',h'80',SPEED2
		pattern_ h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',h'40',SPEED2
		pattern_ h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',h'20',SPEED2
		pattern_ h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',h'10',SPEED2
		pattern_ h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',h'08',SPEED2
		pattern_ h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',h'04',SPEED2
		pattern_ h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',h'02',SPEED2
		pattern_ h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',h'01',SPEED2
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED2
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED2
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED2

		entry -1


		label pattern3

SPEED3	equ	d'8'

		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',SPEED3
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'ff',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'ff',h'ff',SPEED3
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'ff',SPEED3

		entry -1


		label pattern4

SPEED4	equ	d'5'

		pattern_ h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',SPEED4
		pattern_ h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',SPEED4
		pattern_ h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',SPEED4

		pattern_ h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',SPEED4
		pattern_ h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',SPEED4
		pattern_ h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',SPEED4
		pattern_ h'00',h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',SPEED4
		pattern_ h'00',h'ff',h'00',h'00',h'00',h'00',h'00',h'ff',h'00',h'00',h'00',h'00',SPEED4

		entry -1


		label pattern5

SPEED5	equ	d'10'

		pattern_ h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',SPEED5
		pattern_ h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',SPEED5
		pattern_ h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',SPEED5
		pattern_ h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',SPEED5
		pattern_ h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',SPEED5
		pattern_ h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',SPEED5
		pattern_ h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',SPEED5
		pattern_ h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',SPEED5
		pattern_ h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',SPEED5
		pattern_ h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',SPEED5
		pattern_ h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',SPEED5
		pattern_ h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',h'ff',h'00',h'00',h'ff',SPEED5

		entry -1


		label pattern6

SPEED6	equ	d'8'

		pattern_ h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',SPEED6
		pattern_ h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',SPEED6
		pattern_ h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',SPEED6
		pattern_ h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',SPEED6
		pattern_ h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',SPEED6
		pattern_ h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',SPEED6

		pattern_ h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',SPEED6
		pattern_ h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',SPEED6
		pattern_ h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',SPEED6
		pattern_ h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',SPEED6
		pattern_ h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',SPEED6
		pattern_ h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',SPEED6

		pattern_ h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',SPEED6
		pattern_ h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',SPEED6
		pattern_ h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',SPEED6
		pattern_ h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',SPEED6
		pattern_ h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',SPEED6
		pattern_ h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',SPEED6

		pattern_ h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',SPEED6
		pattern_ h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',SPEED6
		pattern_ h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',SPEED6
		pattern_ h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',SPEED6
		pattern_ h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',SPEED6
		pattern_ h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',SPEED6

		pattern_ h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',SPEED6
		pattern_ h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',SPEED6
		pattern_ h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',SPEED6
		pattern_ h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',SPEED6
		pattern_ h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',SPEED6
		pattern_ h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',SPEED6

		pattern_ h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',SPEED6
		pattern_ h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',SPEED6
		pattern_ h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',SPEED6
		pattern_ h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',SPEED6
		pattern_ h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',SPEED6
		pattern_ h'00',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'00',h'00',SPEED6

		entry -1


		label pattern7

SPEED7	equ	d'8'

		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'00',h'00',h'00',h'ff',h'ff',h'00',h'00',h'00',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',SPEED7
		pattern_ h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',SPEED7
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED7
		pattern_ h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',SPEED7
		pattern_ h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',SPEED7
		pattern_ h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'00',h'00',h'ff',h'ff',h'ff',h'ff',h'00',h'00',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'00',h'00',h'00',h'ff',h'ff',h'00',h'00',h'00',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED7
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED7

		entry -1


		label pattern8

SPEED8	equ	d'10'

		pattern_ h'40',h'ff',h'40',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',SPEED8
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'40',h'ff',h'40',h'00',h'00',h'00',SPEED8
		pattern_ h'00',h'00',h'00',h'40',h'ff',h'40',h'00',h'00',h'00',h'00',h'00',h'00',SPEED8
		pattern_ h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'00',h'40',h'ff',h'40',SPEED8

		entry -1


		end
