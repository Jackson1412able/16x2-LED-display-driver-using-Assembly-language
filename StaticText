        ORG	$400400
        MOVEA.L	#$800001,A0	*						$800001 PGCR in A0 address register
        MOVE.B	#$80,$C(A0)	*						PACR contains 1000 0000 (port A submode 1x)
        MOVE.B	#$FF,$4(A0)	*						PADDR contains 1111 1111 (port A as output)
        MOVE.B 	#$80,$E(A0)	*						PBCR contains 1000 0000 (port B submode 1x)
	MOVE.B 	#$E0,$6(A0)	*						PBDDR contains 1110 0000 (pin 7,6,5 of port B as output, the rest inputs)
	MOVE.B 	#$14,$8(A0)	*						PCDDR contains 0001 0100 (pin 4,2 of port C as output, the rest inputs)
	BSR	_CLEAR
	BSR	_DELAY

*************************************************************************************************************************
*** LCD INITIALIZATION SUBROUTINE
	BSR	_DELAY
        
*** FUNCTION SET COMMAND (8-BIT INTERFACE): FIRST TIME (BF cannot be checked before this command)
        BCLR	#5,$12(A0)	*put RS line to 0					PBDR bit 5 cleared
        BCLR	#6,$12(A0)	*put R/W line to 0					PBDR bit 6 cleared
        MOVE.B	#$30,$10(A0)	*Function Set Command - 8-bit length			PADR contains 0011 0000
        BSR	_PULS_E
        BSR	_DELAY
        
*** FUNCTION SET COMMAND (8-BIT INTERFACE): SECOND TIME (BF cannot be checked before this command)
        BCLR	#5,$12(A0)	*put RS line to 0					PBDR bit 5 cleared
        BCLR	#6,$12(A0)	*put R/W line to 0					PBDR bit 6 cleared
        MOVE.B	#$30,$10(A0)	*Function Set Command - 8-bit length			PADR contains 0011 0000
        BSR	_PULS_E
	BSR	_DELAY
        
*** FUNCTION SET COMMAND (8-BIT INTERFACE): THIRD TIME (BF can be checked after this command)
        BCLR	#5,$12(A0)	*put RS line to 0					PBDR bit 5 cleared
        BCLR	#6,$12(A0)	*put R/W line to 0					PBDR bit 6 cleared
        MOVE.B	#$30,$10(A0)	*Function Set Command - 8-bit length			PADR contains 0011 0000
        BSR	_PULS_E
        BSR	_DELAY
        
*** FUNCTION SET COMMAND (8-BIT INTERFACE; 2-LINE; 5X7 DOTS): THIRD TIME (BF can be checked after this command)
        BCLR	#5,$12(A0)	*put RS line to 0					PBDR bit 5 cleared
        BCLR	#6,$12(A0)	*put R/W line to 0					PBDR bit 6 cleared
        MOVE.B	#$38,$10(A0)	*Function Set Command - 8-bit length, 2-line display, 5x7 dots	PADR contains 0011 1000
        BSR	_PULS_E

*************************************************************************************************************************
***LCD INITIALIZATION SUBROUTINE (CONTINUED)

REPEAT
	BSR	_CHK_BF
    
*** TURN DISPLAY OFF
        BCLR	#5,$12(A0)	*put RS line to 0					PBDR bit 5 cleared
        BCLR	#6,$12(A0)	*put R/W line to 0					PBDR bit 6 cleared
        MOVE.B	#$08,$10(A0)	*turn display OFF					PADR contains 0000 1000
        BSR	_PULS_E
        BSR	_CHK_BF
        
*** CLEAR DISPLAY
        BCLR	#5,$12(A0)	*put RS line to 0					PBDR bit 5 cleared
        BCLR	#6,$12(A0)	*put R/W line to 0					PBDR bit 6 cleared
        MOVE.B	#$01,$10(A0)	*clear display					PADR contains 0000 0001
        BSR	_PULS_E
        BSR	_CHK_BF
        
*** ENTRY MODE SET: INCREMENT DISPLAY DATA RAM (DD RAM) ADDRESS BY 1 AND SHIFT THE ENTIRE DISPALY TO THE LEFT
        BCLR	#5,$12(A0)	*put RS line to 0					PBDR bit 5 cleared
        BCLR	#6,$12(A0)	*put R/W line to 0					PBDR bit 6 cleared
        MOVE.B	#$06,$10(A0)	*increment DD RAM but do not shift the display		PADR contains 0000 0110
        BSR	_PULS_E
        BSR	_CHK_BF
        
*** TURN DISPLAY ON
        BCLR	#5,$12(A0)	*put RS line to 0					PBDR bit 5 cleared
        BCLR	#6,$12(A0)	*put R/W line to 0					PBDR bit 6 cleared
        MOVE.B	#$0E,$10(A0)	*turn display ON					PADR contains 0000 1110
        BSR	_PULS_E
        
*************************************************************************************************************************
*************************************************************************************************************************

MAIN
	BSR	_CHK_BF
AGAIN
        MOVEA.L	#TABLE,A1	*load data from #TABLE using A1 addresss register	
DISPLAY
        BSR	_CHK_DDR
	BSET	#5,$12(A0)	*put RS line to 1					PBDR bit 5 set
	BCLR	#6,$12(A0)	*put R/W line to 0					PBDR bit 6 cleared
	MOVE.B	(A1)+,$10(A0)
	BSR	_PULS_E
	BSR	_DELAY
	CMPI.B	#'<',(A1)
	BNE	DISPLAY
	BRA	REPEAT
        
*************************************************************************************************************************

_CLEAR
        MOVE.B	#$00,$10(A0)	*						PADR contains 0000 0000
	MOVE.B	#$00,$12(A0)	*						PBDR contains 0000 0000
CLEAR_
        RTS
        
*************************************************************************************************************************

_DELAY	
	MOVE.L	#55555,D0	*						D0 contains 55555 in decimal
DEL1	
	SUBQ.L	#1,D0		*						D0 is subtracted by 1 at a time (subtraction is performed 55555 times)
	BNE	DEL1		*						Branch taken if Z flag = 0 (not zero)
DELAY_	
	RTS
	
*************************************************************************************************************************
***PULSING OF "E LINE" SUBROUTINE: WHEN WRITING (i.e., SENDING COMMANDS OR CHARACTER DATA) TO THE DISPLAY, DATA IS 
***TRANSFERRED ONLY ON THE HIGH TO LOW TRANSITION OF THIS SIGNAL

_PULS_E
	BSET	#7,$12(A0)	*take E line HIGH					PBDR bit 7 set
	NOP			*hold it high for one clock cycle
	BCLR	#7,$12(A0)	*take E line LOW					PBDR bit 7 cleared
PULS_E_
	RTS
	
*************************************************************************************************************************
***CHECKING OF LCD CONTROLLER BUSY FLAG SUBROUTINE: THE LCD CONTROLLER IS SLOWER THAN THE MICROPROCESSOR. SO, WHEN THE
***MICROPROCESSOR IS TOLD TO DO SOMETHING, WE HAVE TO CHECK WHETHER OR NOT IT HAS COMPLETED THE GIVEN TASK BEFORE ASKING
***IT TO EXECUTE A NEW ONE. WE CAN CHECK THE BUSY FLAG (BF) OF THE LCD CONTROLLER BY READING BACK INFO FROM IT. IF BF = 1,
***THE LCD CONTROLLER IS STILL BUSY. CONTRARY TO WRITING, READING FROM THE DISPLAY CAN BE PERFORMED WHEN THE "E LINE" IS
***HIGH AND THE DATA REMAIN AVAILABLE UNTIL THE "E LINE" IS TAKEN LOW AGAIN.

_CHK_BF		
        MOVE.B	#$00,$4(A0)		*					PADDR contains 0000 0000 (port A as input)
	BCLR	#5,$12(A0)		*put RS line to 0, 				PBDR bit 5 cleared
	BSET	#6,$12(A0)		*put R/W line to 1, 				PBDR bit 6 set
	NOP
	BSET	#7,$12(A0)		*take E line HIGH, 				PBDR bit 7 set
	NOP				*hold it high for one clock cycle
	MOVE.B	$10(A0),D6		*D6 contains PADR
      	BCLR	#7,$12(A0)		*take E line LOW, 				PBDR bit 7 cleared
     	NOP
     	BTST	#7,D6			*Check bit 7 in D6, affect Z flag, Z = 0 if bit =1, Z=1 if bit = 0
     	BNE	_CHK_BF			*Loop again if Z flag = 1
     	MOVE.B	#$FF,$4(A0)		*					PADDR contains 1111 1111 (port A as output)     
CHK_BF_	
        RTS
        
*************************************************************************************************************************
***CHECKING OF DISPLAY DATA RAM (DD RAM) ADDRESS SUBROUTINE: THE LCD DISPLAY USED IS A 16 CHARACTER X 2 LINE DISPLAY.
***EVEN THOUGH IT IS SPECIFIED AS A 16 CHARACTER/ROW DISPLAY, WE CAN ACTUALLY STORE UP TO 40 CHARACTERS/ROW - 
***THE DISPLAY CAN ONLY SHOW THE FIRST 16 CHARACTERS REPRESENTED BY DD RAM LOCATIONS $00 THROUGH $0F. HERE, WE STORE UP TO 
***16 CHARACTERS PER ROW. SO, WE NEED TO CHECK WHETHER THE CURSOR HAS ALREADY REACHED AND COVERED LOCATION $0F.

_CHK_DDR	
        MOVE.B	#$00,$4(A0)		*prepare Port A as input port			PADDR contains 0000 0000 (port A as input)
      	BCLR	#5,$12(A0)		*put RS line to 0				PBDR bit 5 cleared
      	BSET	#6,$12(A0)		*put R/W line to 1				PBDR bit 6 set			
      	NOP
      	BSET	#7,$12(A0)		*take E line HIGH				PBDR bit 7 set
        NOP				*hold it high for one clock cycle
        MOVE.B	$10(A0),D5		*read info from LCD display (i.e., LCD controller)	D5 contains info of PADR
        BCLR	#7,$12(A0)		*take E line LOW				PBDR bit 7 cleared
        NOP
        MOVE.B	#$FF,$4(A0)		*change back Port A as output port		PADDR contains 1111 1111 (port A as output)
        ANDI.B	#$7F,D5			*Ignore busy flag(8th bit)
        CMPI.B	#$10,D5			*17th segment in LCD
        BNE	CHK_DDR_
        BCLR	#5,$12(A0)		*put RS line to 0				PBDR bit 5 cleared
        BCLR	#6,$12(A0)		*put R/W line to 0				PBDR bit 6 cleared
        MOVE.B	#$C0,$10(A0)		*Set Display Address Command - go to first location of second row	PADR contains 1100 0000
        BSR	_PULS_E
        BSR	_CHK_BF
CHK_DDR_
        RTS
	
*************************************************************************************************************************

TABLE	
        DC.B	'C','O','N','G','R','A','T','U','L','A','T','I','O','N','S',','
	DC.B	'Y','o','u',' ','m','a','d','e',' ','i','t',' ','m','a','n','!','<'

	END
