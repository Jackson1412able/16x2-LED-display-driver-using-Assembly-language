;************************************************************************************************************************     	
;       THIS PROGRAM WAS DEVELOPED FOR A MICROCHIP PIC18F4550 MICROCONTROLLER TO DISPLAY THE FOLLOWING STATIC           ;
;       MESSAGE ON THE LCD DISPLAY:                                                                                     ;
;                                                                                                                       ;
;                       ROW 1: MicroP & MicroC,                                                                         ;
;                       ROW 2: You Really Rock!                                                                         ;
;                                                                                                                       ;
;       THE CONNECTIONS OF THE MICROCHIP PIC18F4550 MICROCONTROLLER AND THE 16X2 LCD DISPLAY ARE SHOWN AS BELOW:        ;
;                                                                                                                       ;
;			PORTD output to LCD display                                                                     ;
;			RC0 = RS                                                                                        ;
;			RC1 = R/W                                                                                       ;
;			RC2 = E                                                                                         ;
;                                                                                                                       ;
;************************************************************************************************************************

;*** DECLARING AND CONFIGURING A MICROCONTROLLER ************************************************************************   
                        list p=18F4550, f=inhx32
                        #include "p18f4550.inc"
                        
;*** CONFIGURATION SETTING OF MICROCONTROLLER ***************************************************************************   
                        CONFIG FOSC=INTOSC_HS,WDT=OFF,LVP=OFF
                        CONFIG DEBUG=OFF,CP0=OFF,CP1=OFF,CP2=OFF,CP3=OFF
                        
;*** STRUCTURE OF PROGRAM MEMORY ****************************************************************************************   
                        ORG	0x00            ;BASE ADDRESS
Main
delayyy			equ	0x08            ;DELAYYY REGISTER ASSIGNED TO LOCATION 0X08
delayyyy		equ	0x09            ;DELAYYYY REGISTER ASSIGNED TO LOCATION 0X09
checkDDR		equ	0x0a            ;CHECKDDR REGISTER ASSIGNED TO LOCATION 0X0A 	
			movlw	0x0f            ;MOVE 0X0F LITERAL TO WORKING REGISTER
			movwf	delayyy	        ;MOVE 0X0F FROM WORKING REGISTER TO DELAYYY
			movwf	delayyyy        ;MOVE 0X0F FROM WORKING REGISTER TO DELAYYYY
			clrf	PORTD           ;CLEAR CONTENTS IN PORTD
			clrf	PORTC		;CLEAR CONTENTS IN PORTC		               
        		clrf 	TRISD		;SET PORTD AS OUTPUT
        		clrf    TRISC		;SET PORTC AS OUTPUT        		
        		call	ldelay          ;LONG DELAY, AT LEAST 4.1MS
                        
;****FUNCTION SET COMMAND (8-BIT INTERFACE): FIRST TIME******************************************************************     		
        		bcf	PORTC,0		;RS=0
			bcf	PORTC,1		;RW=0
			movlw	0x30            ;0011 0000
			movwf	PORTD           ;FUNCTION SET COMMAND 0011 0000
			call	pulse           ;SEND FUNCTION SET COMMAND TO LCD
			call	sdelay	        ;DELAY, AT LEAST 100US
                        
;****FUNCTION SET COMMAND (8-BIT INTERFACE): SECOND TIME*****************************************************************		
			bcf	PORTC,0		;RS=0
			bcf	PORTC,1		;RW=0
			movlw	0x30	        ;0011 0000
			movwf	PORTD           ;FUNCTION SET COMMAND 0011 0000
			call	pulse           ;SEND FUNCTION SET COMMAND TO LCD
			call	sdelay	        ;DELAY
                        
;****FUNCTION SET COMMAND (8-BIT INTERFACE): THIRD TIME******************************************************************   			
			bcf	PORTC,0		;RS=0
			bcf	PORTC,1		;RW=0
			movlw	0x30	        ;0011 0000
			movwf	PORTD           ;FUNCTION SET COMMAND 0011 0000
			call	pulse           ;SEND FUNCTION SET COMMAND TO LCD
			call	sdelay	        ;DELAY
                        
;****FUNCTION SET COMMAND (8-BIT INTERFACE; 2-LINE; 5X7 DOTS)************************************************************   
			bcf	PORTC,0		;RS=0
			bcf	PORTC,1		;RW=0
			movlw	0x38		;0011 1000
			movwf	PORTD           ;N=1(2 line display), F=0(5x7 dots)
			call	pulse           ;SEND FUNCTION SET COMMAND TO LCD
			call	checkBF	        ;CHECK IF LCD IS BUSY
                        
;****TURN DISPLAY OFF****************************************************************************************************   
                        bcf     PORTC,0		;RS=0
                        bcf     PORTC,1		;RW=0
                        movlw   0x08		;0000 1000
                        movwf   PORTD           ;DISPLAY OFF, CURSOR OFF, CURSOR BLINK OFF
                        call    pulse           ;SEND INSTRUCTION TO LCD
                        call    checkBF         ;CHECK IF LCD IS BUSY
                        
;****CLEAR DISPLAY*******************************************************************************************************
                        bcf     PORTC,0		;RS=0
                        bcf     PORTC,1		;RW=0
                        movlw   0x01		;0000 0001
                        movwf   PORTD           ;CLEAR DISPLAY
                        call    pulse           ;SEND INSTRUCTION TO LCD
                        call    checkBF         ;CHECK IF LCD IS BUSY
                        
;****ENTRY MODE SET: INCREMENT DISPLAY DATA RAM (DD RAM) ADDRESS BY 1 AND SHIFT THE ENTIRE DISPALY TO THE LEFT***********
                        bcf     PORTC,0		;RS=0
                        bcf     PORTC,1		;RW=0
                        movlw   0x06		;0000 0110
                        movwf   PORTD           ;I/D=1(INCREMENT DDRAM), S=0(NO SHIFT)
                        call    pulse           ;SEND INSTRUCTION TO LCD
                        call    checkBF         ;CHECK IF LCD IS BUSY
                        
;****TURN DISPLAY ON*****************************************************************************************************              
			bcf	PORTC,0		;RS=0
			bcf	PORTC,1		;RW=0
			movlw	0x0e		;0000 1110
			movwf	PORTD           ;C=1(cursor on), B=0(blink off)
			call	pulse           ;SEND INSTRUCTION TO LCD
			call	checkBF         ;CHECK IF LCD IS BUSY
                        
;****COPY CONTENTS FROM TABLE********************************************************************************************   				
senddata		movlw	upper(data1)    ;COPY CONTENTS FROM UPPER BYTE OF DATA1
			movwf	TBLPTRU         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER UPPER BYTE
			movlw	high(data1)     ;COPY CONTENTS FROM HIGH BYTE OF DATA1
			movwf	TBLPTRH         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER HIGH BYTE
			movlw	low(data1)      ;COPY CONTENTS FROM LOW BYTE OF DATA1
			movwf	TBLPTRL         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER LOW BYTE

;****MAIN PROGRAM********************************************************************************************************   
lcdoutput		call	checkDDRAM      ;CHECK DDRAM ADDRESS
			tblrd*+                 ;INCREMENT TABLE POINTER
			movf	TABLAT,w        ;EXTRACT CHARACTER AND COPY TO WORKING REGISTER
			iorlw	0x00            ;CHECK IF IT IS 0X00 OR 0 IN DECIMAL AND HEXADECIMAL, IF 0, ZERO FLAG HIGH
			btfsc	STATUS,2	;IF ZERO FLAG HIGH, DONT SKIP NEXT INSTRUCTION
			bra	endd		;END PROGRAM
			call	display         ;PROCEED TO SEND DATA TO LCD
			goto	lcdoutput       ;BACK TO LCDOUTPUT LABEL AGAIN
				
;************************************************************************************************************************
;*** SUBROUTINES ********************************************************************************************************
;************************************************************************************************************************
;****SEND DATA FROM PORTD TO LCD*****************************************************************************************   
display			movwf	PORTD           ;COPY CHARACTER TO PORTD
			bsf	PORTC,0		;RS=1
			bcf	PORTC,1		;R/W=0
			call	pulse           ;SEND PORTD DATA TO LCD
			call	sdelay          ;DELAY
			return                  ;RETURN FROM SUBROUTINE
                        
;****CUSTOM DELAY SUBROUTINE*********************************************************************************************
ldelay			call	sdelay          ;JUMP TO SDELAY LABEL
			decfsz	delayyy,f       ;DECREASE VALUE IN DELAYYY BY 1, SKIP NEXT INSTRUCTION IF ZF IS HIGH
			bra	ldelay          ;JUMP TO LDELAY LABEL
			return                  ;RETURN FROM SUBROUTINE
                        
sdelay			decfsz	delayyyy,f      ;DECREASE VALUE IN DELAYYYY BY 1, SKIP NEXT INSTRUCTION IF ZF HIGH
			bra	sdelay          ;JUMP TO SDELAY LABEL
			return                  ;RETURN FROM SUBROUTINE
                        
;****ENABLING DATA FROM PIC18F4550 TO BE SENT TO LCD**********************************************************************
pulse			bsf	PORTC,2		;E=1
			call	sdelay          ;DELAY
			bcf	PORTC,2		;E=0
			return                  ;RETURN FROM SUBROUTINE
                        
;****CHECK IF LCD IS BUSY BY CHECKING BUSY FLAG***************************************************************************
checkBF			setf	TRISD		;SET PORTD AS INPUT
			bcf	PORTC,0		;RS=0
			bsf	PORTC,1		;R/W=1
			nop
			bsf	PORTC,2		;E=1
			nop
			movf	PORTD,0		;MOVE READ DATA TO WORKING REGISTER
			bcf	PORTC,2		;E=0
			nop
			btfsc	WREG,7		;CHECK BUSY FLAG OR 7TH BIT, SKIP NEXT INSTRUCTION IF BF LOW
			bra	checkBF		;RECHECK BUSY FLAG
			clrf	TRISD		;SET PORTD AS OUTPUT
			return                  ;RETURN FROM SUBROUTINE
                        
;****CHECK DDRAM ADDRESS**************************************************************************************************
checkDDRAM		movlw	0x10            ;0X10 IS 17 IN DECIMAL
			movwf	checkDDR	;MOVE 0X10 LITERAL TO CHECKDDR REGISTER
			setf	TRISD		;SET PORTD AS INPUT
			bcf	PORTC,0		;RS CLEARED
			bsf	PORTC,1		;R/W SET
			nop
			bsf	PORTC,2		;E SET
			nop
			movf	PORTD,0		;MOVE READ DATA TO WORKING REGISTER
			bcf	PORTC,2		;E CLEARED
			nop
			clrf	TRISD		;SET PORTD AS OUTPUT
			bcf	WREG,7		;CLEAR BUSY FLAG OR 7TH BIT
			cpfseq	checkDDR        ;COMPARE READ ADDRESS AND ADDRESS STORED IN CHECKDDR REGISTER, SKIP NEXT IF EQUAL
			bra	exit
			bcf	PORTC,0		;RS CLEARED
			bcf	PORTC,1		;R/W CLEARED
			movlw	0xc0		;ADDRESS FOR SECOND ROW FIRST COLUMN
			movwf	PORTD		;WRITE ADDRESS TO PORTD
			call	pulse		;SEND ADDRESS FROM PORTD TO LCD
			call	sdelay          ;DELAY
exit			return                  ;RETURN FROM SUBROUTINE

;*************************************************************************************************************************
data1			db	"MicroP & MicroC,You Really Rock!",0
endd                    End
