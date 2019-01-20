;************************************************************************************************************************     	
;       THIS PROGRAM WAS DEVELOPED FOR A MICROCHIP PIC18F4550 MICROCONTROLLER TO DISPLAY AN ENTRY MADE FROM             ;
;       A 4X4 MATRIX KEYPAD ONTO THE 16X2 LCD DISPLAY. FOR EXAMPLE, IF ONE OF THE KEYPAD BUTTONS IS PUSHED, THE         ;
;       LCD SHOULD BE COMMANDED TO DISPLAY AN APPROPRIATE CHARACTER. FOR EXTRA FLEXIBILITIES, A BUTTON TO CLEAR         ;
;       THE LCD SCREEN AND A BUTTON TO DELETE CHARACTERS ENTERED WERE IMPLEMENTED                                       ;
;       THE CONNECTIONS OF THE MICROCHIP PIC18F4550 MICROCONTROLLER AND THE 16X2 LCD DISPLAY ARE SHOWN AS BELOW:        ;
;                                                                                                                       ;
;                       PORTD output to LCD display                                                                     ;
;                       RC0 = RS                                                                                        ;
;                       RC1 = R/W                                                                                       ;
;                       RC2 = E                                                                                         ;
;                       RB0~RB3 = R1,R2,R3,R4 of keypad (inputs)                                                        ;
;                       RB4~RB7 = C1,C2,C3,C4 of keypad (outputs)                                                       ;
;                       RA0 = Switch 1 (clear LCD screen)                                                               ;
;                       RA1 = Switch 2 (backspace)                                                                      ;
;                                                                                                                       ;
;************************************************************************************************************************

;*** DECLARING AND CONFIGURING A MICROCONTROLLER ************************************************************************ 
                        LIST P=18F4550, F=INHX32  ;directive to define processor
                        #include <P18F4550.INC> ;processor specific variable definitions
                        
;*** CONFIGURATION SETTING OF MICROCONTROLLER ***************************************************************************
                        CONFIG FOSC = INTOSC_HS,WDT=OFF,LVP=OFF
                        CONFIG DEBUG=OFF,CP0=OFF,CP1=OFF,CP2=OFF,CP3=OFF
                        CONFIG PBADEN=OFF
                        
;*** STRUCTURE OF PROGRAM MEMORY ****************************************************************************************
                        org     0x00            ;BASE ADDRESS  
delrg                   equ     0x80            ;DELRG REGISTER ASSIGNED TO LOCATION 0X80
delayyy                 equ     0x0f            ;DELAYYY REGISTER ASSIGNED TO LOCATION 0X0F
delayyyy                equ     0x0e            ;DELAYYYY REGISTER ASSIGNED TO LOCATION 0X0E
checkzero               equ     0x25            ;CHECKZERO REGISTER ASSIGNED TO LOCATION 0X25
pressed_key             equ     0x26            ;PRESSED_KEY REGISTER ASSIGNED TO LOCATION 0X26
checkDDR                equ     0x0d            ;CHECKDDR REGISTER ASSIGNED TO LOCATION 0X0D
current_A               equ     0x10            ;CURRENT_A REGISTER ASSIGNED TO LOCATION 0X10
                        
;INITIALIZATION              
main                    movlw   0x0f            ;MOVE 0X0F LITERAL TO WORKING REGISTER
                        movwf   delayyy         ;MOVE 0X0F FROM WORKING REGISTER TO DELAYYY REGISTER
                        movwf   delayyyy        ;MOVE 0X0F FROM WORKING REGISTER TO DELAYYYY REGISTER
                        clrf    PORTB           ;CLEAR CONTENTS IN PORTB
                        clrf    PORTC           ;CLEAR CONTENTS IN PORTC
                        clrf    PORTD           ;CLEAR CONTENTS IN PORTD
                        clrf    PORTA           ;CLEAR CONTENTS IN PORTA
                        MOVLW   0Fh             ;CONFIGURE A/D
                        MOVWF   ADCON1          ;FOR DIGITAL INPUTS
                        MOVLW   07h             ;CONFIGURE COMPARATORS
                        MOVWF   CMCON           ;FOR DIGIAL INPUT
                        movlw   0xf0            ;MOVE 1111 0000 TO WORKING REGISTER (1 IS INPUT, 0 IS OUTPUT)
                        movwf   TRISB           ;RB4~RB7 INPUTS(COLUMN), RB0~RB3 OUTPUTS(ROW)
                        clrf    TRISD           ;PORTD AS OUTPUT (0000 0000)
                        clrf    TRISC           ;PORTC AS OUTPUT (RS=RC0, RW=RC1, E=RC2)
                        movlw   0x03            ;MOVE 0000 0011 TO WORKING REGISTER
                        movwf   TRISA           ;RA1 AND RA0 TO DETECT SWITCHES STATES
                        call    ldelay          ;DELAY
                        
;****FUNCTION SET COMMAND (8-BIT INTERFACE): FIRST TIME******************************************************************
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;RW=0
                        movlw   0x30            ;0011 0000
                        movwf   PORTD           ;FUNCTION SET COMMAND 0011 0000
                        call    pulse           ;SEND FUNCTION SET COMMAND TO LCD
                        call    sdelay          ;JUMP TO SDELAY SUBROUTINE, AT LEAST 100US
                        
;****FUNCTION SET COMMAND (8-BIT INTERFACE): SECOND TIME*****************************************************************
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;RW=0
                        movlw   0x30            ;0011 0000
                        movwf   PORTD           ;FUNCTION SET COMMAND 0011 0000
                        call    pulse           ;SEND FUNCTION SET COMMAND TO LCD
                        call    sdelay          ;JUMP TO SDELAY SUBROUTINE, AT LEAST 100US
                        
;****FUNCTION SET COMMAND (8-BIT INTERFACE): THIRD TIME******************************************************************   
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;RW=0
                        movlw   0x30	          ;0011 0000
                        movwf   PORTD           ;FUNCTION SET COMMAND 0011 0000
                        call    pulse           ;SEND FUNCTION SET COMMAND TO LCD
                        call    sdelay	        ;JUMP TO SDELAY SUBROUTINE
                        
;*** FUNCTION SET COMMAND (8-BIT INTERFACE; 2-LINE; 5X7 DOTS)************************************************************ 
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;RW=0
                        movlw   0x38            ;0011 1000
                        movwf   PORTD           ;N=1(2 line display), F=0(5x7 dots)
                        call    pulse           ;SEND FUNCTION SET COMMAND TO LCD
                        call    checkBF	        ;CHECK IF LCD IS BUSY
                        
;****TURN DISPLAY OFF****************************************************************************************************
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;RW=0
                        movlw   0x08            ;0000 1000
                        movwf   PORTD           ;DISPLAY OFF, CURSOR OFF, CURSOR BLINK OFF
                        call    pulse           ;SEND INSTRUCTION TO LCD
                        call    checkBF         ;CHECK IF LCD IS BUSY
                        
;****CLEAR DISPLAY*******************************************************************************************************
                        clear   bcf PORTC,0     ;RS=0
                        bcf     PORTC,1         ;RW=0
                        movlw   0x01	        	;0000 0001
                        movwf   PORTD           ;CLEAR DISPLAY
                        call    pulse           ;SEND INSTRUCTION TO LCD
                        call    checkBF         ;CHECK IF LCD IS BUSY
                        
;****ENTRY MODE SET: INCREMENT DISPLAY DATA RAM (DD RAM) ADDRESS BY 1 AND SHIFT THE ENTIRE DISPALY TO THE LEFT***********
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;RW=0
                        movlw   0x06	        	;0000 0110
                        movwf   PORTD           ;I/D=1(INCREMENT DDRAM), S=0(NO SHIFT)
                        call    pulse           ;SEND INSTRUCTION TO LCD
                        call    checkBF         ;CHECK IF LCD IS BUSY
                        
;****TURN DISPLAY ON***************************************************************************************************** 
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;RW=0
                        movlw   0x0d            ;000 1101            
                        movwf   PORTD           ;C=1(cursor on), B=0(blink off)
                        call    pulse           ;SEND INSTRUCTION TO LCD
                        call    checkBF         ;CHECK IF LCD IS BUSY
                        
;****COPY CONTENTS FROM TABLE********************************************************************************************
senddata                movlw   upper(data1)    ;COPY CONTENTS FROM UPPER BYTE OF DATA1
                        movwf   TBLPTRU         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER UPPER BYTE
                        movlw   high(data1)     ;COPY CONTENTS FROM HIGH BYTE OF DATA1
                        movwf   TBLPTRH         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER HIGH BYTE
                        movlw   low(data1)      ;COPY CONTENTS FROM LOW BYTE OF DATA1
                        movwf   TBLPTRL         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER LOW BYTE

;****MAIN PROGRAM********************************************************************************************************
lcdoutput               tblrd*+                 ;INCREMENT TABLE POINTER
                        movf    TABLAT,w        ;EXTRACT CHARACTER AND COPY TO WORKING REGISTER
                        iorlw   0x00            ;CHECK IF IT IS 0X00 OR 0 IN DECIMAL AND HEXADECIMAL, IF 0, ZF HIGH
                        btfsc   STATUS,2	      ;IF ZERO FLAG HIGH, DONT SKIP NEXT INSTRUCTION
                        bra     keyoutput       ;JUMP TO KEYOUTPUT LABEL
                        call    display         ;PROCEED TO SEND DATA TO LCD
                        goto    lcdoutput       ;BACK TO LCDOUTPUT LABEL AGAIN
                        
;****DISPLAY IN SECOND ROW OF LCD****************************************************************************************
keyoutput
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;R/W=0
                        movlw   0xc0            ;DDRAM ADDRESS OF ROW 2 OF LCD DISPLAY
                        movwf   PORTD           ;MOVE DDRAM ADDRESS TO PORTD
                        call    pulse           ;SEND DATA TO LCD
                        nop
                        call    sdelay          ;JUMP TO SDELAY SUBROUTINE
                        
;****DETECT KEYPRESS AND OUTPUT TO LCD***********************************************************************************
detectloop              call    checkDDRAM      ;CHECK CURRENT DDRAM ADDRESS
                        bra     clear_screen?   ;CHECK WHETHER CLEAR SCREEN BUTTON IS PRESSED
no_clear                bra     backspace?      ;CHECK WHETHER BACKSPACE BUTTON IS PRESSED
backspace               clrf    checkzero       ;CLEAR CHECKZERO REGISTER CONTENTS
                        call    getkey          ;CHECK KEY PRESS ON KEYPAD, IF ANY KEYS ARE PRESSED, VALUE PASSED TO WREG
                        iorwf   checkzero       ;COMPARE WORKING REGISTER WITH EMPTY CONTENT (0X00)
                        bz      detectloop      ;BACK TO DETECT KEY PRESS AGAIN
                        movwf   pressed_key     ;PASS PRESSED VALUE TO PRESSED_KEY REGISTER
waitkeyrelease          clrf    checkzero       ;CLEAR CHECKZERO REGISTER CONTENTS
                        call    getkey          ;CHECK KEY PRESS ON KEYPAD
                        iorwf   checkzero       ;COMPARE VALUE PRESSED WITH 0X00
                        bnz     waitkeyrelease  ;IF PRESSED KEY IS NOT RELEASED, PROGRAM WAITS UNTIL RELEASE
                        movf    pressed_key,0   ;MOVE PRESSED VALUE TO WORKING REGISTER
                        call    display         ;JUMP TO DISPLAY SUBROUTINE
                        nop
                        bra     detectloop      ;JUMP BACK TO DETECTLOOP TO CHECK FOR ANY KEY PRESSES AGAIN
                        
;****KEYPAD FUNCTIONS****************************************************************************************************
clear_screen?           btfsc   PORTA,0         ;RA0==0? 0 MEANS RA0 IS PRESSED
                        bra     no_clear        ;JUMP BACK TO NO_CLEAR LABEL 
                        bcf     PORTC,0         ;RS=0   
                        bcf     PORTC,1         ;RW=0
                        movlw   0x01            ;0000 0001
                        movwf   PORTD           ;CLEAR DISPLAY
                        call    pulse           ;SEND DATA TO LCD
                        call    checkBF         ;CHECK IF LCD IS BUSY
                        bra     senddata        ;REPRINT "ENTER KEY" TEXT ON FIRST ROW OF LCD AGAIN AFTER CLEAR DISPLAY
                        
backspace?              btfsc   PORTA,1         ;RA1=0? 0 MEANS RA1 IS PRESSED
                        bra     backspace       ;JUMP BACK TO BACKSPACE LABEL
waitswitch1             nop
                        btfss   PORTA,1         ;WAIT UNTIL SWITCH 1 IS RELEASED
                        goto    waitswitch1     ;RA1=1 IF SWITCH 1 IS RELEASED
                        call    back            ;DECREASE DDRAM by 1
                        movlw   ' '
                        call    display         ;CLEAR THE LAST PRINTED CHARACTER
                        call    back            ;DECREASE DDRAM by 1
                        bra     backspace       ;JUMP BACK TO BACKSPACE LABEL
                        
;************************************************************************************************************************
;*** SUBROUTINES ********************************************************************************************************
;************************************************************************************************************************
;*** DISPLAY CHARACTERS ON LCD ******************************************************************************************
display                 movwf   PORTD
                        bsf     PORTC,0         ;RS=1
                        bcf     PORTC,1         ;R/W=0
                        call    pulse           ;SEND DATA TO LCD
                        call    sdelay          ;JUMP TO SDELAY SUBROUTINE
                        return
                        
;*** GET PRESSED CHARACTER **********************************************************************************************
getkey                  movlw   0xfe            ;1111 1110
                        movwf   PORTB           ;SEND HIGH SIGNALS TO RB4~RB7
                        call    delayf          ;JUMP TO DELAYF SUBROUTINE
                        btfss   PORTB,4         ;check column 1
                        retlw   '1'
                        btfss   PORTB,5         ;check column 2
                        retlw   '2'
                        btfss   PORTB,6         ;check column 3
                        retlw   '3'
                        btfss   PORTB,7         ;check column 4
                        retlw   'A'
                        movlw   0xfd            ;1111 1101
                        movwf   PORTB
                        call    delayf
                        btfss   PORTB,4         ;check column 1
                        retlw   '4'
                        btfss   PORTB,5         ;check column 2
                        retlw   '5'
                        btfss   PORTB,6         ;check column 3
                        retlw   '6'
                        btfss   PORTB,7         ;check column 4
                        retlw   'B'
                        ;
                        movlw   0xfb            ;1111 1011
                        movwf   PORTB
                        call    delayf
                        btfss   PORTB,4         ;check column 1
                        retlw   '7'
                        btfss   PORTB,5         ;check column 2
                        retlw   '8'
                        btfss   PORTB,6         ;check column 3
                        retlw   '9'
                        btfss   PORTB,7         ;check column 4
                        retlw   'C'
                        ;
                        movlw   0xf7            ;1111 0111
                        movwf   PORTB
                        call    delayf 
                        btfss   PORTB,4         ;check column 1
                        retlw   '*'
                        btfss   PORTB,5         ;check column 2
                        retlw   '0'
                        btfss   PORTB,6         ;check column 3
                        retlw   '#'
                        btfss   PORTB,7         ;check column 4
                        retlw   'D'
                        retlw   0x00
                        
;****ENABLING DATA FROM PIC18F4550 TO BE SENT TO LCD**********************************************************************
pulse                   bsf     PORTC,2         ;E=1
                        call    sdelay          ;JUMP TO SDELAY SUBROUTINE
                        bcf     PORTC,2         ;E=0
                        return                  ;RETURN FROM SUBROUTINE
                        
;****CUSTOM DELAY SUBROUTINE*********************************************************************************************
delayf                  movlw   D'255'          ;MOVE 255 IN DECIMAL TO WORKING REGISTER
                        movwf   delrg           ;MOVE 255 IN DECIMAL TO DELRG REGISTER
delay                   decf    delrg,f         ;DECREASE THE VALUE IN DELRG BY 1 IN DECIMAL
                        bnz     delay           ;JUMP TO DELAY LABEL IF ZERO FLAG IS NOT SET HIGH
                        return                  ;EXIT FROM SUBROUTINE
                        
ldelay                  call    sdelay          ;JUMP TO SDELAY LABEL
                        decfsz  delayyy,f       ;DECREMENT THE VALUE IN DELAYYY BY 1 IN DECIMAL
                        bra     ldelay          ;JUMP BACK TO LDELAY LABEL
                        return                  ;EXIT FROM SUBROUTINE
                        
sdelay                  decfsz  delayyyy,f      ;DECREMENT THE VALUE IN DELAYYYY BY 1 IN DECIMAL
                        bra     sdelay          ;JUMP BACK TO SDELAY LABEL
                        return                  ;EXIT FROM SUBROUTINE

;****CHECK IF LCD IS BUSY BY CHECKING BUSY FLAG***************************************************************************
checkBF                 setf    TRISD           ;set PORTD as inputs
                        bcf     PORTC,0         ;RS=0
                        bsf     PORTC,1         ;R/W=1
                        nop
                        bsf     PORTC,2         ;E=1
                        nop
                        movf    PORTD,0         ;MOVE READ DATA TO WORKING REGISTER
                        bcf     PORTC,2         ;E=0
                        nop
                        btfsc   WREG,7          ;CHECK BUSY FLAG OR 7TH BIT, SKIP NEXT INSTRUCTION IF BF LOW
                        bra     checkBF         ;RECHECK BUSY FLAG
                        clrf    TRISD           ;SET PORTD AS OUTPUT
                        return                  ;RETURN FROM SUBROUTINE
                        
;****CHECK DDRAM ADDRESS**************************************************************************************************
checkDDRAM              movlw   0x50            ;ADDRESS OF 0X50 IS THE 17TH LOCATION IN THE SECOND ROW
                        movwf   checkDDR        ;MOVE 0X50 LITERAL TO CHECKDDR REGISTER
                        setf    TRISD           ;SET PORTD AS INPUT
                        bcf     PORTC,0         ;RS=0
                        bsf     PORTC,1         ;R/W=1
                        nop
                        bsf     PORTC,2         ;E=1
                        nop
                        movf    PORTD,0         ;MOVE READ DATA TO WORKING REGISTER
                        bcf     PORTC,2         ;E=0
                        nop
                        clrf    TRISD           ;SET PORTD AS OUTPUT
                        bcf     WREG,7          ;CLEAR BUSY FLAG OR 7TH BIT
                        cpfseq  checkDDR        ;COMPARE READ ADDRESS AND ADDRESS IN CHECKDDR REGISTER, SKIP NEXT IF EQUAL
                        bra     exit            ;JUMP TO EXIT SUBROUTINE
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;R/W=0
                        movlw   0xc0            ;SHIFT DISPLAY TO LEFT, CURSOR TO RIGHT
                        movwf   PORTD           ;WRITE ADDRESS TO PORTD TO WRITE TO LCD DDRAM
                        call    pulse           ;SEND ADDRESS FROM PORTD TO LCD
                        call    sdelay          ;JUMP TO SDELAY SUBROUTINE
exit                    return                  ;RETURN FROM SUBROUTINE

;****BACKSPACE FUNCTIONALITY***********************************************************************************************
back                    setf    TRISD           ;SET PORTD AS INPUTS
                        bcf     PORTC,0         ;RS=0
                        bsf     PORTC,1         ;R/W=1
                        nop
                        bsf     PORTC,2         ;E=1
                        nop
                        movf    PORTD,0         ;MOVE CURRENT DDRAM ADDRESS TO WORKING REGISTER
                        bcf     PORTC,2         ;E=0
                        nop
                        clrf    TRISD           ;SET PORTD AS OUTPUTS
                        movwf   current_A       ;MOVE DDRAM ADDRESS TO CURRENT_A
                        movlw   0x01
                        subwf   current_A       ;CURRENT_A - 1
                        movf    current_A,0     ;MOVE CURRENT_A TO WORKING REGISTER
                        bsf     WREG,7          ;SET BIT 7 FOR DDRAM ADDRESSING INSTRUCTION
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;R/W=0
                        movwf   PORTD           ;WRITE ADDRESS TO DDRAM
                        call    pulse           ;SEND ADDRESS FROM PORTD TO LCD
                        call    sdelay          ;JUMP TO SDELAY SUBROUTINE
                        return                  ;RETURN FROM SUBROUTINE
                        
;********************************************************************************
data1                   db      "Enter key:",0
                        End
