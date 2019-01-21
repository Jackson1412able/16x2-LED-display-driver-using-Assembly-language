;************************************************************************************************************************     	
;       THIS PROGRAM WAS DEVELOPED FOR A MICROCHIP PIC18F4550 MICROCONTROLLER TO CARRY OUT FUNCTIONAL TESTS.            ;
;       THE SEQUENCE OF THE FUNCTIONAL TEST IS AS FOLLOWS:                                                              ;
;                                                                                                                       ;
;       DISPLAY AN INITIAL SCROLLING MESSAGE (MSG0) ON ROW 1 OF THE LCD DISPLAY AS FOLLOWS:                             ;
;         Welcome to the Task #2D functional tests. Press switch 1 (SW1 is the one connected to RB0)                    ;
;         of your SK40C board to light up LED1 (i.e., LED connected to RB6).                                            ;
;                                                                                                                       ;
;       AFTER SW1 HAS BEEN PRESSED ONCE, DISPLAY THE FOLLOWING SCROLLING MESSAGE (MSG1) ON THE FIRST ROW OF THE         ;
;       LCD DISPLAY:                                                                                                    ;
;         If LED1 lights up, your SW1 and LED1 connections are correct. Next, press switch 2                            ;
;         (SW2 is the one connected to RB1) to light up LED2 (i.e., LED connected to RB7).                              ;
;                                                                                                                       ;
;       AFTER SW2 HAS BEEN PRESSED ONCE, DISPLAY THE FOLLOWING SCROLLING MESSAGE (MSG2) ON THE FIRST ROW OF THE         ;
;       LCD DISPLAY:                                                                                                    ;
;         If LED2 lights up, your SW2 and LED2 connections are correct. Next, press SW1 to enter the keypad             ;
;         testing zone.                                                                                                 ;
;                                                                                                                       ;
;       AFTER SW1 HAS BEEN PRESSED ONCE, DISPLAY THE FOLLOWING SCROLLING MESSAGE (MSG3) ON THE FIRST ROW OF THE         ;
;       LCD DISPLAY:                                                                                                    ;
;         This is a keypad testing zone! Now, press any button on the keypad. Use the "C" button as a "backspace"       ;
;         key. Once you are satisfied with the keypad functionality, you can exit the test by pressing SW2.             ;
;                                                                                                                       ;
;       AFTER SW2 HAS BEEN PRESSED ONCE, DISPLAY THE FOLLOWING SCROLLING MESSAGE (MSG4) ON THE FIRST ROW OF THE         ;
;       LCD DISPLAY:                                                                                                    ;
;         CONGRATULATIONS!!! You have successfully evaluated your test code for Task 1D. Proceed immediately to         ;
;         Task 1E. To go back to the welcome page, press SW1.                                                           ;
;                                                                                                                       ;
;       THE LCD DISPLAY MUST SHOW MSG0 ONCE SW1 IS PRESSED.                                                             ;
;                                                                                                                       ;
;************************************************************************************************************************    
;       THE CONNECTIONS OF THE MICROCHIP PIC18F4550 MICROCONTROLLER AND THE 16X2 LCD DISPLAY ARE SHOWN AS BELOW:        ;
;                                                                                                                       ;
;                       PORTD output to LCD display                                                                     ;
;                       RC0 = RS                                                                                        ;
;                       RC1 = R/W                                                                                       ;
;                       RC2 = E                                                                                         ;
;                       RB0~RB3 = R1,R2,R3,R4 of keypad (inputs)                                                        ;
;                       RB4~RB7 = C1,C2,C3,C4 of keypad (outputs)                                                       ;
;                       RA0 = Switch 1                                                                                  ;
;                       RA1 = Switch 2                                                                                  ;
;                       RC6 = LED1                                                                                      ;
;                       RC7 = LED2                                                                                      ;
;                                                                                                                       ;
;************************************************************************************************************************

;*** DECLARING AND CONFIGURING A MICROCONTROLLER ************************************************************************
                        LIST P=18F4550, F=INHX32 ;directive to define processor
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
R2                      equ     0x07            ;R2 REGISTER ASSIGNED TO LOCATION 0X07
R3                      equ     0x08            ;R3 REGISTER ASSIGNED TO LOCATION 0X08
checkDDR                equ     0x0d            ;CHECKDDR REGISTER ASSIGNED TO LOCATION 0X0D

;INITIALIZATION
                        org     0x100
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
                        movlw   0x30            ;0011 0000
                        movwf   TRISC           ;RS=RC0,RW=RC1,E=RC2,SW1=RC4,SW2=RC5,LED1=RC6,LED2=RC7
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
                        movlw   0x0C            ;000 1100            
                        movwf   PORTD           ;C=1(cursor on), B=0(blink off)
                        call    pulse           ;SEND INSTRUCTION TO LCD
                        call    checkBF         ;CHECK IF LCD IS BUSY
                        
;****TASK FROM MSG0******************************************************************************************************
sendMSG0                movlw   upper(MSG0)     ;COPY CONTENTS FROM UPPER BYTE OF MSG0
                        movwf   TBLPTRU         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER UPPER BYTE
                        movlw   high(MSG0)      ;COPY CONTENTS FROM HIGH BYTE OF MSG0
                        movwf   TBLPTRH         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER HIGH BYTE
                        movlw   low(MSG0)       ;COPY CONTENTS FROM LOW BYTE OF MSG0
                        movwf   TBLPTRL         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER LOW BYTE
                        call    DDRAM_0         ;SET ENTRY MODE AND SET DDRAM LOCATION
                        call    clearr          ;CLEAR SCREEN SUBROUTINE
                    
lcdoutput1              call    checkDDRAM      ;CHECK DDRAM ADDRESS
                        tblrd*+                 ;INCREMENT TABLE POINTER
                        movf    TABLAT,w        ;EXTRACT CHARACTER AND COPY TO WORKING REGISTER
                        iorlw   0x00            ;CHECK IF IT IS 0X00 OR 0 IN DECIMAL AND HEXADECIMAL, IF 0, ZF HIGH
                        btfsc   STATUS,2        ;IF ZERO FLAG HIGH, DONT SKIP NEXT INSTRUCTION
                        bra     waitSW1         ;JUMP TO WAITSW1 LABEL
                        call    display         ;PROCEED TO SEND DATA TO LCD
                        call    l2delay         ;DELAY
                        goto    lcdoutput1      ;GO TO LCDOUTPUT1 LABEL
waitSW1                 btfss   PORTA,0         ;TEST SW1 IS PRESSED OR NOT
                        bra     lightLED1       ;JUMP TO LIGHTLED1 LABEL
                        bra     waitSW1         ;JUMP TO WAITSW1 LABEL
lightLED1               bsf     PORTC,6         ;LED1 HIGH WHEN SW1 IS PRESSED

;****TASK FROM MSG1******************************************************************************************************
sendMSG1                movlw   upper(MSG1)     ;COPY CONTENTS FROM UPPER BYTE OF MSG1
                        movwf   TBLPTRU         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER UPPER BYTE
                        movlw   high(MSG1)      ;COPY CONTENTS FROM HIGH BYTE OF MSG1
                        movwf   TBLPTRH         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER HIGH BYTE
                        movlw   low(MSG1)       ;COPY CONTENTS FROM LOW BYTE OF MSG1
                        movwf   TBLPTRL         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER LOW BYTE
                        call    DDRAM_0         ;SET ENTRY MODE AND SET DDRAM LOCATION
                        call    clearr          ;CLEAR SCREEN SUBROUTINE
                        
lcdoutput2              call    checkDDRAM      ;CHECK DDRAM ADDRESS
                        tblrd*+                 ;INCREMENT TABLE POINTER
                        movf    TABLAT,w        ;EXTRACT CHARACTER AND COPY TO WORKING REGISTER
                        iorlw   0x00            ;CHECK IF IT IS 0X00 OR 0 IN DECIMAL AND HEXADECIMAL, IF 0, ZF HIGH
                        btfsc   STATUS,2        ;IF ZERO FLAG HIGH, DONT SKIP NEXT INSTRUCTION
                        bra     waitSW2         ;JUMP TO WAITSW2 LABEL
                        call    display         ;PROCEED TO SEND DATA TO LCD
                        call    l2delay         ;DELAY
                        goto    lcdoutput2      ;GO TO LCDOUTPUT2 LABEL
waitSW2                 btfss   PORTA,1         ;TEST SW2 IS PRESSED OR NOT
                        bra     lightLED2       ;JUMP TO LIGHTLED2 LABEL
                        bra     waitSW2         ;JUMP TO WAITSW2 LABEL
lightLED2               bsf     PORTC,7         ;LED2 high when SW2 is pressed

;****TASK FROM MSG2******************************************************************************************************
sendMSG2                movlw   upper(MSG2)     ;COPY CONTENTS FROM UPPER BYTE OF MSG2
                        movwf   TBLPTRU         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER UPPER BYTE
                        movlw   high(MSG2)      ;COPY CONTENTS FROM HIGH BYTE OF MSG2
                        movwf   TBLPTRH         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER HIGH BYTE
                        movlw   low(MSG2)       ;COPY CONTENTS FROM LOW BYTE OF MSG2
                        movwf   TBLPTRL         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER LOW BYTE
                        call    DDRAM_0         ;SET ENTRY MODE AND SET DDRAM LOCATION
                        call    clearr          ;CLEAR SCREEN SUBROUTINE
                        
lcdoutput3              call    checkDDRAM      ;CHECK DDRAM ADDRESS
                        tblrd*+                 ;INCREMENT TABLE POINTER
                        movf    TABLAT,w        ;EXTRACT CHARACTER AND COPY TO WORKING REGISTER
                        iorlw   0x00            ;CHECK IF IT IS 0X00 OR 0 IN DECIMAL AND HEXADECIMAL, IF 0, ZF HIGH
                        btfsc   STATUS,2        ;IF ZERO FLAG HIGH, DONT SKIP NEXT INSTRUCTION
                        bra     SW1_for_keypad  ;JUMP TO SW1_FOR_KEYPAD LABEL
                        call    display         ;PROCEED TO SEND DATA TO LCD
                        call    l2delay         ;DELAY
                        goto    lcdoutput3      ;GO TO LCDOUTPUT3 LABEL
SW1_for_keypad          btfss   PORTA,0         ;TEST SW1 IS PRESSED OR NOT
                        bra     sendMSG3        ;JUMP TO SENDMSG3 LABEL
                        bra     SW1_for_keypad  ;JUMP TO SW1_FOR_KEYPAD LABEL
                        
;****TASK FROM MSG3******************************************************************************************************
sendMSG3                movlw   upper(MSG3)     ;COPY CONTENTS FROM UPPER BYTE OF MSG3
                        movwf   TBLPTRU         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER UPPER BYTE
                        movlw   high(MSG3)      ;COPY CONTENTS FROM HIGH BYTE OF MSG3
                        movwf   TBLPTRH         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER HIGH BYTE
                        movlw   low(MSG3)       ;COPY CONTENTS FROM LOW BYTE OF MSG3
                        movwf   TBLPTRL         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER LOW BYTE
                        call    DDRAM_0         ;SET ENTRY MODE AND SET DDRAM LOCATION
                        call    clearr          ;CLEAR SCREEN SUBROUTINE
                        
lcdoutput4              call    checkDDRAM      ;CHECK DDRAM ADDRESS
                        tblrd*+                 ;INCREMENT TABLE POINTER
                        movf    TABLAT,w        ;EXTRACT CHARACTER AND COPY TO WORKING REGISTER
                        iorlw   0x00            ;CHECK IF IT IS 0X00 OR 0 IN DECIMAL AND HEXADECIMAL, IF 0, ZF HIGH
                        btfsc   STATUS,2        ;IF ZERO FLAG HIGH, DONT SKIP NEXT INSTRUCTION
                        bra     keyoutput       ;JUMP TO KEYOUTPUT
                        call    display         ;PROCEED TO SEND DATA TO LCD
                        call    l2delay         ;DELAY
                        goto    lcdoutput4      ;GO TO LCDOUTPUT4 LABEL
                        
keyoutput
senddata6               movlw   upper(data6)    ;COPY CONTENTS FROM UPPER BYTE OF DATA6
                        movwf   TBLPTRU         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER UPPER BYTE
                        movlw   high(data6)     ;COPY CONTENTS FROM HIGH BYTE OF DATA6
                        movwf   TBLPTRH         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER HIGH BYTE
                        movlw   low(data6)      ;COPY CONTENTS FROM LOW BYTE OF DATA6
                        movwf   TBLPTRL         ;COPY CONTENTS TO PROGRAM MEMORY TABLE POINTER LOW BYTE
                        call    DDRAM_0         ;SET ENTRY MODE AND SET DDRAM LOCATION
                        call    clearr          ;CLEAR SCREEN SUBROUTINE
                        
lcdoutput6              call    checkDDRAM      ;CHECK DDRAM ADDRESS
                        tblrd*+                 ;INCREMENT TABLE POINTER
                        movf    TABLAT,w        ;EXTRACT CHARACTER AND COPY TO WORKING REGISTER
                        iorlw   0x00            ;CHECK IF IT IS 0X00 OR 0 IN DECIMAL AND HEXADECIMAL, IF 0, ZF HIGH
                        btfsc   STATUS,2        ;IF ZERO FLAG HIGH, DONT SKIP NEXT INSTRUCTION
                        bra     aiyoyoo         ;JUMP TO AIYOYOO LABEL
                        call    display         ;PROCEED TO SEND DATA TO LCD
                        call    delayf          ;DELAY
                        goto    lcdoutput6      ;GO TO LCDOUTPUT6 LABEL
                        
;****DETECTING KEY PRESSED***********************************************************************************************
aiyoyoo                 btfss   PORTA,1         ;TEST SW2 IS PRESSED OR NOT
                        bra     message5        ;JUMP TO MESSAGE5 BRANCH
                        call    displaykey      ;DISPLAYKEY SUBROUTINE TO SET DDRAM ADDRESS AT SECOND ROW FIRST COLUMN
                        clrf    checkzero       ;CLEAR CHECKZERO CONTENTS
                        call    getkey          ;GET ENTERED KEY
                        iorwf   checkzero       ;CHECK IF THERE IS ANY ENTERED KEY
                        bz      aiyoyoo         ;DETECT ANY KEY PRESSED AGAIN IF NO KEY ENTERED
                        movwf   pressed_key     ;MOVE PRESSED KEY EQ VALUE TO PRESSED_KEY REGISTER
waitkeyrelease          clrf    checkzero       ;CLEAR CHECKZERO CONTENTS
                        call    getkey          ;GET ENTERED KEY
                        iorwf   checkzero       ;CHECK IF THE PRESSED KEY IS STILL PRESSING
                        bnz     waitkeyrelease  ;IF THE KEY IS STILL PRESSED, JUMP BACK TO WAITKEYRELEASE BRANCH
                        movf    pressed_key,0   ;MOVE PRESSED KEY VALUE TO WORKING REGISTER
                        call    display         ;DISPLAY ENTERED KEY
                        nop
                        bra     aiyoyoo         ;JUMP BACK TO AIYOYOO LABEL TO DETECT ANY KEY PRESSES AGAIN
                        
;****TASK FROM MSG4******************************************************************************************************
message5                movlw   upper(MSG4)     ;COPY CONTENTS FROM UPPER BYTE OF MSG4
                        movwf   TBLPTRU         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER UPPER BYTE
                        movlw   high(MSG4)      ;COPY CONTENTS FROM HIGH BYTE OF MSG4
                        movwf   TBLPTRH         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER HIGH BYTE
                        movlw   low(MSG4)       ;COPY CONTENTS FROM LOW BYTE OF MSG4
                        movwf   TBLPTRL         ;COPY CONTENTS POINTED BY PROGRAM MEMORY TABLE POINTER LOW BYTE
                        call    DDRAM_0         ;SET ENTRY MODE AND SET DDRAM LOCATION
                        call    clearr          ;CLEAR SCREEN SUBROUTINE
lcdoutput5              call    checkDDRAM      ;CHECK DDRAM ADDRESS
                        tblrd*+                 ;INCREMENT TABLE POINTER
                        movf    TABLAT,w        ;EXTRACT CHARACTER AND COPY TO WORKING REGISTER
                        iorlw   0x00            ;CHECK IF IT IS 0X00 OR 0 IN DECIMAL AND HEXADECIMAL, IF 0, ZF HIGH
                        btfsc   STATUS,2        ;IF ZERO FLAG HIGH, DONT SKIP NEXT INSTRUCTION
                        bra     return_ma?      ;IF MSG4 DISPLAYED COMPLETELY, JUMP TO RETURN_MA? LABEL
                        call    display         ;PROCEED TO SEND DATA TO LCD
                        call    l2delay         ;DELAY
                        goto    lcdoutput5      ;JUMP BACK TO LCDOUTPUT5 LABEL TO PRINT MSG4
return_ma?              btfss   PORTA,0         ;test SW1 is pressed or not
                        bra     sendMSG0        ;RESTART PROGRAM BY JUMPING BACK TO MSG0
                        bra     return_ma?      ;IF SW1 NOT PRESSED, CHECK AGAIN UNTIL SW1 IS PRESSED
                        
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
l2delay                 movlw   D'50'           ;MOVE 50 IN DECIMAL TO WORKING REGISTER
                        movwf   R2              ;MOVE 50 IN DECIMAL TO R2 REGISTER
check1                  movlw   D'50'           ;MOVE 50 IN DECIMAL TO WORKING REGISTER
                        movwf   R3              ;MOVE 50 IN DECIMAL TO R3 REGISTER
check2                  nop
                        nop
                        decf    R3,f            ;DECREASE THE VALUE IN R3 BY 1 IN DECIMAL
                        bnz     check2          ;JUMP TO DELAY CHECK2 IF ZERO FLAG IS NOT SET HIGH
                        decf    R2,f            ;DECREASE THE VALUE IN R2 BY 1 IN DECIMAL
                        bnz     check1          ;JUMP TO DELAY CHECK1 IF ZERO FLAG IS NOT SET HIGH
                        return
                        
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
                        
;****CLEAR DISPLAY********************************************************************************************************
clearr                  bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;RW=0
                        movlw   0x01            ;CLEAR DISPLAY
                        movwf   PORTD           ;WRITE ADDRESS TO DDRAM
                        call    pulse           ;SEND DATA FROM PORTD TO LCD
                        call    checkBF         ;CHECK BUSY FLAG OF LCD
                        return                  ;RETURN FROM SUBROUTINE
                        
;****CHECK DDRAM ADDRESS**************************************************************************************************
checkDDRAM              movlw   0x10            ;0X10 IS THE 17TH ADDRESS ON THE FIRST ROW OF LCD
                        movwf   checkDDR        ;MOVE 0X10 TO CHECKDDR ADDRESS
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
                        movlw   0x07            ;DDRAM ADDRESS INCREASES AS CHARACTER IS DISPLAYED, ACCOMPANIES WITH DISPLAY SHIFT
                        movwf   PORTD           ;WRITE ADDRESS TO DDRAM
                        call    pulse           ;SEND DATA FROM PORTD TO LCD
                        call    checkBF         ;CHECK BUSY FLAG OF LCD
exit                    return                  ;RETURN FROM SUBROUTINE
                        
;****DISPLAY THE ENTERED KEY AT A FIXED ADDRESS ON LCD DISPLAY************************************************************
displaykey              bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;R/W=0
                        movlw   0x8c            ;set DDRAM address at 0x0C
                        movwf   PORTD           ;write address to DDRAM
                        call    pulse           ;SEND DATA FROM PORTD TO LCD
                        call    checkBF         ;CHECK BUSY FLAG OF LCD
                        return                  ;RETURN FROM SUBROUTINE
                        
;****LCD FUNCTION SETUP***************************************************************************************************
DDRAM_0                 bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;R/W=0
                        movlw   0x06            ;SHIFT DISPLAY TO LEFT, CURSOR TO RIGHT
                        movwf   PORTD           ;WRITE ADDRESS TO DDRAM
                        call    pulse           ;SEND DATA FROM PORTD TO LCD
                        call    checkBF         ;CHECK BUSY FLAG OF LCD
                        bcf     PORTC,0         ;RS=0
                        bcf     PORTC,1         ;R/W=0
                        movlw   0x80            ;SET DDRAM ADDRESS AT 0X00, 1ST ROW 1ST COLUMN
                        movwf   PORTD           ;WRITE ADDRESS TO DDRAM
                        call    pulse           ;SEND DATA FROM PORTD TO LCD
                        call    checkBF         ;CHECK BUSY FLAG OF LCD
                        return                  ;RETURN FROM SUBROUTINE
                        
;**************************************************************************************************************************
MSG0                    db      "Welcome to the Task #2D functional tests. Press switch 1 (SW1 is the one connected to RA0) light up LED1 (LED connected to RC6).",0
MSG1                    db      "If LED1 lights up, your SW1 and LED1 connections are correct. Next, press switch 2 (SW2 is the one connected to RA1) to light up LED2 (LED connected to RC7).",0
MSG2                    db      "If LED2 lights up, your SW2 and LED2 connections are correct. Next, press SW1 to enter the keypad testing zone.",0
MSG3                    db      "This is a keypad testing zone! Now, press any button on the keypad. Once you are satisfied with the keypad functionality, you can exit the test by pressing SW2.",0
MSG4                    db      "CONGRATULATIONS!!! You have successfully evaluated your test code for Task 1D. Proceed immediately to Task 1E. To go back to the welcome page, press SW1.",0
data6                   db      "Key entered:",0
                        End
