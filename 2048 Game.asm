[org 0x0100]
jmp start
b1: dw 0
b2: dw 0
b3: dw 0
b4: dw 0
b5: dw 0
b6: dw 0
b7: dw 0
b8: dw 0
b9: dw 0
b10: dw 0
b11: dw 2
b12: dw 2
b13: dw 0
b14: dw 0
b15: dw 0
b16: dw 0
winvar: db 0
losevar: db 0
;randomNum: db 0
genrand: dw 0
randomnumbercount: dw 0

winstat: db'Congratulations Uou have Won the Game'
losestat: db'You Lose The Game Try AGain'

buffer:       times 81 db 0           		
EnterName:	  db	10, 13, 'Please enter your name: $'
maxlength:    dw	80  
greetings:    db	10, 13, 'Welcome and hello $'  	; greetings message


partners: db ' Developed by Abdullah Basim(21L-5320) '
	partner_len: dw 51
	
game_n: db '          2 0 4 8          '
	game_len: dw 27
	
	play: db ' Press "P" to PLAY GAME '
	play_len: dw 24

	playgame: db 'Press "W" for Up "S" for Down "A" for left D for Right'
	playgame_len: dw 54
	
	escape_: db ' Press "ESCAPE" to QUIT '
	escape_len: dw 24



message: db 'Welcome to 2048 Game' 
stat: db'Press Any key To Continue'
shifting : db 0
oldisr dd 0
randomNum: db 0



;--------------------------------------------------------------------
; subroutine to clear the screen with animation
;--------------------------------------------------------------------
			
animation_removing:

    push ax
	push cx
	push si
	push di
	push bx
	push bp

	mov cx,40
	mov si,3920
	mov di,78
	mov ax,0xb800
	mov es,ax
	mov bx,0
	mov bp,3918
	mov bx,80

	mov cx,10
	animationLoop:

		push cx
		;add bx,80
		mov cx,40
		;sub bp,8

		Animaloper:

			mov word[es:di],0x0000				;setting colour
            mov word[es:di+160],0x0000
			
			call delay
			mov word[es:si],0x0000				;removing previous colour
            mov word[es:si-160],0x0000

			mov word[es:bx],0x0000				;setting colour
            mov word[es:bx+160],0x0000


			mov word[es:bp],0x0000				;removing previous colour
            mov word[es:bp-160],0x0000
            
			call delay
			
			add bx,2
			add si,2
			sub di,2
			sub bp,2


		loop Animaloper

		sub si,480
		add di,480
		sub bp,320
		add bx,320

		pop cx

	loop animationLoop

	pop bp
	pop bx
	pop di
	pop si
	pop cx
	pop ax

ret

;--------------------------------------------------------------------
; subroutine to delay the screen
;--------------------------------------------------------------------
delay:     
        push cx
        mov cx, 0x55FF


loop1:                     
        loop loop1
        mov cx, 0x7777


;loop2:                     
        ;loop loop2
        pop cx
        ret
;-----------delay with more time---------------	
delay2:

	push cx
	mov cx, 20 ; change the values  to increase delay time

	_delay_loop1:
	
	push cx
	mov cx, 0xFFFF

	_delay_loop2:

	loop _delay_loop2

	pop cx
	loop _delay_loop1

	pop cx
ret
;--------------------------------------------------------------------
; subroutine to for welcome screeen
;--------------------------------------------------------------------
				
WelcomeBackGround:
              push es
			  push ax
			  push cx
			  push di
			  push si
			  push dx
 
	
			  mov ax, 0xb800
			  mov es, ax
			  mov ax, 0x07db
			  mov cx,1920
			  cld
			  rep stosw    

				mov ax, 0x9efe;design on upper line
				mov di, 640;leave some space above
				mov cx, 80;run for only 1 line
				cld
				rep stosw;welcome line 1
				
				add di, 174
				mov ah, 0x0f
				mov al, 'W'
				mov word[es:di], ax
				add di, 22
				mov al, 'E'
				mov word[es:di], ax
				add di, 22
				mov al, 'L'
				mov word[es:di], ax
				add di, 22
				mov al, 'C'
				mov word[es:di], ax
				add di, 22
				mov al, 'O'
				mov word[es:di], ax
				add di, 22
				mov al, 'M'
				mov word[es:di], ax
				add di, 22
				mov al, 'E'
				mov word[es:di], ax
				add di, 174

				mov ax, 0x9efe;design on upper line
				mov cx, 80;run for only 1 line
				cld
				rep stosw;welcome line 2

	 
				
          pop dx
          pop si
          pop di
          pop cx
          pop ax
          pop es
          ret

welcomeText:

       push es
			  push ax
			  push cx
			  push di
			  push si
			  push dx
 

			  mov ah, 0x13
		      mov bh, 0			
		
		      mov bl, 01000111B
		      mov cx, [game_len]	
		      mov dx, 0x0B1C		
		
			  push ds
		      pop es				
		      mov bp, game_n		
		
		      INT 0x10			
		
			  mov  dx, EnterName      ; User will be asked to enter his/her name               
			  mov  ah, 9              ; service 9 – write string               
			  int  0x21               ; dos services
			  mov  cx, [maxlength]    ; load maximum length in cx              
			  mov  si, buffer         ; point si to start of buffer 
 
    takingName:  	
				mov  ah, 1              ; service 1 – read character               
			    int  0x21               ; dos services 
				cmp  al, 13             ; is enter pressed               
				je   nextScenerio       ; yes, leave input               
				mov  [si], al           ; no, save this character               
				inc  si                 ; increment buffer pointer               
				loop takingName
           ; repeat for next input char 
 
nextScenerio: mov byte [si], '$'     ; append $ to user input 
	
                        ; dos services 
		 mov dx, greetings      ; greetings message               
			  mov ah, 9              ; service 9 – write string               
			  int 0x21               ; dos services 
 
              mov dx, buffer         ; user input buffer               
			  mov ah, 9              ; service 9 – write string               
			  int 0x21      
		
		 pop dx
          pop si
          pop di
          pop cx
          pop ax
          pop es
          ret
;--------------------------------------------------------------------
; subroutine to for starting screen
;--------------------------------------------------------------------


startingScreen:
              
			  push es
			  push ax
			  push cx
			  push di
			  push si
			  push dx
			  
     call clrscr
	 
	          mov ah, 0x13
		      mov bh, 0			
		
		     
		    

			 mov bl, 11111100B
		      mov cx, [play_len]	
		      mov dx, 0x0319		
		
			  push ds
		      pop es				
		      mov bp, play		
		
		      INT 0x10	
			  
			   mov bl, 11000000B
		      mov cx, [escape_len]	
		      mov dx, 0x0519		
		
			  push ds
		      pop es				
		      mov bp, escape_		
		
		      INT 0x10
               
               mov bl, 01000111B
			   

			  mov cx, [playgame_len]	
		      mov dx, 0x070A		
		
			  push ds
		      pop es				
		      mov bp, playgame		
		
		      INT 0x10
			  
			 ; mov bl, 01111001B
			; mov bl, 01011010B
			   mov bl, 01100000B
			  mov cx, [game_len]	
		      mov dx, 0x0B1C		
		
			  push ds
		      pop es				
		      mov bp, game_n	
		
		      INT 0x10
			  
              
			  
			  
			  mov bl, 01010111B
			  ;mov bl, 01111111B
		      mov cx, [partner_len]	
		      mov dx, 0x171D		
		
			  push ds
		      pop es				
		      mov bp, partners		
		
		      INT 0x10


 pop dx
          pop si
          pop di
          pop cx
          pop ax
          pop es
          ret
		  
		  
 displaystartmenu:
		 call clrscr

	    call WelcomeBackGround
		call welcomeText
		call animation_removing
		call startingScreen
	;loop for checking interrupts of welcome screen
	looper_:

	    mov ah, 0 ; service 0 – get keystroke
		int 0x16 ; call BIOS keyboard service

	    cmp al,27 ;Check for Escape Key    
		je gameExit

		cmp al,0x70
		je gameExit

		cmp al,0x50
		je gameExit

	jmp looper_
	
	;call StarttngSound
		
	gameExit:
	ret 



printnum: 
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov ax, [bp+4] ; load number in ax 
 mov bx, 10 ; use base 10 for division 
 mov cx, 0 ; initialize count of digits 
nextdigit: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz nextdigit ; if no divide it again 
 
 mov di, [bp+6] ; point di to top left column 

nextpos: 
 pop dx ; remove a digit from the stack 
 mov dh, 0x07 ; use normal attribute  
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextpos ; repeat for all digits on stack
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax 
 pop es 
 pop bp 
 ret 4 









printblock:
push bp
mov bp,sp
pusha


mov ax,0xb800
mov es,ax

mov di,[bp+4]
mov dx,[bp+6]
;mov dh ,0x07

mov al, 0xdb
mov ah , 0x07

mov cx,6
rep stosw

add di,160
sub di,2

std 
mov cx,6
rep stosw


add di,160
add di,2

cld
mov cx,6
rep stosw

sub di,170

mov ax,dx
push di
push ax
call printnum


popa
pop bp
ret 4



printstruct:
pusha

mov ax,0xb800
mov es,ax


mov cx,16
mov di,38
mov ax,0x07b3

lo:
stosw
sub di,2
add di,160
loop lo 


mov ax,0x07c4
mov cx,28
rep stosw 


mov ax,0x07b3
mov cx,16

lo2:
stosw
sub di,2
sub di,160
loop lo2 

std
mov ax,0x07c4
mov cx,28
rep stosw 

cld

mov di,0


mov si,b1
mov dx,200

mov bx,4

l1:
mov cx,4
l2:
mov ax,[si]
push ax
push dx
call printblock 
add si,2
add dx,14
loop l2

sub dx,56
add dx,640
sub dx,160
mov di,dx
mov cx,27
mov ax,0x07c4
rep stosw

add dx,160

dec bx
cmp bx,0
jnz l1

mov cx,16
mov di,52
mov ax,0x07b3
lo3:
stosw
sub di,2
add di,160
loop lo3 



mov cx,16
mov di,66
mov ax,0x07b3

lo4:
stosw
sub di,2
add di,160
loop lo4 


mov cx,16
mov di,80
mov ax,0x07b3

lo5:
stosw
sub di,2
add di,160
loop lo5 



popa 
ret


printwelcome:
pusha
mov ah, 0x13 ; service 13 - print string 
 mov al, 1 ; subservice 01 – update cursor 
 mov bh, 0 ; output on page 0 
 mov bl, 6 ; normal attrib 
 mov dx, 0x0110 ; row 2 column 10 
 mov cx, 20 ; length of string 
 push cs 
 pop es ; segment of string 
 mov bp, message ; offset of string 
int 0x10 ; call BIOS video service 

popa
ret

printstar:
pusha

mov ax,0xb800
mov es,ax
mov di,30
mov ah, 0x07
mov al, 0xdb
mov cx,22
rep stosw 

mov di,190
stosw 

add di,40 
stosw

mov di,350
mov cx,22
rep stosw


popa
ret


printstat
pusha
mov ah, 0x13 ; service 13 - print string 
 mov al, 1 ; subservice 01 – update cursor 
 mov bh, 0 ; output on page 0 
 mov bl, 6 ; normal attrib 
 mov dx, 0x0A10 ; row 2 column 10 
 mov cx, 25 ; length of string 
 push cs 
 pop es ; segment of string 
 mov bp, stat ; offset of string 
int 0x10 ; call BIOS video service 

popa
ret

clrscr:
mov ax,0xb800
mov es,ax
mov di,0
mov ax,0x0720
mov cx,2000
rep stosw
ret


kbisr: push ax 
 push es 
 mov ax, 0xb800 
 mov es, ax ; point es to video memory 
 
 game:
 
 in al, 0x60 ; read a char from keyboard port 
 
 cmp al, 0x01
 je terminategame
 
 
 cmp al, 0x1e ; is the key left shift 
 jne nextcmp ; no, try next comparison 
 mov byte [es:0], 'L' ; yes, print R at top left 
 ;mov word [b1],206
 call left
 call printstruct
 jmp nomatch ; leave interrupt routine 
 
 
 
nextcmp: cmp al, 0x20 ; is the key right shift 
 jne nextcmp2 ; no, leave interrupt routine 
 mov byte [es:0], 'R' ; yes, print R at top left
  call right
  call printstruct

 jmp nomatch ; leave interrupt routine 
 
 nextcmp2: cmp al, 0x11 ; is the key right shift 
 jne nextcmp3 ; no, leave interrupt routine 
 mov byte [es:0], 'U' ; yes, print R at top left 
 call up
 call printstruct

 jmp nomatch ; leave interrupt routine 
 
 nextcmp3: cmp al, 0x1f ; is the key right shift  ; no, leave interrupt routine 
 JNE nomatch 
 mov byte [es:0], 'D' ; yes, print R at top left 
 call down
 call printstruct
	
 
nomatch: ; mov al, 0x20 
 ; out 0x20, al 
 mov word [randomnumbercount],0

call wincheck
call losecheck

cmp byte [winvar],1
je win

cmp byte [losevar],1
je lose
jne game

win:
call printwinstat
jmp gamexit

lose:
call printlosestat


 gamexit:
 terminategame:

 pop es 
 pop ax  
 jmp far [cs:oldisr] ; call the original ISR 
 ;iret 
 
 wincheck:
pusha 

mov si,b1
mov cx,16

winl:
cmp word[si],2048
je wine

add si,2

loop winl

jmp wexit

wine: 
mov byte [winvar],1

wexit:
popa
ret

losecheck:
pusha 

popa
ret



allowtogeneraterandomnumber:
cmp word [randomnumbercount],0
je allow
jne dontallow

allow:
 mov word [genrand],1
jmp j

dontallow:
 mov word [genrand],0

j:
ret


printwinstat
pusha
mov ah, 0x13 ; service 13 - print string 
 mov al, 1 ; subservice 01 – update cursor 
 mov bh, 0 ; output on page 0 
 mov bl, 6 ; normal attrib 
 mov dx, 0x0A10 ; row 2 column 10 
 mov cx, 37 ; length of string 
 push cs 
 pop es ; segment of string 
 mov bp, winstat ; offset of string 
int 0x10 ; call BIOS video service 

popa
ret


printlosestat
pusha
mov ah, 0x13 ; service 13 - print string 
 mov al, 1 ; subservice 01 – update cursor 
 mov bh, 0 ; output on page 0 
 mov bl, 6 ; normal attrib 
 mov dx, 0x1510 ; row 2 column 10 
 mov cx, 27 ; length of string 
 push cs 
 pop es ; segment of string 
 mov bp, losestat ; offset of string 
int 0x10 ; call BIOS video service 

popa
ret

GenerateRandomNumber:
pusha 

cmp word [genrand],1
je gen

popa
ret


gen:
rdtsc      ;getting a random number in ax dx
xor dx,dx  ;making dx 0
mov cx,16
div cx 
mov al,dl


rand:
add al,1

cmp al,0
je random0

cmp al,1
je random1

cmp al,2
je random2

cmp al,3
je tempr3

cmp al,4
je tempr4

cmp al,5
je tempr5

cmp al,6
je tempr6

cmp al,7
je tempr7

cmp al,8
je tempr8

cmp al,9
je tempr9

cmp al,10
je tempr10

cmp al,11
je tempr11

tempr5:
jmp random5

cmp al,12
je tempr12

cmp al,13
je tempr13

cmp al,14
je tempr14

cmp al,15
je tempr15

tempr4:
jmp random4

tempr6:
jmp random6

tempr7:
jmp random7

tempr3:
jmp random3

tempr8:
jmp random8

tempr9:
jmp random9

tempr10:
jmp random10

tempr11:
jmp random11


random0:
mov si,b1
cmp word [si],0
jne rand
mov word [si],2
jmp randomexit

random1:
mov si,b2
cmp word [si],0
jne rand
mov word [si],2
jmp randomexit

random2:
mov si,b3
cmp word [si],0
jne rand
mov word [si],2
jmp randomexit

tempr12
jmp random12

tempr13
jmp random13

tempr14
jmp random14

tempr15
jmp random15



random3:
mov si,b4
cmp word [si],0
jne rand
mov word [si],2
jmp randomexit

random4:
mov si,b5
cmp word [si],0
jne rand
mov word [si],2
jmp randomexit

random5:
mov si,b6
cmp word [si],0
jne rand
mov word [si],2
jmp randomexit

random6:
mov si,b7
cmp word [si],0
jne rand
mov word [si],2
jmp randomexit

random7:
mov si,b8
cmp word [si],0
jne rand 
mov word [si],2
jmp randomexit

random8:
mov si,b9
cmp word [si],0
jne rand 
mov word [si],2
jmp randomexit

random9:
mov si,b10
add si,18
cmp word [si],0
jne rand 
mov word [si],2
jmp randomexit

random10:
mov si,b11
cmp word [si],0
jne rand 
mov word [si],2
jmp randomexit

random11:
mov si,b12
cmp word [si],0
jne rand 
mov word [si],2
jmp randomexit

random12:
mov si,b13
cmp word [si],0
jne rand 
mov word [si],2
jmp randomexit

random13:
mov si,b14
cmp word [si],0
jne rand 
mov word [si],2
jmp randomexit

random14:
mov si,b15
cmp word [si],0
jne rand 
mov word [si],2
jmp randomexit

random15:
mov si,b16
cmp word [si],0
jne rand 
mov word [si],2
jmp randomexit



randomexit:
add word [randomnumbercount],1
popa
ret



right:
pusha
;call leftshifting
cmp word  [shifting] ,1
jne rand 
je norand


rrand:
mov word [randomnumbercount],0
call allowtogeneraterandomnumber
call GenerateRandomNumber

norand:

mov si,b1
mov dx,4

rcheck1:
mov cx,4
rcheck2:
mov ax,[si]
cmp ax,0
je rzero
add si,2
cmp word [si],0
je rnext
cmp ax,[si]
je radd12l

mov ax,[si]
add si,2
cmp ax,[si]
je radd23l


mov ax,[si]
add si,2
cmp ax,[si]
je radd34l


radd12l:
add word [si],ax
sub si,2
mov word [si],0
add si,2
jmp rnext

;add b3 b4
 
radd23l: 
add word [si],ax
sub si,2
mov word [si],0
add si,2
jmp rnext


radd34l:
add word [si],ax
sub si,2
mov word [si],0
add si,2
jmp rnext

rzero:
add si,2

rnext:
loop rcheck2



add si,2

dec dx

cmp dx,3
je rputsib5

cmp dx,2
je rputsib9

cmp dx,1
je rputsib13

cmp dx,0
jnz rcheck1


cmp byte [shifting],1
;je gen
;jne finalcheck

mov byte [shifting],0


popa
ret 


rightshifting:
pusha 
mov byte [shifting],1 

popa
ret


rputsib5:
mov si,b1
add si,8
jmp rcheck1

rputsib9:
mov si,b1
add si,16
jmp rcheck1


rputsib13:
mov si,b1
add si,24
jmp rcheck1



up:
pusha
;call leftshifting

cmp word [shifting],1
jne urand 
je unorand


urand:
mov word [randomnumbercount],0
call allowtogeneraterandomnumber
call GenerateRandomNumber

unorand:


mov si,b1
mov dx,4

ucheck1:
mov cx,4
ucheck2:
mov ax,[si]
cmp ax,0
je uzero

add si,8
cmp word [si],0
je unext
cmp ax,[si]
je uadd12l


mov ax,[si]
add si,8
cmp ax,[si]
je uadd23l


mov ax,[si]
add si,8
cmp ax,[si]
je uadd34l


uadd12l:
mov ax,[si]
mov word [si],0
sub si,8
add word [si],ax

jmp unext

;add b3 b4
 
uadd23l: 
mov ax,[si]
mov word [si],0
sub si,8
add word [si],ax
jmp unext


uadd34l:
mov ax,[si]
mov word [si],0
sub si,8
add word [si],ax
jmp unext

uzero:
add si,8

unext:

loop ucheck2



;add si,8

dec dx

cmp dx,3
je uputsib5

cmp dx,2
je uputsib9

cmp dx,1
je uputsib13

cmp dx,0
jnz ucheck1


cmp byte [shifting],1
;je gen
;jne finalcheck

mov byte [shifting],0


popa
ret 


upshifting:
pusha 
mov byte [shifting],1 

popa
ret


uputsib5:
mov si,b1
add si,2
jmp ucheck1

uputsib9:
mov si,b1
add si,4
jmp ucheck1


uputsib13:
mov si,b1
add si,6
jmp ucheck1


down:
pusha
call leftshifting
cmp word [shifting],1
jne drand 
je dnorand


drand:
mov word [randomnumbercount],0
call allowtogeneraterandomnumber
call GenerateRandomNumber

dnorand:

mov si,b1
mov dx,4

drcheck1:
mov cx,4
drcheck2:
mov ax,[si]
cmp ax,0
je drzero

add si,8
cmp word [si],0
je drnext

cmp ax,[si]
je dradd12l

mov ax,[si]
add si,8
cmp ax,[si]
je dradd23l


mov ax,[si]
add si,8
cmp ax,[si]
je dradd34l


dradd12l:
add word [si],ax
sub si,8
mov word [si],0
add si,8
jmp drnext

;add b3 b4
 
dradd23l: 
add word [si],ax
sub si,8
mov word [si],0
add si,8
jmp drnext


dradd34l:
add word [si],ax
sub si,8
mov word [si],0
add si,8
jmp drnext

drzero:
add si,8

drnext:
loop drcheck2



;add si,2

dec dx

cmp dx,3
je drputsib5

cmp dx,2
je drputsib9

cmp dx,1
je drputsib13

cmp dx,0
jnz drcheck1


cmp byte [shifting],1
;je gen
;jne finalcheck

mov byte [shifting],0


popa
ret 

downshifting:
pusha 
mov byte [shifting],1 

popa
ret



drputsib5:
mov si,b1
add si,2
jmp drcheck1

drputsib9:
mov si,b1
add si,4
jmp drcheck1


drputsib13:
mov si,b1
add si,6
jmp drcheck1

;remove adddl34 in addl21


left:
pusha
call leftshifting

cmp word [shifting],1
jne lrand 
je lnorand


lrand:
mov word [randomnumbercount],0
call allowtogeneraterandomnumber
call GenerateRandomNumber


lnorand:

mov si,b1
mov dx,4

check1:
mov cx,4
check2:
mov ax,[si]
cmp ax,0
je zero

add si,2
cmp word [si],0
je next
cmp ax,[si]
je add12l

mov ax,[si]
add si,2
cmp ax,[si]
je add23l


mov ax,[si]
add si,2
cmp ax,[si]
je add34l


add12l:
mov ax,[si]
mov word [si],0
sub si,2
add word [si],ax

jmp next

;add b3 b4
 
add23l: 
mov ax,[si]
mov word [si],0
sub si,2
add word [si],ax
jmp next


add34l:
mov ax,[si]
mov word [si],0
sub si,2
add word [si],ax
jmp next

zero:
add si,2

next:
loop check2



add si,2

dec dx

cmp dx,3
je putsib5

cmp dx,2
je putsib9

cmp dx,1
je putsib13

cmp dx,0
jnz check1


cmp byte [shifting],1
je lgen
;jne finalcheck

mov byte [shifting],0


popa
ret 

putsib5:
mov si,b1
add si,8
jmp check1

putsib9:
mov si,b1
add si,16
jmp check1


putsib13:
mov si,b1
add si,24
jmp check1







lgen:
mov byte [es:2], 'S' ; yes, print R at top left 
mov byte [shifting],0
popa
ret


leftshifting:
pusha

mov byte [shifting],0

mov si,b1
mov di,b1
mov bx,4

al1:
mov cx,4
al2:
mov ax,[si]

cmp ax,0
je neq

mov word [si],0
mov word [di],ax
mov byte [shifting],1
add di,2

neq:
add si,2

loop al2

mov di,si

dec bx
cmp bx,0
jnz al1


mov byte [shifting],1

popa
ret 


start:

;call clrscr
;call displaystartmenu
;call printstar
;call printwelcome
;call printstat
mov ah,0
int 0x16

call clrscr

call printstruct


 xor ax, ax 
 mov es, ax ; point es to IVT base 
 mov ax, [es:9*4] 
 mov [oldisr], ax ; save offset of old routine 
 mov ax, [es:9*4+2] 
 mov [oldisr+2], ax ; save segment of old routine 
 cli ; disable interrupts 
 mov word [es:9*4], kbisr ; store offset at n*4 
 mov [es:9*4+2], cs ; store segment at n*4+2 
 sti ; enable interrupts 
l: mov ah, 0 ; service 0 – get keystroke 
 int 0x16 ; call BIOS keyboard service 
 cmp al, 27 ; is the Esc key pressed 
 jne l ; if no, check for next key 
 mov ax, [oldisr] ; read old offset in ax 
 mov bx, [oldisr+2] ; read old segment in bx 
 cli ; disable interrupts 
 mov [es:9*4], ax ; restore old offset from ax 
 mov [es:9*4+2], bx ; restore old segment from bx 
 sti 


mov ax,0x4c00
int 0x21
