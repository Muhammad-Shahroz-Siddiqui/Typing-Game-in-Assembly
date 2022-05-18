;Name : Muhammad Shahroz Siddiqui
;Project : COAL (3C)


[org 0x0100]

 jmp start


 msg1: db '-'
 msg2: db '|'
 msg3: db 'Time'
 lenT: dw 4
 msg4: db 'Score'
 lenS: dw 5
 
 tick: db 0
 timeTick: db 0
 seconds: dw 0
 sc: db 'Final Score: '
 over: db 'Time Out! Game is Over'
 Exitflag: db 0
 score: dw 0
 ascii: db 97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122
 scancode: db 30,48,46,32,18,33,34,35,23,36,37,38,50,49,24,25,16,19,31,20,22,47,17,45,21,44
;;;;;;;;;;;;;; A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P ,Q ,R, S, T, U, V, W, X, Y, Z


;max 4 balloons assumed
balloons: db 11001111b, 'x', 11001111b, 'y', 11001111b, 'a' , 11001111b, 'l'
r_balloons: db 'qazwsxedcrfvtgbyhnujimkopl'
balloonsXPos: dw 20, 35, 48 , 55
balloonsYPos: dw 6, 10, 16, 23
random: dw 25,44,60,15,45,16,65,19,65,30,15,55,36,24,60,43,14,17,29,37,53,27,18,33,25,55
r_index: dw 0

OldTimer: dd 0

;now to create function to print all balloons
PrintBalloons:
	pusha
	push es
	
	push word 0xb800
	pop es
	
	
	mov al, 80
	mul byte [balloonsYPos]
	add ax, [balloonsXPos]
	shl ax, 1
	
	mov bx, ax
	mov ah, [balloons]
	mov al, [balloons+1]
	mov WORD [es: bx], ax
	
	
	
	mov al, 80
	mul byte [balloonsYPos +2]
	add ax, [balloonsXPos + 2]
	shl ax, 1
	
	mov bx, ax
	mov ah, [balloons + 2]
	mov al, [balloons+3]
	mov WORD [es: bx], ax
	
	
	
	mov al, 80
	mul byte [balloonsYPos +4]
	add ax, [balloonsXPos +4]
	shl ax, 1
	
	mov bx, ax
	mov ah, [balloons+4]
	mov al, [balloons+5]
	mov WORD [es: bx], ax
	
	
	
	mov al, 80
	mul byte [balloonsYPos+6]
	add ax, [balloonsXPos+6]
	shl ax, 1
	
	mov bx, ax
	mov ah, [balloons+6]
	mov al, [balloons+7]
	mov WORD [es: bx], ax
	
	
	pop es
	popa
ret

EraseBalloons:
	pusha
	push es
	
	push word 0xb800
	pop es
	
	
	mov al, 80
	mul byte [balloonsYPos]
	add ax, [balloonsXPos]
	shl ax, 1
	
	mov bx, ax
	mov ah, 0
	mov WORD [es: bx], ax
	
	
	
	mov al, 80
	mul byte [balloonsYPos +2]
	add ax, [balloonsXPos + 2]
	shl ax, 1
	
	mov bx, ax
	mov ah, 0
	mov WORD [es: bx], ax
	
	
	
	mov al, 80
	mul byte [balloonsYPos +4]
	add ax, [balloonsXPos +4]
	shl ax, 1
	
	mov bx, ax
	mov ah, 0
	mov WORD [es: bx], ax
	
	
	
	mov al, 80
	mul byte [balloonsYPos+6]
	add ax, [balloonsXPos+6]
	shl ax, 1
	
	mov bx, ax
	mov ah, 0
	mov WORD [es: bx], ax
	
	
	pop es
	popa
ret

ResetBalloons:
pusha
	cmp word[r_index],26
	jnz noreset
	mov word[r_index],0
	
	noreset:
	cmp WORD [balloonsYPos], 0
	jnz skipReset0
	
	mov bx, [r_index]
	mov ax,[random+bx]
	mov dl, [r_balloons+bx]
	add word[r_index],2
	
	mov word[balloonsXPos],ax
	mov WORD [balloonsYPos], 23
	mov BYTE [balloons+1], dl
	mov BYTE [balloons], 11001111b
	skipReset0:
	
	cmp WORD [balloonsYPos+2], 0
	jnz skipReset1
	mov bx, [r_index]
	mov ax,[random+bx]
	mov dl, [r_balloons+bx]
	add word[r_index],2
	mov word[balloonsXPos+ 2],ax
	mov WORD [balloonsYPos+2], 23
	mov BYTE [balloons+3], dl
	mov BYTE [balloons+2], 11001111b
	skipReset1:
	
	cmp WORD [balloonsYPos+4], 0
	jnz skipReset2
	mov bx, [r_index]
	mov ax,[random+bx]
	mov dl, [r_balloons+bx]
	add word[r_index],2
	mov word[balloonsXPos+ 4],ax
	mov WORD [balloonsYPos+4], 23
	mov BYTE [balloons+5], dl
	mov BYTE [balloons+4], 11001111b
	skipReset2:
	
	cmp WORD [balloonsYPos+6], 0
	jnz skipReset3
	mov bx, [r_index]
	mov ax,[random+bx]
	mov dl, [r_balloons+bx]
	add word[r_index],2
	mov word[balloonsXPos+ 6],ax
	mov WORD [balloonsYPos+6], 23
	mov BYTE [balloons+7], dl
	mov BYTE [balloons+6], 11001111b
	skipReset3:
popa
	ret

printtime:
	push bp
	mov bp, sp
	pusha

	mov ax, 0xb800
	mov es, ax
	mov di, 622


	mov ax, [bp+4]
	cmp ax, 99
	jg l1
	push di
	mov di, 626
	mov word[es:di], 0x0720
	pop di

	cmp ax, 9
	jg l1
	push di
	mov di, 624
	mov word[es:di], 0x0720
	pop di
	l1:
	mov bx, 10
	mov cx, 0

	nextdigit:
	mov dx, 0
	div bx
	add dl, 0x30
	push dx
	inc cx
	cmp ax, 0
	jnz nextdigit

	nextpos:
	pop dx
	mov dh, 0x07
	mov [es:di], dx
	add di, 2
	loop nextpos

	popa
	pop bp
ret 2


printScore:
	push bp
	mov bp, sp
	pusha

	mov ax, 0xb800
	mov es, ax
	mov di, 492


	mov ax, [bp+4]
	mov bx, 10
	mov cx, 0

	nextdigit2:
	mov dx, 0
	div bx
	add dl, 0x30
	push dx
	inc cx
	cmp ax, 0
	jnz nextdigit2

	nextpos2:
	pop dx
	mov dh, 0x07
	mov [es:di], dx
	add di, 2
	loop nextpos2

	popa
	pop bp
ret 2

GameScore:
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov di, 2138 ; point di to required location
	mov si, sc ; point si to string
	mov cx, 13 ; load length of string in cx
	mov ah, 0xC7 ; load attribute in ah
	nextchr3: 
	mov al, [si] ; load next char of string
	mov [es:di], ax ; show this char on screen
	add di, 2 ; move to next screen location
	add si, 1 ; move to next char in string
	loop nextchr3 ; repeat the operation cx times
	 
	mov ax, [score]
	mov bx, 10
	mov cx, 0

	nextdigit3:
	mov dx, 0
	div bx
	add dl, 0x30
	push dx
	inc cx
	cmp ax, 0
	jnz nextdigit3

	nextpos3:
	pop dx
	mov dh, 0xC7
	mov [es:di], dx
	add di, 2
	loop nextpos3
	pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret
 
 Gameover:
 push bp
 mov bp, sp
 push es
 push ax
 push cx
 push si
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov di, 1818 ; point di to required location
 mov si, over ; point si to string
 mov cx, 22 ; load length of string in cx
 mov ah, 0xC7 ; load attribute in ah
nextchr: 
 mov al, [si] ; load next char of string
 mov [es:di], ax ; show this char on screen
 add di, 2 ; move to next screen location
 add si, 1 ; move to next char in string
 loop nextchr ; repeat the operation cx times
 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret




;escape snippet for timer
Escape:
	mov BYTE [Exitflag],1
	
	mov al, 0x20 
	out 0x20, al ; end of interrupt 
	popa
iret


timer:
	
	pusha
	
	;GET KEYBOARD IPUT FROM PORT
	in al, 0x60
	
	cmp al, 1    ;esc 
	je Escape
	
	mov si,26
	
	l9:
	cmp si, 0
	jz NoMatch
	dec si
	cmp al,[scancode+si]
	jne l9
	jz Match
	
	
	Match:
	mov al, [ascii+si]
	cmp byte[balloons+1], al
	jnz DontPop0
	add word[score],5
	mov byte[balloons], 0
	
	DontPop0:
	cmp byte[balloons+3], al
	jnz DontPop1
	add word[score],5
	mov byte[balloons+2], 0
	
	DontPop1:
	cmp byte[balloons+5], al
	jnz DontPop2
	add word[score],5
	mov byte[balloons+4], 0
	
	DontPop2:
	cmp byte[balloons+7], al
	jnz NoMatch
	add word[score],5
	mov byte[balloons+6], 0
	
	NoMatch:
	

	inc BYTE [timeTick]
	cmp byte[timeTick], 18
	jnz dontIncTime
	inc WORD [seconds]
	push word[seconds]
	call printtime
	push word[score]
	call printScore
	cmp word[seconds],120
	je Escape
	mov BYTE [timeTick], 0
	
	dontIncTime:
	inc BYTE [tick]
	cmp BYTE [tick], 3
	jnz DontFly
	
	call EraseBalloons
	
	dec word[balloonsYPos]
	dec word[balloonsYPos + 2]
	dec word[balloonsYPos + 4]
	dec word[balloonsYPos + 6]
	
	call ResetBalloons
	
	call PrintBalloons
	
	mov BYTE [tick], 0
	
	DontFly:
	
	
	mov al, 0x20 
	out 0x20, al ; end of interrupt 
	popa 
iret

clrscr:
 push es
 push ax
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov di, 0 ; point di to top left column
nextloc:
 mov word [es:di], 0x0720 ; clear next char on screen
 add di, 2 ; move to next screen location
 cmp di, 4000 ; has the whole screen cleared
 jne nextloc ; if no clear next position
 pop di
 pop ax
 pop es
 ret

end1:
 push es
 push ax
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov di, 0 ; point di to top left column
nextloc1:
 mov word [es:di], 0x4720 ; clear next char on screen
 add di, 2 ; move to next screen location
 cmp di, 4000 ; has the whole screen cleared
 jne nextloc1 ; if no clear next position
 pop di
 pop ax
 pop es
 ret


printHead: 
 push bp
 mov bp, sp
 push es
 push ax
 push cx
 push si
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov al, 80 ; load al with columns per row
 mul byte [bp+10] ; multiply with y position
 add ax, [bp+12] ; add x position
 shl ax, 1 ; turn into byte offset
 mov di, ax ; point di to required location
 mov si, [bp+6] ; point si to string
 mov cx, [bp+4] ; load length of string in cx
 mov ah, [bp+8] ; load attribute in ah
nextch: 
 mov al, [si] ; load next char of string
 mov [es:di], ax ; show this char on screen
 add di, 2 ; move to next screen location
 add si, 1 ; move to next char in string
 loop nextch ; repeat the operation cx times
 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 10

 
printbdr: push bp
 mov bp, sp
 push es
 push ax
 push cx
 push si
 push di
 mov ax, 0xb800
 mov es, ax ; point es to video base
 mov al, 80 ; load al with columns per row
 mul byte [bp+8]; multiply with y position 
 add ax, [bp+10] ; add x position
 shl ax, 1 ; turn into byte offset
 mov di,ax ; point di to required location
 mov si, [bp+6] ; point si to string
 mov cx, 1 ; load length of string in cx
 mov ah, [bp+4] ; load attribute in ah
nextpnt: 
 mov al, [si] ; load next char of string
 mov [es:di], ax ; show this char on screen
 add di, 2
 add si, 1 ; move to next char in string
 loop nextpnt ; repeat the operation cx times
 pop di
 pop si
 pop cx
 pop ax
 pop es
 pop bp
 ret 8
 
start:
 call clrscr
	
	cli
	xor ax,ax
	mov es,ax   ;es points to IVT

;TimerHook
	mov ax,[es:8*4]
	mov [OldTimer], ax
	mov ax,[es:8*4+2]
	mov [OldTimer+2], ax

	mov word[es:8*4],timer
	mov [es:8*4+2],cs
	sti

;border
leftb:
 mov bx,0
lloopb:
 mov ax, 0
 push ax ; push x position
 mov ax, bx
 push ax ; push y position
 mov ax, msg2
 push ax
 mov ax, 0x44
 push ax
 call printbdr
 add bx,1
 cmp bx,25
 jne lloopb
 
upb:
 mov bx,0
uloopb:
 mov ax, bx
 push ax ; push x position
 mov ax, 0
 push ax ; push y position
  mov ax, msg1
 push ax
 mov ax, 0x44
 push ax
 call printbdr
 add bx,1
 cmp bx,80
 jne uloopb
 
downb:
 mov bx,0
dloopb:
 mov ax, bx
 push ax ; push x position
 mov ax, 24
 push ax ; push y position
 mov ax, msg1
 push ax
 mov ax, 0x44
 push ax
 call printbdr
 add bx,1
 cmp bx,80
 jne dloopb
 
rightb:
 mov bx,1
rloopb:mov ax, 79
 push ax ; push x position
 mov ax, bx
 push ax ; push y position
 mov ax, msg2
 push ax
 mov ax, 0x44
 push ax
 call printbdr
 add bx,1
 cmp bx,24
 jne rloopb
 
;timer
timerHead:
mov ax, 70
push ax ; push x position
mov ax, 1
push ax ; push y position
mov ax, 0xB ; blue on black attribute
push ax ; push attribute
mov ax, msg3
push ax ; push address of message
push word [lenT] ; push message length
call printHead ; call the printstr subroutine

leftt:
 mov bx,2
lloopt:
 mov ax, 70
 push ax ; push x position
 mov ax, bx
 push ax ; push y position
 mov ax, msg2
 push ax
 mov ax, 0x44
 push ax
 call printbdr
 add bx,1
 cmp bx, 4
 jne lloopt
 
rightt:
 mov bx,3
rloopt:mov ax, 75
 push ax ; push x position
 mov ax, bx
 push ax ; push y position
 mov ax, msg2
 push ax
 mov ax, 0x44
 push ax
 call printbdr
 add bx,1
 cmp bx,4
 jne rloopt
 
upt:
 mov bx,70
uloopt:
 mov ax, bx
 push ax ; push x position
 mov ax, 2
 push ax ; push y position
 mov ax, msg1
 push ax
 mov ax, 0xE
 push ax
 call printbdr
 add bx,1
 cmp bx,76
 jne uloopt
 
downt:
 mov bx,70
dloopt:
 mov ax, bx
 push ax ; push x position
 mov ax, 4
 push ax ; push y position
 mov ax, msg1
 push ax
 mov ax, 0xE
 push ax
 call printbdr
 add bx,1
 cmp bx,76
 jne dloopt
 
;score
scoreHead:
mov ax, 5
push ax ; push x position
mov ax, 1
push ax ; push y position
mov ax, 0xB ; blue on black attribute
push ax ; push attribute
mov ax, msg4
push ax ; push address of message
push word [lenS] ; push message length
call printHead ; call the printstr subroutine

lefts:
 mov bx,2
lloops:
 mov ax, 5
 push ax ; push x position
 mov ax, bx
 push ax ; push y position
 mov ax, msg2
 push ax
 mov ax, 0x44
 push ax
 call printbdr
 add bx,1
 cmp bx, 4
 jne lloops
 
rights:
 mov bx,3
rloops:mov ax, 10
 push ax ; push x position
 mov ax, bx
 push ax ; push y position
 mov ax, msg2
 push ax
 mov ax, 0x44
 push ax
 call printbdr
 add bx,1
 cmp bx,4
 jne rloops
 
ups:
 mov bx,5
uloops:
 mov ax, bx
 push ax ; push x position
 mov ax, 2
 push ax ; push y position
 mov ax, msg1
 push ax
 mov ax, 0xE
 push ax
 call printbdr
 add bx,1
 cmp bx,11
 jne uloops
 
downs:
 mov bx,5
dloops:
 mov ax, bx
 push ax ; push x position
 mov ax, 4
 push ax ; push y position
 mov ax, msg1
 push ax
 mov ax, 0xE
 push ax
 call printbdr
 add bx,1
 cmp bx,11
 jne dloops
 
 
looop:
	cmp BYTE [Exitflag], 1
	jnz looop


ExitGame:
	cli
	xor ax,ax
	mov es,ax
    mov ax, [OldTimer]
    mov [es:8*4], ax
    mov ax, [OldTimer+2]
    mov [es:8*4+2], ax
    sti
	
	mov ax, 3
	int 10h		;clear screen
	mov ah, 1
	mov ch, 28h
	int 10h
	call end1
	call Gameover
	call GameScore
	;terminate
	mov ah, 4ch
	int 21h

 mov ax, 0x4c00 ; terminate program
 int 0x21 
 
