.model small
.stack 200h

.data

SCREEN_WIDTH equ 0A0h       ;screen width in bytes
SCREEN_HEIGHT equ 19h       ;screen height in characters

FIELD_WIDTH equ 72h

factor db 2 

PACX dw 30       
PACY dw 22

next_x dw 0
next_y dw 0

array dw 10 dup(0)

PAC_IMAGE0 db 35h,44h,35h,44h,35h,44h,35h,44h   ;pacman characters and attributes 
           db 35h,44h,35h,44h,35h,44h,35h,44h
           
PAC_WIDTH equ 4
PAC_HEIGHT equ 2           
           

black_char db 35h,0      ;black character on black background

blue_char db 38h,11h    ;blue character on blue background

gray_char db 30h,77h     ;gray character in gray background

green_char db 34h,22h

red_char db 31h,33h

lightblue_char db 32h,33h

blue equ 11h

gray equ 77h

RIGHT equ 4Dh
LEFT equ 4Bh
UP equ 48h
DOWN equ 50h
       
current_dir db RIGHT

testx equ 30
testy equ 0
test_width equ 4
test_height equ 12

wait_time dw 0

clock_period dw SIDE_DELAY

SIDE_DELAY equ 2
VERTICAL_DELAY equ 3

;GHOST PART

GHOST_IMAGE0 db 35h,33h,35h,33h,35h,33h,35h,33h   ;ghost characters and attributes 
             db 35h,33h,35h,33h,35h,33h,35h,33h
             
MAX_GHOST_NUMBER equ 50

ghosts_coordinates dw 2*MAX_GHOST_NUMBER dup(0)

previous_directions dw 2*MAX_GHOST_NUMBER dup(0)

state_changed dw 2*MAX_GHOST_NUMBER dup(0)

GHOST_UP equ 1
GHOST_DOWN equ 2
GHOST_LEFT equ 3
GHOST_RIGHT equ 4

GHOST_NUMBER equ 7

GHOST_HEIGHT equ 2
GHOST_WIDTH equ 4

directions db 4*MAX_GHOST_NUMBER dup(0)      ;up,down,left,right

;MESSAGES

score_text db "Score:"

score dw 0 

SCORE_INC equ 10

SCORE_X equ 68
SCORE_Y equ 4

sc_x db 0
sc_y db 0

score_att db 0Fh

gameover_text db "GAME OVER"

GAMEOVER_X equ 35
GAMEOVER_Y equ 12

firstdir_text1 db "Choose first"
firstdir_text2 db "direction"

FIRSTDIR_X1 equ 64
FIRSTDIR_Y1 equ 14

FIRSTDIR_X2 equ 64
FIRSTDIR_Y2 equ 15

message_att db 8Fh             

.code

print_image PROC             ;X and Y - initial coordinates, width, height of the image, offset of the image  
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push bp
    
    mov bp,sp
   
    mov si,[ss:bp+16]        ;offset of the image
   
    mov ax,[ss:bp+22]       ;move y to bx
    mov bx,[ss:bp+24]       ;move x to ax
    call convert_to_offset
    mov di,dx
    
    mov cx,[ss:bp+18]        ;number of lines
   
    another_line:
   
    push cx
    mov ax,[ss:bp+20]        ;number of characters in the line
    mul factor
    mov cx,ax
    
    push di
    rep movsb
    pop di
    
    add di,SCREEN_WIDTH
    pop cx
    
    loop another_line
   
    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
print_image ENDP

print_rect PROC        ;accepts X and Y - initial coordinates, width, height of the rectangle, char and attribute in one parameter 
    push ax
    push bx
    push cx
    push dx
    push di
    push bp
    mov bp,sp
    
    mov ax,[bp+20]
    mov bx,[bp+22]
    
    call convert_to_offset
    mov di,dx
    
    mov ax,[bp+14]
    mov al,bh
    mov cx,[bp+16]
    
    another_string:
    
    push cx
    mov cx,[bp+18]
   
    push di
    rep stosw
    pop di
    
    add di,SCREEN_WIDTH
    
    pop cx
    loop another_string
    
    pop bp
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
print_rect ENDP

convert_to_offset PROC       ;accepts Y in ax, X in bx. returns offset in DX 
    
    push cx
    
    mov cl,SCREEN_WIDTH
    mul cl
    mov dx,ax
    
    mov ax,bx
    mul factor
    add dx,ax
    
    pop cx
    ret
convert_to_offset ENDP

move_pacman PROC        ;changes PACX, PACY according to current direction and prints pacman
    push ax
    push bx
    push cx
    push si
    push di
    
    mov ax,PACX
    mov bx,PACY
    mov next_x,ax
    mov next_y,bx
    
    mov di,offset array
    
    mov [di],ax
    mov [di+2],bx
    
    inc bx
    mov [di+4],ax
    mov [di+6],bx
    
    add ax,2
    mov [di+8],ax
    mov [di+10],bx
    
    inc ax
    mov [di+12],ax
    mov [di+14],bx
    
    cmp current_dir,LEFT
    je move_left
    
    cmp current_dir,RIGHT
    je move_right
    
    cmp current_dir,UP
    je move_up
    
    cmp current_dir,DOWN
    je move_down
    
    jmp stop
    
    move_left:
    sub [di],1
    sub [di+4],1
    sub next_x,1
    jmp check_side
    
    move_right:
    add [di],4
    add [di+4],4
    add next_x,1
    jmp check_side
    
    move_up:
    sub [di+2],1
    add [di+4],1
    sub [di+6],2
    sub [di+10],2
    sub [di+14],2
    sub next_y,1
    jmp check_vertical
    
    move_down:
    add [di+2],2
    add [di+4],1
    add [di+6],1
    add [di+10],1
    add [di+14],1
    add next_y,1
    jmp check_vertical
    
    check_side:
    mov cx,2
    jmp start_check
    
    check_vertical:
    mov cx,4
    
    start_check:
    xor bx,bx
    
    check_another:
    
    push [di+bx]
    push [di+bx+2]
    push word ptr [blue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    jne not_stop
    jmp stop
    not_stop:
    
    add bx,4
    
    loop check_another
    xor bx,bx
    
    push next_x
    push next_y
    push word ptr [gray_char]
    
    call check_surroundings
    add sp,6
    
    cmp dx,1
    jne print_pac
    add score,10
    mov bx,1
    
    print_pac:
    
    push PACX
    push PACY
    push PAC_WIDTH
    push PAC_HEIGHT
    push word ptr [black_char]
    
    call print_rect             ;paint old pacman black
    add sp,10
    
    mov ax,next_x
    mov bx,next_y
    mov PACX,ax
    mov PACY,bx
    
    push PACX
    push PACY
    push PAC_WIDTH
    push PAC_HEIGHT
    push offset PAC_IMAGE0
    
    call print_image            ;print new pacman
    add sp,10
    
    cmp dx,1
    jne stop
    call print_random_food
    
    stop:
    
    pop di
    pop si
    pop cx
    pop bx
    pop ax
    ret
move_pacman ENDP

check_input PROC        ;reads character from buffer and sets new PACX or PACY if the character is one of the arrow keys 
    push ax
    
    xor dx,dx
    
    mov ah,1
    int 16h
    jz _end
    
    mov ah,0
    int 16h
    
    cmp ah,LEFT
    mov clock_period,SIDE_DELAY
    je set_dir
    
    cmp ah,RIGHT
    mov clock_period,SIDE_DELAY
    je set_dir
    
    cmp ah,UP
    mov clock_period,VERTICAL_DELAY
    je set_dir
    
    cmp ah,DOWN
    mov clock_period,VERTICAL_DELAY
    je set_dir
    
    jmp _end
    
    set_dir:
    
    mov current_dir,ah
    mov dx,1
    
    _end:
    
    pop ax
    ret
check_input ENDP

print_layout PROC              ;prints the labirinth
    
    ;top horizontal line
    push 0                     ;X coordinate
    push 0                      ;Y coordinate
    push 60                      ;width of the rectangle
    push 1                     ;height of the rectangle
    push word ptr [blue_char]   ;char with attribute (blue for the labirinth)
    
    call print_rect
    add sp,10
    
    ;top left vertical line 
    push 0
    push 1
    push 2
    push 24
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;bottom horizontal line
    push 2
    push 24
    push 58
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;top right vertical line
    push 58
    push 1
    push 2
    push 23
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;left rectangle
    push 6
    push 11
    push 4
    push 5
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;left vertical line near rectangle
    push 14
    push 9
    push 2
    push 4
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;bottom left small horizontal line
    push 2
    push 18
    push 8
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;bottom left small vertical line
    push 14
    push 18
    push 2
    push 3
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;vertical line to the left of center line 
    push 20
    push 11
    push 2
    push 8
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;horizontal part of the previous line
    push 14
    push 15
    push 6
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;bottom right vertical line
    push 52
    push 14
    push 2
    push 8
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;horizontal part of the previous line
    push 46
    push 17
    push 6
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;bottom small vertical line
    push 46
    push 20
    push 2
    push 4 
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;bottom left small vertical line
    push 34
    push 19
    push 2
    push 5
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;bottom vertical line near the center
    push 40
    push 17
    push 2
    push 5
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;left horizontal line
    push 40
    push 14
    push 8
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;vertical part of the previous line
    push 40
    push 11
    push 2
    push 3
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;rectangle near the center
    push 26
    push 11
    push 10
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;vertical part of the previous line
    push 26
    push 12
    push 2
    push 5
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;rectangle near previous one
    push 32
    push 14
    push 4
    push 3
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;top vertical line (angle 1)
    push 34
    push 3
    push 2
    push 3
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;horizontal part of the previous line
    push 24
    push 3
    push 12
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;vertical part of the previous line
    push 22
    push 3
    push 2
    push 3
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;center vertical line (angle 2)
    push 28
    push 6
    push 2
    push 2
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;horizontal part of the previous line
    push 24
    push 8
    push 12
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;right small horizontal line
    push 52
    push 11
    push 6
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;right vertical line near the top
    push 46
    push 6
    push 2
    push 6
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;horizontal part of the previous line
    push 40
    push 8
    push 6
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;top right vertical line
    push 52
    push 3
    push 2
    push 6
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;horizontal part of the previous line
    push 46
    push 3
    push 6
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;bottom rectangle
    push 26
    push 19
    push 4
    push 3
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;bottom left horizontal line
    push 6
    push 21
    push 16
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;top left small rectangle
    push 2
    push 8
    push 4
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;top left rectangle
    push 6
    push 3
    push 6
    push 3
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
  
    ;top vertical line 1
    push 16
    push 1
    push 2
    push 5
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;top right vertical line
    push 40
    push 1
    push 2
    push 5
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
    
    ;left top horizontal line
    push 10
    push 8
    push 10
    push 1
    push word ptr [blue_char]
    
    call print_rect
    add sp,10
   
    ret
print_layout ENDP

print_score PROC
    push ax
    push bx
    push cx
    push dx
    push si
    
    mov bx,10
    mov ax,[score]
    mov cx,0
    
    again:
    
    xor dx,dx
    div bx
    push dx
    inc cx
    
    cmp ax,0
    jne again
    
    mov sc_x,SCORE_X        
    mov sc_y,SCORE_Y
    
    mov si,cx
    another_char:
    
    mov ah,2
    mov bh,0
    mov dh,sc_y
    mov dl,sc_x
    int 10h
    
    pop dx
    add dx,30h
    
    mov ah,9
    mov bl,score_att
    mov al,dl
    mov cx,1
    int 10h
    
    inc sc_x
    dec si
    
    cmp si,0
    jne another_char
    
    mov ah,2
    mov dh,26
    mov dl,81
    int 10h
    
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
print_score ENDP

print_text PROC
    push bx
    push cx
    push dx
    
    mov ax,es
    push ax
    mov ax,@data
    mov es,ax
    
    push bp
    mov bp,sp
    
    mov dl,byte ptr [bp+20]
    mov dh,byte ptr [bp+18]
    mov cx,word ptr [bp+16]
    mov bl,byte ptr [bp+12]
    
    mov bp,word ptr [bp+14]
    xor al,al
    mov ah,13h
    int 10h
    
    pop bp
    pop ax
    mov es,ax
    
    pop dx
    pop cx
    pop bx
    ret
print_text ENDP    

print_random_food PROC
    push ax
    push bx
    push cx
    push dx
    push si
    
    get_random:
    
    mov ah,2Ch
    int 21h
    
    xor dh,dh
    mov ax,dx
    mov bl,28
    mul bl
    
    xor dx,dx
    mov bx,FIELD_WIDTH
    div bx
    mov cx,ax
    
    mov ax,dx
    xor dx,dx
    mov bx,2
    div bx
    
    push ax
    push cx
    push word ptr [blue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je get_random
    
    push ax
    push cx
    push word ptr [lightblue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je get_random
    
    push ax
    push cx
    push word ptr [red_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je get_random
                  
    push ax
    push cx
    push 1
    push 1
    push word ptr [gray_char]
    
    call print_rect
    add sp,10
    
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
print_random_food ENDP

get_random_ghost_position PROC      ;returns in ax and bx random valid X and Y of the ghost
    push cx
    push dx
    push si
    get_random_pos:
    
    mov ah,2Ch
    int 21h
    
    xor dh,dh
    mov bx,dx
    
    mov ah,0
    int 1Ah
    
    mov ax,bx
    mul dl
    
    xor dx,dx
    mov bx,9
    div bx
    
    xor dx,dx
    mov bx,FIELD_WIDTH
    div bx
    mov cx,ax
    
    mov ax,dx
    xor dx,dx
    mov bx,2
    div bx
    
    push ax
    push cx
    push word ptr [blue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je get_random_pos
    
    push ax
    push cx
    
    sub ax,1
    
    push ax
    push cx
    push word ptr [blue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je correct_horizontal_position
    pop cx
    pop ax
    jmp get_random_pos
    
    correct_horizontal_position:
 
    inc ax
    inc cx
    
    push ax
    push cx
    push word ptr [blue_char]
    
    call check_move
    add sp,6
    
    cmp si,1
    je correct_vertical_position
    pop cx
    pop ax
    jmp get_random_pos
    
    correct_vertical_position:
    
    pop cx
    pop ax
    
    push ax
    push cx
    push word ptr [lightblue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je get_random_pos
    
    mov bx,cx
    
    pop si
    pop dx
    pop cx
    ret
get_random_ghost_position ENDP

create_ghosts PROC
    push ax
    push bx
    push cx
    push di
    
    mov cx,GHOST_NUMBER
    lea di,ghosts_coordinates
    
    another_ghost:
    
    call get_random_ghost_position
    
    mov [di],ax
    mov [di+2],bx
    
    push ax
    push bx
    push GHOST_WIDTH
    push GHOST_HEIGHT
    push offset GHOST_IMAGE0
    
    call print_image
    add sp,10
    
    add di,4
    
    loop another_ghost
    
    pop di
    pop cx
    pop bx
    pop ax
    ret
create_ghosts ENDP

check_up_dir PROC
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    
    push bp
    mov bp,sp
    
    mov bx,[bp+16]
    mov di,bx
    mov ax,ghosts_coordinates[di]
    mov dx,ghosts_coordinates[di+2]
    dec dx
    
    mov cx,4
    
    test_up_dir:
    
    push ax
    push dx
    push word ptr [blue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je cannot_go_there
    inc ax
    
    loop test_up_dir
    
    cmp directions[bx],0
    jne was_one
    mov state_changed[di],1
    was_one:
    mov directions[bx],1
    jmp go_to_end
    
    cannot_go_there:
    cmp directions[bx],1
    jne was_zero
    mov state_changed[di],1
    was_zero:
    mov directions[bx],0
    go_to_end:
    
    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
check_up_dir ENDP

check_down_dir PROC
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    
    push bp
    mov bp,sp
    
    mov bx,[bp+16]
    mov di,bx
    mov ax,ghosts_coordinates[di]
    mov dx,ghosts_coordinates[di+2]
    add dx,2
    
    mov cx,4
    
    test_down_dir:
    
    push ax
    push dx
    push word ptr [blue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je cannot_go_there_
    inc ax
    
    loop test_down_dir
    
    cmp directions[bx+1],0
    jne was_one_
    mov state_changed[di],1
    was_one_:
    mov directions[bx+1],1
    jmp go_to_end_
    
    cannot_go_there_:
    cmp directions[bx+1],1
    jne was_zero_
    mov state_changed[di],1
    was_zero_:
    mov directions[bx+1],0
    go_to_end_:
    
    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
check_down_dir ENDP

check_left_dir PROC
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    
    push bp
    mov bp,sp
    
    mov bx,[bp+16]
    mov di,bx
    mov ax,ghosts_coordinates[di]
    mov dx,ghosts_coordinates[di+2]
    dec ax
    
    mov cx,2
    
    test_left_dir:
    push ax
    push dx
    push word ptr [blue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je _cannot_go_there_
    inc dx
    
    loop test_left_dir
    
    cmp directions[bx+2],0
    jne _was_one_
    mov state_changed[di],1
    _was_one_:
    mov directions[bx+2],1
    jmp _go_to_end_
    
    _cannot_go_there_:
    cmp directions[bx+2],1
    jne _was_zero_
    mov state_changed[di],1
    _was_zero_:
    mov directions[bx+2],0
    _go_to_end_:
    
    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
check_left_dir ENDP

check_right_dir PROC
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    
    push bp
    mov bp,sp
    
    mov bx,[bp+16]
    mov di,bx
    mov ax,ghosts_coordinates[di]
    mov dx,ghosts_coordinates[di+2]
    add ax,4
    
    mov cx,2
    
    test_right_dir:
    push ax
    push dx
    push word ptr [blue_char]
    
    call check_move
    add sp,6
    
    cmp si,0
    je cannot_go_there__
    inc dx
    
    loop test_right_dir
    
    cmp directions[bx+3],0
    jne was_one__
    mov state_changed[di],1
    was_one__:
    mov directions[bx+3],1
    jmp go_to_end__
    
    cannot_go_there__:
    cmp directions[bx+3],1
    jne was_zero__
    mov state_changed[di],1
    was_zero__:
    mov directions[bx+3],0
    go_to_end__:
    
    pop bp 
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
check_right_dir ENDP

move_one_ghost PROC
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push bp
    mov bp,sp
   
    mov si,[bp+16]
    mov di,si
    mov ax,ghosts_coordinates[si]
    mov bx,ghosts_coordinates[si+2]
    push ax
    
    push si
    call get_available_directions
    add sp,2
    
    mov cx,4
    xor dx,dx
    mov ax,di
    
    choose_direction:
    
    cmp directions[di],1
    jne not_interested
    
    cmp state_changed[si],1
    je _changed
    
    mov dx,previous_directions[si]
    done_:
    jmp got_direction
    
    _changed:
    
    cmp previous_directions[si],0
    jne not_zero_
    mov dx,di
    sub dx,ax
    inc dx
    jmp got_direction
    not_zero_:
    
    mov dx,di
    sub dx,ax
    inc dx
    cmp dx,previous_directions[si]
    je not_this_one
    
    cmp dx,previous_directions[si+2]
    je opposite_dir
    mov previous_directions[si],dx
    jmp got_direction
    opposite_dir:
    
    not_this_one:
    
    not_interested:
    
    inc di
    loop choose_direction
    
    got_direction:
    mov state_changed[si],0
    mov previous_directions[si],dx
    
    pop ax
    push dx
    test dx,1
    jz even_number
    inc dx
    mov previous_directions[si+2],dx
    jmp done_testing
    even_number:
    dec dx
    mov previous_directions[si+2],dx
    done_testing:
    pop dx
    
    cmp dx,GHOST_UP
    je move_ghost_up
    
    cmp dx,GHOST_DOWN
    je move_ghost_down
    
    cmp dx,GHOST_LEFT
    je move_ghost_left
    
    cmp dx,GHOST_RIGHT
    je move_ghost_right
    
    jmp none_of_them
    
    move_ghost_up:
    dec bx
    jmp print_ghost
    
    move_ghost_down:
    inc bx
    jmp print_ghost
    
    move_ghost_left:
    dec ax
    jmp print_ghost
    
    move_ghost_right:
    inc ax
    jmp print_ghost
    
    print_ghost:
    
    push ghosts_coordinates[si]             ;paint old ghost image black
    push ghosts_coordinates[si+2]
    push GHOST_WIDTH
    push GHOST_HEIGHT
    push word ptr [black_char]
    
    call print_rect
    add sp,10
    
    mov ghosts_coordinates[si],ax
    mov ghosts_coordinates[si+2],bx
    
    xor cx,cx
    push ax
    push bx
    push word ptr [gray_char]
    call check_surroundings
    add sp,6
    
    cmp dx,1
    jne no_food_nearby
    mov cx,1
    no_food_nearby:
    
    push ax
    push bx
    push GHOST_WIDTH
    push GHOST_HEIGHT
    push offset GHOST_IMAGE0
    
    call print_image
    add sp,10
    
    cmp cx,1
    jne none_of_them
    call print_random_food
    
    none_of_them:
    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret
move_one_ghost ENDP

get_available_directions PROC
    push ax
    push dx
    push si
    push di
    
    push bp
    mov bp,sp
    
    mov si,[bp+12]
    mov di,si
    xor dx,dx
    
    cmp word ptr directions[di],0
    jne test_other
    inc dl
    test_other:
    
    cmp word ptr directions[di+2],0
    jne test_other_
    inc dl
    test_other_:
   
    push si
    call check_up_dir
    add sp,2
    cmp state_changed[si],1
    jne up_not_changed
    mov dh,1
    mov state_changed[si],0
    up_not_changed:

    push si
    call check_down_dir
    add sp,2
    cmp state_changed[si],1
    jne down_not_changed
    mov dh,1
    mov state_changed[si],0
    down_not_changed:
    
    push si
    call check_left_dir
    add sp,2
    cmp state_changed[si],1
    jne left_not_changed
    mov dh,1
    mov state_changed[si],0
    left_not_changed:
    
    push si
    call check_right_dir
    add sp,2
    cmp state_changed[si],1
    jne right_not_changed
    mov dh,1
    mov state_changed[si],0
    right_not_changed:
    
    mov state_changed[si],0
    
    cmp dl,1
    jne not_gonna_set
    
    cmp dh,1
    jne stays_the_same
    
    mov state_changed[si],1
    jmp end_proc
    stays_the_same:
    not_gonna_set:
    
    cmp dl,2
    jne end_proc
    mov state_changed[si],1
    
    end_proc:
    pop bp
    pop di
    pop si
    pop dx
    pop ax
    ret
get_available_directions ENDP

check_surroundings PROC         ;accepts X,Y and char with attribute.checks if that char is near.returns 0 or 1 in dx
    push ax
    push bx
    push cx
    push si
    
    push bp
    mov bp,sp
    
    mov ax,[bp+16]
    mov bx,[bp+14]
    
    mov cx,2
    
    scan_vertical_dir:
    push cx
    mov cx,4
    one_more_char:
    
    push ax
    push bx
    push [bp+12]
    
    call check_move
    add sp,6
    
    cmp si,0
    jne empty
    pop cx
    jmp found_smth
    empty:
    inc ax
    
    loop one_more_char
    
    pop cx
    cmp cx,2
    jne second_time_
    sub ax,4
    inc bx
    second_time_:
    
    loop scan_vertical_dir
    
    mov ax,[bp+16]
    mov bx,[bp+14]
    
    mov cx,2
    scan_horizontal_dir:
    push cx
    mov cx,2
    one_more_char_:
    
    push ax
    push bx
    push [bp+12]
    
    call check_move
    add sp,6
    
    cmp si,0
    jne empty_
    pop cx
    jmp found_smth
    empty_:
    
    inc bx
    loop one_more_char_
    
    pop cx
    cmp cx,2
    jne _second_time_
    sub bx,2
    add ax,3
    _second_time_:
    
    loop scan_horizontal_dir
    jmp nothing_found
    
    found_smth:
    mov dx,1
    jmp the_end
    
    nothing_found:
    xor dx,dx
    the_end:
    
    pop bp
    pop si
    pop cx
    pop bx
    pop ax
    ret
check_surroundings ENDP    

move_all_ghosts PROC
    push cx
    push di
    
    mov cx,GHOST_NUMBER
    xor di,di
    
    move_next_ghost:
    
    push di
    call move_one_ghost
    add sp,2
    
    add di,4
    
    loop move_next_ghost 
    
    pop di
    pop cx
    ret
move_all_ghosts ENDP    

check_move PROC        ;accepts X,Y and char with attribute. returns 0 in si if we can't move to (X,Y), 1 if we can 
    push ax
    push bx
    push dx
    push di
    push bp
    mov bp,sp
    
    mov ax,[bp+14]
    mov bx,[bp+16]
    call convert_to_offset
    
    mov di,dx

    mov ax,word ptr es:[di]
    
    mov si,0
    cmp ah,byte ptr [bp+13]
    je _done
    
    mov si,1
    _done:
    
    pop bp
    pop di
    pop dx
    pop bx
    pop ax
    ret
check_move ENDP

time_cycle PROC         ;moves pacman on the screen if enough time has passed from the last call 

    mov ah,0
    int 1Ah
    
    xor cx,cx
    
    cmp dx,wait_time
    jae clock
    jb end_procedure
    
    clock:
    
    add dx,CLOCK_PERIOD
    mov wait_time,dx
    
    call check_input
    call move_pacman
    call move_all_ghosts
    call print_score
                    
    end_procedure:
    
    ret
time_cycle ENDP

check_dead PROC     ;returns 1 in dx if we are dead
    push ax
    
    push PACX
    push PACY
    push word ptr [lightblue_char]
    
    call check_surroundings
    add sp,6
    
    cmp dx,1
    jne not_dead
    
    mov ah,00h
    mov al,3
    int 10h
    
    push GAMEOVER_X
    push GAMEOVER_Y
    push 9
    push offset gameover_text
    push word ptr [message_att]
    
    call print_text
    add sp,10
    jmp skip_it
    
    not_dead:
    xor dx,dx
    
    skip_it:
    pop ax
    ret
check_dead ENDP

get_first_dir PROC
    push dx
    
    push FIRSTDIR_X1
    push FIRSTDIR_Y1
    push 12
    push offset firstdir_text1
    push word ptr [message_att]

    call print_text
    add sp,10
    
    push FIRSTDIR_X2
    push FIRSTDIR_Y2
    push 9
    push offset firstdir_text2
    push word ptr [message_att]
    
    call print_text
    add sp,10

    get_first_direction:
    call check_input
    cmp dx,1
    jne get_first_direction
    
    push FIRSTDIR_X1
    push FIRSTDIR_Y1
    push 12
    push 2
    push word ptr [black_char]
    
    call print_rect
    add sp,10
    
    pop dx
    ret
get_first_dir ENDP    

start:

mov ax,@data
mov ds,ax

mov ah,00h
mov al,3
int 10h

mov ax,0B800h
mov es,ax

push SCORE_X-1
push SCORE_Y-2
push 6
push offset score_text
push word ptr [score_att]

call print_text
add sp,10

call print_layout
call create_ghosts
call print_random_food

push PACX
push PACY
push PAC_WIDTH
push PAC_HEIGHT
push offset PAC_IMAGE0
call print_image
add sp,10

call get_first_dir

game_loop:

call time_cycle

call check_dead
cmp dx,1
je game_over

jmp game_loop

game_over:
wait_for_any_key:

mov ah,1
int 16h
jz wait_for_any_key

mov ax,4c00h
int 21h

end start
            