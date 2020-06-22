.model	small
.stack	100h
.data
.386            
MaxArrayLength              equ 30  
          
NL                    db      0Dh, 0Ah, '$'  ;1 байт   
Num                   dw 0   ;2 байта
KeyBuf                db 7, ?, 8 dup('$')     
ArrayLength                 dw  ?
InputArrayLengthMsgStr      db  'Input array length: $'
maxNull                     db 0Ah,0Dh,'All infinity','$',0Ah,0Dh                                
ErrorInputMsgStr            db  0Ah,0Dh,'Incorrect value!Try again!',0Ah,0Dh, '$' 
ErrorInputArrayLengthMsgStr db  0Ah,0Dh,'Array length should be not less than 0 and not bigger than 30!', '$'
                                
InputMsgStr                 db 0Ah, 0Dh,'Input element (-32 768..32 767) : $'    

Answer                      db  20 dup('$'), 0Ah,0Dh, '$';  выделил 20 символов и заполнил ничем
                                        

nextStr                     db 0Ah,0Dh,'$'       
ten                         dw 10
check	                    db 0
precision                   db  5             
minus                       db  0  
Max                         dw  0
MaxTemp                     dw  0
Array                       dw  MaxArrayLength dup (0) 
 
print MACRO string                         
    mov ah,09h                    
    mov dx,  offset [string]     
    int 21h 
	
ENDM  ; конец макроса
                             
.code      
.386
start:                            
mov	ax,@data                      
mov	ds,ax 
mov     es, ax                         
                                  
xor	ax,ax      ;ax=0                   
                               
call inputMas                  
call FindMax                      
call MakeNormal
                                  
inputMas proc                      
    call inputLength         
    call inputArray                                     
    ret                           
endp     

inputLength proc
              
    inputArrayLengthLoop: 
       call ShowInputArrayLengthMsg
       push cx                    
       call inputElement ;вызов процедуры
       pop cx
       mov ArrayLength,ax
       cmp ArrayLength,0
       jle lengthError
       cmp ArrayLength,30
       jg  lengthError 
                                
    ret      
endp

lengthError:
    call ErrorInput
    jmp  inputArrayLengthLoop
    
inputArray proc

    xor di,di                     
                                              
    mov cx,ArrayLength 
           
    inputArrayLoop:
       call ShowInputMsg
       push cx             
       push di                 
       call inputElement
       pop di
       pop cx      
       mov Array[di], ax 

       add di,2                   
    loop inputArrayLoop           
    ret      
endp  

  
inputElement proc 
  
inp_:; метка выделяется двоеточием 

    mov     dx, offset KeyBuf
    call input

   print NL 

     
   lea     si, KeyBuf+1 
   lea     di, Num
                                
    push si
    xor ax,ax 
    xor cx,cx
   
    mov cl , ds:[si] 
    xor ch,ch

    inc si 
    
    mov     bx, 10 
    mov minus , 0
    cmp     byte ptr [si], '-' 
    jne     loopX 
    mov minus , 1
    inc     si
    dec     cx 
    
loopX: 
    mul bx 
    mov [di] , ax
    cmp     dx, 0 
    jnz error 

    mov al , [si] 
    cmp al , '0' 
    jb error  
    
    cmp al , '9'
    ja error ; если >
    
    sub al , '0' 
    xor ah , ah
    add ax , [di] 
    cmp minus , 1
    jne comp_pos 
    cmp ax , 8000h 
    ja error
    jmp endloop ;

comp_pos: ; метка 
    cmp ax , 8000h
    jae error 
endloop:
    inc si
    
loop loopX 
 
    pop si
    inc si
    cmp     byte ptr [si], '-'
    je Negative

StoreRes:
    mov [di] , ax
    xor di , di
    ret

Negative:
    neg ax
    jmp StoreRes

error:
    clc; сброс флага переноса
    pop si 
    mov Num , 0
    lea dx, ErrorInputMsgStr      
    mov ah, 09h                   
    int 21h 
    jmp inputElement
                      
endp

     
input proc 
    mov ah,0Ah
    int 21h
    ret ;return

input endp

ErrorInput proc                   
    lea dx, ErrorInputMsgStr      
    mov ah, 09h                   
    int 21h                       
    ret                           
endp                              
      

ShowInputArrayLengthMsg proc
    push ax
    push dx
      
    print InputArrayLengthMsgStr 
    
    pop dx
    pop ax 
     
    ret
endp          
                                  
ShowInputMsg proc 
                    
    push ax
    push dx                     
        
    print InputMsgStr                        
    
    pop dx
    pop ax                    
    ret 
                          
endp                        
   

FindMax proc  ;near
    xor di,di
    lea di , Array                 
    mov cx, ArrayLength
    mov ax,[di] 
    add di,2 
    dec cx 
    cmp cx,0
    je saveMax 

    find:

        mov dx , [di]
        scasw
        jl savetempMax ;jl
      
    loop find

    saveMax:
    mov Max,ax
    mov MaxTemp,ax
    ret

    savetempMax:
        mov ax,dx
        jmp find
                                   
endp                                   


MakeNormal proc ;near
    cmp Max,0
    je  divNull
    xor cx,cx
    mov cl,byte ptr [ArrayLength]
    xor di,di
    xor si,si
    xor ax,ax
    xor dx,dx
    
    findSign:
        mov ax,MaxTemp
        mov Max,ax
        mov check,0
        cmp cl,0 
        je goEnd
        xor dx,dx 
        mov ax, Array[si] 
        xor ch,ch
        lea di,Answer 
        cmp Max,0
        jg setSign 
        cmp Max,0
        jl MaxIsNeg ; jl

    setPlus:
        mov byte ptr [di],'+'
        inc di

    makeNum: 
        cmp ch,precision
        jg saveNum 
        mov bx,ax ;
        
        idiv Max 
        
        cmp check,1 
        je makeFract
        
        
        call makeMainPart
        jmp increase
        
     makeFract:
        
        add ax,'0'
        
        mov [di],ax
        inc di
        
         mov ax,dx 
         mul ten  
         inc ch
         jmp makeNum
        
 
            
    increase:        
         mov ax , dx
         mul ten
         jmp makeNum

  
   saveNum:
        call output 
        add si,2
        jmp findSign 

   setSign:
    cmp ax,0 
    jge setPlus 
    mov byte ptr [di],'-' 
    inc di   
    mov bx,-1  
    imul bx
    jmp makeNum
     
   MaxIsNeg:
    push ax 
    mov ax,Max
    neg ax 
    mov Max,ax
    pop ax
    neg ax 
    jmp setPlus

   divNull:

    print maxNull
    mov ax,4c00h
    int 21h   
    ret   
    
    goEnd:
        mov ax,4c00h
        int 21h  
        ret 



endp
    
makeMainPart proc 

    push bx
 
    cmp ax,0
    jz ifNoMainPart  

MainPartExsists:
    
    mov bx , ax 
    add bx , '0' 
    mov  [di],bx 
    inc di
    mov byte ptr [di],'.'
    inc di
    jmp fin

ifNoMainPart:
    mov byte ptr [di],'0'
    inc di
    mov byte ptr [di],'.'
    inc di

fin:
    pop bx
    mov check,1      
    ret

endp
  


output proc
    print nextStr    
    print Answer        
    dec cl                
    ret                           
endp  
                                                       
end	start                         