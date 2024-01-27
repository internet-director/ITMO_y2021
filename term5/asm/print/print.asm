section .text
    extern print

SECTION .data
    hi: db "10",0
SECTION .text

_uint128_is_not_null:
    push    ebp
    xor     eax, eax
    xor     edx, edx
    mov     ecx, [esp + 8]

null_rec:
    inc     edx
    mov     ebp, [ecx]
    or      eax, ebp
    add     ecx, 4
    cmp     edx, 4
    jne     null_rec
    pop     ebp
    ret

_hex_to_dec:
    movzx   eax, byte [esp + 4]
    cmp     eax, '9'
    jg      parseA
    sub     eax, '0'
    jmp     hex_ret
parseA:
    cmp     eax, 'F'
    jg      parsea
    sub     eax, 'A' - 10
    jmp     hex_ret
parsea:
    sub     eax, 'a' - 10
hex_ret:
    ret

_mstrlen:
    mov     ecx, [esp + 4]
    mov     eax, -1
len_rec:
    add     eax, 1
    cmp     byte [ecx + eax], 0
    jne     len_rec
    ret

_reverse:
    push    ebx
    push    edi
    mov     ecx, [esp + 12]
    mov     edx, [esp + 16]
    cmp     edx, 1
    jle     skip
    xor     edi, edi
loop:
    mov     ebx, edx
    sub     ebx, edi
    movzx   eax, byte [ecx + ebx - 1]
    movzx   ebx, byte [ecx + edi]
    mov     byte [ecx + edi], al
    mov     eax, ebx
    mov     ebx, edx
    sub     ebx, edi
    mov     byte [ecx + ebx - 1], al
    mov     ebx, edx
    shr     ebx, 1
    inc     edi
    cmp     edi, ebx
    jne     loop
skip:
    pop     edi
    pop     ebx
    ret


_uint128_init:
    push    ebx
    push    edi
    push    esi
    mov     ecx, [esp + 16]
    xor     eax, eax
init_arr:
    mov     word[ecx + eax * 2], 0
    inc     eax
    cmp     eax, 8
    jne     init_arr
    mov     ecx, [esp + 20]
init_inc:
    push    ecx
    call    _mstrlen
    add     esp, 4
    mov     ebx, eax
    xor     edx, edx
    xor     edi, edi
init_loop1:
    cmp     ebx, 0
    je      init_skip
    cmp     edi, 4
    je      init_skip
    dec     ebx
    mov     esi, [ecx + ebx]
    push    esi
    call    _hex_to_dec
    pop     esi
    mov     esi, edi
    inc     edi
    cmp     esi, 0
    je     skip_pow
pow:
    shl     eax, 4
    dec     esi
    cmp     esi, 0
    jne     pow
skip_pow:
    mov     esi, [esp + 16]
    add     word [esi + edx * 2], ax
    jmp     init_loop1
init_skip:
    xor     edi, edi
    inc     edx
    cmp     edx, 8
    jne     init_loop1
    pop     esi
    pop     edi
    pop     ebx
    ret


_uint128_not:
    mov     ecx, [esp + 4]
    xor     eax, eax
xor_rec:
    not     word [ecx + eax * 2]
    inc     eax
    cmp     eax, 8
    jne     xor_rec
    ret

_uint128_inc:
    mov     ecx, [esp + 4]
    xor     eax, eax
    mov     ebx, 1
xor_inc:
    movzx   edx, word [ecx + eax * 2]
    add     edx, ebx
    mov     word [ecx + eax * 2], dx
    shr     edx, 16
    mov     ebx, edx
    inc     eax
    cmp     eax, 8
    jne     xor_inc
    ret

_uint128_divmod:
    push    ebp
    push    ebx
    mov     ebp, [esp + 12] 
    mov     ebx, 10
    mov     ecx, 7
    xor     eax, eax
div_loop:
    xor     edx, edx
    add     ax, word [ebp+ecx*2]
    div     ebx
    mov     word [ebp+ecx*2], ax
    mov     eax, edx
    shl     eax, 16
    dec     ecx
    cmp     ecx, -1
    jne     div_loop

    pop     ebx
    pop     ebp
    mov     eax, edx
    add     eax, '0'
    ret


_print_hex_convert:
    push    ebp
    mov     ebp, esp
    push    ebx
    push    edi
    push    esi
    sub     esp, 24

    mov     ebx, [ebp+24]   ; hex_number

    push    ecx
    push    edx
    push    ebx
    call    _mstrlen
    add     esp, 4
    pop     edx
    pop     ecx

    mov     edi, eax        ; edi - length(hex_number
    xor     edx, edx        ; sign_minus

print_minus_loop:
    cmp     byte [ebx], '-'
    jne     print_minus_loop_end
    xor     edx, 1
    dec     edi
    inc     ebx
    jmp     print_minus_loop
print_minus_loop_end:

    cmp     edi, 2
    jl      print_0x_skip
    cmp     byte [ebx], '0'
    jne     print_0x_skip
    cmp     byte [ebx + 1], 'x'
    je      print_0x_start
    cmp     byte [ebx + 1], 'X'
    jne     print_0x_skip
print_0x_start:
    add     edi, 2
    add     ebx, 2
print_0x_skip:

    sub     esp, 16
    lea     esi, [esp]          ; number address
    push    ecx
    push    edx
    push    ebx
    push    esi
    call    _uint128_init
    add     esp, 8
    pop     edx
    pop     ecx

    xor     eax, eax
    mov     al, byte [esp+15]
    and     eax, 0x80
    cmp     eax, 0x80
    jne      print_sign_change_end

    xor     edx, 1

    push    ecx
    push    edx
    push    esi
    push    esi
    call    _uint128_not
    call    _uint128_inc
    add     esp, 8
    pop     edx
    pop     ecx

    xor     eax, eax
    mov     al, byte [esp+15]
    and     eax, 0x80
    cmp     eax, 0x80
    jne      print_sign_change_end
    mov     edx, 1

print_sign_change_end:
    mov     ebx, [ebp+16]           ; out_buf address
    mov     ecx, [ebp+12]           ; save sign
    mov     [ecx], edx
    xor     edx, edx

print_skip_init_loop:
    
    push    ecx
    push    edx
    push    esi
    call    _uint128_is_not_null
    add     esp, 4
    pop     edx
    pop     ecx
    cmp     eax, 0
    je      print_skip_init_loop_end
    
    push    ecx
    push    edx
    push    esi
    call    _uint128_divmod
    add     esp, 4
    pop     edx
    pop     ecx
    mov     byte [ebx+edx], al
    inc     edx
    jmp     print_skip_init_loop

print_skip_init_loop_end:
    mov     esi, [ebp+8]            ; save out_buf_length
    mov     [esi], edx

    cmp     edx, 0
    jne     print_skip_out_init
    mov     byte [ebx], '0'
    mov     byte [ebx+1], 0
    inc     dword [esi]
    mov     ecx, [ebp+12]           ; save sign
    mov     dword [ecx], 0

print_skip_out_init:
    push    ecx
    push    edx
    push    edx
    push    ebx
    call    _reverse
    add     esp, 8
    pop     edx
    pop     ecx

    add     esp, 16

    mov     ecx, [ebp+20]           ; format addr
print_format_loop:
;    cmp     

print_format_loop_end:

    add     esp, 24
    pop     esi
    pop     edi
    pop     ebx
    pop     ebp
    ret


_print_format_checker:
    push    ebp
    mov     ebp, esp
    push    ebx
    push    edi
    push    esi

    xor     eax, eax
    xor     ecx, ecx
    mov     ebx, [ebp+8]    ; format
    mov     edx, [ebp+12]   ; minus
    mov     edi, [ebp+16]   ; zero
    mov     esi, [ebp+20]   ; plus
    mov     ebp, [ebp+24]   ; empty


format_main_loop:
    mov     al, byte [ebx+ecx]
    cmp     eax, 0
    je      format_main_loop_end

    cmp     eax, '-'
    jne     format_first_skip
    mov     dword [edx], 1
    mov     dword [edi], 0
format_first_skip:

    cmp     eax, '+'
    jne     format_second_skip
    mov     dword [esi], 1
    mov     dword [ebp], 0
format_second_skip:

    cmp     eax, ' '
    jne     format_ff_skip
    cmp     dword [esi], 0
    jne     format_ff_skip
    mov     dword [ebp], 1
format_ff_skip:

    cmp     eax, '0'
    jne     format_ss_skip
    cmp     dword [edx], 0
    jne     format_ss_skip
    mov     dword [edi], 1
format_ss_skip:
    
    cmp     eax, '1'
    jl      format_end_skip
    cmp     eax, '9'
    jg      format_end_skip
    mov     esi, [esp+20+20]
    mov     dword [esi], ecx
    jmp     format_main_loop_end
    
format_end_skip:

    inc     ecx
    jmp     format_main_loop

format_main_loop_end
    pop     esi
    pop     edi
    pop     ebx
    pop     ebp
    ret



_print_format_setter:
    push    ebp
    mov     ebp, esp
    push    ebx
    push    edi
    push    esi

    mov     esi, [ebp+8]    ; format
    xor     eax, eax

    cmp     dword [ebp+16] , -1
    je      format_white_loop_end
    mov     ecx, [ebp+16]
    mov     ebx, 10

format_white_loop:
    movzx   edi, byte [esi+ecx]
    cmp     edi, 0
    je      format_white_loop_end
    mul     ebx
    sub     edi, '0'
    add     eax, edi
    inc     ecx
    jmp     format_white_loop
format_white_loop_end:

    cmp     dword [ebp+24], 0
    je      format_out_loop_end
    mov     ecx, [ebp+20]
    mov     edi, [ebp+12]   ; out_bif

format_out_loop:
    cmp     ecx, 0
    je      format_out_loop_end

    mov     dl, byte[edi+ecx-1]
    mov     byte [edi+ecx], dl

    dec     ecx
    jmp     format_out_loop
format_out_loop_end:

    pop     esi
    pop     edi
    pop     ebx
    pop     ebp
    ret



_print_format_final:
    push    ebp
    mov     ebp, esp
    push    ebx
    push    edi
    push    esi

    mov     esi, [ebp+8] ; out
    mov     eax, [ebp+16] ; pos
    mov     ebx, [ebp+20] ; sign
    mov     edi, [ebp+12] ; width
    mov     edx, eax
    add     edx, ebx        ; kostil
    mov     al, byte [ebp+32] ;fill

    cmp     edi, edx
    jle     format_final_end 
    
    
    cmp     dword [ebp+24], 0
    jne     format_final_skip_minus

    mov     ecx, edx
    sub     edx, edi
    neg     edx

format_final_loop1:
    dec     ecx
    cmp     ecx, -1
    je      format_final_loop1_end
    mov     al, byte [esi+ecx]
    add     ecx, edx
    mov     [esi+ecx], al
    sub     ecx, edx
    jmp     format_final_loop1

format_final_loop1_end:
    mov     eax, edi
    sub     eax, [ebp+16]   ; diff

    cmp     dword [ebp+28], 0
    jne     format_final_skip_step
    sub     eax, ebx
    xor     ebx, ebx

format_final_skip_step:
    mov     cl, byte [ebp+32] ;fill

format_final_skip_loop:
    cmp     ebx, eax
    je      format_final_skip_loop_end
    mov     byte [esi+ebx], cl
    inc     ebx
    jmp     format_final_skip_loop

format_final_skip_loop_end:
    jmp     format_funal_skip

format_final_skip_minus:
    dec     edi
    cmp     edi, edx
    jl      format_funal_skip
    mov     byte [esi+edi], al
    jmp     format_final_skip_minus

format_funal_skip:
    mov     edx, [ebp+12]

format_final_end:
    mov     byte [esi+edx], 0

    pop     esi
    pop     edi
    pop     ebx
    pop     ebp
    ret


print:
    push    ebp
    mov     ebp, esp
    push    ebx
    push    edi
    push    esi


    sub     esp, 32
    xor     esi, esi

print_base_loop:
    cmp     esi, 8
    je      print_base_loop_end
    mov     dword [esp+esi*4], 0
    inc     esi
    jmp     print_base_loop
print_base_loop_end:

    mov     dword [esp+24], -1
    mov     edx, [ebp+8]    ; out_buf
    mov     ebx, [ebp+12]   ; format
    mov     ecx, [ebp+16]   ; hex_number

    lea     edi, [esp+4]
    lea     esi, [esp]

    push    ecx
    push    ebx
    push    edx
    push    edi
    push    esi  
    call    _print_hex_convert
    add     esp, 8
    pop     edx
    pop     ebx
    pop     ecx



    push    edx
    lea     esi, [esp+24+4]
    push    esi
    lea     esi, [esp+20+8]
    push    esi
    lea     esi, [esp+16+12]
    push    esi
    lea     esi, [esp+12+16]
    push    esi
    lea     esi, [esp+8+20]
    push    esi
    push    ebx
    call    _print_format_checker
    pop     ebx
    add     esp, 20
    pop     edx

    xor     edi, edi
    or      edi, [esp+4]
    or      edi, [esp+16]
    or      edi, [esp+20]
    
    
    push    edi
    mov     esi, [esp+4]
    push    esi
    mov     esi, [esp+24+8]
    push    esi
    push    edx
    push    ebx
    call    _print_format_setter
    pop     ebx
    pop     edx
    add     esp,  8
    pop     edi


    cmp     dword [esp+4], 0
    jne     print_minus
    cmp     dword [esp+16], 0
    jne     print_plus
    cmp     dword [esp+20], 0
    jne     print_empty

print_end_of_sign:
    mov     esi, ' '
    cmp     dword [esp+12], 0
    je      print_skip_fill
    mov     esi, '0' 

print_skip_fill:
    push    esi
    mov     esi, [esp+12+4]
    push    esi
    mov     esi, [esp+8+8]
    push    esi
    push    edi
    mov     esi, [esp+16]
    push    esi
    push    eax
    push    edx
    call    _print_format_final
    add     esp, 28
    add     esp, 32

    pop     esi
    pop     edi
    pop     ebx
    pop     ebp
    ret


print_minus:
    mov     byte [edx], '-'
    jmp     print_end_of_sign
print_plus:
    mov     byte [edx], '+'
    jmp     print_end_of_sign
print_empty:
    mov     byte [edx], ' '
    jmp     print_end_of_sign