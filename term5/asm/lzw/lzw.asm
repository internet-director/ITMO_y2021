global _lzw_decodes

section .text
;    extern lzw_decode

_lzw_decodes:
    mov     eax, dword [esp+4]
    test    eax, eax
    jz      invalid_args_error          ; in == NULL

    mov     eax, dword [esp+12]
    test    eax, eax
    jz      invalid_args_error          ; out == NULL

    mov     eax, dword [esp+8]
    test    eax, eax
    jz      invalid_empty_input         ; in_size == NULL

    mov     eax, dword [esp+16]
    test    eax, eax
    jz      invalid_args_error          ; out_size == NULL

    push    ebp
    push    ebx
    push    edi
    push    esi
    sub     esp, 16444

    mov     dword [esp+4], 0            ; bbuf_high
    mov     dword [esp+12], 0           ; bbuf_low
    mov     dword [esp+8], 0            ; bbits
    mov     edx, 9                      ; cursize
    mov     dword [esp+16], eax         ; out_size
    mov     dword [esp+20], 512         ; top_slot
    mov     dword [esp+24], 258         ; slot
    mov     dword [esp+28], -1          ; oc
    mov     dword [esp+32], -1          ; fc
    mov     eax, dword [esp+12368+4096]
    mov     dword [esp+36], eax         ; in
    mov     eax, dword [esp+12372+4096]
    mov     dword [esp+44], eax         ; in_size
    mov     eax, dword [esp+16444+16+12]
    mov     dword [esp+48], eax         ; out

; edx - cursize

main_loop:
    mov     ebx, dword [esp+4]          ; get bbuf_high
    mov     esi, dword [esp+12]         ; get bbuf_low
    mov     ebp, dword [esp+8]          ; get bbits
    mov     edi, dword [esp+44]         ; get in_size

bbuf_bbits_start:
    cmp     ebp, edx
    jge     init_code                   ; bbits < cursize
    cmp     edi, 0
    jl      init_code

    cmp     edi, 4
    jl      bbuf_bbits_end              ; last part of data

    add     ebp, 32                     ; bbits += 32
    sub     dword [esp+44], 4           ; in_size -= 4
    mov     ebx, esi
    mov     eax, dword [esp+36]
    mov     esi, [eax]
    add     dword [esp+36], 4           ; in += 4
    bswap   esi

    mov     dword [esp+4], ebx          ; put bbuf_high
    mov     dword [esp+12], esi         ; put bbuf_low

init_code:
    sub     ebp, edx                    ; bbits -= cursize
    mov     dword [esp+8], ebp          ; put bbits
    mov     ecx, ebp

    shr     esi, cl                     ; bbuf_low >>= bbits
    cmp     ebp, 0
    je      skip_code
    mov     ecx, 32
    sub     ecx, ebp
    shl     ebx, cl
    or      esi, ebx

skip_code:
    mov     eax, dword [esp+20]         ; get topslot
    dec     eax
    and     esi, eax
    mov     ebx, esi

; ebx - code(c)
    cmp     ebx, 256
    je      code_is_256

    cmp     ebx, 257
    je      skip_main_loop

; ebx - code
; esi - tmp code
; edx - cursize
; edi - slot
    mov     ebp, esp
    add     ebp, 52+4096*3
    mov     esi, ebx                    ; save code
    mov     edi, dword [esp+24]         ; get slot

    cmp     ebx, edi                    ; code == p.slot
    jne     hard_part_cup_else
    cmp     dword [esp+32], 0           ; p.fc >= 0
    jl      hard_part_cup_else

    mov     eax, dword [esp+32]
    mov     byte [ebp], al              ; p.sp++ = p.fc
    inc     ebp
    mov     esi, dword [esp+28]         ; code = p.oc
    
    jmp     hard_part_codeu_loop_start

hard_part_cup_else:
    cmp     ebx, edi                    ; code >= p.slot
    jge     skip_main_loop

hard_part_codeu_loop_start:
    cmp     esi, 258                       ; code >= 258
    jl      hard_part_codeu_loop_end

    mov     eax, dword [esp+52+esi]
    mov     byte [ebp], al                 ; p.sp++ = p.suffix[code]
    inc     ebp

    movzx   esi, word [esp+52+4096+esi*2]  ; code = p.prefix[code]
    jmp     hard_part_codeu_loop_start

hard_part_codeu_loop_end:
    mov     eax, esi
    mov     byte [ebp], al                 ; *p.sp++ = code;
    inc     ebp
    mov     dword [esp+32], esi           ; fc set code 
    mov     dword [esp+40], esi

    mov     ecx, esp
    add     ecx, 52+4096*3

write_to_out_start:
    cmp     ebp, ecx
    je      write_to_out_end

    dec     ebp
    movzx   eax, byte [ebp]
    mov     esi, dword [esp+48]
    mov     byte [esi], al
    inc     dword [esp+48]

    dec     dword [esp+16]                 ; l--
    cmp     dword [esp+16], 0
    je      skip_main_loop
    jmp     write_to_out_start

write_to_out_end:

    cmp     edi, dword [esp+20]           ; p.slot < topslot
    jge     skip_supre_update_skip
    cmp     dword [esp+28], 0             ; p.oc >= 0
    jl      skip_supre_update_skip
    mov     eax, dword [esp+40]
    mov     byte [esp+52+edi], al         ; p.suffix[p.slot] = code
    mov     eax, dword [esp+28]
    mov     word [esp+52+4096+edi*2], ax  ; p.prefix[p.slot] = p.oc;
    inc     dword [esp+24]                ; p.slot++
    inc     edi

skip_supre_update_skip:
    mov     dword [esp+28], ebx           ; oc set code   
    
    inc     edi
    cmp     edi, dword [esp+20]         ; p.slot + 1 >= topslot
    jl      hard_part_end
    cmp     edx, 12                     ; cursize < 12
    jge     hard_part_end
    inc     edx                         ; cursize++
    shl     dword [esp+20], 1           ; topslot<<=1
hard_part_end:
    jmp     main_loop

    
bbuf_bbits_end:
    mov     dword [esp+44], 0           ; in_size = 0
    shl     edi, 3
    add     ebp, edi
    mov     ecx, edi

    shl     esi, cl
    mov     ecx, 32                     ; bbuf_low <<=  in_size;
    sub     ecx, edi                    ; bbuf_high >>= (32 - in_size);
    shr     ebx, cl
    mov     ecx, edi
    sub     ecx, 8

bbuf_low_init:
    cmp     ecx, 0
    jl      bbuf_low_end
    mov     eax, dword [esp+36]
    movzx   eax, byte [eax]
    shl     eax, cl
    or      esi, eax
    inc     dword [esp+36]
    sub     ecx, 8
    jmp     bbuf_low_init

bbuf_low_end:
    
    mov     dword [esp+4], ebx          ; put bbuf_high
    mov     dword [esp+12], esi         ; put bbuf_low
    jmp     init_code

skip_main_loop:
    mov     eax, dword [esp+12380+4096]
    sub     eax, dword [esp+16]
    add     esp, 16444
    pop     esi
    pop     edi
    pop     ebx
    pop     ebp
    ret

invalid_args_error:
    mov     eax, -1
    ret

invalid_empty_input:
    mov     eax, 0
    ret

; edx - cursize
code_is_256:
    mov     edx, 9                      ; cursize
    mov     dword [esp+20], 512         ; top_slot
    mov     dword [esp+24], 258         ; slot
    mov     dword [esp+28], -1          ; oc
    mov     dword [esp+32], -1          ; fc
    jmp     main_loop
