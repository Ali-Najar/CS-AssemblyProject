segment .data
print_int_format: db        "%ld", 0

print_float_format: db        "%lf", 0

read_int_format: db         "%ld", 0

read_float_format: db        "%f", 0
segment .text
    global print_int
    global print_char
    global print_string
    global print_nl
    global read_int
    global read_char
    global print_float
    global read_float
    extern printf
    extern putchar
    extern puts
    extern scanf
    extern getchar

print_int:
    sub rsp, 8

    mov rsi, rdi

    mov rdi, print_int_format
    mov rax, 1 ; setting rax (al) to number of vector inputs
    call printf
    
    add rsp, 8 ; clearing local variables from stack

    ret


print_char:
    sub rsp, 8

    call putchar
    
    add rsp, 8 ; clearing local variables from stack

    ret


print_string:
    sub rsp, 8

    call puts
    
    add rsp, 8 ; clearing local variables from stack

    ret


print_nl:
    sub rsp, 8

    mov rdi, 10
    call putchar
    
    add rsp, 8 ; clearing local variables from stack

    ret


read_int:
    sub rsp, 8

    mov rsi, rsp
    mov rdi, read_int_format
    mov rax, 1 ; setting rax (al) to number of vector inputs
    call scanf

    mov rax, [rsp]

    add rsp, 8 ; clearing local variables from stack

    ret


read_char:
    sub rsp, 8

    call getchar

    add rsp, 8 ; clearing local variables from stack

    ret

print_float:
    sub rsp, 8

    mov eax, edi
    movd xmm0, eax
    cvtss2sd xmm0, xmm0
    
    mov rdi, print_float_format
    mov rax, 1

    call printf

    add rsp, 8

    ret

read_float:
    sub rsp, 8

    mov rsi, rsp
    mov rdi, read_float_format
    mov rax, 1
    call scanf

    mov eax, dword [rsp]
    add rsp, 8

    ret
