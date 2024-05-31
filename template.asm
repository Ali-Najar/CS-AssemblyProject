%include "asm_io.inc" ;Ali Najar 401102701
section .data
    ;Defining the variables needed for calculating convolution
    kernel: times 1000 dd 0
    kernelSize: dq 0
    kernelRows: dq 0 
    matrix: times 1000000 dd 0
    matrixR: times 1000000 dd 0
    matrixG: times 1000000 dd 0
    matrixB: times 1000000 dd 0
    matrixSize: dq 0
    matrixRows: dq 0 
    resultMatR: times 1000000 dd 0
    resultMatG: times 1000000 dd 0
    resultMatB: times 1000000 dd 0
    convMatAddress: dq 0
    resMatAddress: dq 0
    resultMat: times 100000 dd 0
    resultSize: dq 0
    resultRows: dq 0 
    subMatrix: times 1000 dd 0 ;has the same size as kernel and traverses across the matrix
    matRow: dq 0
    matCol: dq 0
    subRow: dq 0
    subCol: dq 0
    resultRow: dq 0
    resultCol: dq 0
    sum : dd 0.0
    counter: dq 0
    print_address: dq 0
    print_row: dq 0
    print_col: dq 0
    print_size: dq 0
    calc_row: dq 0
    calc_col: dq 0
    calc_rows: dq 0
    conv_size: dq 0
    start_time: dq 0
    end_time: dq 0
    matrixM: times 1000000 dd 0
    Mrow: dq 0
    Mcol: dq 0
    Mrows: dq 0
    tSquare: dq 0
    convRes: times 1000000 dd 0

segment .text
    global asm_main
    extern printf

asm_main:
	push rbp ;saving the value of base pointer to restore it after the execution of program
    push rbx ;saving the value of registers to restore it after the execution of program
    push r12
    push r13
    push r14
    push r15

    sub rsp, 8 ;aligning the stack


    call matrixConvolution

    add rsp, 8 ;restoring the initial value of stack pointer

	pop r15  ;restoring the value of registers after the execution of program
	pop r14
	pop r13
	pop r12
    pop rbx
    pop rbp ;restoring the value of base pointer after the execution of program

	ret

matrixConvolution:
    sub rsp, 8
    
    call read_int
    mov [kernelRows],rax ;moving the kernel number of rows into kernelRows
    imul rax,rax
    mov [kernelSize],rax ;moving the kernel size into kernelSize

    ;getting kernel
    mov rbp, kernel ;using rbp as the address of kernel to get as input
    xor rdi, rdi
    mov rdi , qword[kernelSize] ;using rdi as the number of elements to get as input
    call getMat ;calling getMat to get the kernel

    call read_int
    mov [matrixRows],rax ;moving the matrix number of rows into matrixRows
    imul rax,rax
    mov [matrixSize],rax ;moving the matrix size into matrixSize

    ;getting matrix
    mov rbp, matrixR ;using rbp as the address of matrix to get as input
    xor rdi, rdi
    mov rdi , qword[matrixSize] ;using rdi as the number of elements to get as input
    call getMat ;calling getMat to get the matrix
    ;getting matrix
    mov rbp, matrixG ;using rbp as the address of matrix to get as input
    xor rdi, rdi
    mov rdi , qword[matrixSize] ;using rdi as the number of elements to get as input
    call getMat ;calling getMat to get the matrix
    ;getting matrix
    mov rbp, matrixB ;using rbp as the address of matrix to get as input
    xor rdi, rdi
    mov rdi , qword[matrixSize] ;using rdi as the number of elements to get as input
    call getMat ;calling getMat to get the matrix

    mov rdi,[matrixRows]
    call print_int
    call print_nl

    ;calculateConvolution
    mov r14,[matrixRows] ;r14 is used for size
    sub r14,[kernelRows]
    inc r14
    mov rbx,matrixR
    mov [convMatAddress],rbx
    mov rbx,resultMatR
    mov [resMatAddress],rbx
    call calculateConvolution ;calculate ordinary convolution


    ;calculateConvolution
    mov r14,[matrixRows] ;r14 is used for size
    sub r14,[kernelRows]
    inc r14
    mov rbx,matrixG
    mov [convMatAddress],rbx
    mov rbx,resultMatG
    mov [resMatAddress],rbx
    call calculateConvolution ;calculate ordinary convolution

    ;calculateConvolution
    mov r14,[matrixRows] ;r14 is used for size
    sub r14,[kernelRows]
    inc r14
    mov rbx,matrixB
    mov [convMatAddress],rbx
    mov rbx,resultMatB
    mov [resMatAddress],rbx
    call calculateConvolution ;calculate ordinary convolution

    mov r14,[matrixRows] ; r14 is used for size
    sub r14,[kernelRows]
    inc r14
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,resultMatR ;r12 for mat address
    call printResultMat ;printing ordinary convolution result

    mov r14,[matrixRows] ; r14 is used for size
    sub r14,[kernelRows]
    inc r14
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,resultMatG ;r12 for mat address
    call printResultMat ;printing ordinary convolution result
    
    mov r14,[matrixRows] ; r14 is used for size
    sub r14,[kernelRows]
    inc r14
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,resultMatB ;r12 for mat address
    call printResultMat ;printing ordinary convolution result
    
    ; call print_nl
    ; call fillMatrixM

    ; call convUsingMatMul

    ; mov r14,[matrixRows] ; r14 is used for size
    ; sub r14,[kernelRows]
    ; inc r14
    ; xor r12,r12
    ; mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    ; mov [print_col],r12
    ; mov r12,convRes
    ; call printResultMat ;printing ordinary convolution result

    xor rbx,rbx
    mov [matCol],rbx ;making matCol and matRow zero for upcoming usages
    mov [matRow],rbx

    add rsp, 8
    ret


getMat: ;this sub-routine uses rdi as the size of input (square of number of rows) and rbp as the address of matrix
    sub rsp, 8

    xor r12,r12 ;r12 is used as a counter
    mov r13,rdi ;r13 is used here instead of rdi
    readLoop:
        call read_float
        mov dword[rbp + r12 * 4],eax ;moving input into the r12 index of matrix
        inc r12
        cmp r12,r13 
        jl readLoop ;continuing the loop to get next input

    add rsp, 8
    ret

printResultMat: ;r14 is used to store to total number of rows of the matrix
    sub rsp, 8
    
    jmp traversing_result_mat_row ;initiating the loop

    changing_result_mat_row: ;outer loop
        call print_nl
        inc qword[print_row] ;incrementing the row
        sub [print_col],r14 ;making print_col zero

        cmp [print_row],r14 ;checking the criteria to end the outer loop
        jge endPrint ;ending the loop
 
        traversing_result_mat_row: ;inner loop

            mov rbx,[print_row] ;setting rbx to 4*([print_row]*r14+[print_col]) to get the index of current element in the matrix
            imul rbx,r14
            add rbx,[print_col]
            imul rbx,4
            
            mov edi,[r12+rbx] ;printing the current element of matrix
            call print_float
            mov edi,' '
            call print_char

            inc qword[print_col] ;incrementing print_col

            cmp [print_col],r14
            jl traversing_result_mat_row ;repeating the inner loop

            cmp [print_row],r14
            jl changing_result_mat_row ;repeating the outer loop

    endPrint:
    
    add rsp, 8
    ret

fillingSubMatrix: ;r12 for matrix address ;this sub-routine creates a sub-matrix of the original matrix which is to be multiplied by kernel
    sub rsp, 8

    mov r15,[kernelRows] ;move kernelRows into r15
    jmp traversing_row ;starting the for loop

    changing_row: ;outer for loop
        inc qword[subRow] ;incrementing the row
        inc qword[calc_row] ;incrementing the calc_row
        mov rbx,[kernelRows]
        sub [subCol],rbx ;making subCol and calc_col zero
        sub [calc_col],rbx
        
        cmp [subRow],r15 ;checking the criteria to end the construction of submatrix
        jge endFilling ;ending the construction

        traversing_row: ;inner for loop

            mov rbx,[subRow] ;setting rbx to 4*([subRow]*[kernelRows]+[subCol]) to get the index of current element in sub-matrix
            imul rbx,[kernelRows]
            add rbx,[subCol]
            imul rbx,4
            mov r13,[calc_row] ;setting r13 to 4*([calc_row]*[calc_rows]+[calc_col]) to get the index of current element in matrix
            imul r13,[calc_rows] 
            add r13,[calc_col]
            imul r13,4
            movss xmm1 , [r12+r13] ;moving the r13-th element of matrix to xmm1
            movss [subMatrix+rbx],xmm1 ;moving xmm1 into its corresponding index in subMatrix

            inc qword[subCol] ;incrementing subCol
            inc qword[calc_col] ;incrementing calc_col

            cmp [subCol],r15
            jl traversing_row ;repeating the inner loop

            cmp [subRow],r15
            jl changing_row ;repeating the outer loop

    endFilling: ;here we set subRow and calc_row to zero for further usage
    mov rbx,[kernelRows]
    sub [subRow],rbx
    sub [calc_row],rbx

    add rsp, 8
    ret

matrixMulForKernel:
    sub rsp, 8

    xorps xmm1,xmm1 ;setting xmm1 to zero to store the result in it
    xor r12,r12 ;using r12 as the loop counter
    multiplyLoop1:
        movss xmm1,[kernel + r12*4] ;move the r12-th element of kernel into xmm1
        mulss xmm1,[subMatrix + r12*4] ;multiplying the r12-th element of subMatrix with xmm1
        addss xmm1,[sum] ;adding sum to xmm1
        movss [sum],xmm1 ;moving xmm1 into sum
        inc r12 ;incrementing the counter
        cmp r12,[kernelSize] ;checking loop criteria
        jl multiplyLoop1 ;repeating the loop
    
    add rsp, 8
    ret


calculateConvolution:
    sub rsp, 8
    
    jmp traversing_matrix_row ;initiating the loop (the process of calculating covolution)

    changing_matrix_row: ;outer loop
        inc qword[matRow] ;incrementing the row
        sub [matCol],r14 ;making matCol zero

        cmp [matRow],r14 ;checking the criteria to end the outer loop
        jge endCalc ;ending the loop

        traversing_matrix_row: ;inner loop

            mov r12,[matrixRows] 
            mov [calc_rows],r12 ;setting calc_rows to matrixRows to be used in fillingSubMatrix
            mov r12,[matRow]
            mov [calc_row],r12 ;setting calc_row to matRow to be used in fillingSubMatrix
            mov r12,[matCol]
            mov [calc_col],r12 ;setting calc_col to matCol to be used in fillingSubMatrix
            mov r12,[convMatAddress]
            call fillingSubMatrix ;filling the sub-matrix (moving the elements of matrix to submatrix to be multiplied by kernel)

            call matrixMulForKernel ;multiplying kernel and sub-matrix

            mov rbx , [matrixRows] ;moving the index of resultMat to be filled into rbx
            sub rbx , [kernelRows] ;moving the index of resultMat to be filled into rbx
            inc rbx
            imul rbx, [matRow]
            add rbx , [matCol]
            movss xmm1,[sum] ;moving sum into register xmm1
            mov r15,[resMatAddress]
            movss [r15+rbx*4],xmm1 ;moving sum into the index rbx of resultMat

            xorps xmm1,xmm1 
            movss [sum],xmm1 ;setting sum to zero for the next iteration

            inc qword[matCol] ;incrementing matCol

            cmp [matCol],r14 
            jl traversing_matrix_row ;repeating inner loop

            cmp [matRow],r14
            jl changing_matrix_row ;repeating the outer loop

    endCalc: ;end of sub-routine

    xor rbx,rbx
    mov [matCol],rbx
    mov [matRow],rbx
    mov [calc_col],rbx
    mov [calc_row],rbx

    add rsp, 8
    ret

sys_gettimeofday_ms:
    mov rax, 96
    lea rdi, [rsp - 16]
    xor esi, esi
    syscall
    mov ecx, 1000
    mov rax, [rdi + 8]
    xor edx, edx
    div rcx
    mov rdx, [rdi]
    imul rdx, rcx
    add rax, rdx
    ret
