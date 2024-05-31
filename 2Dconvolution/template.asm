%include "asm_io.inc" ;Ali Najar 401102701
section .data
    ;Defining the variables needed for matrix multiplication
    mat1: times 1000000 dd 0
    mat2: times 1000000 dd 0
    matNuElements: dq 0
    matNuRows: dq 0
    matR: dq 0
    matC: dq 0
    result: dd 0.0
    resultParallel: dd 0.0
    mulResult: times 1000000 dd 0
    mulSIMDResult: times 1000000 dd 0
    transposeMat: times 1000000 dd 0
    zero_Aligned_Mat: times 1000000 dd 0
    zero_Aligned_Mat_For_T: times 1000000 dd 0
    zeroAlignedSize: dq 0
    ;Defining the variables needed for calculating convolution
    kernel: times 100 dd 0
    kernelSize: dq 0
    kernelRows: dq 0 
    matrix: times 10000 dd 0
    matrixSize: dq 0
    matrixRows: dq 0 
    resultMat: times 10000 dd 0
    resultSize: dq 0
    resultRows: dq 0 
    subMatrix: times 100 dd 0 ;has the same size as kernel and traverses across the matrix
    matRow: dq 0
    matCol: dq 0
    subRow: dq 0
    subCol: dq 0
    resultRow: dq 0
    resultCol: dq 0
    sum : dd 0.0
    edgeExtended: times 50000 dd 0
    exRow: dq 0
    exCol: dq 0
    counter: dq 0
    edgeMirrored: times 50000 dd 0
    mirRow: dq 0
    mirCol: dq 0
    edgeZero: times 50000 dd 0
    zeroRow: dq 0
    zeroCol: dq 0
    print_address: dq 0
    print_row: dq 0
    print_col: dq 0
    print_size: dq 0
    calc_row: dq 0
    calc_col: dq 0
    calc_rows: dq 0
    conv_size: dq 0
    matrixM: times 2000000 dd 0
    Mrow: dq 0
    Mcol: dq 0
    Mrows: dq 0
    tSquare: dq 0
    convRes: times 1000000 dd 0
    start_time_h: dq 0
    end_time_h: dq 0
    start_time_l: dq 0
    end_time_l: dq 0
    time: dq 0
    counterForRepeatingSIMD: dq 0

    ;Defining the messages of user interface.
    menu : dq "Please choose an action by entering its number: ",0
    matrix_mul : dq "1. multiplication of two matrices. ",0
    twoDconvolution : dq "2. calculating 2D-convolution of two matrices. ",0
    exit_message : dq "3. exit. ",0
    choosing_action_failed : dq "You should enter either 1 , 2 or 3 ! please try again. ",0
    entering_mat_dim : dq "Please enter the dimension of the square matrices:(an integer between 1 and 200) ",0
    entering_dim_error : dq "Enter an integer between 1 and 200 ! try again. ",0
    entering_mat1 : dq "Please enter the entries of matrix 1: ",0
    entering_mat2 : dq "Please enter the entries of matrix 2: ",0
    result_message : dq "Result of product using ordinary method: ",0
    resultParallel_message : dq "Result of product using parallel processing method(using vector dot product): ",0
    result_matrix_mul : dq "Result of matrix multiplication using ordinary method: ",0
    result_matrix_mul_parallel : dq "Result of matrix multiplication using parallel method: ",0
    result_time: dq "calculated running time of the ordinary multiplication:(In nanoseconds) ",0
    result_time_SIMD: dq "calculated running time of the parallel multiplication:(In nanoseconds) ",0
    
    enter_kernel_size : dq "Enter the dimension of the kernel:(an integer between 1 and 10) ",0
    enter_kernel : dq "Enter the entries of the kernel: ",0
    enter_matrix_size : dq "Enter the dimension of the matrix.(an integer between 1 and 100) ",0
    enter_matrix : dq "Enter the entries of the matrix: ",0
    ordinary_result_matrix_output : dq "Result of ordinary 2D-convolution of matrix and kernel: ",0
    extended_result_matrix_output : dq "Result of extended edge 2D-convolution of matrix and kernel: ",0
    mirrored_result_matrix_output : dq "Result of mirrored edge 2D-convolution of matrix and kernel: ",0
    zero_result_matrix_output : dq "Result of zero edge 2D-convolution of matrix and kernel: ",0
    result_matrix_output_using_mul : dq "Result of 2D-convolution of matrix and kernel using matrix multiplication: ",0

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

    menu_loop:

    mov rdi,menu  ;printing the menu
    call print_string

    mov rdi , matrix_mul ;printing action 1
    call print_string

    mov rdi , twoDconvolution ;printing action 2
    call print_string

    mov rdi , exit_message ;printing action 3
    call print_string

    jmp skipMessage ;jumping the error message for the first time
    failed:
    mov rdi , choosing_action_failed ;printing error message for invalid inputs
    call print_string
    skipMessage:

    call read_int ;getting the number of action to be done.
    cmp rax,1
    je action1 ;jumping to action 1 if the input was 1
    cmp rax,2
    je action2 ;jumping to action 2 if the input was 2
    cmp rax,3
    je action3 ;jumping to action 3 if the input was 3
    jmp failed
    
    action1: ;calling the matrix multiplication sub-routine
        call matrixMultiplication
        jmp menu_loop ;going back to the menu to do another action

    action2: ;calling the two dimensional convolution sub-routine
        call matrixConvolution
        jmp menu_loop ;going back to the menu to do another action

    action3: ;ending the program
        jmp end

    end:

    add rsp, 8 ;restoring the initial value of stack pointer

	pop r15  ;restoring the value of registers after the execution of program
	pop r14
	pop r13
	pop r12
    pop rbx
    pop rbp ;restoring the value of base pointer after the execution of program

	ret

matrixMultiplication:
    sub rsp, 8

    mov rdi,entering_mat_dim ;printing the message of getting matrix dimension
    call print_string
    jmp firstTime ;skipping the print of error for the first time

    ;getting dimension
    gettingDim:
        mov rdi,entering_dim_error ;printing error if matrices dimension was invalid
        call print_string
        
        firstTime: ;getting matrix dimension
        call read_int
        cmp rax,1
        jl gettingDim ;if mat dim was less than one skip
        cmp rax,200
        jg gettingDim ;if mat dim was more than one hundred skip
        mov [matNuRows],rax ;moving the matrix number of rows into matNuRows
        imul rax,rax ;squaring rax
        mov [matNuElements],rax ;moving rax into matNuElements

    ;getting mat1
    mov rdi,entering_mat1 ;entering matrix 1 message
    call print_string
    mov rbp, mat1 ;using rbp as the address of matrix to get as input
    xor rdi, rdi
    mov rdi , qword[matNuElements] ;using rdi as the number of elements to get as imput
    call getMat ;calling getMat to get the matrix

    ;getting mat2
    mov rdi,entering_mat2 ;entering matrix 2 message
    call print_string
    mov rbp, mat2 ;moving the address of matrix 2 into rbp
    xor rdi, rdi
    mov rdi , [matNuElements] ;using rdi as the number of elements to get as imput
    call getMat ;calling getMat to get the matrix
    
    call matrixMul ;call the matrix multiplication sub-routine (element-wise multiplication and summing all of them)
    call MulSIMD ;call the matrix multiplication sub-routine (element-wise multiplication and summing all of them) (USING PARALLEL PROCESSING METHODS)
    
    call print_nl
    mov rdi,result_message ;printing result message
    call print_string
    mov edi,[result] ;printing the result value
    call print_float
    call print_nl

    call print_nl
    mov rdi,resultParallel_message ;printing result message using SIMD
    call print_string
    mov edi,[resultParallel] ;printing result value SIMD
    call print_float
    call print_nl
    call print_nl

    xorps xmm1,xmm1
    movss [result],xmm1 ;setting result to zero for next actions
    movss [resultParallel],xmm1 ;setting resultParallel to zero for next actions

    call print_nl
    mov rdi,result_matrix_mul ;printing result message
    call print_string

    rdtsc; save the current clock of computer to save it in rax and rdx
    mov [start_time_h],rdx ;move the high 32-bit of clock which is in rdx into start_time_h   ;this part is used for better performance
    mov [start_time_l],rax ;move the low 32-bit of clock which is in rax into start_time_l
    mov [end_time_h],rdx
    mov [end_time_l],rax
    mov [time],rdx
    call matrixOrdinaryMul


    rdtsc; save the current clock of computer to save it in rax and rdx
    mov [start_time_h],rdx ;move the high 32-bit of clock which is in rdx into start_time_h
    mov [start_time_l],rax ;move the low 32-bit of clock which is in rax into start_time_l
    
    call matrixOrdinaryMul ;calculating the ordinary multiplication of matrices

    rdtsc; save the current clock of computer to save it in rax and rdx
    mov [end_time_h],rdx ;move the high 32-bit of clock which is in rdx into end_time_h
    mov [end_time_l],rax ;move the low 32-bit of clock which is in rax into end_time_l
    mov rbx , [end_time_h] ; rbx = end_time_h
    sub rbx , [start_time_h] ; rbx = end_time_h - start_time_h
    shl rbx , 32 ; rbx = (end_time_h - start_time_h)*(2^32)
    add rbx , [end_time_l] ; rbx = (end_time_h - start_time_h)*(2^32) + end_time_l
    sub rbx , [start_time_l] ; rbx = (end_time_h - start_time_h)*(2^32) + end_time_l - start_time_l
    mov rax , rbx ; we are trying to divide rbx by the clock rate of cpu
    imul rax , 10 ; rax = 10*rbx
    mov rbx , 30
    xor rdx,rdx
    idiv rbx ; rax = rbx/3 (my cpu is 3GHz)
    mov [time],rax ;execution time in nanoseconds

    xor rbx,rbx
    mov [matR],rbx ;setting matR and matC to zero for upcoming usages
    mov [matC],rbx 

    mov r14,[matNuRows] ; r14 is used for size
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,mulResult ;r12 for mat address
    call printResultMat ;printing the result of multiplication

    call print_nl
    mov rdi,result_time
    call print_string

    mov rdi , [time] ;get time in nanoseconds
    call print_int
    call print_nl

    call print_nl
    mov rdi,result_matrix_mul_parallel ;printing result message
    call print_string

    call matrixSIMDMul ;calculating the ordinary multiplication of matrices using SIMD

    xor rbx,rbx
    mov [matR],rbx ;setting matR and matC to zero for upcoming usages
    mov [matC],rbx

    mov r14,[matNuRows] ; r14 is used for size
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,mulResult ;r12 for mat address
    call printResultMat ;printing the result of multiplication using SIMD
    
    call print_nl
    mov rdi,result_time_SIMD
    call print_string

    mov rdi , [time] ;get time in nanoseconds
    call print_int
    call print_nl

    call print_nl
    call print_nl

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

matrixMul: ;multiplies mat1 and mat2 element-wise
    sub rsp, 8

    xorps xmm1,xmm1 ;setting xmm1 to zero to store the result in it
    xor r12,r12 ;using r12 as the loop counter
    multiplyLoop:
        movss xmm1,[mat1 + r12*4] ;move the r12-th element of mat1 into xmm1
        mulss xmm1,[mat2 + r12*4] ;multiplying the r12-th element of mat2 with xmm1
        addss xmm1,[result] ;adding the result to xmm1
        movss [result],xmm1 ;moving xmm1 into result
        inc r12 ;incrementing the counter
        cmp r12,[matNuElements] ;checking loop criteria
        jl multiplyLoop ;repeating the loop

    add rsp, 8
    ret

MulSIMD: ;multiplies mat1 and mat2 element-wise using SIMD
    sub rsp, 8

    xor r12,r12
    multiplySIMDLoop: 
        movups xmm1,[mat1 + r12*4] ;move the r12-th to (r12+3)th element of mat1 into xmm1
        movups xmm2,[mat2 + r12*4] ;move the r12-th to (r12+3)th element of mat2 into xmm2
        dpps xmm1,xmm2,0xF1 ;calculate the dot product of xmm1 and xmm2
        addss xmm1,[resultParallel] ;adding the resultParallel to xmm1
        movss [resultParallel],xmm1 ;moving xmm1 into resultParallel
        add r12,4 ;adding the index with 4
        cmp r12,[matNuElements] ;checking loop criteria
        jl multiplySIMDLoop ;repeating the loop

    add rsp, 8
    ret

matrixConvolution:
    sub rsp, 8
    
    mov rdi,enter_kernel_size ;enter kernel size message
    call print_string

    jmp firstTimeKernel ;skipping error for the first time

    ;getting dimension
    gettingKernelSize:
        mov rdi,entering_dim_error ;printing error if kernel dimension was invalid
        call print_string
        
        firstTimeKernel: ;getting kernel dimension
        call read_int
        cmp rax,1
        jl gettingKernelSize ;if kernel dim was less than one skip
        cmp rax,10
        jg gettingKernelSize ;if kernel dim was more than ten skip
        mov [kernelRows],rax ;moving the kernel number of rows into kernelRows
        imul rax,rax
        mov [kernelSize],rax ;moving the kernel size into kernelSize

    ;getting kernel
    mov rdi,enter_kernel ;entering kernel message
    call print_string
    mov rbp, kernel ;using rbp as the address of kernel to get as input
    xor rdi, rdi
    mov rdi , qword[kernelSize] ;using rdi as the number of elements to get as input
    call getMat ;calling getMat to get the kernel

    mov rdi,enter_matrix_size ;enter matrix size message
    call print_string

    jmp firstTimeMatrix ;skipping error for the first time

    ;getting dimension
    gettingMatrixSize:
        mov rdi,entering_dim_error ;printing error if matrix dimension was invalid
        call print_string
        
        firstTimeMatrix: ;getting matrix dimension
        call read_int
        cmp rax,1
        jl gettingMatrixSize ;if matrix dim was less than one skip
        cmp rax,100
        jg gettingMatrixSize ;if matrix dim was more than one hundred skip
        mov [matrixRows],rax ;moving the matrix number of rows into matrixRows
        imul rax,rax
        mov [matrixSize],rax ;moving the matrix size into matrixSize

    ;getting matrix
    mov rdi,enter_matrix ;entering matrix message
    call print_string
    mov rbp, matrix ;using rbp as the address of matrix to get as input
    xor rdi, rdi
    mov rdi , qword[matrixSize] ;using rdi as the number of elements to get as input
    call getMat ;calling getMat to get the matrix

    ;calculateConvolution
    mov r14,[matrixRows] ;r14 is used for size
    sub r14,[kernelRows]
    inc r14
    call calculateConvolution ;calculate ordinary convolution

    ;print result
    call print_nl
    mov rdi,ordinary_result_matrix_output ;printing ordinary convolution result message
    call print_string

    mov r14,[matrixRows] ; r14 is used for size
    sub r14,[kernelRows]
    inc r14
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,resultMat ;r12 for mat address
    call printResultMat ;printing ordinary convolution result

    call print_nl
    call extendedConvolution ;calculate extended convolution
        
    ;print result
    mov rdi,extended_result_matrix_output ;printing extended convolution result message
    call print_string

    mov r14,[conv_size] ; r14 is used for size
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,resultMat ;r12 for mat address
    call printResultMat ;printing extended convolution result

    call print_nl
    call mirroredConvolution ;calculate mirrored convolution
    
    ;print result
    mov rdi,mirrored_result_matrix_output ;printing mirrored convolution result message
    call print_string

    mov r14,[conv_size] ; r14 is used for size
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,resultMat ;r12 for mat address
    call printResultMat ;printing mirrored convolution result

    call print_nl
    call zeroConvolution ;calculate zero-edge convolution

    ;print result
    mov rdi,zero_result_matrix_output ;printing zero-edge convolution result message
    call print_string

    mov r14,[conv_size] ; r14 is used for size
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,resultMat ;r12 for mat address
    call printResultMat ;printing zero-edge convolution result

    ;print result
    call print_nl
    mov rdi,result_matrix_output_using_mul ;printing convolution result message
    call print_string

    call fillMatrixM ;creating matrix M

    call convUsingMatMul ;calculating the convolution
    
    mov r14,[matrixRows] ; r14 is used for size
    sub r14,[kernelRows]
    inc r14
    xor r12,r12
    mov [print_row],r12 ;print_row and print_col are used for usage in printResultMat
    mov [print_col],r12
    mov r12,convRes ;r12 for mat address
    call printResultMat ;printing ordinary convolution result

    call print_nl
    call print_nl

    xor rbx,rbx
    mov [matCol],rbx ;making matCol and matRow zero for upcoming usages
    mov [matRow],rbx

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
            mov r12,matrix
            call fillingSubMatrix ;filling the sub-matrix (moving the elements of matrix to submatrix to be multiplied by kernel)
            
            call matrixMulForKernel ;multiplying kernel and sub-matrix

            mov rbx , [matrixRows] ;moving the index of resultMat to be filled into rbx
            sub rbx , [kernelRows] ;moving the index of resultMat to be filled into rbx
            inc rbx
            imul rbx, [matRow]
            add rbx , [matCol]
            movss xmm1,[sum] ;moving sum into register xmm1
            movss [resultMat+rbx*4],xmm1 ;moving sum into the index rbx of resultMat

            xorps xmm1,xmm1 
            movss [sum],xmm1 ;setting sum to zero for the next iteration

            inc qword[matCol] ;incrementing matCol

            cmp [matCol],r14 
            jl traversing_matrix_row ;repeating inner loop

            cmp [matRow],r14
            jl changing_matrix_row ;repeating the outer loop

    endCalc: ;end of sub-routine

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

fillExtended:
    sub rsp, 8
    
    xor rbx,rbx
    mov [counter],rbx ;setting the counter to zero
    mov rax,[kernelRows]
    mov rbx ,2
    xor rdx,rdx
    idiv rbx ;calculating [kernelRows]/2
    mov r15,rax ;moving the quotient to r15
    mov r13,r15 ;moving the r15 to r13
    add r15,r15
    add r15,[matrixRows] ;setting r15 to be the number of rows of the extended matrix
    mov rax,[kernelRows]
    mov rbx ,2
    xor rdx,rdx
    idiv rbx ;calculating [kernelRows]/2
    mov r14,rax
    add r13,[matrixRows]
    dec r13 ;setting r13 to be the number of rows of the matrix plus [kernelRows]/2 minus 1
    imul r15,r15 ;setting r15 to be the number of elements of the extended matrix

    repeatLoop: ;iterating on the cells of the extended matrix
        cmp [counter],r15 ;checking the end of process
        jge end_ex

        mov rax,[counter] ;calculating [counter]/rbx where rbx is number of rows of extended matrix
        mov rbx, r13
        add rbx ,r14
        inc rbx
        xor rdx,rdx
        idiv rbx
        mov [exRow],rax ;moving the quotinet into [exRow]
        mov [exCol],rdx ;moving the remainder into [exCol]
        cmp [exCol],r14 ;we compute the position of this cell in the extended matrix
        jl left_part ;if [exCol]<r14 then its in the left part of matrix
        cmp [exCol],r13
        jg right_part ;if [exCol]>r13 then its in the right part of matrix
        jmp middle_part ;else its in the middle part 

        left_part:
        cmp [exRow],r14
        jl left_upper_part ;if [exRow]<r14 then its in the left upper part of matrix
        cmp [exRow],r13
        jg left_lower_part ;if [exRow]>r13 then its in the left lower part of matrix
        jmp left_middle_part ;else its in the left middle part

        left_upper_part:
            movss xmm1,[matrix] ;we move the element in the coordinate (0,0) of matrix into extended matrix
            mov rbx,[counter]
            movss [edgeExtended+rbx*4],xmm1
            jmp end_of_loop ;jump to the end of loop

        left_lower_part:
            mov rbx,[matrixRows] ;we move the element in the coordinate (matrixRows-1,0) of matrix into extended matrix
            imul rbx,rbx
            sub rbx,[matrixRows]
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeExtended+rbx*4]
            mov rbx,[counter]
            movss [edgeExtended+rbx*4],xmm1
            jmp end_of_loop ;jump to the end of loop
        
        left_middle_part:
            mov rax,[kernelRows] ;we move the element in the coordinate (exRow-[kernelRows]/2,0) of matrix into extended matrix
            mov rbx,2
            xor rdx,rdx
            idiv rbx ;calculating [kernelRows]/2
            mov rbx,[exRow]
            sub rbx,rax
            imul rbx,[matrixRows]
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeExtended+rbx*4]
            mov rbx,[counter]
            movss [edgeExtended+rbx*4],xmm1
            jmp end_of_loop ;jump to the end of loop

        right_part:
        cmp [exRow],r14
        jl right_upper_part ;if [exRow]<r14 then its in the right upper part of matrix
        cmp [exRow],r13
        jg right_lower_part ;if [exRow]>r13 then its in the right lower part of matrix
        jmp right_middle_part ;else its in the right middle part

        right_upper_part:
            mov rbx,[matrixRows] ;we move the element in the coordinate (0,matrixRows-1) of matrix into extended matrix
            dec rbx
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeExtended+rbx*4]
            mov rbx,[counter]
            movss [edgeExtended+rbx*4],xmm1
            jmp end_of_loop ;jump to the end of loop
        
        right_lower_part:
            mov rbx,[matrixRows] ;we move the element in the coordinate (matrixRows-1,matrixRows-1) of matrix into extended matrix
            imul rbx,rbx
            dec rbx
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeExtended+rbx*4]
            mov rbx,[counter]
            movss [edgeExtended+rbx*4],xmm1
            jmp end_of_loop ;jump to the end of loop
        
        right_middle_part:
            mov rax,[kernelRows] ;we move the element in the coordinate (exRow-[kernelRows]/2,matrixRows-1) of matrix into extended matrix
            mov rbx,2
            xor rdx,rdx
            idiv rbx ;calculating [kernelRows]/2
            mov rbx,[exRow]
            sub rbx,rax
            imul rbx,[matrixRows]
            add rbx,[matrixRows]
            dec rbx
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeExtended+rbx*4]
            mov rbx,[counter]
            movss [edgeExtended+rbx*4],xmm1
            jmp end_of_loop ;jump to the end of loop

        middle_part:
        cmp [exRow],r14
        jl middle_upper_part ;if [exRow]<r14 then its in the middle upper part of matrix
        cmp [exRow],r13
        jg middle_lower_part ;if [exRow]>r13 then its in the middle lower part of matrix
        jmp middle_middle_part ;else its in the middle middle part

        middle_upper_part:
            mov rax,[kernelRows] ;we move the element in the coordinate (0,exCol-[kernelRows]/2) of matrix into extended matrix
            mov rbx ,2
            xor rdx,rdx
            idiv rbx ;calculating [kernelRows]/2
            mov rbx,[exCol]
            sub rbx,rax
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeExtended+rbx*4]
            mov rbx,[counter]
            movss [edgeExtended+rbx*4],xmm1
            jmp end_of_loop ;jump to the end of loop
            
        middle_lower_part: ;we move the element in the coordinate (matrixRows-1,exCol-[kernelRows]/2) of matrix into extended matrix
            mov rax,[kernelRows]
            mov rbx , 2
            xor rdx,rdx
            idiv rbx ;calculating [kernelRows]/2
            mov rbx,[exCol]
            sub rbx,rax
            mov rax,[matrixRows]
            imul rax,rax
            sub rax,[matrixRows]
            add rbx,rax
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeExtended+rbx*4]
            mov rbx,[counter]
            movss [edgeExtended+rbx*4],xmm1
            jmp end_of_loop ;jump to the end of loop
            
        middle_middle_part: ;we move the element in the coordinate (exRow-[kernelRows]/2,exCol-[kernelRows]/2) of matrix into extended matrix
            mov rax,[kernelRows]
            mov rbx ,2
            xor rdx,rdx
            idiv rbx ;calculating [kernelRows]/2
            mov rbx,[exCol]
            sub rbx,rax
            mov r12,rbx ;moving exCol-[kernelRows]/2 into r12
            mov rax,[kernelRows]
            mov rbx , 2
            xor rdx,rdx
            idiv rbx ;calculating [kernelRows]/2
            mov rbx,[exRow]
            sub rbx,rax
            imul rbx,[matrixRows]
            add rbx,r12 ;moving matrixRows*(exRow-[kernelRows]/2)+(exCol-[kernelRows]/2) into r12
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeExtended+rbx*4]
            mov rbx,[counter]
            movss [edgeExtended+rbx*4],xmm1
            jmp end_of_loop ;jump to the end of loop
    
        end_of_loop: ;incrementing counter and repeating the loop
            inc qword[counter]
            jmp repeatLoop

    end_ex: ;making exRow and exCol zero for further usage
    xor r12,r12
    mov [exRow],r12
    mov [exCol],r12
    
    add rsp, 8
    ret

extendedConvolution:
    sub rsp, 8

    call fillExtended ;creating the extended matrix


    mov r14,[matrixRows] ; r14 is used for the size of convolution output
    mov [conv_size],r14

    jmp traversing_matrix_row1 ;initiating the loop (the process of calculating covolution)

    changing_matrix_row1: ;outer loop
        inc qword[exRow] ;incrementing the row
        sub [exCol],r14 ;making exCol zero

        cmp [exRow],r14 ;checking the criteria to end the outer loop
        jge endCalc1 ;ending the loop

        traversing_matrix_row1: ;inner loop

            mov r12,[matrixRows] ;calculating the size of extended matrix
            mov rax,[kernelRows]
            mov rbx,2
            xor rdx,rdx
            idiv rbx
            add rax,rax
            add r12,rax
            mov [calc_rows],r12 ;setting calc_rows to exRows to be used in fillingSubMatrix
            mov r12,[exRow]
            mov [calc_row],r12 ;setting calc_row to exRow to be used in fillingSubMatrix
            mov r12,[exCol]
            mov [calc_col],r12 ;setting calc_col to exCol to be used in fillingSubMatrix
            mov r12,edgeExtended ;moving the address of edgeExtended to r12

            call fillingSubMatrix ;filling the sub-matrix (moving the elements of matrix to submatrix to be multiplied by kernel)

            call matrixMulForKernel ;multiplying kernel and sub-matrix

            mov rbx,[exRow] ;moving the index of resultMat to be filled, into rbx
            imul rbx,r14
            add rbx,[exCol]
            imul rbx,4

            movss xmm1,[sum] ;moving sum into register xmm1
            movss [resultMat+rbx],xmm1 ;moving sum into the index rbx of resultMat

            xorps xmm1,xmm1
            movss [sum],xmm1 ;setting sum to zero for the next iteration

            inc qword[exCol] ;incrementing exCol
            
            

            cmp [exCol],r14
            jl traversing_matrix_row1 ;repeating inner loop

            cmp [exRow],r14
            jl changing_matrix_row1 ;repeating the outer loop

    endCalc1: ;end of sub-routine   

    add rsp, 8
    ret

fillMirror:
    sub rsp, 8
    
    xor rbx,rbx
    mov [counter],rbx ;setting the counter to zero
    mov rax,[kernelRows]
    mov rbx ,2
    xor rdx,rdx
    idiv rbx ;calculating [kernelRows]/2
    mov r15,rax ;moving the quotient to r15
    mov r13,r15 ;moving the r15 to r13
    mov r12,r13 ;r12 equals [kernelRows]/2
    add r15,r15
    add r15,[matrixRows] ;setting r15 to be the number of rows of the mirrored matrix
    mov rax,[kernelRows]
    mov rbx ,2
    xor rdx,rdx
    idiv rbx ;calculating [kernelRows]/2
    mov r14,rax
    add r13,[matrixRows]
    dec r13 ;setting r13 to be the number of rows of the matrix plus [kernelRows]/2 minus 1
    imul r15,r15 ;setting r15 to be the number of elements of the mirrored matrix

    repeatLoop1: ;iterating on the cells of the mirrored matrix
        cmp [counter],r15 ;checking the end of process
        jge end_mir

        mov rax,[counter] ;calculating [counter]/rbx where rbx is number of rows of mirrored matrix
        mov rbx, r13
        add rbx ,r14
        inc rbx
        xor rdx,rdx
        idiv rbx
        mov [mirRow],rax ;moving the quotinet into [mirRow]
        mov [mirCol],rdx ;moving the remainder into [mirCol]
        cmp [mirCol],r14 ;we compute the position of this cell in the mirrored matrix
        jl left_part1 ;if [mirCol]<r14 then its in the left part of matrix
        cmp [mirCol],r13
        jg right_part1 ;if [mirCol]>r13 then its in the right part of matrix
        jmp middle_part1 ;else its in the middle part 

        left_part1:
        cmp [mirRow],r14
        jl left_upper_part1 ;if [mirRow]<r14 then its in the left upper part of matrix
        cmp [mirRow],r13
        jg left_lower_part1 ;if [mirRow]>r13 then its in the left lower part of matrix
        jmp left_middle_part1 ;else its in the left middle part

        left_upper_part1:
            mov rbx,r12 ;we move the element in the coordinate (r12-[mirRow]-1,r12-[mirCol]-1) of matrix into mirrored matrix
            sub rbx,[mirRow]
            dec rbx
            imul rbx,[matrixRows]
            add rbx,r12
            sub rbx,[mirCol]
            dec rbx
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeMirrored+rbx*4]
            mov rbx,[counter]
            movss [edgeMirrored+rbx*4],xmm1
            jmp end_of_loop1 ;jump to the end of loop

        left_lower_part1:
            mov rbx,[matrixRows] ;we move the element in the coordinate (2*[matrixRows]+r12-[mirRow]-1,r12-[mirCol]-1) of matrix into mirrored matrix
            add rbx,rbx
            add rbx,r12
            sub rbx,[mirRow]
            dec rbx
            imul rbx,[matrixRows]
            add rbx,r12
            sub rbx,[mirCol]
            dec rbx
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeMirrored+rbx*4]
            mov rbx,[counter]
            movss [edgeMirrored+rbx*4],xmm1
            jmp end_of_loop1 ;jump to the end of loop
        
        left_middle_part1: ;we move the element in the coordinate (mirRow-[kernelRows]/2,r12-[mirCol]-1) of matrix into mirrored matrix
            mov rbx,[mirRow]
            sub rbx,r12
            imul rbx,[matrixRows]
            add rbx,r12
            sub rbx,[mirCol]
            dec rbx
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeMirrored+rbx*4]
            mov rbx,[counter]
            movss [edgeMirrored+rbx*4],xmm1
            jmp end_of_loop1 ;jump to the end of loop

        right_part1:
        cmp [mirRow],r14
        jl right_upper_part1 ;if [mirRow]<r14 then its in the right upper part of matrix
        cmp [mirRow],r13
        jg right_lower_part1 ;if [mirRow]>r13 then its in the right lower part of matrix
        jmp right_middle_part1 ;else its in the right middle part

        right_upper_part1:
            mov rbx,r12 ;we move the element in the coordinate (r12-[mirRow]-1,2*[matrixRows]+r12-[mirCol]-1) of matrix into mirrored matrix
            sub rbx,[mirRow]
            dec rbx
            imul rbx,[matrixRows]
            add rbx,[matrixRows]
            add rbx,[matrixRows]
            add rbx,r12
            sub rbx,[mirCol]
            dec rbx
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeMirrored+rbx*4]
            mov rbx,[counter]
            movss [edgeMirrored+rbx*4],xmm1
            jmp end_of_loop1  ;jump to the end of loop
        
        right_lower_part1: ;we move the element in the coordinate (2*[matrixRows]+r12-[mirRow]-1,2*[matrixRows]+r12-[mirCol]-1) of matrix into mirrored matrix
            mov rbx,[matrixRows]
            add rbx,rbx
            add rbx,r12
            sub rbx,[mirRow]
            dec rbx
            imul rbx,[matrixRows]
            add rbx,[matrixRows]
            add rbx,[matrixRows]
            add rbx,r12
            sub rbx,[mirCol]
            dec rbx
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeMirrored+rbx*4]
            mov rbx,[counter]
            movss [edgeMirrored+rbx*4],xmm1
            jmp end_of_loop1 ;jump to the end of loop
        
        right_middle_part1:
            mov rbx,[mirRow] ;we move the element in the coordinate (r12-[mirRow]-1,2*[matrixRows]+r12-[mirCol]-1) of matrix into mirrored matrix
            sub rbx,r12
            imul rbx,[matrixRows]
            add rbx,[matrixRows]
            add rbx,[matrixRows]
            add rbx,r12
            sub rbx,[mirCol]
            dec rbx
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeMirrored+rbx*4]
            mov rbx,[counter]
            movss [edgeMirrored+rbx*4],xmm1
            jmp end_of_loop1 ;jump to the end of loop

        middle_part1:
        cmp [mirRow],r14
        jl middle_upper_part1 ;if [mirRow]<r14 then its in the middle upper part of matrix
        cmp [mirRow],r13
        jg middle_lower_part1 ;if [mirRow]>r13 then its in the middle lower part of matrix
        jmp middle_middle_part1 ;else its in the middle middle part

        middle_upper_part1:
            mov rbx,r12 ;we move the element in the coordinate (r12-[mirRow]-1,mirCol-[kernelRows]/2) of matrix into mirrored matrix
            sub rbx,[mirRow]
            dec rbx
            imul rbx,[matrixRows]
            add rbx,[mirCol]
            sub rbx,r12
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeMirrored+rbx*4]
            mov rbx,[counter]
            movss [edgeMirrored+rbx*4],xmm1
            jmp end_of_loop1 ;jump to the end of loop
            
        middle_lower_part1:
            mov rbx,[matrixRows] ;we move the element in the coordinate (2*[matrixRows]+r12-[mirRow]-1,mirCol-[kernelRows]/2) of matrix into mirrored matrix
            add rbx,rbx
            add rbx,r12
            sub rbx,[mirRow]
            dec rbx
            imul rbx,[matrixRows]
            add rbx,[mirCol]
            sub rbx,r12
            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeMirrored+rbx*4]
            mov rbx,[counter]
            movss [edgeMirrored+rbx*4],xmm1
            jmp end_of_loop1
            
        middle_middle_part1: 
            mov rax,[kernelRows] ;we move the element in the coordinate (mirRow-[kernelRows]/2,mirCol-[kernelRows]/2) of matrix into mirrored matrix
            mov rbx ,2
            xor rdx,rdx
            idiv rbx
            mov rbx,[mirCol]
            sub rbx,rax

            push r12 ;pushing r12 onto stack to save its value

            mov r12,rbx
            mov rax,[kernelRows] ;we are computing the coordinate of this cell in the original matrix
            mov rbx , 2
            xor rdx,rdx
            idiv rbx
            mov rbx,[mirRow]
            sub rbx,rax
            imul rbx,[matrixRows] 
            add rbx,r12

            pop r12 ;restoring the previous value of r12 from the stack

            movss xmm1,[matrix+rbx*4] ;we use xmm1 to move [matrix+rbx*4] into [edgeMirrored+rbx*4]
            mov rbx,[counter]
            movss [edgeMirrored+rbx*4],xmm1
            jmp end_of_loop1 ;jump to the end of loop
    
        end_of_loop1: ;incrementing counter and repeating the loop
            inc qword[counter]
            jmp repeatLoop1

    end_mir: ;making mirRow and mirCol zero for further usage
    xor r12,r12
    mov [mirRow],r12
    mov [mirCol],r12
    
    add rsp, 8
    ret


mirroredConvolution:
    sub rsp, 8

    call fillMirror ;creating the mirrored matrix


    mov r14,[matrixRows] ; r14 is used for the size of convolution output
    mov [conv_size],r14

    jmp traversing_matrix_row2 ;initiating the loop (the process of calculating covolution)

    changing_matrix_row2: ;outer loop
        inc qword[mirRow] ;incrementing the row
        sub [mirCol],r14 ;making mirCol zero

        cmp [mirRow],r14 ;checking the criteria to end the outer loop
        jge endCalc2 ;ending the loop

        traversing_matrix_row2: ;inner loop

            mov r12,[matrixRows] ;calculating the size of mirrored matrix
            mov rax,[kernelRows]
            mov rbx,2
            xor rdx,rdx
            idiv rbx
            add rax,rax
            add r12,rax
            mov [calc_rows],r12 ;setting calc_rows to mirRows to be used in fillingSubMatrix
            mov r12,[mirRow]
            mov [calc_row],r12 ;setting calc_row to mirRow to be used in fillingSubMatrix
            mov r12,[mirCol]
            mov [calc_col],r12 ;setting calc_col to mirCol to be used in fillingSubMatrix
            mov r12,edgeMirrored ;moving the address of edgeMirrored to r12

            call fillingSubMatrix ;filling the sub-matrix (moving the elements of matrix to submatrix to be multiplied by kernel)

            call matrixMulForKernel ;multiplying kernel and sub-matrix

            mov rbx,[mirRow] ;moving the index of resultMat to be filled, into rbx
            imul rbx,r14
            add rbx,[mirCol]
            imul rbx,4

            movss xmm1,[sum] ;moving sum into register xmm1
            movss [resultMat+rbx],xmm1 ;moving sum into the index rbx of resultMat

            xorps xmm1,xmm1
            movss [sum],xmm1 ;setting sum to zero for the next iteration

            inc qword[mirCol] ;incrementing mirCol
            
            

            cmp [mirCol],r14
            jl traversing_matrix_row2 ;repeating inner loop

            cmp [mirRow],r14
            jl changing_matrix_row2 ;repeating the outer loop

    endCalc2: ;end of sub-routine

    add rsp, 8
    ret


filLZero:
    sub rsp, 8
    
    xor rbx,rbx
    mov [counter],rbx ;setting the counter to zero
    mov rax,[kernelRows]
    mov rbx ,2
    xor rdx,rdx
    idiv rbx ;calculating [kernelRows]/2
    mov r15,rax ;moving the quotient to r15
    mov r13,r15 ;moving the r15 to r13
    mov r12,r13 ;r12 equals [kernelRows]/2
    add r15,r15
    add r15,[matrixRows] ;setting r15 to be the number of rows of the zero-edge matrix
    mov rax,[kernelRows]
    mov rbx ,2
    xor rdx,rdx
    idiv rbx ;calculating [kernelRows]/2
    mov r14,rax
    add r13,[matrixRows]
    dec r13 ;setting r13 to be the number of rows of the matrix plus [kernelRows]/2 minus 1
    imul r15,r15 ;setting r15 to be the number of elements of the zero-edge matrix

    repeatLoop2: ;iterating on the cells of the zero-edge matrix
        cmp [counter],r15 ;checking the end of process
        jge end_zero

        mov rax,[counter] ;calculating [counter]/rbx where rbx is number of rows of zero-edge matrix
        mov rbx, r13
        add rbx ,r14
        inc rbx
        xor rdx,rdx
        idiv rbx
        mov [zeroRow],rax ;moving the quotinet into [zeroRow]
        mov [zeroCol],rdx ;moving the remainder into [zeroCol]
        cmp [zeroCol],r14 ;we compute the position of this cell in the zero-edge matrix
        jl edge  ;if [zeroCol]<r14 then its in the edge part of matrix
        cmp [zeroCol],r13
        jg edge ;if [zeroCol]>r13 then its in the edge part of matrix
        cmp [zeroRow],r14
        jl edge ;if [zeroRow]<r14 then its in the edge part of matrix
        cmp [zeroRow],r13
        jg edge ;if [zeroRow]>r13 then its in the edge part of matrix
        jmp middle_middle_part2 ;else it's in the middle part of the matrix

        edge:
            mov rbx,[counter] ;moving the index of current cell into rbx
            xorps xmm1,xmm1
            movss [edgeZero+rbx*4],xmm1 ;setting the edge part to zero
            jmp end_of_loop2 ;jump to the end of loop

        middle_middle_part2:
            mov rax,[kernelRows]  ;we move the element in the coordinate (zeroRow-[kernelRows]/2,zeroCol-[kernelRows]/2) of matrix into mirrored matrix
            mov rbx ,2
            xor rdx,rdx
            idiv rbx
            mov rbx,[zeroCol]
            sub rbx,rax

            push r12 ;pushing r12 onto stack to save its value

            mov r12,rbx
            mov rax,[kernelRows] ;we are computing the coordinate of this cell in the original matrix
            mov rbx , 2
            xor rdx,rdx
            idiv rbx
            mov rbx,[zeroRow]
            sub rbx,rax
            imul rbx,[matrixRows]
            add rbx,r12

            pop r12 ;restoring the previous value of r12 from the stack

            movss xmm1,[matrix+rbx*4]  ;we use xmm1 to move [matrix+rbx*4] into [edgeZero+rbx*4]
            mov rbx,[counter]
            movss [edgeZero+rbx*4],xmm1
            jmp end_of_loop2 ;jump to the end of loop
    
        end_of_loop2: ;incrementing counter and repeating the loop
            inc qword[counter]
            jmp repeatLoop2

    end_zero: ;making zeroRow and zeroCol zero for further usage
    xor r12,r12
    mov [zeroRow],r12
    mov [zeroCol],r12
    
    add rsp, 8
    ret


zeroConvolution:
    sub rsp, 8

    call filLZero ;creating the mirrored matrix

    mov r14,[matrixRows] ; r14 is used for the size of convolution output
    mov [conv_size],r14

    jmp traversing_matrix_row3 ;initiating the loop (the process of calculating covolution)

    changing_matrix_row3: ;outer loop
        inc qword[zeroRow] ;incrementing the row
        sub [zeroCol],r14 ;making zeroCol zero

        cmp [zeroRow],r14 ;checking the criteria to end the outer loop
        jge endCalc3 ;ending the loop

        traversing_matrix_row3: ;inner loop

            mov r12,[matrixRows] ;calculating the size of zero-edge matrix
            mov rax,[kernelRows]
            mov rbx,2
            xor rdx,rdx
            idiv rbx
            add rax,rax
            add r12,rax
            mov [calc_rows],r12 ;setting calc_rows to zeroRows to be used in fillingSubMatrix
            mov r12,[zeroRow]
            mov [calc_row],r12 ;setting calc_row to zeroRow to be used in fillingSubMatrix
            mov r12,[zeroCol]
            mov [calc_col],r12 ;setting calc_col to zeroCol to be used in fillingSubMatrix
            mov r12,edgeZero ;moving the address of edgeZero to r12

            call fillingSubMatrix ;filling the sub-matrix (moving the elements of matrix to submatrix to be multiplied by kernel)

            call matrixMulForKernel ;multiplying kernel and sub-matrix

            mov rbx,[zeroRow] ;moving the index of resultMat to be filled, into rbx
            imul rbx,r14
            add rbx,[zeroCol]
            imul rbx,4

            movss xmm1,[sum] ;moving sum into register xmm1
            movss [resultMat+rbx],xmm1 ;moving sum into the index rbx of resultMat

            xorps xmm1,xmm1
            movss [sum],xmm1 ;setting sum to zero for the next iteration

            inc qword[zeroCol] ;incrementing zeroCol
            
            

            cmp [zeroCol],r14
            jl traversing_matrix_row3 ;repeating inner loop

            cmp [zeroRow],r14
            jl changing_matrix_row3 ;repeating the outer loop

    endCalc3: ;end of sub-routine

    add rsp, 8
    ret

matrixOrdinaryMul:
    sub rsp, 8

    mov r12,mulResult ;r12 holds the address of mulResult
    mov r14,[matNuRows] ;r14 holds the number of matrix rows

    jmp traversing_matrix_row4 ;initiating the loop (the process of calculating matrix multiplication)

    changing_matrix_row4: ;outer loop
        inc qword[matR] ;incrementing the row
        sub [matC],r14 ;making matC zero

        cmp [matR],r14 ;checking the criteria to end the outer loop
        jge endCalc4 ;ending the loop

        traversing_matrix_row4: ;inner loop

            xor r13,r13 ;using r13 to calculate the dot product of matR-th row of mat1 and matC-th column of mat2 

            sum_loop: ;calculating the value of cell (matR,matC) in the result matrix

                mov rbx , [matR] ;setting rbx to be the index of cell (matR,r13) in mat1
                imul rbx , [matNuRows]
                add rbx, r13
                movss xmm1, [mat1+rbx*4] ;moving the value of the cell into xmm1
                
                mov rbx , r13 ;setting rbx to be the index of cell (r13,matC) in mat2
                imul rbx, [matNuRows]
                add rbx , [matC]
                movss xmm2 , [mat2+rbx*4] ;moving the value of the cell into xmm2
                
                mulss xmm1 , xmm2 ;multiplying these two values
                movss xmm2 , [sum] 
                addss xmm1, xmm2 ;and adding the result to sum
                movss [sum] , xmm1 ;moving the result back to sum

                inc r13 ;incrementing r13
                cmp r13,[matNuRows] ;if r13>=[matNuRows] we end the calculation of (matR,matC) in the result matrix
                jl sum_loop ;repeating the loop until (matR,matC) is calculated

                mov rbx, [matR] ;getting the index of (matR,matC) in the result matrix
                imul rbx , [matNuRows]
                add rbx , [matC]
                movss xmm1, [sum]
                movss [mulResult+rbx*4],xmm1 ;moving sum to cell (matR,matC) in the result matrix
                xorps xmm1,xmm1
                movss [sum],xmm1 ;setting sum to zero for upcoming iterations

                
             
                inc qword[matC] ;incrementing matC

                cmp [matC],r14
                jl traversing_matrix_row4 ;repeating inner loop

                cmp [matR],r14
                jl changing_matrix_row4 ;repeating the outer loop

    endCalc4: ;end of sub-routine

    add rsp, 8
    ret


matrixSIMDMul:
    sub rsp, 8

    mov r12 , mat2 ;r12 holds the address of mat2
    mov r13 , [matNuRows] ;r13 holds the number of mat2 rows
    call transpose ;creating the transpose of mat2 and moving it into transposeMat

    mov r12,mat1 ;r12 holds the address of mat1
    mov r13,[matNuRows] ;r13 holds the address of [matNuRows]
    mov r14,zero_Aligned_Mat ;r14 holds the address of zero_Aligned_Mat
    call zeroAlignedMat ;extend edges of mat1 to be a matrix such that its number of rows is a multiple of 4 

    mov r12,transposeMat ;r12 holds the address of mat2 transpose
    mov r13,[matNuRows] ;r13 holds the address of [matNuRows]
    mov r14,zero_Aligned_Mat_For_T ;r14 holds the address of zero_Aligned_Mat_For_T
    call zeroAlignedMat ;extend edges of mat2 transpose to be a matrix such that its number of rows is a multiple of 4 

    mov r12,mulResult ;r12 holds the address of mulResult
    mov r14,[matNuRows] ;r14 holds the number of matrix rows
    
    rdtsc; save the current clock of computer to save it in rax and rdx
    mov [start_time_h],rdx ;move the high 32-bit of clock which is in rdx into start_time_h
    mov [start_time_l],rax ;move the low 32-bit of clock which is in rax into start_time_l


    jmp traversing_matrix_row5 ;initiating the loop (the process of calculating matrix multiplication)

    changing_matrix_row5: ;outer loop
        inc qword[matR] ;incrementing the row
        sub [matC],r14 ;making matC zero

        cmp [matR],r14 ;checking the criteria to end the outer loop
        jge endCalc5 ;ending the loop

        traversing_matrix_row5: ;inner loop

            xor r13,r13 ;using r13 to calculate the dot product of matR-th row of zero_Aligned_Mat and matC-th column of zero_Aligned_Mat_For_T 

            sum_loop2: ;calculating the value of cell (matR,matC) in the result matrix

                mov rbx , [matR] ;setting rbx to be the index of cell (matR,r13) in zero_Aligned_Mat
                imul rbx , [zeroAlignedSize]
                add rbx, r13
                movups xmm1,[zero_Aligned_Mat + rbx*4] ;moving the values of four consecutive cells into xmm1


                mov rbx , [matC] ;setting rbx to be the index of cell (r13,matC) in zero_ALigned_Mat_For_T transpose
                imul rbx , [zeroAlignedSize]
                add rbx, r13
                movups xmm2,[zero_Aligned_Mat_For_T + rbx*4]  ;moving the values of four consecutive cells into xmm1

                dpps xmm1,xmm2,0xF1 ;calculating the dot product of xmm1 and xmm2
                addss xmm1,[sum] ;and adding the result to sum
                movss [sum],xmm1 ;moving the result back to sum

                add r13,4 ;moving to the next four cells

                cmp r13,[matNuRows] ;if r13>=[matNuRows] we end the calculation of (matR,matC) in the result matrix
                jl sum_loop2 ;repeating the loop until (matR,matC) is calculated

                mov rbx, [matR] ;getting the index of (matR,matC) in the result matrix
                imul rbx , [matNuRows]
                add rbx , [matC]
                movss xmm1, [sum]
                movss [mulResult+rbx*4],xmm1 ;moving sum to cell (matR,matC) in the result matrix
                xorps xmm1,xmm1
                movss [sum],xmm1 ;setting sum to zero for upcoming iterations
             
                inc qword[matC] ;incrementing matC

                cmp [matC],r14
                jl traversing_matrix_row5 ;repeating inner loop

                cmp [matR],r14
                jl changing_matrix_row5 ;repeating the outer loop


    endCalc5: ;end of sub-routine
    
    rdtsc; save the current clock of computer to save it in rax and rdx
    mov [end_time_h],rdx ;move the high 32-bit of clock which is in rdx into end_time_h
    mov [end_time_l],rax ;move the low 32-bit of clock which is in rax into end_time_l
    mov rbx , [end_time_h] ; rbx = end_time_h
    sub rbx , [start_time_h] ; rbx = end_time_h - start_time_h
    shl rbx , 32 ; rbx = (end_time_h - start_time_h)*(2^32)
    add rbx , [end_time_l] ; rbx = (end_time_h - start_time_h)*(2^32) + end_time_l
    sub rbx , [start_time_l] ; rbx = (end_time_h - start_time_h)*(2^32) + end_time_l - start_time_l
    mov rax , rbx ; we are trying to divide rbx by the clock rate of cpu
    imul rax , 10 ; rax = 10*rbx
    mov rbx , 30
    xor rdx,rdx
    idiv rbx ; rax = rbx/3 (my cpu is 3GHz)
    mov [time],rax ;execution time in nanoseconds

    add rsp, 8
    ret

transpose: ;mov mat address into r12 , mov mat number of rows into r13
    sub rsp, 8

    xor rbx , rbx
    mov [counter],rbx ;set counter to zero
    mov r14,r13
    imul r14 , r14 ;r14 holds the number of elements of mat

    loop_for_transpose:
        mov rax , [counter] ;calculating the (row,column) using index
        xor rdx,rdx
        idiv r13
        mov [calc_row] ,rax ;the quotient is the row
        mov [calc_col] ,rdx ;the remainder is the column
        mov rbx , [calc_row]
        imul rbx , r13
        add rbx , [calc_col] ;rbx now points to the cell with coordinate (row,column)
        movss xmm1 , [r12+rbx*4] ;moving the value into xmm1
        mov rbx , [calc_col]
        imul rbx , r13
        add rbx , [calc_row] ;rbx now points to the cell with coordinate (row,column)
        movss [transposeMat+rbx*4],xmm1 ;moving the value of (row,column) into (column,row)

        inc qword[counter] ;incrementing the counter
        cmp [counter],r14
        jl loop_for_transpose ;repeating the loop

    xor rbx , rbx
    mov [counter],rbx ;setting counter back to zero
    
    add rsp, 8
    ret    


zeroAlignedMat: ;mov mat address into r12 , mov mat number of rows into r13, r14 for destination
    sub rsp, 8
    
    mov r15,4
    mov rax , r13
    mov rbx ,4
    xor rdx,rdx
    idiv rbx
    sub r15,rdx ;r15 holds the number of rows we need to add to make them a multiple of 4

    xor rbx,rbx
    mov [counter],rbx ;setting counter to zero
    mov [calc_col],rbx ;setting calc_col to zero
    mov [calc_row],rbx ;setting calc_row to zero
    mov rbx,r15
    add rbx,r13
    mov [calc_rows],rbx ;moving the updated number of rows into calc_rows
    mov [zeroAlignedSize],rbx ;zeroAlignedSize holds the number of rows in the new matrix

    jmp aligning_loop ;starting the loop
    updating:
        inc qword[calc_row] ;incrementing the row
        xor rbx, rbx
        mov [calc_col],rbx ;making calc_col zero

        cmp [calc_row],r13 ;checking the criteria to end the outer loop
        jge endCalc6 ;ending the loop

        aligning_loop: ;inner loop
            mov rbx , [calc_row]
            imul rbx , r13
            add rbx , [calc_col]
            imul rbx,4
            movss xmm1,[r12+rbx] ;moving the (calc_row,calc_col) cell of mat into xmm1

            mov rbx , [calc_rows]
            imul rbx , [calc_row]
            add rbx , [calc_col]
            imul rbx ,4
            movss [r14+rbx],xmm1  ;moving xmm1 into the (calc_row,calc_col) cell of zero-aligned-matrix

            inc qword[calc_col] ;incrementing the column
            cmp [calc_col],r13 ;check to see if we have reached the end of column in the original mat
            jl aligning_loop ;repeating this process

            mov rbx ,rbx
            mov [counter],rbx ;setting counter to zero
            edges: ;filling the remaining cols with zero (we do this r15 times)
            cmp [counter],r15 ;if the counter is less than r15 then mov to next col else go to next row
            jge updating ;going to the next row
            mov rbx , [calc_rows]
            imul rbx , [calc_row]
            add rbx , [calc_col]
            xorps xmm1,xmm1
            movss [r14+rbx*4],xmm1 ;moving zero to this cell
            inc qword[calc_col] ;incrementing col
            inc qword[counter] ;incrementing counter
            jmp edges ;continue filling the edges

    endCalc6: ;end of sub-routine
    xor rbx,rbx
    mov [counter],rbx ;setting counter to zero for further usage
    mov [calc_col],rbx ;setting calc_col to zero for further usage
    mov [calc_row],rbx ;setting calc_row to zero for further usage
    mov [calc_rows],rbx ;setting calc_rows to zero for further usage

    add rsp, 8
    ret


fillMatrixM:
    sub rsp, 8

    mov r12,[matrixRows] ;setting r12 equal to n-k+1=t
    sub r12,[kernelRows]
    inc r12
    xor r13,r13 ;r13 equals zero (the number of zeros on the left side of K_i)
    mov r14,r12
    dec r14 ;r14 equals t-1 (the number of zeros on the right side of K_i)
    imul r12,r12 ;r12 equals (n-k+1)^2 = t^2
    mov [tSquare], r12 ;moving r12 into [tSquare]
    xor r15,r15 ;using r15 as counter

    xor rbx,rbx
    mov [calc_row],rbx
    mov [counter],rbx ;using [counter] to save the number of blocks K-hat we have used so far
    jmp adding_prev_zero ;initiating the loop

    repeating_t_times:
        inc qword[counter] ;incrementing counter
        xor rbx,rbx
        mov [Mrow],rbx ;setting Mrow and Mcol to zero
        mov [Mcol],rbx
        mov rbx,[matrixRows] ;mov t to rbx
        sub rbx,[kernelRows]
        inc rbx
        xor r13,r13 ;setting r13 to zero
        mov r14,rbx 
        dec r14 ;setting r14 to t-1
        cmp [counter],rbx ;if counter>=rbx then matrixM is constructed
        jge endCalc7
        jmp adding_prev_zero ;starting the inner loop

    changing_matrix_row6: ;outer loop
        inc qword[Mrow] ;incrementing the row
        xor rbx,rbx
        mov [Mcol],rbx ;making Mcol zero

        mov rbx,[matrixRows] ;rbx equals n-k+1
        sub rbx,[kernelRows]
        inc rbx
        cmp [Mrow],rbx ;checking the criteria to end the outer loop
        jge repeating_t_times ;ending the loop

            adding_prev_zero: ;adding zeros before the K-hats
                mov rbx,[counter] ;setting rbx equal to n*counter
                imul rbx,[matrixRows]
                cmp r15,rbx ;if r15>=rbx then end adding zeros other wise continue adding zero to matrixM
                jge end_adding_prev_zero

                mov rbx,[matrixRows] ;mov t to rbx
                sub rbx,[kernelRows]
                inc rbx
                imul rbx,[counter] ;setting rbx = [counter]*(t)
                add rbx,[Mrow] ;setting rbx to counter*t+Mrow
                imul rbx, [matrixSize]
                add rbx,[Mcol] ;setting rbx to be the index of cell (counter*t+Mrow,Mcol) in matrixM
                xorps xmm1,xmm1
                movss [matrixM+rbx*4],xmm1 ;moving the value into matrixM
                
                inc qword[Mcol] ;incrementing the column
                inc r15 ;incrementing r15

                jmp adding_prev_zero ;continuing loop

            end_adding_prev_zero:
                xor r15,r15 ;setting counter to zero

            traversing_kernel: ;creating K-hat

                mov rbx,[kernelRows] ;comparing calc_row with kernelRows
                cmp [calc_row],rbx ;if calc_row>=rbx then K-hat has been created
                jge end_traversing_kernel

                adding_zero: ;adding r13 zeros before K-i
                    
                    cmp r15,r13 ;doing this part r13 times
                    jge end_adding_zero

                    mov rbx,[matrixRows] ;mov t to rbx
                    sub rbx,[kernelRows]
                    inc rbx
                    imul rbx,[counter] ;setting rbx = [counter]*(t)
                    add rbx,[Mrow] ;setting rbx to counter*t+Mrow
                    imul rbx, [matrixSize]
                    add rbx,[Mcol] ;setting rbx to be the index of cell (counter*t+Mrow,Mcol) in matrixM
                    xorps xmm1,xmm1
                    movss [matrixM+rbx*4],xmm1 ;moving the value into matrixM

                    inc qword[Mcol] ;incrementing the column
                    inc r15 ;incrementing r15

                    jmp adding_zero ;continuing loop

                end_adding_zero:
                    xor r15,r15 ;setting counter to zero

                add_kernel:
                
                    cmp r15,[kernelRows] ;doing this part kernelRows times
                    jge end_adding_kernel

                    mov rbx,[calc_row]
                    imul rbx,[kernelRows] ;going to start of the calc_row-th row of kernel
                    add rbx , r15 ;getting r15-th element of the calc_row-th row of kernel
                    movss xmm1,[kernel+rbx*4] ;moving that element into xmm1
                    mov rbx,[matrixRows] ;mov t to rbx
                    sub rbx,[kernelRows]
                    inc rbx
                    imul rbx,[counter] ;setting rbx = [counter]*(t)
                    add rbx,[Mrow] ;setting rbx to counter*t+Mrow
                    imul rbx, [matrixSize]
                    add rbx,[Mcol] ;setting rbx to be the index of cell (counter*t+Mrow,Mcol) in matrixM
                    movss [matrixM+rbx*4],xmm1 ;moving the value into matrixM

                    inc qword[Mcol] ;incrementing the column
                    inc r15 ;incrementing r15

                    jmp add_kernel ;continuing loop

                end_adding_kernel:
                    xor r15,r15 ;setting counter to zero
                
                adding_zero_again:
                
                    cmp r15,r14 ;doing this part r14 times
                    jge end_adding_zero_again

                    mov rbx,[matrixRows] ;mov t to rbx
                    sub rbx,[kernelRows]
                    inc rbx
                    imul rbx,[counter] ;setting rbx = [counter]*(t)
                    add rbx,[Mrow] ;setting rbx to counter*t+Mrow
                    imul rbx, [matrixSize]
                    add rbx,[Mcol] ;setting rbx to be the index of cell (counter*t+Mrow,Mcol) in matrixM
                    xorps xmm1,xmm1
                    movss [matrixM+rbx*4],xmm1 ;moving the value into matrixM

                    inc qword[Mcol] ;incrementing the column
                    inc r15 ;incrementing r15
                    
                    jmp adding_zero_again ;continuing loop

                end_adding_zero_again:
                    xor r15,r15 ;setting counter to zero

                inc qword[calc_row] ;incrementing kernel row
                jmp traversing_kernel ;continue traversing through the next row of kernel
                
            end_traversing_kernel:
                inc r13 ;incrementing r13
                dec r14 ;decrementing r14
                xor rbx,rbx
                mov [calc_row],rbx ;setting calc_row to zero
                xor r15,r15 ;setting counter to zero

            adding_post_zero:
                mov rbx , [matrixRows] ;setting rbx equal to n
                sub rbx,[kernelRows] ;setting rbx equal to n-k=t
                sub rbx,[counter] ;setting rbx to t-counter
                imul rbx,[matrixRows] ;setting rbx to n*(t-1-counter)
                cmp r15,rbx ;if r15>=rbx then stop inserting zeros 
                jge end_adding_post_zero

                mov rbx,[matrixRows] ;mov t to rbx
                sub rbx,[kernelRows]
                inc rbx
                imul rbx,[counter] ;setting rbx = [counter]*(t)
                add rbx,[Mrow] ;setting rbx to counter*t+Mrow
                imul rbx, [matrixSize]
                add rbx,[Mcol] ;setting rbx to be the index of cell (counter*t+Mrow,Mcol) in matrixM
                xorps xmm1,xmm1
                movss [matrixM+rbx*4],xmm1 ;moving the value into matrixM

                
                inc qword[Mcol] ;incrementing the column
                inc r15 ;incrementing r15

                jmp adding_post_zero ;continuing loop

            end_adding_post_zero:
                xor r15,r15 ;setting counter to zero

            jmp changing_matrix_row6 ;going to next next row of block matrix

    endCalc7: ;end of sub-routine

    xor rbx,rbx
    mov [calc_row],rbx ;setting calc_row and calc_col zero for upcoming actions
    mov [counter],rbx

    add rsp,8
    ret

convUsingMatMul:
    sub rsp, 8

    mov r12,[matrixSize] ;r12 is n^2
    xor r15,r15 ;r15 is our counter
    mov [calc_col],r15
    mov [calc_row],r15
    mov r13,[tSquare] ;r13 is t^2
    xor r14,r14 ;using r14 as a counter to traverse rows of M
    xorps xmm1,xmm1
    movss [sum],xmm1 ;setting sum to zero for upcoming iterations

    jmp sum_loop3 ;initiating the loop (the process of calculating matrix multiplication)

    changing_matrix_row8: ;outer loop
        inc qword[calc_row] ;incrementing the row
        xor rbx,rbx
        mov [calc_col],rbx ;setting col to zero

        cmp [calc_row],r13 ;checking the criteria to end the outer loop
        jge endCalc8 ;ending the loop

            sum_loop3:

                mov rbx , [calc_row] ;setting rbx to be the index of cell (calc_row,calc_col) in matrixM
                imul rbx , r12
                add rbx, [calc_col]
                movss xmm1, [matrixM+rbx*4] ;moving the value of the cell into xmm1
                
                mov rbx , [calc_col] ;setting rbx to be the index of calc_col-th cell in matrix
                movss xmm2 , [matrix+rbx*4] ;moving the value of the cell into xmm2
                
                mulss xmm1 , xmm2 ;multiplying these two values
                movss xmm2 , [sum] 
                addss xmm1, xmm2 ;and adding the result to sum
                movss [sum] , xmm1 ;moving the result back to sum

                inc qword[calc_col]
                inc r14 ;incrementing r14
                cmp r14,[matrixSize] ;if r14>=[matrixSize] we end the calculation of calc_row-th cell of the result matrix
                jl sum_loop3 ;repeating the loop until the calc_row-th cell of the result matrix is calculated

                mov rbx, [calc_row] ;getting the index of the calc_row-th cell of the result matrix
                movss xmm1, [sum]
                movss [convRes+rbx*4],xmm1 ;moving sum to the calc_row-th cell of the result matrix
                xorps xmm1,xmm1
                movss [sum],xmm1 ;setting sum to zero for upcoming iterations
                xor r14,r14 ;using r14 to calculate the traverse through the rows of matrixM

                jmp changing_matrix_row8

    endCalc8: ;end of sub-routine

    xor r15,r15 ;making variables zero for upcoming actions
    mov [calc_col],r15
    mov [calc_row],r15
    
    add rsp, 8
    ret