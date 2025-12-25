.globl main
main:
pushq %rbp
movq %rsp, %rbp
movq $0, %rax
movq $0, %rax
movq $1, %rax
cmpq $0, %rax
je _clause1
movq $1, %rax
jmp _end1
_clause1:
movq $0, %rax
cmpq $0, %rax
jne _clause2
movq $0, %rax
jmp _end2
_clause2:
movq $2, %rax
cmpq $0, %rax
movq $1, %rax
jne _end2
movq $0, %rax
_end2:
cmpq $0, %rax
movq $0, %rax
je _end1
movq $1, %rax
_end1:
movq %rbp, %rsp
popq %rbp
retq
