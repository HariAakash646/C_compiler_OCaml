.globl main
main:
pushq %rbp
movq %rsp, %rbp
movq $0, %rax
movq $0, %rax
movq $0, %rax
cmpq $0, %rax
je _clause1
movq $3, %rax
jmp _end2
_clause1:
movq $2, %rax
_end2:
movq %rbp, %rsp
popq %rbp
retq
