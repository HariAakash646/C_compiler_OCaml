.globl main
main:
movq $1, %rax
pushq %rax
movq $0, %rax
pushq %rax
movq $2, %rax
movq %rax, %rcx
popq %rax
cmpq $0, %rax
jne _clause1
movq $0, %rax
jmp _end1
_clause1:
cmpq $0, %rcx
movq $1, %rax
jne _end1
movq $0, %rax
_end1:
movq %rax, %rcx
popq %rax
cmpq $0, %rax
je _clause2
movq $1, %rax
jmp _end2
_clause2:
cmpq $0, %rcx
movq $0, %rax
je _end2
movq $1, %rax
_end2:
retq
