.globl main
main:
movq $12, %rax
negq %rax
pushq %rax
movq $5, %rax
movq %rax, %rcx
popq %rax
cqo
idivq %rcx
retq
