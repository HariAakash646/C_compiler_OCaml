.globl main
main:
movq $10, %rax
cmpq $0, %rax
movq $0, %rax
sete %al
retq
