	.text
	.file	"MicroJ"
	.globl	Stack                           # -- Begin function Stack
	.p2align	4, 0x90
	.type	Stack,@function
Stack:                                  # @Stack
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$24, %edi
	callq	malloc@PLT
	movq	my_struct_ptr@GOTPCREL(%rip), %rcx
	movq	%rax, (%rcx)
	movq	$0, (%rax)
	movq	$0, 8(%rax)
	movq	$0, 16(%rax)
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	Stack, .Lfunc_end0-Stack
	.cfi_endproc
                                        # -- End function
	.globl	Stack.1                         # -- Begin function Stack.1
	.p2align	4, 0x90
	.type	Stack.1,@function
Stack.1:                                # @Stack.1
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	$24, %edi
	callq	malloc@PLT
	movq	my_struct_ptr@GOTPCREL(%rip), %rcx
	movq	%rax, (%rcx)
	movq	$0, (%rax)
	movq	$0, 16(%rax)
	movq	%rbx, 8(%rax)
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	Stack.1, .Lfunc_end1-Stack.1
	.cfi_endproc
                                        # -- End function
	.globl	init                            # -- Begin function init
	.p2align	4, 0x90
	.type	init,@function
init:                                   # @init
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%rdi, 8(%rsp)
	movl	8(%rdi), %edi
	shll	$3, %edi
	callq	malloc@PLT
	movq	%rax, (%rbx)
	addq	$16, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	init, .Lfunc_end2-init
	.cfi_endproc
                                        # -- End function
	.globl	push                            # -- Begin function push
	.p2align	4, 0x90
	.type	push,@function
push:                                   # @push
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	callq	isFull@PLT
	testb	$1, %al
	je	.LBB3_3
# %bb.1:                                # %then
	xorl	%eax, %eax
	jmp	.LBB3_2
.LBB3_3:                                # %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -16(%rax)
	movq	-8(%rbp), %rax
	movq	16(%rax), %rax
	movq	-16(%rbp), %rdx
	movq	%rdx, (%rcx,%rax,8)
	movq	-8(%rbp), %rax
	incq	16(%rax)
	movb	$1, %al
.LBB3_2:                                # %then
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end3:
	.size	push, .Lfunc_end3-push
	.cfi_endproc
                                        # -- End function
	.globl	pop                             # -- Begin function pop
	.p2align	4, 0x90
	.type	pop,@function
pop:                                    # @pop
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	callq	isEmpty@PLT
	testb	$1, %al
	je	.LBB4_3
# %bb.1:                                # %then
	movq	$-1, %rax
	jmp	.LBB4_2
.LBB4_3:                                # %else
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -16(%rax)
	movq	%rsp, %rcx
	leaq	-16(%rcx), %rsp
	movq	-8(%rbp), %rdx
	movq	16(%rdx), %rdx
	movq	-16(%rax), %rax
	movq	-8(%rax,%rdx,8), %rax
	movq	%rax, -16(%rcx)
	movq	-8(%rbp), %rax
	decq	16(%rax)
	movq	-16(%rcx), %rax
.LBB4_2:                                # %then
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end4:
	.size	pop, .Lfunc_end4-pop
	.cfi_endproc
                                        # -- End function
	.globl	peak                            # -- Begin function peak
	.p2align	4, 0x90
	.type	peak,@function
peak:                                   # @peak
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, -8(%rsp)
	movq	(%rdi), %rax
	movq	%rax, -16(%rsp)
	movq	16(%rdi), %rcx
	movq	-8(%rax,%rcx,8), %rax
	retq
.Lfunc_end5:
	.size	peak, .Lfunc_end5-peak
	.cfi_endproc
                                        # -- End function
	.globl	isEmpty                         # -- Begin function isEmpty
	.p2align	4, 0x90
	.type	isEmpty,@function
isEmpty:                                # @isEmpty
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, -8(%rsp)
	cmpq	$0, 16(%rdi)
	sete	%al
	retq
.Lfunc_end6:
	.size	isEmpty, .Lfunc_end6-isEmpty
	.cfi_endproc
                                        # -- End function
	.globl	isFull                          # -- Begin function isFull
	.p2align	4, 0x90
	.type	isFull,@function
isFull:                                 # @isFull
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rdi, -8(%rsp)
	movq	16(%rdi), %rax
	cmpq	8(%rdi), %rax
	sete	%al
	retq
.Lfunc_end7:
	.size	isFull, .Lfunc_end7-isFull
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movl	$10, %edi
	callq	Stack.1@PLT
	movq	%rax, 8(%rsp)
	movq	%rax, %rdi
	callq	init@PLT
	movq	$0, (%rsp)
	cmpq	$9, (%rsp)
	jg	.LBB8_3
	.p2align	4, 0x90
.LBB8_2:                                # %while_body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsp), %rdi
	movq	(%rsp), %rsi
	callq	push@PLT
	incq	(%rsp)
	cmpq	$9, (%rsp)
	jle	.LBB8_2
.LBB8_3:                                # %merge
	movq	8(%rsp), %rdi
	callq	isFull@PLT
	testb	$1, %al
	je	.LBB8_4
# %bb.10:                               # %then
	leaq	.Lfmt.3(%rip), %rdi
	leaq	.Lstr(%rip), %rsi
	xorl	%eax, %eax
	callq	printf@PLT
.LBB8_4:                                # %merge8
	movq	$0, (%rsp)
	leaq	.Lfmt(%rip), %rbx
	cmpq	$9, (%rsp)
	jg	.LBB8_7
	.p2align	4, 0x90
.LBB8_6:                                # %while_body10
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsp), %rdi
	callq	pop@PLT
	movq	%rbx, %rdi
	movq	%rax, %rsi
	xorl	%eax, %eax
	callq	printf@PLT
	incq	(%rsp)
	cmpq	$9, (%rsp)
	jle	.LBB8_6
.LBB8_7:                                # %merge11
	movq	8(%rsp), %rdi
	callq	isEmpty@PLT
	testb	$1, %al
	je	.LBB8_9
# %bb.8:                                # %then20
	leaq	.Lfmt.3(%rip), %rdi
	leaq	.Lstr.4(%rip), %rsi
	xorl	%eax, %eax
	callq	printf@PLT
.LBB8_9:                                # %merge19
	xorl	%eax, %eax
	addq	$16, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end8:
	.size	main, .Lfunc_end8-main
	.cfi_endproc
                                        # -- End function
	.type	my_struct_ptr,@object           # @my_struct_ptr
	.bss
	.globl	my_struct_ptr
	.p2align	3
my_struct_ptr:
	.quad	0
	.size	my_struct_ptr, 8

	.type	.Lfmt,@object                   # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.2,@object                 # @fmt.2
.Lfmt.2:
	.asciz	"%f\n"
	.size	.Lfmt.2, 4

	.type	.Lfmt.3,@object                 # @fmt.3
.Lfmt.3:
	.asciz	"%s\n"
	.size	.Lfmt.3, 4

	.type	.Lstr,@object                   # @str
.Lstr:
	.asciz	"\"Full\""
	.size	.Lstr, 7

	.type	.Lstr.4,@object                 # @str.4
.Lstr.4:
	.asciz	"\"Empty\""
	.size	.Lstr.4, 8

	.type	.Lfmt.5,@object                 # @fmt.5
.Lfmt.5:
	.asciz	"%d\n"
	.size	.Lfmt.5, 4

	.type	.Lfmt.6,@object                 # @fmt.6
.Lfmt.6:
	.asciz	"%f\n"
	.size	.Lfmt.6, 4

	.type	.Lfmt.7,@object                 # @fmt.7
.Lfmt.7:
	.asciz	"%s\n"
	.size	.Lfmt.7, 4

	.type	.Lfmt.8,@object                 # @fmt.8
.Lfmt.8:
	.asciz	"%d\n"
	.size	.Lfmt.8, 4

	.type	.Lfmt.9,@object                 # @fmt.9
.Lfmt.9:
	.asciz	"%f\n"
	.size	.Lfmt.9, 4

	.type	.Lfmt.10,@object                # @fmt.10
.Lfmt.10:
	.asciz	"%s\n"
	.size	.Lfmt.10, 4

	.type	.Lfmt.11,@object                # @fmt.11
.Lfmt.11:
	.asciz	"%d\n"
	.size	.Lfmt.11, 4

	.type	.Lfmt.12,@object                # @fmt.12
.Lfmt.12:
	.asciz	"%f\n"
	.size	.Lfmt.12, 4

	.type	.Lfmt.13,@object                # @fmt.13
.Lfmt.13:
	.asciz	"%s\n"
	.size	.Lfmt.13, 4

	.type	.Lfmt.14,@object                # @fmt.14
.Lfmt.14:
	.asciz	"%d\n"
	.size	.Lfmt.14, 4

	.type	.Lfmt.15,@object                # @fmt.15
.Lfmt.15:
	.asciz	"%f\n"
	.size	.Lfmt.15, 4

	.type	.Lfmt.16,@object                # @fmt.16
.Lfmt.16:
	.asciz	"%s\n"
	.size	.Lfmt.16, 4

	.type	.Lfmt.17,@object                # @fmt.17
.Lfmt.17:
	.asciz	"%d\n"
	.size	.Lfmt.17, 4

	.type	.Lfmt.18,@object                # @fmt.18
.Lfmt.18:
	.asciz	"%f\n"
	.size	.Lfmt.18, 4

	.type	.Lfmt.19,@object                # @fmt.19
.Lfmt.19:
	.asciz	"%s\n"
	.size	.Lfmt.19, 4

	.type	.Lfmt.20,@object                # @fmt.20
.Lfmt.20:
	.asciz	"%d\n"
	.size	.Lfmt.20, 4

	.type	.Lfmt.21,@object                # @fmt.21
.Lfmt.21:
	.asciz	"%f\n"
	.size	.Lfmt.21, 4

	.type	.Lfmt.22,@object                # @fmt.22
.Lfmt.22:
	.asciz	"%s\n"
	.size	.Lfmt.22, 4

	.section	".note.GNU-stack","",@progbits
