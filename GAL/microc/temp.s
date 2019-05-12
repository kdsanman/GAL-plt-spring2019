	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$64, %rsp
	.cfi_def_cfa_offset 80
	.cfi_offset %rbx, -16
	leaq	L_string(%rip), %rax
	movq	%rax, 24(%rsp)
	callq	_make_list
	movq	%rax, %rbx
	movl	$4, %edi
	callq	_malloc
	movl	$1, (%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_add_tail
	movl	$4, %edi
	callq	_malloc
	movl	$2, (%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_add_tail
	movq	%rbx, 16(%rsp)
	callq	_make_list
	movq	%rax, %rbx
	movl	$8, %edi
	callq	_malloc
	leaq	L_string.3(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_add_tail
	movl	$8, %edi
	callq	_malloc
	leaq	L_string.4(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_add_tail
	movl	$8, %edi
	callq	_malloc
	leaq	L_string.5(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_add_tail
	movq	%rbx, 8(%rsp)
	movq	24(%rsp), %rsi
	leaq	L_fmt.1(%rip), %rdi
	xorl	%eax, %eax
	callq	_printf
	movq	16(%rsp), %rdi
	callq	_printil
	movq	8(%rsp), %rdi
	callq	_printl
	xorl	%eax, %eax
	addq	$64, %rsp
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%d\n"

L_fmt.1:                                ## @fmt.1
	.asciz	"%s\n"

L_fmt.2:                                ## @fmt.2
	.asciz	"%g\n"

L_string:                               ## @string
	.asciz	"hhelo"

L_string.3:                             ## @string.3
	.asciz	"x"

L_string.4:                             ## @string.4
	.asciz	"y"

L_string.5:                             ## @string.5
	.asciz	"z"


.subsections_via_symbols
