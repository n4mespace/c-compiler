
	jmp __start_program
	
__func_main:
	enter 8, 0
	
	
	mov eax, 1
	
	cmp eax, 0
	je __wsqrnl_endif
	
	mov eax, 3
	leave
	ret
	
__wsqrnl_endif:
	mov eax, 1
	leave
	ret
	
	leave
	ret
	
__start_program:
	call __func_main
	mov b, eax
	