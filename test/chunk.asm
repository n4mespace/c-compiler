
	jmp __start_program
	
__func_main:
	enter 12, 0
	
	mov eax, 0
	mov dword ptr [ebp - 4], eax
	mov eax, 100
	mov dword ptr [ebp - 8], eax
	
	mov eax, dword ptr [ebp - 4]
	cmp eax, 0
	sete al
	cmp eax, 0
	jne __zljxlr_if
	je __zljxlr_else
	
__zljxlr_if:
	mov eax, 13
	mov dword ptr [ebp - 12], eax
	mov eax, dword ptr [ebp - 12]
	leave
	ret
	jmp __zljxlr_endif
	
__zljxlr_else:
	mov eax, 4
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	cdq
	idiv ebx
	leave
	ret
	
__zljxlr_endif:
	
	leave
	ret
	
__start_program:
	call __func_main
	mov b, eax
	