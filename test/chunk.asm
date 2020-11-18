
	jmp __start_program
	
__func_divByTen:
	enter 4, 0
	
	mov eax, 10
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	cdq
	idiv ebx
	
	leave
	ret	
	
__func_main:
	enter 12, 0
	
	
	mov eax, 10
	push eax
	call __func_divByTen
	add esp, 4
	mov dword ptr [ebp - 4], eax
	__break
	mov eax, dword ptr [ebp - 4]
	
	leave
	ret	
	
__start_program:
	call __func_main
	mov b, eax
	