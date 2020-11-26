
	jmp __start_program
	
__func_main:
	enter 12, 0
	
	
	call __func_one
	mov dword ptr [ebp - 4], eax
	mov eax, 4
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	xor edx, edx
	div ebx
	mov eax, edx
	mov dword ptr [ebp - 4], eax
	mov eax, dword ptr [ebp - 4]
	
	leave
	ret	
	
__func_one:
	enter 4, 0
	
	mov eax, 1
	
	leave
	ret	
	
__start_program:
	call __func_main
	mov b, eax
	