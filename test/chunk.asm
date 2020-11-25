
	jmp __start_program
	
__func_main:
	enter 12, 0
	
	mov eax, 2
	mov dword ptr [ebp - 4], eax
	__break
	mov eax, dword ptr [ebp - 4]
	
	leave
	ret	
	
__start_program:
	call __func_main
	mov b, eax
	