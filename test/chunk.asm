
	jmp __start_program
	
__func_main:
	enter 12, 0
	
	mov eax, 1
	mov dword ptr [ebp - 4], eax
	mov eax, 1
	mov dword ptr [ebp - 8], eax
	mov eax, dword ptr [ebp - 4]
	leave
	ret
	
	leave
	ret
	
__start_program:
	call __func_main
	mov b, eax
	