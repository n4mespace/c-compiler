
	jmp __start_program
	
__func_main:
	enter 12, 0
	
	mov eax, 4
	mov dword ptr [ebp - 4], eax
	mov eax, 4
	mov dword ptr [ebp - 8], eax
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	sub eax, ebx
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
	
__start_program:
	call __func_main
	mov b, eax
	