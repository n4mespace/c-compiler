
	jmp __start_program
	
__func_main:
	enter 8, 0
	
	
	mov eax, 10
	push eax
	call __func_fib
	add esp, 4
	mov dword ptr [ebp - 4], eax
	mov eax, dword ptr [ebp - 4]
	
	leave
	ret	
	
__func_fib:
	enter 20, 0
	
	mov eax, 0
	mov dword ptr [ebp - 4], eax
	mov eax, 1
	mov dword ptr [ebp - 8], eax
	mov eax, 0
	mov dword ptr [ebp - 12], eax
	mov eax, dword ptr [ebp + 8]
	mov dword ptr [ebp - 16], eax
	mov eax, dword ptr [ebp - 12]
	
	leave
	ret	
	
__func_main1:
	enter 12, 0
	
	mov eax, 2
	mov dword ptr [ebp - 4], eax
	mov eax, dword ptr [ebp - 4]
	mov dword ptr [ebp - 8], eax
	
	mov eax, 0
	cmp eax, 0
	jne __vidcgc_if
	je __sulsea_else
	
__vidcgc_if:
	mov eax, 13
	mov dword ptr [ebp - 12], eax
	mov eax, 3
	mov dword ptr [ebp - 16], eax
	mov eax, dword ptr [ebp - 12]
	push eax
	mov eax, 2
	pop ebx
	imul ebx
	mov dword ptr [ebp - 20], eax
	mov eax, dword ptr [ebp - 12]
	
	leave
	ret
	jmp __ifjoyq_endif
	
__sulsea_else:
	mov eax, 4
	
	leave
	ret
	
__ifjoyq_endif:
	leave
	ret
	
__start_program:
	call __func_main
	mov b, eax
	