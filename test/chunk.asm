
	jmp __start_program
	
__func_main:
	enter 20, 0
	
	mov eax, 1
	mov dword ptr [ebp - 4], eax
	mov eax, 99
	mov dword ptr [ebp - 8], eax
	mov eax, 8
	push eax
	call addOne
	add esp, 4
	mov dword ptr [ebp - 12], eax
	mov eax, dword ptr [ebp - 4]
	push eax
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp - 12]
	pop ebx
	imul ebx
	push eax
	call addTwoIfFlag
	add esp, 8
	mov dword ptr [ebp - 16], eax
	mov eax, dword ptr [ebp - 4]
	cmp eax, 0
	sete al
	push eax
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	add eax, ebx
	push eax
	call addTwoIfFlag
	add esp, 8
	
	leave
	ret
	
__func_addOne:
	enter 4, 0
	
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	add eax, ebx
	
	leave
	ret
	
__func_addTwoIfFlag:
	enter 4, 0
	
	
	mov eax, dword ptr [ebp + 8]
	cmp eax, 0
	jne __fsfkzl_if
	je __qlhgoi_else
	
__fsfkzl_if:
	mov eax, dword ptr [ebp + 12]
	jmp __yynxha_endif
	
__qlhgoi_else:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp + 12]
	pop ebx
	add eax, ebx
	
__yynxha_endif:
	
	leave
	ret
	
__start_program:
	call __func_main
	mov b, eax
	