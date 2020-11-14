
	jmp __start_program
	
__func_main:
	enter 8, 0
	
	mov eax, 10
	push eax
	call fib
	add esp, 4
	mov dword ptr [ebp - 4], eax
	mov eax, dword ptr [ebp - 4]
	
	leave
	ret	
	
__func_fib:
	enter 28, 0
	
	mov eax, 0
	mov dword ptr [ebp - 8], eax
	mov eax, 1
	mov dword ptr [ebp - 12], eax
	mov eax, 0
	mov dword ptr [ebp - 16], eax
	mov eax, 0
	mov dword ptr [ebp - 20], eax
	mov eax, dword ptr [ebp - 12]
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 24], eax
	
	jmp __aprvql_while
	
__jazfdf_wloop:
	mov eax, dword ptr [ebp - 24]
	push eax
	mov eax, dword ptr [ebp - 12]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 16], eax
	mov eax, dword ptr [ebp - 24]
	mov dword ptr [ebp - 12], eax
	mov eax, dword ptr [ebp - 16]
	mov dword ptr [ebp - 24], eax
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 20]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 20], eax
	
__aprvql_while:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp + 8]
	push eax
	mov eax, dword ptr [ebp - 20]
	pop ebx
	cmp eax, ebx
	setl al
	pop ebx
	sub eax, ebx
	cmp eax, 1
	je __jazfdf_wloop
	mov eax, dword ptr [ebp - 16]
	
	leave
	ret	
	
__start_program:
	call __func_main
	mov b, eax
	