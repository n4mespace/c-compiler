
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
	
	leave
	ret
	
__func_fib:
	enter 32, 0
	
	mov eax, 0
	mov dword ptr [ebp - 8], eax
	mov eax, 1
	mov dword ptr [ebp - 12], eax
	mov eax, 0
	mov dword ptr [ebp - 16], eax
	mov eax, dword ptr [ebp - 12]
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 20], eax
	
	jmp __gevkld_while
	
__uyytjk_wloop:
	mov eax, dword ptr [ebp - 20]
	push eax
	mov eax, dword ptr [ebp - 12]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 24], eax
	mov eax, dword ptr [ebp - 20]
	mov dword ptr [ebp - 12], eax
	mov eax, dword ptr [ebp - 24]
	mov dword ptr [ebp - 20], eax
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 16]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 16], eax
	
__gevkld_while:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp + 8]
	push eax
	mov eax, dword ptr [ebp - 16]
	pop ebx
	cmp eax, ebx
	setl al
	pop ebx
	sub eax, ebx
	cmp eax, 1
	je __uyytjk_wloop
	mov eax, dword ptr [ebp - 24]
	leave
	ret
	
	leave
	ret
	
__start_program:
	call __func_main
	mov b, eax
	