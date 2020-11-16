
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
	enter 24, 0
	
	mov eax, 0
	mov dword ptr [ebp - 4], eax
	mov eax, 1
	mov dword ptr [ebp - 8], eax
	mov eax, 0
	mov dword ptr [ebp - 12], eax
	mov eax, dword ptr [ebp - 8]
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 16], eax
	
	
	jmp __cgubxz_for_cond
	
__tyuwis_for_start:
	
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	sub eax, ebx
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	cmp eax, ebx
	setl al
	cmp eax, 0
	jne __ufpequ_if
	je __sbvqhb_else
	
__ufpequ_if:
	mov eax, dword ptr [ebp - 16]
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 12], eax
	mov eax, dword ptr [ebp - 16]
	mov dword ptr [ebp - 8], eax
	mov eax, dword ptr [ebp - 12]
	mov dword ptr [ebp - 16], eax
	jmp __cgubxz_for_cond
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp - 16]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 16], eax
	jmp __bmisqy_endif
	
__sbvqhb_else:
	jmp __bulyax_for_end
	
__bmisqy_endif:
	
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 4], eax
	
__cgubxz_for_cond:
	cmp eax, 1
	je __tyuwis_for_start
	
__bulyax_for_end:
	mov eax, dword ptr [ebp - 12]
	
	leave
	ret	
	
__start_program:
	call __func_main
	mov b, eax
	