
	jmp __start_program
	
__func_main:
	enter 16, 0
	
	
	mov eax, 10
	push eax
	call __func_factLoop
	add esp, 4
	mov dword ptr [ebp - 4], eax
	
	mov eax, 10
	push eax
	call __func_factRecursion
	add esp, 4
	mov dword ptr [ebp - 8], eax
	
	mov eax, dword ptr [ebp - 8]
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	cmp eax, ebx
	sete al
	
	cmp eax, 0
	je __akevdt_endif
	
	mov eax, 0
	
	leave
	ret
	
__akevdt_endif:
	mov eax, 1
	
	leave
	ret	
	
__func_factLoop:
	enter 12, 0
	
	mov eax, 1
	mov dword ptr [ebp - 4], eax
	
	mov eax, 2
	mov dword ptr [ebp - 8], eax
	
	jmp __oeprhh_for_cond
	
__ixxvxe_for_start:
	mov eax, dword ptr [ebp - 8]
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	imul ebx
	mov dword ptr [ebp - 4], eax
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 8], eax
	
	
__oeprhh_for_cond:
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	sub eax, ebx
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	cmp eax, ebx
	setl al
	cmp eax, 1
	je __ixxvxe_for_start
	
__xixidx_for_end:
	mov eax, dword ptr [ebp - 4]
	
	leave
	ret	
	
__func_factRecursion:
	enter 8, 0
	
	
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	cmp eax, ebx
	setl al
	
	cmp eax, 0
	je __pxxrye_endif
	
	mov eax, 1
	
	leave
	ret
	
__pxxrye_endif:
	
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	sub eax, ebx
	push eax
	call __func_factRecursion
	add esp, 4
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	imul ebx
	
	leave
	ret	
	
__start_program:
	call __func_main
	mov b, eax
	