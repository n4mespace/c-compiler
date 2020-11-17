
	jmp __start_program
	
__func_fact:
	enter 12, 0
	
	mov eax, 1
	mov dword ptr [ebp + 4], eax
	
	mov eax, 2
	mov dword ptr [ebp + 0], eax
	
	jmp __kuctox_for_cond
	
__qbtoer_for_start:
	
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	sub eax, ebx
	push eax
	mov eax, dword ptr [ebp + 0]
	pop ebx
	cmp eax, ebx
	setl al
	cmp eax, 0
	sete al
	cmp eax, 0
	jne __zrxkoy_if
	je __khpbxf_else
	
__zrxkoy_if:
	mov eax, dword ptr [ebp + 0]
	push eax
	mov eax, dword ptr [ebp + 4]
	pop ebx
	imul ebx
	mov dword ptr [ebp + 4], eax
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp + 0]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp + 0], eax
	jmp __jonesl_endif
	
__khpbxf_else:
	jmp __dtmehn_for_end
	
__jonesl_endif:
	jmp __kuctox_for_cond
	mov eax, 100
	push eax
	mov eax, dword ptr [ebp + 0]
	pop ebx
	imul ebx
	mov dword ptr [ebp + 0], eax
	
	
__kuctox_for_cond:1
	cmp eax, 1
	je __qbtoer_for_start
	
__dtmehn_for_end:
	mov eax, dword ptr [ebp + 4]
	
	leave
	ret	
	
__func_main:
	enter 4, 0
	
	
	mov eax, 10
	push eax
	call __func_fact
	add esp, 4
	
	leave
	ret	
	
__start_program:
	call __func_main
	mov b, eax
	