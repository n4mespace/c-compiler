
	jmp __start_program
	
__func_main:
	enter 8, 0
	
	mov eax, 10
	mov dword ptr [ebp - 4], eax
	
	mov eax, dword ptr [ebp - 4]
	push eax
	call __func_algorithm
	add esp, 4
	
	leave
	ret	
	
__func_algorithm:
	enter 16, 0
	
	
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	cmp eax, ebx
	setl al
	
	cmp eax, 0
	je __omncgi_endif
	
	mov eax, 0
	
	leave
	ret
	
__omncgi_endif:
	
	mov eax, dword ptr [ebp + 8]
	push eax
	call __func_series_a
	add esp, 4
	mov dword ptr [ebp - 4], eax
	
	mov eax, dword ptr [ebp + 8]
	push eax
	call __func_series_b
	add esp, 4
	mov dword ptr [ebp - 8], eax
	
	mov eax, dword ptr [ebp - 8]
	push eax
	mov eax, dword ptr [ebp - 4]
	push eax
	call __func_getMax
	add esp, 8
	
	leave
	ret	
	
__func_series_a:
	enter 24, 0
	
	mov eax, 2
	neg eax
	mov dword ptr [ebp - 4], eax
	mov eax, 2
	mov dword ptr [ebp - 8], eax
	mov eax, 3
	mov dword ptr [ebp - 12], eax
	mov eax, 1
	mov dword ptr [ebp - 16], eax
	
	mov eax, 1
	mov dword ptr [ebp - 20], eax
	
	jmp __ffbenj_for_cond
	
__zgkqlb_for_start:
	mov eax, dword ptr [ebp - 12]
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	imul ebx
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 4], eax
	
	mov eax, dword ptr [ebp - 16]
	cmp eax, 0
	jne __vimpgp_if
	je __yohxhf_else
	
__vimpgp_if:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 8], eax
	jmp __gygavw_endif
	
__yohxhf_else:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp - 12]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 12], eax
	
__gygavw_endif:
	mov eax, dword ptr [ebp - 16]
	cmp eax, 0
	sete al
	mov dword ptr [ebp - 16], eax
	
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 20]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 20], eax
	
__ffbenj_for_cond:
	mov eax, dword ptr [ebp + 8]
	push eax
	mov eax, dword ptr [ebp - 20]
	pop ebx
	cmp eax, ebx
	setl al
	cmp eax, 1
	je __zgkqlb_for_start
	
__nixuke_for_end:
	mov eax, dword ptr [ebp - 4]
	
	leave
	ret	
	
__func_series_b:
	enter 16, 0
	
	mov eax, 1
	mov dword ptr [ebp - 4], eax
	mov eax, 4
	mov dword ptr [ebp - 8], eax
	
	mov eax, 2
	mov dword ptr [ebp - 12], eax
	
	jmp __erfonh_for_cond
	
__xdgdos_for_start:
	
	mov eax, 0
	push eax
	mov eax, dword ptr [ebp - 12]
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	xor edx, edx
	div ebx
	mov eax, edx
	pop ebx
	cmp eax, ebx
	sete al
	
	cmp eax, 0
	je __sbjiok_endif
	
	mov eax, dword ptr [ebp - 12]
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	cdq
	idiv ebx
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	imul ebx
	mov dword ptr [ebp - 4], eax
	
__sbjiok_endif:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	imul ebx
	mov dword ptr [ebp - 8], eax
	
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 12]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 12], eax
	
__erfonh_for_cond:
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	add eax, ebx
	push eax
	mov eax, dword ptr [ebp - 12]
	pop ebx
	cmp eax, ebx
	setl al
	cmp eax, 1
	je __xdgdos_for_start
	
__muqgmw_for_end:
	mov eax, dword ptr [ebp - 4]
	
	leave
	ret	
	
__func_getMax:
	enter 8, 0
	
	
	mov eax, 0
	push eax
	mov eax, dword ptr [ebp + 8]
	push eax
	mov eax, dword ptr [ebp + 12]
	pop ebx
	sub eax, ebx
	pop ebx
	cmp eax, ebx
	setl al
	
	cmp eax, 0
	je __camfbn_endif
	
	mov eax, dword ptr [ebp + 8]
	
	leave
	ret
	
__camfbn_endif:
	mov eax, dword ptr [ebp + 12]
	
	leave
	ret	
	
__start_program:
	call __func_main
	mov b, eax
	