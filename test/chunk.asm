
	jmp __start_program
	
__func_main:
	enter 16, 0
	
	mov eax, 1
	mov dword ptr [ebp - 4], eax
	mov eax, dword ptr [ebp - 4]
	mov dword ptr [ebp - 8], eax
	
	jmp __rrxubp_while_cond
	
__pfbbff_while_start:
	
	
	jmp __hidkzl_for_cond
	
__sylytg_for_start:
	mov eax, dword ptr [ebp - 4]
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	imul ebx
	mov dword ptr [ebp - 8], eax
	
	jmp __ykkjgm_while_cond
	
__xspdmw_while_start:
	jmp __ycurqr_while_end
	
__ykkjgm_while_cond:
	mov eax, 1
	cmp eax, 1
	je __xspdmw_while_start
	
__ycurqr_while_end:
	
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 8], eax
	
__hidkzl_for_cond:
	mov eax, 10
	push eax
	mov eax, dword ptr [ebp - 8]
	pop ebx
	cmp eax, ebx
	setl al
	cmp eax, 1
	je __sylytg_for_start
	
__ivdblw_for_end:
	mov eax, 1
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp - 4], eax
	jmp __rrxubp_while_cond
	mov eax, 100
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	imul ebx
	mov dword ptr [ebp - 4], eax
	
__rrxubp_while_cond:
	mov eax, 7
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	cmp eax, ebx
	setl al
	cmp eax, 1
	je __pfbbff_while_start
	
__qosrlr_while_end:
	mov eax, dword ptr [ebp - 8]
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	add eax, ebx
	
	leave
	ret	
	
__start_program:
	call __func_main
	mov b, eax
	