	push ebp
	mov ebp, esp
	
	mov eax, 0
	mov dword ptr [ebp + 4], eax
	
	mov eax, 1
	cmp eax, 0
	jne __vzuygt_if
	je __wdyqut_else
	
__vzuygt_if:
	mov eax, 13
	push eax
	mov eax, dword ptr [ebp + 4]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp + 4], eax
	jmp __wfiaxc_endif
	
__wdyqut_else:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp + 4]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp + 4], eax
	
__wfiaxc_endif:
	mov eax, dword ptr [ebp + 4]
	jmp __ret
	
__ret:
	mov esp, ebp
	pop ebp
	
	mov b, eax