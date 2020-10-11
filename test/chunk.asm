
	mov eax, 10
	push eax
	mov eax, 2
	neg eax
	pop ebx
	sub eax, ebx
	push eax
	mov eax, 2
	push eax
	mov eax, 10
	pop ebx
	sub eax, ebx
	pop ebx
	add eax, ebx
	xor eax, -1
	mov b, eax