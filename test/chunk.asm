
	mov eax, 10
	push eax
	mov eax, 2
	pop ebx
	sub eax, ebx
	push eax
	mov eax, 2
	push eax
	mov eax, 10
	pop ebx
	sub eax, ebx
	pop ebx
	sub eax, ebx
	xor eax, -1
	mov b, eax