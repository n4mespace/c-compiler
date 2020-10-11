
	mov eax, 3
	push eax
	mov eax, 10
	pop ebx
	xor edx, edx
	div ebx
	mov eax, edx
	push eax
	mov eax, 1
	pop ebx
	sub eax, ebx
	mov b, eax