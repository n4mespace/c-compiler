
	mov eax, 4
	push eax
	mov eax, 10
	pop ebx
	xor edx, edx
	div ebx
	mov eax, edx
	mov b, eax