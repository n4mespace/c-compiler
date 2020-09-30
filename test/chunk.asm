	
	mov eax, 5
	push eax
	mov eax, 3
	pop ebx
	sub eax, ebx
	push eax
	mov eax, 10
	pop ebx
	sub eax, ebx

	mov b, eax