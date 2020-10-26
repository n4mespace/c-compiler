	push ebp
	mov ebp, esp
	
	mov eax, 2
	push eax
	mov eax, 13
	pop ebx
	xor edx, edx
	div ebx
	mov eax, edx
	mov dword ptr [ebp + 4], eax
	mov eax, 3
	mov dword ptr [ebp + 8], eax
	mov eax, dword ptr [ebp + 4]
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp + 8], eax
	mov eax, dword ptr [ebp + 4]
	push eax
	mov eax, a
	neg eax
	pop ebx
	xor edx, edx
	div ebx
	mov eax, edx
	xor eax, -1
	
	mov esp, ebp
	pop ebp
	
	mov b, eax