	push ebp
	mov ebp, esp
	
	mov eax, 3
	push eax
	mov eax, 15
	pop ebx
	sub eax, ebx
	mov dword ptr [ebp + 4], eax
	mov eax, 97
	mov dword ptr [ebp + 8], eax
	mov eax, dword ptr [ebp + 4]
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp + 8], eax
	mov eax, dword ptr [ebp + 8]
	
	mov esp, ebp
	pop ebp
	
	mov b, eax