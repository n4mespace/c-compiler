	enter 0, 0
	
	mov eax, 1
	mov dword ptr [ebp + 4], eax
	mov eax, 3
	mov dword ptr [ebp + 8], eax
	mov eax, dword ptr [ebp + 8]
	push eax
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp + 4]
	pop ebx
	add eax, ebx
	pop ebx
	add eax, ebx
	mov dword ptr [ebp + 4], eax
	mov eax, dword ptr [ebp + 4]
	
	leave
	mov b, eax