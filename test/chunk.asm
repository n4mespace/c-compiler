	push ebp
	mov ebp, esp
	
	mov eax, 3
	mov dword ptr [ebp + 4], eax
	mov eax, dword ptr [ebp + 4]
	push eax
	mov eax, 2
	pop ebx
	add eax, ebx
	mov dword ptr [ebp + 8], eax
	mov eax, 0
	mov dword ptr [ebp + 12], eax
	mov eax, 4
	mov dword ptr [ebp + 4], eax
	mov eax, dword ptr [ebp + 4]
	
	mov esp, ebp
	pop ebp
	
	mov b, eax