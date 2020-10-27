	push ebp
	mov ebp, esp
	
	mov eax, 1
	mov dword ptr [ebp + 4], eax
	mov eax, 4
	mov dword ptr [ebp + 8], eax
	
	mov eax, dword ptr [ebp + 4]
	cmp eax, 0
	jne __etezevhs
	je __nmyxqbqi
	
__etezevhs:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	cmp eax, ebx
	setg al
	jmp __ret
	
__nmyxqbqi:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	cmp eax, ebx
	setl al
	jmp __ret
	
__ret:
	mov esp, ebp
	pop ebp
	
	mov b, eax