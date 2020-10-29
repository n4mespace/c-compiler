	push ebp
	mov ebp, esp
	
	mov eax, 0
	mov dword ptr [ebp + 4], eax
	
	mov eax, 1
	cmp eax, 0
	jne __fnydcw_if
	je __slyjfe_else
	
__fnydcw_if:
	mov eax, 13
	push eax
	mov eax, dword ptr [ebp + 4]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp + 4], eax
	jmp__mkrmka_endif
	
__slyjfe_else:
	mov eax, 2
	push eax
	mov eax, dword ptr [ebp + 4]
	pop ebx
	add eax, ebx
	mov dword ptr [ebp + 4], eax
	
__mkrmka_endif:
	mov eax, dword ptr [ebp + 4]
	jmp__ret
	
__ret:
	mov esp, ebp
	pop ebp
	
	mov b, eax