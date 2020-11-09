	push ebp
	mov ebp, esp
	
	mov eax, 3
	jmp __ret
	
__ret:
	mov esp, ebp
	pop ebp
	
	mov b, eax	push ebp
	mov ebp, esp
	
	mov eax, 2
	mov dword ptr [ebp + 4], eax
	mov eax, 3
	push eax
	mov eax, dword ptr [ebp + 4]
	pop ebx
	cdq
	idiv ebx
	mov dword ptr [ebp + 4], eax
	mov eax, dword ptr [ebp + 4]
	jmp __ret
	
__ret:
	mov esp, ebp
	pop ebp
	
	mov b, eax