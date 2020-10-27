	push ebp
	mov ebp, esp
	
	mov eax, 1
	mov dword ptr [ebp + 4], eax
	mov eax, 2
	mov dword ptr [ebp + 4], eax
	mov eax, 3
	mov dword ptr [ebp + 4], eax
	
	mov eax, dword ptr [ebp + 4]
	
	cmp eax, 0
	je __buqkzq_if
	
	mov eax, 4
	mov dword ptr [ebp + 4], eax
	
__buqkzq_if:
	mov eax, dword ptr [ebp + 4]
	jmp __ret
	
__ret:
	mov esp, ebp
	pop ebp
	
	mov b, eax