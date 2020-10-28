	push ebp
	mov ebp, esp
	
	mov eax, 0
	mov dword ptr [ebp + 4], eax
	mov eax, 100
	mov dword ptr [ebp + 8], eax
	
	mov eax, dword ptr [ebp + 4]
	cmp eax, 0
	sete al
	cmp eax, 0
	jne __tdpcdw_if
	je __pkqtpn_else
	
__tdpcdw_if:
	mov eax, 13
	mov dword ptr [ebp + 12], eax
	mov eax, dword ptr [ebp + 12]
	push eax
	mov eax, 2
	pop ebx
	imul ebx
	mov dword ptr [ebp + 16], eax
	mov eax, dword ptr [ebp + 12]
	jmp __ret
	
__pkqtpn_else:
	mov eax, 4
	push eax
	mov eax, dword ptr [ebp + 8]
	pop ebx
	cdq
	idiv ebx
	jmp __ret
	
__ret:
	mov esp, ebp
	pop ebp
	
	mov b, eax