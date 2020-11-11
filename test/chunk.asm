
	jmp __start_program
	
__func_two:
	push ebp
	mov ebp, esp
	
	mov eax, 2
	
	mov esp, ebp
	pop ebp
	ret
	
__func_main:
	push ebp
	mov ebp, esp
	
	call two
	mov dword ptr [ebp - 4], eax
	mov eax, dword ptr [ebp - 4]
	push eax
	mov eax, dword ptr [ebp - 4]
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	imul ebx
	push eax
	mov eax, dword ptr [ebp - 4]
	push eax
	mov eax, dword ptr [ebp - 4]
	pop ebx
	imul ebx
	push eax
	call add
	add esp, 8
	pop ebx
	add eax, ebx
	
	mov esp, ebp
	pop ebp
	ret
	
__func_add:
	push ebp
	mov ebp, esp
	
	mov eax, dword ptr [ebp + 8]
	push eax
	mov eax, dword ptr [ebp + 12]
	pop ebx
	sub eax, ebx
	
	mov esp, ebp
	pop ebp
	ret
	
__start_program:
	call __func_main
	mov b, eax
	