
	jmp __start_program
	
__func_one:
	push ebp
	mov ebp, esp
	
	mov eax, 1
	
	mov esp, ebp
	pop ebp
	ret 0
	
__func_main:
	push ebp
	mov ebp, esp
	
	call one
	mov dword ptr [ebp - 4], eax
	mov eax, dword ptr [ebp - 4]
	push eax
	mov eax, dword ptr [ebp - 4]
	push eax
	call addOne
	pop ebx
	add eax, ebx
	
	mov esp, ebp
	pop ebp
	ret 0
	
__func_addOne:
	push ebp
	mov ebp, esp
	
	mov eax, dword ptr [ebp + 8]
	push eax
	mov eax, 1
	pop ebx
	add eax, ebx
	
	mov esp, ebp
	pop ebp
	ret 4
	
__start_program:
	call __func_main
	