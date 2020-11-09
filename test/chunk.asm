	jmp __start_main
	
__start_two:
	mov eax, 2
	jmp __end_two
	
__start_main:
	push ebp
	mov ebp, esp
	
	jmp __start_one
	
__end_one:
	mov dword ptr [ebp + 4], eax
	mov eax, 3
	push eax
	mov eax, dword ptr [ebp + 4]
	pop ebx
	imul ebx
	mov dword ptr [ebp + 4], eax
	mov eax, dword ptr [ebp + 4]
	push eax
	jmp __start_two
	
__end_two:
	pop ebx
	imul ebx
	
	mov esp, ebp
	pop ebp
	
	mov b, eax
	jmp __end_main
	
__start_one:
	mov eax, 1
	jmp __end_one
	
__end_main: