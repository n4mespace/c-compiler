	jmp __start_main
	
__start_one:
	mov eax, 1
	jmp __end_one
	
__start_main:
	push ebp
	mov ebp, esp
	
	jmp __start_one
	
__end_one:
	mov dword ptr [ebp + 8], eax
	mov eax, dword ptr [ebp + 8]
	push eax
	jmp __start_addOne
	
__end_addOne:
	pop ebx
	add eax, ebx
	
	mov esp, ebp
	pop ebp
	
	mov b, eax
	jmp __end_main
	
__start_addOne:
	mov eax, dword ptr [ebp + 12]
	push eax
	mov eax, 1
	pop ebx
	add eax, ebx
	jmp __end_addOne
	
__end_main: