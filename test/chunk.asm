jmp __start_main
	
__start_main:
	push ebp
	mov ebp, esp
	
	jmp __start_test
	
__end_test:
	mov dword ptr [ebp + 4], eax
	mov eax, 3
	push eax
	mov eax, dword ptr [ebp + 4]
	pop ebx
	cdq
	idiv ebx
	mov dword ptr [ebp + 4], eax
	mov eax, dword ptr [ebp + 4]
	
	mov esp, ebp
	pop ebp
	
	mov b, eax
	jmp __end_main
	
__start_test:
	mov eax, 1
	jmp __end_test
	
__end_main: