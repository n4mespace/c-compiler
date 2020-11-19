# c-compiler

## Description
Compile C file into asm (masm32) chunk for using 
it within cpp `__asm {}` for instance

## Supports and limitations
- API consists of `Lib.compileFile` and `Lib.compileString` functions
- function declaration / definition / call ("main" -> program entry point)
- for function call use cdecl declaration (possible to use standart C function in asm as well)
- if / if-else
- for / while loops with break, continue
- variables of type int, char, bool (everything become 32 bit int)
- global / local scopes
- binary / unary operators with different nesting level ([check full grammar](src/Compiler/Syntax))
- no imports
- no optimizations

## Run all tests ([here](test/Spec.hs))
```bash
>> cabal test --test-show-details=direct
```

## Test your example
- build c-compiler or run in interpreter
- create file test.c with code to compile
- execute program and get chunk.asm
- fill it in this cpp template

```c++
#include <iostream>
using namespace std;

int main() {
	int b;
	__asm {
		... <- put here
	}
	cout << b << endl;
}
```

## Usage example

> test.c

```c
int factLoop(int n);
int factRecursion(int n);
             
int main() {
	l = factLoop(10);
	r = factLoop(10);
	if (l == r) {
		return 0;
	}
	return 1;
}

int factLoop(int n) {
	int c = 1;
	for (int i = 2; i < n - 1;) {
		c *= i;
		i += 1;
	}
	return c;
}

int factRecursion(int n) {
	if (n < 1) {
		return 1;
	}
	return n * factRecursion(n - 1);
}
```

> chunk.asm

```assembly
        jmp __start_program

__func_main:
        enter 16, 0


        mov eax, 10
        push eax
        call __func_factLoop
        add esp, 4
        mov dword ptr [ebp - 4], eax

        mov eax, 10
        push eax
        call __func_factLoop
        add esp, 4
        mov dword ptr [ebp - 8], eax

        mov eax, dword ptr [ebp - 8]
        push eax
        mov eax, dword ptr [ebp - 4]
        pop ebx
        cmp eax, ebx
        sete al

        cmp eax, 0
        je __ofmdjx_endif

        mov eax, 0

        leave
        ret

__ofmdjx_endif:
        mov eax, 1

        leave
        ret

__func_factLoop:
        enter 12, 0

        mov eax, 1
        mov dword ptr [ebp - 4], eax

        mov eax, 2
        mov dword ptr [ebp - 8], eax

        jmp __caxaxl_for_cond

__wtfddi_for_start:
        mov eax, dword ptr [ebp - 8]
        push eax
        mov eax, dword ptr [ebp - 4]
        pop ebx
        imul ebx
        mov dword ptr [ebp - 4], eax
        mov eax, 1
        push eax
        mov eax, dword ptr [ebp - 8]
        pop ebx
        add eax, ebx
        mov dword ptr [ebp - 8], eax


__caxaxl_for_cond:
        mov eax, 1
        push eax
        mov eax, dword ptr [ebp + 8]
        pop ebx
        sub eax, ebx
        push eax
        mov eax, dword ptr [ebp - 8]
        pop ebx
        cmp eax, ebx
        setl al
        cmp eax, 1
        je __wtfddi_for_start

__lewqjb_for_end:
        mov eax, dword ptr [ebp - 4]

        leave
        ret

__func_factRecursion:
        enter 8, 0


        mov eax, 1
        push eax
        mov eax, dword ptr [ebp + 8]
        pop ebx
        cmp eax, ebx
        setl al

        cmp eax, 0
        je __dtgaoi_endif

        mov eax, 1

        leave
        ret

__dtgaoi_endif:

        mov eax, 1
        push eax
        mov eax, dword ptr [ebp + 8]
        pop ebx
        sub eax, ebx
        push eax
        call __func_factRecursion
        add esp, 4
        push eax
        mov eax, dword ptr [ebp + 8]
        pop ebx
        imul ebx

        leave
        ret

__start_program:
        call __func_main
        mov b, eax
```

## Build with cabal

``` bash
>> cabal build
>> cabal install
```
