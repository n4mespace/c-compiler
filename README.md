# c-compiler

## Description
Compile C file into asm (masm32) chunk for using 
it within cpp `__asm {}` for instance

## Supports and limitations
- function declaration / definition / call ("main" -> program entry point)
- if / if-else
- for / while loops with break, continue
- variables of type int, char, bool (everything become 32 bit int)
- global / local scopes
- binary / unary operators with different nesting level ([check full grammar](src/Compiler/Syntax))
- no imports

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

## Build with cabal

``` bash
>> cabal build
>> cabal install
```
