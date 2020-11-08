# c-compiler

## To build with stack

``` bash
>> stack build
>> stack exec c-compiler
```

## To build with cabal

``` bash
>> cabal build
>> cabal install
```
and run your exe in /dist/build

## To test generated masm32:
- create file test.c with C code
- execute program and get chunk.asm
- fill it in this cpp template

```c++
#include <iostream>
#include <string>
#include <stdint.h>

using namespace std;

int main() {
	int b;
	__asm {
		... <- put here
	}
	cout << b << endl;
}
```
