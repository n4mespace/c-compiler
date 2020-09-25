# c-compiler

## To build with stack

``` bash
>> stack build
>> stack exec c-compiler
```

## To build with cabal

``` bash
>> cabal install
>> cabal build
```
## and run your exe in /dist/build

## To test generated masm32:
## - create file lab1.txt with C code
## - execute program and get  
## - fill this cpp template

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
