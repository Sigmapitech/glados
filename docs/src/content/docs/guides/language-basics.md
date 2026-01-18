---
title: Language Basics
description: Learn the fundamental concepts of Quant programming
---

## Variables

Variables in Quant must be declared with an explicit type annotation using the colon (`:`) syntax.

### Declaration

```quant
name: type;
```

### Declaration with Initialization

```quant
name: type = value;
```

### Examples

```quant
fn main() -> int {
    // Integer variable
    x: int = 42;
    
    // Boolean variable
    is_active: bool = True;
    
    // String variable
    message: str = "Hello";
    
    // Uninitialized variable
    count: int;
    count = 10;
    
    return 0;
}
```

## Constants

Use the `const` qualifier to make variables immutable:

```quant
fn main() -> int {
    pi: const float = 3.14159;
    max_size: const int = 100;
    
    // pi = 3.14;  // Error: cannot modify const variable
    
    return 0;
}
```

The `const` qualifier can also be used in type declarations:

```quant
matrix: [[const int]];  // Array of arrays containing const integers
```

## Primitive Types

Quant supports several primitive types:

### Integer Types

- **`int`**: Default 32-bit signed integer
- **`int<N>`**: Custom bit-size signed integer (e.g., `int<64>`)
- **`int<N, s>`**: Custom bit-size with signedness (e.g., `int<32, u>` for unsigned)

```quant
a: int = 42;              // 32-bit signed
b: int<64> = 1000000;     // 64-bit signed
c: int<32, u> = 100;      // 32-bit unsigned
```

### Float Types

- **`float`**: Default 64-bit floating-point
- **`float<32>`**: 32-bit floating-point
- **`float<64>`**: 64-bit floating-point

```quant
x: float = 3.14159;
y: float<32> = 2.718;
```

### Boolean Type

- **`bool`**: True or False

```quant
flag: bool = True;
is_valid: bool = False;
```

### String Type

- **`str`**: String type for text

```quant
message: str = "Hello, Quant!";
```

### Void Type

- **`void`**: Used for functions that don't return a value

```quant
fn print_message() -> void {
    print("No return value\n");
}
```

## Type Casting

Quant supports explicit type casting using the type name as a function:

```quant
import math

fn main() -> int {
    x: float = 3.14;
    y: int = int(x);  // Cast float to int: y = 3
    
    z: float = float(42);  // Cast int to float: z = 42.0
    
    // Casting in expressions
    result: int = int(math.sqrt(16));
    
    return 0;
}
```

## Expressions

### Arithmetic Expressions

```quant
a: int = 10 + 5;      // Addition
b: int = 20 - 8;      // Subtraction
c: int = 6 * 7;       // Multiplication
d: int = 20 / 4;      // Division
e: int = 17 % 5;      // Modulo
```

### Comparison Expressions

```quant
x: bool = (5 > 3);    // Greater than
y: bool = (5 < 3);    // Less than
z: bool = (5 == 5);   // Equal to
w: bool = (5 != 3);   // Not equal to
a: bool = (5 >= 5);   // Greater than or equal
b: bool = (5 <= 3);   // Less than or equal
```

### Logical Expressions

```quant
x: bool = True && False;   // Logical AND
y: bool = True || False;   // Logical OR
z: bool = !True;           // Logical NOT
```

### Bitwise Expressions

```quant
a: int = 5 & 3;       // Bitwise AND
b: int = 5 | 3;       // Bitwise OR
c: int = 5 ^ 3;       // Bitwise XOR
d: int = 5 << 1;      // Left shift
e: int = 5 >> 1;      // Right shift
```

## Assignment Operators

Basic assignment and compound assignments:

```quant
x: int = 10;

x = 20;        // Assignment
x += 5;        // Add and assign: x = x + 5
x -= 3;        // Subtract and assign: x = x - 3
x *= 2;        // Multiply and assign: x = x * 2
x /= 4;        // Divide and assign: x = x / 4
x %= 3;        // Modulo and assign: x = x % 3
```

## Function Calls

Call functions using parentheses:

```quant
from std import print

fn add(a: int, b: int) -> int {
    return a + b;
}

fn main() -> int {
    result: int = add(5, 3);
    print("Result: %d\n", result);
    return 0;
}
```

## Scope

Variables are scoped to the block in which they are declared:

```quant
fn main() -> int {
    x: int = 10;  // Outer scope
    
    {
        y: int = 20;  // Inner scope
        print("%d\n", x);  // Can access outer scope
    }
    
    // print("%d\n", y);  // Error: y not in scope
    
    return 0;
}
```
