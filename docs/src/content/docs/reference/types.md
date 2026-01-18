---
title: Types
description: Complete reference for Quant's type system
---

Quant features a rich, statically-typed system that provides both safety and flexibility. All variables must have explicit type annotations.

## Primitive Types

### Integer Types

Quant provides flexible integer types with customizable bit sizes and signedness.

#### Basic Integer

```quant
x: int;  // 32-bit signed integer by default
```

#### Custom Bit Size

```quant
small: int<8>;      // 8-bit signed integer
medium: int<16>;    // 16-bit signed integer
large: int<64>;     // 64-bit signed integer
huge: int<128>;     // 128-bit signed integer
```

#### Custom Signedness

```quant
positive: int<32, u>;   // 32-bit unsigned integer
signed: int<32, s>;     // 32-bit signed integer (explicit)
```

**Signedness options:**

- `s` - Signed (can represent negative numbers)
- `u` - Unsigned (only non-negative numbers)

#### Integer Literal Formats

Quant supports multiple number bases:

```quant
decimal: int = 42;        // Decimal (base 10)
hex: int = 0x2A;          // Hexadecimal (base 16)
octal: int = 0o52;        // Octal (base 8)
binary: int = 0b101010;   // Binary (base 2)
```

### Float Types

Floating-point types for decimal numbers.

```quant
f1: float;        // 64-bit float by default
f2: float<32>;    // 32-bit float
f3: float<64>;    // 64-bit float (explicit)
```

**Float literal:**

```quant
pi: float = 3.14159;
e: float<32> = 2.71828;
```

### Boolean Type

Boolean type for true/false values.

```quant
flag: bool = True;
is_valid: bool = False;
```

**Boolean values:**

- `True`
- `False`

### String Type

String type for text data.

```quant
message: str = "Hello, World!";
name: str = "Quant";
```

**String literals** use double quotes (`"`) and support escape sequences:

- `\n` - Newline
- `\t` - Tab
- `\"` - Quote
- `\\` - Backslash

### Character Type

Character type for single characters.

```quant
ch: char = 'A';
newline: char = '\n';
```

**Character literals** use single quotes (`'`).

### Void Type

The void type indicates no value.

```quant
fn print_message() -> void {
    print("This function returns nothing\n");
}
```

## Composite Types

### Array Types

Arrays are declared using square brackets around the element type.

#### One-Dimensional Arrays

```quant
numbers: [int];           // Array of integers
values: [float];          // Array of floats
flags: [bool];            // Array of booleans
```

#### Multi-Dimensional Arrays

```quant
matrix: [[int]];          // 2D array (array of arrays)
cube: [[[int]]];          // 3D array
```

#### Arrays with Const Elements

```quant
constants: [const int];           // Array of immutable integers
matrix: [[const int]];            // 2D array of immutable integers
```

See the [Arrays Guide](/guides/arrays/) for more details on array usage.

### Function Types

Functions are declared with parameter types and a return type.

```quant
fn add(a: int, b: int) -> int {
    return a + b;
}

fn print_message(msg: str) -> void {
    print("%s\n", msg);
}

fn process() -> float {
    return 3.14;
}
```

**Function signature syntax:**

```quant
fn name(param1: type1, param2: type2, ...) -> return_type {
    // body
}
```

## Type Qualifiers

### Const Qualifier

The `const` qualifier makes a type immutable after initialization.

```quant
pi: const float = 3.14159;
max_size: const int = 100;

// pi = 3.14;  // Error: cannot modify const variable
```

#### Const in Types

```quant
values: [const int];              // Array of const integers
data: [[const float]];            // 2D array of const floats
```

## Qualified Types

A qualified type combines a base type with optional qualifiers:

```quant
x: int;                   // Plain type
y: const int;             // Qualified with const
z: [const int];           // Array type with const elements
```

## Type Inference

While Quant requires explicit type annotations for variable declarations, it can infer types in certain contexts:

### In Function Calls

```quant
fn calculate() -> int {
    return 42;
}

fn main() -> int {
    x: int = calculate();  // Type of calculate() is inferred
    return 0;
}
```

### In Expressions

```quant
fn main() -> int {
    x: int = 10;
    y: int = 20;
    z: int = x + y;  // Type of (x + y) is inferred
    return 0;
}
```

## Type Casting

Explicit type conversion using the target type as a function:

```quant
fn main() -> int {
    // Float to int
    x: float = 3.14;
    y: int = int(x);  // y = 3
    
    // Int to float
    a: int = 42;
    b: float = float(a);  // b = 42.0
    
    // In expressions
    c: int = int(3.9) + int(2.1);  // c = 5
    
    return 0;
}
```

## Type Compatibility

### Assignment Compatibility

- Types must match exactly for assignment
- No implicit conversions (must use explicit casting)

```quant
x: int = 42;
y: float = float(x);    // OK: explicit cast
// z: float = x;        // Error: type mismatch
```

### Parameter Compatibility

Function arguments must match parameter types exactly:

```quant
fn add(a: int, b: int) -> int {
    return a + b;
}

fn main() -> int {
    x: int = 10;
    y: int = 20;
    result: int = add(x, y);  // OK: types match
    
    // z: float = 5.0;
    // add(x, z);  // Error: float doesn't match int parameter
    
    return 0;
}
```

## Type Sizes and Ranges

### Integer Ranges

Integer value ranges depend on bit size and signedness:

| Type | Bits | Signed Range | Unsigned Range |
|------|------|--------------|----------------|
| `int<8, s>` | 8 | -128 to 127 | N/A |
| `int<8, u>` | 8 | N/A | 0 to 255 |
| `int<16, s>` | 16 | -32,768 to 32,767 | N/A |
| `int<16, u>` | 16 | N/A | 0 to 65,535 |
| `int` (default) | 32 | -2,147,483,648 to 2,147,483,647 | N/A |
| `int<32, u>` | 32 | N/A | 0 to 4,294,967,295 |
| `int<64, s>` | 64 | -(2^63) to (2^63)-1 | N/A |
| `int<64, u>` | 64 | N/A | 0 to (2^64)-1 |

### Float Precision

| Type | Bits | Precision |
|------|------|-----------|
| `float<32>` | 32 | ~7 decimal digits |
| `float` (default) | 64 | ~15 decimal digits |
