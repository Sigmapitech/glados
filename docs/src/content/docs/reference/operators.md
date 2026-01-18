---
title: Operators
description: Complete reference for all operators in Quant
---

Quant provides a comprehensive set of operators for arithmetic, comparison, logical, bitwise operations, and assignments.

## Operator Precedence

Operators are evaluated in the following order (highest to lowest precedence):

1. Unary operators (`!`, `-`, `~`)
2. Multiplicative (`*`, `/`, `%`)
3. Additive (`+`, `-`)
4. Shift (`<<`, `>>`)
5. Relational (`<`, `<=`, `>`, `>=`)
6. Equality (`==`, `!=`)
7. Bitwise AND (`&`)
8. Bitwise XOR (`^`)
9. Bitwise OR (`|`)
10. Logical AND (`&&`)
11. Logical OR (`||`)
12. Assignment (`=`, `+=`, `-=`, etc.)

Use parentheses to override precedence:

```quant
x: int = (5 + 3) * 2;  // 16, not 11
```

## Arithmetic Operators

### Binary Arithmetic

| Operator | Name           | Example | Description                     |
|----------|----------------|---------|---------------------------------|
| `+`      | Addition       | `a + b` | Adds two numbers                |
| `-`      | Subtraction    | `a - b` | Subtracts b from a              |
| `*`      | Multiplication | `a * b` | Multiplies two numbers          |
| `/`      | Division       | `a / b` | Divides a by b                  |
| `%`      | Modulo         | `a % b` | Remainder of a divided by b     |

```quant
fn main() -> int {
    a: int = 10 + 5;    // 15
    b: int = 20 - 8;    // 12
    c: int = 6 * 7;     // 42
    d: int = 20 / 4;    // 5
    e: int = 17 % 5;    // 2

    return 0;
}
```

### Unary Arithmetic

| Operator | Name     | Example | Description                  |
|----------|----------|---------|------------------------------|
| `-`      | Negation | `-a`    | Negates the value            |
| `+`      | Plus     | `+a`    | Returns the value (identity) |

```quant
fn main() -> int {
    x: int = 10;
    y: int = -x;    // -10
    z: int = +x;    // 10

    return 0;
}
```

## Comparison Operators

Comparison operators return boolean values.

| Operator | Name                   | Example   | Description                              |
|----------|------------------------|-----------|------------------------------------------|
| `==`     | Equal to               | `a == b`  | True if a equals b                       |
| `!=`     | Not equal to           | `a != b`  | True if a does not equal b               |
| `<`      | Less than              | `a < b`   | True if a is less than b                 |
| `<=`     | Less than or equal     | `a <= b`  | True if a is less than or equal to b     |
| `>`      | Greater than           | `a > b`   | True if a is greater than b              |
| `>=`     | Greater than or equal  | `a >= b`  | True if a is greater than or equal to b  |

```quant
fn main() -> int {
    x: int = 5;
    y: int = 10;

    flag1: bool = (x == y);   // False
    flag2: bool = (x != y);   // True
    flag3: bool = (x < y);    // True
    flag4: bool = (x <= y);   // True
    flag5: bool = (x > y);    // False
    flag6: bool = (x >= y);   // False

    return 0;
}
```

## Logical Operators

Logical operators work with boolean values.

| Operator | Name        | Example   | Description                       |
|----------|-------------|-----------|-----------------------------------|
| `&&`     | Logical AND | `a && b`  | True if both a and b are true     |
| `||`     | Logical OR  | `a || b`  | True if either a or b is true     |
| `!`      | Logical NOT | `!a`      | True if a is false                |

```quant
fn main() -> int {
    a: bool = True;
    b: bool = False;

    c: bool = a && b;    // False
    d: bool = a || b;    // True
    e: bool = !a;        // False
    f: bool = !b;        // True

    // Short-circuit evaluation
    result: bool = (5 > 3) && (10 < 20);  // True

    return 0;
}
```

### Short-Circuit Evaluation

Logical `&&` and `||` use short-circuit evaluation:

- `a && b`: If `a` is false, `b` is not evaluated
- `a || b`: If `a` is true, `b` is not evaluated

## Bitwise Operators

Bitwise operators perform operations on individual bits.

| Operator | Name        | Example   | Description                              |
|----------|-------------|-----------|------------------------------------------|
| `&`      | Bitwise AND | `a & b`   | Sets each bit to 1 if both bits are 1    |
| `|`      | Bitwise OR  | `a | b`   | Sets each bit to 1 if either bit is 1    |
| `^`      | Bitwise XOR | `a ^ b`   | Sets each bit to 1 if only one bit is 1  |
| `<<`     | Left shift  | `a << n`  | Shifts bits left by n positions          |
| `>>`     | Right shift | `a >> n`  | Shifts bits right by n positions         |
| `~`      | Bitwise NOT | `~a`      | Inverts all bits                         |

```quant
fn main() -> int {
    a: int = 0b1100;     // 12 in binary
    b: int = 0b1010;     // 10 in binary

    c: int = a & b;      // 0b1000 = 8
    d: int = a | b;      // 0b1110 = 14
    e: int = a ^ b;      // 0b0110 = 6
    f: int = a << 2;     // 0b110000 = 48
    g: int = a >> 2;     // 0b0011 = 3
    h: int = ~a;         // Inverts all bits

    return 0;
}
```

## Assignment Operators

### Simple Assignment

| Operator | Name       | Example | Description    |
|----------|------------|---------|----------------|
| `=`      | Assignment | `a = b` | Assigns b to a |

```quant
fn main() -> int {
    x: int;
    x = 42;  // Assign 42 to x

    return 0;
}
```

### Compound Assignment

Compound assignment operators combine an operation with assignment.

| Operator | Name               | Example    | Equivalent To  |
|----------|--------------------|------------|----------------|
| `+=`     | Add assign         | `a += b`   | `a = a + b`    |
| `-=`     | Subtract assign    | `a -= b`   | `a = a - b`    |
| `*=`     | Multiply assign    | `a *= b`   | `a = a * b`    |
| `/=`     | Divide assign      | `a /= b`   | `a = a / b`    |
| `%=`     | Modulo assign      | `a %= b`   | `a = a % b`    |
| `&=`     | AND assign         | `a &= b`   | `a = a & b`    |
| `|=`     | OR assign          | `a |= b`   | `a = a | b`    |
| `^=`     | XOR assign         | `a ^= b`   | `a = a ^ b`    |
| `<<=`    | Left shift assign  | `a <<= b`  | `a = a << b`   |
| `>>=`    | Right shift assign | `a >>= b`  | `a = a >> b`   |

```quant
fn main() -> int {
    x: int = 10;

    x += 5;   // x = 15
    x -= 3;   // x = 12
    x *= 2;   // x = 24
    x /= 4;   // x = 6
    x %= 4;   // x = 2

    return 0;
}
```

## Array Index Operator

Access array elements using square brackets.

| Operator | Name  | Example   | Description                   |
|----------|-------|-----------|-------------------------------|
| `[]`     | Index | `arr[i]`  | Access element at index i     |

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 20;

    x: int = numbers[0];  // x = 10
    y: int = numbers[1];  // y = 20

    // Multi-dimensional arrays
    matrix: [[int]];
    matrix[0][0] = 1;
    matrix[0][1] = 2;

    return 0;
}
```

## Parentheses

Parentheses group expressions and override operator precedence.

```quant
fn main() -> int {
    // Without parentheses (follows precedence)
    x: int = 2 + 3 * 4;        // 14 (multiplication first)

    // With parentheses
    y: int = (2 + 3) * 4;      // 20 (addition first)

    // Complex expressions
    z: int = ((10 + 5) * 2) - (3 * 4);  // 18

    return 0;
}
```

## Operator Associativity

### Left-Associative Operators

Most operators are left-associative (evaluated left-to-right):

```quant
a - b - c  // Evaluated as (a - b) - c
a / b / c  // Evaluated as (a / b) / c
```

### Right-Associative Operators

Assignment operators are right-associative (evaluated right-to-left):

```quant
a = b = c  // Evaluated as a = (b = c)
```

## Type Requirements

### Arithmetic Operators

- Both operands must be numeric types (`int` or `float`)
- Result type matches operand types

### Comparison Operators

- Operands must be comparable (same type)
- Result is always `bool`

### Logical Operators

- Operands must be `bool`
- Result is `bool`

### Bitwise Operators

- Operands must be integer types
- Result type matches operand types

## Examples

### Complex Expression

```quant
fn main() -> int {
    x: int = 10;
    y: int = 20;
    z: int = 30;

    // Complex arithmetic
    result: int = (x + y) * z - (x % 3);

    // Complex logical
    flag: bool = (x < y) && (y < z) || (x == 10);

    // Mixed operations
    value: int = (x + y) * 2 / 3 % 5;

    return 0;
}
```

### Bitwise Manipulation

```quant
fn main() -> int {
    flags: int = 0b0000;

    // Set bit 0
    flags = flags | 0b0001;     // flags = 0b0001

    // Set bit 2
    flags = flags | 0b0100;     // flags = 0b0101

    // Clear bit 0
    flags = flags & 0b1110;     // flags = 0b0100

    // Toggle bit 1
    flags = flags ^ 0b0010;     // flags = 0b0110

    return 0;
}
```
