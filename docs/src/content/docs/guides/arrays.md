---
title: Arrays
description: Working with arrays in Quant - from basics to multi-dimensional arrays
---

## Overview

Arrays in Quant are dynamic collections of elements of the same type. They support indexing, multi-dimensional structures, and can contain const elements.

## Array Declaration

### Basic Syntax

Arrays are declared using square brackets around the element type:

```quant
name: [type];
```

### Examples

```quant
fn main() -> int {
    numbers: [int];        // Array of integers
    values: [float];       // Array of floats
    flags: [bool];         // Array of booleans
    messages: [str];       // Array of strings
    
    return 0;
}
```

## Array Indexing

Access and modify array elements using square bracket notation with zero-based indexing.

### Basic Indexing

```quant
fn main() -> int {
    numbers: [int];
    
    // Set values
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    
    // Get values
    x: int = numbers[0];  // x = 10
    y: int = numbers[1];  // y = 20
    
    return 0;
}
```

### Index Operations

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    
    // Read and modify
    numbers[0] = numbers[0] + 5;  // numbers[0] = 15
    
    // Use in expressions
    sum: int = numbers[0] + numbers[1];  // sum = 35
    
    // Compound assignment
    numbers[1] += 10;  // numbers[1] = 30
    
    return 0;
}
```

## Iterating Over Arrays

### For Loop with Index

The most common way to iterate over arrays:

```quant
from std import print

fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    numbers[3] = 40;
    numbers[4] = 50;
    
    for (i: int = 0; i < 5; i++) {
        print("numbers[%d] = %d\n", i, numbers[i]);
    }
    
    return 0;
}
```

**Output:**

```
numbers[0] = 10
numbers[1] = 20
numbers[2] = 30
numbers[3] = 40
numbers[4] = 50
```

### Processing Array Elements

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 1;
    numbers[1] = 2;
    numbers[2] = 3;
    numbers[3] = 4;
    numbers[4] = 5;
    
    // Double each element
    for (i: int = 0; i < 5; i++) {
        numbers[i] = numbers[i] * 2;
    }
    
    // Print results
    for (i: int = 0; i < 5; i++) {
        print("%d ", numbers[i]);  // Output: 2 4 6 8 10
    }
    print("\n");
    
    return 0;
}
```

## Multi-Dimensional Arrays

Quant supports arrays of arrays (multi-dimensional arrays).

### 2D Arrays

Declare a 2D array (matrix) using nested square brackets:

```quant
matrix: [[int]];  // 2D array of integers
```

### Accessing 2D Array Elements

```quant
fn main() -> int {
    matrix: [[int]];
    
    // Set values
    matrix[0][0] = 1;
    matrix[0][1] = 2;
    matrix[0][2] = 3;
    
    matrix[1][0] = 4;
    matrix[1][1] = 5;
    matrix[1][2] = 6;
    
    // Get values
    x: int = matrix[0][0];  // x = 1
    y: int = matrix[1][2];  // y = 6
    
    return 0;
}
```

### Iterating Over 2D Arrays

Use nested loops to iterate over 2D arrays:

```quant
from std import print

fn main() -> int {
    matrix: [[int]];
    
    // Initialize 3x3 matrix
    for (i: int = 0; i < 3; i++) {
        for (j: int = 0; j < 3; j++) {
            matrix[i][j] = i * 3 + j + 1;
        }
    }
    
    // Print matrix
    for (i: int = 0; i < 3; i++) {
        for (j: int = 0; j < 3; j++) {
            print("%d ", matrix[i][j]);
        }
        print("\n");
    }
    
    return 0;
}
```

**Output:**

```
1 2 3 
4 5 6 
7 8 9 
```

### 3D Arrays and Beyond

You can create arrays with any number of dimensions:

```quant
cube: [[[int]]];              // 3D array
hypercube: [[[[int]]]];       // 4D array
```

**Accessing 3D array:**

```quant
fn main() -> int {
    cube: [[[int]]];
    
    cube[0][0][0] = 1;
    cube[1][2][3] = 42;
    
    x: int = cube[0][0][0];  // x = 1
    
    return 0;
}
```

## Const Arrays

Use the `const` qualifier to create arrays with immutable elements:

```quant
constants: [const int];
```

### Setting Const Array Values

You can initialize const array elements, but cannot modify them after:

```quant
fn main() -> int {
    constants: [const int];
    
    constants[0] = 10;  // OK: initial assignment
    constants[1] = 20;  // OK: initial assignment
    
    // constants[0] = 30;  // Error: cannot modify const element
    
    return 0;
}
```

### Const in Multi-Dimensional Arrays

```quant
matrix: [[const int]];        // 2D array with const elements
cube: [[[const float]]];      // 3D array with const elements
```

## Common Array Patterns

### Finding Maximum Element

```quant
from std import print

fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 50;
    numbers[2] = 30;
    numbers[3] = 90;
    numbers[4] = 20;
    
    max: int = numbers[0];
    max_index: int = 0;
    
    for (i: int = 1; i < 5; i++) {
        if (numbers[i] > max) {
            max = numbers[i];
            max_index = i;
        }
    }
    
    print("Maximum value: %d at index %d\n", max, max_index);
    
    return 0;
}
```

**Output:**

```
Maximum value: 90 at index 3
```

### Finding Minimum Element

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 50;
    numbers[2] = 5;
    numbers[3] = 90;
    numbers[4] = 20;
    
    min: int = numbers[0];
    
    for (i: int = 1; i < 5; i++) {
        if (numbers[i] < min) {
            min = numbers[i];
        }
    }
    
    print("Minimum value: %d\n", min);  // Output: Minimum value: 5
    
    return 0;
}
```

### Calculating Sum and Average

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    numbers[3] = 40;
    numbers[4] = 50;
    
    sum: int = 0;
    
    for (i: int = 0; i < 5; i++) {
        sum += numbers[i];
    }
    
    average: int = sum / 5;
    
    print("Sum: %d\n", sum);          // Output: Sum: 150
    print("Average: %d\n", average);  // Output: Average: 30
    
    return 0;
}
```

### Reversing an Array

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 1;
    numbers[1] = 2;
    numbers[2] = 3;
    numbers[3] = 4;
    numbers[4] = 5;
    
    size: int = 5;
    
    // Reverse in place
    for (i: int = 0; i < size / 2; i++) {
        temp: int = numbers[i];
        numbers[i] = numbers[size - 1 - i];
        numbers[size - 1 - i] = temp;
    }
    
    // Print reversed array
    for (i: int = 0; i < size; i++) {
        print("%d ", numbers[i]);  // Output: 5 4 3 2 1
    }
    print("\n");
    
    return 0;
}
```

### Searching in an Array

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    numbers[3] = 40;
    numbers[4] = 50;
    
    target: int = 30;
    found_index: int = -1;
    
    for (i: int = 0; i < 5; i++) {
        if (numbers[i] == target) {
            found_index = i;
            break;
        }
    }
    
    if (found_index != -1) {
        print("Found %d at index %d\n", target, found_index);
    } else {
        print("%d not found\n", target);
    }
    
    return 0;
}
```

### Matrix Operations

#### Matrix Addition

```quant
fn main() -> int {
    a: [[int]];
    b: [[int]];
    result: [[int]];
    
    // Initialize matrices
    for (i: int = 0; i < 2; i++) {
        for (j: int = 0; j < 2; j++) {
            a[i][j] = i + j;
            b[i][j] = i * j;
        }
    }
    
    // Add matrices
    for (i: int = 0; i < 2; i++) {
        for (j: int = 0; j < 2; j++) {
            result[i][j] = a[i][j] + b[i][j];
        }
    }
    
    // Print result
    for (i: int = 0; i < 2; i++) {
        for (j: int = 0; j < 2; j++) {
            print("%d ", result[i][j]);
        }
        print("\n");
    }
    
    return 0;
}
```

#### Matrix Transpose

```quant
fn main() -> int {
    matrix: [[int]];
    transposed: [[int]];
    
    // Initialize 3x3 matrix
    for (i: int = 0; i < 3; i++) {
        for (j: int = 0; j < 3; j++) {
            matrix[i][j] = i * 3 + j + 1;
        }
    }
    
    // Transpose
    for (i: int = 0; i < 3; i++) {
        for (j: int = 0; j < 3; j++) {
            transposed[j][i] = matrix[i][j];
        }
    }
    
    // Print transposed matrix
    for (i: int = 0; i < 3; i++) {
        for (j: int = 0; j < 3; j++) {
            print("%d ", transposed[i][j]);
        }
        print("\n");
    }
    
    return 0;
}
```

## Complete Example

Here's a comprehensive example using various array features:

```quant
from std import print

fn main() -> int {
    print("=== Quant Array Showcase ===\n\n");
    
    // 1. Basic array indexing
    print("1. Basic Array:\n");
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    numbers[3] = 40;
    numbers[4] = 50;
    
    for (i: int = 0; i < 5; i++) {
        print("   numbers[%d] = %d\n", i, numbers[i]);
    }
    print("\n");
    
    // 2. 2D array (matrix)
    print("2. Matrix (3x3):\n");
    matrix: [[const int]];
    for (i: int = 0; i < 3; i++) {
        for (j: int = 0; j < 3; j++) {
            matrix[i][j] = i * 3 + j + 1;
        }
    }
    
    for (i: int = 0; i < 3; i++) {
        print("   ");
        for (j: int = 0; j < 3; j++) {
            print("%d ", matrix[i][j]);
        }
        print("\n");
    }
    print("\n");
    
    // 3. Finding maximum
    print("3. Finding Maximum:\n");
    max: int = numbers[0];
    max_index: int = 0;
    
    for (i: int = 1; i < 5; i++) {
        if (numbers[i] > max) {
            max = numbers[i];
            max_index = i;
        }
    }
    
    print("   Maximum: %d at index %d\n\n", max, max_index);
    
    return 0;
}
```

**Output:**

```
=== Quant Array Showcase ===

1. Basic Array:
   numbers[0] = 10
   numbers[1] = 20
   numbers[2] = 30
   numbers[3] = 40
   numbers[4] = 50

2. Matrix (3x3):
   1 2 3 
   4 5 6 
   7 8 9 

3. Finding Maximum:
   Maximum: 50 at index 4
```

## Best Practices

1. **Always track array size** - Store the size in a variable when iterating
2. **Use meaningful index names** - `i` for single loops, `i, j` for nested loops
3. **Check bounds carefully** - Make sure loop conditions don't exceed array size
4. **Use const for immutable data** - Mark array elements as const when they shouldn't change
5. **Initialize before use** - Always set array values before reading them
