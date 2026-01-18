---
title: Control Flow
description: Learn about conditionals, loops, and control statements in Quant
---

## Conditional Statements

### If Statement

Execute code conditionally based on a boolean expression.

**Syntax:**

```quant
if (condition) {
    // code executed if condition is true
}
```

**Example:**

```quant
fn main() -> int {
    x: int = 10;

    if (x > 5) {
        print("x is greater than 5\n");
    };

    return 0;
}
```

### If-Else Statement

Execute one block if condition is true, another if false.

**Syntax:**

```quant
if (condition) {
    // code executed if condition is true
} else {
    // code executed if condition is false
};
```

**Example:**

```quant
fn main() -> int {
    x: int = 3;

    if (x > 5) {
        print("x is greater than 5\n");
    } else {
        print("x is not greater than 5\n");
    };

    return 0;
}
```

### Else-If Chain

Chain multiple conditions together.

**Syntax:**

```quant
if (condition1) {
    // executed if condition1 is true
} else if (condition2) {
    // executed if condition1 is false and condition2 is true
} else {
    // executed if all conditions are false
};
```

**Example:**

```quant
fn main() -> int {
    score: int = 85;

    if (score >= 90) {
        print("Grade: A\n");
    } else if (score >= 80) {
        print("Grade: B\n");
    } else if (score >= 70) {
        print("Grade: C\n");
    } else {
        print("Grade: F\n");
    };

    return 0;
}
```

### Nested If Statements

If statements can be nested inside each other.

```quant
fn main() -> int {
    x: int = 10;
    y: int = 20;

    if (x > 5) {
        if (y > 15) {
            print("Both conditions are true\n");
        };
    };

    return 0;
}
```

## Loops

### While Loop

Repeat code while a condition is true.

**Syntax:**

```quant
while (condition) {
    // code to repeat
};
```

**Example:**

```quant
fn main() -> int {
    count: int = 0;

    while (count < 5) {
        print("Count: %d\n", count);
        count += 1;
    };

    return 0;
}
```

**Output:**

```quant
Count: 0
Count: 1
Count: 2
Count: 3
Count: 4
```

### For Loop

Loop with initialization, condition, and update in one statement.

**Syntax:**

```quant
for (initialization; condition; update) {
    // code to repeat
}
```

**Example:**

```quant
fn main() -> int {
    for (i: int = 0; i < 5; i += 1) {
        print("i = %d\n", i);
    };
     return 0;
}
```

**Output:**

```quant
i = 0
i = 1
i = 2
i = 3
i = 4
```

### For Loop Components

The for loop has three parts:

1. **Initialization**: Executed once before the loop starts
2. **Condition**: Checked before each iteration
3. **Update**: Executed after each iteration

```quant
for (i: int = 0; i < 10; i++) {
    //   ^           ^        ^
    //   |           |        |
    //   Init        Cond     Update
}
```

### For Loop with Assignment Operators

```quant
fn main() -> int {
    // Using +=
    for (i: int = 0; i < 5; i += 1) {
        print("%d ", i);
    };
    print("\n");

    // Using -= (counting down)
    for (i: int = 5; i > 0; i -= 1) {
        print("%d ", i);
    };
    print("\n");

    return 0;
}
```

### Nested Loops

Loops can be nested inside each other.

```quant
fn main() -> int {
    for (i: int = 0; i < 3; i++) {
        for (j: int = 0; j < 3; j++) {
            print("(%d, %d) ", i, j);
        };
        print("\n");
    };
     return 0;
}
```

**Output:**

```quant
(0, 0) (0, 1) (0, 2)
(1, 0) (1, 1) (1, 2)
(2, 0) (2, 1) (2, 2)
```

## Loop Control Statements

### Break Statement

Exit a loop immediately.

```quant
fn main() -> int {
    for (i: int = 0; i < 10; i++) {
        if (i == 5) {
            break;  // Exit loop when i equals 5
        };
        print("%d ", i);
    };
    print("\n");  // Output: 0 1 2 3 4

    return 0;
}
```

**With while loop:**

```quant
fn main() -> int {
    count: int = 0;

    while (count < 100) {
        print("%d ", count);
        count += 1;

        if (count >= 5) {
            break;  // Exit loop early
        };
    };
    print("\n");  // Output: 0 1 2 3 4

    return 0;
}
```

### Continue Statement

Skip the rest of the current iteration and continue with the next one.

```quant
fn main() -> int {
    for (i: int = 0; i < 10; i++) {
        if (i % 2 == 0) {
            continue;  // Skip even numbers
        };
        print("%d ", i);
    };
    print("\n");  // Output: 1 3 5 7 9

    return 0;
}
```

**With while loop:**

```quant
fn main() -> int {
    count: int = 0;

    while (count < 10) {
        count += 1;

        if (count % 2 == 0) {
            continue;  // Skip even numbers
        };
         print("%d ", count);
    };
    print("\n");  // Output: 1 3 5 7 9

    return 0;
}
```

## Return Statement

Exit a function and optionally return a value.

**Syntax:**

```quant
return;           // Return from void function
return value;     // Return a value
```

**Example:**

```quant
fn find_first_negative(arr: [int], size: int) -> int {
    for (i: int = 0; i < size; i++) {
        if (arr[i] < 0) {
            return i;  // Return index immediately
        };
    };
    return -1;  // Not found
}

fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = -5;
    numbers[3] = 30;

    index: int = find_first_negative(numbers, 4);
    print("First negative at index: %d\n", index);  // Output: 2

    return 0;
}
```

## Block Statements

Curly braces `{}` create a new scope.

```quant
fn main() -> int {
    x: int = 10;

    {
        y: int = 20;  // y only exists in this block
        print("x = %d, y = %d\n", x, y);
    };
     // print("%d", y);  // Error: y not in scope

    return 0;
}
```

## Common Patterns

### Loop with Array

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    numbers[3] = 40;
    numbers[4] = 50;

    for (i: int = 0; i < 5; i++) {
        print("numbers[%d] = %d\n", i, numbers[i]);
    };
     return 0;
}
```

### Finding Maximum

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 50;
    numbers[2] = 30;
    numbers[3] = 90;
    numbers[4] = 20;

    max: int = numbers[0];

    for (i: int = 1; i < 5; i++) {
        if (numbers[i] > max) {
            max = numbers[i];
        };
    };
     print("Maximum: %d\n", max);  // Output: Maximum: 90

    return 0;
}
```

### Counting Elements

```quant
fn main() -> int {
    numbers: [int];
    numbers[0] = 10;
    numbers[1] = 20;
    numbers[2] = 30;
    numbers[3] = 20;
    numbers[4] = 20;

    target: int = 20;
    count: int = 0;

    for (i: int = 0; i < 5; i++) {
        if (numbers[i] == target) {
            count += 1;
        };
    };
     print("Count of %d: %d\n", target, count);  // Output: Count of 20: 3

    return 0;
}
```

### Prime Number Check

```quant
import math

fn is_prime(n: int) -> bool {
    if (n < 2) {
        return False;
    };
     if (n == 2) {
        return True;
    };
     if (n % 2 == 0) {
        return False;
    };
     for (i: int = 3; i < int(math.sqrt(n)) + 1; i += 2) {
        if (n % i == 0) {
            return False;
        };
    };
     return True;
}

fn main() -> int {
    for (i: int = 1; i <= 20; i++) {
        if (is_prime(i)) {
            print("%d ", i);
        };
    };
    print("\n");  // Output: 2 3 5 7 11 13 17 19

    return 0;
}
```

### Nested Loop for 2D Array

```quant
fn main() -> int {
    matrix: [[int]];

    // Initialize
    for (i: int = 0; i < 3; i++) {
        for (j: int = 0; j < 3; j++) {
            matrix[i][j] = i * 3 + j + 1;
        };
    };
     // Print
    for (i: int = 0; i < 3; i++) {
        for (j: int = 0; j < 3; j++) {
            print("%d ", matrix[i][j]);
        };
        print("\n");
    };
     return 0;
}
```

**Output:**

```quant
1 2 3
4 5 6
7 8 9
```
