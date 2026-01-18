---
title: Getting Started
description: Learn how to write your first Quant program
---

## Introduction

Quant is a statically-typed programming language with C-like syntax. It features a powerful type system, module imports, and modern programming constructs.

## Your First Program

Let's start with a simple "Hello, World!" program:

```quant
from std import print

fn main() -> int {
    print("Hello, World!\n");
    return 0;
}
```

### Breaking It Down

- **`from std import print`**: Imports the `print` function from the standard library
- **`fn main() -> int`**: Declares the main function that returns an integer
- **`print("Hello, World!\n");`**: Prints a message
- **`return 0;`**: Returns 0 to indicate successful execution

## Basic Program Structure

Every Quant program consists of:

1. **Imports** (optional): Import functions or modules you need
2. **Function Definitions**: Define functions with the `fn` keyword
3. **Main Function**: The entry point of your program (unless it's a library)

```quant
import math

from std import print

fn main() -> int {
    // Your code here
    return 0;
}
```

## Import Styles

Quant supports two import styles:

### Module Import

```quant
import math

fn main() -> int {
    x: int = int(math.sqrt(16));
    return 0;
}
```

### Selective Import

```quant
from std import print

fn main() -> int {
    print("Direct function call\n");
    return 0;
}
```

## Comments

Quant supports two types of comments:

```quant
// Single-line comment

/* 
   Multi-line comment
   can span multiple lines
*/

# Alternative single-line comment (Python-style)
```

## Statements and Semicolons

All statements in Quant must end with a semicolon (`;`):

```quant
fn main() -> int {
    x: int = 10;        // Variable declaration
    print("%d\n", x);   // Function call
    return 0;           // Return statement
}
```

## Next Steps

Now that you understand the basics, explore:

- [Language Basics](/guides/language-basics/) - Variables, types, and expressions
- [Types Reference](/reference/types/) - Comprehensive type system documentation
- [Control Flow](/guides/control-flow/) - Conditionals and loops
