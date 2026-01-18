# Quant Standard Library

The Quant standard library provides essential functionality for common programming tasks.

## Structure

The standard library is organized into the following modules:

```sh
std/
├── io.qa           # Input/Output operations (print, read, file operations)
├── math.qa         # Mathematical functions and constants
├── string.qa       # String manipulation utilities
├── array.qa        # Array/collection operations
├── sys.qa          # System-level operations
└── README.md       # This file
```

## Modules

### `io` - Input/Output

Provides functions for console I/O and file operations.

**Functions:**

- `print(msg: str) -> void` - Print a string to stdout
- `println(msg: str) -> void` - Print a string with newline
- `read() -> str` - Read a line from stdin
- `read_file(path: str) -> str` - Read entire file contents
- `write_file(path: str, content: str) -> bool` - Write content to file

### `math` - Mathematics

Common mathematical functions and constants.

**Constants:**

- `PI: float` - Mathematical constant π (3.14159...)
- `E: float` - Euler's number (2.71828...)

**Functions:**

- `abs(x: int) -> int` / `abs(x: float) -> float` - Absolute value
- `min(a: int, b: int) -> int` - Minimum of two values
- `max(a: int, b: int) -> int` - Maximum of two values
- `pow(base: float, exp: float) -> float` - Power function
- `sqrt(x: float) -> float` - Square root
- `floor(x: float) -> int` - Round down
- `ceil(x: float) -> int` - Round up
- `round(x: float) -> int` - Round to nearest integer

### `string` - String Operations

String manipulation and utilities.

**Functions:**

- `len(s: str) -> int` - String length
- `concat(a: str, b: str) -> str` - Concatenate strings
- `substring(s: str, start: int, end: int) -> str` - Extract substring
- `split(s: str, delim: str) -> [str]` - Split string into array
- `join(arr: [str], delim: str) -> str` - Join array into string
- `trim(s: str) -> str` - Remove leading/trailing whitespace
- `to_upper(s: str) -> str` - Convert to uppercase
- `to_lower(s: str) -> str` - Convert to lowercase
- `contains(s: str, substr: str) -> bool` - Check if string contains substring
- `replace(s: str, old: str, new: str) -> str` - Replace occurrences

### `array` - Array Operations

Utilities for working with arrays.

**Functions:**

- `len(arr: [T]) -> int` - Array length
- `push(arr: [T], item: T) -> void` - Append to array
- `pop(arr: [T]) -> T` - Remove and return last element
- `reverse(arr: [T]) -> [T]` - Reverse array
- `sort(arr: [int]) -> [int]` - Sort array in ascending order
- `map(arr: [T], fn: (T) -> U) -> [U]` - Apply function to each element
- `filter(arr: [T], fn: (T) -> bool) -> [T]` - Filter elements
- `reduce(arr: [T], fn: (T, T) -> T, init: T) -> T` - Reduce array
- `slice(arr: [T], start: int, end: int) -> [T]` - Extract slice

### `sys` - System Operations

System-level functionality.

**Functions:**

- `exit(code: int) -> void` - Exit program with status code
- `args() -> [str]` - Get command-line arguments
- `env(name: str) -> str` - Get environment variable
- `time() -> int` - Current Unix timestamp
- `sleep(ms: int) -> void` - Sleep for milliseconds

## Usage Examples

### Using io module

```quant
from std.io import print, println, read;

fn main() -> int {
    println("What's your name?");
    name: str = read();
    print("Hello, ");
    println(name);
    return 0;
}
```

### Using math module

```quant
from std.math import PI, sqrt, pow;

fn main() -> int {
    radius: float = 5.0;
    area: float = PI * pow(radius, 2.0);
    println("Area: " + area);

    diagonal: float = sqrt(pow(3.0, 2.0) + pow(4.0, 2.0));
    println("Diagonal: " + diagonal);

    return 0;
}
```

### Using string module

```quant
from std.string import len, split, join, to_upper;

fn main() -> int {
    text: str = "hello world";
    words: [str] = split(text, " ");

    upper: str = to_upper(text);
    println(upper);  // "HELLO WORLD"

    joined: str = join(words, "-");
    println(joined);  // "hello-world"

    return 0;
}
```

### Using array module

```quant
from std.array import len, sort, reverse;

fn main() -> int {
    numbers: [int] = [5, 2, 8, 1, 9];

    sorted: [int] = sort(numbers);
    // sorted = [1, 2, 5, 8, 9]

    reversed: [int] = reverse(sorted);
    // reversed = [9, 8, 5, 2, 1]

    println("Length: " + len(numbers));

    return 0;
}
```

## Implementation Status

| Module | Status | Notes |
|--------|--------|-------|
| io     | In Progress | Basic I/O implemented |
| math   | Planned | Not yet started |
| string | Planned | Not yet started |
| array  | Planned | Not yet started |
| sys    | Planned | Not yet started |

## Contributing

To add a new standard library module:

1. Create a new `.qa` file in the `std/` directory
2. Implement the functions following the naming conventions
3. Update this README with documentation
4. Add examples demonstrating usage
5. Write tests in `tests/std/`

## Design Principles

- **Simple and consistent** - Easy to understand and use
- **Type-safe** - Proper type annotations for all functions
- **Well-documented** - Clear documentation and examples
- **Tested** - Comprehensive test coverage
- **Minimal dependencies** - Core functionality only

## Future Additions

Potential future modules:

- `json` - JSON parsing and serialization
- `http` - HTTP client
- `regex` - Regular expressions
- `datetime` - Date and time operations
- `random` - Random number generation
- `crypto` - Cryptographic functions
