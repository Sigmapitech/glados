; Keywords
[
  "fn"
  "return"
  "if"
  "elif"
  "else"
  "for"
  "while"
  "break"
  "continue"
  "import"
  "from"
  "const"
] @keyword

; Types
(primitive_type) @type.builtin
(array_type) @type

; Function definitions
(function_definition
  name: (identifier) @function)

; Function calls
(call_expression
  function: (identifier) @function.call)

; Parameters
(parameter
  name: (identifier) @variable.parameter)

; Variables
(variable_declaration
  name: (identifier) @variable)

(const_declaration
  name: (identifier) @constant)

; Operators
[
  "="
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
  "&&"
  "||"
  "!"
  "&"
  "|"
  "^"
  "~"
  "<<"
  ">>"
  "+="
  "-="
  "*="
  "/="
  "%="
] @operator

; Punctuation
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ":"
  ";"
  ","
  "."
] @punctuation.delimiter

"->" @punctuation.special

; Literals
(number) @number
(float) @number.float
(string) @string
(boolean) @boolean

; Comments
(comment) @comment

; Identifiers (fallback)
(identifier) @variable

; Module paths in imports
(module_path
  (identifier) @namespace)
