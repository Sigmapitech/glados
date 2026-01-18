; Scopes
(function_definition) @local.scope
(block) @local.scope

; Definitions
(function_definition
  name: (identifier) @local.definition.function)

(parameter
  name: (identifier) @local.definition.parameter)

(variable_declaration
  name: (identifier) @local.definition.variable)

(const_declaration
  name: (identifier) @local.definition.constant)

; References
(identifier) @local.reference
