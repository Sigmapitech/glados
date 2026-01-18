((function_definition) @fold
  (#trim! @fold))

((block) @fold
  (#trim! @fold))

((comment) @fold
  (#match? @fold "^/\\*"))
