/**
 * @file Quant language grammar for tree-sitter
 * @author Quant Team
 * @license BSD-2-Clause
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-nocheck

module.exports = grammar({
  name: 'quant',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => choice(
      $.function_definition,
      $.variable_declaration,
      $.const_declaration,
      $.import_statement,
      $.expression_statement,
      $.return_statement,
      $.if_statement,
      $.for_statement,
      $.while_statement,
      $.break_statement,
      $.continue_statement,
      $.block,
    ),

    // Comments
    comment: $ => token(choice(
      seq('//', /.*/),
      seq('#', /.*/),
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/'
      )
    )),

    // Function definition
    function_definition: $ => seq(
      'fn',
      field('name', $.identifier),
      field('parameters', $.parameter_list),
      '->',
      field('return_type', $.type),
      field('body', $.block)
    ),

    parameter_list: $ => seq(
      '(',
      optional(seq(
        $.parameter,
        repeat(seq(',', $.parameter))
      )),
      ')'
    ),

    parameter: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $.type)
    ),

    // Variable declaration
    variable_declaration: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $.type),
      '=',
      field('value', $._expression),
      ';'
    ),

    // Const declaration
    const_declaration: $ => seq(
      'const',
      field('name', $.identifier),
      ':',
      field('type', $.type),
      '=',
      field('value', $._expression),
      ';'
    ),

    // Import statements
    import_statement: $ => choice(
      seq('import', $.module_path, ';'),
      seq(
        'from',
        $.module_path,
        'import',
        $.import_list,
        ';'
      )
    ),

    module_path: $ => sep1($.identifier, '.'),

    import_list: $ => sep1($.identifier, ','),

    // Statements
    expression_statement: $ => seq($._expression, ';'),

    return_statement: $ => seq(
      'return',
      optional($._expression),
      ';'
    ),

    break_statement: $ => seq('break', ';'),

    continue_statement: $ => seq('continue', ';'),

    // If statement
    if_statement: $ => seq(
      'if',
      field('condition', $._expression),
      field('consequence', $.block),
      repeat($.elif_clause),
      optional($.else_clause)
    ),

    elif_clause: $ => seq(
      'elif',
      field('condition', $._expression),
      field('consequence', $.block)
    ),

    else_clause: $ => seq(
      'else',
      field('consequence', $.block)
    ),

    // For statement
    for_statement: $ => seq(
      'for',
      field('initializer', $.variable_declaration),
      field('condition', $._expression),
      ';',
      field('update', $.assignment),
      field('body', $.block)
    ),

    // While statement
    while_statement: $ => seq(
      'while',
      field('condition', $._expression),
      field('body', $.block)
    ),

    // Block
    block: $ => seq(
      '{',
      repeat($._statement),
      '}'
    ),

    // Expressions
    _expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $.call_expression,
      $.index_expression,
      $.identifier,
      $.number,
      $.float,
      $.string,
      $.boolean,
      $.array_literal,
      $.parenthesized_expression,
      $.assignment,
    ),

    binary_expression: $ => choice(
      ...[
        ['||', 1],
        ['&&', 2],
        ['==', 3],
        ['!=', 3],
        ['<', 4],
        ['<=', 4],
        ['>', 4],
        ['>=', 4],
        ['|', 5],
        ['^', 6],
        ['&', 7],
        ['<<', 8],
        ['>>', 8],
        ['+', 9],
        ['-', 9],
        ['*', 10],
        ['/', 10],
        ['%', 10],
      ].map(([operator, precedence]) =>
        prec.left(precedence, seq(
          field('left', $._expression),
          field('operator', operator),
          field('right', $._expression)
        ))
      )
    ),

    unary_expression: $ => prec(11, seq(
      field('operator', choice('!', '-', '~')),
      field('operand', $._expression)
    )),

    assignment: $ => prec.right(0, seq(
      field('left', choice($.identifier, $.index_expression)),
      field('operator', choice('=', '+=', '-=', '*=', '/=', '%=')),
      field('right', $._expression)
    )),

    call_expression: $ => prec(12, seq(
      field('function', $.identifier),
      field('arguments', $.argument_list)
    )),

    argument_list: $ => seq(
      '(',
      optional(seq(
        $._expression,
        repeat(seq(',', $._expression))
      )),
      ')'
    ),

    index_expression: $ => prec(12, seq(
      field('array', $._expression),
      '[',
      field('index', $._expression),
      ']'
    )),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    array_literal: $ => seq(
      '[',
      optional(seq(
        $._expression,
        repeat(seq(',', $._expression))
      )),
      ']'
    ),

    // Types
    type: $ => choice(
      $.primitive_type,
      $.array_type
    ),

    primitive_type: $ => choice('int', 'float', 'bool', 'str', 'void'),

    array_type: $ => seq('[', $.type, ']'),

    // Literals
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    number: $ => token(choice(
      /0[xX][0-9a-fA-F]+/,  // hex
      /0[oO][0-7]+/,         // octal
      /0[bB][01]+/,          // binary
      /[0-9]+/               // decimal
    )),

    float: $ => /[0-9]+\.[0-9]+/,

    string: $ => choice(
      seq('"', repeat(choice(/[^"\\]/, /\\./)), '"'),
      seq("'", repeat(choice(/[^'\\]/, /\\./)), "'")
    ),

    boolean: $ => choice('True', 'False'),
  }
});

/**
 * Creates a rule that matches one or more occurrences of a rule separated by a separator
 */
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
