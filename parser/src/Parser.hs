module Parser
  ( parseExpr,
    parseStmt,
    parseBlock,
    parseDeclFunction,
    parseType,
    parseLiteral,
    TokenParser,
  )
where

import Parser.Decl (parseDeclFunction)
import Parser.Expr (parseExpr)
import Parser.Literal (parseLiteral)
import Parser.Stmt (parseBlock, parseStmt)
import Parser.Type (parseType)
import Parser.Utils (TokenParser)
