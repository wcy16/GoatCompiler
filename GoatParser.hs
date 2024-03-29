{-|
Module      : GoatParser
Description : A parser for Goat written using the Parsec parser combinator 
              library. It depends on GoatAST.hs.
-}

module GoatParser where

import GoatAST
import GoatPrettyPrinter
import Data.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q

-----------------------------------------------------------------
--  Some definitions
-----------------------------------------------------------------


type Parser a
  = Parsec String Int a

lexer :: Q.TokenParser Int
lexer
  = Q.makeTokenParser
    (emptyDef
    { Q.commentLine     = "#"
    , Q.nestedComments  = True
    , Q.identStart      = letter
    , Q.opStart         = oneOf "+-*/|&!=<>:"
    , Q.opLetter        = oneOf "+-*/|&!=<>:"
    , Q.reservedNames   = myReserved
    , Q.reservedOpNames = myOpnames
    })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
-- float      = Q.float lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer
brackets   = Q.brackets lexer
naturalOrFloat = Q.naturalOrFloat lexer

myReserved, myOpnames :: [String]

myReserved
  = ["proc", "main", "begin", "end", "val", "ref",
     "int", "bool", "float", "true", "false", "string",
     "if", "then", "else", "fi",
     "while", "do", "od",
     "read", "write", "call"]

myOpnames 
  = ["+", "-", "*", "/", ":=", "||", "&&", 
     "!", "=", "!=", "<", "<=", ">", ">="]

-----------------------------------------------------------------
--  pProg is the topmost parsing function.
-----------------------------------------------------------------

pProg :: Parser GoatProgram
pProg
  = do
      proc1 <- many (try pProc)
      mainProc <- pMainProc
      proc2 <- many pProc
      return (Program proc1 mainProc proc2)

-----------------------------------------------------------------
--  pMainProc looks for the main procedure, in Goat, only one
--  main procedure is allowed.
-----------------------------------------------------------------

pMainProc :: Parser MainProc
pMainProc
  = do
      reserved "proc"
      reserved "main"
      parens (return ())
      (decls,stmts) <- pProgBody
      return (MainProc decls stmts)

-----------------------------------------------------------------
--  pProc looks for all the procejures except the main procedure
-----------------------------------------------------------------

pProc :: Parser Proc
pProc
  = do
      reserved "proc"
      ident <- identifier
      params <- parens (option [] pParams)
      (decls,stmts) <- pProgBody
      return (Proc ident params decls stmts)

-----------------------------------------------------------------
--  pParams looks for the procedure parameters
-----------------------------------------------------------------

pParams :: Parser [Param]
pParams
  = sepBy pParam comma

pParam :: Parser Param
pParam
  = do
      paramtype 
        <- do {reserved "val"; return Val} <|> do {reserved "ref"; return Ref}
      basetype <- pBaseType
      ident <- identifier
      whiteSpace
      return (Param paramtype basetype ident)

pBaseType :: Parser BaseType
pBaseType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }

pArrayShape :: Parser ArrayShape   -- parse the array shape
pArrayShape
  = do
      m <- natural
      do
        do
          comma
          n <- natural
          return (MatrixShape (fromInteger m :: Int) (fromInteger n :: Int))
        <|>
        return (ArrayShape (fromInteger m :: Int))
      
-----------------------------------------------------------------
--  pProgBody looks for a sequence of declarations followed by a
--  sequence of statements.
-----------------------------------------------------------------

pProgBody :: Parser ([Decl],[Stmt])
pProgBody
  = do
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt
      reserved "end"
      return (decls,stmts)

-----------------------------------------------------------------
--  pDecl looks for a declaration line.
-----------------------------------------------------------------

pDecl :: Parser Decl
pDecl
  = do
      basetype <- pBaseType
      ident <- identifier
      datatype <- do
                    shape <- brackets pArrayShape
                    return (DTArrayType (ArrayType basetype shape))
                  <|>
                  return (DTBaseType basetype)
      whiteSpace
      semi
      return (Decl ident datatype)


      
-----------------------------------------------------------------
--  pStmt is the main parser for statements. It wants to recognise
--  read , write, assign, call, if and while statements.
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg, pCall, pIf, pWhile :: Parser Stmt

pStmt 
  = choice [pRead, pWrite, pAsg, pCall, pIf, pWhile]

pRead
  = do 
      reserved "read"
      lvalue <- pLvalue
      semi
      return (Read lvalue)

pWrite
  = do 
      reserved "write"
      exp <- (pString <|> pExp)
      semi
      return (Write exp)

pAsg
  = do
      lvalue <- pLvalue
      reservedOp ":="
      rvalue <- pExp
      semi
      return (Assign lvalue rvalue)


pIf
  = do
      reserved "if"
      exp <- pExp
      reserved "then"
      stmt1 <- many pStmt
      do                  -- check the if statement structure
        reserved "else"
        stmt2 <- many pStmt
        reserved "fi"
        return (IfElse exp stmt1 stmt2)
        <|>
        do {reserved "fi"; return (If exp stmt1)}

pWhile
  = do
      reserved "while"
      exp <- pExp
      reserved "do"
      stmt <- many pStmt
      reserved "od"
      return (While exp stmt)

pCall
  = do
      reserved "call"
      ident <- identifier
      explist <- parens (sepBy pExp comma)
      semi
      return (Call ident explist)

-----------------------------------------------------------------
--  pExp is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative except the relational operators, which
--  are non-associative.
--
--  pOr, pAnd, pNot ect., are expressions after or, and, not 
--  operations
-----------------------------------------------------------------

pExp, pOr, pAnd, pNot, 
    pRelational, pAddMinus, pFactor, 
    pUminus, pConst, pVarExp, pString :: Parser Expr
pOrOp, pAndOp, pRelationalOp, 
    pAddMinusOp, pMulDivOp :: Parser (Expr -> Expr -> Expr)

pExp 
  = pString
    <|>
    chainl1 pOr pOrOp
    <?>
    "expression"

pString 
  = do
      char '"'
      str <- many (satisfy (/= '"'))
      char '"'
      whiteSpace
      return (StrConst str)
    <?>
    "string"

pOrOp
  = do
      reservedOp "||"
      return Or
    <?>
    "operator"

pOr
  = chainl1 pAnd pAndOp

pAndOp
  = do
      reservedOp "&&"
      return And
    <?>
    "operator"

pAnd
  = do
      reservedOp "!"
      exp <- pNot
      return (Not exp)
      <|>
      pNot

pNot
  = do
      exp1 <- pRelational
      do
        op <- pRelationalOp
        exp2 <- pRelational
        return (op exp1 exp2)
        <|>
        return exp1


pRelationalOp   -- non-associative operators
  = do { reservedOp "="; return Equal }
    <|>
    do { reservedOp "!="; return NotEqual }
    <|>
    do { reservedOp "<="; return LessEqual }
    <|>
    do { reservedOp "<"; return Less }
    <|>
    do { reservedOp ">="; return GreaterEqual }
    <|>
    do { reservedOp ">"; return Greater }
    <?>
    "operator"   -- maybe we should change err msg to "relational op"
                 -- but that will output different from the desired output

pRelational
  = chainl1 pAddMinus pAddMinusOp
      
      
pAddMinusOp
  = do { reservedOp "+"; return Add }
    <|>
    do { reservedOp "-"; return Minus }
    <?>
    "operator"

pAddMinus
 = chainl1 pFactor pMulDivOp

pMulDivOp
  = do { reservedOp "*"; return Mul }
    <|>
    do { reservedOp "/"; return Div }
    <?>
    "operator"

pFactor
 = choice [pUminus, parens pExp, pConst, pVarExp]

pUminus
  = do 
      reservedOp "-"
      exp <- pFactor
      return (UnaryMinus exp)

pConst
  = 
    do { d <- naturalOrFloat;
         case d of 
          Left n -> return (IntConst (fromInteger n :: Int))
          Right n -> return (FloatConst (realToFrac n))}
    <|>
    do { reserved "true"; return (BoolConst True) }
    <|>
    do { reserved "false"; return (BoolConst False) }

pVarExp 
  = do
      var <- pVar
      return (Var var)
    <?>
    "identifier"


-----------------------------------------------------------------
--  pVar parse a variable, it can be just an identifier of a base
--  type or an array type.
-----------------------------------------------------------------

pVar :: Parser Variable
pVar
  = do
      ident <- identifier
      option (Prim ident) $ brackets $ do
        m <- pExp
        option (Arr ident m) $ do
                                  comma
                                  n <- pExp
                                  return (Mat ident m n)


pLvalue :: Parser Lvalue
pLvalue
  = do
      var <- pVar
      return (Lvalue var)
    <?>
    "lvalue"
      
-----------------------------------------------------------------
--  main
-----------------------------------------------------------------

pMain :: Parser GoatProgram
pMain
  = do
      whiteSpace
      p <- pProg
      eof
      return p

-----------------------------------------------------------------
--  parseGoat receive a string representing the Goat program and 
--  will either generate the abstract syntax tree or get parse 
--  error.
-----------------------------------------------------------------

parseGoat :: String -> Either ParseError GoatProgram
parseGoat = runParser pMain 0 ""

