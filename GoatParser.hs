module Main where

import GoatAST
import GoatPrettyPrinter
import Data.Char
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

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
float      = Q.float lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer
brackets   = Q.brackets lexer

myReserved, myOpnames :: [String]

myReserved
  = ["proc", "main", "begin", "end", "val", "ref",
     "int", "bool", "float", "true", "false", "string",
     "if", "then", "else", "fi",
     "while", "do", "od",
     "read", "write"]

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
      return (Program mainProc (proc1 ++ proc2))

-----------------------------------------------------------------
--  pMainProc looks for the main procedure
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
      paramtype <- do {reserved "val"; return Val} <|> do {reserved "ref"; return Ref}
      basetype <- pBaseType
      ident <- identifier
      datatype <- do
                    shape <- brackets pArrayShape
                    return (DTArrayType (ArrayType basetype shape))
                  <|>
                  return (DTBaseType basetype)
      whiteSpace
      return (Param paramtype datatype ident)
    
pBaseType :: Parser BaseType
pBaseType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }

pArrayShape :: Parser ArrayShape  -- todo
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
--  read and write statements, and assignments.
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
      do
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
      return (Call ident explist)

-----------------------------------------------------------------
--  pExp is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative.
-----------------------------------------------------------------

pExp, pUminus, pConst, pVarExp, pString :: Parser Expr

pExp 
  = pString
    <|>
    pConst
    <|>
    parens pExp
    <|>
    (chainl1 pOr pOrOp)
    <?>
    "expression"

pString 
  = do
      char '"'
      str <- many (satisfy (/= '"'))
      char '"'
      return (StrConst str)
    <?>
    "string"

pOrOp, pAndOp, pRelationalOp, pAddMinusOp, pMulDivOp :: Parser (Expr -> Expr -> Expr)
pOr, pAnd, pNot, pRelational, pAddMinus, pMulDiv :: Parser Expr

pOrOp
  = do
      reservedOp "||"
      return Or

pOr
  = chainl1 pAnd pAndOp

pAndOp
  = do
      reservedOp "&&"
      return And

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


pRelationalOp
  = do { reservedOp "="; return Equal }
    <|>
    do { reserved "!="; return NotEqual }
    <|>
    do { reserved "<="; return LessEqual }
    <|>
    do { reserved "<"; return Less }
    <|>
    do { reserved ">="; return GreaterEqual }
    <|>
    do { reserved ">"; return Greater }

pRelational
  = chainl1 pAddMinus pAddMinusOp
      
      
pAddMinusOp
  = do { reservedOp "+"; return Add }
    <|>
    do { reserved "-"; return Minus }

pAddMinus
 = chainl1 pMulDiv pMulDivOp

pMulDivOp
  = do { reservedOp "*"; return Mul }
    <|>
    do { reserved "/"; return Div }

pMulDiv
 = choice [pUminus, parens pExp, pConst, pVarExp]

pUminus
  = do 
      reservedOp "-"
      exp <- pMulDiv
      return (UnaryMinus exp)

pConst
  = do { n <- natural; return (IntConst (fromInteger n :: Int)) }
    <|>
    do { d <- float; return (FloatConst (realToFrac d)) }
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


pLvalue :: Parser Lvalue   ---todo
pLvalue
  = do
      var <- pVar
      return (Lvalue var)
    <?>
    "lvalue"
      
-----------------------------------------------------------------
-- main
-----------------------------------------------------------------

pMain :: Parser GoatProgram
pMain
  = do
      whiteSpace
      p <- pProg
      eof
      return p

main :: IO ()
main
  = do { progname <- getProgName
        ; args <- getArgs
        ; checkArgs progname args
        ; input <- readFile (head args)
        ; let output = runParser pMain 0 "" input
        ; let command = head (tail args)
        ; case output of
            Right ast -> putStr $ ppProg ast
            --Right ast -> print ast
            Left  err -> do { putStr "Parse error at "
                            ; print err
                            }
        }

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
    = return ()
checkArgs progname _
    = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
        ; exitWith (ExitFailure 1)
        }

