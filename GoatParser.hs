module Main where

import GoatAST
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
  = do
      firstparam <- pParam
      nextparam <- many pNextParam
      return (firstparam:nextparam)

pParam :: Parser Param
pParam
  = do
      paramtype <- do {reserved "val"; return Val} <|> do {reserved "ref"; return Ref}
      datatype <- pDataType
      ident <- identifier
      whiteSpace
      return (Param paramtype datatype ident)

pNextParam :: Parser Param
pNextParam
  = do
      comma
      param <- pParam
      return param
    
pBaseType :: Parser BaseType
pBaseType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }

-- pDataType :: Parser DataType
-- pDataType
--   = pArrayType
--     <|>
--     do { basetype <- pBaseType; return (DTBaseType basetype) }
--     <|>
--     do {reserved "string"; return DTStringType }
pDataType :: Parser DataType
pDataType
  = do 
      basetype <- pBaseType
      do
        do
          shape <- brackets pArrayShape
          return (DTArrayType (ArrayType basetype shape))
        <|>
        return (DTBaseType basetype)
    <|>
    do {reserved "string"; return DTStringType }

-- pArrayType :: Parser DataType   -- todo
-- pArrayType
--   = do 
--       try $ do
--         basetype <- pBaseType
--         shape <- brackets pArrayShape
--         return shape
--       return (DTArrayType basetype shape)
    

pArrayShape :: Parser ArrayShape  -- todo
pArrayShape
  = do
      m <- natural
      do
        do
          comma
          n <- natural
          return (MatrixShape m n)
        <|>
        return (ArrayShape m)
      
      
      
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
      datatype <- pDataType
      ident <- identifier
      whiteSpace
      semi
      return (Decl ident datatype)


      
-----------------------------------------------------------------
--  pStmt is the main parser for statements. It wants to recognise
--  read and write statements, and assignments.
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg :: Parser Stmt

pStmt 
  = choice [pRead, pWrite, pAsg]

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

-----------------------------------------------------------------
--  pExp is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative.
-----------------------------------------------------------------

pExp, pTerm, pFactor, pUminus, pNum, pIdent, pString :: Parser Expr

pExp 
  = pString <|> (chainl1 pTerm pAddOp)
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

pAddOp, pMulOp :: Parser (Expr -> Expr -> Expr)

pAddOp
  = do 
      reservedOp "+"
      return Add

pTerm 
  = chainl1 pFactor pMulOp
    <?>
    "\"term\""

pMulOp
  = do 
      reservedOp "*"
      return Mul

pFactor
  = choice [pUminus, parens pExp, pNum, pIdent]
    <?> 
    "\"factor\""

pUminus
  = do 
      reservedOp "-"
      exp <- pFactor
      return (UnaryMinus exp)

pNum
  = do
      n <- natural <?> ""
      return (IntConst (fromInteger n :: Int))
    <?>
    "number"

pIdent 
  = do
      ident <- identifier
      return (Id ident)
    <?>
    "identifier"

pLvalue :: Parser Lvalue
pLvalue
  = do
      ident <- identifier
      return (LId ident)
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
        ; case output of
            Right ast -> print ast
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

