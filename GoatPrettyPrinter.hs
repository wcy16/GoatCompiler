{-|
Module      : GoatPrettyPrinter
Description : A pretty printer for Goat programming language, generating a
              well-formatted Goat source code from the abstract syntax tree.
              It depends on GoatAST.hs.
Author      : Chunyao Wang
-}

module GoatPrettyPrinter where

import GoatAST

indent :: String
indent = "    "

-- | pretty printer for goat program
prettyprintGoat :: GoatProgram -> String
prettyprintGoat program = pprog where
  Program proc1 mainproc proc2 = program
  MainProc maindecls mainstmts = mainproc
  pprocs = [ppProc proc | proc <- proc1] 
    ++ [ppProc (Proc "main" [] maindecls mainstmts)]
    ++ [ppProc proc | proc <- proc2]
  pprog = ppSeperate "\n\n" pprocs

-- | pretty print the procedure
ppProc :: Proc -> String
ppProc proc = pheader ++ pdecls ++ pbody where
  Proc id params decls stmts = proc
  pheader = "proc " ++ id ++ " (" ++ (ppParams params) ++ ")\n"
  pdecls = foldl (\x y -> x ++ indent ++ y ++ "\n") "" [ppDecl decl | decl <- decls]
  pbody = "begin\n" ++ concat [ppStmt 1 stmt | stmt <- stmts] ++ "end"

-- | pretty print the parameters
ppParams :: [Param] -> String
ppParams [] = []
ppParams (x:[]) = pptype ++ " " ++ pdtype where
  Param paramtype datatype id = x
  pptype = case paramtype of
               Val -> "val"
               Ref -> "ref"
  pdtype = ppDataType datatype id
ppParams (x:xs) = pptype ++ " " ++ pdtype ++ ", " ++ ppParams xs where
  Param paramtype datatype id = x
  pptype = case paramtype of
               Val -> "val"
               Ref -> "ref"
  pdtype = ppDataType datatype id

-- | pretty print the declaration
ppDecl :: Decl -> String
ppDecl decl = (ppDataType datatype id) ++ ";" where
  Decl id datatype = decl
  
ppBaseType :: BaseType -> Ident -> String
ppBaseType basetype id = typename ++ id where
  typename = case basetype of
                  BoolType -> "bool "
                  IntType -> "int "
                  FloatType -> "float "

-- | pretty print the array type, like "[1, 2]"
ppArrayType ::  ArrayType -> Ident -> String
ppArrayType arrtype id = pbasetype ++ "[" ++ pshape ++ "]" where
  ArrayType basetype shape = arrtype
  pbasetype = ppBaseType basetype id
  pshape = case shape of
                ArrayShape m -> show m
                MatrixShape m n -> (show m) ++ ", " ++ (show n)

-- | pretty print the data type
ppDataType :: DataType -> Ident -> String
ppDataType datatype id = case datatype of
                              DTBaseType basetype -> ppBaseType basetype id
                              DTArrayType arraytype -> ppArrayType arraytype id

-- | pretty print the statement and add enough indentations at the beginning
--   of every statement
ppStmt :: Int -> Stmt -> String
ppStmt ind stmt = prefix ++ pstmt where
  prefix = concat $ take ind (repeat indent)
  pstmt = case stmt of
               Assign (Lvalue var) expr -> ppVariable var ++ " := " ++ ppTopLevelExpr expr ++ ";\n"
               Read (Lvalue var) -> "read " ++ ppVariable var ++ ";\n"
               Write expr -> "write " ++ ppTopLevelExpr expr ++ ";\n"
               Call id exprs -> "call " ++ id ++ "(" ++ ppSeperate ", " [ppTopLevelExpr expr | expr <- exprs] ++ ");\n"
               If expr stmts -> "if " ++ ppTopLevelExpr expr ++ " then\n" ++ concat [ppStmt (ind + 1) stmt | stmt <- stmts] ++ prefix ++ "fi\n"
               IfElse expr stmts1 stmts2 ->
                 "if " ++ ppTopLevelExpr expr ++ " then\n" ++ concat [ppStmt (ind + 1) stmt | stmt <- stmts1] ++ prefix ++ "else\n" ++ concat [ppStmt (ind + 1) stmt | stmt <- stmts2] ++ prefix ++ "fi\n"
               While expr stmts -> "while " ++ ppTopLevelExpr expr ++ " do\n" ++ concat [ppStmt (ind + 1) stmt | stmt <- stmts] ++ prefix ++ "od\n"

-- | pretty print the top level expression. 
--   the top level expression do not need a bracket.       
ppTopLevelExpr :: Expr -> String
ppTopLevelExpr expr = pexpr where
  pexpr = case expr of
               Var variable -> ppVariable variable
               BoolConst b -> show b
               IntConst i -> show i
               FloatConst f -> show f
               StrConst s -> "\"" ++ s ++ "\""
               Bracket expr -> ppExpr expr
               Or expr1 expr2 -> ppExpr expr1 ++ " || " ++ ppExpr expr2
               And expr1 expr2 -> ppExpr expr1 ++ " && " ++ ppExpr expr2
               Equal expr1 expr2 -> ppExpr expr1 ++ " = " ++ ppExpr expr2
               NotEqual expr1 expr2 -> ppExpr expr1 ++ " != " ++ ppExpr expr2
               Less expr1 expr2 -> ppExpr expr1 ++ " < " ++ ppExpr expr2
               LessEqual expr1 expr2 -> ppExpr expr1 ++ " <= " ++ ppExpr expr2
               Greater expr1 expr2 -> ppExpr expr1 ++ " > " ++ ppExpr expr2
               GreaterEqual expr1 expr2 -> ppExpr expr1 ++ " >= " ++ ppExpr expr2
               Add expr1 expr2 -> ppExpr expr1 ++ " + " ++ ppExpr expr2
               Minus expr1 expr2 -> ppExpr expr1 ++ " - " ++ ppExpr expr2
               Mul expr1 expr2 -> ppExpr expr1 ++ " * " ++ ppExpr expr2
               Div expr1 expr2 -> ppExpr expr1 ++ " / " ++ ppExpr expr2
               UnaryMinus expr -> "-" ++ ppExpr expr
               Not expr -> "!" ++ ppExpr expr

-- | pretty print the expression.
--   all of the expressions except the variables and constants will be wrapped in a parenthesis.               
ppExpr :: Expr -> String
ppExpr expr = pexpr where
  pexpr = case expr of
               Var variable -> ppVariable variable
               BoolConst b -> show b
               IntConst i -> show i
               FloatConst f -> show f
               StrConst s -> "\"" ++ s ++ "\""
               Bracket expr -> ppExpr expr
               Or expr1 expr2 -> "(" ++ ppExpr expr1 ++ " || " ++ ppExpr expr2 ++ ")"
               And expr1 expr2 -> "(" ++ ppExpr expr1 ++ " && " ++ ppExpr expr2 ++ ")"
               Equal expr1 expr2 -> "(" ++ ppExpr expr1 ++ " = " ++ ppExpr expr2 ++ ")"
               NotEqual expr1 expr2 -> "(" ++ ppExpr expr1 ++ " != " ++ ppExpr expr2 ++ ")"
               Less expr1 expr2 -> "(" ++ ppExpr expr1 ++ " < " ++ ppExpr expr2 ++ ")"
               LessEqual expr1 expr2 -> "(" ++ ppExpr expr1 ++ " <= " ++ ppExpr expr2 ++ ")"
               Greater expr1 expr2 -> "(" ++ ppExpr expr1 ++ " > " ++ ppExpr expr2 ++ ")"
               GreaterEqual expr1 expr2 -> "(" ++ ppExpr expr1 ++ " >= " ++ ppExpr expr2 ++ ")"
               Add expr1 expr2 -> "(" ++ ppExpr expr1 ++ " + " ++ ppExpr expr2 ++ ")"
               Minus expr1 expr2 -> "(" ++ ppExpr expr1 ++ " - " ++ ppExpr expr2 ++ ")"
               Mul expr1 expr2 -> "(" ++ ppExpr expr1 ++ " * " ++ ppExpr expr2 ++ ")"
               Div expr1 expr2 -> "(" ++ ppExpr expr1 ++ " / " ++ ppExpr expr2 ++ ")"
               UnaryMinus expr -> "-" ++ ppExpr expr
               Not expr -> "! " ++ ppExpr expr

-- | pretty print the variables
ppVariable :: Variable -> String
ppVariable var = pvar where
  pvar = case var of
              Prim id -> id
              Arr id expr -> id ++ "[" ++ (ppTopLevelExpr expr) ++ "]"
              Mat id expr1 expr2 -> id ++ "[" ++ (ppTopLevelExpr expr1) ++ ", " ++ (ppTopLevelExpr expr2) ++ "]"      

-- | a helper function to concat given list with a string seperator.
ppSeperate :: String -> [String] -> String
ppSeperate sep [] = []
ppSeperate sep (x:[]) = x
ppSeperate sep (x:xs) = x ++ sep ++ ppSeperate sep xs