module CodeGenerate where

import GoatAST
import qualified Data.Map as Map
import Data.List

type Symbol = (String, (BaseType, Int))

type Table = Map.Map String (BaseType, Int)

type Code = Either String String

(<++>) :: Code -> Code -> Code
(<++>) code1 code2 = (++) <$> code1 <*> code2

symbolTable :: [Param] -> [Decl] -> Table
symbolTable params decls = table where
    table = Map.fromList(generateSymbolTable 0 params decls)

generateSymbolTable :: Int -> [Param] -> [Decl] -> [Symbol]
generateSymbolTable _ [] [] = []
generateSymbolTable slot [] (decl:decls) = symbol : (generateSymbolTable (slot + 1) [] decls) where
  Decl ident dtype = decl
  DTBaseType btype = dtype   -- todo arrays
  symbol = (ident, (btype, slot))
generateSymbolTable slot (param:params) decls = symbol : (generateSymbolTable (slot + 1) params decls) where
  Param _ dtype ident = param
  DTBaseType btype = dtype   -- todo arrays
  symbol = (ident, (btype, slot))

getReg :: Int -> String
getReg registerNo = "r" ++ (show registerNo)

getLabel :: Int -> String
getLabel label = "label_" ++ (show label)

getStackDepth :: Table -> Int
getStackDepth table = Map.size table

generateOzCode :: GoatProgram -> Code
generateOzCode goat = oz where
  Program _ mainProc _ = goat   -- todo parse other procedures
  MainProc decls stmts = mainProc
  oz = (Right "call proc_main\n") <++> (generateProcCode "proc_main" [] decls stmts) <++> (Right "halt\n")


generateProcCode :: String -> [Param] -> [Decl] -> [Stmt] -> Code
generateProcCode name params decls stmts = header <++> pcodes where
  table = symbolTable params decls
  depth = getStackDepth table
  pushStack = "push_stack_frame " ++ (show depth) ++ "\n"
  header = Right $ name ++ ":\n" ++ pushStack
  -- codes = map (generateStmtCode table) stmts
  -- pcodes = foldl (<++>) (Right "") codes
  -- pcodes = foldl (\(code1, counter) stmt -> do
  --                                             (code2, nextCounter) <- generateStmtCode table counter stmt
  --                                             (code1 <++> code2, nextCounter))
  --                (Right "", 0)
  --                stmts
  (pcodes, _) = generateStmts table 0 stmts

generateStmts :: Table -> Int -> [Stmt] -> (Code, Int)
generateStmts table counter stmts
  = foldl (\(code1, counter) stmt -> do
                  let (code2, nextCounter) = generateStmtCode table counter stmt
                  (code1 <++> code2, nextCounter))
          (Right "", counter)
          stmts

generateStmtCode :: Table -> Int -> Stmt -> (Code, Int)
generateStmtCode table counter stmt = (code, newCter) where
  (code, newCter) = case stmt of
    Assign lval expr -> (code1 <++> code2, counter) where
                            Lvalue (Prim id) = lval
                            (code1, type1) = generateExprCode table 0 expr
                            (type2, slot) = table Map.! id
                            code2
                              | type1 == IntType && type2 == FloatType  = return $ intToReal 1 0 ++ "store " ++ (show slot) ++ "," ++  (getReg 1) ++ "\n"
                              | type1 == type2 = return $ "store " ++ (show slot) ++ "," ++ (getReg 0) ++ "\n"
                              | otherwise = Left "parameters not match"

    Read lval -> (code1, counter) where
                    Lvalue (Prim id) = lval
                    (type1, slot) = table Map.! id
                    code1 = Right ("call_builtin read_" ++ type1Code ++ load) where
                      type1Code = case type1 of
                                    BoolType -> "bool"
                                    IntType -> "int"
                                    FloatType -> "real"
                      load = "\nstore " ++ (show slot) ++ ",r0\n"

    -- string const is only used in write
    Write (StrConst str) -> (Right $ "string_const " ++ (getReg 0) ++ ",\"" ++ str ++ "\"\ncall_builtin print_string\n"
                             , counter)
    Write expr -> (code1 <++> (Right $ "call_builtin " ++ builtin ++ "\n"), counter) where
                                (code1, type1) = generateExprCode table 0 expr
                                builtin = case type1 of 
                                    IntType -> "print_int"
                                    FloatType -> "print_real"
                                    BoolType -> "print_bool"
                   
    -- if then                                
    If expr stmts -> (codeif, labels) where
                        (code1, type1) = generateExprCode table 0 expr
                        (codeif, labels) =  if type1 /= BoolType then (Left "Not a Boolean type in IF", 0)
                                            else (code2, cter) where
                                                    label = getLabel counter
                                                    -- if
                                                    code3 = Right $ "branch_on_false " ++ (getReg 0) ++ "," ++ label ++ "\n"
                                                    -- if body
                                                    (code4, cter) = generateStmts table (counter + 1) stmts
                                                    code5 = Right $ label ++ ":\n"
                                                    code2 = code1 <++> code3 <++> code4 <++> code5
    
    -- if else
    IfElse expr stmts1 stmts2 -> (codeif, labels) where
                                    (code1, type1) = generateExprCode table 0 expr
                                    (codeif, labels) =  if type1 /= BoolType then (Left "Not a Boolean type in IF", 0)
                                                        else (code2, cter2) where   
                                                          labelElse = getLabel counter
                                                          -- if
                                                          code3 = Right $ "branch_on_false " ++ (getReg 0) ++ "," ++ labelElse ++ "\n"
                                                          -- then
                                                          (code4, cter) = generateStmts table (counter + 1) stmts1
                                                          labelFi = getLabel cter
                                                          code5 = Right $ "branch_uncond " ++ labelFi ++ "\n"
                                                          -- else
                                                          code6 = Right $ labelElse ++ ":\n"
                                                          (code7, cter2) = generateStmts table (cter + 1) stmts2
                                                          code8 = Right $ labelFi ++ ":\n"
                                                          -- code
                                                          code2 = code1 <++> code3 <++> code4 <++> code5 <++> code6 <++> code7 <++> code8

    -- while
    While expr stmts -> (codewhile, labels) where
                            (code1, type1) = generateExprCode table 0 expr
                            (codewhile, labels) = if type1 /= BoolType then (Left "Not a Boolean type in WHILE", 0)
                                                  else (code2, cter) where  
                                                    labelWhile = getLabel counter
                                                    -- jump to condition
                                                    code3 = Right $ "branch_uncond " ++ labelWhile ++ "\n"
                                                    -- label loop body
                                                    labelBody = getLabel $ counter + 1
                                                    code4 = Right $ labelBody ++ ":\n"
                                                    (code5, cter) = generateStmts table (counter + 2) stmts     
                                                    -- condition
                                                    code6 = Right $ labelWhile ++ ":\n"
                                                    -- code1
                                                    code7 = Right $ "branch_on_true " ++ (getReg 0) ++ "," ++ labelBody ++ "\n"        
                                                    -- code
                                                    code2 = code3 <++> code4 <++> code5 <++> code6 <++> code1 <++> code7                             
                                          
                          
generateExprCode :: Table -> Int -> Expr -> (Code, BaseType)
generateExprCode table reg expr = case expr of
  -- const
  IntConst const -> (Right ("int_const " ++ (getReg reg) ++ "," ++ (show const) ++ "\n"),
                    IntType)
  FloatConst const -> (Right ("real_const " ++ (getReg reg) ++ "," ++ (show const) ++ "\n"),
                    FloatType)
  -- StrConst const -> ("string_const " ++ (getReg reg) ++ "," ++ const ++ "\n",
  --                   StringType)
  ---- string const is not needed because string is only used in write

  -- variable
  Var (Prim id) -> (Right("load " ++ (getReg reg) ++ "," ++ (show slot) ++ "\n"),
                    btype) where (btype, slot) = table Map.! id
  
  -- binary arithmetic ops
  Add expr1 expr2 -> binaryOpCode table "add" reg expr1 expr2
  Minus expr1 expr2 -> binaryOpCode table "sub" reg expr1 expr2
  Mul expr1 expr2 -> binaryOpCode table "mul" reg expr1 expr2
  Div expr1 expr2 -> binaryOpCode table "div" reg expr1 expr2

  -- binary logic ops
  And expr1 expr2 -> binaryOpCode table "and" reg expr1 expr2
  Or expr1 expr2 -> binaryOpCode table "or" reg expr1 expr2

  -- binary compare ops
  Equal expr1 expr2 -> (code, BoolType) where (code, _) = binaryOpCode table "cmp_eq" reg expr1 expr2
  NotEqual expr1 expr2 -> (code, BoolType) where (code, _) = binaryOpCode table "cmp_ne" reg expr1 expr2
  Greater expr1 expr2 -> (code, BoolType) where (code, _) = binaryOpCode table "cmp_gt" reg expr1 expr2
  GreaterEqual expr1 expr2 -> (code, BoolType) where (code, _) = binaryOpCode table "cmp_ge" reg expr1 expr2
  Less expr1 expr2 -> (code, BoolType) where (code, _) = binaryOpCode table "cmp_lt" reg expr1 expr2
  LessEqual expr1 expr2 -> (code, BoolType) where (code, _) = binaryOpCode table "cmp_le" reg expr1 expr2

  Not expr -> (code, BoolType) where (code1, type1) = generateExprCode table (reg + 1) expr
                                     code = if type1 == BoolType
                                             then code1 <++> Right ("not " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "\n")
                                             else Left "cannot apply NOT op to non-boolean type!"

  Bracket expr1 -> generateExprCode table reg expr1

intToReal :: Int -> Int -> String
intToReal reg1 reg2 = "int_to_real " ++ (getReg reg1) ++ "," ++ (getReg reg2) ++ "\n"
  
binaryOpCode :: Table -> String -> Int -> Expr -> Expr -> (Code, BaseType)
binaryOpCode table instruction reg expr1 expr2 = (code1 <++> code2 <++> code3, type3) where    -- instruction is add/sub/mul/div
  (code1, type1) = generateExprCode table (reg + 1) expr1
  (code2, type2) = generateExprCode table (reg + 2) expr2
  -- code3 = if type1 == IntType && type2 == FloatType then
  --           (intToReal (reg + 3) (reg + 1)) ++ 
  --           instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 3)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
  --         else
  --           if type1 == FloatType && type2 == IntType then
  --             (intToReal (reg + 3) (reg + 2)) ++ 
  --             instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 3)) ++ "\n"
  --           else
  --             if type1 == FloatType then 
  --               instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
  --             else 
  --               instruction ++ "_int " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
  -- type3 = if type1 == IntType && type2 == IntType
  --         then IntType
  --         else FloatType
  (code3, type3)
    | type1 == IntType && type2 == FloatType = (
            Right ((intToReal (reg + 3) (reg + 1)) ++ 
                    instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 3)) ++ "," ++ (getReg (reg + 2)) ++ "\n")
            , FloatType)
    | type1 == FloatType && type2 == IntType = (
            Right ((intToReal (reg + 3) (reg + 2)) ++ 
                    instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 3)) ++ "\n")
            , FloatType)
    | type1 == FloatType && type2 == FloatType = (
            Right (instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n")
            , FloatType)
    | type1 == IntType && type2 == IntType = (
            Right (instruction ++ "_int " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n")
            , IntType)
    | type1 == BoolType && type2 == BoolType = (   -- and / or
            Right (instruction ++ " " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n")
            , BoolType)
    | otherwise = (Left "error binary op", type1)
