module CodeGenerate where

import GoatAST
import qualified Data.Map as Map
import Data.List

type Symbol = (String, (BaseType, Int))

type Table = Map.Map String (BaseType, Int)

type Code = Either String String

appendCode :: Code -> Code -> Code
appendCode code1 code2 = (++) <$> code1 <*> code2

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

getStackDepth :: Table -> Int
getStackDepth table = Map.size table

generateOzCode :: GoatProgram -> Code
generateOzCode goat = oz where
  Program _ mainProc _ = goat   -- todo parse other procedures
  MainProc decls stmts = mainProc
  oz = "call proc_main\n" ++ (generateProcCode "proc_main" [] decls stmts) ++ "halt\n"


generateProcCode :: String -> [Param] -> [Decl] -> [Stmt] -> String
generateProcCode name params decls stmts = name ++ ":\n" ++ pushStack ++ pcodes where
  table = symbolTable params decls
  depth = getStackDepth table
  pushStack = "push_stack_frame " ++ (show depth) ++ "\n"
  codes = map (generateStmtCode table) stmts
  pcodes = intercalate "" codes

generateStmtCode :: Table -> Stmt -> String
generateStmtCode table stmt = code where
  code = case stmt of
    Assign lval expr -> code1 ++ code2 where
                            Lvalue (Prim id) = lval
                            (code1, type1) = generateExprCode table 0 expr
                            (type2, slot) = table Map.! id
                            code2 = if type1 == IntType && type2 == FloatType then 
                                      intToReal 1 0 ++ "store " ++ (show slot) ++ "," ++  (getReg 1) ++ "\n"
                                    else 
                                      "store " ++ (show slot) ++ "," ++ (getReg 0) ++ "\n"
    -- string const is only used in write
    Write (StrConst str) -> "string_const " ++ (getReg 0) ++ ",\"" ++ str ++ "\"\ncall_builtin print_string\n"
    Write expr -> code1 ++ "call_builtin " ++ builtin ++ "\n" where
                            (code1, type1) = generateExprCode table 0 expr
                            builtin = case type1 of 
                                IntType -> "print_int"
                                FloatType -> "print_real"
                                BoolType -> "print_bool"

generateExprCode :: Table -> Int -> Expr -> (String, BaseType)
generateExprCode table reg expr = case expr of
  -- const
  IntConst const -> ("int_const " ++ (getReg reg) ++ "," ++ (show const) ++ "\n",
                    IntType)
  FloatConst const -> ("real_const " ++ (getReg reg) ++ "," ++ (show const) ++ "\n",
                    FloatType)
  -- StrConst const -> ("string_const " ++ (getReg reg) ++ "," ++ const ++ "\n",
  --                   StringType)
  ---- string const is not needed because string is only used in write

  -- variable
  Var (Prim id) -> ("load " ++ (getReg reg) ++ "," ++ (show slot) ++ "\n",
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
                                             then code1 ++ "not " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "\n"
                                             else error "cannot apply NOT op to non-boolean type!"

  Bracket expr1 -> generateExprCode table reg expr1

intToReal :: Int -> Int -> String
intToReal reg1 reg2 = "int_to_real " ++ (getReg reg1) ++ "," ++ (getReg reg2) ++ "\n"
  
binaryOpCode :: Table -> String -> Int -> Expr -> Expr -> (String, BaseType)
binaryOpCode table instruction reg expr1 expr2 = (code1 ++ code2 ++ code3, type3) where    -- instruction is add/sub/mul/div
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
            (intToReal (reg + 3) (reg + 1)) ++ 
            instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 3)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
            , FloatType)
    | type1 == FloatType && type2 == IntType = (
            (intToReal (reg + 3) (reg + 2)) ++ 
            instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 3)) ++ "\n"
            , FloatType)
    | type1 == FloatType && type2 == FloatType = (
            instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
            , FloatType)
    | type1 == IntType && type2 == IntType = (
            instruction ++ "_int " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
            , IntType)
    | type1 == BoolType && type2 == BoolType = (   -- and / or
            instruction ++ " " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
            , BoolType)

