module CodeGenerate where

import GoatAST
import qualified Data.Map as Map
import Data.List

type Symbol = (String, (BaseType, Int))

symbolTable :: [Param] -> [Decl] -> Map.Map String (BaseType, Int)
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

getStackDepth :: Map.Map String (BaseType, Int) -> Int
getStackDepth table = Map.size table

generateOzCode :: GoatProgram -> String
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

generateStmtCode :: Map.Map String (BaseType, Int) -> Stmt -> String
generateStmtCode table stmt = code where
  code = case stmt of
    Assign lval expr -> code1 ++ code2 where
                            Lvalue (Prim id) = lval
                            (code1, type1) = generateExprCode table 0 expr
                            (type2, slot) = table Map.! id
                            code2 = if type1 == IntType && type2 == FloatType then 
                                      intToReal 0 1 ++ "store " ++ (show slot) ++ "," ++  (getReg 1) ++ "\n"
                                    else 
                                      "store " ++ (show slot) ++ "," ++ (getReg 0) ++ "\n"
    Write expr -> code1 ++ "call_builtin " ++ builtin ++ "\n" where
                            (code1, type1) = generateExprCode table 0 expr
                            builtin = case type1 of 
                                IntType -> "print_int"
                                FloatType -> "print_real"
                                BoolType -> "print_bool"

generateExprCode :: Map.Map String (BaseType, Int) -> Int -> Expr -> (String, BaseType)
generateExprCode table reg expr = case expr of
  IntConst const -> ("int_const " ++ (getReg reg) ++ "," ++ (show const) ++ "\n",
                    IntType)
  FloatConst const -> ("real_const " ++ (getReg reg) ++ "," ++ (show const) ++ "\n",
                    FloatType)
  Var (Prim id) -> ("load " ++ (getReg reg) ++ "," ++ (show slot) ++ "\n",
                    btype) where (btype, slot) = table Map.! id
  
  Add expr1 expr2 -> binaryOpCode table "add" reg expr1 expr2
  Minus expr1 expr2 -> binaryOpCode table "sub" reg expr1 expr2
  Mul expr1 expr2 -> binaryOpCode table "mul" reg expr1 expr2
  Div expr1 expr2 -> binaryOpCode table "div" reg expr1 expr2

intToReal :: Int -> Int -> String
intToReal reg1 reg2 = "int_to_real " ++ (getReg reg1) ++ "," ++ (getReg reg2) ++ "\n"
  
binaryOpCode :: Map.Map String (BaseType, Int) -> String -> Int -> Expr -> Expr -> (String, BaseType)
binaryOpCode table instruction reg expr1 expr2 = (code1 ++ code2 ++ code3, type3) where    -- instruction is add/sub/mul/div
  (code1, type1) = generateExprCode table (reg + 1) expr1
  (code2, type2) = generateExprCode table (reg + 2) expr2
  code3 = if type1 == IntType && type2 == FloatType then
            (intToReal (reg + 3) (reg + 1)) ++ 
            instruction ++ "_real" ++ (getReg reg) ++ "," ++ (getReg (reg + 3)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
          else
            if type1 == FloatType && type2 == IntType then
              (intToReal (reg + 3) (reg + 2)) ++ 
              instruction ++ "_real" ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 3)) ++ "\n"
            else
              if type1 == FloatType then 
                instruction ++ "_real" ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
              else 
                instruction ++ "_int" ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "," ++ (getReg (reg + 2)) ++ "\n"
  type3 = if type1 == IntType && type2 == IntType
          then IntType
          else FloatType