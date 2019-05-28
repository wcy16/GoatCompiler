{-|
Module      : CodeGenerate
Description : Generate codes for Oz 
Author      : Chunyao Wang
-}

module CodeGenerate where

import GoatAST
import qualified Data.Map as Map
import Data.List

-- | symbol stored in symbol table
--       id : (basetype, slot_number)
type Symbol = (String, (BaseType, Int))

-- | symbol table
type Table = Map.Map String (BaseType, Int)

-- | environment table store all of the functions and its parameters
type Env = Map.Map String [Param]

-- | define a Code type to be either error_message or oz_code
type Code = Either String String

-- | put 2 code together
(<++>) :: Code -> Code -> Code
(<++>) code1 code2 = (++) <$> code1 <*> code2

-- | function table for goat
environment :: GoatProgram -> Env
environment program = env where
  Program proc1s mainProc proc2s = program
  env = Map.fromList([(func_name, params) | Proc func_name params _ _ <- (proc1s ++ proc2s)])

-- | symbol table for goat
--   when we run this method, it will generate one more entry for the table. The last entry
--   is to keep an recod of how many stack slots used. we discard that last entry in our
--   code generate function
symbolTable :: [Param] -> [Decl] -> (Table, Int)
symbolTable params decls = (table, depth) where
    tableWithEnd = generateSymbolTable 0 params decls
    (_, (_, depth)) = last tableWithEnd
    table = Map.fromList(init tableWithEnd)

-- | generate symbol table.
--   if a varable is a matrix:
--    we treat matrix an array of array, so a matrix of shape (x, y) takes (x * y + x) frames
--    the first x frame store addresses for the rest x arrays of length y
--   if a variable is a reference:
--    we store 2 symbols for that variable, the first one is the address of that variable,
--    the second one is the value of that variable. The address's id is prepended with a '&'
--    since the '&' char is not a valid identifier.
generateSymbolTable :: Int -> [Param] -> [Decl] -> [Symbol]
generateSymbolTable slot [] [] = [("end", (BoolType, slot))]
generateSymbolTable slot [] (decl:decls) = symbol : (generateSymbolTable nextSlot [] decls) where  --for decls
  Decl ident dtype = decl
  (symbol, nextSlot) = case dtype of
    DTBaseType btype -> ((ident, (btype, slot)), slot + 1)   -- base type
    DTArrayType (ArrayType btype shape) -> ((ident, (btype, slot)), slot + getArrayOffset shape)  -- array type
generateSymbolTable slot (param:params) decls = symbols where   -- for params
  Param ptype btype ident = param
  symbols = if ptype == Val then (ident, (btype, slot)) : (generateSymbolTable (slot + 1) params decls)
            else -- & is not a valid var id, prepend varid with a & means it is a reference
             ('&' : ident, (btype, slot)) : (ident, (btype, slot + 1)) : (generateSymbolTable (slot + 2) params decls)

-- | calculate how many stack frames is required
getArrayOffset :: ArrayShape -> Int
getArrayOffset (ArrayShape x) = x
getArrayOffset (MatrixShape x y) = x * y + x   -- matrix is array of array

-- | helper function to get register name by giving register number
getReg :: Int -> String
getReg registerNo = "r" ++ (show registerNo)

-- | helper function to get label name by giving label number
getLabel :: String -> Int -> String
getLabel funcid label = funcid ++ "_label_" ++ (show label)

-- | generate goat code by given a goat AST
generateOzCode :: GoatProgram -> Code
generateOzCode goat = oz where
  env = environment goat
  Program proc1s mainProc proc2s = goat
  MainProc decls stmts = mainProc
  mainCode = generateProcCode env "proc_main" [] decls stmts
  procCode = foldl (<++>) mainCode [generateProcCode env id params decls stmts | Proc id params decls stmts <- proc1s ++ proc2s]
  oz = (Right "call proc_main\nhalt\n") <++> procCode
  -- oz = (Right "call proc_main\n") <++> (generateProcCode "proc_main" [] decls stmts) <++> (Right "halt\n")

-- | the first step of running a procedure is to store all of the parameters to stack
storeParam :: Table -> Int -> Int -> Param -> Code
storeParam table depth regNo param = code where
              Param ptype btype id = param
              (_, slot) = table Map.! id
              code = (if ptype == Val then
                        Right $ "store " ++ (show slot) ++ "," ++ (getReg regNo) ++ "\n"
                      else do
                            let (_, addr) = table Map.! ('&' : id)
                            let code2 = "load_indirect " ++ (getReg depth) ++ "," ++ (getReg regNo) ++ "\n"
                            let code3 = "store " ++ (show slot) ++ "," ++ (getReg depth) ++ "\n"
                            let code4 = "store " ++ (show addr) ++ "," ++ (getReg regNo) ++ "\n"
                            return (code2 ++ code3 ++ code4)
                      )

-- | when a procedure is about to return, it will try to modify the actual value for the reference type
returnReference :: Table -> Param -> Code
returnReference table param = Right code where
  Param ptype dtype id = param
  (_, slot) = table Map.! id
  (_, addr) = table Map.! ('&' : id)
  -- load value
  code1 = "load " ++ (getReg 0) ++ "," ++ (show slot) ++ "\n"
  -- load addr
  code2 = "load " ++ (getReg 1) ++ "," ++ (show addr) ++ "\n"
  -- store val
  code3 = "store_indirect " ++ (getReg 1) ++ "," ++ (getReg 0) ++ "\n"
  code = code1 ++ code2 ++ code3

-----------------------------------------------------------------------------

-- | helper function to initialize matrix
--   we use reg2 because reg0 and reg1 is stored
initMatrixN :: Table -> BaseType -> Int -> Int -> Int -> Int -> Code
initMatrixN table btype slot x y n = code where  -- n = 0..x-1
  -- address of matrix[n][0]
  code1 = Right $ "load_address " ++ (getReg 2) ++ "," ++ (show $ slot + x + n * y) ++ "\n"
  -- init matrix[n][0..y]
  code2 = initArr table btype (slot + x + n * y) y 0
  -- init matrix header
  code3 = Right $ "store " ++ (show $ slot + n) ++ "," ++ (getReg 2) ++ "\n"
  -- combine
  code = code1 <++> code2 <++> code3

-- | we initialize matrix(x, y) to make the first x frames store the correct address
initMatrix :: Table -> String -> Int -> Int -> Code
initMatrix table id x y = code where
  (btype, slot) = table Map.! id
  code = foldl (<++>) (Right "") [initMatrixN table btype slot x y n | n <- [0..(x-1)]]

-- | initialize declarations to default value
--   reg0 int/bool 0
--   reg1 float 0.0
initDecl :: Table -> Decl -> Code
initDecl table decl = code where
  Decl id dtype = decl
  (_, slot) = table Map.! id
  code = case dtype of
    DTBaseType btype -> do
                          let code = if btype == FloatType then
                                        "store " ++ (show slot) ++ "," ++ (getReg 1) ++ "\n"
                                      else
                                        "store " ++ (show slot) ++ "," ++ (getReg 0) ++ "\n"
                          return code
    DTArrayType (ArrayType btype shape) -> case shape of
                                                  ArrayShape x -> initArr table btype slot x 0
                                                  MatrixShape x y -> initMatrix table id x y

-- | initialize arrays
initArr :: Table -> BaseType -> Int -> Int -> Int -> Code
initArr table btype slot x n = code where
  code1
    | btype == BoolType || btype == IntType = "store " ++ (show $ slot + n) ++ "," ++ (getReg 0) ++ "\n"
    | otherwise = "store " ++ (show $ slot + n) ++ "," ++ (getReg 1) ++ "\n"
  code = if x == n + 1 then
          Right code1
         else
          (Right code1) <++> (initArr table btype slot x (n + 1))



-- | generate procedure code
--   A procedure contain following part:
--      1. label
--      2. push stack frame
--      3. store parameters to stack
--      4. initialize vars, arrays and matrices
--      5. procedure body
--      6. store reference values
--      7. pop stack frame and return
generateProcCode :: Env -> String -> [Param] -> [Decl] -> [Stmt] -> Code
generateProcCode env name params decls stmts = header <++> stores <++> inits <++> pcodes <++> returns where
  (table, depth) = symbolTable params decls   -- generate symbol table
  pushStack = "push_stack_frame " ++ (show depth) ++ "\n"   -- the frames used
  popStack = "pop_stack_frame " ++ (show depth) ++ "\n"     -- after procedure finished, we pop the frames
  header = Right $ name ++ ":\n" ++ pushStack
  -- store params to stack
  stores = foldl (<++>) (Right "") $ zipWith (storeParam table depth) [0..depth] params
  -- init decls
  initIntRegs = "int_const " ++ (getReg 0) ++ ",0\n"
  initRealRegs = "real_const " ++ (getReg 1) ++ ",0.0\n"
  inits = foldl (<++>) (Right $ initIntRegs ++ initRealRegs) $ map (initDecl table) decls
  -- -- init matrix decls
  -- inits = foldl (<++>) (Right "") [initMatrix table id x y | Decl id (DTArrayType (ArrayType btype (MatrixShape x y))) <- decls]
  -- procedure body
  (pcodes, _) = generateStmts name env table 0 stmts
  -- after execute procedure we need to return
  returns = foldr (<++>) (Right (popStack ++ "return\n")) $ map (returnReference table) $ filter (\(Param ptype _ _) -> ptype == Ref ) params

-- | generate codes for statements
generateStmts :: String -> Env -> Table -> Int -> [Stmt] -> (Code, Int)
generateStmts funcid env table counter stmts
  = foldl (\(code1, counter) stmt -> do
                  let (code2, nextCounter) = generateStmtCode funcid env table counter stmt
                  (code1 <++> code2, nextCounter))
          (Right "", counter)
          stmts

-- | generate codes for a single statement
generateStmtCode :: String -> Env -> Table -> Int -> Stmt -> (Code, Int)
generateStmtCode name env table counter stmt = (code, newCter) where
  (code, newCter) = case stmt of
    -- assign values
    Assign (Lvalue (Prim id)) expr -> (code1 <++> code2, counter) where
                            (code1, type1) = generateExprCode table 0 expr
                            (type2, slot) = table Map.! id
                            code2
                              | type1 == IntType && type2 == FloatType  = return $ intToReal 1 0 ++ "store " ++ (show slot) ++ "," ++  (getReg 1) ++ "\n"
                              | type1 == type2 = return $ "store " ++ (show slot) ++ "," ++ (getReg 0) ++ "\n"
                              | otherwise = Left "parameters not match"
    -- assign array
    Assign (Lvalue (Arr id expr1)) expr2 -> (code1 <++> code2 <++> code3, counter) where
                      -- find array addr in reg0
                      (code1, type1) = getArrayAddr table 0 (Arr id expr1)
                      -- calculate expr2 in reg1
                      (code2, type2) = generateExprCode table 1 expr2
                      code3
                        | type1 == FloatType && type2 == IntType = return $ intToReal 2 1 ++ "store_indirect " ++ (getReg 0) ++ "," ++ (getReg 2) ++ "\n"
                        | type1 == type2 = return $ "store_indirect " ++ (getReg 0) ++ "," ++ (getReg 1) ++ "\n"
                        | otherwise = Left "parameters not match"
    -- assign matrix
    Assign (Lvalue (Mat id expr1 expr2)) expr3 -> (code1 <++> code2 <++> code3, counter) where
                      -- find array addr in reg0
                      (code1, type1) = getArrayAddr table 0 (Mat id expr1 expr2)
                      -- calculate expr2 in reg1
                      (code2, type2) = generateExprCode table 1 expr3
                      code3
                        | type1 == FloatType && type2 == IntType = return $ intToReal 2 1 ++ "store_indirect " ++ (getReg 0) ++ "," ++ (getReg 2) ++ "\n"
                        | type1 == type2 = return $ "store_indirect " ++ (getReg 0) ++ "," ++ (getReg 1) ++ "\n"
                        | otherwise = Left "parameters not match"

    -- read values  
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
                                                    label = getLabel name counter
                                                    -- if
                                                    code3 = Right $ "branch_on_false " ++ (getReg 0) ++ "," ++ label ++ "\n"
                                                    -- if body
                                                    (code4, cter) = generateStmts name env table (counter + 1) stmts
                                                    code5 = Right $ label ++ ":\n"
                                                    code2 = code1 <++> code3 <++> code4 <++> code5
    
    -- if else
    IfElse expr stmts1 stmts2 -> (codeif, labels) where
                                    (code1, type1) = generateExprCode table 0 expr
                                    (codeif, labels) =  if type1 /= BoolType then (Left "Not a Boolean type in IF", 0)
                                                        else (code2, cter2) where   
                                                          labelElse = getLabel name counter
                                                          -- if
                                                          code3 = Right $ "branch_on_false " ++ (getReg 0) ++ "," ++ labelElse ++ "\n"
                                                          -- then
                                                          (code4, cter) = generateStmts name env table (counter + 1) stmts1
                                                          labelFi = getLabel name cter
                                                          code5 = Right $ "branch_uncond " ++ labelFi ++ "\n"
                                                          -- else
                                                          code6 = Right $ labelElse ++ ":\n"
                                                          (code7, cter2) = generateStmts name env table (cter + 1) stmts2
                                                          code8 = Right $ labelFi ++ ":\n"
                                                          -- code
                                                          code2 = code1 <++> code3 <++> code4 <++> code5 <++> code6 <++> code7 <++> code8

    -- while
    While expr stmts -> (codewhile, labels) where
                            (code1, type1) = generateExprCode table 0 expr
                            (codewhile, labels) = if type1 /= BoolType then (Left "Not a Boolean type in WHILE", 0)
                                                  else (code2, cter) where  
                                                    labelWhile = getLabel name counter
                                                    -- jump to condition
                                                    code3 = Right $ "branch_uncond " ++ labelWhile ++ "\n"
                                                    -- label loop body
                                                    labelBody = getLabel name $ counter + 1
                                                    code4 = Right $ labelBody ++ ":\n"
                                                    (code5, cter) = generateStmts name env table (counter + 2) stmts     
                                                    -- condition
                                                    code6 = Right $ labelWhile ++ ":\n"
                                                    -- code1
                                                    code7 = Right $ "branch_on_true " ++ (getReg 0) ++ "," ++ labelBody ++ "\n"        
                                                    -- code
                                                    code2 = code3 <++> code4 <++> code5 <++> code6 <++> code1 <++> code7
                                                    
    -- call func
    Call funcid exprs -> (codeCall, counter) where
                          codeCall = do
                                      let exprCnt = length(exprs)
                                      let params = env Map.! funcid
                                      if exprCnt /= length(params) then
                                        return "Not correct parameters count"
                                      else do
                                            -- store exprs into reg
                                            let codes = zipWith3 (generateParamCode table) [0..exprCnt] exprs params
                                            foldr (<++>) (return ("call " ++ funcid ++ "\n")) codes
                                            -- -- type check
                                            -- let typeCheck = foldl (&&) True $ zipWith (\(Param _ btype1 _) (_, btype2) -> btype1 == btype2) params prep
                                            -- if not typeCheck then
                                            --   Left "parameter type check fail"
                                            -- else
                                            --   foldr (<++>) (return ("call " ++ funcid ++ "\n")) [ code | (code, _) <- prep]

-- | load parameters to registers when perform a call function
generateParamCode :: Table -> Int -> Expr -> Param -> Code
generateParamCode table reg expr param = code where
  Param ptype btype id = param
  code = if ptype == Val then 
          do
            let (exprCode, exprType) = generateExprCode table reg expr
            if exprType /= btype then
              Left "parameter type check fail"
            else
              exprCode
         else case expr of
                Var (Prim id) -> do
                                  let (vartype, slot) = table Map.! id
                                  if vartype /= btype then   -- parameter not match
                                    Left "parameter type check fail"
                                  else
                                    Right $ "load_address " ++ (getReg reg) ++ "," ++ (show slot) ++ "\n"
                _ -> Left "cannot parse reference type"
                
-- | generate expression codes, the result is stored in a given register
--   return the code and the type of the value                         
generateExprCode :: Table -> Int -> Expr -> (Code, BaseType)
generateExprCode table reg expr = case expr of
  -- const
  IntConst const -> (Right ("int_const " ++ (getReg reg) ++ "," ++ (show const) ++ "\n"),
                    IntType)
  FloatConst const -> (Right ("real_const " ++ (getReg reg) ++ "," ++ (show const) ++ "\n"),
                    FloatType)
  BoolConst const -> (Right ("int_const " ++ (getReg reg) ++ "," ++ (show $ boolToInt const) ++ "\n"), BoolType)
  -- StrConst const -> ("string_const " ++ (getReg reg) ++ "," ++ const ++ "\n",
  --                   StringType)
  ---- string const is not needed because string is only used in write

  -- variable
  Var (Prim id) -> (Right("load " ++ (getReg reg) ++ "," ++ (show slot) ++ "\n"),
                    btype) where (btype, slot) = table Map.! id
  Var (Arr id expr1) -> (code, btype) where
                        (code1, btype) = getArrayAddr table (reg + 1) (Arr id expr1)
                        code2 = Right $ "load_indirect " ++ (getReg reg) ++ "," ++ (getReg $ reg + 1) ++ "\n"
                        code = code1 <++> code2
  Var (Mat id expr1 expr2) -> (code, btype) where
                              (code1, btype) = getArrayAddr table (reg + 1) (Mat id expr1 expr2)
                              code2 = Right $ "load_indirect " ++ (getReg reg) ++ "," ++ (getReg $ reg + 1) ++ "\n"
                              code = code1 <++> code2
  
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
  
  -- unary ops
  Not expr -> (code, BoolType) where (code1, type1) = generateExprCode table (reg + 1) expr
                                     code = if type1 == BoolType
                                             then code1 <++> Right ("not " ++ (getReg reg) ++ "," ++ (getReg (reg + 1)) ++ "\n")
                                             else Left "cannot apply NOT op to non-boolean type!"
  UnaryMinus expr -> (code1 <++> code2, type1) where
                                    (code1, type1) = generateExprCode table (reg + 1) expr      
                                    code2
                                      | type1 == IntType = Right $ "neg_int " ++ (getReg reg) ++ "," ++ (getReg $ reg + 1) ++ "\n"
                                      | type1 == FloatType = Right $ "neg_real " ++ (getReg reg) ++ "," ++ (getReg $ reg + 1) ++ "\n"
                                      | otherwise = Left "UnaryMinus to a non int/float type"
  
  -- brackets
  Bracket expr1 -> generateExprCode table reg expr1

-- | helper function to generate int_to_real code
intToReal :: Int -> Int -> String
intToReal reg1 reg2 = "int_to_real " ++ (getReg reg1) ++ "," ++ (getReg reg2) ++ "\n"

-- | helper function to generate int value for bool type
boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- | helper function to generate binary operation code
binaryOpCode :: Table -> String -> Int -> Expr -> Expr -> (Code, BaseType)
binaryOpCode table instruction reg expr1 expr2 = (code1 <++> code2 <++> code3, type3) where    -- instruction is add/sub/mul/div
  (code1, type1) = generateExprCode table (reg + 1) expr1
  (code2, type2) = generateExprCode table (reg + 2) expr2
  (code3, type3)
    | type1 == IntType && type2 == FloatType = (    -- need to convert int to float
            Right ((intToReal (reg + 3) (reg + 1)) ++ 
                    instruction ++ "_real " ++ (getReg reg) ++ "," ++ (getReg (reg + 3)) ++ "," ++ (getReg (reg + 2)) ++ "\n")
            , FloatType)
    | type1 == FloatType && type2 == IntType = (    -- need to convert int to float
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

-- | helper function to get address of an array/matrix value
getArrayAddr :: Table -> Int -> Variable -> (Code, BaseType)
getArrayAddr table reg (Arr id expr1)   -- get address of an array value
  = (code, btype) where
    (btype, slot) = table Map.! id
    (code1, type1) = generateExprCode table (reg + 1) expr1
    code = if type1 /= IntType then
            Left "array position can only be int"
           else do
                  -- load addr
                  let code2 = "load_address " ++ (getReg $ reg + 2) ++ "," ++ (show slot) ++ "\n"
                  -- get addr, offset is stored in reg1
                  let code3 = "sub_offset " ++ (getReg reg) ++ "," ++ (getReg $ reg + 2) ++ "," ++ (getReg $ reg + 1) ++ "\n"
                  -- combine
                  code1 <++> Right (code2 ++ code3)
getArrayAddr table reg (Mat id expr1 expr2)    -- get address of a matrix value
  = (code, btype) where
    (btype, slot) = table Map.! id
    (code1, type1) = generateExprCode table (reg + 1) expr1
    (code2, type2) = generateExprCode table (reg + 2) expr2
    code = if type1 /= IntType || type2 /= IntType then
            Left "array position can only be int"
          else do
                  -- load addr to array
                  let code3 = "load_address " ++ (getReg $ reg + 3) ++ "," ++ (show slot) ++ "\n"
                  -- get addr, offset is stored in reg1
                  let code4 = "sub_offset " ++ (getReg $ reg + 4) ++ "," ++ (getReg $ reg + 3) ++ "," ++ (getReg $ reg + 1) ++ "\n"
                  -- load *r4, address of array
                  let code5 = "load_indirect " ++ (getReg $ reg + 5) ++ "," ++ (getReg $ reg + 4) ++ "\n"
                  -- get addr, offset is stored in reg2
                  let code6 = "sub_offset " ++ (getReg reg) ++ "," ++ (getReg $ reg + 5) ++ "," ++ (getReg $ reg + 2) ++ "\n"
                  -- combine
                  code1 <++> code2 <++> Right (code3 ++ code4 ++ code5 ++ code6)
                  -- let code5 = "sub_offset " ++ (getReg reg) ++ "," ++ (getReg $ reg + 4) ++ "," ++ (getReg $ reg + 2) ++ "\n"
                  -- code1 <++> code2 <++> Right (code3 ++ code4 ++ code5)
