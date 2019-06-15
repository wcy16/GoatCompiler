{-|
Module      : CodeGenerate
Description : Generate codes for Oz 

Perform semantic analyse and code generate.
known bugs:
  call by value-results
  strict logical operators
-}

module CodeGenerate where

import GoatAST
import qualified Data.Map as Map
import Data.List

-- | symbol stored in symbol table
--   the entry in symbol table is like
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
  env = Map.fromList 
      [(func_name, params) | Proc func_name params _ _ <- (proc1s ++ proc2s)]

-- | symbol table for goat
--   when we run this method, it will generate one more entry for the table. 
--   The last entry is to keep an recod of how many stack slots used. we 
--   discard that last entry when we generate table.
--
--   It returns a table and stack depth for one procedure
symbolTable :: [Param] -> [Decl] -> (Table, Int)
symbolTable params decls = (table, depth) where
    tableWithEnd = generateSymbolTable 0 params decls
    (_, (_, depth)) = last tableWithEnd
    table = Map.fromList(init tableWithEnd)

-- | generate a list of symbols for symbol table
--
--   If a variable is an array:
--    for an array of length x, we store x frames in stack. The array id is 
--    prepended with a '+'. we also store an entry with the array name but the 
--    slot is set -1 to show that this id is taken
--   If a varable is a matrix:
--    we treat matrix an array of array, so a matrix of shape (x, y) takes 
--    (x * y + x) frames. The first x frame store addresses for the rest x 
--    arrays of length y. The matrix is prepended with a '-'. we also store an 
--    entry with the array name but the slot is set -1 to show that this id is 
--    taken
--   If a variable is a reference:
--    we store 2 symbols for that variable, the first one is the address of 
--    that variable, the second one is the value of that variable. The 
--    address's id is prepended with a '&' since the '&' char is not a valid 
--    identifier.
generateSymbolTable :: Int -> [Param] -> [Decl] -> [Symbol]
-- the last symbol is to keep an record of how many slots used
generateSymbolTable slot [] [] = [("end", (BoolType, slot))]
-- generate symbols for decls
generateSymbolTable slot [] ((Decl ident dtype):decls)
  = case dtype of
      DTBaseType btype -> 
        (ident, (btype, slot)) : (generateSymbolTable (slot + 1) [] decls)
      DTArrayType (ArrayType btype (ArrayShape x)) 
        -> (ident, (btype, -1)) 
            : ('+' : ident, (btype, slot)) 
            : (generateSymbolTable (slot + x) [] decls)
      DTArrayType (ArrayType btype (MatrixShape x y)) 
        -> (ident, (btype, -1)) 
            : ('-' : ident, (btype, slot)) 
            : (generateSymbolTable (slot + x * y + x) [] decls)
-- generate symbols for params
generateSymbolTable slot (param:params) decls = symbols where   
  Param ptype btype ident = param
  symbols = if ptype == Val then 
              (ident, (btype, slot)) 
                : (generateSymbolTable (slot + 1) params decls)
            else
              ('&' : ident, (btype, slot)) 
                : (ident, (btype, slot + 1)) 
                : (generateSymbolTable (slot + 2) params decls)

-- | helper function to get register name by giving register number
getReg :: Int -> String
getReg registerNo = "r" ++ (show registerNo)

-- | helper function to get label name by giving label number
getLabel :: String -> Int -> String
getLabel funcid label = "proc_" ++ funcid ++ "_label_" ++ (show label)

-- | generate goat code by given a goat AST
generateOzCode :: GoatProgram -> Code
generateOzCode goat = oz where
  env = environment goat
  Program proc1s mainProc proc2s = goat
  MainProc decls stmts = mainProc
  mainCode = generateProcCode env "main" [] decls stmts
  procCode = foldl (<++>) 
    mainCode 
      [generateProcCode env id params decls stmts 
        | Proc id params decls stmts <- proc1s ++ proc2s]
  oz = (Right "call proc_main\nhalt\n") <++> procCode

-- | generate codes to store parameters passed to that procedure into a stack.
storeParam :: Table -> Int -> Int -> Param -> Code
storeParam table depth regNo param = code where
  Param ptype btype id = param
  (_, slot) = table Map.! id
  code = (if ptype == Val then
            Right $ "store " ++ (show slot) ++ "," ++ (getReg regNo) ++ "\n"
          else 
            do
              let (_, addr) = table Map.! ('&' : id)
              let code2 = "load_indirect " 
                    ++ (getReg 1023) ++ "," ++ (getReg regNo) ++ "\n"
              let code3 = "store " 
                    ++ (show slot) ++ "," ++ (getReg 1023) ++ "\n"
              let code4 = "store " 
                    ++ (show addr) ++ "," ++ (getReg regNo) ++ "\n"
              return (code2 ++ code3 ++ code4)
          )

-- | generate codes to modify the actual value for the reference type when 
--   return from a procedure
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
-- Initialization
-- 
-- All of variables are initialized with a default value 0 or 0.0
-- The default value is stored in
--        reg0: 0
--        reg1: 0.0
-- So when we initialize variables we use registers start from 2
-----------------------------------------------------------------------------

-- | helper function to initialize matrix
--   register number start from 2
initMatrixN :: Table -> BaseType -> Int -> Int -> Int -> Int -> Code
initMatrixN table btype slot x y n = code where  -- n = 0..x-1
  -- address of matrix[n][0]
  code1 = Right $ "load_address " 
            ++ (getReg 2) ++ "," ++ (show $ slot + x + n * y) ++ "\n"
  -- init matrix[n][0..y]
  code2 = initArr table btype (slot + x + n * y) y 0
  -- init matrix header
  code3 = Right $ "store " ++ (show $ slot + n) ++ "," ++ (getReg 2) ++ "\n"
  -- combine
  code = code1 <++> code2 <++> code3

-- | initialize matrix[x][y] to make the first x frames store the address of
--   matrix[n][0] (n = 0 .. x - 1)
initMatrix :: Table -> String -> Int -> Int -> Code
initMatrix table id x y = code where
  (btype, slot) = table Map.! ('-' : id)
  code = foldl (<++>) 
               (Right "") 
               [initMatrixN table btype slot x y n | n <- [0..(x-1)]]

-- | initialize declarations to default value
initDecl :: Table -> Decl -> Code
initDecl table decl = code where
  Decl id dtype = decl
  code = case dtype of
    DTBaseType btype 
      -> do
          let (_, slot) = table Map.! id
          let code = if btype == FloatType then
                        "store " ++ (show slot) ++ "," ++ (getReg 1) ++ "\n"
                      else
                        "store " ++ (show slot) ++ "," ++ (getReg 0) ++ "\n"
          return code
    DTArrayType (ArrayType btype shape) 
      -> case shape of
          ArrayShape x -> do
                            let (_, slot) = table Map.! ('+' : id)
                            initArr table btype slot x 0
          MatrixShape x y -> do
                                let (_, slot) = table Map.! ('-' : id)
                                initMatrix table id x y

-- | initialize arrays
initArr :: Table -> BaseType -> Int -> Int -> Int -> Code
initArr table btype slot x n = code where
  code1
    | btype == BoolType || btype == IntType 
        = "store " ++ (show $ slot + n) ++ "," ++ (getReg 0) ++ "\n"
    | otherwise 
        = "store " ++ (show $ slot + n) ++ "," ++ (getReg 1) ++ "\n"
  code = if x == n + 1 then
          Right code1
         else
          (Right code1) <++> (initArr table btype slot x (n + 1))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

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
generateProcCode env name params decls stmts 
  = header <++> stores <++> inits <++> pcodes <++> returns where
    -- generate symbol table
    (table, depth) = symbolTable params decls   
    -- the frames used
    pushStack = "push_stack_frame " ++ (show depth) ++ "\n"   
    -- after procedure finished, we pop the frames
    popStack = "pop_stack_frame " ++ (show depth) ++ "\n"     
    header = Right $ ("proc_" ++ name) ++ ":\n" ++ pushStack
    -- store params to stack
    stores = foldl (<++>) 
                   (Right "")
                   (zipWith (storeParam table depth) [0..depth] params)
    -- init decls
    initIntRegs = "int_const " ++ (getReg 0) ++ ",0\n"
    initRealRegs = "real_const " ++ (getReg 1) ++ ",0.0\n"
    inits = foldl (<++>) 
                  (Right $ initIntRegs ++ initRealRegs) 
                  (map (initDecl table) decls)
    -- procedure executing code
    (pcodes, _) = generateStmts name env table 0 stmts
    -- after execute procedure we need to return
    returns = foldr (<++>) 
              (Right (popStack ++ "return\n")) 
              (map (returnReference table)
                   (filter (\(Param ptype _ _) -> ptype == Ref ) params))

-- | generate codes for statements
--   Counter is to keep a track of label counts so that the program will not
--   generate labels of the same name. It will be updated when a label is 
--   created
generateStmts :: String -> Env -> Table -> Int -> [Stmt] -> (Code, Int)
generateStmts funcid env table counter stmts
  = foldl (\(code1, counter) stmt -> do
                  let (code2, nextCounter) 
                        = generateStmtCode funcid env table counter stmt
                  (code1 <++> code2, nextCounter))
          (Right "", counter)
          stmts

-- | generate codes for a single statement
generateStmtCode :: String -> Env -> Table -> Int -> Stmt -> (Code, Int)
generateStmtCode name env table counter stmt = (code, newCter) where
  (code, newCter) = case stmt of
    -- assign values
    Assign (Lvalue (Prim id)) expr 
      -> (code1 <++> code2, counter) where
        (code1, type1) = generateExprCode table 0 expr
        (type2, slot) = table Map.! id
        code2
          | slot == -1 = Left "not a variable"
          | type1 == IntType && type2 == FloatType 
              = return $ intToReal 1 0 
                          ++ "store " ++ (show slot) 
                          ++ "," ++  (getReg 1) ++ "\n"
          | type1 == type2
               = return $ "store " ++ (show slot) ++ "," ++ (getReg 0) ++ "\n"
          | otherwise = Left "parameters not match"
    -- assign array
    Assign (Lvalue (Arr id expr1)) expr2 
      -> (code1 <++> code2 <++> code3, counter) where
        -- find array addr in reg0
        (code1, type1) = getArrayAddr table 0 (Arr id expr1)
        -- calculate expr2 in reg1
        (code2, type2) = generateExprCode table 1 expr2
        code3
          | type1 == FloatType && type2 == IntType 
              = return $ intToReal 2 1 
                            ++ "store_indirect " ++ (getReg 0) 
                            ++ "," ++ (getReg 2) ++ "\n"
          | type1 == type2 
              = return $ "store_indirect " ++ (getReg 0) 
                            ++ "," ++ (getReg 1) ++ "\n"
          | otherwise = Left "parameters not match"
    -- assign matrix
    Assign (Lvalue (Mat id expr1 expr2)) expr3 
      -> (code1 <++> code2 <++> code3, counter) where
        -- find array addr in reg0
        (code1, type1) = getArrayAddr table 0 (Mat id expr1 expr2)
        -- calculate expr2 in reg1
        (code2, type2) = generateExprCode table 1 expr3
        code3
          | type1 == FloatType && type2 == IntType 
              = return $ intToReal 2 1 
                            ++ "store_indirect " ++ (getReg 0) 
                            ++ "," ++ (getReg 2) ++ "\n"
          | type1 == type2 
              = return $ "store_indirect " ++ (getReg 0) 
                            ++ "," ++ (getReg 1) ++ "\n"
          | otherwise = Left "parameters not match"

    -- read values  
    Read (Lvalue (Prim id)) 
      -> (code1, counter) where
        (type1, slot) = table Map.! id
        code1 = if slot == -1 then Left "not a variable"
                else
                  Right ("call_builtin read_" ++ type1Code ++ load) where
                  type1Code = case type1 of
                                BoolType -> "bool"
                                IntType -> "int"
                                FloatType -> "real"
                  load = "\nstore " ++ (show slot) ++ ",r0\n"
    Read (Lvalue (Arr id expr1)) 
      -> (code, counter) where
        (typeExpr, slot) = table Map.! ('+' : id)
        -- get arr addr
        (code1, type1) = getArrayAddr table 1 (Arr id expr1)
        -- read from keyboard
        code2 = Right ("call_builtin read_" ++ type1Code ++ "\n") where
          type1Code = case type1 of
                        BoolType -> "bool"
                        IntType -> "int"
                        FloatType -> "real"
        -- read reg0 to * reg1
        code3 = Right $ "store_indirect " ++ (getReg 1) 
                            ++ "," ++ (getReg 0) ++ "\n"
        code = code2 <++> code1 <++> code3
    Read (Lvalue (Mat id expr1 expr2)) 
      -> (code, counter) where
        (typeExpr, slot) = table Map.! ('-' : id)
        -- get arr addr
        (code1, type1) = getArrayAddr table 1 (Mat id expr1 expr2)
        -- read from keyboard
        code2 = Right ("call_builtin read_" ++ type1Code ++ "\n") where
          type1Code = case type1 of
                        BoolType -> "bool"
                        IntType -> "int"
                        FloatType -> "real"
        -- read reg0 to * reg1
        code3 = Right $ "store_indirect " ++ (getReg 1) 
                            ++ "," ++ (getReg 0) ++ "\n"
        code = code2 <++> code1 <++> code3

    -- string const is only used in write
    Write (StrConst str) 
      -> (Right $ "string_const " ++ (getReg 0) 
                      ++ ",\"" ++ str ++ "\"\ncall_builtin print_string\n"
          , counter)
    Write expr 
      -> (code1 <++> (Right $ "call_builtin " ++ builtin ++ "\n"), 
                                                            counter) where
        (code1, type1) = generateExprCode table 0 expr
        builtin = case type1 of 
            IntType -> "print_int"
            FloatType -> "print_real"
            BoolType -> "print_bool"
                   
    -- if then                                
    If expr stmts 
      -> (codeif, labels) where
        (code1, type1) = generateExprCode table 0 expr
        (codeif, labels) 
          = if type1 /= BoolType then (Left "Not a Boolean type in IF", 0)
            else (code2, cter) where
              label = getLabel name counter
              -- if
              code3 = Right $ "branch_on_false " ++ (getReg 0) 
                                  ++ "," ++ label ++ "\n"
              -- if body
              (code4, cter) = generateStmts name env table (counter + 1) stmts
              code5 = Right $ label ++ ":\n"
              code2 = code1 <++> code3 <++> code4 <++> code5
    
    -- if else
    IfElse expr stmts1 stmts2 
      -> (codeif, labels) where
        (code1, type1) = generateExprCode table 0 expr
        (codeif, labels) 
          = if type1 /= BoolType then (Left "Not a Boolean type in IF", 0)
            else (code2, cter2) where   
              labelElse = getLabel name counter
              -- if
              code3 = Right $ "branch_on_false " ++ (getReg 0) 
                                  ++ "," ++ labelElse ++ "\n"
              -- then
              (code4, cter) = generateStmts name env table (counter + 1) stmts1
              labelFi = getLabel name cter
              code5 = Right $ "branch_uncond " ++ labelFi ++ "\n"
              -- else
              code6 = Right $ labelElse ++ ":\n"
              (code7, cter2) = generateStmts name env table (cter + 1) stmts2
              code8 = Right $ labelFi ++ ":\n"
              -- code
              code2 = code1 <++> code3 <++> code4 <++> code5 
                        <++> code6 <++> code7 <++> code8

    -- while
    While expr stmts 
      -> (codewhile, labels) where
        (code1, type1) = generateExprCode table 0 expr
        (codewhile, labels) 
          = if type1 /= BoolType then (Left "Not a Boolean type in WHILE", 0)
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
              code7 = Right $ "branch_on_true " ++ (getReg 0) 
                                  ++ "," ++ labelBody ++ "\n"        
              -- code
              code2 = code3 <++> code4 <++> code5 <++> code6 
                        <++> code1 <++> code7
                                                    
    -- call func
    Call funcid exprs 
      -> (codeCall, counter) where
        codeCall 
          = do
              let exprCnt = length(exprs)
              let params = env Map.! funcid
              if exprCnt /= length(params) then
                return "Not correct parameters count"
              else do
                -- store exprs into reg
                let codes = zipWith3 (generateParamCode table) 
                                     [0..exprCnt] 
                                     exprs 
                                     params
                foldr (<++>) 
                      (return ("call " ++ "proc_" ++ funcid ++ "\n")) 
                      codes

-- | load parameters to registers when perform a call function
generateParamCode :: Table -> Int -> Expr -> Param -> Code
generateParamCode table reg expr param = code where
  Param ptype btype id = param
  code = if ptype == Val then -- val type
          do
            let (exprCode, exprType) = generateExprCode table reg expr
            if exprType /= btype then
              if btype == FloatType && exprType == IntType then
                exprCode <++> (Right $ (intToReal (reg + 1) reg) ++ "move " 
                                          ++ (getReg reg) ++ "," 
                                          ++ (getReg $ reg + 1) ++ "\n")
              else
                Left "parameter type check fail"
            else
              exprCode
         else case expr of   -- ref type
                Var (Prim id) 
                  -> do
                      let (vartype, slot) = table Map.! id
                      if slot == -1 then
                        Left "not a variable"
                      else
                        if vartype /= btype then   -- parameter not match
                          Left "parameter type check fail"
                        else
                          Right $ "load_address " ++ (getReg reg) 
                                      ++ "," ++ (show slot) ++ "\n"
                Var (Arr id expr1) 
                  -> do
                      let (arrCode, btype) 
                              = getArrayAddr table reg (Arr id expr1)
                      arrCode
                Var (Mat id expr1 expr2) 
                  -> do
                      let (arrCode, btype) 
                              = getArrayAddr table reg (Mat id expr1 expr2)
                      arrCode
                _ -> Left "cannot parse reference type"
                
-- | generate expression codes, the result is stored in a given register
--   return the code and the type of the value                         
generateExprCode :: Table -> Int -> Expr -> (Code, BaseType)
generateExprCode table reg expr = case expr of
  -- const
  IntConst const -> (Right ("int_const " ++ (getReg reg) 
                                ++ "," ++ (show const) ++ "\n"),
                    IntType)
  FloatConst const -> (Right ("real_const " ++ (getReg reg) 
                                ++ "," ++ (show const) ++ "\n"),
                    FloatType)
  BoolConst const -> (Right ("int_const " ++ (getReg reg) 
                                ++ "," ++ (show $ boolToInt const) ++ "\n"),
                    BoolType)
  -- string const is not needed because string is only used in write

  -- variable
  Var (Prim id) 
    -> do
        let (btype, slot) = table Map.! id
        if slot == -1 then
          (Left "not a valid id", btype)
        else
          (Right("load " ++ (getReg reg) ++ "," ++ (show slot) ++ "\n"), btype)
  Var (Arr id expr1) 
    -> (code, btype) where
      (code1, btype) = getArrayAddr table (reg + 1) (Arr id expr1)
      code2 = Right $ "load_indirect " ++ (getReg reg) 
                          ++ "," ++ (getReg $ reg + 1) ++ "\n"
      code = code1 <++> code2
  Var (Mat id expr1 expr2) 
    -> (code, btype) where
      (code1, btype) = getArrayAddr table (reg + 1) (Mat id expr1 expr2)
      code2 = Right $ "load_indirect " ++ (getReg reg) 
                            ++ "," ++ (getReg $ reg + 1) ++ "\n"
      code = code1 <++> code2
  
  -- binary arithmetic ops
  Add expr1 expr2 -> binaryOpCode table "add" reg expr1 expr2
  Minus expr1 expr2 -> binaryOpCode table "sub" reg expr1 expr2
  Mul expr1 expr2 -> binaryOpCode table "mul" reg expr1 expr2
  Div expr1 expr2 -> binaryOpCode table "div" reg expr1 expr2

  -- binary logic ops
  And expr1 expr2 -> binaryLogicOpCode table "and" reg expr1 expr2
  Or expr1 expr2 -> binaryLogicOpCode table "or" reg expr1 expr2

  -- binary compare ops
  Equal expr1 expr2 -> (code, BoolType) where 
    (code, _) = binaryOpCode table "cmp_eq" reg expr1 expr2
  NotEqual expr1 expr2 -> (code, BoolType) where 
    (code, _) = binaryOpCode table "cmp_ne" reg expr1 expr2
  Greater expr1 expr2 -> (code, BoolType) where 
    (code, _) = binaryOpCode table "cmp_gt" reg expr1 expr2
  GreaterEqual expr1 expr2 -> (code, BoolType) where 
    (code, _) = binaryOpCode table "cmp_ge" reg expr1 expr2
  Less expr1 expr2 -> (code, BoolType) where 
    (code, _) = binaryOpCode table "cmp_lt" reg expr1 expr2
  LessEqual expr1 expr2 -> (code, BoolType) where 
    (code, _) = binaryOpCode table "cmp_le" reg expr1 expr2
  
  -- unary ops
  Not expr -> (code, BoolType) where 
    (code1, type1) = generateExprCode table (reg + 1) expr
    code = if type1 == BoolType
            then code1 <++> Right ("not " ++ (getReg reg) 
                                      ++ "," ++ (getReg (reg + 1)) ++ "\n")
            else Left "cannot apply NOT op to non-boolean type!"
  UnaryMinus expr -> (code1 <++> code2, type1) where
    (code1, type1) = generateExprCode table (reg + 1) expr      
    code2
      | type1 == IntType 
          = Right $ "neg_int " ++ (getReg reg) 
                        ++ "," ++ (getReg $ reg + 1) ++ "\n"
      | type1 == FloatType 
          = Right $ "neg_real " ++ (getReg reg) 
                        ++ "," ++ (getReg $ reg + 1) ++ "\n"
      | otherwise = Left "UnaryMinus to a non int/float type"
  
  -- brackets
  Bracket expr1 -> generateExprCode table reg expr1

-- | helper function to generate int_to_real code
intToReal :: Int -> Int -> String
intToReal reg1 reg2 = "int_to_real " ++ (getReg reg1) 
                          ++ "," ++ (getReg reg2) ++ "\n"

-- | helper function to generate int value for bool type
boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- | helper function to generate binary operation code
--   instructions are add/mul/div/min and six compare ops
binaryOpCode :: Table -> String -> Int -> Expr -> Expr -> (Code, BaseType)
binaryOpCode table instruction reg expr1 expr2 
  = (code1 <++> code2 <++> code3, type3) where
    (code1, type1) = generateExprCode table (reg + 1) expr1
    (code2, type2) = generateExprCode table (reg + 2) expr2
    (code3, type3)
      | type1 == IntType && type2 == FloatType = ( 
              -- need to convert int to float
              Right ((intToReal (reg + 3) (reg + 1)) ++ 
                      instruction ++ "_real " ++ (getReg reg) 
                      ++ "," ++ (getReg (reg + 3)) 
                      ++ "," ++ (getReg (reg + 2)) ++ "\n")
              , FloatType)
      | type1 == FloatType && type2 == IntType = (    
              -- need to convert int to float
              Right ((intToReal (reg + 3) (reg + 2)) ++ 
                      instruction ++ "_real " ++ (getReg reg) 
                      ++ "," ++ (getReg (reg + 1)) 
                      ++ "," ++ (getReg (reg + 3)) ++ "\n")
              , FloatType)
      | type1 == FloatType && type2 == FloatType = (
              Right (instruction ++ "_real " ++ (getReg reg) 
                        ++ "," ++ (getReg (reg + 1)) 
                        ++ "," ++ (getReg (reg + 2)) ++ "\n")
              , FloatType)
      | type1 == IntType && type2 == IntType = (
              Right (instruction ++ "_int " ++ (getReg reg) 
                        ++ "," ++ (getReg (reg + 1)) 
                        ++ "," ++ (getReg (reg + 2)) ++ "\n")
              , IntType)
      | type1 == BoolType && type2 == BoolType = ( -- boolean is treated as int
              Right (instruction ++ "_int " ++ (getReg reg) 
                        ++ "," ++ (getReg (reg + 1)) 
                        ++ "," ++ (getReg (reg + 2)) ++ "\n")
              , BoolType)
      | otherwise = (Left "error binary op", type1)

-- | helper function to generate binary logic operation code
--   instructions are and/or
binaryLogicOpCode :: Table -> String -> Int -> Expr -> Expr -> (Code, BaseType)
binaryLogicOpCode table instruction reg expr1 expr2 
  = (code1 <++> code2 <++> code3, type3) where
    (code1, type1) = generateExprCode table (reg + 1) expr1
    (code2, type2) = generateExprCode table (reg + 2) expr2
    (code3, type3)
      | type1 == BoolType && type2 == BoolType = (   -- and / or
              Right (instruction ++ " " ++ (getReg reg) 
                        ++ "," ++ (getReg (reg + 1)) 
                        ++ "," ++ (getReg (reg + 2)) ++ "\n")
              , BoolType)
      | otherwise = (Left "error binary op", type1)

-- | helper function to get address of an array/matrix value
getArrayAddr :: Table -> Int -> Variable -> (Code, BaseType)
-- get address of an array value
getArrayAddr table reg (Arr id expr1)   
  = (code, btype) where
    (btype, slot) = table Map.! ('+' : id)
    (code1, type1) = generateExprCode table (reg + 1) expr1
    code = if type1 /= IntType then
            Left "array position can only be int"
           else 
            do
              -- load addr
              let code2 = "load_address " ++ (getReg $ reg + 2) 
                              ++ "," ++ (show slot) ++ "\n"
              -- get addr, offset is stored in reg1
              let code3 = "sub_offset " ++ (getReg reg) 
                              ++ "," ++ (getReg $ reg + 2) 
                              ++ "," ++ (getReg $ reg + 1) ++ "\n"
              -- combine
              code1 <++> Right (code2 ++ code3)
-- get address of a matrix value
getArrayAddr table reg (Mat id expr1 expr2)    
  = (code, btype) where
    (btype, slot) = table Map.! ('-' : id)
    (code1, type1) = generateExprCode table (reg + 1) expr1
    (code2, type2) = generateExprCode table (reg + 2) expr2
    code = if type1 /= IntType || type2 /= IntType then
            Left "array position can only be int"
          else 
            do
              -- load addr to array
              let code3 = "load_address " ++ (getReg $ reg + 3) 
                              ++ "," ++ (show slot) ++ "\n"
              -- get addr, offset is stored in reg1
              let code4 = "sub_offset " ++ (getReg $ reg + 4) 
                              ++ "," ++ (getReg $ reg + 3) 
                              ++ "," ++ (getReg $ reg + 1) ++ "\n"
              -- load *r4, address of array
              let code5 = "load_indirect " ++ (getReg $ reg + 5) 
                              ++ "," ++ (getReg $ reg + 4) ++ "\n"
              -- get addr, offset is stored in reg2
              let code6 = "sub_offset " ++ (getReg reg) 
                              ++ "," ++ (getReg $ reg + 5) 
                              ++ "," ++ (getReg $ reg + 2) ++ "\n"
              -- combine
              code1 <++> code2 <++> Right (code3 ++ code4 ++ code5 ++ code6)
