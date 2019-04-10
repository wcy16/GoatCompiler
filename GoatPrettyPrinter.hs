module GoatPrettyPrinter where

import GoatAST

indent :: String
indent = "    "

ppProg :: GoatProgram -> String
ppProg program = pprog where
  Program mainproc procs = program
  MainProc maindecls mainstmts = mainproc
  pprog = foldl (\x y -> x ++ "\n" ++ y) 
                (ppProc (Proc "main" [] maindecls mainstmts)) 
                [ppProc proc | proc <- procs]

ppProc :: Proc -> String
ppProc proc = pheader ++ pdecls ++ pbody where
  Proc id params decls stmts = proc
  pheader = "proc " ++ id ++ " (" ++ (ppParams params) ++ ")\n"
  pdecls = foldl (\x y -> x ++ indent ++ y ++ "\n") "" [ppDecl decl | decl <- decls]
  pbody = "begin\nend\n"  -- todo

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

ppDecl :: Decl -> String
ppDecl decl = (ppDataType datatype id) ++ ";" where
  Decl id datatype = decl
  


ppBaseType :: BaseType -> Ident -> String
ppBaseType basetype id = typename ++ id where
  typename = case basetype of
                  BoolType -> "bool "
                  IntType -> "int "
                  FloatType -> "float "

ppArrayType ::  ArrayType -> Ident -> String
ppArrayType arrtype id = pbasetype ++ "[" ++ pshape ++ "]" where
  ArrayType basetype shape = arrtype
  pbasetype = ppBaseType basetype id
  pshape = case shape of
                ArrayShape m -> show m
                MatrixShape m n -> (show m) ++ ", " ++ (show n)
                           
ppDataType :: DataType -> Ident -> String
ppDataType datatype id = case datatype of
                              DTBaseType basetype -> ppBaseType basetype id
                              DTArrayType arraytype -> ppArrayType arraytype id
