module GoatAST where

type Ident = String

data BaseType
  = BoolType | IntType | FloatType
    deriving (Show, Eq)

data ArrayShape
  = ArrayShape Int
  | MatrixShape Int Int
    deriving (Show, Eq)

data ArrayType
  = ArrayType BaseType ArrayShape
    deriving (Show, Eq)

data DataType
  = DTBaseType BaseType
  | DTArrayType ArrayType
  deriving (Show, Eq)

data Variable
  = Prim Ident   -- primitive type
  | Arr Ident Expr
  | Mat Ident Expr Expr
  deriving (Show, Eq)

data Lvalue 
  = Lvalue Variable
    deriving (Show, Eq)

data Unaop
  = Op_not | Op_unaryMinus
    deriving (Show, Eq)

data Binop
  = Op_or | Op_and | Op_equal | Op_notEqual
  | Op_less | Op_lessEqual
  | Op_greater | Op_greaterEqual
  | Op_add | Op_minus | Op_mul | Op_div
    deriving (Show, Eq)

data Expr
  = Var Variable
  | BoolConst Bool | IntConst Int | FloatConst Float | StrConst String
  | Bracket Expr
  | Or Expr Expr
  | And Expr Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Less Expr Expr
  | LessEqual Expr Expr
  | Greater Expr Expr
  | GreaterEqual Expr Expr
  | Add Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | UnaryMinus Expr
  | Not Expr
    deriving (Show, Eq)

data Decl 
  = Decl Ident DataType
    deriving (Show, Eq)

data Stmt
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

data ParamType
  = Val | Ref
    deriving (Show, Eq)

data Param
  = Param ParamType DataType Ident
    deriving (Show, Eq)

data Proc
  = Proc Ident [Param] [Decl] [Stmt]
    deriving (Show, Eq)

data MainProc
  = MainProc [Decl] [Stmt]
    deriving (Show, Eq)

data GoatProgram
  = Program [Proc] MainProc [Proc]
    deriving (Show, Eq)

