module TAC (
    module TAC
) where

import AbsPhynot
import TypeSystem as TS

data Address = ProgVar{ progVar :: ProgVariable, addrBT :: TACBasicType}
            | TacLit{ tacLit :: Literal, addrBT :: TACBasicType}
            | Temporary{ tempName :: String, addrBT :: TACBasicType}
    deriving (Eq, Show)

data TACBasicType = IntegerType | FloatType | CharType | BooleanType | StringType | MemoryAddressType | NoneType
    deriving (Eq, Show)

data Literal = IntLit Integer | FloatLit Double | BoolLit Bool | CharLit Char | StringLit String
    deriving (Eq, Show)

data Value = IntVal Integer | DoubleVal Double | BoolVal Bool | CharVal Char | StringVal String
    deriving (Eq, Show)

data ProgVariable = ProgVariable String
    deriving (Eq, Show)

data Label = Label String
    deriving (Eq, Show)

data TAC = TacInstruction TACInstruction | LabelledInstruction Label TACInstruction
    deriving (Eq, Show)

data TACInstruction = BinaryOperation TACBasicType Address Address Address TypedBinaryOp    -- l = r1 bop r2
                    | UnaryOperation Address Address UnaryOp                                -- l = uop r
                    | NullaryOperation Address Address                                      -- l = r
                    | UnconditionalJump Label                                               -- goto label  
                    | ConditionalJump Address Label                                         -- if r goto label
                    | IndexedAssignment Address Address Address                             -- id[r1] = r2
                    | IndexedCopyAssignment Address Address Address                         -- r1 = id[r2]
                    | FunctionDef Address Int                                               -- def r1 n
                    | Return Address                                                        -- return r
                    | ReturnVoid                                                            -- return
                    | FunctionCall Address Address Int                                      -- r = fcall fun / n
                    | ProcedureCall Address Int                                             -- pcall proc / n
                    | FunctionParam Address                                                 -- param r 
                    | NoOperation                                                           -- nop
    deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Exp | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or
    deriving (Eq, Show)

data TypedBinaryOp = AddInt | SubInt | MulInt | DivInt | ExpInt | 
                    AddFloat | SubFloat | MulFloat | DivFloat | ExpFloat |
                    EqInt | EqFloat | EqBool | EqChar | EqString |
                    NeInt | NeFloat | NeBool | NeChar | NeString |
                    LtInt | LtFloat | LtChar | LeInt | LeFloat | LeChar |
                    GtInt | GtFloat | GtChar | GeInt | GeFloat | GeChar |
                    AndBool | OrBool
    deriving (Eq, Show)

data UnaryOp = Neg | Not | Ref | Deref
    deriving (Eq, Show)

toTacType :: TS.Type -> TACBasicType
toTacType t = case t of
    TS.Base TS.INT -> IntegerType
    TS.Base TS.FLOAT -> FloatType
    TS.Base TS.BOOL -> BooleanType
    TS.Base TS.CHAR -> CharType
    TS.Base TS.STRING -> StringType
    TS.ADDRESS _ -> MemoryAddressType
    TS.POINTER _ -> MemoryAddressType

toTypedBinaryOp :: TS.Type -> BinaryOp -> TypedBinaryOp
toTypedBinaryOp t TAC.Add = case t of
    TS.Base TS.INT -> AddInt
    TS.Base TS.FLOAT -> AddFloat

toTypedBinaryOp t TAC.Sub = case t of
    TS.Base TS.INT -> SubInt
    TS.Base TS.FLOAT -> SubFloat

toTypedBinaryOp t TAC.Mul = case t of
    TS.Base TS.INT -> MulInt
    TS.Base TS.FLOAT -> MulFloat

toTypedBinaryOp t TAC.Div = case t of
    TS.Base TS.INT -> DivInt
    TS.Base TS.FLOAT -> DivFloat

toTypedBinaryOp t TAC.Exp = case t of
    TS.Base TS.INT -> ExpInt
    TS.Base TS.FLOAT -> ExpFloat

toTypedBinaryOp t TAC.Eq = case t of
    TS.Base TS.INT -> EqInt
    TS.Base TS.FLOAT -> EqFloat
    TS.Base TS.BOOL -> EqBool
    TS.Base TS.CHAR -> EqChar
    TS.Base TS.STRING -> EqString

toTypedBinaryOp t TAC.Ne = case t of
    TS.Base TS.INT -> NeInt
    TS.Base TS.FLOAT -> NeFloat
    TS.Base TS.BOOL -> NeBool
    TS.Base TS.CHAR -> NeChar
    TS.Base TS.STRING -> NeString

toTypedBinaryOp t TAC.Lt = case t of
    TS.Base TS.INT -> LtInt
    TS.Base TS.FLOAT -> LtFloat
    TS.Base TS.CHAR -> LtChar

toTypedBinaryOp t TAC.Le = case t of
    TS.Base TS.INT -> LeInt
    TS.Base TS.FLOAT -> LeFloat
    TS.Base TS.CHAR -> LeChar

toTypedBinaryOp t TAC.Gt = case t of
    TS.Base TS.INT -> GtInt
    TS.Base TS.FLOAT -> GtFloat
    TS.Base TS.CHAR -> GtChar

toTypedBinaryOp t TAC.Ge = case t of
    TS.Base TS.INT -> GeInt
    TS.Base TS.FLOAT -> GeFloat
    TS.Base TS.CHAR -> GeChar

toTypedBinaryOp _ TAC.And = AndBool
toTypedBinaryOp _ TAC.Or = OrBool

generateAddr :: TS.Type -> String -> Address
generateAddr bt s = case bt of
    TS.Base TS.INT -> ProgVar (ProgVariable s) IntegerType
    TS.Base TS.FLOAT -> ProgVar (ProgVariable s) FloatType
    TS.Base TS.BOOL -> ProgVar (ProgVariable s) BooleanType
    TS.Base TS.CHAR -> ProgVar (ProgVariable s) CharType
    TS.Base TS.STRING -> ProgVar (ProgVariable s) StringType
    TS.Base TS.NONE -> ProgVar (ProgVariable s) NoneType
    TS.ARRAY _ -> ProgVar (ProgVariable s) MemoryAddressType
    _ -> ProgVar (ProgVariable s) MemoryAddressType

generateLit :: TS.Type -> Value -> Address
generateLit bt val = case (bt, val) of
    (TS.Base TS.INT, IntVal i) -> TacLit (IntLit i) IntegerType
    (TS.Base TS.FLOAT, DoubleVal d) -> TacLit (FloatLit d) FloatType
    (TS.Base TS.BOOL, BoolVal b) -> TacLit (BoolLit b) BooleanType
    (TS.Base TS.CHAR, CharVal c) -> TacLit (CharLit c) CharType
    (TS.Base TS.STRING, StringVal s) -> TacLit (StringLit s) StringType
    _ -> error "Type and value do not match"

isTACLit :: Address -> Bool
isTACLit (TacLit _ _) = True
isTACLit _ = False

mkArrayIndex :: Int -> [Int] -> [Int] -> Int -> Int
mkArrayIndex tsize [] _ tot = tot
mkArrayIndex tsize index arrsize tot = mkArrayIndex tsize (tail index) (tail arrsize) (head index * product (tail arrsize) * tsize + tot)

generateArrayEmpty :: Address -> [Int] -> TS.Type -> [TAC]
generateArrayEmpty a x t  = case t of
    (TS.ARRAY t) -> generateArrayEmpty a x t
    (TS.Base TS.INT) -> generateArrayEmpty' a (product x) (TS.getTypeSize t) t (IntVal 0) 0
    (TS.Base TS.FLOAT) -> generateArrayEmpty' a (product x) (TS.getTypeSize t) t (DoubleVal 0) 0
    (TS.Base TS.BOOL) -> generateArrayEmpty' a (product x) (TS.getTypeSize t) t (BoolVal False) 0
    (TS.Base TS.CHAR) -> generateArrayEmpty' a (product x) (TS.getTypeSize t) t (CharVal '0') 0
    (TS.Base TS.STRING) -> generateArrayEmpty' a (product x) (TS.getTypeSize t) t (StringVal "") 0

generateArrayEmpty' :: Address -> Int -> Int -> TS.Type -> Value -> Int -> [TAC]
generateArrayEmpty' a 0 size t val c = []
generateArrayEmpty' a x size t val c = TacInstruction (IndexedAssignment a (generateLit (TS.Base TS.INT) (IntVal (toInteger (c * size)))) (generateLit t val))
    : generateArrayEmpty' a (x-1) size t val (c+1)

generateArray :: Address -> [Int] -> TS.Type -> [Address] -> [TAC]
generateArray a x t vals = case t of
    (TS.Base TS.INT) -> generateArray' a (product x) (TS.getTypeSize t) vals 0
    (TS.Base TS.FLOAT) -> generateArray' a (product x) (TS.getTypeSize t) vals 0
    (TS.Base TS.BOOL) -> generateArray' a (product x) (TS.getTypeSize t) vals 0
    (TS.Base TS.CHAR) -> generateArray' a (product x) (TS.getTypeSize t) vals 0
    (TS.Base TS.STRING) -> generateArray' a (product x) (TS.getTypeSize t) vals 0
    (TS.ARRAY t) -> generateArray a x t vals

generateArray' :: Address -> Int -> Int -> [Address] -> Int -> [TAC]
generateArray' a 0 size vals c = []
generateArray' a x size vals  c = TacInstruction (IndexedAssignment a (generateLit (TS.Base TS.INT) (IntVal (toInteger (c * size)))) (head vals))
    : generateArray' a (x-1) size (tail vals) (c+1)

generateFuncCall :: Address -> Address -> Int -> [Address] -> [TAC]
generateFuncCall t p n d = map (TacInstruction . FunctionParam) d ++ [TacInstruction (TAC.FunctionCall t p n)]

generateProcCall :: Address -> Int -> [Address] -> [TAC]
generateProcCall p n d = map (TacInstruction . FunctionParam) d ++ [TacInstruction (TAC.ProcedureCall p n)]

type State = (Int, Int)

incrementTemp :: State -> State
incrementTemp (k, l) = (k + 1, l)

incrementLabel :: State -> State
incrementLabel (k, l) = (k, l + 1)

newtemp :: State -> TS.Type -> Address
newtemp (k, l) t = case t of
    TS.Base TS.INT -> Temporary ("t" ++ show k) IntegerType
    TS.Base TS.FLOAT -> Temporary ("t" ++ show k) FloatType
    TS.Base TS.BOOL -> Temporary ("t" ++ show k) BooleanType
    TS.Base TS.CHAR -> Temporary ("t" ++ show k) CharType
    TS.Base TS.STRING -> Temporary ("t" ++ show k) StringType
    TS.ADDRESS _ -> Temporary ("t" ++ show k) MemoryAddressType
    TS.POINTER _ -> Temporary ("t" ++ show k) MemoryAddressType
    _ -> Temporary ("t" ++ show k) MemoryAddressType

newLabel :: State -> Label
newLabel (k, l) = Label ("L" ++ show l)

tacTypeToString :: TACBasicType -> String
tacTypeToString IntegerType = "int"
tacTypeToString FloatType = "float"
tacTypeToString BooleanType = "bool"
tacTypeToString CharType = "char"
tacTypeToString StringType = "string"
tacTypeToString NoneType = "none"
tacTypeToString MemoryAddressType = "memory"

printBinaryOp :: TypedBinaryOp -> String
printBinaryOp AddInt = "add_int"
printBinaryOp SubInt = "sub_int"
printBinaryOp MulInt = "mul_int"
printBinaryOp DivInt = "div_int"
printBinaryOp ExpInt = "exp_int"
printBinaryOp AddFloat = "add_float"
printBinaryOp SubFloat = "sub_float"
printBinaryOp MulFloat = "mul_float"
printBinaryOp DivFloat = "div_float"
printBinaryOp ExpFloat = "exp_float"

printBinaryOp EqInt = "eq_int"
printBinaryOp EqFloat = "eq_float"
printBinaryOp EqBool = "eq_bool"
printBinaryOp EqChar = "eq_char"
printBinaryOp EqString = "eq_string"

printBinaryOp NeInt = "ne_int"
printBinaryOp NeFloat = "ne_float"
printBinaryOp NeBool = "ne_bool"
printBinaryOp NeChar = "ne_char"
printBinaryOp NeString = "ne_string"
printBinaryOp LtInt = "lt_int"
printBinaryOp LtFloat = "lt_float"
printBinaryOp LtChar = "lt_char"
printBinaryOp LeInt = "le_int"
printBinaryOp LeFloat = "le_float"
printBinaryOp LeChar = "le_char"
printBinaryOp GtInt = "gt_int"
printBinaryOp GtFloat = "gt_float"
printBinaryOp GtChar = "gt_char"
printBinaryOp GeInt = "ge_int"
printBinaryOp GeFloat = "ge_float"
printBinaryOp GeChar = "ge_char"

printBinaryOp AndBool = "and_bool"
printBinaryOp OrBool = "or_bool"

printUnaryOp :: UnaryOp -> String
printUnaryOp TAC.Neg = "-"
printUnaryOp TAC.Not = "!"
printUnaryOp TAC.Ref = "&"
printUnaryOp TAC.Deref = "*"

printAddr :: Address -> String
printAddr (ProgVar (ProgVariable s) _) = s
printAddr (TacLit (IntLit i) _) = show i
printAddr (TacLit (FloatLit d) _) = show d
printAddr (TacLit (BoolLit b) _) = show b
printAddr (TacLit (CharLit c) _) = show c
printAddr (TacLit (StringLit s) _) = s
printAddr (Temporary s _) = s

printTypedAddr :: Address -> String
printTypedAddr (ProgVar (ProgVariable s) t) = tacTypeToString t ++ "_" ++ s
printTypedAddr (TacLit (IntLit i) _) = "int_" ++ show i
printTypedAddr (TacLit (FloatLit d) _) = "float_" ++ show d
printTypedAddr (TacLit (BoolLit b) _) = "bool_" ++ show b
printTypedAddr (TacLit (CharLit c) _) = "char_" ++ show c
printTypedAddr (TacLit (StringLit s) _) = "string_" ++ s
printTypedAddr (Temporary s t) = tacTypeToString t ++ "_" ++ s

printTAC :: [TAC] -> String
printTAC [] = ""
printTAC (LabelledInstruction (Label l) i : xs) = l ++ ": " ++ printTAC (TacInstruction i : xs)
printTAC (TacInstruction (BinaryOperation t a1 a2 a3 op) : xs) = "\t" ++ printAddr a1 ++ " = " ++ printTypedAddr a2 ++ " " ++ printBinaryOp op ++ " " ++ printTypedAddr a3 ++ "\n" ++ printTAC xs
printTAC (TacInstruction (UnaryOperation a1 a2 op) : xs) = "\t" ++ printAddr a1 ++ " = " ++ printUnaryOp op ++ " " ++ printTypedAddr a2 ++ "\n" ++ printTAC xs
printTAC (TacInstruction (NullaryOperation a1 a2) : xs) = "\t" ++ printAddr a1 ++ " = " ++ printTypedAddr a2 ++ "\n" ++ printTAC xs
printTAC (TacInstruction (UnconditionalJump (Label l)) : xs) = "\tgoto " ++ l ++ "\n" ++ printTAC xs
printTAC (TacInstruction (ConditionalJump a1 (Label l)) : xs) = "\tifFalse " ++ printAddr a1 ++ " goto " ++ l ++ "\n" ++ printTAC xs
printTAC (TacInstruction (IndexedAssignment a1 a2 a3) : xs) = "\t" ++ printAddr a1 ++ "[" ++ printAddr a2 ++ "] = " ++ printTypedAddr a3 ++ "\n" ++ printTAC xs
printTAC (TacInstruction (IndexedCopyAssignment a1 a2 a3) : xs) = "\t" ++ printAddr a1 ++ " = " ++ printTypedAddr a2 ++ "[" ++ printAddr a3 ++ "]\n" ++ printTAC xs
printTAC (TacInstruction (FunctionDef (ProgVar (ProgVariable s) t) n) : xs) = tacTypeToString t ++ " " ++ s ++ " / " ++ show n ++ "\n" ++ printTAC xs
printTAC (TacInstruction (TAC.Return a) : xs) = "\treturn " ++ printTypedAddr a ++ "\n" ++ printTAC xs
printTAC (TacInstruction ReturnVoid : xs) = "\treturn_none\n" ++ printTAC xs
printTAC (TacInstruction (FunctionCall a f n) : xs) = "\t" ++ printAddr a ++ " = fcall " ++ printTypedAddr f ++ " / " ++ show n ++ "\n" ++ printTAC xs
printTAC (TacInstruction (TAC.ProcedureCall p n) : xs) = "\tpcall " ++ printTypedAddr p ++ " / " ++ show n ++ "\n" ++ printTAC xs
printTAC (TacInstruction (FunctionParam a) : xs) = "\tparam " ++ printTypedAddr a ++ "\n" ++ printTAC xs
printTAC (TacInstruction NoOperation : xs) = "\t\n" ++ printTAC xs
