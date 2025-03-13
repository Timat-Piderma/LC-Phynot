module TAC (
    module TAC
) where

import AbsPhynot
import TypeSystem as TS

data Address = ProgVar{ progVar :: ProgVariable, addrBT :: TACBasicType}
            | TacLit{ tacLit :: Literal, addrBT :: TACBasicType}
            | Temporary{ tempName :: String, addrBT :: TACBasicType}
    deriving (Eq, Show)

data TACBasicType = IntegerType | FloatType | CharType | BooleanType | StringType | MemoryAddressType
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

data TACInstruction = BinaryOperation Address Address Address BinaryOp     -- l = r1 bop r2
                    | UnaryOperation Address Address UnaryOp               -- l = uop r
                    | NullaryOperation Address Address                     -- l = r
                    | UnconditionalJump Label                              -- goto label  
                    | ConditionalJump Address Label                        -- if r goto label
                    | IndexedAssignment Address Address Address            -- id[r1] = r2
                    | IndexedCopyAssignment Address Address Address        -- r1 = id[r2]
                    | FunctionDef [Address]                                -- def r1 (r2, r3, ...) {
                    | EndFunction
                    | Return Address                                       -- return r
                    | ReturnVoid                                           -- return
                    | FunctionCall Address Address Int                     -- r = fcall fun / n
                    | ProcedureCall Address Int                            -- pcall proc / n
                    | FunctionParam Address                                -- param r 
                    | NoOperation                                          -- nop
    deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Exp | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge | And | Or
    deriving (Eq, Show)

data UnaryOp = Neg | Not | Ref | Deref
    deriving (Eq, Show)

generateAddr :: TS.Type -> String -> Address
generateAddr bt s = case bt of
    TS.Base TS.INT -> ProgVar (ProgVariable s) IntegerType
    TS.Base TS.FLOAT -> ProgVar (ProgVariable s) FloatType
    TS.Base TS.BOOL -> ProgVar (ProgVariable s) BooleanType
    TS.Base TS.CHAR -> ProgVar (ProgVariable s) CharType
    TS.Base TS.STRING -> ProgVar (ProgVariable s) StringType
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

mkArrayIndex :: Int -> [Int] -> [Int] -> Int -> Int
mkArrayIndex tsize [] _ tot = tot
mkArrayIndex tsize index arrsize tot = mkArrayIndex tsize (tail index) (tail arrsize) (head index * product (tail arrsize) * tsize + tot)

generateArrayEmpty :: Address -> [Int] -> TS.Type -> [TAC]
generateArrayEmpty a x t  = case t of
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

generateArray' :: Address -> Int -> Int -> [Address] -> Int -> [TAC]
generateArray' a 0 size vals c = []
generateArray' a x size vals  c = TacInstruction (IndexedAssignment a (generateLit (TS.Base TS.INT) (IntVal (toInteger (c * size)))) (head vals))
    : generateArray' a (x-1) size (tail vals) (c+1)

generateFuncDef :: Address -> [(String, (Int, Int), TS.Type)] -> TAC
generateFuncDef f d = TacInstruction (FunctionDef (f : map (\(s, (x, y), t) -> generateAddr t (s++"@"++show x)) d))

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


newLabel :: State -> Label
newLabel (k, l) = Label ("L" ++ show l)

printBinaryOp :: BinaryOp -> String
printBinaryOp TAC.Add = "+"
printBinaryOp TAC.Sub = "-"
printBinaryOp TAC.Mul = "*"
printBinaryOp TAC.Exp = "^"
printBinaryOp TAC.Div = "/"
printBinaryOp TAC.Mod = "%"
printBinaryOp TAC.Eq = "=="
printBinaryOp TAC.Ne = "!="
printBinaryOp TAC.Lt = "<"
printBinaryOp TAC.Le = "<="
printBinaryOp TAC.Gt = ">"
printBinaryOp TAC.Ge = ">="
printBinaryOp TAC.And = "&&"
printBinaryOp TAC.Or = "||"

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

printTAC :: [TAC] -> String
printTAC [] = ""
printTAC (LabelledInstruction (Label l) i : xs) = l ++ ":" ++ printTAC (TacInstruction i : xs)
printTAC (TacInstruction (BinaryOperation a1 a2 a3 op) : xs) = "\t" ++ printAddr a1 ++ " = " ++ printAddr a2 ++ " " ++ printBinaryOp op ++ " " ++ printAddr a3 ++ "\n" ++ printTAC xs
printTAC (TacInstruction (UnaryOperation a1 a2 op) : xs) = "\t" ++ printAddr a1 ++ " = " ++ printUnaryOp op ++ " " ++ printAddr a2 ++ "\n" ++ printTAC xs
printTAC (TacInstruction (NullaryOperation a1 a2) : xs) = "\t" ++ printAddr a1 ++ " = " ++ printAddr a2 ++ "\n" ++ printTAC xs
printTAC (TacInstruction (UnconditionalJump (Label l)) : xs) = "\tgoto " ++ l ++ "\n" ++ printTAC xs
printTAC (TacInstruction (ConditionalJump a1 (Label l)) : xs) = "\tifFalse " ++ printAddr a1 ++ " goto " ++ l ++ "\n" ++ printTAC xs
printTAC (TacInstruction (IndexedAssignment a1 a2 a3) : xs) = "\t" ++ printAddr a1 ++ "[" ++ printAddr a2 ++ "] = " ++ printAddr a3 ++ "\n" ++ printTAC xs
printTAC (TacInstruction (IndexedCopyAssignment a1 a2 a3) : xs) = "\t" ++ printAddr a1 ++ " = " ++ printAddr a2 ++ "[" ++ printAddr a3 ++ "]\n" ++ printTAC xs
printTAC (TacInstruction (FunctionDef (f:addrs)) : xs) = "def " ++ printAddr f ++ " (" ++ concatMap printAddr addrs  ++ ") {\n"++ printTAC xs
printTAC (TacInstruction EndFunction : xs) = "}\n" ++ printTAC xs
printTAC (TacInstruction (TAC.Return a) : xs) = "\treturn " ++ printAddr a ++ "\n" ++ printTAC xs
printTAC (TacInstruction ReturnVoid : xs) = "\treturn\n" ++ printTAC xs
printTAC (TacInstruction (FunctionCall a f n) : xs) = "\t" ++ printAddr a ++ " = fcall " ++ printAddr f ++ " / " ++ show n ++ "\n" ++ printTAC xs
printTAC (TacInstruction (TAC.ProcedureCall p n) : xs) = "\tpcall " ++ printAddr p ++ " / " ++ show n ++ "\n" ++ printTAC xs
printTAC (TacInstruction (FunctionParam a) : xs) = "\tparam " ++ printAddr a ++ "\n" ++ printTAC xs
printTAC (TacInstruction NoOperation : xs) = "\t\n" ++ printTAC xs
