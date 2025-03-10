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

data TACInstruction = BinaryOperation Address Address Address BinaryOp     -- l = r1 bop r2
                    | UnaryOperation Address Address UnaryOp               -- l = uop r
                    | NullaryOperation Address Address                     -- l = r
                    | UnconditionalJump Label                              -- goto label  
                    | ConditionalJump Address Label                        -- if r goto label
                    | IndexedCopyAssignment Address Address Address        -- l = id[r2]  ;  id[r1] = r2
                    | ReferenceAssignment Address Address                  -- l = &id  ;  l1 = *l2  ;  *l = r
                    | FunctionDef [Address]                                -- pcall proc, n  ;  l = fcall fun, n  ;  return  ;  return r
                    | EndFunction
                    | FunctionCall Address Address                         -- r = fcall fun
                    | ProcedureCall Address                                -- pcall proc, n
                    | FunctionParam Address                                -- param r 
                    | NoOperation                                          -- nop
    deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Exp | Div | Mod | Eq | Ne | Lt | Le | Gt | Ge
    deriving (Eq, Show)

data UnaryOp = Neg | Not
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

generateFuncDef :: Address -> [(String, (Int, Int), TS.Type)] -> TAC
generateFuncDef f d = TacInstruction (FunctionDef (f : map (\(s, (x, y), t) -> generateAddr t (s++"@"++show x)) d))

generateFuncCall :: Address -> Address -> [Address] -> [TAC]
generateFuncCall t p d = map (TacInstruction . FunctionParam) d ++ [TacInstruction (TAC.FunctionCall t p)]

generateProcCall :: Address -> [Address] -> [TAC]
generateProcCall p d = map (TacInstruction . FunctionParam) d ++ [TacInstruction (TAC.ProcedureCall p)]

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

printUnaryOp :: UnaryOp -> String
printUnaryOp TAC.Neg = "-"
printUnaryOp TAC.Not = "!"

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
printTAC (TacInstruction (NullaryOperation a1 a2) : xs) = "\t" ++ printAddr a1 ++ " = " ++ printAddr a2 ++ "\n" ++ printTAC xs
printTAC (TacInstruction (UnconditionalJump (Label l)) : xs) = "\tgoto " ++ l ++ "\n" ++ printTAC xs
printTAC (TacInstruction (ConditionalJump a1 (Label l)) : xs) = "\tifFalse " ++ printAddr a1 ++ " goto " ++ l ++ "\n" ++ printTAC xs
printTAC (TacInstruction (FunctionDef (f:addrs)) : xs) = "def " ++ printAddr f ++ " (" ++ concatMap printAddr addrs  ++ ") {\n"++ printTAC xs
printTAC (TacInstruction EndFunction : xs) = "}\n" ++ printTAC xs
printTAC (TacInstruction (FunctionCall a f) : xs) = "\t" ++ printAddr a ++ " = fcall " ++ printAddr f ++ "\n" ++ printTAC xs
printTAC (TacInstruction (TAC.ProcedureCall p) : xs) = "\tpcall " ++ printAddr p ++ "\n" ++ printTAC xs
printTAC (TacInstruction (FunctionParam a) : xs) = "\tparam " ++ printAddr a ++ "\n" ++ printTAC xs
printTAC (TacInstruction NoOperation : xs) = "\t\n" ++ printTAC xs
