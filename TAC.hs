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

data TACInstruction = BinaryOperation Address Address Address BinaryOp     -- l = r1 bop r2
                    | UnaryOperation Address Address UnaryOp               -- l = uop r
                    | NullaryOperation Address Address                     -- l = r
                    | UnconditionalJump Label                              -- goto label  
                    | ConditionalJump Address Label                        -- if r goto label
                    | RelationalJump Address Address Label RelationalOp    -- if r1 rop r2 goto label  ;  ifFalse r goto label
                    | IndexedCopyAssignment Address Address Address        -- l = id[r2]  ;  id[r1] = r2
                    | ReferenceAssignment Address Address                  -- l = &id  ;  l1 = *l2  ;  *l = r
                    | Function                                             -- param r ; pcall proc, n  ;  l = fcall fun, n  ;  return  ;  return r
    deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Exp | Div | Mod
    deriving (Eq, Show)

data UnaryOp = Neg | Not
    deriving (Eq, Show)

data RelationalOp = Eq | Ne | Lt | Le | Gt | Ge
    deriving (Eq, Show)

generateAddr :: TS.Type -> String -> Address
generateAddr bt s = case bt of
    TS.Base TS.INT -> ProgVar (ProgVariable s) IntegerType
    TS.Base TS.FLOAT -> ProgVar (ProgVariable s) FloatType
    TS.Base TS.BOOL -> ProgVar (ProgVariable s) BooleanType
    TS.Base TS.CHAR -> ProgVar (ProgVariable s) CharType
    TS.Base TS.STRING -> ProgVar (ProgVariable s) StringType

generateLit :: TS.Type -> Value -> Address
generateLit bt val = case (bt, val) of
    (TS.Base TS.INT, IntVal i) -> TacLit (IntLit i) IntegerType
    (TS.Base TS.FLOAT, DoubleVal d) -> TacLit (FloatLit d) FloatType
    (TS.Base TS.BOOL, BoolVal b) -> TacLit (BoolLit b) BooleanType
    (TS.Base TS.CHAR, CharVal c) -> TacLit (CharLit c) CharType
    (TS.Base TS.STRING, StringVal s) -> TacLit (StringLit s) StringType
    _ -> error "Type and value do not match"

type State = Int

newtemp :: State -> TS.Type -> Address
newtemp k t = case t of
    TS.Base TS.INT -> Temporary ("t" ++ show k) IntegerType
    TS.Base TS.FLOAT -> Temporary ("t" ++ show k) FloatType
    TS.Base TS.BOOL -> Temporary ("t" ++ show k) BooleanType
    TS.Base TS.CHAR -> Temporary ("t" ++ show k) CharType
    TS.Base TS.STRING -> Temporary ("t" ++ show k) StringType
