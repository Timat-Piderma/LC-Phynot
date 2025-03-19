module Env where
import Data.Map as Map

import TAC
import TypeSystem as TS 
import AbsPhynot as Abs

type EnvT = Map.Map String EnvEntity

data EnvEntity = 
    Variable {
    id :: String,
    modality :: Modality,
    pos :: (Int, Int),
    btype :: Type,
    addr :: Address
    }
    |Constant {
    id :: String,
    pos :: (Int, Int),
    btype :: Type,
    addr :: Address
    }
    | Array {
    id :: String,
    pos :: (Int, Int),
    btype :: Type,
    arrLength :: [Int],
    addr :: Address
    }
    | Function {
    id :: String,
    pos :: (Int, Int),
    btype :: Type,
    params :: [(Modality, Type)],
    addr :: Address
    }
    | Prototype 
    {
    id :: String,
    pos :: (Int, Int),
    btype :: Type,
    params :: [(Modality, Type)],
    addr :: Address
    }
    deriving (Show)

emptyEnv :: EnvT
emptyEnv = Map.insert "writeInt" (mkFunc "writeInt" (-1, -1) (Base NONE) [(Modality1, Base INT)] (ProgVar (ProgVariable "writeInt") MemoryAddressType)) (
    Map.insert "writeFloat" (mkFunc "writeFloat" (-1, -1) (Base NONE) [(Modality1, Base FLOAT)] (ProgVar (ProgVariable "writeFloat") MemoryAddressType)) (
    Map.insert "writeChar" (mkFunc "writeChar" (-1, -1) (Base NONE) [(Modality1, Base CHAR)] (ProgVar (ProgVariable "writeString") MemoryAddressType)) (
    Map.insert "writeString" (mkFunc "writeString" (-1, -1) (Base NONE) [(Modality1, Base STRING)] (ProgVar (ProgVariable "writeChar") MemoryAddressType)) (
    Map.insert "readInt" (mkFunc "readInt" (-1, -1) (Base INT) [] (ProgVar (ProgVariable "readInt") IntegerType)) (
    Map.insert "readFloat" (mkFunc "readFloat" (-1, -1) (Base FLOAT) [] (ProgVar (ProgVariable "readFloat") FloatType)) (
    Map.insert "readChar" (mkFunc "readChar" (-1, -1) (Base CHAR) [] (ProgVar (ProgVariable "readChar") CharType)) (
    Map.insert "readString" (mkFunc "readString" (-1, -1) (Base STRING) [] (ProgVar (ProgVariable "readString") StringType)) Map.empty
    )))))))

mkVar :: String -> Modality -> (Int, Int) -> Type -> Address -> EnvEntity
mkVar varName mod varPos varType addr = Variable varName mod varPos varType addr

mkArray :: String -> (Int, Int) -> Type -> [Int] -> Address -> EnvEntity
mkArray varName varPos varType arrLength addr = Array varName varPos varType arrLength addr

mkFunc :: String -> (Int, Int) -> Type -> [(Modality, Type)] -> Address -> EnvEntity
mkFunc funcName funcPos funcType funcParams addr = Env.Function funcName funcPos funcType funcParams addr

-- inserts only if not already in the environment
insertVar :: String -> Modality -> (Int, Int) -> Type -> Address -> EnvT -> EnvT
insertVar varName mod varPos varType addr env= if containsEntry varName env
    then env
    else Map.insert varName (mkVar varName mod varPos varType addr) env 

insertConst :: String -> (Int, Int) -> Type -> Address -> EnvT -> EnvT
insertConst cosName varPos varType addr env = if containsEntry cosName env
    then env
    else Map.insert cosName (Constant cosName varPos varType addr) env

insertArray :: String -> (Int, Int) -> Type -> [Int] -> Address -> EnvT -> EnvT
insertArray varName varPos varType arrLength addr env = if containsEntry varName env
    then env
    else Map.insert varName (mkArray varName varPos varType arrLength addr) env

insertFunc :: String -> (Int, Int) -> Type -> [(Modality, Type)] -> Address -> EnvT -> EnvT
insertFunc funcName funcPos funcType funcParams addr env = if containsEntry funcName env
    then if containsPrototype funcName env
        then Map.insert funcName (mkFunc funcName funcPos funcType funcParams addr) env
        else env
    else Map.insert funcName (mkFunc funcName funcPos funcType funcParams addr) env

insertPrototype :: String -> (Int, Int) -> Type -> [(Modality, Type)] -> Address -> EnvT -> EnvT
insertPrototype funcName pos funcType funcParams addr env = if containsPrototype funcName env
    then env
    else Map.insert funcName (Prototype funcName pos funcType funcParams addr) env

containsEntry :: String -> EnvT -> Bool
containsEntry varName env = 
    case Map.lookup varName env of
        Just _  -> True  
        Nothing -> False  

containsPrototype :: String -> EnvT -> Bool
containsPrototype funcName env = 
    case Map.lookup funcName env of
        Just (Prototype _ _ _ _ _)  -> True  
        _                        -> False

getVarPos :: String -> EnvT -> (Int, Int)
getVarPos varName env = case Map.lookup varName env of
    Just entry  -> pos entry
    Nothing     -> (0,0)

getVarType :: String -> EnvT -> Type
getVarType varName env = case Map.lookup varName env of
    Just entry  -> btype entry
    Nothing     -> Base (ERROR ("Variable '" ++ varName ++ "' not declared"))

getArrayType :: String -> EnvT -> Type
getArrayType varName env = case Map.lookup varName env of
    Just entry -> case btype entry of
        ARRAY t -> ARRAY t
        _       -> Base (ERROR ("Variable '" ++ varName ++ "' is not an array"))
    Nothing     -> Base (ERROR ("Array '" ++ varName ++ "' not declared"))

getArrayDim :: String -> EnvT -> Int
getArrayDim varName env = case Map.lookup varName env of
    Just entry -> case btype entry of
        ARRAY _ -> length (arrLength entry)
        _       -> 0
    Nothing     -> 0

getArrayLength :: String -> EnvT -> [Int]
getArrayLength varName env = case Map.lookup varName env of
    Just entry -> case btype entry of
        ARRAY _ -> arrLength entry
        _       -> []
    Nothing     -> []

getFuncType :: String -> EnvT -> Type 
getFuncType funcName env = case Map.lookup funcName env of
    Just entry -> btype entry
    Nothing     -> Base (ERROR ("Function '" ++ funcName ++ "' not declared"))

getFuncParams :: String -> EnvT -> [(Modality, Type)]
getFuncParams funcName env = case Map.lookup funcName env of
    Just entry -> params entry
    Nothing     -> [(Modality1, Base (ERROR ("Function '" ++ funcName ++ "' not declared")))]

getAddr :: String -> EnvT -> Address
getAddr varName env = case Map.lookup varName env of
    Just entry -> addr entry
    Nothing     -> error ("Variable '" ++ varName ++ "' not declared")
