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
    btype :: TS.Type,
    addr :: Address
    }
    | Constant {
    id :: String,
    modality :: Modality,
    pos :: (Int, Int),
    btype :: TS.Type,
    addr :: Address,
    value :: String
    }
    | Array {
    id :: String,
    modality :: Modality,
    pos :: (Int, Int),
    btype :: TS.Type,
    arrLength :: [Int],
    addr :: Address
    }
    | Function {
    id :: String,
    pos :: (Int, Int),
    btype :: TS.Type,
    params :: [(Modality, TS.Type)],
    addr :: Address
    }
    | Prototype 
    {
    id :: String,
    pos :: (Int, Int),
    btype :: TS.Type,
    params :: [(Modality, TS.Type)],
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

mkVar :: String -> Modality -> (Int, Int) -> TS.Type -> Address -> EnvEntity
mkVar varName mod varPos varType addr = Variable varName mod varPos varType addr

mkArray :: String -> Modality -> (Int, Int) -> TS.Type -> [Int] -> Address -> EnvEntity
mkArray varName mod varPos varType arrLength addr = Array varName mod varPos varType arrLength addr

mkFunc :: String -> (Int, Int) -> TS.Type -> [(Modality, TS.Type)] -> Address -> EnvEntity
mkFunc funcName funcPos funcType funcParams addr = Env.Function funcName funcPos funcType funcParams addr

-- inserts only if not already in the environment
insertVar :: String -> Modality -> (Int, Int) -> TS.Type -> Address -> EnvT -> EnvT
insertVar varName mod varPos varType addr env= if containsEntry varName env
    then env
    else Map.insert varName (mkVar varName mod varPos varType addr) env 

insertConst :: String -> Modality -> (Int, Int) -> TS.Type -> Address -> String -> EnvT -> EnvT
insertConst varName mod varPos varType addr value env = if containsEntry varName env
    then env
    else Map.insert varName (Constant varName mod varPos varType addr value) env

insertArray :: String -> Modality -> (Int, Int) -> TS.Type -> [Int] -> Address -> EnvT -> EnvT
insertArray varName mod varPos varType arrLength addr env = if containsEntry varName env
    then env
    else Map.insert varName (mkArray varName mod varPos varType arrLength addr) env

insertFunc :: String -> (Int, Int) -> TS.Type -> [(Modality, TS.Type)] -> Address -> EnvT -> EnvT
insertFunc funcName funcPos funcType funcParams addr env = if containsEntry funcName env
    then if containsPrototype funcName env
        then Map.insert funcName (mkFunc funcName funcPos funcType funcParams addr) env
        else env
    else Map.insert funcName (mkFunc funcName funcPos funcType funcParams addr) env

insertPrototype :: String -> (Int, Int) -> TS.Type -> [(Modality, TS.Type)] -> Address -> EnvT -> EnvT
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

getVarMod :: String -> EnvT -> Modality
getVarMod varName env = case Map.lookup varName env of
    Just entry  -> modality entry
    Nothing     -> Modality1

getVarPos :: String -> EnvT -> (Int, Int)
getVarPos varName env = case Map.lookup varName env of
    Just entry  -> pos entry
    Nothing     -> (0,0)

getVarType :: String -> EnvT -> TS.Type
getVarType varName env = case Map.lookup varName env of
    Just entry  -> btype entry
    Nothing     -> Base (ERROR ("Variable '" ++ varName ++ "' not declared"))

getConstValue :: String -> EnvT -> String
getConstValue varName env = case Map.lookup varName env of
    Just entry  -> value entry
    Nothing     -> error ("Constant '" ++ varName ++ "' not declared")

getArrayType :: String -> EnvT -> TS.Type
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

getFuncType :: String -> EnvT -> TS.Type 
getFuncType funcName env = case Map.lookup funcName env of
    Just entry -> btype entry
    Nothing     -> Base (ERROR ("Function '" ++ funcName ++ "' not declared"))

getFuncParams :: String -> EnvT -> [(Modality, TS.Type)]
getFuncParams funcName env = case Map.lookup funcName env of
    Just entry -> params entry
    Nothing     -> [(Modality1, Base (ERROR ("Function '" ++ funcName ++ "' not declared")))]

getAddr :: String -> EnvT -> Address
getAddr varName env = case Map.lookup varName env of
    Just entry -> addr entry
    Nothing     -> error ("Variable '" ++ varName ++ "' not declared")
