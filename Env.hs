module Env where
import Data.Map as Map

--import TAC
import TypeSystem as TS 
import AbsPhynot as Abs

type EnvT = Map.Map String EnvEntity

data EnvEntity = 
    Variable {
    id :: String,
    pos :: (Int, Int),
    btype :: Type
    }
    | Array {
    id :: String,
    pos :: (Int, Int),
    btype :: Type,
    arrLength :: [Int]
    }
    | Function {
    id :: String,
    pos :: (Int, Int),
    btype :: Type,
    params :: [Type]
    }
    | Prototype 
    {
    id :: String,
    pos :: (Int, Int),
    btype :: Type,
    params :: [Type]
    }
    deriving (Show, Read)

emptyEnv :: EnvT
emptyEnv = Map.insert "writeInt" (mkFunc "writeInt" (-1, -1) (Base NONE) [Base INT]) (
    Map.insert "writeFloat" (mkFunc "writeFloat" (-1, -1) (Base NONE) [Base FLOAT]) (
    Map.insert "writeChar" (mkFunc "writeChar" (-1, -1) (Base NONE) [Base CHAR]) (
    Map.insert "writeString" (mkFunc "writeString" (-1, -1) (Base NONE) [Base STRING]) (
    Map.insert "readInt" (mkFunc "readInt" (-1, -1) (Base INT) []) (
    Map.insert "readFloat" (mkFunc "readFloat" (-1, -1) (Base FLOAT) []) (
    Map.insert "readChar" (mkFunc "readChar" (-1, -1) (Base CHAR) []) (
    Map.insert "readString" (mkFunc "readString" (-1, -1) (Base STRING) []) Map.empty
    )))))))

mkVar :: String -> (Int, Int) -> Type -> EnvEntity
mkVar varName varPos varType = Variable varName varPos varType

mkArray :: String -> (Int, Int) -> Type -> [Int] -> EnvEntity
mkArray varName varPos varType arrLength = Array varName varPos varType arrLength

mkFunc :: String -> (Int, Int) -> Type -> [Type] -> EnvEntity
mkFunc funcName funcPos funcType funcParams = Function funcName funcPos funcType funcParams

-- inserts only if not already in the environment
insertVar :: String -> (Int, Int) -> Type -> EnvT -> EnvT
insertVar varName varPos varType env = if containsEntry varName env
    then env
    else Map.insert varName (mkVar varName varPos varType) env 

insertArray :: String -> (Int, Int) -> Type -> [Int] -> EnvT -> EnvT
insertArray varName varPos varType arrLength env = if containsEntry varName env
    then env
    else Map.insert varName (mkArray varName varPos varType arrLength) env

insertFunc :: String -> (Int, Int) -> Type -> [Type] -> EnvT -> EnvT
insertFunc funcName funcPos funcType funcParams env = if containsEntry funcName env
    then if containsPrototype funcName env
        then Map.insert funcName (mkFunc funcName funcPos funcType funcParams) env
        else env
    else Map.insert funcName (mkFunc funcName funcPos funcType funcParams) env

insertPrototype :: String -> (Int, Int) -> Type -> [Type] -> EnvT -> EnvT
insertPrototype funcName pos funcType funcParams env = if containsPrototype funcName env
    then env
    else Map.insert funcName (Prototype funcName pos funcType funcParams) env

containsEntry :: String -> EnvT -> Bool
containsEntry varName env = 
    case Map.lookup varName env of
        Just _  -> True  
        Nothing -> False  

containsPrototype :: String -> EnvT -> Bool
containsPrototype funcName env = 
    case Map.lookup funcName env of
        Just (Prototype _ _ _ _)  -> True  
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

getFuncParams :: String -> EnvT -> [Type]
getFuncParams funcName env = case Map.lookup funcName env of
    Just entry -> params entry
    Nothing     -> [Base (ERROR ("Function '" ++ funcName ++ "' not declared"))]