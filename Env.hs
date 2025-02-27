module Env where
import Data.Map as Map

--import TAC
import TypeSystem as TS 
import AbsPhynot as Abs

type EnvT = Map.Map String EnvEntity

data EnvEntity = Variable {
    id :: String,
    pos :: (Int, Int),
    btype :: Type,
    params :: [Type]
    }
    | Array {
    id :: String,
    pos :: (Int, Int),
    btype :: Type,
    dimensions :: Int
    }
    deriving (Show, Read)

emptyEnv :: EnvT
emptyEnv = Map.empty

mkVar :: String -> (Int, Int) -> Type -> EnvEntity
mkVar varName varPos varType = Variable varName varPos varType []

mkArray :: String -> (Int, Int) -> Type -> Int -> EnvEntity
mkArray varName varPos varType dim = Array varName varPos varType dim

-- inserts only if not already in the environment
insertVar :: String -> (Int, Int) -> Type -> EnvT -> EnvT
insertVar varName varPos varType env = if containsEntry varName env
    then env
    else Map.insert varName (mkVar varName varPos varType) env 

insertArray :: String -> (Int, Int) -> Type -> Int -> EnvT -> EnvT
insertArray varName varPos varType dim env = if containsEntry varName env
    then env
    else Map.insert varName (mkArray varName varPos varType dim) env

insertFunc :: String -> (Int, Int) -> Type -> [Type] -> EnvT -> EnvT
insertFunc funcName funcPos funcType funcParams env = if containsEntry funcName env
    then env
    else Map.insert funcName (Variable funcName funcPos funcType funcParams) env

containsEntry :: String -> EnvT -> Bool
containsEntry varName env = 
    case Map.lookup varName env of
        Just _  -> True  
        Nothing -> False  

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
        ARRAY _ -> dimensions entry
        _       -> 0
    Nothing     -> 0

getFuncType :: String -> EnvT -> Type 
getFuncType funcName env = case Map.lookup funcName env of
    Just entry -> btype entry
    Nothing     -> Base (ERROR ("Function '" ++ funcName ++ "' not declared"))

getFuncParams :: String -> EnvT -> [Type]
getFuncParams funcName env = case Map.lookup funcName env of
    Just entry -> params entry
    Nothing     -> [Base (ERROR ("Function '" ++ funcName ++ "' not declared"))]