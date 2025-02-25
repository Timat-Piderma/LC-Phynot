module ErrorsBuilder where

import TypeSystem as TS
import Env as E

mkAssignmentErrs :: Type -> Type -> (Int, Int) -> [String]
mkAssignmentErrs t1 t2 pos
  | isERROR t1 && isERROR t2 = [ mkSerr t1 pos, mkSerr t2 pos]
  | isERROR t1 = [ mkSerr t1 pos]
  | isERROR t2 = [ mkSerr t2 pos]
  | sup t1 t2 == t1 = []
  | otherwise = [ mkSerr (Base (ERROR ("Type mismatch: " ++ typeToString t1 ++ " and " ++ typeToString t2))) pos]

mkSerr :: Type -> (Int, Int) -> String
(mkSerr (Base (ERROR s))) (a,b) = "[" ++ show a ++ ":" ++ show b ++ "] " ++ s  ;
mkSerr _ _ = "Internal Error" -- Should never happen

mkIfErrs :: Type -> [String] -> (Int, Int) -> [String]
mkIfErrs t errs pos = case t of
  Base (ERROR e) ->  mkSerr (Base (ERROR (e ++ " in if guard expression"))) pos : errs
  Base BOOL -> errs
  _ -> mkSerr (Base (ERROR "Error: guard not bool")) pos : errs

mkDeclErrs :: EnvT -> String -> (Int, Int) -> [String]
mkDeclErrs env varName pos
    | containsEntry varName env = [mkSerr (Base (ERROR ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)))) pos]
    | otherwise = []

mkDeclInitErrs :: Type -> Type -> EnvT -> String -> (Int, Int) -> [String]
mkDeclInitErrs varType initType env varName pos
    | containsEntry varName env = [mkSerr (Base (ERROR ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)))) pos] 
    | isERROR varType && isERROR initType = [ mkSerr varType pos , mkSerr initType pos]
    | isERROR varType = [ mkSerr varType pos]
    | isERROR initType = [ mkSerr initType pos]
    | sup varType initType == varType = []
    | otherwise = [ mkSerr (Base (ERROR ("Type mismatch: can't convert " ++ typeToString varType ++ " to " ++ typeToString initType))) pos]

mkArrayDeclErrs :: Type -> EnvT -> String -> (Int, Int) -> [String]
mkArrayDeclErrs indexType env varName pos
    | containsEntry varName env = [mkSerr (Base (ERROR ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)))) pos]
    | sup indexType (Base INT) /= Base INT = [ mkSerr (Base (ERROR "Error: array index must be an integer")) pos]
    | otherwise = [] 

mkPointerDeclInitErrs :: Type -> Type -> EnvT -> String -> (Int, Int) -> [String]
mkPointerDeclInitErrs pointerType initType env varName pos
    | containsEntry varName env = [mkSerr (Base (ERROR ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)))) pos]
    | isERROR (sup pointerType initType) = [ mkSerr (sup pointerType initType) pos]
    | otherwise = []

mkParamErrs :: String -> String -> EnvT -> (Int, Int) -> [String]
mkParamErrs parName funcName env pos
    | containsEntry parName env = [mkSerr (Base (ERROR ("Duplicate paramater '" ++ parName ++ "' in function declaration: '" ++ funcName ++ "'"))) pos]
    | otherwise = []

prettyFuncErr :: [String] -> String -> [String]
prettyFuncErr errs funcName = map (++ " inside function '" ++ funcName ++ "'") errs

mkFuncDeclErrs :: Type -> EnvT -> String -> [Type] -> (Int, Int) -> [String]
mkFuncDeclErrs funcType env funcName params pos
    | containsEntry funcName env = [mkSerr (Base (ERROR ("Function '" ++ funcName ++ "' already declared at: " ++ show (getVarPos funcName env)))) pos] 
    | otherwise = []

mkReturnErrs :: EnvT -> Type -> (Int, Int) -> [String]
mkReturnErrs env retType pos
    | getVarType "return" env == retType = []
    | isERROR retType = [mkSerr retType pos]
    | containsEntry "return" env = [mkSerr (Base (ERROR ("Error: the return value " ++ typeToString retType ++" is not " ++ typeToString (getVarType "return" env)))) pos]
    | otherwise = [ mkSerr (Base (ERROR "Error: return statement outside function")) pos]

mkFuncCallErrs :: String -> [Type] -> EnvT -> (Int, Int) -> [String]
mkFuncCallErrs funcName params env pos
    | containsEntry funcName env && (params == getFuncParams funcName env) = []
    | containsEntry funcName env && (length params /=  length (getFuncParams funcName env)) = [mkSerr (Base (ERROR ("Error: function '" ++ funcName ++ "' expects " ++ show (length (getFuncParams funcName env)) ++ " parameters, found: " ++ show (length params)))) pos]
    | containsEntry funcName env = mkFuncCallParamErrs funcName params (getFuncParams funcName env) pos
    | otherwise = []

mkFuncCallParamErrs :: String -> [Type] -> [Type] -> (Int, Int) -> [String]
mkFuncCallParamErrs _ [] [] _= []
mkFuncCallParamErrs funcName (x:xs) (y:ys) pos
    | x == y    = mkFuncCallParamErrs funcName xs ys pos
    | otherwise = mkSerr (Base (ERROR ("Error: can't match " ++ typeToString x ++ " with expected type " ++ typeToString y ++ " in function '" ++ funcName ++ "' call"))) pos : mkFuncCallParamErrs funcName xs ys pos

mkProcedureCallErrs :: String -> [Type] -> EnvT -> (Int, Int) -> [String]
mkProcedureCallErrs procName params env pos
    | containsEntry procName env && (params == getFuncParams procName env) = []
    | containsEntry procName env && (length params /=  length (getFuncParams procName env)) = [mkSerr (Base (ERROR ("Error: function '" ++ procName ++ "' expects " ++ show (length (getFuncParams procName env)) ++ " parameters, found: " ++ show (length params)))) pos]
    | containsEntry procName env = mkFuncCallParamErrs procName params (getFuncParams procName env) pos
    | not (containsEntry procName env) = [mkSerr (Base (ERROR ("Error: function '" ++ procName ++ "' not declared"))) pos]
    | otherwise = []