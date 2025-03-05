{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module ErrorsBuilder where

import Data.List
import TypeSystem as TS
import Env as E

mkAssignmentErrs :: Type -> Type -> (Int, Int) -> (Int, Int) -> [String]
mkAssignmentErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ = [s1, s2]
mkAssignmentErrs (Base (ERROR s)) _ _ _ = [s]
mkAssignmentErrs _ (Base (ERROR s)) _ _ = [s]
mkAssignmentErrs varType assType varPos assPos
    | sup varType assType == varType = []
    | otherwise = [ mkStringError  ("Type mismatch: can't assign " ++ typeToString assType ++ " value to " ++ typeToString varType ++ " variable") varPos]

mkArrayLenErrs :: String -> [Int] -> [Int] -> (Int, Int) -> [String]
mkArrayLenErrs _ [] [] _ = []

mkArrayLenErrs varName (x:varLength) (y:assLength) pos
    | length (x:varLength) /= length (y:assLength) = [mkStringError ("Error: can't assign array of length " ++ show y ++ " to array " ++ varName ++ " of length " ++ show x) pos]
    | x >= y = mkArrayLenErrs varName varLength assLength pos
    | otherwise = mkStringError ("Error: can't assign array of length " ++ show y ++ " to array " ++ varName ++ " of length " ++ show x) pos : mkArrayLenErrs varName varLength assLength pos

mkStringError :: String -> (Int, Int) -> String
mkStringError s (a, b) = "[" ++ show a ++ ":" ++ show b ++ "] " ++ s

mkError :: String -> (Int, Int) -> Type
mkError s (a, b) = Base (ERROR (mkStringError s (a, b)));

mkIfErrs :: Type -> (Int, Int) -> [String]
mkIfErrs t pos = case t of
  Base (ERROR e) -> [e]
  Base BOOL -> []
  _ -> [mkStringError ("Error: if statement guard not boolean, found: " ++ typeToString t) pos]

mkWhileErrs :: Type -> (Int, Int) -> [String]
mkWhileErrs t pos = case t of
  Base (ERROR e) -> [e]
  Base BOOL -> []
  _ ->[mkStringError ("Error: while statement guard not boolean, found: " ++ typeToString t) pos]

prettySequenceErr :: String -> [String] -> [String]
prettySequenceErr blockName errs = map (++ " inside '" ++ blockName ++ "' block") errs

mkDeclErrs :: EnvT -> String -> (Int, Int) -> [String]
mkDeclErrs env varName pos
    | containsEntry varName env = [mkStringError  ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)) pos]
    | otherwise = []

mkDeclInitErrs :: Type -> Type -> EnvT -> String -> (Int, Int) -> [String]
mkDeclInitErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ _ = [s1, s2]
mkDeclInitErrs (Base (ERROR s)) _ _ _ _= [s]
mkDeclInitErrs _ (Base (ERROR s)) _ _ _= [s]
mkDeclInitErrs varType initType env varName pos
    | containsEntry varName env = [mkStringError ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)) pos] 
    | sup varType initType == varType = []
    | otherwise = [ mkStringError ("Type mismatch: can't convert " ++ typeToString initType ++ " to " ++ typeToString varType) pos]

mkArrayDeclErrs :: EnvT -> String -> (Int, Int) -> [String]
mkArrayDeclErrs env varName pos
    | containsEntry varName env = [mkStringError ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)) pos]
    | otherwise = [] 

mkArrayDeclInitErrs :: EnvT -> String -> Type -> Type -> (Int, Int) -> [String]
mkArrayDeclInitErrs _ _ (Base (ERROR s1)) (Base (ERROR s2)) _ = [s1, s2]
mkArrayDeclInitErrs _ _ (Base (ERROR s)) _ _ = [s]
mkArrayDeclInitErrs _ _ _ (Base (ERROR s)) _ = [s]
mkArrayDeclInitErrs env varName arrType valType  pos
    | containsEntry varName env = [mkStringError ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)) pos]
    | isERROR (TS.sup arrType valType) = [mkStringError (TS.getErrorMessage (sup arrType valType)) pos]
    | otherwise = []

mkArrayIndexErrs :: Type -> (Int, Int) -> [String]
mkArrayIndexErrs (Base (ERROR s)) _ = [s]
mkArrayIndexErrs t pos
    | isInt t = []
    | otherwise = [ mkStringError ("Array index must be an integer, found: " ++ typeToString t) pos]

mkNotErrs :: Type -> (Int, Int) -> [String]
mkNotErrs (Base (ERROR s)) _ = [s]
mkNotErrs t pos
    | isBoolean t = []
    | otherwise = [ mkStringError ("'not' expects a boolean parameter, found " ++ typeToString t) pos]

mkPointerDeclInitErrs :: Type -> Type -> EnvT -> String -> (Int, Int) -> [String]
mkPointerDeclInitErrs pointType derefType env varName pos
    | isERROR pointType && isERROR derefType = [TS.getErrorMessage pointType, TS.getErrorMessage derefType]
    | isERROR derefType = [TS.getErrorMessage derefType]
    | isERROR pointType = [TS.getErrorMessage pointType]
    | containsEntry varName env = [mkStringError ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)) pos]
    | isERROR (TS.sup pointType derefType) = [mkStringError (TS.getErrorMessage (sup pointType derefType)) pos]
    | otherwise = []

mkParamErrs :: String -> String -> EnvT -> (Int, Int) -> [String]
mkParamErrs parName funcName env pos
    | containsEntry parName env = [mkStringError("Duplicate paramater '" ++ parName ++ "' in function declaration: '" ++ funcName ++ "'") pos]
    | otherwise = []

prettyFuncErr :: [String] -> String -> [String]
prettyFuncErr errs funcName = map (++ " inside function '" ++ funcName ++ "'") errs

mkFuncDeclErrs :: Type -> EnvT -> String -> [Type] -> (Int, Int) -> [String]
mkFuncDeclErrs funcType env funcName params pos
    | getVarPos funcName env == (-1,-1) = [mkStringError ("Primitive function '" ++ funcName ++ "' can not be redefined") pos] 
    | containsPrototype funcName env = protoToFuncErrs funcType funcName params env pos
    | containsEntry funcName env = [mkStringError ("Function '" ++ funcName ++ "' already declared at: " ++ show (getVarPos funcName env)) pos] 
    | otherwise = []

protoToFuncErrs :: Type -> String -> [Type] -> EnvT -> (Int, Int) -> [String]
protoToFuncErrs (Base (ERROR s)) _ _ _ _ = [s]
protoToFuncErrs funcType funcName params env pos
    | getFuncType funcName env == funcType && getFuncParams funcName env == params = []
    | getFuncType funcName env /= funcType && getFuncParams funcName env /= params = [mkStringError ("Error: function '" ++ funcName ++ "' has different return type: '" ++ typeToString (getFuncType funcName env) ++ "' and parameters: '" ++ intercalate ", " (map typeToString (getFuncParams funcName env)) ++ "' than prototype") pos]
    | getFuncType funcName env /= funcType = [mkStringError ("Error: function '" ++ funcName ++ "' has different return type: '" ++ typeToString (getFuncType funcName env) ++ "' than prototype") pos]
    | getFuncParams funcName env /= params = [mkStringError ("Error: function '" ++ funcName ++ "' has different parameters: '" ++ intercalate ", " (map typeToString (getFuncParams funcName env)) ++ "' than prototype") pos]
    | otherwise = []

mkPrototypeErrs :: Type -> EnvT -> String -> [Type] -> (Int, Int) -> [String]
mkPrototypeErrs funcType env funcName params pos
    | containsEntry funcName env = [mkStringError ("Prototype function '" ++ funcName ++ "' already declared at: " ++ show (getVarPos funcName env)) pos] 
    | otherwise = []

mkReturnErrs :: EnvT -> Type -> (Int, Int) -> [String]
mkReturnErrs _ (Base (ERROR s)) _ = [s]
mkReturnErrs env retType pos
    | getVarType "return" env == retType = []
    | containsEntry "return" env = [mkStringError ("Error: the return value " ++ typeToString retType ++" is not " ++ typeToString (getVarType "return" env)) pos]
    | otherwise = [ mkStringError "Error: return statement outside function" pos]

mkFuncCallErrs :: String -> [Type] -> EnvT -> (Int, Int) -> [String]
mkFuncCallErrs funcName params env pos
    | funcName == "writeInt" && length params == 1 && (mathtype (head params) == Base INT) = []
    | funcName == "writeFloat" && length params == 1 && (mathtype (head params) == Base FLOAT) = []
    | funcName == "writeChar" && length params == 1 && (mathtype (head params) == Base CHAR) = []
    | funcName == "writeString" && length params == 1 && (head params == Base STRING) = []
    | containsEntry funcName env && (params == getFuncParams funcName env) = []
    | containsEntry funcName env && (length params /=  length (getFuncParams funcName env)) = [mkStringError ("Error: function '" ++ funcName ++ "' expects " ++ show (length (getFuncParams funcName env)) ++ " parameters, found: " ++ show (length params)) pos]
    | containsEntry funcName env = mkFuncCallParamErrs funcName params (getFuncParams funcName env) pos
    | otherwise = []

mkFuncCallParamErrs :: String -> [Type] -> [Type] -> (Int, Int) -> [String]
mkFuncCallParamErrs _ [] [] _= []
mkFuncCallParamErrs funcName (x:xs) (y:ys) pos
    | x == y    = mkFuncCallParamErrs funcName xs ys pos
    | otherwise = mkStringError ("Error: can't match " ++ typeToString x ++ " with expected type " ++ typeToString y ++ " in function '" ++ funcName ++ "' call") pos : mkFuncCallParamErrs funcName xs ys pos

mkProcedureCallErrs :: String -> [Type] -> EnvT -> (Int, Int) -> [String]
mkProcedureCallErrs procName params env pos
    | containsEntry procName env && (params == getFuncParams procName env) = []
    | containsEntry procName env && (length params /=  length (getFuncParams procName env)) = [mkStringError ("Error: function '" ++ procName ++ "' expects " ++ show (length (getFuncParams procName env)) ++ " parameters, found: " ++ show (length params)) pos]
    | containsEntry procName env = mkFuncCallParamErrs procName params (getFuncParams procName env) pos
    | not (containsEntry procName env) = [mkStringError ("Error: function '" ++ procName ++ "' not declared") pos]
    | otherwise = []

mkBoolRelErrs :: Type -> Type -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [String]
mkBoolRelErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ _ = [s1, s2]
mkBoolRelErrs (Base (ERROR s)) _ _ _ _ = [s]
mkBoolRelErrs _ (Base (ERROR s)) _ _ _ = [s]
mkBoolRelErrs t1 t2 t1Pos t2Pos relPos
    | sup t1 t2 == Base BOOL = []
    | otherwise = [ mkStringError ("Type mismatch: can't compare " ++ typeToString t1 ++ " with " ++ typeToString t2) relPos]

mkRelErrs :: Type -> Type -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [String]
mkRelErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ _ = [s1, s2]
mkRelErrs (Base (ERROR s)) _ _ _ _ = [s]
mkRelErrs _ (Base (ERROR s)) _ _ _ = [s]
mkRelErrs t1 t2 t1Pos t2Pos relPos
    | rel t1 t2 == Base BOOL = []
    | otherwise = [ mkStringError ("Type mismatch: can't compare " ++ typeToString t1 ++ " with " ++ typeToString t2) relPos]

prettyRelErr :: [String] -> String -> [String]
prettyRelErr errs relName = map (++ " in '" ++ relName ++ "' expression") errs

mkBinOppErrs :: Type -> Type -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> [String]
mkBinOppErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ _ _ = [s1, s2]
mkBinOppErrs (Base (ERROR s)) _ _ _ _ _ = [s]
mkBinOppErrs _ (Base (ERROR s)) _ _ _ _ = [s]
mkBinOppErrs t1 t2 t1Pos t2Pos oppPos opName
    | isERROR (mathtype t1) = [ mkStringError ("Can not perform arithmetic operation in the first argument of '" ++ opName ++ "' expression, type found: " ++ typeToString t1) oppPos]
    | isERROR (mathtype t2) = [ mkStringError ("Can not perform arithmetic operation in the second argument of '" ++ opName ++ "' expression, type found: " ++ typeToString t2) oppPos]
    | mathtype t1 == mathtype t2 = []
    | otherwise = [ mkStringError ("Type mismatch: can't perform arithmetic operations on " ++ typeToString t1 ++ " and " ++ typeToString t2) oppPos]

mkJumpStatementErrs :: String -> EnvT -> (Int, Int) -> [String]
mkJumpStatementErrs s env pos
    | containsEntry s env = []
    | otherwise = [ mkStringError (s ++ " statement outside of loop") pos]