{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module ErrorsBuilder where

import Text.Read
import Data.List
import TypeSystem as TS
import Env as E
import AbsPhynot

mkAssignmentErrs :: Modality -> String -> TS.Type -> TS.Type -> (Int, Int) -> (Int, Int) -> [String]
mkAssignmentErrs Modality_const varName _ _ varPos _ = [mkStringError  ("Error: constant '" ++ varName ++ "' can not be modified") varPos]
mkAssignmentErrs _ _ (Base (ERROR s1)) (Base (ERROR s2)) _ _ = [s1, s2]
mkAssignmentErrs _ _ (Base (ERROR s)) _ _ _ = [s]
mkAssignmentErrs _ _ _ (Base (ERROR s)) _ _ = [s]
mkAssignmentErrs _ varName varType assType varPos assPos
    | sup varType assType == varType = []
    | otherwise = [mkStringError  ("Type mismatch: can't assign " ++ typeToString assType ++ " value to variable " ++ varName ++ " of type " ++ typeToString varType ) varPos]

mkArrayAssignmentErrs :: String -> [Int] -> [Int] -> (Int, Int) -> [String]
mkArrayAssignmentErrs _ [] [] _ = []
mkArrayAssignmentErrs varName (x:varLength) (y:assLength) pos
    | length (x:varLength) /= length (y:assLength) = [mkStringError ("Error: can't assign array of length " ++ show (length (y:assLength)) ++ " to array " ++ varName ++ " of length " ++ show (length (x:varLength))) pos]
    | x == y = mkArrayAssignmentErrs varName varLength assLength pos
    | otherwise = mkStringError ("Error: can't assign array of length " ++ show y ++ " to array '" ++ varName ++ "' of length " ++ show x) pos : mkArrayAssignmentErrs varName varLength assLength pos

mkArrayErrs :: TS.Type -> TS.Type -> Int -> Int -> (Int, Int) -> [String]
mkArrayErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ _ = [s1, s2]
mkArrayErrs (Base (ERROR s)) _ _ _ _ = [s]
mkArrayErrs _ (Base (ERROR s)) _ _ _ = [s]
mkArrayErrs t1 t2 len1 len2 pos
    | t1 /= t2 = [mkStringError ("Array elements must be of the same type: found '" ++ TS.typeToString t1 ++ "' and " ++ TS.typeToString t2) pos]
    | TS.isArray t1 && TS.isArray t2 && len1 /= len2 = [mkStringError ("Error: array lengths are not consistent, found " ++ show len1 ++ " and " ++ show len2) pos]
    | otherwise = []

mkArrayIndexErrs :: String -> (Int, Int) -> [String]
mkArrayIndexErrs s pos = case t of
    Nothing -> [mkStringError "Array index must be an integer literal or costant" pos]
    Just 0 -> [mkStringError "Error: Array index must be a integer literal greater than 0" pos]
    Just x -> []
    where t = readMaybe s :: Maybe Int
   
mkStringError :: String -> (Int, Int) -> String
mkStringError s (a, b) = "[" ++ show a ++ ":" ++ show b ++ "] " ++ s

mkError :: String -> (Int, Int) -> TS.Type
mkError s (a, b) = Base (ERROR (mkStringError s (a, b)));

mkIfErrs :: TS.Type -> (Int, Int) -> [String]
mkIfErrs t pos = case t of
  Base (ERROR e) -> [e]
  Base BOOL -> []
  _ -> [mkStringError ("Error: if statement guard not boolean, found: " ++ typeToString t) pos]

mkWhileErrs :: TS.Type -> (Int, Int) -> [String]
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

mkDeclInitErrs :: TS.Type -> TS.Type -> EnvT -> String -> (Int, Int) -> [String]
mkDeclInitErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ _ = [s1, s2]
mkDeclInitErrs (Base (ERROR s)) _ _ _ _= [s]
mkDeclInitErrs _ (Base (ERROR s)) _ _ _= [s]
mkDeclInitErrs varType initType env varName pos
    | containsEntry varName env = [mkStringError ("Variable '" ++ varName ++ "' already declared at: " ++ show (getVarPos varName env)) pos] 
    | sup varType initType == varType = []
    | otherwise = [ mkStringError ("Type mismatch: can't convert " ++ typeToString initType ++ " to " ++ typeToString varType) pos]

mkArrayInitErrs :: TS.Type -> TS.Type -> String -> [Int] -> [Int] -> (Int, Int) -> [String]
mkArrayInitErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ _ _ = [s1, s2]
mkArrayInitErrs (Base (ERROR s)) _ _ _ _ _ = [s]
mkArrayInitErrs _ (Base (ERROR s)) _ _ _ _ = [s]
mkArrayInitErrs varType initType varName varLength initLength pos
    | varType /= initType = [mkStringError ("Can not assign '" ++ TS.typeToString varType ++ "' to '" ++ TS.typeToString initType ++ "'") pos]
    | otherwise = mkArrayAssignmentErrs varName varLength initLength pos


mkConstDeclErrs :: TS.Type -> TS.Type -> EnvT -> String -> (Int, Int) -> [String]
mkConstDeclErrs cosType initType env cosName pos
    | containsEntry cosName env = [mkStringError ("Costant value '" ++ cosName ++ "' already declared at: " ++ show (getVarPos cosName env)) pos]
    | sup cosType initType == cosType = []
    | otherwise = [ mkStringError ("Type mismatch: can't convert " ++ typeToString initType ++ " to " ++ typeToString cosType) pos]

mkArrayAccessErrs :: TS.Type -> (Int, Int) -> [String]
mkArrayAccessErrs (Base (ERROR s)) _ = [s]
mkArrayAccessErrs t pos
    | isInt t = []
    | otherwise = [ mkStringError ("Array index must be an integer, found: " ++ typeToString t) pos]

mkNotErrs :: TS.Type -> (Int, Int) -> [String]
mkNotErrs (Base (ERROR s)) _ = [s]
mkNotErrs t pos
    | isBoolean t = []
    | otherwise = [ mkStringError ("'not' expects a boolean parameter, found " ++ typeToString t) pos]

mkParamErrs :: String -> String -> EnvT -> (Int, Int) -> [String]
mkParamErrs parName funcName env pos
    | containsEntry parName env = [mkStringError("Duplicate paramater '" ++ parName ++ "' in function declaration: '" ++ funcName ++ "'") pos]
    | otherwise = []

prettyFuncErr :: [String] -> String -> [String]
prettyFuncErr errs funcName = map (++ " inside function '" ++ funcName ++ "'") errs

mkFuncDeclErrs :: TS.Type -> EnvT -> String -> [(Modality, TS.Type)] -> (Int, Int) -> [String]
mkFuncDeclErrs funcType env funcName params pos
    | getVarPos funcName env == (-1,-1) = [mkStringError ("Primitive function '" ++ funcName ++ "' can not be redefined") pos] 
    | containsPrototype funcName env = protoToFuncErrs funcType funcName params env pos
    | containsEntry funcName env = [mkStringError ("Function '" ++ funcName ++ "' already declared at: " ++ show (getVarPos funcName env)) pos] 
    | otherwise = []

protoToFuncErrs :: TS.Type -> String -> [(Modality, TS.Type)] -> EnvT -> (Int, Int) -> [String]
protoToFuncErrs (Base (ERROR s)) _ _ _ _ = [s]
protoToFuncErrs funcType funcName params env pos
    | getFuncType funcName env == funcType && getFuncParams funcName env == params = []
    | getFuncType funcName env /= funcType && getFuncParams funcName env /= params = [mkStringError ("Error: function '" ++ funcName ++ "' has different return type: '" ++ typeToString (getFuncType funcName env) ++ "' and parameters: '" ++ intercalate ", " (map (\(mod, typ) -> show mod ++ ": " ++ typeToString typ) (getFuncParams funcName env)) ++ "' than prototype") pos]
    | getFuncType funcName env /= funcType = [mkStringError ("Error: function '" ++ funcName ++ "' has different return type: '" ++ typeToString (getFuncType funcName env) ++ "' than prototype") pos]
    | getFuncParams funcName env /= params = [mkStringError ("Error: function '" ++ funcName ++ "' has different parameters: '" ++ intercalate ", " (map (\(mod, typ) -> show mod ++ ": " ++ typeToString typ) (getFuncParams funcName env))++ "' than prototype") pos]
    | otherwise = []

mkPrototypeErrs :: TS.Type -> EnvT -> String -> [(Modality, TS.Type)] -> (Int, Int) -> [String]
mkPrototypeErrs funcType env funcName params pos
    | containsEntry funcName env = [mkStringError ("Prototype function '" ++ funcName ++ "' already declared at: " ++ show (getVarPos funcName env)) pos] 
    | otherwise = []

mkReturnErrs :: EnvT -> TS.Type -> (Int, Int) -> [String]
mkReturnErrs _ (Base (ERROR s)) _ = [s]
mkReturnErrs env retType pos
    | getVarType "return" env == retType = []
    | containsEntry "return" env = [mkStringError ("Error: the return value " ++ typeToString retType ++" is not " ++ typeToString (getVarType "return" env)) pos]
    | otherwise = [ mkStringError "Error: return statement outside function" pos]

mkFuncCallErrs :: String -> [(Modality, TS.Type)] -> EnvT -> (Int, Int) -> [String]
mkFuncCallErrs funcName params env pos
    | funcName == "writeInt" && length params == 1 && (mathtype (snd (head params)) == Base INT) = []
    | funcName == "writeFloat" && length params == 1 && (mathtype (snd (head params)) == Base FLOAT) = []
    | funcName == "writeChar" && length params == 1 && (mathtype (snd (head params)) == Base CHAR) = []
    | funcName == "writeString" && length params == 1 && (snd (head params) == Base STRING) = []
    | containsEntry funcName env && (params == getFuncParams funcName env) = []
    | containsEntry funcName env && (length params /=  length (getFuncParams funcName env)) = [mkStringError ("Error: function '" ++ funcName ++ "' expects " ++ show (length (getFuncParams funcName env)) ++ " parameters, found: " ++ show (length params)) pos]
    | containsEntry funcName env = mkFuncCallParamErrs funcName params (getFuncParams funcName env) pos
    | otherwise = []

mkFuncCallParamErrs :: String -> [(Modality, TS.Type)] -> [(Modality, TS.Type)] -> (Int, Int) -> [String]
mkFuncCallParamErrs _ [] [] _= []
mkFuncCallParamErrs funcName ((xi, xj):xs) ((yi, yj):ys) pos
    | any (TS.isERROR . snd) ((xi, xj):xs) = concatMap (\(_, t) -> case t of
        Base (ERROR s) -> [s ++ " in function '" ++ funcName ++ "' call"]
        _ -> []) ((xi, xj):xs)
    | yi == Modality_ref && (xi == Modality1 || xi == Modality_const) = 
        mkStringError ("Error: parameter can not be passed as reference in function '" ++ funcName ++ "' call") pos : mkFuncCallParamErrs funcName xs ys pos
    | yi == Modality_res && (xi == Modality1 || xi == Modality_const) = 
        mkStringError ("Error: parameter can not be passed as result in function '" ++ funcName ++ "' call") pos : mkFuncCallParamErrs funcName xs ys pos
    | yi == Modality_valres && (xi == Modality1 || xi == Modality_const) = 
        mkStringError ("Error: parameter can not be passed as value-result in function '" ++ funcName ++ "' call") pos : mkFuncCallParamErrs funcName xs ys pos
    | xj == yj = mkFuncCallParamErrs funcName xs ys pos
    | otherwise = mkStringError ("Error: can't match " ++ typeToString xj ++ " with expected type " ++ typeToString yj ++ " in function '" ++ funcName ++ "' call") pos : mkFuncCallParamErrs funcName xs ys pos

mkProcedureCallErrs :: String -> [(Modality, TS.Type)] -> EnvT -> (Int, Int) -> [String]
mkProcedureCallErrs procName params env pos
    | containsEntry procName env && (params == getFuncParams procName env) = []
    | containsEntry procName env && (length params /=  length (getFuncParams procName env)) = [mkStringError ("Error: function '" ++ procName ++ "' expects " ++ show (length (getFuncParams procName env)) ++ " parameters, found: " ++ show (length params)) pos]
    | containsEntry procName env = mkFuncCallParamErrs procName params (getFuncParams procName env) pos
    | not (containsEntry procName env) = [mkStringError ("Error: function '" ++ procName ++ "' not declared") pos]
    | otherwise = []

mkBoolRelErrs :: TS.Type -> TS.Type -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [String]
mkBoolRelErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ _ = [s1, s2]
mkBoolRelErrs (Base (ERROR s)) _ _ _ _ = [s]
mkBoolRelErrs _ (Base (ERROR s)) _ _ _ = [s]
mkBoolRelErrs t1 t2 t1Pos t2Pos relPos
    | sup t1 t2 == Base BOOL = []
    | otherwise = [ mkStringError ("Type mismatch: can't compare " ++ typeToString t1 ++ " with " ++ typeToString t2) relPos]

mkRelErrs :: TS.Type -> TS.Type -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [String]
mkRelErrs (Base (ERROR s1)) (Base (ERROR s2)) _ _ _ = [s1, s2]
mkRelErrs (Base (ERROR s)) _ _ _ _ = [s]
mkRelErrs _ (Base (ERROR s)) _ _ _ = [s]
mkRelErrs t1 t2 t1Pos t2Pos relPos
    | rel t1 t2 == Base BOOL = []
    | otherwise = [ mkStringError ("Type mismatch: can't compare " ++ typeToString t1 ++ " with " ++ typeToString t2) relPos]

prettyRelErr :: [String] -> String -> [String]
prettyRelErr errs relName = map (++ " in '" ++ relName ++ "' expression") errs

mkBinOppErrs :: TS.Type -> TS.Type -> (Int, Int) -> (Int, Int) -> (Int, Int) -> String -> [String]
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