module TypeSystem where
import GHC.Windows (getErrorMessage)

data BasicType = ERROR String | INT | FLOAT | BOOL | CHAR | STRING | NONE
  deriving (Eq, Show, Read)

data Type = Base BasicType | ARRAY Type | POINTER Type | ADDRESS Type
  deriving (Eq, Show, Read)

-- Given two Types, returns the superior one, ERROR if not compatible
sup :: Type -> Type -> Type
sup (Base INT)(Base INT)          = Base INT
sup (Base FLOAT)(Base FLOAT)      = Base FLOAT
sup (Base BOOL)(Base BOOL)        = Base BOOL
sup (Base CHAR)(Base CHAR)        = Base CHAR
sup (Base STRING)(Base STRING)    = Base STRING

sup (Base FLOAT) (Base INT)       = Base FLOAT
sup (Base INT) (Base FLOAT)       = Base FLOAT
sup (Base INT) (Base BOOL)        = Base INT
sup (Base BOOL)  (Base INT)       = Base INT
sup (Base STRING) (Base CHAR)     = Base STRING
sup (Base CHAR) (Base STRING)     = Base STRING
sup (Base INT) (Base CHAR)        = Base INT
sup (Base CHAR) (Base INT)        = Base INT

sup (ARRAY t1) (ARRAY t2) = 
  if t1 == t2 
    then ARRAY t1 
  else Base (ERROR ("Array types "++ typeToString t1 ++ " and " ++ typeToString t2 ++ " do not match"))

sup (POINTER t1) (POINTER t2) =
  if t1 == t2
    then POINTER t1
  else Base (ERROR ("Pointer types "++ typeToString t1 ++ " and " ++ typeToString t2 ++ " do not match"))

sup (ADDRESS t1) (POINTER t2) =  
  if t1 == t2
    then POINTER t1
  else Base (ERROR ("Pointer of Type "++ typeToString t2 ++ " is pointing to an Address of Type " ++ typeToString t1))

sup (POINTER t1) (ADDRESS t2) =
  if t1 == t2
    then POINTER t1
  else Base (ERROR ("Pointer of Type "++ typeToString t1 ++ " is pointing to an Address of Type " ++ typeToString t2))

sup (Base (ERROR s)) _            = Base (ERROR s)
sup _ (Base (ERROR s))            = Base (ERROR s)

sup t1 t2                         = Base (ERROR ("Type mismatch: " ++ typeToString t1 ++ " and " ++ typeToString t2 ++ " are not compatible"))

getArrayCurrentType :: Type -> Int -> Type
getArrayCurrentType (Base (ERROR s)) _ = Base (ERROR s)
getArrayCurrentType t 0           = t
getArrayCurrentType (ARRAY t) n   = getArrayCurrentType t (n - 1)
getArrayCurrentType _ _           = Base (ERROR "Invalid array access")

getDereferencedType :: Type -> Type
getDereferencedType (Base (ERROR s)) = Base (ERROR s)
getDereferencedType (POINTER t)   = t
getDereferencedType _ = Base (ERROR "Invalid pointer dereference")

-- Given a type, returns a string representation of it
typeToString :: Type -> String
typeToString (Base (ERROR s))     = s
typeToString (Base INT)           = "Integer"
typeToString (Base FLOAT)         = "Double"
typeToString (Base BOOL)          = "Boolean"
typeToString (Base CHAR)          = "Character"
typeToString (Base STRING)        = "String"
typeToString (Base NONE)          = "None"
typeToString (ARRAY t2)           = "Array of " ++ typeToString t2;
typeToString (POINTER t)          = "Pointer to " ++ typeToString t;
typeToString (ADDRESS t)          = "Address of " ++ typeToString t;

-- Given a type, returns the basic arithmetic type
mathtype :: Type -> Type
mathtype (Base FLOAT)             = Base FLOAT
mathtype (Base INT)               = Base INT
mathtype (Base BOOL)              = Base INT
mathtype (Base CHAR)              = Base INT
mathtype t                        = Base (ERROR ("Cannot perform arithmetic operations on type '" ++ typeToString t ++ "'"))

-- Given two types, returns the BOOL if they are compatible
rel :: Type -> Type -> Type
rel (Base BOOL) (Base BOOL)       = Base (ERROR "Cannot compare with boolean values")
rel (Base BOOL) _                 = Base (ERROR "Cannot compare with boolean values")
rel _ (Base BOOL)                 = Base (ERROR "Cannot compare with boolean values")
rel x y = case sup x y of
  Base (ERROR d)                  -> Base (ERROR d)
  _                               -> Base BOOL

getErrorMessage :: Type -> String
getErrorMessage (Base (ERROR s))  = s
getErrorMessage _                 = "No error"

-- Checks if a value is BOOL
isBoolean :: Type -> Bool
isBoolean (Base BOOL)             = True
isBoolean _                       = False

-- Checks if a value is INT
isInt :: Type -> Bool
isInt (Base INT)                  = True
isInt _                           = False

-- Checks if a value is FLOAT
isFloat :: Type -> Bool
isFloat (Base FLOAT)              = True
isFloat _                         = False

-- Checks if a value is CHAR
isChar :: Type -> Bool
isChar (Base CHAR)                = True
isChar _                          = False

-- Checks if a value is STRING
isString :: Type -> Bool
isString (Base STRING)            = True
isString _                        = False

-- Checks if a value is an ERROR
isERROR :: Type -> Bool
isERROR (Base (ERROR _))          = True
isERROR _                         = False

-- Checks if a value is a ARRAY
isArray :: Type -> Bool
isArray (ARRAY _)                 = True
isArray _                         = False