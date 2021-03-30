{- InterJS.hs
 - Defines the abstract data structures for "Intermediate Javascript", a
 - simplified version of Javascript that can be more easily translated
 - into modeling languages.
 -}

module InterJS where

data MProg = MProg [MVarDecl] [MStmt]
   deriving Show

data MVarDecl
   = MIntVarDecl String MIntExpr
   | MBoolVarDecl String MBoolExpr
   deriving Show

data MStmt
   = MSetBoolVar String MBoolExpr
   | MSetIntVar String MIntExpr
   | MIf MBoolExpr [MStmt]
   | MIfElse MBoolExpr [MStmt] [MStmt]
   | MWhile MBoolExpr [MStmt]
   deriving Show

data MBoolExpr
   = MTrue
   | MFalse
   | MBoolVar String
   | MEqu MIntExpr MIntExpr
   | MNeq MIntExpr MIntExpr
   | MLT MIntExpr MIntExpr
   | MLE MIntExpr MIntExpr
   | MGT MIntExpr MIntExpr
   | MGE MIntExpr MIntExpr
   | MOr MBoolExpr MBoolExpr
   | MAnd MBoolExpr MBoolExpr
   | MNot MBoolExpr
   | MEqv MBoolExpr MBoolExpr
   | MXor MBoolExpr MBoolExpr
   deriving Show

data MIntExpr
   = MIntVal Integer
   | MIntVar String
   | MAdd MIntExpr MIntExpr
   | MSub MIntExpr MIntExpr
   | MMul MIntExpr MIntExpr
   | MDiv MIntExpr MIntExpr
   | MMod MIntExpr MIntExpr
   | MNeg MIntExpr
   deriving Show

