{- Clang.hs
 - Contains function definitions for converting a Javascript abstract
 - syntax tree into an abstract C program.
 -}

module Clang
  ( CProg(..)
  , CFuncDecl(..)
  , CStmt(..)
  , CExpr(..)
  , cprog
  ) where

import Control.Applicative (liftA2, liftA3)
import Language.JavaScript.Parser.Parser as Parser
import Language.JavaScript.Parser.AST as AST
import qualified Data.Map as Map

data CProg = CProg (Map.Map String CExpr) [CFuncDecl]
           deriving Show

data CFuncDecl = CFuncDecl String [String] [CStmt]
               deriving Show

data CStmt = CVarDecl String (Maybe CExpr)
           | CSetVar String CExpr
           | CFuncStmt String [CExpr]
           | CLabelled String CStmt
           | CIf CExpr [CStmt]
           | CIfElse CExpr [CStmt] [CStmt]
           | CWhile CExpr [CStmt]
           | CReturn CExpr
           deriving Show

data CExpr = CTrue
           | CFalse
           | CUndefined
           | CNaN
           | CInfinity
           | CNumber String
           | CVar String
           | CFuncCall String [CExpr]
           | CAdd CExpr CExpr
           | CSub CExpr CExpr
           | CMul CExpr CExpr
           | CDiv CExpr CExpr
           | CMod CExpr CExpr
           | CPos CExpr
           | CNeg CExpr
           | CAnd CExpr CExpr
           | COr CExpr CExpr
           | CNot CExpr
           | CEqu CExpr CExpr
           | CSequ CExpr CExpr
           | CNeq CExpr CExpr
           | CSneq CExpr CExpr
           | CLT CExpr CExpr
           | CLE CExpr CExpr
           | CGT CExpr CExpr
           | CGE CExpr CExpr
           | CTer CExpr CExpr CExpr
           deriving Show

-- Converts a JSCommaList to a regular list
fromCommaList :: JSCommaList a -> [a]
fromCommaList (JSLCons head _ tail) = (fromCommaList head) ++ [tail]
fromCommaList (JSLOne x) = [x]
fromCommaList JSLNil = []

-- Extracts the variable initializations from the comma list provided in
-- a JSVariable construct. Uninitialized variables are initialized to
-- undefined in the C context.
getVarInits :: JSCommaList JSExpression -> Maybe [(String, CExpr)]
getVarInits = combine1 . (f<$>) . fromCommaList
  where f (JSVarInitExpression (JSIdentifier _ s) (JSVarInit _ e)) =
          case cexpr e of
            Just e' -> Just (s, e')
            Nothing -> Nothing
        f (JSVarInitExpression (JSIdentifier _ s) JSVarInitNone) =
          Just (s, CUndefined)
        f _ = Nothing

cprog :: JSAST -> Maybe CProg
cprog (JSAstProgram stmts _) = cprog' stmts
cprog _ = Nothing

-- Converts JSStatements into CDecl and CStmt objects. All function/variable
-- declarations must appear before any other statement. The C main function
-- begins with the first statement that is not a func/var declaration.
cprog' :: [JSStatement] -> Maybe CProg
cprog' [] = Just (CProg Map.empty [])
cprog' ((JSFunction _ (JSIdentName _ ident) _ arglist _ (JSBlock _ stmts _) _):rest) =
  do args <- combine1 (f <$> (fromCommaList arglist))
     body <- getBody stmts
     (CProg rv rf) <- cprog' rest
--     (CProg rd rs) <- cprog' rest
     return $ CProg rv ((CFuncDecl ident args body):rf)
--     return $ CProg ((CFuncDecl ident args body):rd) rs
    where f (JSIdentName _ x) = Just x
          f _ = Nothing
          getBody [] = Just [CReturn CUndefined]
          getBody [JSReturn _ Nothing _] = Just [CReturn CUndefined]
          getBody [JSReturn _ (Just e) _] = cexpr e >>= (\x -> Just [CReturn x])
          getBody [s] = liftA2 (++) (cstmt s) (Just [CReturn CUndefined])
          getBody (s:ss) = liftA2 (++) (cstmt s) (getBody ss)
cprog' ((JSVariable _ clist _):rest) =
  do vars <- getVarInits clist
     (CProg rv rf) <- cprog' rest
     return $ CProg (Map.union rv (Map.fromList vars)) rf
cprog' _ = Nothing

-- Converts a single JS statement into a list of C statements
cstmt :: JSStatement -> Maybe [CStmt]
cstmt (JSStatementBlock _ stmts _ _) = combine2 (cstmt <$> stmts)
cstmt (JSIf _ _ e _ s) =
  do cond <- cexpr e
     ifBlock <- cstmt s
     return [CIf cond ifBlock]
cstmt (JSIfElse _ _ e _ s1 _ s2) =
  do cond <- cexpr e
     ifBlock <- cstmt s1
     elseBlock <- cstmt s2
     return [CIfElse cond ifBlock elseBlock]
cstmt (JSLabelled (JSIdentName _ l) _ s) =
  do (s':ss') <- cstmt s
     return $ (CLabelled l s'):ss'
cstmt (JSAssignStatement e1 op e2 _) =
  do lhs <- cexpr e1
     rhs <- cexpr e2
     lhs' <- case lhs of
       CVar x -> Just x
       _      -> Nothing
     rhs' <- case op of
       JSAssign _       -> Just rhs
       JSPlusAssign _   -> Just (CAdd lhs rhs)
       JSMinusAssign _  -> Just (CSub lhs rhs)
       JSTimesAssign _  -> Just (CMul lhs rhs)
       JSDivideAssign _ -> Just (CDiv lhs rhs)
       JSModAssign _    -> Just (CMod lhs rhs)
       _                -> Nothing
     return [CSetVar lhs' rhs']
cstmt (JSWhile _ _ e _ s) =
  do cond <- cexpr e
     whileBlock <- cstmt s
     return [CWhile cond whileBlock]
cstmt (JSMethodCall (JSIdentifier _ fname) _ argList _ _) =
  do args <- combine1 (cexpr <$> (fromCommaList argList))
     return [CFuncStmt fname args]
cstmt (JSReturn _ Nothing _) = Just [CReturn CUndefined]
cstmt (JSReturn _ (Just e) _) = cexpr e >>= (\x -> Just [CReturn x])
cstmt (JSVariable _ clist _) =
  do vars1 <- getVarInits clist
     return (fmap (\(s,e) -> CVarDecl s (Just e)) vars1)
cstmt _ = Nothing

-- Builds a C expression
cexpr :: JSExpression -> Maybe CExpr
cexpr (JSIdentifier _ "undefined") = Just CUndefined
cexpr (JSIdentifier _ "NaN") = Just CNaN
cexpr (JSIdentifier _ "Infinity") = Just CInfinity
cexpr (JSIdentifier _ s) = Just (CVar s)
cexpr (JSDecimal _ s) = Just (CNumber s)
cexpr (JSLiteral _ "true") = Just CTrue
cexpr (JSLiteral _ "false") = Just CFalse
cexpr (JSExpressionBinary e1 op e2) =
  do loperand <- cexpr e1
     roperand <- cexpr e2
     operator <- case op of
       JSBinOpPlus _      -> Just CAdd
       JSBinOpMinus _     -> Just CSub
       JSBinOpTimes _     -> Just CMul
       JSBinOpDivide _    -> Just CDiv
       JSBinOpMod _       -> Just CMod
       JSBinOpAnd _       -> Just CAnd
       JSBinOpOr _        -> Just COr
       JSBinOpEq _        -> Just CEqu
       JSBinOpStrictEq _  -> Just CSequ
       JSBinOpNeq _       -> Just CNeq
       JSBinOpStrictNeq _ -> Just CSneq
       JSBinOpGe _        -> Just CGE
       JSBinOpGt _        -> Just CGT
       JSBinOpLe _        -> Just CLE
       JSBinOpLt _        -> Just CLT
       _                  -> Nothing
     return (operator loperand roperand)
cexpr (JSExpressionParen _ e _) = cexpr e
cexpr (JSExpressionTernary c _ e1 _ e2) =
  liftA3 CTer (cexpr c) (cexpr e1) (cexpr e2)
cexpr (JSMemberExpression (JSIdentifier _ fname) _ argList _) =
  do args <- combine1 (cexpr <$> (fromCommaList argList))
     return (CFuncCall fname args)
cexpr (JSUnaryExpression op e) =
  do operand <- cexpr e
     operator <- case op of
       JSUnaryOpPlus _  -> Just CPos
       JSUnaryOpMinus _ -> Just CNeg
       JSUnaryOpNot _   -> Just CNot
       _                -> Nothing
     return (operator operand)
cexpr _ = Nothing

-- Utility functions
combine1 :: [Maybe a] -> Maybe [a]
combine1 = foldr (liftA2 (:)) (Just [])

combine2 :: [Maybe [a]] -> Maybe [a]
combine2 = foldr (liftA2 (++)) (Just [])
