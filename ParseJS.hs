{- ParseJS.hs
 - Handles all translation from Javascript into Intermediate Javascript.
 -}

module ParseJS where

import qualified Data.Set as Set
import Control.Applicative (liftA2, (<|>))
import Language.JavaScript.Parser.Parser as Parser
import Language.JavaScript.Parser.AST as AST
import InterJS

-- Converts a JSCommaList into a regular list
fromCommaList :: JSCommaList a -> [a]
fromCommaList (JSLCons head _ tail) = (fromCommaList head) ++ [tail]
fromCommaList (JSLOne x) = [x]
fromCommaList JSLNil = []

-- Constructs an intermediate program
mprog :: JSAST -> Maybe MProg
mprog (JSAstProgram [] _) = Just (MProg [] [])
mprog (JSAstProgram ((JSVariable _ clist _):ss) semi) =
  do vars1 <- combine1 (mvardecl <$> (fromCommaList clist))
     (MProg vars2 stmts) <- mprog (JSAstProgram ss semi)
     if isDisjoint vars1 vars2 then Just (MProg (vars1 ++ vars2) stmts) else Nothing
    where v (MIntVarDecl s _) = s
          v (MBoolVarDecl s _) = s
          isDisjoint l1 l2 =
            Set.null (Set.intersection (Set.fromList (v <$> l1)) (Set.fromList (v <$> l2)))
mprog (JSAstProgram stmts _) =
  do stmts' <- combine2 (mstmt <$> stmts)
     return (MProg [] stmts')
mprog _ = Nothing

-- Tries to interpret the JSExpression as a variable declaration
mvardecl :: JSExpression -> Maybe MVarDecl
mvardecl (JSVarInitExpression (JSIdentifier _ s) (JSVarInit _ e)) =
  (MBoolVarDecl s <$> mboolexpr e) <|> (MIntVarDecl s <$> mintexpr e)
mvardecl _ = Nothing

mstmt :: JSStatement -> Maybe [MStmt]
mstmt (JSStatementBlock _ stmts _ _) = combine2 (mstmt <$> stmts)
mstmt (JSIf _ _ e _ s) =
  do cond <- mboolexpr e
     ifBlock <- mstmt s
     return [MIf cond ifBlock]
mstmt (JSIfElse _ _ e _ s1 _ s2) =
  do cond <- mboolexpr e
     ifBlock <- mstmt s1
     elseBlock <- mstmt s2
     return [MIfElse cond ifBlock elseBlock]
mstmt (JSAssignStatement e1 op e2 _) =
  do rhs <- mboolexpr e2
     lhs <- mboolexpr e1
     lhs' <- case lhs of
       MBoolVar x -> Just x
       _          -> Nothing
     rhs' <- case op of
       JSAssign _ -> Just rhs
       _          -> Nothing
     return [MSetBoolVar lhs' rhs']
  <|>
  do rhs <- mintexpr e2
     lhs <- mintexpr e1
     lhs' <- case lhs of
       MIntVar x -> Just x
       _         -> Nothing
     rhs' <- case op of
       JSAssign _       -> Just rhs
       JSTimesAssign _  -> Just (MMul lhs rhs)
       JSDivideAssign _ -> Just (MDiv lhs rhs)
       JSModAssign _    -> Just (MMod lhs rhs)
       JSPlusAssign _   -> Just (MAdd lhs rhs)
       JSMinusAssign _  -> Just (MSub lhs rhs)
     return [MSetIntVar lhs' rhs']
mstmt (JSWhile _ _ e _ s) =
  do cond <- mboolexpr e
     whileBlock <- mstmt s
     return [MWhile cond whileBlock]
mstmt _ = Nothing

-- Boolean Expressions
mboolexpr :: JSExpression -> Maybe MBoolExpr
mboolexpr (JSIdentifier _ "undefined") = Nothing
mboolexpr (JSIdentifier _ s) = Just (MBoolVar s)
mboolexpr (JSLiteral _ "true") = Just MTrue
mboolexpr (JSLiteral _ "false") = Just MFalse
mboolexpr (JSExpressionBinary e1 op e2) =
  do loperand <- mboolexpr e1
     roperand <- mboolexpr e2
     operator <- case op of
       JSBinOpOr _  -> Just MOr
       JSBinOpAnd _ -> Just MAnd
       JSBinOpEq _  -> Just MEqv
       JSBinOpNeq _ -> Just MXor
       _            -> Nothing
     return (operator loperand roperand)
  <|>
  do loperand <- mintexpr e1
     roperand <- mintexpr e2
     operator <- case op of
       JSBinOpEq _  -> Just MEqu
       JSBinOpNeq _ -> Just MNeq
       JSBinOpLt _  -> Just MLT
       JSBinOpLe _  -> Just MLE
       JSBinOpGt _  -> Just MGT
       JSBinOpGe _  -> Just MGE
       _            -> Nothing
     return (operator loperand roperand)
mboolexpr (JSExpressionParen _ e _) = mboolexpr e
mboolexpr (JSMemberExpression (JSIdentifier _ fname) _ argList _) =
  case (fname, fromCommaList argList) of
    ("opEQU", x:y:[]) -> liftA2 MEqu (mintexpr x) (mintexpr y)
    ("opNEQ", x:y:[]) -> liftA2 MNeq (mintexpr x) (mintexpr y)
    ("opLT", x:y:[])  -> liftA2 MLT (mintexpr x) (mintexpr y)
    ("opLE", x:y:[])  -> liftA2 MLE (mintexpr x) (mintexpr y)
    ("opGT", x:y:[])  -> liftA2 MGT (mintexpr x) (mintexpr y)
    ("opGE", x:y:[])  -> liftA2 MGE (mintexpr x) (mintexpr y)
    ("opOR", x:y:[])  -> liftA2 MOr (mboolexpr x) (mboolexpr y)
    ("opAND", x:y:[]) -> liftA2 MAnd (mboolexpr x) (mboolexpr y)
    ("opNOT", x:[])   -> MNot <$> (mboolexpr x)
    ("opEQV", x:y:[]) -> liftA2 MEqv (mboolexpr x) (mboolexpr y)
    ("opXOR", x:y:[]) -> liftA2 MXor (mboolexpr x) (mboolexpr y)
    _               -> Nothing
mboolexpr (JSUnaryExpression op e) =
  do operand <- mboolexpr e
     operator <- case op of
       JSUnaryOpNot _ -> Just MNot
       _              -> Nothing
     return (operator operand)
mboolexpr _ = Nothing

-- Integer Expressions
mintexpr :: JSExpression -> Maybe MIntExpr
mintexpr (JSIdentifier _ "undefined") = Nothing
mintexpr (JSIdentifier _ s) = Just (MIntVar s)
mintexpr (JSDecimal _ s) = Just (MIntVal (read s))
mintexpr (JSExpressionBinary e1 op e2) =
  do loperand <- mintexpr e1
     roperand <- mintexpr e2
     operator <- case op of
       JSBinOpPlus _   -> Just MAdd
       JSBinOpMinus _  -> Just MSub
       JSBinOpTimes _  -> Just MMul
       JSBinOpDivide _ -> Just MDiv
       JSBinOpMod _    -> Just MMod
       _               -> Nothing
     return (operator loperand roperand)
mintexpr (JSExpressionParen _ e _) = mintexpr e
mintexpr (JSMemberExpression (JSIdentifier _ fname) _ argList _) =
  case (fname, fromCommaList argList) of
    ("opADD", x:y:[]) -> liftA2 MAdd (mintexpr x) (mintexpr y)
    ("opSUB", x:y:[]) -> liftA2 MSub (mintexpr x) (mintexpr y)
    ("opMUL", x:y:[]) -> liftA2 MMul (mintexpr x) (mintexpr y)
    ("opDIV", x:y:[]) -> liftA2 MDiv (mintexpr x) (mintexpr y)
    ("opMOD", x:y:[]) -> liftA2 MMod (mintexpr x) (mintexpr y)
    ("opNEG", x:[])   -> MNeg <$> (mintexpr x)
    _               -> Nothing
mintexpr (JSUnaryExpression op e) =
  do operand <- mintexpr e
     operator <- case op of
       JSUnaryOpMinus _ -> Just MNeg
       _                -> Nothing
     return (operator operand)
mintexpr _ = Nothing

-- Utility functions
combine1 :: [Maybe a] -> Maybe [a]
combine1 = foldr (liftA2 (:)) (Just [])

combine2 :: [Maybe [a]] -> Maybe [a]
combine2 = foldr (liftA2 (++)) (Just [])
