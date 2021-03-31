module FSM
  ( FState
  , convert
  , symexec
  , fstate2string
  ) where

import Control.Applicative (liftA2)
import Data.List (intercalate)
import Text.Printf
import qualified Data.Map as Map
import Clang

{-
type FSM = [FBoolExpr] [FTrans]

data FTrans = String [(FBoolExpr, FState)]
         deriving Show

data FBoolExpr = FEqu FIntExpr FIntExpr
               | FLT FIntExpr FIntExpr
               | FAnd FBoolExpr FBoolExpr
               | FOr FBoolExpr FBoolExpr
               | FNot FBoolExpr
               | FTrue
         deriving Show

data FIntExpr = FVar String
              | FNum Integer
              | FAdd FIntExpr FIntExpr
              | FSub FIntExpr FIntExpr
         deriving Show
-}

data FTrans = FTrans String FState

instance Show FTrans where
  show (FTrans name fstate) = "Transition "++name++"\n"++(fstate2string fstate)

type FState = Map.Map String CExpr

fstate2string :: FState -> String  
fstate2string fstate =
  let m = \(k, a) -> show k ++ " -> " ++ show a
      lstrings = m <$> Map.toList fstate
   in intercalate "\n" lstrings

convert :: CProg -> Maybe [FTrans]
convert (CProg s0 decls) = combine1 (mapDecl <$> decls)
  where s0' = Map.mapWithKey (\k a -> CVar k) s0
        mapDecl (CFuncDecl name _ stmts) =
          FTrans name <$> symexec stmts s0'
        combine1 = foldr (liftA2 (:)) (Just [])

-- Given a list of statements and an initial
-- state, symbolically execute the statements
-- and return the final state
symexec :: [CStmt] -> FState -> Maybe FState
symexec [] state = Just state
symexec ((CReturn _):rest) state = Just state
symexec ((CSetVar v e):rest) state =
  do let newval = subst state e
     let newstate = Map.insert v newval state
     symexec rest newstate
symexec _ _ = Nothing

-- subst repls e
-- return e with the given substitutions made
subst :: (Map.Map String CExpr) -> CExpr -> CExpr
subst m (CVar v') =
  case Map.lookup v' m of
    Just e -> e
    Nothing -> CVar v'
subst m e =
  case e of
    CAdd e1 e2 -> CAdd (subst m e1) (subst m e2)
    CSub e1 e2 -> CSub (subst m e1) (subst m e2)
    CMul e1 e2 -> CMul (subst m e1) (subst m e2)
    CDiv e1 e2 -> CDiv (subst m e1) (subst m e2)
    eother     -> eother
