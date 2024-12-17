module PATPlay.Calculus.FindTerm where

import Prelude

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (State, StateT(..), execStateT, get, modify_, runStateT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Fmt as Fmt
import PATPlay.Calculus.Context (Context)
import PATPlay.Calculus.Syntax (Expr(..))
import Partial.Unsafe (unsafeCrashWith)

type TermConstraints = Map String Expr

type Goal = 
  { ctx :: Context
  , term :: Either String Expr
  , typ :: Either String Expr 
  }

type FindTermState = 
  { constraints :: TermConstraints
  , goals :: Array Goal
  , next :: Int
  }

type TermFinder a = StateT FindTermState (Reader Context) a

nextGoal :: TermFinder _
nextGoal = do 
  { goals } <- get
  case Array.uncons goals of 
    Nothing -> pure Nothing
    Just { head, tail } -> Just head <$ modify_ (\s -> s { goals = tail })

addGoal :: Goal -> TermFinder _ 
addGoal goal = do 
  modify_ \s -> s { goals = s.goals `Array.snoc` goal }

addConstraint :: String -> Expr -> TermFinder _
addConstraint v exp = do 
  modify_ \s -> s { constraints = Map.insert v exp s.constraints }

nextUnknown :: TermFinder String
nextUnknown = do 
  { next } <- get 
  modify_ (\s -> s { next = next + 1 })
  pure $ Fmt.fmt @"?{next}" { next }

nextVar :: String -> TermFinder String
nextVar prfx = do 
  { next } <- get 
  modify_ (\s -> s { next = next + 1 })
  pure $ prfx <> show next

searchContextByType :: Context -> Expr -> Maybe Expr
searchContextByType ctx typ = case ctx of 
  Nil -> Nothing
  (tm /\ typ'):tl 
    | typ == typ' -> Just (ExprVar tm) 
    | otherwise -> searchContextByType tl typ 

findTerm :: Context -> Expr -> _
findTerm ctx exp = do 
  let 
    s0 :: FindTermState
    s0 = 
        { goals: [ { ctx, term: Left "?0", typ: Right exp } ]
        , constraints: Map.empty
        , next: 1
        }
  runReader (execStateT loop s0) ctx
  where
  loop = do 
    goal <- nextGoal
    case goal of 
      Nothing -> pure unit
      Just { ctx: ctx', typ, term } -> case term, typ of 
        Left termVar, Right typ' -> do 
          find ctx' termVar typ'
          loop
        Right term, Left typVar -> do 
          loop
        _, _ -> unsafeCrashWith "Not implemented!"


  find ctx' termVar = case _ of 
    ExprPi var exp1 exp2 -> do 
      next <- nextUnknown
      addGoal { ctx: ctx', term: Right exp, typ: Left next }
      absVar <- if var == "_" then nextVar "v" else pure var
      addGoal { ctx: (absVar /\ exp1) : ctx', term: Left next, typ: Right exp2  }
      addConstraint termVar (ExprLam absVar exp1 (ExprVar next))
    ExprVar var -> do
      -- コンテキスト内にただちに使える(i.e. var の型を持つ)項があるか？ 
      case searchContextByType ctx' (ExprVar var) of 
        Just tm -> do 
          unsafeCrashWith "Oops1"
        Nothing -> do 
          unsafeCrashWith "Oops2"
    _ -> pure unit
