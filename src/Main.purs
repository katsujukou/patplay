module Main where

import Prelude

import Data.List (List(..), (:))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import PATPlay.Calculus.FindTerm (findTerm)
import PATPlay.Calculus.Syntax (Expr(..))

main :: Effect Unit
main = do
  let 
    context = ("A" /\ ExprStar) : ("B" /\ ExprStar) : Nil
    typ = ExprPi "_" (ExprVar "A") (ExprPi "_" (ExprPi "_" (ExprVar "A") (ExprVar "B")) (ExprVar "B"))
  logShow $ findTerm context typ 
  