module PATPlay.Calculus.Printer where

import Prelude

import Fmt as Fmt
import PATPlay.Calculus.Syntax (Expr(..))

prettyPrint :: Expr -> String
prettyPrint = case _ of 
  ExprSup -> "□"
  ExprStar -> "*"
  ExprVar v -> v 
  ExprApp e1 e2 -> Fmt.fmt @"({e1}) ({e2})" { e1: prettyPrint e1, e2: prettyPrint e2 }
  ExprLam v e1 e2 -> Fmt.fmt @"λ({v}: {e1}). ({e2})" { v, e1: prettyPrint e1, e2: prettyPrint e2 }
  ExprPi v e1 e2 -> Fmt.fmt @"Π({v}: {e1}). ({e2})" { v, e1: prettyPrint e1, e2: prettyPrint e2 }

