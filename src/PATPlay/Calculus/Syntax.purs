module PATPlay.Calculus.Syntax where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Expr
  = ExprSup
  | ExprStar
  | ExprVar String
  | ExprApp Expr Expr 
  | ExprLam String Expr Expr
  | ExprPi String Expr Expr 

derive instance Eq Expr 
derive instance Generic Expr _ 
instance Show Expr where
  show it = genericShow it 

