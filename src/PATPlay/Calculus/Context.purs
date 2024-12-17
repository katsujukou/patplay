module PATPlay.Calculus.Context where

import Prelude

import Data.List (List)
import Data.Tuple (Tuple(..))
import PATPlay.Calculus.Syntax (Expr)

type Context = List (Tuple String Expr)
