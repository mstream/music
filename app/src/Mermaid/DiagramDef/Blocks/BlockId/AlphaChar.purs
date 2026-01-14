module Mermaid.DiagramDef.Blocks.BlockId.AlphaChar (AlphaChar(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Mermaid.DiagramDef.Blocks.BlockId.Class
  ( class CharLike
  )
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data AlphaChar
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z

derive instance Eq AlphaChar
derive instance Generic AlphaChar _
derive instance Ord AlphaChar

instance Arbitrary AlphaChar where
  arbitrary = genericArbitrary

instance CharLike AlphaChar where
  fromChar 'a' = Just A
  fromChar 'b' = Just B
  fromChar 'c' = Just C
  fromChar 'd' = Just D
  fromChar 'e' = Just E
  fromChar 'f' = Just F
  fromChar 'g' = Just G
  fromChar 'h' = Just H
  fromChar 'i' = Just I
  fromChar 'j' = Just J
  fromChar 'k' = Just K
  fromChar 'l' = Just L
  fromChar 'm' = Just M
  fromChar 'n' = Just N
  fromChar 'o' = Just O
  fromChar 'p' = Just P
  fromChar 'q' = Just Q
  fromChar 'r' = Just R
  fromChar 's' = Just S
  fromChar 't' = Just T
  fromChar 'u' = Just U
  fromChar 'v' = Just V
  fromChar 'w' = Just W
  fromChar 'x' = Just X
  fromChar 'y' = Just Y
  fromChar 'z' = Just Z
  fromChar _ = Nothing
  toChar A = 'a'
  toChar B = 'b'
  toChar C = 'c'
  toChar D = 'd'
  toChar E = 'e'
  toChar F = 'f'
  toChar G = 'g'
  toChar H = 'h'
  toChar I = 'i'
  toChar J = 'j'
  toChar K = 'k'
  toChar L = 'l'
  toChar M = 'm'
  toChar N = 'n'
  toChar O = 'o'
  toChar P = 'p'
  toChar Q = 'q'
  toChar R = 'r'
  toChar S = 's'
  toChar T = 't'
  toChar U = 'u'
  toChar V = 'v'
  toChar W = 'w'
  toChar X = 'x'
  toChar Y = 'y'
  toChar Z = 'z'

instance Show AlphaChar where
  show = genericShow

