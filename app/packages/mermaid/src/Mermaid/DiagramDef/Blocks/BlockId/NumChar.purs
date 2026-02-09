module Mermaid.DiagramDef.Blocks.BlockId.NumChar (NumChar(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Mermaid.DiagramDef.Blocks.BlockId.Class
  ( class CharLike
  )
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data NumChar
  = N0
  | N1
  | N2
  | N3
  | N4
  | N5
  | N6
  | N7
  | N8
  | N9

derive instance Eq NumChar
derive instance Generic NumChar _
derive instance Ord NumChar

instance Arbitrary NumChar where
  arbitrary = genericArbitrary

instance CharLike NumChar where
  fromChar '0' = Just N0
  fromChar '1' = Just N1
  fromChar '2' = Just N2
  fromChar '3' = Just N3
  fromChar '4' = Just N4
  fromChar '5' = Just N5
  fromChar '6' = Just N6
  fromChar '7' = Just N7
  fromChar '8' = Just N8
  fromChar '9' = Just N9
  fromChar _ = Nothing
  toChar N0 = '0'
  toChar N1 = '1'
  toChar N2 = '2'
  toChar N3 = '3'
  toChar N4 = '4'
  toChar N5 = '5'
  toChar N6 = '6'
  toChar N7 = '7'
  toChar N8 = '8'
  toChar N9 = '9'

instance Show NumChar where
  show = genericShow

