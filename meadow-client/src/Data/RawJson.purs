module Data.RawJson where

import Data.Generic.Rep
  ( class Generic
  )
import Data.Lens
  ( Iso'
  )
import Data.Lens.Iso.Newtype
  ( _Newtype
  )
import Data.Newtype
  ( class Newtype
  )

newtype RawJson
  = RawJson String

derive instance genericRawJson ::
  Generic RawJson _

derive instance newtypeRawJson ::
  Newtype RawJson _

_RawJson ::
  Iso' RawJson String
_RawJson = _Newtype
