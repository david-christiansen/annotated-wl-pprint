module Text.PrettyPrint.Annotated.Modern (
  module X,
  (<#>),
  (<##>)
) where

import Text.PrettyPrint.Annotated.Leijen as X hiding ((<$>), (<$$>))
import qualified Text.PrettyPrint.Annotated.Leijen as Leijen

infixr 5 <#>, <##>

(<#>), (<##>) :: Doc a -> Doc a -> Doc a
(<#>) = (Leijen.<$>)
(<##>) = (Leijen.<$$>)
{-# INLINE (<#>) #-}
{-# INLINE (<##>) #-}
